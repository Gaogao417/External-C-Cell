

(* 

    the logic in this module is used to read / write from stdout

    Warning, Technical debt:

    The logic in this module is needed in general for externalevaluate, because we need to read/write from stdout while waiting for zmq messages.
    However an almost identical logic is used for "Shell" evaluator, that needs to simply read / write commands directly to stdout of a bash process.
    
    The logic to read and write is very tricky, a lot of things needs to be done right, including encoding, aborting, different handling of windows / unix, it's important that we implement this logic once and right.

    For 13.0 release we are going to centrailze this logic here in order to fix https://bugs.wolfram.com/show?number=415057 however we need to properly share this logic between ExternalEvaluate and ExternalEvaluateShell by writing a common abstraction.

    Right now in order to minimize bugs we are going to maintain here all the logic needed to run a shell command, even if this is not the right place to do it.

*)


(* fast pseudo id generator not using UUID because are using dashes for something else      *)
(* using character range for 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz *)
With[
    {range = Join[Range[48, 57], Range[65, 90], Range[97, 122]]}, 
    $UUIDGenerator := FromCharacterCode[Part[range, RandomInteger[{1, 62}, 20]]]
]

$ShellEncoding = "CP936"; (*"UTF-8"*)

$Windows       = $OperatingSystem === "Windows"
$NewLine       = If[$Windows, "\r\n", "\n"]
$UUID          = $UUIDGenerator <> "-"
$WindowsPrompt = $UUIDGenerator <> ">"

$SplitBy       = Join[
    {
        ToCharacterCode[$NewLine, $ShellEncoding] :> EndOfLine, 
        ToCharacterCode[$UUID,    $ShellEncoding] :> EndOfString
    },
    If[
        $Windows,
        {ToCharacterCode[$WindowsPrompt, $ShellEncoding] :> EndOfPrompt},
        {}
    ]
]

(* read / write commands that are respecting $CharacterEncoding *)

read[p:HoldPattern[_ProcessObject], rest___] := 
    read[ProcessConnection[p, "StandardOutput"], rest]


read[p:HoldPattern[_InputStream], extra_List] := 
    Replace[
        Quiet @ BinaryReadList[p], {
            {}     :> extra,
            s_List :> Join[SequenceSplit[s, $SplitBy], extra], 
            _   :> {$Aborted} (* anything that is not a list is either going to be Unevaluated or Failure, in that case for some reason we can't read anymore, we need to abort *)
        }
    ]
read[p_, f_?FailureQ] := read[p, {$Aborted}]
read[p_, _] := read[p, {}]

(* buffer and flash output logic *)

buffer[pre___][post___][rest___] := 
    buffer[pre][post, rest]

buffer[callback_, id_][EndOfString, s__List, EndOfLine, rest___] := With[
    {res = Success @@ StringSplit[FromCharacterCode[Join[s], $ShellEncoding], "-"]},
    callback @ res;
    If[
        MatchQ[res, Success[id, ___]],
        finished[callback, id][rest],
        buffer[callback, id][rest]
    ]
]

buffer[callback_, args___][s__List, sep:EndOfString|EndOfPrompt, rest___] := (
    callback @ FromCharacterCode[Join[s], $ShellEncoding]; 
    buffer[callback, args][sep, rest]
)
 
buffer[callback_, args___][s__List, EndOfLine, rest___] := (
    callback @ FromCharacterCode[Join[s], $ShellEncoding]; 
    buffer[callback, args][rest]
);   

buffer[callback_, args___][EndOfLine, r:EndOfLine|_List, rest___] := (
    callback @ "";
    buffer[callback, args][r, rest]
)

buffer[callback_, args___][EndOfLine, EndOfString, rest___] := (
    callback @ "";
    callback @ "";
    buffer[callback, args][EndOfString, rest]
)

(* there might be a case where the process is aborted and there is a non empty buffer, we need to flush it by adding a newline *)

buffer[args___][s__List, end:EndOfPrompt|$Aborted, rest___] := 
    buffer[args][s, EndOfLine, end, rest]

(* windows will re-write the command, jumping ahead until we hit a newline *)
buffer[args___][EndOfLine, end:EndOfPrompt|$Aborted, rest___] := 
    buffer[args][end, rest]

buffer[args___][EndOfPrompt, ___, EndOfLine, rest___] := 
    buffer[args][rest]

buffer[args___][$Aborted, rest___] :=
    finished[rest]


finished[___] := finished


flushOutput[p:HoldPattern[_ProcessObject], stdoutcallback_, stderrcallback_, extra_, stopwith_:_String] := 
    Block[
        {
            stdout = buffer[stdoutcallback, stopwith][If[$Windows, EndOfPrompt, Sequence @@ {}]], 
            stderr = buffer[stderrcallback, stopwith],
            action = Null
        },
        (* 
            buffer will execute any callback when an EndOfLine is reached 
            every time a line is completed we want to do 2 operations:
            1. collect the line for the final result using Sow
            2. immediately print it in the frontend
            3. the program will finish when finished is reached in stdout or stderr
        *)
        Do[
            action = extra[p, counter];
            Apply[
                Function[
                    {key, sym},
                    Replace[
                        read[ProcessConnection[p, key], action], {
                            {}|{{}} :> Null,
                            s_List :> Set[sym, Apply[sym, s]]
                        }
                    ],
                    HoldAllComplete
                ],
                {
                    Hold["StandardOutput", stdout],
                    Hold["StandardError", stderr]
                },
                {1}
            ];
            If[
                Or[
                    stdout === finished,
                    stderr === finished
                ],
                Break[]
            ], 
            {counter, Infinity}
        ];
    ]


