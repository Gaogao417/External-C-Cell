BeginPackage["ExternalEvaluateShell`"]

Begin["`Private`"]

(* 
    Technical debt: 
    properly export and use runShellCommand from a common location insteaf of hardcoding the path, 
    see ExternalEvaluate/Code/ExternalEvaluateShell.m for more details
*)

flushOutput    := ExternalEvaluate`Private`flushOutput;
$Windows       := ExternalEvaluate`Private`$Windows;
$WindowsPrompt := ExternalEvaluate`Private`$WindowsPrompt;
$NewLine       := ExternalEvaluate`Private`$NewLine;
$ShellEncoding := ExternalEvaluate`Private`$ShellEncoding;
$UUIDGenerator := ExternalEvaluate`Private`$UUIDGenerator;
$UUID          := ExternalEvaluate`Private`$UUID;

(* command logic *)

runCommand[p_, cmd_] := runCommand[p, cmd, Print]
runCommand[p_, cmd_List, rest___] := Map[runCommand[p, #, rest] &, cmd]
runCommand[p_, cmd_String, printer_] := collect @ With[
    {id = $UUIDGenerator},
    
    (* we send the initial value so that Reap will collect keys in the right order *)
    Sow[cmd, key["Command"]];
    
    (* we write the command *)
    Quiet[
        BinaryWrite[
            ProcessConnection[p, "StandardInput"],
            StringToByteArray[
                StringJoin[
                    StringTrim[cmd], 
                    $NewLine, 
                    "echo ",
                    $UUID, 
                    id,
                    "-",
                    If[
                        $Windows, 
                        (* The windows logic needs to fix several very weird behaviours.

                        1. the logic to echo the last error code is different between powershell and cmd.exe, we need to echo all 
                        2. cmd.exe needs to reset the exit code manually otherwise if something fails the same exit code will keep popping up*)
                        {"%ErrorLevel%-$?", $NewLine, "cmd /c \"exit /b 0\""}, 
                        "$?"
                    ],
                    $NewLine
                ], 
                $ShellEncoding
            ]
        ], 
        {BinaryWrite::errfile}
    ];

    (* 
        flushOutput is going to keep flushing the input unless we stop it, or the user is aborting the computation.
        if a shell long running task is aborted the program will flush at some point, an Success symbolic wrapper where the first argument is the sender id, and the following arguments are the exit code.

        if the output is flushing another Success[id, ...] it means that it was produced by a previous task it was never waited correctly, in that case we need to collect this information, so that all the output coming before a $Aborted is not collected in the final result.

    *)

    flushOutput[
        p,
        (* StandardError Callback*) 
        Replace[
            {   
                Success[id, status___String] :> 
                    Scan[
                        Sow[#, key["ExitCode"]] &,
                        {status}
                    ],

                _Success :> (
                    Sow[$Aborted, key["StandardOutput"]];
                    Sow[$Aborted, key["StandardError"]]; (* TODO: stderr might not be in sync if we just throw a terminator when reading stdout *)
                ),

                any_String :> (
                    Sow[any, key["StandardOutput"]];
                    printer[any]
                )
            }
        ],
        (* StandardError Callback*) 
        Function[
            Sow[#, key["StandardError"]];
            printer[Style[#, "Message", FontFamily -> "Source Code Pro"]];
        ],
        Function[If[#2 > 50, Pause[0.05]]],
        id
    ];  
]



(* collect, merge and dispatch logic *)

SetAttributes[collect, HoldFirst]

collect[expr_] := 
    Association[
        "Command" -> None,
        "ExitCode" -> "ProcessTerminated",
        "StandardError" -> "",
        "StandardOutput" -> "",
        Reap[expr; {}, _key, merge]
    ]


(* adding the merging logic, we need to collect all bits of informations collected 
    StandardOutput and StandardError might see a $Aborted during the execution.
    That happens if a previous command has been aborted, in that case we are still printing the output, but it should be discarded, 
    because it wasn't coming from the same command. 
    We might consider also issuing a message when this happens.
*)

merge[key[s_String], rest___] := merge[s, rest]
merge[k:("StandardError"|"StandardOutput"), {___, $Aborted, Shortest[rest___]}] := k -> StringJoin @ Riffle[{rest}, $NewLine]
merge[k:("StandardError"|"StandardOutput"), list_] := k -> StringJoin @ Riffle[list, "\n"] (* using \r\n is causing the frontend to display 2 lines *)
merge[k:"ExitCode", {rest___, code:"0"|"1"|"True"|"False"}] := k -> ToExpression[code]
merge[k:"ExitCode", {rest___, "$?"|"%ErrorLevel%"}] := merge[k, {rest}]
merge[k:"ExitCode", {___, s_String}] := k -> If[StringMatchQ[s, DigitCharacter..], FromDigits[s], s]
merge[k_, el_] := k -> Last[el]


dispatch["expression", res:KeyValuePattern["ExitCode" -> 0|True]] := Success["ExecutionCompleted", res]
dispatch["expression", res_] := Failure["RuntimeError", res]
dispatch["association", res_] := res
dispatch["exitcode", res_] := res["ExitCode"]
dispatch["standardoutput", res_] := res["StandardOutput"]
dispatch["standarderror", res_] := res["StandardError"]


ExternalEvaluate`RegisterSystem[
    "Shell",
    <|

        "StringTemplateSupportQ" -> True,

        (*the default version exec command just runs the executable with --version as the argument*)
        "VersionExecCommandFunction"->Switch[
            $OperatingSystem,
            "Windows",
            Null,
            _,
            Function[{#, "--version"}]
        ],

        "VersionStringConformFunction" -> Function[
            First[StringCases[#, RegularExpression["[0-9\\.]+"]], Missing["NotAvailable"]]
        ],
        "NonZMQInitializeFunction"-> 
            If[
                $Windows,
                Function @ runCommand[
                    #2, 
                    "function prompt { return '" <> $WindowsPrompt <> "'}", 
                    Function[Null]
                ],
                Function @ read[ProcessConnection[#2, "StandardError"]]
            ],
        "NonZMQEvaluationFunction"-> Function[
            {obj, msg},
            dispatch[
                msg["return_type"], 
                runCommand[
                    obj["Process"], 
                    msg["input"]
                ]
            ]
        ],
        "ReturnTypes" -> "Expression"|"Association"|"ExitCode"|"StandardOutput"|"StandardError",
        "ScriptExecCommandFunction" -> 
            If[
                $Windows,
                Function[{exec, file, uuid}, {exec, "-noexit"}], (* add "/K", "chcp", "65001" to use UTF-8 encoding *)
                Function[{exec, file, uuid}, {exec}]
            ],

        "FileToCommandFunction" -> Function[<|"Command" -> StringReplace[#, " " -> "\ "]|>],
        "ProcessEnvironmentFunction"->Switch[
            $OperatingSystem,
            "Windows",
            Function[Join[GetEnvironment[], {"PROMPT" -> $WindowsPrompt, "PS1" -> $WindowsPrompt}]],
            _,
            Function[Inherited]
        ],

        "StringTemplateVariableNameFunction" -> Function[TextString[#2]],


        "TargetDiscoveryFunction" -> 
            Switch[
                $OperatingSystem,
                "Windows",
                Function @ Select[{
                    Environment["ComSpec"],
                    FileNameJoin @ {
                        Environment["SystemRoot"],
                        "system32",
                        "WindowsPowerShell",
                        "v1.0",
                        "powershell.exe"
                    }},
                    FileExistsQ
                ],
                "MacOSX",
                Function @ Union[{$SystemShell}, FileNames["bash"|"sh"|"zsh", {"/bin", "/usr/bin", "/usr/local/bin", "/usr/local/sbin", "/usr/local/Cellar/bin"}]],
                "Unix",
                Function @ Union[{$SystemShell}, FileNames["bash"|"sh"|"zsh", {"/bin", "/usr/bin", "/usr/local/bin", "/usr/local/sbin"}]]
            ],
        "SerializationFunction" -> Identity,
        "DeserializationFunction" -> Identity
    |>
]

End[]
EndPackage[]
