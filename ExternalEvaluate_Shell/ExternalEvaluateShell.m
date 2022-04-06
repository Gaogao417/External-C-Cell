BeginPackage["ExternalEvaluateShell`"]

Begin["`Private`"]

Needs["PacletManager`"]

$Windows = $OperatingSystem === "Windows"

$UUID = "4e3775c88b494ec9a9b3fdc2e98aff20-"
$UUIDLength = StringLength[$UUID]
$UUIDCommand = StringJoin["echo ", $UUID, If[$Windows, "%ErrorLevel%-$?", "$?"]]
$WindowsPrompt = "074ad1caa43840278475b0e838427885>"
$NewLine = If[$Windows, "\r\n", "\n"]

writeLine[p:HoldPattern[_ProcessObject], rest___] := writeLine[ProcessConnection[p, "StandardInput"], rest]
writeLine[p:HoldPattern[_OutputStream], s_String] := WriteLine[p, s]

buffer[callback__][args___][rest___] := buffer[callback][args, rest]
buffer[callback__][EndOfLine, rest___] := buffer[callback][rest]
buffer[callback__][s__String, EndOfLine, rest___] := (
    Composition[callback, StringJoin][s]; 
    buffer[callback][rest]
);

flushStdout[p_] := flushStdout[p, Identity, Infinity]
flushStdout[p_, processor_] := flushStdout[p, processor, Infinity]
flushStdout[p_, processor_, timeout_] := 
    While[
        True,
        Replace[
            TimeConstrained[Read[ProcessConnection[p, "StandardOutput"], String], timeout], {
                s_String :> 
                    If[
                        processor[s];
                        StringStartsQ[s, $UUID],
                        Return[Null]
                    ],
                (* bug on windows. for some reason I cannot reproduce in a simple example why EndOfFile is returned by powershell 
                    even if the process is still running - riccardod
                *)
                EndOfFile :> 
                    If[
                        ProcessStatus[p] =!= "Running",
                        Return[Null]
                    ],
                $Aborted :> 
                    $Aborted,
                any_ :> 
                    Throw[{p, any}] (* debug code *)
            }
        ]
    ]

SetAttributes[collect, HoldFirst]

merge[key[s_String], rest___] := merge[s, rest]
merge[k:("StandardError"|"StandardOutput"), list_] := k -> StringTrim @ StringJoin @ Riffle[list, $NewLine]
merge[k:"ExitCode", {rest___, "0"|"True"}] := k -> 0
merge[k:"ExitCode", {rest___, "1"|"False"}] := k -> 1
merge[k:"ExitCode", {rest___, "$?"}] := merge[k, {rest}]
merge[k:"ExitCode", {___, s_String}] := k -> If[StringMatchQ[s, DigitCharacter..], FromDigits[s], s]

merge[k_, el_] := k -> Last[el]

collect[expr_] := 
    Replace[
        Association @ Quiet[Reap[expr; {}, _key, merge], {BinaryWrite::errfile}], {
            a:KeyValuePattern["ExitCode" -> 0] :> Success["ExecutionCompleted", a],
            a_ :> Failure["RuntimeError", a]
        }
    ]

write[p_, cmd_] := collect @ Block[
    {stderr, stdout, stderrcallback, stdoutcallback},

    (* 
        buffer will execute any callback when an EndOfLine is reached 
        every time a line is completed we want to do 2 operations:
        1. collect the line for the final result using Sow
        2. immediately print it in the frontend
    *)
                
    stderr = buffer[Print[Style[#, "Message", FontFamily -> "Source Code Pro"]] &, Sow[#, key["StandardError"]] &];
    stdout = buffer[Print, Sow[#, key["StandardOutput"]] &];

    (* 
        now we need to define 2 closures that are executed every time something is coming out from stdout/stderr.
        they are a bit different because stdout is always reading a full line, while stderr is always reading until the end of the buffer,
        for this reason they are debouncing the input in a different way.

        stdout 

        stderr also needs to filter out everything that starts with the prompt.
    *)
    
    stderrflush = Function[
        Replace[
            ReadString[ProcessConnection[p, "StandardError"], EndOfBuffer], {
            "" :> Null,
            s_String :> Set[stderr, Apply[stderr, StringSplit[s, $NewLine -> EndOfLine]]]
        }]
    ];


    stdoutcallback = Function[
        Which[
            ! StringQ[#],
            Null,
            And[
                $Windows,
                StringStartsQ[#, $WindowsPrompt|$UUIDCommand]
            ],
            Null,
            ! StringStartsQ[#, $UUID],
            stdout = stdout[#, EndOfLine],
            True,
            Scan[
                Sow[#, key["ExitCode"]] &,
                StringSplit[#, "-"][[2;;]]
            ]
        ];
        stderrflush[];
    ];
    

    
    (* we send the initial value so that Reap will collect keys in the right order *)
    Sow[cmd, key["Command"]];
    Sow["ProcessTerminated", key["ExitCode"]];
    Sow["", key["StandardError"]];
    Sow["", key["StandardOutput"]];

    
    (* we write the command *)
    
    writeLine[p, StringTrim[cmd]];

    (* we immediatily ask for the exit code, by prefixing it with a uuid *)
    
    writeLine[p, $UUIDCommand];

    (* 
        at this point we need to keep flushing the stderr until the command is completed. 
        every 0.1 seconds we are also flushing everything that is coming out from StandardOutput and print it in the notebook
    *)

    
    While[
        SameQ[
            flushStdout[p, stdoutcallback, 0.1], 
            $Aborted
        ],
        stderrflush[]
    ];
    
    stderrflush[];

    (* the buffer might contain a non flushed line, becase it was waiting for a newline, let's flush whatever it's left there *)
    
    stderr[EndOfLine];
    stdout[EndOfLine];

    
]

dispatch["expression", res_] := res
dispatch["association", res_] := Last[res]
dispatch["exitcode", res_] := res["ExitCode"]
dispatch["standardoutput", res_] := res["StandardOutput"]
dispatch["standarderror", res_] := res["StandardError"]

ExternalEvaluate`RegisterSystem[
    "Shell",
    <|

        "Icon" -> Import[PacletManager`PacletResource["ExternalEvaluate_Shell", "Icon"]],
        "IconCell" -> Import[PacletManager`PacletResource["ExternalEvaluate_Shell", "IconCell"]],
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
        "NonZMQInitializeFunction"-> Function[

            If[
                $Windows,
                writeLine[#2, "function prompt { return '" <> $WindowsPrompt <> "'} \n " <> $UUIDCommand];
                flushStdout[#2];
                ReadString[ProcessConnection[#2, "StandardError"], EndOfBuffer];
            ]

        ],
        "NonZMQEvaluationFunction"-> Function[
            {obj, msg},
            dispatch[
                msg["return_type"], 
                write[
                    obj["Process"], 
                    msg["input"]
                ]
            ]
        ],
        "ReturnTypes" -> "Expression"|"Association"|"ExitCode"|"StandardOutput"|"StandardError",
        "ScriptExecCommandFunction" -> 
            If[
                $Windows,
                Function[{exec, file, uuid}, {exec, "-noexit"}],
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
