

(*this evaluates both the system setting for session prolog/epilog as well as the user setting for the session if specified*)
runExternalEvaluateLog[session_, logType_]:=
    runExternalEvaluate[
        logType,
        session, {
            GetLanguageRules[session["System"], logType], 
            session[logType]
        }
    ]

$SessionUserKeys = {
    "System", 
    "Version", 
    "Kernel", 
    "Target", 
    "Executable", (* this is an alias for Target, deprecated in 12.2 *)
    "Prolog", 
    "SessionProlog",
    "Epilog", 
    "SessionEpilog",
    "ReturnType", 
    "Name"
};


compatibleVersion[system_, v1_String, v2_String] :=
    GetLanguageRules[system, "VersionSameQFunction"][
        v1,
        GetLanguageRules[system, "VersionStringConformFunction"][v2]
    ]
compatibleVersion[system_, v1_String, v2_] :=
    True
compatibleVersion[system_, v1_, v2_String] :=
    False
compatibleVersion[system_, v1_, v2_]:=
    True


(* Processors are executed in order, those are the rules used to normalize options *)

$SessionNormalizationRules = {

    (* first we check no extra keys are passed *)

    {} -> Function @ With[
        {extra = Complement[Keys @ #, $SessionUserKeys]},
        If[
            Length[extra] > 0,
            autofail[$Failed, "unknownOpts", extra],
            {}
        ]
    ],
    "System" -> 
        Function @ Replace[
            #System, {
                s_String :> s,
                any_ :> autofail[$Failed, "invalidOpt", "System", any]
            }
        ], 
    "Name" -> 
        Function @ Replace[
            #Name, {
                Automatic|None|_Missing|Default :> None,
                s_String :> s,
                any_ :> autofail[$Failed, "invalidOpt", "Name", any]
            }
        ],

    (* this rule is added to preserve backward compatibility with pre version 12.2, Target was called Executable *)

    "Target" -> 
        Function @ FirstCase[{#Target, #Executable}, Except[Automatic|None|_Missing|Default]],

    "Version" -> 
        Function @ Replace[
            FirstCase[{#Version, #Kernel}, Except[_Missing|None|Automatic]], {
                "*"|_Missing :> Automatic,
                s_String :> If[
                    MissingQ[#Target],
                    s,
                    autofail[$Failed, "novertar"]
                ],
                any_ :> autofail[$Failed, "invalidOpt", "Version", any]
            }
        ], 
    "Kernel" -> 
        Function @ #Version,

    {"Target", "Version"} -> 
        Function @ Replace[
            #Target, {
                (* If no evaluator was specified, we need to go ahead and filter all available evaluators *)
                Automatic|None|_Missing|Default :> 
                    decrypt @ Lookup[
                        SelectFirst[
                            getRegisteredInstallations[#System],
                            Function[
                                spec,
                                compatibleVersion[#System, spec["Version"], #Version]
                            ],
                            help[#System, $Failed, "noinstall", #System]
                        ],
                        {"Target", "Version"}
                    ],
                target_ :> {normalizeTarget[#System, target], Missing["NotAvailable"]}
            }
            
        ],

    (* we can now set other keys *)

    "Executable" ->
        Function @ #Target (* legacy name for Target *)

}

(* those are the rules used to actually start an already normalized session *)

safeRead[expr_] := Replace[expr, s_Symbol :> Missing[SymbolName[s]]]

$SessionInitilizationRules = {

    "SessionTime" -> 
        Function @ AbsoluteTime[],

    "EvaluationCount" -> 
        Function @ 0,

    "Active" -> 
        Function @ True,

    "UUID" -> 
        Function @ CreateUUID[],

    (* now we can start the process *)


    "StartupCommand" -> Function @ autofail @ GetLanguageRules[#System, "ScriptExecCommandFunction"][
        #Target,
        (*the program file could depend on the version, so call the function*)
        GetLanguageRules[#System, "ProgramFileFunction"][#Version],
        #UUID
    ],

    "Process" -> 
        Function @ Module[
            {process},

            (*to start the session, we use the script execution*)
        
            autofail @ GetLanguageRules[#System, "PreLaunchEvaluationFunction"][#Target];


            process = If[
                Or[StringQ[#StartupCommand], ListQ[#StartupCommand]],
                autofail @ StartProcess[
                    #StartupCommand,
                    ProcessDirectory->GetLanguageRules[#System, "ProcessDirectoryFunction"][],
                    ProcessEnvironment->GetLanguageRules[#System, "ProcessEnvironmentFunction"][]
                ],
                Null
            ];

            (*check the process to make sure it didn't fail to start for some reason*)
            If[
                ! Or[
                    process === Null,
                    ProcessStatus[process] === "Running"
                ],
                (*failed, some error so raise a message with the stderr stream and return $Failed*)
                help[
                    #System,
                    Failure[
                        "ProcessError", <|
                            "Command" -> #StartupCommand, 
                            "StandardError" -> safeRead @ ReadString[ProcessConnection[process, "StandardError"], EndOfBuffer],
                            "StandardOutput" -> safeRead @ ReadString[ProcessConnection[process, "StandardOutput"], EndOfBuffer]
                        |>
                    ],
                    "replFail",
                    #System,
                    #Target
                ]
            ];

            process
        ],

    "Socket" -> 
        Function @ Replace[
            GetLanguageRules[#System, "NonZMQInitializeFunction"], {

                (* the default behaviour is to start a ZMQ socket that is communicating with the main process *)

                Automatic|None|_Missing|Default :> Module[
                    {response, address},

                    response = TimeConstrained[safeRead @ ReadLine[#Process], 10]; (* we give the process a reasonable amount of time to print something *)

                    If[
                        StringQ[response],
                        address = First[
                            StringCases[
                                response, 
                                "tcp://" ~~ (WordCharacter | "." | ":") ..
                            ],
                            None
                        ]
                    ];

                    (*check the address returned from ReadLine to ensure that it's a string and we didn't hit EndOfFile*)
                    If[
                        Or[
                            response === EndOfFile,
                            Not @ StringQ[address]
                        ],
                        help[
                            #System,
                            WithCleanup[
                                Failure[
                                    "ProcessError", <|
                                        "Command" -> #StartupCommand, 
                                        "StandardError" -> safeRead @ ReadString[ProcessConnection[#Process, "StandardError"], EndOfBuffer],
                                        "StandardOutput" -> response
                                    |>
                                ],
                                Quiet @ KillProcess[#Process]
                            ],
                            "replFail",
                            #System,
                            #Target
                        ]
                    ];

                    autofail @ SocketConnect[address,"ZMQ_Pair"]

                ],

                (* in this case we need to use a custom function, we can just check that the function is not returning $Failed *)

                function_ :> autofail @ function[#Target, #Process, #UUID]
            }
        ]
}


(* 

    NormalizationFunction is a global function that is collecting all patterns that are not String and File
    and is dispatching them to the right System, the system can register patterns by defining NormalizationRules,
    If successful Normalization functions returns an association with System and Target.
*)


processEvaluationSelection[opts_?AssociationQ, extra__] :=
    applyProcessors[
        <|Thread[$SessionUserKeys -> None], opts|>,
        extra
    ]

processEvaluationSelection[opts_?AssociationQ] :=
    processEvaluationSelection[opts, $SessionNormalizationRules]

processEvaluationSelection[opts_, rest___] := 
    processEvaluationSelection[NormalizationFunction @ opts, rest]

StartExternalSession[opts_]:=

    Module[
        {session, options},

        (* we run the validation pipeline this is constructing rules one by one *)

        options = processEvaluationSelection[
            opts, 
            $SessionNormalizationRules,
            $SessionInitilizationRules
        ];

        (* now we safe the rules *)

        setSessionOpts[options["UUID"], options];

        (* then we construct the session object *)

        session = ExternalSessionObject[options["UUID"]];

        (* we run the session prolog once *)

        runExternalEvaluateLog[session, "SessionProlog"];

        (* we return the object *)

        session

    ]



HoldPattern[e:StartExternalSession[]] := returnUnevaluated @ System`Private`Arguments[e,2]
HoldPattern[e:StartExternalSession[_, _, __]] := returnUnevaluated @ System`Private`Arguments[e,2]