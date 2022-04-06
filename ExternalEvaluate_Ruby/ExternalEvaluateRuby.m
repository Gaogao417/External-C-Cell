

BeginPackage["ExternalEvaluate`"]

Begin["`Ruby`"]

Needs["PacletManager`"]

ExternalEvaluate`RegisterSystem["Ruby",
	<|
        "StringTemplateSupportQ" -> True,
        "VersionStringConformFunction"->Function[
            First[StringCases[#, RegularExpression["[0-9\\.]+"]], Missing["NotAvailable"]]
        ],


        "TargetDiscoveryFunction" -> 
            Function @ FileNames[
                StringJoin["ruby", Switch[$OperatingSystem, "Windows", ".exe", _, ""]], 
                Switch[
                    $OperatingSystem,
                    "Windows",
                    Flatten @ {
                        (*versioned folder names*)
                        FileNames[FileNameJoin[{First[FileNameSplit[Environment["PROGRAMFILES"]]], "Ruby*x64", "bin"}]],
                        FileNames[FileNameJoin[{Environment["LOCALAPPDATA"], "Ruby*x64", "bin"}]],
                        FileNames[FileNameJoin[{Environment["PROGRAMFILES"], "Ruby*x64", "bin"}]]
                    },
                    "MacOSX", {
                        "/usr/bin",
                        "/usr/local/bin/",
                        "/usr/local/sbin",
                        "/usr/local/Cellar/bin",
                        FileNameJoin[{$HomeDirectory, ".rbenv/shims"}]
                    },
                    "Unix",
                    {"/usr/bin", "/usr/local/bin/", "/usr/local/sbin"}
                ]
            ],

		
		"ProgramFileFunction"->Function[PacletManager`PacletResource["ExternalEvaluate_Ruby", "REPL"]],
		
		"Icon"->Import[PacletManager`PacletResource["ExternalEvaluate_Ruby", "Icon"]],		
		"IconCell"->Import[PacletManager`PacletResource["ExternalEvaluate_Ruby", "IconCell"]],
		"DeserializationFunction" -> Function[
            Replace[
            	Developer`ReadRawJSONString[ByteArrayToString[#]], {
            		KeyValuePattern["output" -> expr_] :> expr,
					KeyValuePattern["Function"  -> expr_] :> ExternalFunction[expr],
            		KeyValuePattern[{"error"  -> expr_, "message" -> msg_}] :> 
            			Failure["RubyError", <|
            				"MessageTemplate" -> msg,
            				"MessageParameters" -> {},
            				"Traceback" -> Column @ expr
            			|>]
            	}
        	]
        ]
	|>
];


End[]
EndPackage[]
