




makeCommandNormalizationFunction[spec_] := 
	With[
		{
			vname  = spec["StringTemplateVariableNameFunction"], 
			vid    = spec["StringTemplateVariableIdFunction"],
			rtypes = spec["ReturnTypes"]
		},
		{
			validators = <|
				"Arguments" -> {
					keymissing :> keymissing, 
					s_List :> s, 
					s_ :> {s}
				},
				"TemplateArguments" -> {
					keymissing :> {}
				},
				"Constants" -> {
					rule:_Rule|_RuleDelayed|{Repeated[_Rule|_RuleDelayed]} :> <|rule|>,
					assoc_Association?AssociationQ :> assoc,
					keymissing :> <||>,
					any_ :> autofail[$Failed, "keyinvalid", "Constants", any]
				},
				"Command" -> {
					s_String :> s,
					keymissing :> autofail[$Failed, "keyrequired", "Command"],
					any_ :> autofail[$Failed, "keyinvalid", "Command", any]
				},
				"ReturnType" -> {
					Automatic|_Missing|None|Null|keymissing -> ToLowerCase[First[rtypes]],
					s:rtypes :> ToLowerCase[s],
					any_ :> autofail[
						$Failed,
						"keyinvalidchoice",
						"ReturnType",
						any,
	                    Row[List @@ rtypes, ","]
					]
				}
			|>
		},
		{
			validatorskeys = Keys[validators]
		},
		Composition[
			Function[
				DeleteCases[
					AssociationThread[
						{"input", "args", "constants", "return_type"},
						Lookup[#, {"Command", "Arguments", "Constants", "ReturnType"}, keymissing]
					],
					keymissing
				]
			],
			If[
				TrueQ @ spec["StringTemplateSupportQ"],
				Function[
					If[
						StringMatchQ[#Command, Alternatives[
							___~~"<*" ~~ ___ ~~ "*>" ~~ ___,
							___~~"`" ~~ ___ ~~ "`" ~~ ___
						]],
						Append[#, convertStringTemplate[
							vname,
							vid,
							#["Command"],
							#["TemplateArguments"],
							#["Constants"]
						]],
						#
					]
				],
				Identity
			],
			Function[
				input,
				AssociationThread[
					validatorskeys,
					Apply[
						Replace,
						Transpose[Lookup[{input, validators}, validatorskeys, keymissing]],
						{1}
					]
				]
			]
		]
	]


convertStringTemplate[vname_, vid_, str_, arguments_, extra___] := 

	(* 
		if the input is a string template we need to replace all slots and expressions with variables.
		We use InsertionFunction to replace every slot with a variable name and we store the expressions
		in a Context variable.
		After TemplateApply runs the following will happen:
			
			Using: x = 2, y = 3
			In:    "<* x *> * 10 + <* y + 2 *> * 4"
			Out:   "wl_expr_1 * 10 +wl_expr_2 * 4"

		At the same time Reap is collecting a Context that looks like <|"wl_expr_1" -> 2, "wl_expr_2" -> 3 + 2|>

		The code is then executed using the following:
		<|"Command" -> "expr_1 * 10 + expr_2 * 4", "Constants" -> <|"wl_expr_1" -> 2, "wl_expr_2" -> 3 + 2|>|>
	 *)
	Module[
		{counter = 1},
		Replace[
			Reap[
				TemplateApply[
					autofail @ StringTemplate[
						str,
						arguments,
						InsertionFunction -> Function[
							With[
								{n = IntegerString[counter ++]},
								Sow[vid[n, #] -> #];
								vname[n, #]									
							]
						]
					]
				]
			],
			{res_, context_} :> <|"Command" -> res, "Constants" -> <|context, extra|>|>
		]
	]
	




$DefaultIcon := $DefaultIcon = Import[PacletManager`PacletResource["ExternalEvaluate", "Icon"]];

$LanguageOptions = <|
	"Icon" :> None,
	"IconCell" :> None,
	"ShowInFrontendCellQ" -> True,

	(* this is a temporary implementation until we figure out how to do it properly 
		in the meantime we need know if a system can handle this feature
	*)
	"StringTemplateSupportQ" -> False,
	"StringTemplateVariableNameFunction" -> Function["wl_expr_" <> #],
	"StringTemplateVariableIdFunction" -> Function["wl_expr_" <> #],

	(* 
		this allows to define a pattern that should be encrypted before saved to disk. 
		the pattern will be matched in every leaf of the registry entry, so don't specify things like "Password".
		sampleusage: _DatabaseReference 
	*)

	"SensitiveInformationPattern" -> Alternatives[],

	"TargetDiscoveryFunction" -> Function[{}],
	"TargetNormalizationRules" -> {
		s_String :> s,
		File[s_String] :> s
	},
	"TargetValidationFunction" -> Function @ And[
    	FileExistsQ[#],
    	! DirectoryQ[#]
    ],
    "NormalizeTargetExpressions" -> False,

	"DependencyTestFile" -> None,
	"ProgramFileFunction" -> Function[None],

	(*the default version conform function simply trims the string and deletes all non number/period characters*)
	"VersionStringConformFunction"->Function[
		Replace[#, s_String :> StringTrim @ StringDelete[s, Except["." | DigitCharacter]]]
	],

	(*to compare the versions, we compare the default conformed strings as a list by making them the same length*)
	"VersionSameQFunction"->Function[
		{execVersion, userVersion},
		With[
			{
				userSplit = StringTrim @ StringSplit[userVersion, "."],
				execSplit = StringTrim @ StringSplit[execVersion, "."]
			},
			Take[execSplit, UpTo[Length @ userSplit]] === userSplit
		]
	],

	(*the default version exec command just runs the executable with --version as the argument*)
	"VersionExecCommandFunction"->Function[{#, "--version"}],
	"VersionParserFunction"->Identity,

	(*this is the function used to create an environent using a package manager such as pip or npm.*)
	"CreateEnvironmentFunction" -> None,


	(*the default script exec function is just the executable and the file to execute*)
	"ScriptExecCommandFunction"->Function[{exec, file, uuid}, {exec, file}],
	"FileToCommandFunction" -> Function[<|"Command" -> Import[#, "Text"]|>],


	(* Returns Hyperlink if the tutorial for configuring the external evaluator exists, otherwise Null *)
	"TutorialLinkFunction" -> Automatic,

	(*the default epilog and prolog are None*)
	"DefaultEpilog"->None,
	"DefaultProlog"->None,

	(*these two options are for starting the external evaluator if different options need to be provided for some reason*)
	"ProcessEnvironmentFunction"->Function[Inherited],
	"ProcessDirectoryFunction"->Function[Inherited],
	"PreLaunchEvaluationFunction" -> Function[Null],

	"ReturnTypes" -> "Expression"|"String",

	(*session prolog / epilogs are for the entire session, i.e. always evaluated at the start of the session*)
	(*and always at the end of the session, but each only once per session*)
	(*mainly used for setting up custom types and for providing default imports available immediately, etc.*)
	"SessionProlog"->None,
	"SessionEpilog"->None,

	(*nonzmq evaluation function is used for systems such as webunit or excel that don't interact through zmq*)
	(*if this is something other than None, then a socket is not created for a session, and evaluation happens through*)
	(*this function*)
	"NonZMQEvaluationFunction" -> None,

	(*nonzmq initialize function is a function that is called to setup the system in addition to the process being started for a session*)
	"NonZMQInitializeFunction" -> None,

	(*same as the init function, but for deinit when the session is closed*)
	"NonZMQDeinitializeFunction" -> None,

	"SerializationFunction" -> Function[
		Developer`WriteRawJSONString[
			#,
			"ConversionRules" -> {_Missing | None -> Null},
			"Compact" -> True
		]
	],
	(* The input bytes must be utf8 encoded. *)
	"DeserializationFunction" -> Composition[Developer`ReadRawJSONString, ByteArrayToString],
	
    (* The handler for evaluating ExternalFunction[assoc][args]. The framework converts all calls to ExternalFunction into the fully-populated
       Association form, and then it evaluates that association using the supplied handler
    *)
    "ExternalFunctionHandler" -> Function[
        Replace[
            getExternalSession[#1], (* arg 1 is the Association of language rules *)
            session:_ExternalSessionObject|_String :> 
                ExternalEvaluate[
                    session, <|
                        "Command" -> Lookup[#1, "Command"], 
                        "Arguments" -> {##2} (* Args starting with position 2 are the sequence of args provided to the external function. *)
                    |>
                ]
        ]
    ],
    
    (* A pettern that will be tested against the arguments to ExternalFunction to see if the language wants its
       ExternalFunctionDownValueHandler to be invoked. See Java as an example of the use of this.
    *)
    "ExternalFunctionDownValuePattern" -> None,
    
    (* A function that is invoked to implement a DownValue for ExternalEvaluate, so that ExternalFunction["lang", args]
       can evaluate immediately if desired. See Java as an example of the use of this.
    *)
    "ExternalFunctionDownValueHandler" -> None,
    
    (* The handler for evaluating ExternalValue[system, var]. *)
    "ExternalValueGetter" -> Function[{systemNameOrSession, varName},
        If[StringQ[systemNameOrSession],
            Replace[
                getExternalSession[systemNameOrSession],
                session:_ExternalSessionObject|_String :> ExternalEvaluate[session, varName]
            ],
        (* else *)
            (* First arg is an ExternalSessionObject *)
            ExternalEvaluate[systemNameOrSession, varName]
        ]
    ],
    
    (* The handler for evaluating ExternalValue[system, var] = val. 
       By default, issue a message saying this feature is not supported for the given language.
    *)
    "ExternalValueSetter" -> Function[{systemNameOrSession, varName, val},
        Message[ExternalValue::lvalue, getSystemName[systemNameOrSession]];
        val
    ]
	
|>


toImage[spec_] := 
	SelectFirst[spec, MatchQ[Except[None|_Missing|Null|Automatic|_?FailureQ]]]
toImage[spec_, rules_] :=
	Replace[toImage[spec], rules]

makeTutorialFunction[lang_] := 
	Function[
		With[
			{link = "paclet:workflow/Configure" <> lang <> "ForExternalEvaluate"},
			If[
				Documentation`ResolveLink[link] === Null,
				Null,
				(*else*)
				If[
					Head[$FrontEnd] === FrontEndObject,
					Hyperlink[
						"Configure " <> lang <> " for ExternalEvaluate", 
						link,
						Appearance -> {Small, "Frameless"}
					],
					"http://reference.wolfram.com/language/workflow/Configure" <> lang <>"ForExternalEvaluate"
				]
			]
		]
	]

$Processors = {
	"TargetPattern" -> Function[
		Alternatives @@  #TargetNormalizationRules[[All, 1]]
	],
	"TargetNormalizationFunction" -> Function @ Replace[
		Append[#TargetNormalizationRules, t_ :> autofail[$Failed, "invalidTarget", t]]
	],

	"Icon" -> Function @ toImage[
		{#Icon, #IconCell, $DefaultIcon}
	],
	"IconCell" -> Function @ toImage[
		{#IconCell, #Icon, $DefaultIcon}, {
			img:_?ImageQ :> ToBoxes @ ImageResize[img, 20],
			img:_Graphics :> ToBoxes @ img
		}
	],
	"CommandNormalizationFunction" -> makeCommandNormalizationFunction,
	"DependencyTestFile" -> Function @ Flatten @ List @ ReplaceAll[
		#DependencyTestFile, {
			None|_Missing|Null|Automatic :> {},
			s_String :> s,
			File[s_] :> s
		}
	],
	"TutorialLinkFunction" -> Function[
		Replace[
			#TutorialLinkFunction, {
				Automatic :> makeTutorialFunction[#System],
				None|Null|_Missing :> Function[Null]
			}
		]
	]
}

applyProcessors[target_Association, rules_Association] := 
	applyProcessors[target, Normal[rules]]
applyProcessors[target_Association, rules___List] :=
	Module[
		{res = target},
		Apply[
			Function[
				{lhs, rhs},
				res[[lhs]] = rhs[res];
			],
			{rules},
			{2}
		];
		res
	]

ExternalEvaluate`RegisterSystem::usage = "ExternalEvaluate`RegisterSystem[lang,opts] adds heuristics/options for the specified system so that the system can be discovered and used with the ExternalEvaluate system."

(*we support the MergingFunction option for RegisterSystem when using parent / child options*)
Options[ExternalEvaluate`RegisterSystem] = {MergingFunction -> Last}

(*form that implements inheritance*)
ExternalEvaluate`RegisterSystem[lang_?StringQ, parent_?StringQ, sysopts_?AssociationQ, opts:OptionsPattern[]] := 
	ExternalEvaluate`RegisterSystem[
		lang,
		Merge[
			{GetLanguageRules[parent], sysopts},
			OptionValue[MergingFunction]
		]
	]

(*this will add options for the specified language so that external evaluate can find/use it*)
ExternalEvaluate`RegisterSystem[lang_?StringQ, sysopts_?AssociationQ, opts:OptionsPattern[]]:=
	$LanguageInformations[lang] = applyProcessors[
		<|"System" -> lang, $LanguageOptions, sysopts|>,
		$Processors
	]