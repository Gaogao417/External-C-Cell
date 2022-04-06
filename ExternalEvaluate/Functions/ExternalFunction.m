(* Wolfram Language Package *)

(* Exported symbols added here with SymbolName::usage *)

System`ExternalFunction/:MakeBoxes[System`ExternalFunction[meta_Association /; AtomQ[Unevaluated @ meta] && AssociationQ[meta]],StandardForm|TraditionalForm]:= Block[
	{systembox = boxe[meta, "System"],
		argbox = boxe[meta, "Arguments"],
		builtinbox = boxe[meta, "BuiltIn"],
		cmd = Lookup[meta, "Command", None],
		visiblecmdbox, hiddencmdbox, visibleboxes, firsthiddenboxes, resthiddenboxes, sessionbox, sourcebox},

	visiblecmdbox = boxe["Command", shortCode[cmd]];
	hiddencmdbox = boxe["Command", makeScrollableCodePanel[cmd]];
	visibleboxes = Partition[
		{systembox, argbox, visiblecmdbox, builtinbox},
		UpTo[2]
	];
	If[Length[visibleboxes] == 1,
		(* displayed as a column *)
		visibleboxes = First[visibleboxes],
		visibleboxes = MapAt[Append[#, SpanFromLeft]&, visibleboxes, -1];
	];

	firsthiddenboxes = {systembox, argbox, hiddencmdbox, builtinbox};
	resthiddenboxes = KeyValueMap[
		Function[{key,val}, BoxForm`SummaryItem[{key<>": ", val}]],
		KeyDrop[meta, {"System", "Arguments", "Command", "BuiltIn", "Session", "Source"}]
	];
	sessionbox = BoxForm`SummaryItem[{"Session: ", Lookup[meta, "Session"]}];
	sourcebox = If[KeyExistsQ[meta, "Source"],
		BoxForm`SummaryItem[{"Source: ", makeScrollableCodePanel[Lookup[meta, "Source"]]}],
		Nothing
	];
	BoxForm`ArrangeSummaryBox[
		(*first argument is the head to use*)
		System`ExternalFunction,
		(*second argument is the expression*)
		System`ExternalFunction[meta],
		(*third argument is the icon to use*)
		None,
		(*the next argument is the always visisble properties*)
		visibleboxes,
		(*the next argument is the optional items that come down when the plus button is pressed*)
		Join[firsthiddenboxes, resthiddenboxes, {sourcebox, sessionbox}],
		(*lastly,the display form we want to display this with*)
		StandardForm,
		(*we use complete replacement to completely ignore the first set of displayed values*)
		(*with the second one when the button is clicked*)
		"CompleteReplacement"->True
	]
];

boxe[meta_Association, key_String] := boxe[key, Lookup[meta, key, None]]
boxe[name_String, None] := Nothing;
boxe[name_String, value_] := BoxForm`SummaryItem[{name<>": ", value}];

abbreviate[str_String] := If[StringLength[str] < 80,
	str,
	StringTake[str, UpTo[80]] <> "..."
];

shortCode[None] := None;
shortCode[str_] := Row[{
	Style[
		abbreviate[str],
		"Program",
		LineBreakWithin -> False,
		StripOnInput -> True
	]},
	ContentPadding -> False,
	FrameMargins -> 0,
	StripOnInput -> True,
	BaselinePosition -> Baseline,
	ImageSize->{{1,500},Medium}
];
(*this will make a nice monospaced, scrollable Panel type thing*)
(*that makes source code and stack traces display nicely*)
makeScrollableCodePanel[str_] := Framed[
	Pane[
		Style[
			str,
			"Program",
			LineBreakWithin->False,
			StripOnInput->True
		],
		(*we limit the max width to be 500, but only allow a short amount of vertical*)
		(*movement so it doesn't get gigantic*)
		ImageSize->{{1,500},Large},
		ContentPadding->False,
		FrameMargins->0,
		StripOnInput->True,
		BaselinePosition->Baseline
	],
	StripOnInput->True,
	Background->RGBColor[.94,.94,.94],
	FrameStyle->None,
	BaselinePosition->Baseline
];

(* This is the def that actually invokes the ExternalFunction on arguments. *)
ExternalFunction[assoc_Association?AssociationQ][args___] :=
    GetLanguageRules[assoc["System"], "ExternalFunctionHandler"][assoc, args]

(* These defs massage the arguments into the fully-fleshed out form *)

ExternalFunction[assoc:Except[KeyValuePattern["Session" -> _], _Association?AssociationQ]] := 
	ExternalFunction[Append[assoc, "Session" -> $DefaultSession]]

ExternalFunction[assoc_Association?AssociationQ, func_String] :=
	ExternalFunction[Append[assoc, "Command" -> func]]

ExternalFunction[session_ExternalSessionObject, func_String] :=
	ExternalFunction[<|"System" -> session["System"], "Session" -> session, "Command" -> func|>]

(* 
    NormalizationFunction is a global function that is collecting all patterns that are not String and File
    and is dispatching them to the right System, the system can register patterns by defining NormalizationRules,
    If successful Normalization functions returns an association with System and Target.
*)
ExternalFunction[expr:Except[_Association?AssociationQ], rest___] :=
	ExternalFunction[NormalizationFunction[expr], rest]
	
(* A definition for languages that need special DownValue rules on ExternalFunction so that it can evaluate immediately. *)
ExternalFunction[assoc_Association?AssociationQ] /; needsSpecialEFHandling[assoc] :=
    GetLanguageRules[assoc["System"], "ExternalFunctionDownValueHandler"][assoc]


needsSpecialEFHandling[assoc_] := 
    Module[{specialEFPattern},
        specialEFPattern = GetLanguageRules[assoc["System"], "ExternalFunctionDownValuePattern"];
        specialEFPattern =!= None && MatchQ[assoc, specialEFPattern]
    ]
