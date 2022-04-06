
(* UPVALUES *)

(*DeleteObject stops the process, closes the socket, and then deletes the session from $Links*)
ExternalSessionObject /: HoldPattern[DeleteObject][session:ExternalSessionObject[sessionUUID_String]] := 
	If[
		getSessionOpts[session, "Active"],
		(*the session exists, so we can close the socket, kill the process and remove it from the list of links*)
		(*first check if there's a SessionEpilog we need to evaluate*)
		runExternalEvaluateLog[session, "SessionEpilog"];
		(* then we close the socket *)
		Replace[session["Socket"], s:HoldPattern[_SocketObject] :> Close[s]];

		(* Worst case scenario this will do None[session, process] *)
		GetLanguageRules[
			session["System"],
			"NonZMQDeinitializeFunction"
		][
				session["Target"],
				session["Process"],
				session["Socket"],
				session["UUID"]
		];
		KillProcess[session["Process"]];
		(*mark this session as inactive*)
		setSessionOpts[sessionUUID, "StopTime", AbsoluteTime[]];
		setSessionOpts[sessionUUID, "Active", False];
		(*finally if the epilog failed to evaluate and returned a Failure then return that to the user as well*)
		Null,
		(*doesn't exist, so issue message*)
		autofail[$Failed, "invalidSession", session]
	]

(*callable form for getting options directly by calling the object session[...]*)
ExternalSessionObject /: HoldPattern[session:ExternalSessionObject[_?StringQ]][rest___] := getSessionOpts[session,rest]

(*same thing, but when using Options[session,...]*)
ExternalSessionObject /: HoldPattern[Options][session:ExternalSessionObject[_?StringQ],rest___] := getSessionOpts[session,rest]



(* BOXES *)


SetAttributes[dynamicField, HoldAllComplete];

dynamicField[uuid_String, field_, default_:Missing["NotAvailable"]] := Dynamic[
	If[
		TrueQ @ getSessionOpts[uuid, "Exists"],
		field,
		default
	],
	TrackedSymbols :> {$Links}
];


visibleBoxes[session_] := 
	visibleBoxes[session, If[StringQ[session["Kernel"]], "Kernel", "Version"]]
visibleBoxes[session:ExternalSessionObject[uuid_String], ver_String] := {
	{
		BoxForm`SummaryItem @ {
			"System: ",
			session["System"]
		},
		Replace[
			session["Version"], {
				_?FailureQ|_Missing :> Nothing,
				v_ :> BoxForm`SummaryItem @ {ver <> ": ", v}
			}
		]
	}, {
		BoxForm`SummaryItem @ Replace[
			session["Name"], {
				_Missing|None|Null :> {"UUID: ", session["UUID"]},
				name_ :> {"Name: ", name}
			}
		]
	}
}

ExternalSessionObject /: MakeBoxes[session:ExternalSessionObject[uuid_String?StringQ] /; session["Exists"], f:StandardForm|TraditionalForm]:=

	BoxForm`ArrangeSummaryBox[
		(*first argument is the head to use*)
		ExternalSessionObject,
		(*second argument is the expression*)
		session,
		(*third argument is the icon to use*)
		Show[
			GetLanguageRules[session["System"], "Icon"], 
			ImageSize -> Dynamic[{Automatic, 3.5 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}]
		],
		(*the next argument is the always visisble properties*)
		Insert[visibleBoxes[session], SpanFromLeft, {-1, -1}],
		(*the next argument is the optional items that come down when the plus button is pressed*)
		Join[
			Flatten[visibleBoxes[session]],
			KeyValueMap[
				BoxForm`SummaryItem @ {ToString[#1] <> ": ", #2} &,
				<|
					"Active" -> dynamicField[uuid, session["Active"], False],
					"Target" -> session["Target"],
					"UUID" -> session["UUID"],
					DeleteCases[
						session[
							Complement[
								session["Properties"], {
									"Active",
									"SessionTime",
									"EvaluationCount",
									"ProcessMemory",
									"ProcessThreads",
									"System",
									"Name",
									"Target",
									"Version",
									"EvaluationCount"
								}
							]
						],
						_Missing|None|Null
					],
					"EvaluationCount"-> dynamicField[uuid, session["EvaluationCount"], None],
					"ProcessMemory"  -> dynamicField[uuid, Refresh[session["ProcessMemory"],  UpdateInterval -> 5]],
					"ProcessThreads" -> dynamicField[uuid, Refresh[session["ProcessThreads"], UpdateInterval -> 5]],
					"SessionTime"    -> dynamicField[uuid, Refresh[session["SessionTime"],    UpdateInterval -> 1]]
				|>
			]
		],
		(*lastly,the display form we want to display this with*)
		f,
		(*we want to completely replace the output to be a single column when the plus button is clicked*)
		"CompleteReplacement"->True
	]




