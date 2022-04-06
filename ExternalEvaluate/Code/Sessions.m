


(*$Links tracks the actual socket objects we use to communicate with - note this is in memory only*)
If[
	! AssociationQ[$Links],
	$Links=<||>
];

(*no specific options will map over all known options*)

setSessionOpts[HoldPattern[ExternalSessionObject[uuid_String]], rest___] := setSessionOpts[uuid, rest]
setSessionOpts[session_String, key_String, value_] := $Links[session, key] = value
setSessionOpts[session_String, value_]  := $Links[session] = value
setSessionOpts[session_String, "EvaluationCount"] := $Links[session, "EvaluationCount"] ++

getSessionOpts[] := $Links
getSessionOpts[HoldPattern[ExternalSessionObject[uuid_String]], rest___] := getSessionOpts[uuid, rest]
getSessionOpts[session_String] := getSessionOpts[session, All]

(*a list of options just gets mapped over*)
getSessionOpts[session_String, options:{_String?StringQ...}]:=
	AssociationThread[options, getSessionOpts[session, #] & /@ options]
getSessionOpts[session_String, All] := 
	getSessionOpts[session, getSessionOpts[session, "Properties"]]

(*uuid is handled specially, as it's not a key in $Links[sessionUUID], it is the UUID used for that*)
getSessionOpts[session_String, "UUID"]   := session
getSessionOpts[session_String, "Exists"] := KeyExistsQ[$Links, session]
getSessionOpts[session_String, "Data"]   := $Links[session]
getSessionOpts[session_String, "Properties"]:=
	If[
		getSessionOpts[session, "Exists"],
		{
			"Active",
			"Epilog",
			"EvaluationCount",
			"Target",
			"Name",
			"Process",
			"ProcessMemory",
			"ProcessThreads",
			"Prolog",
			"ReturnType",
			"SessionEpilog",
			"SessionProlog",
			"SessionTime",
			"Socket",
			"System",
			"UUID",
			"Version"
		},
		{"Active", "UUID"}
	]

checkProcStats[item:HoldPattern[_String|_Quantity?QuantityQ|_?NumericQ|_Missing]] := item;
checkProcStats[item___] := Missing["NotAvailable"];

getSessionOpts[session_String, "ProcessMemory"] := checkProcStats[getSessionOpts[session, "Process"]["Memory"]]
getSessionOpts[session_String, "ProcessThreads"] := checkProcStats[getSessionOpts[session, "Process"]["Threads"]]

(*normal option getting function - looks up the specified option in $Links and returns it*)

getSessionOpts[session_String, "SessionTime"] :=
	Replace[
		getSessionOpts[session, "Data"], {
			data_Association :> 
				If[
					TrueQ @ data["Active"],
					(*session is running return a dynamically updating time*)
					Round[AbsoluteTime[] - data["SessionTime"]],
					(*session is dead, return static time*)
					Round[data["StopTime"] - data["SessionTime"]]
				],
			_ :> (
				emit["invalidSession", ExternalSessionObject[session]];
				$Failed
			)
		}
	]

getSessionOpts[session_String, "Active"] := Replace[
		getSessionOpts[session, "Data"], {
			data_Association :> TrueQ[Lookup[data, "Active", False]],
			_ -> False
		}
	]

getSessionOpts[session_String, option_String]:=
	Replace[
		getSessionOpts[session, "Data"], {
			data_Association :> 
				Lookup[data, option, Message[Options::optnf, option, ExternalSessionObject[session]]; $Failed],
			_ /; option === "Active" :> False,
			_ :> (
				emit["invalidSession", ExternalSessionObject[session]];
				$Failed
			)
		}
	]

getSessionOpts[session_String, option_] := (
	Message[Options::optnf, option, ExternalSessionObject[session]];
	$Failed
)


getSystemName[session_ExternalSessionObject] := session["System"]
getSystemName[systemName_String] := systemName