



sessionProxy[session_ExternalSessionObject, ___][arg___] := session[arg]

(* message normalization *)

normalizeMessage[type:Except["Message"], session_, None|Null|_Missing, ___] :=
	Null


normalizeMessage[type_, session_, func_ -> args_List, rest___]:=
	normalizeMessage[type, session, <|"Command" -> func, "Arguments" -> args|>, rest]

normalizeMessage[type_, session_, func_ -> arg_, rest___]:=
	normalizeMessage[type, session, <|"Command" -> func, "Arguments" -> {arg}|>, rest]

normalizeMessage[type_, session_, func_ :> arg_, rest___]:=
	normalizeMessage[type, session, func -> arg, rest]

normalizeMessage[type_, session_, evals_List, rest___]:=
	normalizeMessage[type, session, #, rest]& /@ evals


(* the template cannot be used in the association form because it needs to use Context to work *)
normalizeMessage[type_, session_, input_String, rest___] := 
	normalizeMessage[type, session, <|"Command" -> input|>, rest]

(* handling special cases File, CloudObject, URL, LocalObject ..., they can all be used with Context and Arguments *)

normalizeMessage[type_, session_, assoc:KeyValuePattern["Command" -> File[file_?StringQ]], rest___] :=
	normalizeMessage[type, session, Append[assoc, autothrow[type, GetLanguageRules[session["System"], "FileToCommandFunction"][file]]], rest]

normalizeMessage[type_, session_, assoc:KeyValuePattern["Command" -> url:HoldPattern[_URL|_CloudObject|_HTTPRequest]], rest___] :=
	normalizeMessage[type, session, Append[assoc, "Command" -> autothrow[type, URLRead[url, "Body"]]], rest]

normalizeMessage[type_, session_, assoc:KeyValuePattern["Command" -> HoldPattern[lo_LocalObject]], rest___] :=
	normalizeMessage[type, session, Append[assoc, "Command" -> autothrow[type, Get[lo]]], rest]

(* rule delayed needs to be evaluated once *)

normalizeMessage[type_, session_, input:KeyValuePattern["Command" :> a_], rest___]:=
	normalizeMessage[type, session, Append[input, "Command" -> a], rest]






normalizeMessage[type_, session_, input:KeyValuePattern["Command" -> commands_List], rest___] :=
	normalizeMessage[type, session, Append[input, "Command" -> #], rest] & /@ commands

normalizeMessage[type_, session_, input:KeyValuePattern["Command" -> command:_Association?AssociationQ|_Rule|_RuleDelayed], rest___] :=
	normalizeMessage[type, session, Append[input, command], rest]

normalizeMessage[type_, session_, input_Association?AssociationQ, handler_:Identity] :=
	handler @ GetLanguageRules[session["System"], "CommandNormalizationFunction"][<|"ReturnType" -> session["ReturnType"], input|>]


normalizeMessage[type_, session_, input_, rest___] := 
	normalizeMessage[type, session, <|"Command" -> input|>, rest]

(* basic error handling and propagation *)

SetAttributes[{errorHandler}, HoldAllComplete]

autothrow["Message", error:_Failure|$Failed, ___] := autofail[error]
autothrow[type_, error:_Failure|$Failed, ___] := emit["sessionfailure", type, error]
autothrow[type_, error_, ___] := error

errorHandler[session_, code_] :=
    Replace[
        CheckAll[code, HoldComplete], {
            (* everything was fine, we return the result*)
            _[res_, Hold[]] :> 
                res,

            (* Abort[] or Throw should just be propagated. in any case the error was 
               thrown in the middle of an IO operation, we cannot trust the process 
               to work on the next evaluation. we must 🔪 the process.
            *)
            _[res_, Hold[Abort[]]] :> (
            	DeleteObject @ session;
            	autofail[$Aborted]
            ),
            _[res_, Hold[throw_]] :> (
            	DeleteObject @ session;
            	throw
            )
        }
    ]


(*
	the loop writes the message and keeps waiting for messages until the response is NOT ExternalEvaluateKeepListening.
	python might be able to redirect stdout, to do that it will be sending a side effect in the form of
	ExternalEvaluateKeepListening[Print["foo"]], the side effect is evaluated immediately.
	The final result of the computation is not wrapped in ExternalEvaluateKeepListening and loop ends.
*)


zmqSocketWrite := zmqSocketWrite = (
	Needs["ZeroMQLink`"]; (* lazy loading ZMQ *)
	ZeroMQLink`ZMQSocketWriteMessage
)

zmqread[session_, callback_] := (
	While[
		Not @ SocketReadyQ[session["Socket"], 0.01],
		callback @ ReadString[session["Process"], EndOfBuffer]
	];
	SocketReadMessage[session["Socket"]]
)

zmqwrite[session_, bytes_String] := zmqwrite[session, StringToByteArray[bytes]];
zmqwrite[session_, bytes_ByteArray] := zmqSocketWrite[session["Socket"], bytes];

runExternalEvaluate[type_, session_ExternalSessionObject, rest___] :=
	runExternalEvaluate[type, sessionProxy[session], rest]
runExternalEvaluate[type_, sessions_sessionProxy, input_]:=
	Apply[
		Function[
			{struct, messages},
			ReplaceAll[
				struct,
				externalEvaluateLink[type, sessions, First[messages, {}]]
			]
		],
		Module[
			{i = 0},
			Reap[normalizeMessage[type, sessions, input, Function[First @ Sow[++i -> #, tag]]], tag]
		]
	]

processorFactory[sessionProxy[s_]] := processorFactory[s]
processorFactory[sessionProxy[s__]] := With[
	{functions = AssociationMap[processorFactory, {s}]},
	functions[#2][##] &
]
processorFactory[session_ExternalSessionObject] := 
	processorFactory[
		normalizeMessage["Prolog", session, session["Prolog"]], 
		normalizeMessage["Epilog", session, session["Epilog"]]
	]


processorFactory[prolog_, epilog_] := <|"Prolog" -> prolog, #1, "Epilog" -> epilog|> &
processorFactory[Null, epilog_]    := <|#1, "Epilog" -> epilog|> &
processorFactory[Null, Null]       := <|#1|> &
processorFactory[prolog_, Null]    := <|"Prolog" -> prolog, #1|> &
	


externalEvaluateLink[_, _, {}] := {}

externalEvaluateLink[type_, sessionProxy[s_], rest___] := 
	externalEvaluateLink[type, s, rest]

externalEvaluateLink["Message", session_, messages_] := 
	externalEvaluateLink["Message", session, messages, processorFactory[session]]

externalEvaluateLink[type_, session_, messages_] := 
	externalEvaluateLink[type, session, messages, <|type -> Last[#]|> &]

externalEvaluateLink[type_, session_, messages_, processor_] := 
	Block[
		(* 
			all external functions are evaluated in the kernel 
			this global variable is used to set a default kernel session
		*)
		{$DefaultSession = session, system = session["System"]},
		If[
			GetLanguageRules[system, "NonZMQEvaluationFunction"] === None,
			externalEvaluateLink[
				type,
				session, 
				messages, 
				processor,
				GetLanguageRules[system, "SerializationFunction"], 
				GetLanguageRules[system, "DeserializationFunction"]
			],
			externalEvaluateNonZMQLink[
				type, session, messages, GetLanguageRules[system, "NonZMQEvaluationFunction"], processor
			]
		]
	]


buffer[args___][rest___] := buffer[args, rest]
buffer[s__String, EndOfLine, rest___] := (Print @ StringJoin @ s; buffer[rest]);

createStdoutCallback[] := With[
	{u = Unique[]},
	u = buffer[];
	Replace @ {
		"" :> Null,
		s_String :> Set[u, u @@ StringSplit[s, "\n" -> EndOfLine]],
		EndOfFile :> (u[EndOfLine]; ClearAll[u])
	}
]

externalEvaluateLink[type_, session_, messages_, processor_, serializer_, deserializer_] :=
	externalEvaluateLink[
		type,
		session,
		messages,
		processor,
		serializer,
		deserializer,
		createStdoutCallback[]
	]

externalEvaluateLink[type_, session_, messages_, processor_, serializer_, deserializer_, stdoutcallback_] :=
	Association @ Reap @ readAll[
		session -> writeMessage[
			session, 
			messages, 
			processor, 
			serializer, 
			deserializer
		],
		processor, 
		serializer, 
		deserializer,
		stdoutcallback
	]

reorderSessions[{session_ -> Null, rest__}] := 
	{session, Null, {rest}}
reorderSessions[sessions_List] := reorderSessions @ <|sessions|>
reorderSessions[sessions_Association] := 
	Module[
		{assoc = AssociationMap[First[#]["Socket"] -> First[#] &, sessions], socket},
		socket = First @ SocketWaitNext[Keys[assoc]];
		{assoc[socket], sessions[assoc[socket]], sessions}
	]

externalEvaluateLink[type_, sessionProxy[initials___], messages_, processor_, serializer_, deserializer_, stdoutcallback_] := 
	Module[
		{session, rest, queue, sessions = Map[# -> Null &, {initials}]},
		Association @ Reap[
			Scan[
				Function[
					{session, queue, rest} = reorderSessions[sessions];
					readAll[{session, queue}, processor, serializer, deserializer, stdoutcallback];
					queue    = writeMessage[session, #, processor, serializer, deserializer];
					sessions = Append[rest, session -> queue];
				],
				messages
			];
			Scan[
				readAll[#, processor, serializer, deserializer, stdoutcallback] &,
				Normal @ sessions
			];
			{}
		]
	]

writeMessage[session_, messages_List, rest___] := 
	Join @@ Map[writeMessage[session, #, rest] &, messages]
writeMessage[session_, message_Rule, processor_, serializer_, deserializer_] := 
	errorHandler[
		session,
		With[
			{messages = processor[message, session]},
			setSessionOpts[session, "EvaluationCount"];
			Scan[
				zmqwrite[session, serializer[#]] &,
				Values[messages]
			];
			Keys[messages]
		]
	]

sow[type_String, error:_Failure|$Failed] := (emit["sessionfailure", type, error]; error)
sow[type_String, result_] := Null
sow[id_, result_] := Sow[id -> result, tag]

readAll[_[session_, values_List], processor_, serializer_, deserializer_, stdoutcallback_] := 
	errorHandler[
		session,
		Module[
			{result},
			Scan[
				Function[
					id,
					result = ExternalEvaluateKeepListening[];
					While[
						MatchQ[result, _ExternalEvaluateKeepListening|_PythonKeepListening],
						result = deserializer @ zmqread[session, stdoutcallback]
					];
					sow[id, result]
				],
				values
			];
			stdoutcallback @ ReadString[session["Process"], EndOfBuffer];
			stdoutcallback @ EndOfFile;
			{}
		]
	]
readAll[_[_, Null], ___] := Null
readAll[___] := autofail @ $Failed

externalEvaluateNonZMQLink[type_, sessionProxy[__], ___] := $Failed
externalEvaluateNonZMQLink[type_, sessionProxy[s_], rest___] := externalEvaluateNonZMQLink[type, s, rest]
externalEvaluateNonZMQLink[type_, session_ExternalSessionObject, messages_, function_, processor_] :=
	Association @ Reap[
		Scan[
			Function[
				{message},
				KeyValueMap[
					sow[#1, function[session, #2]] &,
					processor[message, session]
				]
			],
			messages
		];
		{},
		tag
	]

ExternalEvaluate[sessions_sessionProxy, input_] :=
	If[
		AllTrue[sessions, #["Active"] &],
		runExternalEvaluate[
			"Message",
			sessions, 
			input
		],
		Scan[
			If[
				Not @ #["Active"],
				emit["invalidSession", #]
			] &,
			sessions
		];
		$Failed
	]

ExternalEvaluate[s_ExternalSessionObject, input_] := ExternalEvaluate[sessionProxy[s], input]
ExternalEvaluate[{s__ExternalSessionObject}, input_] := ExternalEvaluate[sessionProxy[s], input]

(* 
    The next downvalue will make spec evaluate inside StartExternalSession.
    StartExternalSession might raise a ReturnUnevaluated if appropriate.
*)

ExternalEvaluate[spec_, input_]:=
	Replace[
		StartExternalSession[spec], 
		session_ExternalSessionObject :> With[
			(*evaluate all the input on the session*)
			{res = ExternalEvaluate[session, input]},
			(* now close the link and return the result, the session might have been aborted *)
			Quiet[DeleteObject[session]];
			res
		]
	]

(*operator form*)
ExternalEvaluate[spec_][input_]:= ExternalEvaluate[spec, input]

HoldPattern[e:ExternalEvaluate[]] := returnUnevaluated @ System`Private`Arguments[e,{1,2}]
HoldPattern[e:ExternalEvaluate[_, _, __]] := returnUnevaluated @ System`Private`Arguments[e,{1,2}]
