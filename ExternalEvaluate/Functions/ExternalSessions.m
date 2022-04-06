
ExternalSessions[] := 
	Map[
		ExternalSessionObject,
		Keys @ Select[getSessionOpts[], #Active &]
	]

ExternalSessions[lang_?StringQ] := (
	GetLanguageRules[lang]; (* this will raise an error if the lang in not known *)
	Map[
		ExternalSessionObject,
		Keys @ Select[getSessionOpts[], #Active && #System === lang &]
	]
)

HoldPattern[e:ExternalSessions[_, __]] := returnUnevaluated @ System`Private`Arguments[e,1]