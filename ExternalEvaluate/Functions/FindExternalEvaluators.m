
Options[FindExternalEvaluators] = {"ResetCache"->False}

(*external evaluators returns a dataset for the given language of all the installations it knows about for that language*)
FindExternalEvaluators[lang:_String|{___String}|All:All, opts:OptionsPattern[]]:= (
    If[TrueQ[OptionValue["ResetCache"]], cleanupRegistry[lang]];
    Dataset[
        decrypt @ getAllInstallations[lang],
        HiddenItems -> {"Executable"}
    ]
)


HoldPattern[e:FindExternalEvaluators[_, __]] := returnUnevaluated @ System`Private`Arguments[e, 1]