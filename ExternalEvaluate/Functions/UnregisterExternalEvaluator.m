


(*form for UnregisterExternalEvaluator that works with the uuid's returned from FindExternalEvaluators[] *)
UnregisterExternalEvaluator[uuid:_String|{___String}|All]:=
    updateInstallation[False, uuid]

UnregisterExternalEvaluator[lang:{___String}, All]:=
    updateInstallation[False, lang, All]

UnregisterExternalEvaluator[lang_?StringQ, exec_]:=
    updateInstallation[False, lang, exec]



(* 
    NormalizationFunction is a global function that is collecting all patterns that are not String and File
    and is dispatching them to the right System, the system can register patterns by defining NormalizationRules,
    If successful Normalization functions returns an association with System and Target.
*)

UnregisterExternalEvaluator[expr_] :=
    UnregisterExternalEvaluator @@ NormalizationFunction[expr]

HoldPattern[e:UnregisterExternalEvaluator[]] := returnUnevaluated @ System`Private`Arguments[e,{1,2}]
HoldPattern[e:UnregisterExternalEvaluator[_, _, __]] := returnUnevaluated @ System`Private`Arguments[e,{1,2}]
