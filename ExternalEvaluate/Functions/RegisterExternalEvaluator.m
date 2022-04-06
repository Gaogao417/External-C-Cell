

RegisterExternalEvaluator[All] := 
    updateInstallation[True, All]

RegisterExternalEvaluator[lang:_String, exec_]:= 
    updateInstallation[True, lang, exec]

(* 
    NormalizationFunction is a global function that is collecting all patterns that are not String and File
    and is dispatching them to the right System, the system can register patterns by defining NormalizationRules,
    If successful Normalization functions returns an association with System and Target.
*)

RegisterExternalEvaluator[expr:Except[_String|_File]] :=
    RegisterExternalEvaluator @@ NormalizationFunction[expr]

HoldPattern[e:RegisterExternalEvaluator[]] := returnUnevaluated @ System`Private`Arguments[e, 2]
HoldPattern[e:RegisterExternalEvaluator[_, _, __]] := returnUnevaluated @ System`Private`Arguments[e, 2]