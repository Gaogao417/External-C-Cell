
(* Get an external value. *)
ExternalValue[system_String, symbol_String] := 
    GetLanguageRules[system, "ExternalValueGetter"][system, symbol]

ExternalValue[session_ExternalSessionObject, symbol_String] := 
    GetLanguageRules[session["System"], "ExternalValueGetter"][session, symbol]
     
HoldPattern[e:ExternalValue[]] := returnUnevaluated @ System`Private`Arguments[e,2]
HoldPattern[e:ExternalValue[_, _, __]] := returnUnevaluated @ System`Private`Arguments[e,2]


(***********************  l-value support  *************************)

ExternalValue /: (ExternalValue[systemNameOrSession:(_String | _ExternalSessionObject), varName_] = val_) :=
                      GetLanguageRules[getSystemName[systemNameOrSession], "ExternalValueSetter"][systemNameOrSession, varName, val]

