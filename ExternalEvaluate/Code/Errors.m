

$CurrentSymbol = ExternalEvaluate

SetAttributes[{autofail, help, protect}, HoldRest]
SetAttributes[emit, HoldFirst]

emit[] := Null
emit[message_String, rest___] := With[{m = $CurrentSymbol}, emit[MessageName[m, message], rest]]
emit[message_MessageName, rest___] := Message[message, rest]
emit[message_, rest___] := emit @@ {message, rest}

autofail[expr_?FailureQ, rest___] := (emit[rest]; Throw[expr, "__externalevaluate__"])
autofail[expr_, ___] := expr

returnUnevaluated[expr_, rest___] := (emit[rest]; Throw[expr; unevaluated, "__externalevaluate__"])

(* protect is automatically injected at the end of code loading, see ExternalEvaluate/Main.m for details *)

protect[symbol_, expr_] := 
    If[
        Context[symbol] === "System`", 
        Block[
            {protect = Function[{s, e}, e, HoldAll], $CurrentSymbol = symbol}, 
            Catch[expr, "__externalevaluate__"]
        ], 
        Catch[expr, "__externalevaluate__"]
    ]

help[system_, rest___] := 
    Replace[
        GetLanguageRules[system, "TutorialLinkFunction"][], {
            Null :> autofail[rest],
            link_ :> (
                emit["help", system, link];
                autofail[rest]
            )
        }
    ]

