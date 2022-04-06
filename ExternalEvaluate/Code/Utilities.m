

SetAttributes[checkAndSetCache, HoldRest]
checkAndSetCache[key_, value_, test_: Function[True]] := 
    With[
        {result = Internal`CheckCache[{checkAndSetCache, key}]},
        If[
            FailureQ[result] || ! test[result],
            With[
                {v = value}, 
                Internal`SetCache[{checkAndSetCache, key}, v]; 
                v
            ],
            result
        ]
    ]

applyProcessors[target_Association, rules_Association] := 
    applyProcessors[target, Normal[rules]]
applyProcessors[target_Association, rules___List] :=
    Module[
        {res = target},
        Apply[
            Function[
                {lhs, rhs},
                With[{r = rhs[res]}, res[[lhs]] = r];
            ],
            {rules},
            {2}
        ];
        res
    ]