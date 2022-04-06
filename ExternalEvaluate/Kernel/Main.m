
BeginPackage["ExternalEvaluate`"]

Begin["`Private`"]

(* First we unprotect all symbols *)

Unprotect @ Evaluate @ ExternalEvaluate`Private`autoloadSymbols

ClearAll @@ ExternalEvaluate`Private`autoloadSymbols

(* then we load all the code *)


Scan[
    Scan[
        Get,
        FileNames[
            "*.m", 
            FileNameJoin @ {ParentDirectory @ DirectoryName @ $InputFileName, #}
        ]
    ] &,
    {"Code", "Functions"}
]

(* this is automatically adding to all system functions the error handler *)

Scan[
    Function[
        values,
        Scan[
            Function[
                sym,
                values[sym] = Replace[
                    values[sym], {
                        head_[lhs_, rhs_] :> 
                            head[lhs, With[{r = protect[sym, rhs]}, r /; Not[SameQ[r, unevaluated]]]]
                    },
                    {1}
                ]
            ],
            Map[Symbol, ExternalEvaluate`Private`autoloadSymbols]
        ]
    ],
    {DownValues, UpValues}
]

(* this is automatically copying messages from ExternalEvaluate to all symbols *)

With[
    {messages = Messages[ExternalEvaluate]},
    Scan[
        Function[
            sym,
            Messages[sym] = ReplaceAll[messages, ExternalEvaluate -> sym]
        ],
        Select[
            Map[Symbol, ExternalEvaluate`Private`autoloadSymbols],
            And[
                Context[#] === "System`",
                # =!= ExternalEvaluate
            ] &
        ]
    ]
]

(* then we protect all symbols *)

Protect @ Evaluate @ ExternalEvaluate`Private`autoloadSymbols

End[] 

EndPackage[]