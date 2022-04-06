




getExtension[All, rest___] :=
    getExtension[
        DeleteDuplicates[
            PacletFind["*", <|"Extension" -> "ExternalEvaluate"|>], 
            Function[
                {lhs, rhs},
                Or[
                    rhs["Name"] === lhs["Name"],
                    rhs["Name"] === "ExternalEvaluate_WebDriver", (* we will remove those 2 lines during the next major release 12.4 / 13.0 *)
                    lhs["Name"] === "ExternalEvaluate_WebDriver"
                ]
            ]
        ],
        rest
    ]
getExtension[paclet_List, rest___] := 
    Map[getExtension[#, rest] &, paclet]
getExtension[paclet_String, rest___] := 
    getExtension[PacletObject[paclet], rest]
getExtension[paclet_PacletObject, default_ : Null] := 
    First[Cases[paclet["Extensions"], {"ExternalEvaluate", rules___} -> Association["Paclet" -> paclet["Name"], rules]], default]

(*

    $LanguageInformations contains all info of all systems.
    There are some patterns to build to construct utilities that framework is using to dispatch the right target to the right system 
    (example DatabaseReference should be used by SQL).

    To do that we need to load all systems.

    
$LanguageInformations is a mapping containing the system name -> paclet name.
    $LanguageInformations is where RegisterExternalSystem is storing system informations.

    When we first access the symbol, we first need to initialize it to a blank association, then we load all the code in all paclets.
    Get will perform a side effect by in turn calling RegisterExternalSystem and populate the system in the assoc.

    Once all systems are loaded we can return the registry.
 *)

$LanguagePacletOptions = <|
    "TargetAllowedHeads" -> {}, 
    "Ordering" -> Automatic, 
    "ShowInFrontendCellQ" -> True,
    "Context" -> {},
    "Icon" -> None,
    "IconCell" -> None,
    "Paclet" -> None,
    "System" -> None,
    "Loaded" -> False
|>


SetAttributes[toImage, HoldRest]

toImage[] := None
toImage[None|_Missing|Null|Automatic|_?FailureQ, rest___] :=
    toImage[rest]
toImage[(Hold|HoldComplete)[spec___], rest___] := 
    toImage[spec, rest]
toImage[spec:_String|_File, rest___] := 
    toImage[Import[spec], rest]
toImage[spec_, ___] :=
    spec

$LanguagePacletProcessors = <|
    "TargetAllowedHeads" -> Function[
        Alternatives @@ ToExpression[
            Flatten @ #TargetAllowedHeads,
            InputForm,
            Function[sym, HoldPattern[Blank[sym]], HoldFirst]
        ]
    ],
    "Icon" -> Function[
        Hold @ checkAndSetCache[
            {#System, "Icon"},
            toImage[
                #Icon, 
                #IconCell, 
                PacletManager`PacletResource[#Paclet, "Icon"], 
                PacletManager`PacletResource[#Paclet, "IconCell"], 
                PacletManager`PacletResource["ExternalEvaluate", "Icon"]
            ]
        ]
    ],
    "IconCell" -> Function[
        Hold @ checkAndSetCache[
            {#System, "IconCell"},
            Replace[
                toImage[
                    #IconCell, 
                    PacletManager`PacletResource[#Paclet, "IconCell"], 
                    PacletManager`PacletResource["ExternalEvaluate", "IconCell"], 
                    #Icon
                ], {
                    img:_?ImageQ :> ToBoxes @ ImageResize[img, 20],
                    img:_Graphics :> ToBoxes @ img
                }
            ]
        ]
    ]
|>



normalizeLanguageInfo[name_String, rules___] := 
    name -> applyProcessors[<|
            $LanguagePacletOptions,
            rules, 
            "System" -> name
        |>,
        $LanguagePacletProcessors
    ]
normalizeLanguageInfo[(h:Rule|RuleDelayed)[name_, info_], rules___] := normalizeLanguageInfo[name, info, rules]


$LanguageInformations := $LanguageInformations = 
    SortBy[
        Association @ Replace[
            getExtension[All], {
                e:KeyValuePattern[_["System", any_String]] :> 
                    normalizeLanguageInfo[any, rest],
                e:KeyValuePattern[_["System", any_List]]   :> 
                    Map[normalizeLanguageInfo[#, e] &, any],
                e:KeyValuePattern[_["System", any:_Rule|_RuleDelayed]]   :> 
                    normalizeLanguageInfo[any, e],
                e:KeyValuePattern[_["System", any:_Association]]  :> 
                    Map[normalizeLanguageInfo[#, e] &, Normal[any]]
            },
            {1}
        ],
        {Key["Ordering"], Key["System"]}
    ]

(* 
    this function is responsible for getting language information that are registered when a paclet is loaded.
    
$LanguageInformations contains information about local and remote paclets, the first time a paclet is loaded
    we are checking remotely if there is an update, otherwise we are just loading the code and returning the assoc
    registered by RegisterSystem
*)

GetLanguageRules[] := Keys[$LanguageInformations]

GetLanguageRules[All, rest___] := GetLanguageRules[GetLanguageRules[], rest]

GetLanguageRules[s_List, rest___] := AssociationMap[GetLanguageRules[#, rest] &, s]

GetLanguageRules[s_String, k: (Alternatives @@ Keys[$LanguagePacletOptions])] := 
    If[
        KeyExistsQ[$LanguageInformations, s],
        $LanguageInformations[[s, k]],
        autofail[$Failed, "unknownSys", s]
    ]

GetLanguageRules[s_String, prop_String] := 
    Which[
        TrueQ[$LanguageInformations[[s, "Loaded"]]],
        $LanguageInformations[[s, prop]],
        KeyExistsQ[$LanguageInformations, s],
        Block[
            {$ContextPath = $ContextPath},
            $LanguageInformations[[s, "Loaded"]] = True;
            Scan[
                Get, 
                Flatten @ List @ GetLanguageRules[s, "Context"]
            ];
            GetLanguageRules[s, prop]
        ],
        True,
        autofail[$Failed, "unknownSys", s]
    ]

GetLanguageRules[s_String] := (
    GetLanguageRules[s, "ReturnTypes"]; (* need to trigger the loading, any non paclet property is fine *)
    $LanguageInformations[[s]]
)



guard[GetLanguageRules]

(* normalization function is used to dispatch arbitrary expressions to the right evaluator *)

NormalizationFunction := 
    NormalizationFunction = Replace @ Flatten @ {

        (* file and string here are simply not accepted *)
        
        _File :> 
            returnUnevaluated[$Failed], 
        system_String :> 
            <|"System" -> system|>,
        {opts:OptionsPattern[]|_Association?AssociationQ} :> 
            <|opts|>,
        {system_, opts:OptionsPattern[]|_Association?AssociationQ} :> 
            <|NormalizationFunction[system], opts|>,

        {system_, target:_File|_String, opts:OptionsPattern[]|_Association?AssociationQ} :> 
            <|"System" -> system, "Target" -> target, opts|>,

        {system_, target_, opts:OptionsPattern[]|_Association?AssociationQ} :> 
            <|NormalizationFunction[target], "System" -> system, opts|>,

        (h:Rule|RuleDelayed)[system_, type_] :> 
            <|NormalizationFunction[system], h["ReturnType", type]|>,

        (* 
            first we collect all normalization rules for all systems 
            we are using association to delete duplicates, only the last one will match,
            we need to make sure the same pattern is not registed in two different evaluators.

            those patterns might contain String and File, but file is already handled as the first case, and String
            is normally handled as System by ExternalEvaluate and StartExternalSession
        *)

        KeyValueMap[
            Function[
                {system, pattern},
                If[
                    Length[pattern] > 0,
                    expr:pattern :> <|
                        "System" -> system,
                        "Target" -> GetLanguageRules[system, "TargetNormalizationFunction"][expr]
                    |>,
                    {}
                ]
            ],
            GetLanguageRules[All, "TargetAllowedHeads"]
        ],
        (* if nothing is matching we can raise an error *)
        s_ :> returnUnevaluated[$Failed, "unknownSys", s]
    }
