
Needs["PacletManager`"]

$ordering = <|"Python" -> 1, "R" -> 2, "Julia" -> 3, "Octave" -> 4, "Java" -> 5, "Ruby" -> 6, "NodeJS" -> 7, "Shell" -> 8, "SQL" -> 9, "SQL-JDBC" -> 10|>

getExtension[paclet_List, rest___] := 
    Map[getExtension[#, rest] &, paclet]
getExtension[paclet_String, name_] := 
    getExtension[PacletObject[paclet], name]
getExtension[paclet_PacletObject, name_, default_ : Null] := 
    First[Cases[paclet["Extensions"], {"ExternalEvaluate", ___, name -> e_, ___} ->e], default]

extractLanguages[paclet_List] := 
    Map[extractLanguages, paclet]

extractLanguages[paclet_] := 
    extractLanguages[paclet, getExtension[paclet, "Language"]]

extractLanguages[paclet_, s_List] := 
    Map[extractLanguages[paclet, #] &, s]

extractLanguages[paclet_, _Missing|Null] :=
    extractLanguages[
        paclet, 
        Replace[
            StringSplit[paclet["Name"], "ExternalEvaluate_", 2], {
                {_, name_} :> name, 
                _ -> {}
            }
        ]
    ]

extractLanguages[paclet_, s_String] :=
    s -> paclet["Name"]

(*

    $LanguageInformations contains all info of all systems.
    There are some patterns to build to construct utilities that framework is using to dispatch the right target to the right system 
    (example DatabaseReference should be used by SQL).

    To do that we need to load all systems.

    $AvailableLanguages is a mapping containing the system name -> paclet name.
    $LanguageInformations is where RegisterExternalSystem is storing system informations.

    When we first access the symbol, we first need to initialize it to a blank association, then we load all the code in all paclets.
    Get will perform a side effect by in turn calling RegisterExternalSystem and populate the system in the assoc.

    Once all systems are loaded we can return the registry.
 *)

$AvailableLanguages := $AvailableLanguages = 
    KeySortBy[
        <|extractLanguages @ PacletFind["*", <|"Extension" -> "ExternalEvaluate"|>]|>,
        {Lookup[$ordering, #, 1000], #} &
    ]

$LanguageInformations := Block[
    {$ContextPath = $ContextPath},
    $LanguageInformations = <||>;
    Cases[
        DeleteDuplicates @ Flatten @ getExtension[
            DeleteDuplicates @ Values @ $AvailableLanguages,
            "Context"
        ],
        context_String :> Get[context]
    ];
    $LanguageInformations
]

(* 
    this function is responsible for getting language information that are registered when a paclet is loaded.
    $AvailableLanguages contains information about local and remote paclets, the first time a paclet is loaded
    we are checking remotely if there is an update, otherwise we are just loading the code and returning the assoc
    registered by RegisterSystem
*)

GetLanguageRules[] := Keys[$LanguageInformations]

GetLanguageRules[All, rest___] := GetLanguageRules[GetLanguageRules[], rest]

GetLanguageRules[s_List, rest___] := AssociationMap[GetLanguageRules[#, rest] &, s]

GetLanguageRules[s_String, rest___] /; AssociationQ[$LanguageInformations[s]] := 
    Part[$LanguageInformations, s, rest]

GetLanguageRules[s_String, rest___] := 
    autofail[$Failed, "unknownSys", s]

(* normalization function is used to dispatch arbitrary expressions to the right evaluator *)
NormalizationFunction := 
    NormalizationFunction = Replace @ Join[

        (* file and string here are simply not accepted *)
        {
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
                <|NormalizationFunction[system], h["ReturnType", type]|>
        },

        (* 
            first we collect all normalization rules for all systems 
            we are using association to delete duplicates, only the last one will match,
            we need to make sure the same pattern is not registed in two different evaluators.

            those patterns might contain String and File, but file is already handled as the first case, and String
            is normally handled as System by ExternalEvaluate and StartExternalSession
        *)

        Normal @ Association @ KeyValueMap[
            Function[
                {system, rules},
                If[
                    TrueQ @ GetLanguageRules[system, "NormalizeTargetExpressions"],
                    Replace[rules, h_[lhs_, rhs_] :> h[lhs, <|"System" -> system, "Target" -> rhs|>], {1}],
                    {}
                ]
            ],
            GetLanguageRules[All, "TargetNormalizationRules"]
        ],
        (* if nothing is matching we can raise an error *)
        {s_ :> returnUnevaluated[$Failed, "unknownSys", s]}
    ]
