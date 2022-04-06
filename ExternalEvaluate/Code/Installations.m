
(* implementation of UUID3, using hash of an expression *)
uuid3[expr___] := StringInsert[Hash[{expr}, "MD5", "HexString"], "-", {9, 13, 17, 21}]


addUUID3[install_] := uuid3[install["System"], install["Target"]] -> install

(* Register getter / setter, we are using PersistentValue (renamed to PersistentSymbol in 12.3) to store values on disk *)

setRegistryValue[name_String] := setRegistryValue[name, Missing["Cleanup", Now]] (* we want to keep a track of the cleanup operation *)

If[
    $VersionNumber <= 12.2, 
    setRegistryValue[name_String, val_] := Set[System`PersistentValue[name, "Local"], val];
    getRegistryValue[name_String] := System`PersistentValue[name, "Local"], 

    setRegistryValue[name_String, val_] := Set[System`PersistentSymbol[name, "Local"], val];
    getRegistryValue[name_String] := System`PersistentSymbol[name, "Local"]
]


(* 

    get installation logic, we are providing 3 different internal apis:

    1. getRegisteredInstallations, this is a very cheap operation that is returning the internal registry
    2. getLocalInstallations, this is a very expensive operation that is searching the user machine for installations
    3. getAllInstallations, is returning the merge of getLocalInstallations and getRegisterdInstallations


    Implementation details:

    * getRegisteredInstallations is responsabile to return the registry in a state that is computable by the rest of the code.
    to do that we need to make sure the state of the registry is up to date.

    * We are storing a version number in the registry that might changes between releases, when this happens there is a migration step to be executed.

    * The UUID that is created for every installation is stable, it's always goint to be the same for a certain pair of system / target, this is done by using uuid3 implementation which is basically an MD5 hash instead of using CreateUUID.

    * Since UUID is always going to be the same for a certain paid of System / Target we can directly merge entries from the registry with all entries that are found in the user system by just joining 2 associations.
*)

migrateTarget[val_] := 
    Module[
        {copy = val},
        copy[[All, "Target"]] = Values[copy[[All, "Executable"]]];
        copy
    ]

(*  in the new version we are not storing entries with False. 
    we also need to migrate all randomly generated uuids to stable uuid.
*)
migrateUUID[val_] := Association @ Cases[
    val,
    install:KeyValuePattern["Registered" -> Except[False]] :> addUUID3[install]
]

$RegistryLocation = "ExternalEvaluate`EvaluatorCache"

registryLocation[sys_String] := StringJoin[$RegistryLocation, "`", sys]
registryLocation[] := $RegistryLocation

(* The test suite is blocking this variable in order to test clean installations *)

cleanupRegistry[sys_String] := (
    ExternalEvaluate`Private`GetLanguageRules[sys, "Loaded"]; (* this will propagate failure if sys is not known *)
    setRegistryValue @ registryLocation[sys];
)
cleanupRegistry[s_List] := Scan[cleanupRegistry, s];
cleanupRegistry[All:All:All] := (
    cleanupRegistry @ ExternalEvaluate`Private`GetLanguageRules[];
    setRegistryValue @ registryLocation[];
)

getRegisteredInstallations[lang_String] :=  Replace[
    getRegistryValue @ registryLocation[lang], {

        (* latest version of the structure, it includes a Version -> 3 *)
        KeyValuePattern[{"Installations" -> val_Association}] :>
            val,

        (* the previous version used to be a global registry, with everything inside.
            if this is the case, we should try to read the file.
            if something is there, we run all migrations on all results.
            if the registry has no entry for the evaluator or has corrupted data, we initialize a new registry.
        *)

        _Missing|KeyValuePattern["Installations" -> _Missing] :> 
            Replace[
                getRegistryValue @ registryLocation[], {
                    KeyValuePattern["Installations" -> val_Association] :> 
                        Replace[
                            migrateUUID @ migrateTarget @ Select[
                                val,
                                #System === lang &
                            ],
                            <||> :> setInstallations[lang]
                        ],

                    _ :> setInstallations[lang]
                }
            ],

        _ :> (
            emit["cache"];
            setInstallations[lang]
        )
    }
]

getLocalInstallations[lang_String, default_:Automatic] :=
    With[
        {time = UnixTime[]},
        Association @ Map[
            addUUID3 @ makeInstallation[lang, #, default, time] &, 
            (*search for all installations that have an executable file matching the pattern in the directories*)
            Select[
                GetLanguageRules[lang, "TargetDiscoveryFunction"][],
                GetLanguageRules[lang, "TargetValidationFunction"]     
            ]
        ]
    ]


getAllInstallations[] := getAllInstallations[GetLanguageRules[]]
getAllInstallations[All, rest___] := Select[getAllInstallations[GetLanguageRules[], rest], Length[#] > 0 &]
getAllInstallations[lang_List, rest___] := AssociationMap[getAllInstallations[#, rest] &, lang]
getAllInstallations[lang_String] := 
    Join[
        getLocalInstallations[lang, False],
        getRegisteredInstallations[lang]
    ]

getAllInstallations[lang_String, part_] := 
    Part[getAllInstallations[lang], All, part]


(*

    now we start with the setInstallation logic, this is used by to register/unregister evaluators.
    we are storing data in the disk using PersistentValue.

    Any sensitive information is automatically encrypted, see Encrypt.m for more details.

*)

setInstallations[lang_String, value_] := (
    setRegistryValue[
        registryLocation[lang],
        <|"Installations" -> encrypt @ value, "Version" -> 3, "UnixTime" -> UnixTime[]|>
    ];
    value
)

setInstallations[lang_String] := 
    setInstallations[lang, Select[getLocalInstallations[lang, Automatic], #Registered === Automatic &]]


normalizeTarget[lang_, exec_] := 
    With[
        {target = autofail @ GetLanguageRules[lang, "TargetNormalizationFunction"][exec]},
        autofail[
            If[
                TrueQ @ GetLanguageRules[lang, "TargetValidationFunction"][target],
                target,
                $Failed
            ],
            If[
                MatchQ[target, _String|_File],
                "invalidExec",
                "invalidTarget"
            ], 
            target
        ]
    ]



checkDependencies[current_] := 
    If[
        current["Registered"] === "MissingDependencies",
        help[
            current["System"],
            $Failed, 
            "depend"
        ],
        current
    ]


(* update installations is called by register / unregister external evaluator *)


updateInstallation[any_, All] :=
    updateInstallation[any, All, All]

updateInstallation[any_, All, rest__] :=
    updateInstallation[any, GetLanguageRules[], rest]

updateInstallation[False, lang_String, All] :=
    updateInstallation[False, {lang}, All]

updateInstallation[False, lang_List, All] :=
    WithCleanup[
        Join @@ Keys[Map[getRegisteredInstallations, lang]],
        Scan[
            setInstallations[#, <||>] &,
            lang
        ]
    ]



updateInstallation[False, uuids:_String|{___String}] :=
    WithCleanup[
        uuids,
        Scan[
            updateInstallation[False, #, uuidlist[uuids]] &,
            GetLanguageRules[]
        ]
    ]    

updateInstallation[False, lang_String, uuidlist[uuids_]] := 
    WithCleanup[
        uuids,
        setInstallations[
            lang,
            KeyDrop[getRegisteredInstallations[lang], uuids]
        ]
    ] 

updateInstallation[False, lang_String, targets_List] := 
    updateInstallation[
        False, 
        lang, 
        uuidlist @ Map[
            uuid3[lang, normalizeTarget[lang, #]] &,
            targets
        ]
    ]

updateInstallation[False, lang_String, target_] := 
    First @ updateInstallation[False, lang, {target}]

   

updateInstallation[True, lang_String, All] :=
    updateInstallation[True, lang, installationlist @ Select[getLocalInstallations[lang, True], #Registered === True &]]



updateInstallation[True, lang_String, targets_List] := 
    updateInstallation[
        True,
        lang,
        With[
            {time = UnixTime[]},
            installationlist @ Map[
                addUUID3 @ checkDependencies @ makeInstallation[lang, #, True, time] &,
                targets
            ]
        ]
    ]

updateInstallation[True, lang_String, installationlist[targets_]] :=
    WithCleanup[
        Keys @ targets,
        setInstallations[
            lang, 
            <|getRegisteredInstallations[lang], targets|>
        ]
    ]

updateInstallation[True, lang_String, target_] :=
    First @ updateInstallation[True, lang, {target}]

updateInstallation[True, lang:{___String}, rest___] := 
    Union @@ Map[
        updateInstallation[True, #, rest] &,
        lang
    ]

makeInstallation[lang_, executable_, registered_, time_] :=
    With[
        {exec = normalizeTarget[lang, executable]},
        <|
            "System" -> lang, 
            "Version" -> calculateVersion[lang, exec],
            "Target" :> exec,
            "Executable" :> exec,
            "Registered" -> If[
                AllTrue[
                    GetLanguageRules[lang, "DependencyTestFile"],
                    Function @ SameQ[
                        Quiet @ ToUpperCase @ StringTrim @ RunProcess[
                            GetLanguageRules[lang, "ScriptExecCommandFunction"][
                                exec,
                                #1,
                                None
                            ],
                            "StandardOutput"
                        ],
                        "TRUE"
                    ]
                ],
                registered,
                "MissingDependencies"
            ],
            "UnixTime" -> time
        |>
    ]


(* this version is running ONCE, when the evaluator is first registered or every time you run RegisterExternalEvaluator. 
get version install will run the executable's version command, then combine stdout with stderr (this is necessary cause some programs like Python always output to stderr for some reason when used with RunProcess)*)

getVersionInstall[None|Null|_Missing, ___] := ""
getVersionInstall[commandFunc_, executable_]:= 
    Module[
        {exec, res},
        exec = autofail @ commandFunc[executable];
        If[StringQ[exec], Return[exec]];
        If[!ListQ[exec], Return[""]];
        res = Quiet[RunProcess[exec]];
        If[!AssociationQ[res], Return[""]];
        StringJoin[Values[KeyTake[res, {"StandardOutput", "StandardError"}]]]
    ]

calculateVersion[lang_, executable_] := 
    autofail @ With[
        {
            conform = GetLanguageRules[lang, "VersionStringConformFunction"],
            exec    = GetLanguageRules[lang, "VersionExecCommandFunction"],
            parser  = GetLanguageRules[lang, "VersionParserFunction"]
        },
        conform @ parser @ getVersionInstall[exec, executable]
    ]