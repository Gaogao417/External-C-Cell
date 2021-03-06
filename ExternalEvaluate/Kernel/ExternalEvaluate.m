(* 
    This file loaded by Get/Needs["ExternalEvaluate`"]. 
    It must load the package files and also ensure that ExternalEvaluate` context is on $ContextPath, which is not done by ExternalEvaluateLoader.
*)

BeginPackage["ExternalEvaluate`"]
EndPackage[]

(* 
    All loading of the paclet's Wolfram Language code should go through this file.
    Developer maintains this list of symbols.
    autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)

ExternalEvaluate`Private`autoloadSymbols = {
    (* ExternalEvaluate symbols *)
    "System`ExternalEvaluate",
    "System`ExternalFunction",
    "System`ExternalObject",
    "System`ExternalSessionObject",
    "System`ExternalSessions",
    "System`ExternalValue",
    "System`FindExternalEvaluators",
    "System`RegisterExternalEvaluator",
    "System`StartExternalSession",
    "System`UnregisterExternalEvaluator",
    "ExternalEvaluate`RegisterSystem",
    "ExternalEvaluate`FE`ExternalCellEvaluate",
    "ExternalEvaluate`FE`GetDefaultCellEvaluationLanguage", 
    "ExternalEvaluate`FE`GetExternalLanguageMenuData"
};

PacletManager`Package`loadWolframLanguageCode[
    "ExternalEvaluate", 
    "ExternalEvaluate`", 
    DirectoryName[$InputFileName], 
    "Main.m",
    "AutoUpdate"       -> True, 
    "ForceMX"          -> False, 
    "Lock"             -> False,
    "AutoloadSymbols"  -> ExternalEvaluate`Private`autoloadSymbols,
    "SymbolsToProtect" -> ExternalEvaluate`Private`autoloadSymbols
]