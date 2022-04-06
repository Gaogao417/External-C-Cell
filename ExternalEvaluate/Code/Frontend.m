
(* Utility function to get the default external session *)

SetAttributes[GetDefaultExternalSession, HoldRest]

ExternalEvaluate`GetDefaultExternalSession;

GetDefaultExternalSession[sys_String] :=
    With[
        {name = "Default" <> sys <> "Session"},
        SelectFirst[
            ExternalSessions[], 
            #["Name"] === name &,
            StartExternalSession[{sys, "Name" -> name}]
        ]
    ]
    
(**********  Front End ExternalLanguage cells integration  **********)

(* We modify this value whenever anything happens that might cause the ExternalLanguage cell menu properties to change, like
    if a user resets the default language. This will trigger Dynamics in the cell popup menus to re-evaluate. Note that
    nothing sets this currently, as the set of languages is always looked up on the fly, and there is no facility yet
    to set the default language.
*)

$FElang = "Python"

ExternalEvaluate`FE`$ExternalLanguageMenuDataChanged = 1;

(* Called by the front end. For now, we don't support letting users change the default. The main problem is that
    if they open a notebook from 12.0 with s typical Python cell in it, that cell would switch to the new default type,
    because it doesn't have Python burned-in as its language. The 12.1 FE always burns in a language, so this won't
    be a problem in the future.
*)
ExternalEvaluate`FE`GetDefaultCellEvaluationLanguage[] := $FElang
 
(* Called by the front end *)
ExternalEvaluate`FE`GetExternalLanguageMenuData[] := (
    (* This flag variable must appear within this function to get proper Dynamic updating *)
    ExternalEvaluate`FE`$ExternalLanguageMenuDataChanged;
  	Map[
		{#, ReleaseHold @ GetLanguageRules[#, "IconCell"], "  " <> # <> " "}&,
		(* The set of all language names we want to list in the menu, in order *)
		Select[
            GetLanguageRules[],
            GetLanguageRules[#, "ShowInFrontendCellQ"] &
        ]
  	]
)


(*function that the FrontEnd uses for evaluating inside ExternalEvaluate cells *)
ExternalEvaluate`FE`ExternalCellEvaluate[sys_, input_] := (
    (* Capture the cell type so that it becomes the default for future external language cells in this session.  *)
    If[
        StringQ[sys], 
        $FElang = sys
    ];
    Replace[
      	GetDefaultExternalSession[sys],
      	session_ExternalSessionObject :> ExternalEvaluate[session, input]
    ]
)	
