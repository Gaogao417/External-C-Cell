(*this enforces a semantically pleasing ordering*)
(*of the keys in the summary box for ExternalObject*)

orderAssoc[assoc_] := Join[
	KeyTake[assoc, {"Command", "System"}], 
	KeyDrop[assoc, {"Command", "System", "Session"}], 
	KeyTake[assoc, {"Session"}]
]

ExternalObject/:MakeBoxes[ExternalObject[assoc_Association?AssociationQ], StandardForm|TraditionalForm]:=
	BoxForm`ArrangeSummaryBox[
		(*first argument is the head to use*)
		ExternalObject,
		(*second argument is the expression*)
		ExternalObject[assoc],
		(*third argument is the icon to use*)
		None,
		(*the next argument is the always visisble properties*)
		MapAt[
			(*this will make sure that we span on the top row*)
			Append[#,SpanFromLeft]&,
			(*this bit of code will make sure that we never include the Session or the Source keys*)
			(*and that we take up to of the first entries in the association*)
			(*and make them into a grid matrix*)
			Partition[
				KeyValueMap[
					Function[{key,val},BoxForm`SummaryItem[{key<>": ",val}]],
					Take[orderAssoc @ KeyDrop[assoc, {"Session"}],UpTo[4]]
				],
				UpTo[2]
			],
			-1
		],
		(*the next argument is the optional items that come down when the plus button is pressed*)
		KeyValueMap[
			Function[{key,val},BoxForm`SummaryItem[{key<>": ",val}]],
			orderAssoc @ assoc
			],
		(*lastly,the display form we want to display this with*)
		StandardForm,
		(*we use complete replacement to completely ignore the first set of displayed values*)
		(*with the second one when the button is clicked*)
		"CompleteReplacement"->True
	];