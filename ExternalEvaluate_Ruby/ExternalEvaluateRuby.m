(* ::Package:: *)

BeginPackage["ExternalEvaluate`"]

Begin["`Ruby`"]



(*
ExternalEvaluate`RegisterSystem["Ruby",
	<|
        "StringTemplateSupportQ" -> True,
        "VersionStringConformFunction"->Function[
            First[StringCases[#, RegularExpression["[0-9\\.]+"]], Missing["NotAvailable"]]
        ],


        "TargetDiscoveryFunction" -> 
            Function @ FileNames[
                StringJoin["ruby", Switch[$OperatingSystem, "Windows", ".exe", _, ""]], 
                Switch[
                    $OperatingSystem,
                    "Windows",
                    Flatten @ {
                        (*versioned folder names*)
                        FileNames[FileNameJoin[{First[FileNameSplit[Environment["PROGRAMFILES"]]], "Ruby*x64", "bin"}]],
                        FileNames[FileNameJoin[{Environment["LOCALAPPDATA"], "Ruby*x64", "bin"}]],
                        FileNames[FileNameJoin[{Environment["PROGRAMFILES"], "Ruby*x64", "bin"}]]
                    },
                    "MacOSX", {
                        "/usr/bin",
                        "/usr/local/bin/",
                        "/usr/local/sbin",
                        "/usr/local/Cellar/bin",
                        FileNameJoin[{$HomeDirectory, ".rbenv/shims"}]
                    },
                    "Unix",
                    {"/usr/bin", "/usr/local/bin/", "/usr/local/sbin"}
                ]
            ],

		
		"ProgramFileFunction"->Function[PacletManager`PacletResource["ExternalEvaluate_Ruby", "REPL"]],
		

		"DeserializationFunction" -> Function[
            Replace[
            	Developer`ReadRawJSONString[ByteArrayToString[#]], {
            		KeyValuePattern["output" -> expr_] :> expr,
					KeyValuePattern["Function"  -> expr_] :> ExternalFunction[expr],
            		KeyValuePattern[{"error"  -> expr_, "message" -> msg_}] :> 
            			Failure["RubyError", <|
            				"MessageTemplate" -> msg,
            				"MessageParameters" -> {},
            				"Traceback" -> Column @ expr
            			|>]
            	}
        	]
        ]
	|>
]; 
*)



(*Actually, Evaluator for C *)
System`cCell = "C++";
cCell::error = "Invalid cCellType:`1`, Legal cCellType should be \"C\" or \"C++\"";
cCell::fail = "Compilation Failed";

$cHead = 
"#include \"WolframLibrary.h\"

DLLEXPORT mint WolframLibrary_getVersion(){
  return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize( WolframLibraryData libData) {
    return 0;
}

DLLEXPORT void WolframLibrary_uninitialize( WolframLibraryData \
libData) {
    return;
}\n\n"

ExternalEvaluate`RegisterSystem["Ruby",
    <|
        "NonZMQInitializeFunction"-> False,
        "ScriptExecCommandFunction"-> Function[
            Needs["RLink`"];
            If[
                FailureQ[RLink`InstallR["RHomeLocation"->"D:\\Program Files\\R\\R-4.1.2"]], 
                Message[ExternalEvaluate::rstarterr]; $Failed, 
                0
            ]
        ],
        "NonZMQEvaluationFunction"-> Function[
            {session, input}, 
            Module[{lib, csrc},
            Needs["CCompilerDriver`"];
			
			csrc = 
			Which[
				System`cCell == "C++", CPreProcessor@input["input"]//StringReplace["DLLEXPORT"->"EXTERN_C DLLEXPORT"],
				System`cCell = "C", CPreProcessor@input["input"]];
				
            (*Kernel of the Function, generate the C++ Library*)
            lib = 
            Which[
                System`cCell == "C++",
                    CCompilerDriver`CreateLibrary[
                        csrc, 
                        StringReplace[DateString[], " "|":"->"_"], 
                        Language -> "C++", "CompileOptions" -> " /std:c++20"],
                System`cCell == "C",
                    CCompilerDriver`CreateLibrary[
                        csrc, 
                        StringReplace[DateString[], " "|":"->"_"]],
                True,
                    Message[cCellType::error, System`cCell]; Throw[$Failed]
                ];

            If[!FileExistsQ[lib], Message[cCell::fail]; Abort[]];

            (*Functions to write LibraryFunctionLoad in notebook *)
            libraryLoadCell = function$str |->
            Module[{inType, outType, inName, outName, funcName, code},
                {inType, outType, inName, outName, funcName, code} = Parser[function$str];
                CellPrint@Cell[
                    StringTemplate["ClearAll[`funSym`]\n`funSym` = LibraryFunctionLoad[`lib`, \n`funName`, \n`inType`, \n`outType`]"][
                    <|
                        "lib"->ToString[lib, InputForm], "funSym"->StringReplace[funcName, "_"->"$"], "funName"->ToString[funcName, InputForm], 
                        "inType"->(inType//cTypeToLibraryType), "outType"->(outType//cTypeToLibraryType)
                    |>
                    ],
                    "Code"]
                ];
            libraryLoadCells = input$str |-> Map[libraryLoadCell, Rest@getFunctions@input$str];

            libraryLoadCells[input["input"]];
            ]
        ]
    |>
]



getFunctions = input$str |->
StringSplit[input$str, "#Export"]//
(code$strList |-> 
    If[
        StringMatchQ[RegularExpression[";;[^\n]*\n"]~~__][First[code$strList]],
        {
            " ",
            Cases[code$strList, 
              function_?(StringMatchQ[RegularExpression[";;[^\n]*\n"]~~__]) :> 
              StringDelete[RegularExpression[";;[^\n]*\n"]]@function]
        }//Flatten,
        {
            First[code$strList],
            Cases[code$strList, 
              function_?(StringMatchQ[RegularExpression[";;[^\n]*\n"]~~__]) :> 
              StringDelete[RegularExpression[";;[^\n]*\n"]]@function]
        }//Flatten])


bracketLocs::inputError = "\"`1`\" Error";
bracketLocs = function$str |->
Block[
    {
        charList = Characters[function$str],
        declLoc = {0, 0},
        parenthesisLoc = {0, 0}, braceLoc = {0, 0},
        braceStack = CreateDataStructure["Stack"],
        i = 1
    },
    
    While[charList[[i]] != "(" && i < Length[charList], i++];
    If[i == Length[charList], Message[bracketLocs::inputError, "("];Throw[$Failed]];
    parenthesisLoc[[1]] = i;
    
    While[charList[[i]] != ")" && i < Length[charList], i++];
    If[i == Length[charList], Message[bracketLocs::inputError, ")"];Throw[$Failed]];
    parenthesisLoc[[2]] = i;
    
    While[charList[[i]] != "{" && i < Length[charList], i++];
    If[i == Length[charList], Message[bracketLocs::inputError, "{"];Throw[$Failed]];
    braceStack["Push", i];
    braceLoc[[1]] = i;
    
    While[!braceStack["EmptyQ"] && i < Length[charList], 
        i++;
        If[charList[[i]] == "{", braceStack["Push", i]];
        If[charList[[i]] == "}", braceStack["Pop"]];
        ];
    If[!braceStack["EmptyQ"] && i == Length[charList], Message[bracketLocs::inputError, "{"];Throw[$Failed]];
    braceLoc[[2]] = i;
    
    declLoc = {1, parenthesisLoc[[1]]-1};
    {declLoc, {parenthesisLoc[[1]]+1, parenthesisLoc[[2]]-1}, {braceLoc[[1]]+1, braceLoc[[2]]-1}}
];

bracketSplit = function$str |->
Map[
    StringTake[function$str, #]&,
    bracketLocs[function$str]
    ]
    

Parser = function$str |->
Block[{decl, input, src, outType, funcName, inputVars, inType, inName, code, outName},
    {decl, input, src} = bracketSplit[function$str];
    {outType, funcName} = StringSplit[decl][[-2;;-1]];
    inputVars = StringSplit[input, RegularExpression["\\s*,\\s*"]];
    {inType, inName} = Transpose@Map[
        StringSplit[#, RegularExpression["\\s+"]]&,
        inputVars];
    {code, outName} = StringSplit[src, RegularExpression["\\s*Return\\s*"]];
    outName = StringDelete[outName, ";"|"\n"];
    {inType, outType, inName, outName, funcName, code}];

generateInputDecls = StringTemplate[
    "`inType` `inName` = `inCoercion` *(Args[<*i - 1*>].`inMember`);\n\t"
];

generateFunction = StringTemplate[
"DLLEXPORT int `funcName`(WolframLibraryData libData, mint Argc, 
MArgument *Args, MArgument Res){
	`inputDeclaration``code`
	*Res.`outMember` = `outCoercion`(`outName`);
	return LIBRARY_NO_ERROR;
}"];

generateInType = 
StringReplace[
{
	a__~~"&":> 
		StringReplace["$"->"*"][a~~"$"],
	x__:>x
}];

generateInCoercion = 
StringReplace[
{
	a__~~"&":> 
		StringReplace["$"->"*"]["("~~a~~"$)"], 
	___->""
}];
	
generateOutCoercion = 
StringReplace[
	{__~~"&":> "(mint)", ___->""}
];

cTypeToArgumentMember = 
StringReplace[{
    __~~"&"->"integer",
    "mbool"->"boolean",
    "mint"->"integer",
    "mreal"->"real",
    "mcomplex"->"cmplex",
    "MTensor"->"tensor",
    "MSparseArray"->"sparse",
    "MNumericArray"->"numeric",
    "MImage"->"image",
    "char*"->"utf8string"
}];

cTypeToLibraryType = 
StringReplace[{
	__~~"&":>"Integer",
    "mbool"->"\"Boolean\"",
    "mint"->"Integer",
    "mreal"->"Real",
    "mcomplex"->"Complex",
    "MTensor"->"{_, _}",
    "MSparseArray"->"LibraryDataType[SparseArray]",
    "MNumericArray"->"LibraryDataType[NumericArray]",
    "MImage"->"LibraryDataType[Image3D]",
    "char*"->"UTF8String",
    "WSLINK"->"LinkObject"
}]


ClearAll[checkType]
checkType::typeError = "Invalid `1` Type: `2`, Legal types:" <> ToString[(cTypeToArgumentMember/.{Rule->List, RuleDelayed->List}//Transpose)[[1]]];
checkType[types_List] := And@@Map[checkType, types];
checkType[type_] := 
Map[
	MatchQ[type, #]&,
	(cTypeToArgumentMember/.{Rule->List, RuleDelayed->List}//Transpose)[[1]]
	]

functionPreProcessor = function$str |->
Block[{inType, outType, inName, outName, funcName, code, inMember, outMember, inputDeclaration, inCoercion, outCoercion},
    {inType, outType, inName, outName, funcName, code} = Parser[function$str];
    If[!checkType[inType], Message[checkType::typeError, "Input", ToString[inType]]; Throw@$Failed];
    If[!checkType[outType], Message[checkType::typeError, "Output", ToString[outType]]; Throw@$Failed];
    inMember = cTypeToArgumentMember@inType;
    outMember = cTypeToArgumentMember@outType;
    (*{inType, outType, inName, outName, funcName, code}//Echo;
    {inMember, outMember}//Echo;*)
    inCoercion = generateInCoercion@inType;
    outCoercion = generateOutCoercion@outType;
    
    inputDeclaration = inType;
    Do[
        inputDeclaration[[i]] = generateInputDecls[<|"inType"->generateInType@inType[[i]],"inName"->inName[[i]],"inMember"->inMember[[i]], "inCoercion"->inCoercion[[i]]|>],
        {i, Length@inType}];
    inputDeclaration = StringJoin[inputDeclaration];
    
    generateFunction[<|"funcName"->funcName,"inputDeclaration"->inputDeclaration,"outMember"->outMember,"outName"->outName, "outCoercion"->outCoercion, "code"->code|>]
    ];

CPreProcessor = input$str |->
(inputList = getFunctions@input$str;
$cHead<>First[inputList]<>Map[functionPreProcessor, Rest[inputList]])

(*
functionPreProcessor[
"MTensor FuBei(mreal I0, mreal I2, MTensor I3) {
    mint I1 = I0 + I2 + I3;
    mint fubei = I1 * I3;
    return fubei;
"
]
*)

End[]
EndPackage[]









