BeginPackage["CCompilerDriver`MinGWCompiler32`", {"CCompilerDriver`"}]

MinGWCompiler32::usage = "MinGWCompiler32[src, name] compiles the code in src into a DLL and returns the full path of the DLL.";

Begin["`Private`"]

`$ThisDriver = MinGWCompiler32

Needs["CCompilerDriver`CCompilerDriverBase`"]
Needs["CCompilerDriver`CCompilerDriverRegistry`"]

CCompilerRegister[ $ThisDriver, {"Windows"}]

Options[ $ThisDriver] =
	DeriveOptions[
		{
			"SystemCompileOptions" -> {"-O2"}
		}
	]

MinGWCompiler32["Available"] :=
	TrueQ[Directory ===
		Quiet[FileType[$ThisDriver["ResolveInstallation"][Automatic]]]]

MinGWCompiler32["Name"][] := "MinGW"

MinGWCompiler32["Installation"][] := $ThisDriver["ResolveInstallation"][Automatic]

MinGWCompiler32["LibraryPathFlag"][] := "-L"

MinGWCompiler32["ResolveSystemLibraries"]["Windows", translib_] :=
	If[!MatchQ[translib, "WSTP"], {"ml32i4m"}, {"wstp32i4m"}]

MinGWCompiler32["SupportedTargetSystemIDQ"]["Windows-x86-64", ___] := False

MinGWCompiler32["CreateObjectFileCommands"][
	errHd_, installation_, compilerName_, outFile_, workDir_,
	compileOptions_, linkerOptions_, defines_, includePath_, srcFileRules_List,
	tmSrcFiles_, cFiles_, syslibs_, libs_, libpath_, extraObjects_,
	targetSystemID_, cleanIntermediate_, mprepOptions_, translib_, language_, opts_] :=
	Module[{gccExe = compilerCommand[installation, compilerName]},

		{
			SetupEnvironment[gccExe],
			CommandJoin[
				QuoteFile[gccExe],
				" -c",
				" -o ", QuoteFile[WorkingOutputFile[workDir, outFile]],
				" ", compileOptions,
				" ", defines,
				" ", includePath,
				" ", QuoteFiles[cFiles],
				" 2>&1\n"
			]
		}
	]

MinGWCompiler32["CreateLibraryCommands"][
	errHd_, installation_, compilerName_, outFile_, workDir_,
	compileOptions_, linkerOptions_, defines_, includePath_, srcFileRules_List,
	tmSrcFiles_, cFiles_, syslibs_, libs_, libpath_, extraObjects_,
	targetSystemID_, cleanIntermediate_, mprepOptions_, translib_, language_, opts_] :=
	Module[{gccExe = compilerCommand[installation, compilerName]},

		{
			SetupEnvironment[gccExe],
			CommandJoin[
				QuoteFile[gccExe],
				" ", $CreateDLLFlag,
				" ", "-o ", QuoteFile[WorkingOutputFile[workDir, outFile]],
				" ", compileOptions,
				" ", defines,
				" ", includePath,
				" ", QuoteFiles[cFiles],
				" ", QuoteFiles[extraObjects],
				Map[" -Xlinker " <> # &, linkerOptions],
				" ", libpath,
				" ", formatLibraries[syslibs, libs],
				" 2>&1\n"
			]
		}
	]

MinGWCompiler32["CreateExecutableCommands"][
	errHd_, installation_, compilerName_, outFile_, workDir_,
	compileOptions_, linkerOptions_, defines_, includePath_, srcFileRules_List,
	tmSrcFiles_, cFiles_, syslibs_, libs_, libpath_, extraObjects_,
	targetSystemID_, cleanIntermediate_, mprepOptions_, translib_, language_, opts_] :=
	Module[{gccExe = compilerCommand[installation, compilerName]},

		{
			SetupEnvironment[gccExe],
			CommandJoin[
				MprepCalls[tmSrcFiles, workDir, translib, mprepOptions],

				QuoteFile[gccExe],
				" -o ", QuoteFile[WorkingOutputFile[workDir, outFile]],
				" ", compileOptions,
				" ", defines,
				" ", includePath,
				" ", QuoteFiles[cFiles],
				" ", QuoteFiles[extraObjects],
				Map[" -Xlinker " <> # &, linkerOptions],
				" ", libpath,
				" ", formatLibraries[syslibs, libs],
				" ", exeLinkLibraries[targetSystemID],
				" 2>&1\n"
			]
		}
	]

SetupEnvironment[gccPath_] :=
	"set PATH="<>DirectoryName[gccPath]<>";%PATH%"

exeLinkLibraries[_] := "-lgdi32"

$CreateDLLFlag = "-shared"

(*****************************************************************************)
(* Automatic installation detection *)

$PathDirs = StringSplit[Environment["PATH"], $PlatformPathSeparator]

$GCC = "gcc"<>$PlatformExeExtension

validGCCExeQ[path_] := File === Quiet[FileType[path]]

$DefaultMinGWPaths = Flatten[
	Join[
		FileNames[{"msys64/mingw32", "MinGW"},
			FileNameJoin[{Environment["SystemDrive"]}]
		]
		,
		FileNames["*/mingw32",
			FileNameJoin[{Environment["ProgramFiles(x86)"], "mingw-w64"}]
		]
	]
];


MinGWCompiler32["ResolveInstallation"][Automatic] := findCompilerInstallation[]

findCompilerInstallation[] :=
	Select[Join[$PathDirs, gccLocations[$DefaultMinGWPaths, $GCC],
		gccLocations[Environment["MINGW"], $GCC]], validGCCExeQ, 1] /.
		{
			{path_String} :> FileNameDrop[path, -1],
			_ :> $Failed
		}

MinGWCompiler32["Installations"][] :=
	Select[Join[$PathDirs, gccLocations[$DefaultMinGWPaths, $GCC],
		gccLocations[Environment["MINGW"], $GCC]], validGCCExeQ]

MinGWCompiler32["ResolveInstallation"][path_String] := path

MinGWCompiler32["ResolveCompilerName"][Automatic] := $GCC

MinGWCompiler32["ResolveCompilerName"][name_] := name

MinGWCompiler32["ValidInstallationQ"][installation_] :=
	TrueQ[validGCCExeQ[compilerCommand[installation]]]

MinGWCompiler32["ValidInstallationQ"][installation_, name_, ___] :=
	TrueQ[validGCCExeQ[compilerCommand[installation, name]]]

compilerCommand[installation_String] :=
	compilerCommand[installation, Automatic]

compilerCommand[installation_String, Automatic] :=
	compilerCommand[installation, $GCC]

compilerCommand[installation_String, name_String] :=
	Select[gccLocations[installation, name], validGCCExeQ, 1] /. {
		{path_} :> path,
		_ :> FileNameJoin[{installation, name}] (*possibly invalid, try anyway*)
	}

gccLocations[installation_, name_] :=
(
	gccLocations[installation, name] =
		{
			installation,
			FileNameJoin[{installation, name}],
			FileNameJoin[{installation, "bin", name}]
		}
)

gccLocations[installations_List, name_] := Flatten[Map[gccLocations[#, name]&, installations]]

(*****************************************************************************)
formatLibraries[libs_List] :=
	Riffle[formatLibrary /@ libs, " "]

formatLibraries[libs_List, libs2_List] := formatLibraries[Join[libs, libs2]]

formatLibrary[lib_] :=
	If[LibraryPathQ[lib],
		(* lib appears to be an explicit library file path, just quote it *)
		QuoteFile[lib],
		(* lib appears to be a simple lib name, pass it to -l *)
		If[StringMatchQ[lib, "-l" ~~ ___],
			QuoteFile[lib],
			"-l"<>QuoteFile[lib]
		]
	]

LibraryPathQ[lib_] :=
	StringMatchQ[lib,
		(* Files ending in .a or .so followed by 0 or more .N extensions *)
		(___ ~~ (".lib" | ".a" | (".so" ~~ (("." ~~ NumberString) ...)))) |
		(* Or files containing a directorty separator *)
		(___ ~~ "/" ~~ ___)
	]

MinGWCompiler32[method_][args___] :=
	CCompilerDriver`CCompilerDriverBase`BaseDriver[method][args]

CCompilerRegister[ $ThisDriver]

End[]
EndPackage[]
