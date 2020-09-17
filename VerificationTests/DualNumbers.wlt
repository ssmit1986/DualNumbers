BeginTestSection["DualNumbers"]

BeginTestSection["Initialisation"]

VerificationTest[(* 1 *)
	CompoundExpression[Set[$HistoryLength, 10], With[List[Set[dir, ParentDirectory[If[Quiet[TrueQ[FileExistsQ[$TestFileName]]], DirectoryName[$TestFileName], NotebookDirectory[]]]]], PacletDirectoryLoad[dir]], Quiet[Get["DualNumbers`"]], ClearAll["Global`*"], "Done"]
	,
	"Done"	
	,
	TestID->"c6e0c965-8610-471c-82a6-565e62629a21"
]

EndTestSection[]

BeginTestSection["Construction"]

VerificationTest[(* 2 *)
	List[Dual[], Dual[1], Dual[0, 1]]
	,
	List[Dual[0, 1], Dual[1, 0], Dual[0, 1]]	
	,
	TestID->"c239c0e5-4e2a-4e1a-93aa-af52d4971e24"
]

EndTestSection[]

BeginTestSection["Elementary properties"]

EndTestSection[]

EndTestSection[]
