BeginTestSection["HelperFunctions"]

BeginTestSection["Initialization"]

VerificationTest[(* 1 *)
	CompoundExpression[Set[$HistoryLength, 10], With[List[Set[dir, ParentDirectory[If[Quiet[TrueQ[FileExistsQ[$TestFileName]]], DirectoryName[$TestFileName], NotebookDirectory[]]]]], PacletDirectoryLoad[dir]], Quiet[Get["DualNumbers`"]], ClearAll["Global`*"], "Done"]
	,
	"Done"	
	,
	TestID->"30686f48-9f4f-4c52-9f7f-c5a149f8de17"
]

EndTestSection[]

BeginTestSection["DualTuples"]

VerificationTest[(* 2 *)
	Map[DualTuples, Range[0, 4]]
	,
	List[List[], List[List[List[1, 2]]], List[List[List[1, 2], List[2, 1]], List[List[1, 1], List[2, 2]]], List[List[List[1, 2], List[2, 1], List[3, 1]], List[List[1, 1], List[2, 2], List[3, 1]], List[List[1, 1], List[2, 1], List[3, 2]]], List[List[List[1, 2], List[2, 1], List[3, 1], List[4, 1]], List[List[1, 1], List[2, 2], List[3, 1], List[4, 1]], List[List[1, 1], List[2, 1], List[3, 2], List[4, 1]], List[List[1, 1], List[2, 1], List[3, 1], List[4, 2]]]]	
	,
	TestID->"f9ea9141-1da4-4de8-bebf-9937fb358343"
]

EndTestSection[]

EndTestSection[]
