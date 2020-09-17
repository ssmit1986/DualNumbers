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

VerificationTest[(* 3 *)
	List[Dual[List[]], Dual[List[1]], Dual[List[List[1]]], Dual[SparseArray[List[1]]], Dual[List[], List[]], Dual[List[0], List[1]]]
	,
	List[Dual[List[], List[]], Dual[List[1], List[0]], Dual[List[List[1]], List[List[0]]], Dual[SparseArray[Automatic, List[1], 0, List[1, List[List[0, 1], List[List[1]]], List[1]]], SparseArray[Automatic, List[1], 0, List[1, List[List[0, 0], List[]], List[]]]], Dual[List[], List[]], Dual[List[0], List[1]]]	
	,
	TestID->"2cf6f84c-11a7-45e4-b0c5-b9941eb9bbb2"
]

VerificationTest[(* 4 *)
	Dual[1, List[1]]
	,
	Dual[1, List[1]]
	,
	{Dual::array}
	,
	TestID->"fa2600b1-49d2-4345-826f-d458cf7c02fb"
]

VerificationTest[(* 5 *)
	Dual[List[1], 1]
	,
	Dual[List[1], 1]
	,
	{Dual::array}
	,
	TestID->"a747e4f6-f009-4c4b-b753-3170280fb849"
]

VerificationTest[(* 6 *)
	List[ToDual[1, 2], ToDual[List[1], 2], ToDual[List[Dual[1], 1], 2], ToDual[1, List[2, 3]], ToDual[1, SparseArray[List[2, 3]]]]
	,
	List[Dual[1, 2], Dual[List[1], List[2]], Dual[List[1, 1], List[0, 2]], Dual[List[1, 1], List[2, 3]], Dual[SparseArray[Automatic, List[2], 1, List[1, List[List[0, 0], List[]], List[]]], SparseArray[Automatic, List[2], 0, List[1, List[List[0, 2], List[List[1], List[2]]], List[2, 3]]]]]	
	,
	TestID->"38ad5d2a-af92-453f-9914-f8efe977fc20"
]

VerificationTest[(* 7 *)
	ToDual[1, List[Dual[0, 1]]]
	,
	ToDual[1, List[Dual[0, 1]]]
	,
	{ToDual::cons}
	,
	TestID->"a00d4446-0caa-4dc5-93db-dff0ea5a4559"
]

EndTestSection[]

BeginTestSection["Elementary properties"]

VerificationTest[(* 8 *)
	List[Dual[Dual[a, b], c], Dual[a, Dual[c, d]], Dual[Dual[a, b], Dual[c, d]]]
	,
	List[Dual[a, Plus[b, c]], Dual[a, c], Dual[a, Plus[b, c]]]	
	,
	TestID->"4c321ca7-051b-46d9-a095-af99e69a65c8"
]

VerificationTest[(* 9 *)
	List[D[Dual[f[x], g[y]], x], D[Dual[f[x], g[y]], y], D[Dual[f[x, y], g[x, y]], List[List[x, y]]]]
	,
	List[Derivative[1][f][x], Dual[0, Derivative[1][g][y]], List[Dual[Derivative[1, 0][f][x, y], Derivative[1, 0][g][x, y]], Dual[Derivative[0, 1][f][x, y], Derivative[0, 1][g][x, y]]]]	
	,
	TestID->"e752d07a-1151-4fd8-bf34-570d9e99dd40"
]

EndTestSection[]

BeginTestSection["Boolean tests functions"]

VerificationTest[(* 10 *)
	CompoundExpression[Set[testValues, List[1, Dual[0, 1], Dual[1, 0], Dual[List[0, 1], List[2, 3]], Dual[List[List[0, 1]], List[List[2, 3]]], List[Dual[0, 2], Dual[1, 3]], List[Dual[0, 2], List[Dual[1, 3]]], List[Dual[0, 2], 1], List[0, 1]]], AssociationMap[DualQ, testValues]]
	,
	Association[Rule[1, False], Rule[Dual[0, 1], True], Rule[Dual[1, 0], True], Rule[Dual[List[0, 1], List[2, 3]], True], Rule[Dual[List[List[0, 1]], List[List[2, 3]]], True], Rule[List[Dual[0, 2], Dual[1, 3]], False], Rule[List[Dual[0, 2], List[Dual[1, 3]]], False], Rule[List[Dual[0, 2], 1], False], Rule[List[0, 1], False]]	
	,
	TestID->"a5e32721-7666-452f-880e-17f429e50f98"
]

VerificationTest[(* 11 *)
	AssociationMap[DualScalarQ, testValues]
	,
	Association[Rule[1, False], Rule[Dual[0, 1], True], Rule[Dual[1, 0], True], Rule[Dual[List[0, 1], List[2, 3]], False], Rule[Dual[List[List[0, 1]], List[List[2, 3]]], False], Rule[List[Dual[0, 2], Dual[1, 3]], False], Rule[List[Dual[0, 2], List[Dual[1, 3]]], False], Rule[List[Dual[0, 2], 1], False], Rule[List[0, 1], False]]	
	,
	TestID->"e9013855-ed21-4b42-91fa-570483ef2b95"
]

VerificationTest[(* 12 *)
	AssociationMap[StandardQ, testValues]
	,
	Association[Rule[1, True], Rule[Dual[0, 1], False], Rule[Dual[1, 0], False], Rule[Dual[List[0, 1], List[2, 3]], False], Rule[Dual[List[List[0, 1]], List[List[2, 3]]], False], Rule[List[Dual[0, 2], Dual[1, 3]], True], Rule[List[Dual[0, 2], List[Dual[1, 3]]], True], Rule[List[Dual[0, 2], 1], True], Rule[List[0, 1], True]]	
	,
	TestID->"a75a8c93-81f8-4613-9791-42e7b1f53f1e"
]

VerificationTest[(* 13 *)
	AssociationMap[UnpackedDualArrayQ, testValues]
	,
	Association[Rule[1, False], Rule[Dual[0, 1], False], Rule[Dual[1, 0], False], Rule[Dual[List[0, 1], List[2, 3]], False], Rule[Dual[List[List[0, 1]], List[List[2, 3]]], False], Rule[List[Dual[0, 2], Dual[1, 3]], True], Rule[List[Dual[0, 2], List[Dual[1, 3]]], False], Rule[List[Dual[0, 2], 1], False], Rule[List[0, 1], False]]	
	,
	TestID->"af6eba64-c63a-4afb-b49b-4a23de45cea7"
]

VerificationTest[(* 14 *)
	AssociationMap[DualFreeArrayQ, testValues]
	,
	Association[Rule[1, False], Rule[Dual[0, 1], False], Rule[Dual[1, 0], False], Rule[Dual[List[0, 1], List[2, 3]], False], Rule[Dual[List[List[0, 1]], List[List[2, 3]]], False], Rule[List[Dual[0, 2], Dual[1, 3]], False], Rule[List[Dual[0, 2], List[Dual[1, 3]]], False], Rule[List[Dual[0, 2], 1], False], Rule[List[0, 1], True]]	
	,
	TestID->"f19aa1b7-b932-458a-8218-a48c3183a732"
]

EndTestSection[]

EndTestSection[]
