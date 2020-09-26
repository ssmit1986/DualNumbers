BeginTestSection["DualNumbers"]

BeginTestSection["Initialization"]

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
	Dual[List[1], List[2, 3]]
	,
	Dual[List[1], List[2, 3]]
	,
	{Dual::array}
	,
	TestID->"77c69076-76ed-42f7-8d97-cf9bb3d3c055"
]

VerificationTest[(* 7 *)
	Dual[1, 2, 3]
	,
	Dual[1, 2, 3]
	,
	{Dual::argt}
	,
	TestID->"80c8281c-83ec-4962-a10c-221fa9f7004b"
]

VerificationTest[(* 8 *)
	List[ToDual[1, 2], ToDual[List[1], 2], ToDual[List[Dual[1], 1], 2], ToDual[1, List[2, 3]], ToDual[1, SparseArray[List[2, 3]]]]
	,
	List[Dual[1, 2], Dual[List[1], List[2]], Dual[List[1, 1], List[0, 2]], Dual[List[1, 1], List[2, 3]], Dual[SparseArray[Automatic, List[2], 1, List[1, List[List[0, 0], List[]], List[]]], SparseArray[Automatic, List[2], 0, List[1, List[List[0, 2], List[List[1], List[2]]], List[2, 3]]]]]	
	,
	TestID->"38ad5d2a-af92-453f-9914-f8efe977fc20"
]

VerificationTest[(* 9 *)
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

VerificationTest[(* 10 *)
	List[Dual[Dual[a, b], c], Dual[a, Dual[c, d]], Dual[Dual[a, b], Dual[c, d]]]
	,
	List[Dual[a, Plus[b, c]], Dual[a, c], Dual[a, Plus[b, c]]]	
	,
	TestID->"4c321ca7-051b-46d9-a095-af99e69a65c8"
]

VerificationTest[(* 11 *)
	List[D[Dual[f[x], g[y]], x], D[Dual[f[x], g[y]], y], D[Dual[f[x, y], g[x, y]], List[List[x, y]]]]
	,
	List[Derivative[1][f][x], Dual[0, Derivative[1][g][y]], List[Dual[Derivative[1, 0][f][x, y], Derivative[1, 0][g][x, y]], Dual[Derivative[0, 1][f][x, y], Derivative[0, 1][g][x, y]]]]	
	,
	TestID->"e752d07a-1151-4fd8-bf34-570d9e99dd40"
]

VerificationTest[(* 12 *)
	List[Standard[Dual[a, b]], NonStandard[Dual[a, b]], Standard[a], NonStandard[a]]
	,
	List[a, b, a, 0]	
	,
	TestID->"f19cabd3-30a7-4d3c-9b22-39b696d33b8a"
]

EndTestSection[]

BeginTestSection["Type verification"]

VerificationTest[(* 13 *)
	CompoundExpression[Set[testValues, List[1, Dual[0, 1], Dual[1, 0], Dual[List[0, 1], List[2, 3]], Dual[List[List[0, 1]], List[List[2, 3]]], List[Dual[0, 2], Dual[1, 3]], List[Dual[0, 2], List[Dual[1, 3]]], List[Dual[0, 2], 1], List[0, 1]]], AssociationMap[DualQ, testValues]]
	,
	Association[Rule[1, False], Rule[Dual[0, 1], True], Rule[Dual[1, 0], True], Rule[Dual[List[0, 1], List[2, 3]], True], Rule[Dual[List[List[0, 1]], List[List[2, 3]]], True], Rule[List[Dual[0, 2], Dual[1, 3]], False], Rule[List[Dual[0, 2], List[Dual[1, 3]]], False], Rule[List[Dual[0, 2], 1], False], Rule[List[0, 1], False]]	
	,
	TestID->"a5e32721-7666-452f-880e-17f429e50f98"
]

VerificationTest[(* 14 *)
	AssociationMap[DualScalarQ, testValues]
	,
	Association[Rule[1, False], Rule[Dual[0, 1], True], Rule[Dual[1, 0], True], Rule[Dual[List[0, 1], List[2, 3]], False], Rule[Dual[List[List[0, 1]], List[List[2, 3]]], False], Rule[List[Dual[0, 2], Dual[1, 3]], False], Rule[List[Dual[0, 2], List[Dual[1, 3]]], False], Rule[List[Dual[0, 2], 1], False], Rule[List[0, 1], False]]	
	,
	TestID->"e9013855-ed21-4b42-91fa-570483ef2b95"
]

VerificationTest[(* 15 *)
	AssociationMap[StandardQ, testValues]
	,
	Association[Rule[1, True], Rule[Dual[0, 1], False], Rule[Dual[1, 0], False], Rule[Dual[List[0, 1], List[2, 3]], False], Rule[Dual[List[List[0, 1]], List[List[2, 3]]], False], Rule[List[Dual[0, 2], Dual[1, 3]], True], Rule[List[Dual[0, 2], List[Dual[1, 3]]], True], Rule[List[Dual[0, 2], 1], True], Rule[List[0, 1], True]]	
	,
	TestID->"a75a8c93-81f8-4613-9791-42e7b1f53f1e"
]

VerificationTest[(* 16 *)
	AssociationMap[UnpackedDualArrayQ, testValues]
	,
	Association[Rule[1, False], Rule[Dual[0, 1], False], Rule[Dual[1, 0], False], Rule[Dual[List[0, 1], List[2, 3]], False], Rule[Dual[List[List[0, 1]], List[List[2, 3]]], False], Rule[List[Dual[0, 2], Dual[1, 3]], True], Rule[List[Dual[0, 2], List[Dual[1, 3]]], False], Rule[List[Dual[0, 2], 1], False], Rule[List[0, 1], False]]	
	,
	TestID->"af6eba64-c63a-4afb-b49b-4a23de45cea7"
]

VerificationTest[(* 17 *)
	AssociationMap[DualFreeArrayQ, testValues]
	,
	Association[Rule[1, False], Rule[Dual[0, 1], False], Rule[Dual[1, 0], False], Rule[Dual[List[0, 1], List[2, 3]], False], Rule[Dual[List[List[0, 1]], List[List[2, 3]]], False], Rule[List[Dual[0, 2], Dual[1, 3]], False], Rule[List[Dual[0, 2], List[Dual[1, 3]]], False], Rule[List[Dual[0, 2], 1], False], Rule[List[0, 1], True]]	
	,
	TestID->"f19aa1b7-b932-458a-8218-a48c3183a732"
]

EndTestSection[]

BeginTestSection["Packing and unpacking"]

VerificationTest[(* 18 *)
	CompoundExpression[Set[arrays, List[List[], List[List[]], List[Dual[1, 2]], List[List[Dual[1, 2]]]]], Set[packedArrays, Map[PackDualArray, arrays]]]
	,
	List[Dual[List[], List[]], Dual[List[List[]], List[List[]]], Dual[List[1], List[2]], Dual[List[List[1]], List[List[2]]]]	
	,
	TestID->"bea93e5d-4ea8-413f-9c79-a65ba06c8d0b"
]

VerificationTest[(* 19 *)
	SameQ[Map[PackDualArray, packedArrays], packedArrays]
	,
	True	
	,
	TestID->"9daf85e7-8cce-4ce1-ad9a-b6f940199ca8"
]

VerificationTest[(* 20 *)
	PackDualArray[List[1, List[2]]]
	,
	List[1, List[2]]
	,
	{PackDualArray::arrayQ}
	,
	TestID->"0af5fc63-90b3-46ee-8659-4d2abb65afc3"
]

VerificationTest[(* 21 *)
	SameQ[Map[UnpackDualArray, packedArrays], arrays]
	,
	True	
	,
	TestID->"6905b47f-3aa6-4e15-b6fc-a024625e149c"
]

VerificationTest[(* 22 *)
	UnpackDualArray[Dual[1, 2]]
	,
	Dual[1, 2]
	,
	{UnpackDualArray::notArray}
	,
	TestID->"8abc302c-78fd-48dc-ac5d-7907eb47a76f"
]

VerificationTest[(* 23 *)
	UnpackDualArray[Dual[List[1], List[List[2]]]]
	,
	Dual[List[1], List[List[2]]]
	,
	{Dual::array, UnpackDualArray::badarray}
	,
	TestID->"ff2bb1be-a7c2-40d1-939d-d1f93ed866f3"
]

VerificationTest[(* 24 *)
	UnpackDualArray[List[]]
	,
	List[]
	,
	{UnpackDualArray::badarray}
	,
	TestID->"ac548930-8edf-42e2-b141-d69c02b39224"
]

VerificationTest[(* 25 *)
	UnpackDualArray[List[Dual[1, 2]]]
	,
	List[Dual[1, 2]]
	,
	{UnpackDualArray::badarray}
	,
	TestID->"7433757c-e92e-4bcb-b97b-0647cfaed159"
]

VerificationTest[(* 26 *)
	Module[List[res], CompoundExpression[On["Packing"], Set[res, UnpackDualArray[Dual[List[1], List[2]]]], Off["Packing"], res]]
	,
	List[Dual[1, 2]]
	,
	{UnpackDualArray::unpack}
	,
	TestID->"3b73554c-17cc-4e7c-a782-94d26b7fc4da"
]

VerificationTest[(* 27 *)
	CompoundExpression[Set[largePackedArray, Dual[RandomReal[1, Power[10, 6]], RandomReal[1, Power[10, 6]]]], Set[largeUnPackedArray, UnpackDualArray[largePackedArray]], MatchQ[largeUnPackedArray, List[BlankSequence[Dual]]]]
	,
	True	
	,
	TimeConstraint->10, TestID->"99560401-d610-495e-bbf3-17b0becdc712"
]

VerificationTest[(* 28 *)
	With[List[Set[tests, List[DualQ, DualScalarQ, StandardQ, DualArrayQ, UnpackedDualArrayQ, DualFreeArrayQ]], Set[arr, RandomReal[1, Power[10, 6]]]], List[AssociationMap[Function[Slot[1][largePackedArray]], tests], AssociationMap[Function[Slot[1][largeUnPackedArray]], tests], AssociationMap[Function[Slot[1][arr]], tests]]]
	,
	List[Association[Rule[DualQ, True], Rule[DualScalarQ, False], Rule[StandardQ, False], Rule[DualArrayQ, True], Rule[UnpackedDualArrayQ, False], Rule[DualFreeArrayQ, False]], Association[Rule[DualQ, False], Rule[DualScalarQ, False], Rule[StandardQ, True], Rule[DualArrayQ, False], Rule[UnpackedDualArrayQ, True], Rule[DualFreeArrayQ, False]], Association[Rule[DualQ, False], Rule[DualScalarQ, False], Rule[StandardQ, True], Rule[DualArrayQ, False], Rule[UnpackedDualArrayQ, False], Rule[DualFreeArrayQ, True]]]	
	,
	TestID->"50df5c1e-7003-41b9-850d-916fe5680919", TimeConstraint->5
]

EndTestSection[]

EndTestSection[]
