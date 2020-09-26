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
	Map[DualNumbers`Private`dualTuplesPositions, Range[0, 4]]
	,
	List[List[], List[List[List[1, 2]]], List[List[List[1, 2], List[2, 1]], List[List[1, 1], List[2, 2]]], List[List[List[1, 2], List[2, 1], List[3, 1]], List[List[1, 1], List[2, 2], List[3, 1]], List[List[1, 1], List[2, 1], List[3, 2]]], List[List[List[1, 2], List[2, 1], List[3, 1], List[4, 1]], List[List[1, 1], List[2, 2], List[3, 1], List[4, 1]], List[List[1, 1], List[2, 1], List[3, 2], List[4, 1]], List[List[1, 1], List[2, 1], List[3, 1], List[4, 2]]]]	
	,
	TestID->"f9ea9141-1da4-4de8-bebf-9937fb358343"
]

VerificationTest[(* 3 *)
	DualNumbers`Private`dualTuplesPositions[0, 1]
	,
	List[]	
	,
	TestID->"3f70d915-a97f-4ab5-bda9-568b3b77dc83"
]

VerificationTest[(* 4 *)
	SameQ[DualNumbers`Private`dualTuplesPositions[4], Map[Function[DualNumbers`Private`dualTuplesPositions[4, Slot[1]]], Range[4]]]
	,
	True	
	,
	TestID->"56dbc595-4041-4ea7-a9f7-973136b359e4"
]

VerificationTest[(* 5 *)
	SameQ[Reverse[DualNumbers`Private`dualTuplesPositions[4]], Map[Function[DualNumbers`Private`dualTuplesPositions[4, Times[-1, Slot[1]]]], Range[4]]]
	,
	True	
	,
	TestID->"5a073783-ff22-48ad-9727-72957ab829f8"
]

VerificationTest[(* 6 *)
	CompoundExpression[Set[dualListPacked, Dual[Array[a, 10], Array[b, 10]]], Set[dualList, UnpackDualArray[dualListPacked]], List[AssociationMap[Function[DualTuples[Take[dualList, Slot[1]]]], Range[0, 4]], AssociationMap[Function[DualTuples[Take[dualListPacked, Slot[1]]]], Range[0, 4]]]]
	,
	List[Association[Rule[0, List[]], Rule[1, List[List[b[1]]]], Rule[2, List[List[b[1], a[2]], List[a[1], b[2]]]], Rule[3, List[List[b[1], a[2], a[3]], List[a[1], b[2], a[3]], List[a[1], a[2], b[3]]]], Rule[4, List[List[b[1], a[2], a[3], a[4]], List[a[1], b[2], a[3], a[4]], List[a[1], a[2], b[3], a[4]], List[a[1], a[2], a[3], b[4]]]]], Association[Rule[0, List[]], Rule[1, List[List[b[1]]]], Rule[2, List[List[b[1], a[2]], List[a[1], b[2]]]], Rule[3, List[List[b[1], a[2], a[3]], List[a[1], b[2], a[3]], List[a[1], a[2], b[3]]]], Rule[4, List[List[b[1], a[2], a[3], a[4]], List[a[1], b[2], a[3], a[4]], List[a[1], a[2], b[3], a[4]], List[a[1], a[2], a[3], b[4]]]]]]	
	,
	TestID->"0073410b-f0f3-47cd-9b3f-d0463b39086a"
]

VerificationTest[(* 7 *)
	List[AssociationMap[Function[DualTuplesReduce[Take[dualList, Slot[1]], f]], Range[0, 4]], AssociationMap[Function[DualTuplesReduce[Take[dualListPacked, Slot[1]], f]], Range[0, 4]]]
	,
	List[Association[Rule[0, List[]], Rule[1, List[f[b[1]]]], Rule[2, List[f[b[1], a[2]], f[a[1], b[2]]]], Rule[3, List[f[b[1], a[2], a[3]], f[a[1], b[2], a[3]], f[a[1], a[2], b[3]]]], Rule[4, List[f[b[1], a[2], a[3], a[4]], f[a[1], b[2], a[3], a[4]], f[a[1], a[2], b[3], a[4]], f[a[1], a[2], a[3], b[4]]]]], Association[Rule[0, List[]], Rule[1, List[f[b[1]]]], Rule[2, List[f[b[1], a[2]], f[a[1], b[2]]]], Rule[3, List[f[b[1], a[2], a[3]], f[a[1], b[2], a[3]], f[a[1], a[2], b[3]]]], Rule[4, List[f[b[1], a[2], a[3], a[4]], f[a[1], b[2], a[3], a[4]], f[a[1], a[2], b[3], a[4]], f[a[1], a[2], a[3], b[4]]]]]]	
	,
	TestID->"9808cb57-a24f-43d7-bb90-86469ce74498"
]

VerificationTest[(* 8 *)
	List[AssociationMap[Function[DualTuplesReduce[Take[dualList, Slot[1]], f, g]], Range[0, 2]], AssociationMap[Function[DualTuplesReduce[Take[dualListPacked, Slot[1]], f, g]], Range[0, 2]]]
	,
	List[Association[Rule[0, g[]], Rule[1, g[f[b[1]]]], Rule[2, g[g[f[b[1], a[2]]], f[a[1], b[2]]]]], Association[Rule[0, g[]], Rule[1, g[f[b[1]]]], Rule[2, g[g[f[b[1], a[2]]], f[a[1], b[2]]]]]]	
	,
	TestID->"1ce565c0-3d88-470d-80b3-25b3090bb4fd"
]

VerificationTest[(* 9 *)
	List[AssociationMap[Function[DualTuplesReduce[Take[dualList, Slot[1]], Times, Plus]], Range[0, 4]], AssociationMap[Function[DualTuplesReduce[Take[dualListPacked, Slot[1]], Times, Plus]], Range[0, 4]]]
	,
	List[Association[Rule[0, 0], Rule[1, b[1]], Rule[2, Plus[Times[a[2], b[1]], Times[a[1], b[2]]]], Rule[3, Plus[Times[a[2], a[3], b[1]], Times[a[1], a[3], b[2]], Times[a[1], a[2], b[3]]]], Rule[4, Plus[Times[a[2], a[3], a[4], b[1]], Times[a[1], a[3], a[4], b[2]], Times[a[1], a[2], a[4], b[3]], Times[a[1], a[2], a[3], b[4]]]]], Association[Rule[0, 0], Rule[1, b[1]], Rule[2, Plus[Times[a[2], b[1]], Times[a[1], b[2]]]], Rule[3, Plus[Times[a[2], a[3], b[1]], Times[a[1], a[3], b[2]], Times[a[1], a[2], b[3]]]], Rule[4, Plus[Times[a[2], a[3], a[4], b[1]], Times[a[1], a[3], a[4], b[2]], Times[a[1], a[2], a[4], b[3]], Times[a[1], a[2], a[3], b[4]]]]]]	
	,
	TestID->"5d96e02e-4125-4d95-8827-294d67b93017"
]

EndTestSection[]

BeginTestSection["DualApply"]

VerificationTest[(* 10 *)
	List[DualApply[f, Dual[a, b]], DualApply[List[f, g], Dual[a, b]], DualApply[List[Function[List[f[SlotSequence[1]], g[SlotSequence[1]]]]], Dual[a, b]]]
	,
	List[Dual[f[a], f[b]], Dual[f[a], g[b]], Dual[f[a, b], g[a, b]]]	
]

VerificationTest[(* 11 *)
	List[DualApply[f, a], DualApply[List[f, g], a], DualApply[List[Function[List[f[SlotSequence[1]], g[SlotSequence[1]]]]], a]]
	,
	List[Dual[f[a], f[0]], Dual[f[a], g[0]], Dual[f[a, 0], g[a, 0]]]	
]

VerificationTest[(* 12 *)
	List[DualApply[f][Dual[a, b]], DualApply[List[f, g]][Dual[a, b]], DualApply[List[Function[List[f[SlotSequence[1]], g[SlotSequence[1]]]]]][Dual[a, b]]]
	,
	List[Dual[f[a], f[b]], Dual[f[a], g[b]], Dual[f[a, b], g[a, b]]]	
]

VerificationTest[(* 13 *)
	DualApply[List[f], Dual[a, b]]
	,
	DualApply[List[f], Dual[a, b]]
	,
	{DualApply::resultlength}
]

EndTestSection[]

EndTestSection[]
