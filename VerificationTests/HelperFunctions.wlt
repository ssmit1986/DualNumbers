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
	,
	TestID->"b00344b3-e86e-4693-99cb-f765409ac1d2"
]

VerificationTest[(* 11 *)
	List[DualApply[f, a], DualApply[List[f, g], a], DualApply[List[Function[List[f[SlotSequence[1]], g[SlotSequence[1]]]]], a]]
	,
	List[Dual[f[a], f[0]], Dual[f[a], g[0]], Dual[f[a, 0], g[a, 0]]]	
	,
	TestID->"90de102c-7eea-4479-b782-cfe46dcfa4fd"
]

VerificationTest[(* 12 *)
	List[DualApply[f][Dual[a, b]], DualApply[List[f, g]][Dual[a, b]], DualApply[List[Function[List[f[SlotSequence[1]], g[SlotSequence[1]]]]]][Dual[a, b]]]
	,
	List[Dual[f[a], f[b]], Dual[f[a], g[b]], Dual[f[a, b], g[a, b]]]	
	,
	TestID->"3c3b5616-b9c4-41e2-901a-c4babd095bf3"
]

VerificationTest[(* 13 *)
	DualApply[List[f], Dual[a, b]]
	,
	DualApply[List[f], Dual[a, b]]
	,
	{DualApply::resultlength}
	,
	TestID->"b3c5b856-bf34-4958-b832-6dc081c7c66a"
]

VerificationTest[(* 14 *)
	List[DualApply[f, Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]]], DualApply[f, Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]], List[0]], DualApply[f, Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]], List[1]], DualApply[f, Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]], List[2]]]
	,
	List[Dual[f[List[List[a[1, 1]], List[a[2, 1]], List[a[3, 1]]]], f[List[List[b[1, 1]], List[b[2, 1]], List[b[3, 1]]]]], Dual[f[List[List[a[1, 1]], List[a[2, 1]], List[a[3, 1]]]], f[List[List[b[1, 1]], List[b[2, 1]], List[b[3, 1]]]]], Dual[List[f[List[a[1, 1]]], f[List[a[2, 1]]], f[List[a[3, 1]]]], List[f[List[b[1, 1]]], f[List[b[2, 1]]], f[List[b[3, 1]]]]], Dual[List[List[f[a[1, 1]]], List[f[a[2, 1]]], List[f[a[3, 1]]]], List[List[f[b[1, 1]]], List[f[b[2, 1]]], List[f[b[3, 1]]]]]]	
	,
	TestID->"43a922d9-9916-42f4-8784-e60e5516ea21"
]

VerificationTest[(* 15 *)
	List[DualApply[List[f, g], Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]]], DualApply[List[f, g], Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]], List[0]], DualApply[List[f, g], Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]], List[1]], DualApply[List[f, g], Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]], List[2]]]
	,
	List[Dual[f[List[List[a[1, 1]], List[a[2, 1]], List[a[3, 1]]]], g[List[List[b[1, 1]], List[b[2, 1]], List[b[3, 1]]]]], Dual[f[List[List[a[1, 1]], List[a[2, 1]], List[a[3, 1]]]], g[List[List[b[1, 1]], List[b[2, 1]], List[b[3, 1]]]]], Dual[List[f[List[a[1, 1]]], f[List[a[2, 1]]], f[List[a[3, 1]]]], List[g[List[b[1, 1]]], g[List[b[2, 1]]], g[List[b[3, 1]]]]], Dual[List[List[f[a[1, 1]]], List[f[a[2, 1]]], List[f[a[3, 1]]]], List[List[g[b[1, 1]]], List[g[b[2, 1]]], List[g[b[3, 1]]]]]]	
	,
	TestID->"f4746d8a-5c01-4db7-87c0-432919b4a310"
]

VerificationTest[(* 16 *)
	List[DualApply[List[Function[List[f[SlotSequence[1]], g[SlotSequence[1]]]]], Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]]], DualApply[List[Function[List[f[SlotSequence[1]], g[SlotSequence[1]]]]], Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]], 0], DualApply[List[Function[List[f[SlotSequence[1]], g[SlotSequence[1]]]]], Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]], 1], DualApply[List[Function[List[f[SlotSequence[1]], g[SlotSequence[1]]]]], Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]], 2]]
	,
	List[Dual[f[List[List[a[1, 1]], List[a[2, 1]], List[a[3, 1]]], List[List[b[1, 1]], List[b[2, 1]], List[b[3, 1]]]], g[List[List[a[1, 1]], List[a[2, 1]], List[a[3, 1]]], List[List[b[1, 1]], List[b[2, 1]], List[b[3, 1]]]]], Dual[f[List[List[a[1, 1]], List[a[2, 1]], List[a[3, 1]]], List[List[b[1, 1]], List[b[2, 1]], List[b[3, 1]]]], g[List[List[a[1, 1]], List[a[2, 1]], List[a[3, 1]]], List[List[b[1, 1]], List[b[2, 1]], List[b[3, 1]]]]], Dual[List[f[List[a[1, 1]], List[b[1, 1]]], f[List[a[2, 1]], List[b[2, 1]]], f[List[a[3, 1]], List[b[3, 1]]]], List[g[List[a[1, 1]], List[b[1, 1]]], g[List[a[2, 1]], List[b[2, 1]]], g[List[a[3, 1]], List[b[3, 1]]]]], Dual[List[List[f[a[1, 1], b[1, 1]], f[a[2, 1], b[2, 1]], f[a[3, 1], b[3, 1]]]], List[List[g[a[1, 1], b[1, 1]], g[a[2, 1], b[2, 1]], g[a[3, 1], b[3, 1]]]]]]	
	,
	TestID->"e308c611-9731-4a4e-af39-899410dfbe4a"
]

VerificationTest[(* 17 *)
	DualApply[List[f], Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]], 2]
	,
	DualApply[List[f], Dual[List[List[a[1, 1]], List[a[2, 1]], List[a[3, 1]]], List[List[b[1, 1]], List[b[2, 1]], List[b[3, 1]]]], 2]
	,
	{DualApply::resultlength}
]

VerificationTest[(* 18 *)
	DualApply[f, Dual[a, b], List[1]]
	,
	DualApply[f, Dual[a, b], List[1]]
	,
	{DualApply::arraySpec}
	,
	TestID->"7ace9783-e0c8-48f4-8f0a-78c127cc8dff"
]

VerificationTest[(* 19 *)
	DualApply[List[Function[List[f[SlotSequence[1]], g[SlotSequence[1]]]]], Dual[Array[a, List[3, 1]], Array[b, List[3, 1]]], List[2]]
	,
	DualApply[List[Function[List[f[SlotSequence[1]], g[SlotSequence[1]]]]], Dual[List[List[a[1, 1]], List[a[2, 1]], List[a[3, 1]]], List[List[b[1, 1]], List[b[2, 1]], List[b[3, 1]]]], List[2]]
	,
	{DualApply::lvlSpec}
	,
	TestID->"1c6582f3-56c4-48fb-a278-c591976a4769"
]

EndTestSection[]

BeginTestSection["Equation solving"]

VerificationTest[(* 20 *)
	List[FindDualSolution[Equal[x, Dual[1, b]], Rule[x, 1]], FindDualSolution[Equal[x, Dual[1, b]], List[Rule[x, 1]]]]
	,
	List[List[List[Rule[x, Dual[1, b]]]], List[List[Rule[x, Dual[1, b]]]]]	
	,
	TestID->"0f9b56f2-43b3-456c-b3ea-2a9d7be84422"
]

VerificationTest[(* 21 *)
	List[FindDualSolution[Equal[Power[x, 2], Dual[2, b]], Rule[x, Sqrt[2]]], FindDualSolution[Equal[Power[x, 2], Dual[2, b]], Rule[x, Times[-1, Sqrt[2]]]], FindDualSolution[Equal[Exp[x], Dual[2, b]], Rule[x, Log[2]]], FindDualSolution[Equal[Log[x], Dual[2, b]], Rule[x, Exp[2]]]]
	,
	List[List[List[Rule[x, Dual[Sqrt[2], Times[b, Power[Times[2, Sqrt[2]], -1]]]]]], List[List[Rule[x, Dual[Times[-1, Sqrt[2]], Times[-1, Times[b, Power[Times[2, Sqrt[2]], -1]]]]]]], List[List[Rule[x, Dual[Log[2], Times[b, Power[2, -1]]]]]], List[List[Rule[x, Dual[Power[E, 2], Times[b, Power[E, 2]]]]]]]	
	,
	TestID->"14a0b9f2-a16c-4408-b192-9603298cef51"
]

VerificationTest[(* 22 *)
	Quiet[List[FindDualSolution[Equal[Power[x, 2], Dual[2.`, b]], Rule[x, Sqrt[2.`]]], FindDualSolution[Equal[Exp[x], Dual[2.`, b]], Rule[x, Log[2.`]]], FindDualSolution[Equal[Log[x], Dual[2.`, b]], Rule[x, Exp[2.`]]]]]
	,
	List[List[List[Rule[x, Dual[1.4142135623730951`, Times[0.35355339059327373`, b]]]]], List[List[Rule[x, Dual[0.6931471805599453`, Times[0.5`, b]]]]], List[List[Rule[x, Dual[7.38905609893065`, Times[7.3890560989306495`, b]]]]]]	
	,
	TestID->"983bc19e-7dd2-4dc9-8db0-c552e0a86084"
]

VerificationTest[(* 23 *)
	Quiet[List[FindDualSolution[Equal[Power[x, 2], Dual[2.`, 1.`]], Rule[x, Sqrt[2.`]]], FindDualSolution[Equal[Exp[x], Dual[2.`, 1.`]], Rule[x, Log[2.`]]], FindDualSolution[Equal[Log[x], Dual[2.`, 1.`]], Rule[x, Exp[2.`]]]]]
	,
	List[List[List[Rule[x, Dual[1.4142135623730951`, 0.35355339059327373`]]]], List[List[Rule[x, Dual[0.6931471805599453`, 0.5`]]]], List[List[Rule[x, Dual[7.38905609893065`, 7.3890560989306495`]]]]]	
	,
	TestID->"bed46822-1c97-4906-a345-a6fab85f044e"
]

VerificationTest[(* 24 *)
	Assuming[Greater[a, 0], List[FindDualSolution[Equal[Power[x, 2], Dual[a, b]], Rule[x, Sqrt[a]]], FindDualSolution[Equal[Exp[x], Dual[a, b]], Rule[x, Log[a]]], FindDualSolution[Equal[Log[x], Dual[a, b]], Rule[x, Exp[a]]]]]
	,
	List[List[List[Rule[x, Dual[Sqrt[a], Times[b, Power[Times[2, Sqrt[a]], -1]]]]]], List[List[Rule[x, Dual[Log[a], Times[b, Power[a, -1]]]]]], List[List[Rule[x, Dual[Power[E, a], Times[b, Power[E, a]]]]]]]	
	,
	TestID->"615e90c9-7e54-4264-ba16-f6a0c3b9af67"
]

VerificationTest[(* 25 *)
	FindDualSolution[Equal[x, Dual[1, b]], Rule[x, 2]]
	,
	List[List[Rule[x, Dual[2, b]]]]
	,
	{FindDualSolution::nonsol}
	,
	TestID->"0e508e37-8eb8-41bb-94b6-7c9bb6447dca"
]

VerificationTest[(* 26 *)
	CompoundExpression[Set[standardSol, FindRoot[List[Equal[Exp[Plus[x, -2]], y], Equal[Power[y, 2], x]], List[List[x, 1], List[y, 1]]]], Set[equations, List[List[Equal[Exp[Plus[x, -2]], y], Equal[Power[y, 2], x]], List[Equal[Exp[Plus[x, -2]], y], Equal[Times[Dual[1, 1], Power[y, 2]], x]], List[Equal[Exp[Plus[Times[Dual[1, 1], x], -2]], y], Equal[Power[y, 2], x]], List[Equal[Exp[Plus[x, Times[-1, Dual[2, 1]]]], y], Equal[Power[y, 2], x]], List[Equal[Exp[Plus[x, -2]], y], Equal[Power[y, Dual[2, 1]], x]], List[Equal[Exp[Plus[x, Times[-1, Dual[2, 1]]]], y], Equal[Power[y, Dual[2, 1]], x]]]], Set[sols1, Map[Function[FindDualSolution[Slot[1], standardSol]], equations]]]
	,
	List[Repeated[List[List[Rule[x, Dual[PatternTest[Blank[], NumericQ], PatternTest[Blank[], NumericQ]]], Rule[y, Dual[PatternTest[Blank[], NumericQ], PatternTest[Blank[], NumericQ]]]]]]]	
	,
	SameTest->MatchQ, TestID->"315543b7-13e2-4547-b678-71c4e5e7f356"
]

VerificationTest[(* 27 *)
	List[DualFindRoot[Equal[x, Dual[2, 1]], List[x, 1]], DualFindRoot[Equal[Power[x, 2], Dual[2, 1]], List[x, 1]], DualFindRoot[Equal[Exp[x], Dual[2, 1]], List[x, 1]], DualFindRoot[Equal[Log[x], Dual[2, 1]], List[x, 1]]]
	,
	List[List[Rule[x, Dual[2.`, 1]]], List[Rule[x, Dual[1.4142135623730951`, 0.35355339059327373`]]], List[Rule[x, Dual[0.6931471805599453`, 0.5`]]], List[Rule[x, Dual[7.389056098930651`, 7.389056098930651`]]]]	
	,
	TestID->"214622c5-e7e7-4b46-be87-a53671012ca2"
]

VerificationTest[(* 28 *)
	Set[sols2, Map[Function[DualFindRoot[Slot[1], List[List[x, 1], List[y, 1]]]], equations]]
	,
	List[List[Rule[x, Dual[0.019026016103714054`, 0.`]], Rule[y, Dual[0.13793482556524314`, 0.`]]], List[Rule[x, Dual[0.019026016103714054`, 0.019778633294869334`]], Rule[y, Dual[0.13793482556524314`, 0.0027281623334467118`]]], List[Rule[x, Dual[0.019026016103714054`, 0.0007526171911552778`]], Rule[y, Dual[0.13793482556524314`, 0.0027281623334467118`]]], List[Rule[x, Dual[0.019026016103714054`, -0.03955726658973867`]], Rule[y, Dual[0.13793482556524314`, -0.14339115023213655`]]], List[Rule[x, Dual[0.019026016103714054`, -0.039180957994161034`]], Rule[y, Dual[0.13793482556524314`, -0.005404418606403721`]]], List[Rule[x, Dual[0.019026016103714054`, -0.0787382245838997`]], Rule[y, Dual[0.13793482556524314`, -0.14879556883854028`]]]]	
	,
	TestID->"a96358af-f5b7-45f7-a063-1ab112e5fdb5"
]

VerificationTest[(* 29 *)
	Equal[Part[sols1, All, 1], sols2]
	,
	True	
	,
	TestID->"5d4de4af-4f1b-4f84-af8e-252c585b2b1f"
]

EndTestSection[]

BeginTestSection["DualFindMinimum & DualFindMaximum"]

VerificationTest[(* 30 *)
	List[DualFindMinimum[Times[Dual[1, 1], x, Cos[x]], List[x, 2]], DualFindMinimum[Times[x, Cos[Times[Dual[1, 1], x]]], List[x, 2]]]
	,
	List[List[Dual[-3.2883713955908966`, -3.2883713955908966`], List[Rule[x, Dual[3.425618459492147`, -1.0418883362877956`*^-11]]]], List[Dual[-3.2883713955908966`, 3.2883713955908966`], List[Rule[x, Dual[3.425618459492147`, -3.425618459492147`]]]]]	
	,
	TestID->"f33d863f-ebff-4abb-b0ad-f9bad0e75aa4"
]

VerificationTest[(* 31 *)
	List[DualFindMaximum[Times[Times[-1, Dual[1, 1]], x, Cos[x]], List[x, 2]], DualFindMaximum[Times[Times[-1, x], Cos[Times[Dual[1, 1], x]]], List[x, 2]]]
	,
	List[List[Dual[3.2883713955908966`, 3.2883713955908966`], List[Rule[x, Dual[3.425618459492147`, -1.0418883362877956`*^-11]]]], List[Dual[3.2883713955908966`, -3.2883713955908966`], List[Rule[x, Dual[3.425618459492147`, -3.425618459492147`]]]]]	
	,
	TestID->"29d62661-bf9e-4ce0-ad29-d4fda175f628"
]

VerificationTest[(* 32 *)
	List[DualFindMinimum[Times[Sin[x], Sin[Times[Dual[2, 1], y]]], List[List[x, 2], List[y, 2]]], DualFindMinimum[Times[Sin[Times[Dual[1, 1], x]], Sin[Times[2, y]]], List[List[x, 2], List[y, 2]]], DualFindMinimum[Plus[Dual[0, 1], Times[Sin[Times[1, x]], Sin[Times[2, y]]]], List[List[x, 2], List[y, 2]]]]
	,
	List[List[Dual[-1.`, -6.0641305537977`*^-18], List[Rule[x, Dual[1.5707963225561392`, 2.5704377344461602`*^-26]], Rule[y, Dual[2.356194488451062`, -1.1780972433548897`]]]], List[Dual[-1.`, -1.7967064353811206`*^-17], List[Rule[x, Dual[1.5707963225561392`, -1.5707963183173819`]], Rule[y, Dual[2.356194488451062`, 3.1285739149167003`*^-26]]]], List[Dual[-1.`, 1.`], List[Rule[x, Dual[1.5707963225561392`, 0.`]], Rule[y, Dual[2.356194488451062`, 0.`]]]]]	
	,
	TestID->"66b29d4b-b1b6-41b7-9dfe-d4a5a528e986"
]

VerificationTest[(* 33 *)
	List[DualFindMaximum[Times[Times[-1, Sin[x]], Sin[Times[Dual[2, 1], y]]], List[List[x, 2], List[y, 2]]], DualFindMaximum[Times[Times[-1, Sin[Times[Dual[1, 1], x]]], Sin[Times[2, y]]], List[List[x, 2], List[y, 2]]], DualFindMaximum[Plus[Times[-1, Dual[0, 1]], Times[-1, Times[Sin[Times[1, x]], Sin[Times[2, y]]]]], List[List[x, 2], List[y, 2]]]]
	,
	List[List[Dual[1.`, 6.0641305537977`*^-18], List[Rule[x, Dual[1.5707963225561392`, 2.5704377344461602`*^-26]], Rule[y, Dual[2.356194488451062`, -1.1780972433548897`]]]], List[Dual[1.`, 1.7967064353811206`*^-17], List[Rule[x, Dual[1.5707963225561392`, -1.5707963183173819`]], Rule[y, Dual[2.356194488451062`, 3.1285739149167003`*^-26]]]], List[Dual[1.`, -1.`], List[Rule[x, Dual[1.5707963225561392`, 0.`]], Rule[y, Dual[2.356194488451062`, 0.`]]]]]	
	,
	TestID->"aaff126c-2c10-4635-ac9f-3a544d2bdbaa"
]

EndTestSection[]

BeginTestSection["AddDualHandling"]

VerificationTest[(* 34 *)
	CompoundExpression[AddDualHandling[fun, funPrime], List[fun[a], fun[Dual[a, b]], fun[a1, a2], fun[Dual[a1, b1], a2], fun[a1, Dual[a2, b2]], fun[Dual[a1, b1], Dual[a2, b2]]]]
	,
	List[fun[a], Dual[fun[a], Times[b, funPrime[a]]], fun[a1, a2], fun[Dual[a1, b1], a2], fun[a1, Dual[a2, b2]], fun[Dual[a1, b1], Dual[a2, b2]]]	
	,
	TestID->"24e3267b-6b21-49c9-808a-d31aa5871741"
]

VerificationTest[(* 35 *)
	CompoundExpression[AddDualHandling[fun, Array[funPrime, 2]], List[fun[a], fun[Dual[a, b]], fun[a1, a2], fun[Dual[a1, b1], a2], fun[a1, Dual[a2, b2]], fun[Dual[a1, b1], Dual[a2, b2]]]]
	,
	List[fun[a], Dual[fun[a], Times[b, funPrime[a]]], fun[a1, a2], Dual[fun[a1, a2], Times[b1, funPrime[1][a1, a2]]], Dual[fun[a1, a2], Times[b2, funPrime[2][a1, a2]]], Dual[fun[a1, a2], Plus[Times[b1, funPrime[1][a1, a2]], Times[b2, funPrime[2][a1, a2]]]]]	
	,
	TestID->"1f79cfd6-96c2-402d-8855-17a3eeaaa516"
]

VerificationTest[(* 36 *)
	CompoundExpression[AddDualHandling[fun2, 1], List[fun2[a], fun2[Dual[a, b]], fun2[a1, a2], fun2[Dual[a1, b1], a2], fun2[a1, Dual[a2, b2]], fun2[Dual[a1, b1], Dual[a2, b2]]]]
	,
	List[fun2[a], Dual[fun2[a], Times[b, Derivative[1][fun2][a]]], fun2[a1, a2], fun2[Dual[a1, b1], a2], fun2[a1, Dual[a2, b2]], fun2[Dual[a1, b1], Dual[a2, b2]]]	
	,
	TestID->"be5d31f6-1da0-4fe7-829e-1db0a1f63116"
]

VerificationTest[(* 37 *)
	CompoundExpression[AddDualHandling[fun2, 2], List[fun2[a], fun2[Dual[a, b]], fun2[a1, a2], fun2[Dual[a1, b1], a2], fun2[a1, Dual[a2, b2]], fun2[Dual[a1, b1], Dual[a2, b2]]]]
	,
	List[fun2[a], Dual[fun2[a], Times[b, Derivative[1][fun2][a]]], fun2[a1, a2], Dual[fun2[a1, a2], Times[b1, Derivative[1, 0][fun2][a1, a2]]], Dual[fun2[a1, a2], Times[b2, Derivative[0, 1][fun2][a1, a2]]], Dual[fun2[a1, a2], Plus[Times[b2, Derivative[0, 1][fun2][a1, a2]], Times[b1, Derivative[1, 0][fun2][a1, a2]]]]]	
	,
	TestID->"786f0819-448c-4d99-945a-8c13eabc89f9"
]

EndTestSection[]

BeginTestSection["End"]

EndTestSection[]

EndTestSection[]
