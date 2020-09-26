BeginTestSection["UpValues"]

BeginTestSection["Initialization"]

VerificationTest[(* 1 *)
	CompoundExpression[Set[$HistoryLength, 10], With[List[Set[dir, ParentDirectory[If[Quiet[TrueQ[FileExistsQ[$TestFileName]]], DirectoryName[$TestFileName], NotebookDirectory[]]]]], PacletDirectoryLoad[dir]], Quiet[Get["DualNumbers`"]], ClearAll["Global`*"], "Done"]
	,
	"Done"	
	,
	TestID->"1f40eff0-d18a-438b-bc07-9b8653e6ae51"
]

EndTestSection[]

BeginTestSection["Plus, Times, Power"]

VerificationTest[(* 2 *)
	List[Plus[Dual[1, 2], 3], Plus[Dual[1, 2], Dual[3, 4]], Plus[Dual[List[1], List[2]], 3], Plus[Dual[List[1], List[2]], Dual[List[3], List[4]]], Plus[Dual[a1, b1], Dual[a2, b2]]]
	,
	List[Dual[4, 2], Dual[4, 6], Dual[List[4], List[2]], Dual[List[4], List[6]], Dual[Plus[a1, a2], Plus[b1, b2]]]	
	,
	TestID->"1c34a7ec-32a6-4988-8dcf-b4667c102eb3"
]

VerificationTest[(* 3 *)
	List[Times[Dual[1, 2], 3], Times[Dual[1, 2], Dual[3, 4]], Times[Dual[List[1], List[2]], 3], Times[Dual[List[1], List[2]], Dual[List[3], List[4]]], Times[Dual[a1, b1], Dual[a2, b2]]]
	,
	List[Dual[3, 6], Dual[3, 10], Dual[List[3], List[6]], Dual[List[3], List[10]], Dual[Times[a1, a2], Plus[Times[a2, b1], Times[a1, b2]]]]	
	,
	TestID->"04375380-f83b-4293-83ee-f30c1500d280"
]

VerificationTest[(* 4 *)
	Power[0, Dual[0, 1]]
	,
	Dual[Indeterminate, Indeterminate]
	,
	{Power::indet, Power::indet}
	,
	TestID->"ef3b9e7c-1901-4391-ad48-aca96b7c5c76"
]

VerificationTest[(* 5 *)
	Power[Dual[0, 1], 0]
	,
	Dual[Indeterminate, Indeterminate]
	,
	{Power::indet, Power::infy, Infinity::indet}
	,
	TestID->"71c987d7-b03d-40fe-ac61-606e190a8ed2"
]

VerificationTest[(* 6 *)
	Power[Dual[0, 1], Dual[0, 1]]
	,
	Dual[Indeterminate, Indeterminate]
	,
	{Power::indet, Power::infy, Infinity::indet, Power::indet}
	,
	TestID->"e7ca5c2c-9988-4b1a-aeed-ea6353b16463"
]

VerificationTest[(* 7 *)
	CompoundExpression[Set[testValuesSym, Join[List[\[FormalX][1], \[FormalX][2]], UnpackDualArray[Dual[Array[\[FormalA], List[10]], Array[\[FormalB], List[10]]]]]], Set[testValuesNum, BlockRandom[CompoundExpression[SeedRandom[1], UnpackDualArray[Dual[RandomReal[List[-2, 2], 50], RandomReal[List[-2, 2], 50]]]]]], List[Apply[Plus, testValuesSym], Apply[Plus, testValuesNum], DualApply[Expand][Apply[Times, Part[testValuesSym, Span[1, 4]]]], Apply[Times, testValuesNum]]]
	,
	List[Dual[Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalA][6], \[FormalA][7], \[FormalA][8], \[FormalA][9], \[FormalA][10], \[FormalX][1], \[FormalX][2]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5], \[FormalB][6], \[FormalB][7], \[FormalB][8], \[FormalB][9], \[FormalB][10]]], Dual[-8.42866398437041`, -5.969132875714167`], Dual[Times[\[FormalA][1], \[FormalA][2], \[FormalX][1], \[FormalX][2]], Plus[Times[\[FormalA][2], \[FormalB][1], \[FormalX][1], \[FormalX][2]], Times[\[FormalA][1], \[FormalB][2], \[FormalX][1], \[FormalX][2]]]], Dual[-4.023358697120797`*^-9, 5.5480408466810513`*^-8]]	
	,
	TestID->"b1dc5aa4-58a9-4f26-8a74-f453ada0ee3f"
]

VerificationTest[(* 8 *)
	List[Equal[Fold[Plus, testValuesNum], Apply[Plus, testValuesNum]], Equal[Fold[Times, testValuesNum], Apply[Times, testValuesNum]], Equal[NonStandard[Fold[Times, testValuesNum]], Total[Apply[Times, DualTuples[testValuesNum], List[1]]]]]
	,
	List[True, True, True]	
	,
	TestID->"26171df0-0069-4981-8d67-5e2e2efd3d0e"
]

VerificationTest[(* 9 *)
	CompoundExpression[Set[largeTestArray, Join[UnpackDualArray[Apply[Dual, RandomReal[List[0.99`, 1.01`], List[2, 10000]]]], RandomReal[List[0.99`, 1.01`], List[10000]]]], List[TimeConstrained[Equal[Apply[Plus, largeTestArray], Fold[Plus, largeTestArray]], 1], TimeConstrained[Equal[Apply[Times, largeTestArray], Fold[Times, largeTestArray]], 1]]]
	,
	List[True, True]	
	,
	TestID->"1f06c788-1bed-49b6-b76f-c018c57a1320"
]

EndTestSection[]

BeginTestSection["Arrays"]

BeginTestSection["MatrixPower"]

VerificationTest[(* 10 *)
	CompoundExpression[Set[angle, Times[Pi, Power[7, -1]]], Set[mat, RotationMatrix[angle]], List[MatrixPower[N[mat], 1000], MatrixPower[N[mat, 20], 1000], MatrixPower[N[mat, 50], 1000]]]
	,
	ConstantArray[RotationMatrix[Times[1000, angle]], 3]	
	,
	SameTest->Function[Max[Abs[#1 - #2]]<10^(-10)], TimeConstraint->2, TestID->"7175ef12-fab3-41c8-9090-8d7b0294c2db"
]

EndTestSection[]

BeginTestSection["Fold"]

VerificationTest[(* 11 *)
	List[DualApply[Expand][Fold[Plus, Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Times, Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Plus, \[FormalX], Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Times, \[FormalX], Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]]]
	,
	List[Dual[Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5]]], Dual[Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[Times[\[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][1]], Times[\[FormalA][1], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][4], \[FormalA][5], \[FormalB][3]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][5], \[FormalB][4]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalB][5]]]], Dual[Plus[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5]]], Dual[Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][4], \[FormalA][5], \[FormalB][3]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][5], \[FormalB][4]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalB][5]]]]]	
	,
	TestID->"9113ab76-b211-4f2b-b03b-085cd044cdb3"
]

VerificationTest[(* 12 *)
	List[DualApply[Expand][FoldList[Plus, Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Times, Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Plus, \[FormalX], Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Times, \[FormalX], Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]]]
	,
	List[Dual[List[\[FormalA][1], Plus[\[FormalA][1], \[FormalA][2]], Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[\[FormalB][1], Plus[\[FormalB][1], \[FormalB][2]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3]]]], Dual[List[\[FormalA][1], Times[\[FormalA][1], \[FormalA][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[\[FormalB][1], Plus[Times[\[FormalA][2], \[FormalB][1]], Times[\[FormalA][1], \[FormalB][2]]], Plus[Times[\[FormalA][2], \[FormalA][3], \[FormalB][1]], Times[\[FormalA][1], \[FormalA][3], \[FormalB][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalB][3]]]]], Dual[List[\[FormalX], Plus[\[FormalX], \[FormalA][1]], Plus[\[FormalX], \[FormalA][1], \[FormalA][2]], Plus[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[0, \[FormalB][1], Plus[\[FormalB][1], \[FormalB][2]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3]]]], Dual[List[\[FormalX], Times[\[FormalX], \[FormalA][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[0, Times[\[FormalX], \[FormalB][1]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalB][2]]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalA][3], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][3], \[FormalB][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalB][3]]]]]]	
	,
	TestID->"4335f9a5-34b4-4e16-a2a0-85b44a4495bf"
]

VerificationTest[(* 13 *)
	Fold[Times, Dual[1, 2]]
	,
	2
	,
	{Dual::arrayOp}
	,
	TestID->"365ecee2-bd31-40d4-87d2-f63e4feab3df"
]

VerificationTest[(* 14 *)
	Fold[Times, 1, Dual[1, 2]]
	,
	2
	,
	{Dual::arrayOp}
	,
	TestID->"122151f9-4073-4704-8520-10568d9ea09e"
]

VerificationTest[(* 15 *)
	FoldList[Times, Dual[1, 2]]
	,
	Dual[1, 2]
	,
	{Dual::arrayOp}
	,
	TestID->"c4f5161c-13e4-44fb-ac74-15322b838465"
]

VerificationTest[(* 16 *)
	FoldList[Times, 1, Dual[1, 2]]
	,
	Dual[1, 1, 2]
	,
	{Dual::arrayOp, Dual::argt}
	,
	TestID->"a1372ded-f932-44b8-bb40-1e9d41c179e4"
]

VerificationTest[(* 17 *)
	List[Fold[Times, Dual[1, 2], Range[5]], FoldList[Times, Dual[1, 2], Range[5]]]
	,
	List[Dual[120, 240], List[Dual[1, 2], Dual[1, 2], Dual[2, 4], Dual[6, 12], Dual[24, 48], Dual[120, 240]]]	
	,
	TestID->"7bb97bb8-1617-4e4f-9035-0802f2dda2ec"
]

EndTestSection[]

BeginTestSection["Dot"]

VerificationTest[(* 18 *)
	List[Dot[Dual[List[], List[]], List[]], Dot[Dual[List[a1], List[b1]], List[a2]], Dot[Dual[Array[a1, 2], Array[b1, 2]], Array[a2, 2]], Dot[Dual[Array[a1, List[2, 2]], Array[b1, List[2, 2]]], Array[a2, List[2, 2]]]]
	,
	List[Dual[0, 0], Dual[Times[a1, a2], Times[a2, b1]], Dual[Plus[Times[a1[1], a2[1]], Times[a1[2], a2[2]]], Plus[Times[a2[1], b1[1]], Times[a2[2], b1[2]]]], Dual[List[List[Plus[Times[a1[1, 1], a2[1, 1]], Times[a1[1, 2], a2[2, 1]]], Plus[Times[a1[1, 1], a2[1, 2]], Times[a1[1, 2], a2[2, 2]]]], List[Plus[Times[a1[2, 1], a2[1, 1]], Times[a1[2, 2], a2[2, 1]]], Plus[Times[a1[2, 1], a2[1, 2]], Times[a1[2, 2], a2[2, 2]]]]], List[List[Plus[Times[a2[1, 1], b1[1, 1]], Times[a2[2, 1], b1[1, 2]]], Plus[Times[a2[1, 2], b1[1, 1]], Times[a2[2, 2], b1[1, 2]]]], List[Plus[Times[a2[1, 1], b1[2, 1]], Times[a2[2, 1], b1[2, 2]]], Plus[Times[a2[1, 2], b1[2, 1]], Times[a2[2, 2], b1[2, 2]]]]]]]	
	,
	TestID->"13b47a3e-c1a1-48e9-859f-5be87a636b0a"
]

VerificationTest[(* 19 *)
	List[Dot[List[], Dual[List[], List[]]], Dot[List[a2], Dual[List[a1], List[b1]]], Dot[Array[a2, 2], Dual[Array[a1, 2], Array[b1, 2]]], Dot[Array[a2, List[2, 2]], Dual[Array[a1, List[2, 2]], Array[b1, List[2, 2]]]]]
	,
	List[Dual[0, 0], Dual[Times[a1, a2], Times[a2, b1]], Dual[Plus[Times[a1[1], a2[1]], Times[a1[2], a2[2]]], Plus[Times[a2[1], b1[1]], Times[a2[2], b1[2]]]], Dual[List[List[Plus[Times[a1[1, 1], a2[1, 1]], Times[a1[2, 1], a2[1, 2]]], Plus[Times[a1[1, 2], a2[1, 1]], Times[a1[2, 2], a2[1, 2]]]], List[Plus[Times[a1[1, 1], a2[2, 1]], Times[a1[2, 1], a2[2, 2]]], Plus[Times[a1[1, 2], a2[2, 1]], Times[a1[2, 2], a2[2, 2]]]]], List[List[Plus[Times[a2[1, 1], b1[1, 1]], Times[a2[1, 2], b1[2, 1]]], Plus[Times[a2[1, 1], b1[1, 2]], Times[a2[1, 2], b1[2, 2]]]], List[Plus[Times[a2[2, 1], b1[1, 1]], Times[a2[2, 2], b1[2, 1]]], Plus[Times[a2[2, 1], b1[1, 2]], Times[a2[2, 2], b1[2, 2]]]]]]]	
	,
	TestID->"c686d623-4510-4a1b-8183-f763bfb2527a"
]

VerificationTest[(* 20 *)
	List[Dot[Dual[List[], List[]], Dual[List[], List[]]], Dot[Dual[List[a1], List[b1]], Dual[List[a2], List[b2]]], Dot[Dual[Array[a1, 2], Array[b1, 2]], Dual[Array[a2, 2], Array[b2, 2]]]]
	,
	List[Dual[0, 0], Dual[Times[a1, a2], Plus[Times[a2, b1], Times[a1, b2]]], Dual[Plus[Times[a1[1], a2[1]], Times[a1[2], a2[2]]], Plus[Times[a2[1], b1[1]], Times[a2[2], b1[2]], Times[a1[1], b2[1]], Times[a1[2], b2[2]]]]]	
	,
	TestID->"46b86cb7-cffd-4708-a130-6e39d76cc346"
]

VerificationTest[(* 21 *)
	With[List[Set[manyDuals, RandomSample[Join[Map[PackDualArray, Apply[Dual, RandomReal[1, List[1000, 2, 3, 3]], List[1]]], RandomReal[1, List[1000, 3, 3]]]]]], Through[List[DualArrayQ, Dimensions][Apply[Dot, manyDuals]]]]
	,
	List[True, List[3, 3]]	
	,
	TimeConstraint->5, TestID->"a69d21a2-abad-4299-8cd2-a73d6e46f276"
]

VerificationTest[(* 22 *)
	With[List[Set[manyDuals, RandomSample[Join[Map[PackDualArray, Apply[Dual, RandomInteger[10, List[20, 2, 5, 5]], List[1]]], RandomInteger[10, List[20, 5, 5]]]]]], SameQ[Apply[Dot, manyDuals], Fold[Dot, manyDuals]]]
	,
	True	
	,
	TestID->"1cf05627-d07a-45b7-affb-a6e048698d41", TimeConstraint->5
]

EndTestSection[]

EndTestSection[]

EndTestSection[]
