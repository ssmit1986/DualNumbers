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
	Apply[Times, Flatten[List[Dual[a, b], Array[c, 25]]]]
	,
	Dual[Blank[], Blank[]]	
	,
	TestID->"864a7296-c903-4630-9081-b86bd3fcad01", TimeConstraint->2, SameTest-> MatchQ
]

VerificationTest[(* 5 *)
	Apply[Plus, Flatten[List[Dual[a, b], Array[c, 25]]]]
	,
	Plus[c[1], c[2], c[3], c[4], c[5], c[6], c[7], c[8], c[9], c[10], c[11], c[12], c[13], c[14], c[15], c[16], c[17], c[18], c[19], c[20], c[21], c[22], c[23], c[24], c[25], Dual[a, b]]	
	,
	TestID->"48907ddd-0a07-4876-b48e-1aac0c03cbd0", TimeConstraint->2
]

VerificationTest[(* 6 *)
	Power[0, Dual[0, 1]]
	,
	Dual[Indeterminate, Indeterminate]
	,
	{Power::indet, Power::indet}
	,
	TestID->"ef3b9e7c-1901-4391-ad48-aca96b7c5c76"
]

VerificationTest[(* 7 *)
	Power[Dual[0, 1], 0]
	,
	Dual[Indeterminate, Indeterminate]
	,
	{Power::indet, Power::infy, Infinity::indet}
	,
	TestID->"71c987d7-b03d-40fe-ac61-606e190a8ed2"
]

VerificationTest[(* 8 *)
	Power[Dual[0, 1], Dual[0, 1]]
	,
	Dual[Indeterminate, Indeterminate]
	,
	{Power::indet, Power::infy, Infinity::indet, Power::indet}
	,
	TestID->"e7ca5c2c-9988-4b1a-aeed-ea6353b16463"
]

VerificationTest[(* 9 *)
	CompoundExpression[Set[testValuesSym, Join[List[\[FormalX][1], \[FormalX][2]], UnpackDualArray[Dual[Array[\[FormalA], List[10]], Array[\[FormalB], List[10]]]]]], Set[testValuesNum, BlockRandom[CompoundExpression[SeedRandom[1], UnpackDualArray[Dual[RandomReal[List[-2, 2], 50], RandomReal[List[-2, 2], 50]]]]]], List[Apply[Plus, testValuesSym], Apply[Plus, testValuesNum], DualApply[Expand][Apply[Times, Part[testValuesSym, Span[1, 4]]]], Apply[Times, testValuesNum]]]
	,
	List[Dual[Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalA][6], \[FormalA][7], \[FormalA][8], \[FormalA][9], \[FormalA][10], \[FormalX][1], \[FormalX][2]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5], \[FormalB][6], \[FormalB][7], \[FormalB][8], \[FormalB][9], \[FormalB][10]]], Dual[-8.42866398437041`, -5.969132875714167`], Dual[Times[\[FormalA][1], \[FormalA][2], \[FormalX][1], \[FormalX][2]], Plus[Times[\[FormalA][2], \[FormalB][1], \[FormalX][1], \[FormalX][2]], Times[\[FormalA][1], \[FormalB][2], \[FormalX][1], \[FormalX][2]]]], Dual[-4.023358697120797`*^-9, 5.5480408466810513`*^-8]]	
	,
	TestID->"b1dc5aa4-58a9-4f26-8a74-f453ada0ee3f"
]

VerificationTest[(* 10 *)
	List[Equal[Fold[Plus, testValuesNum], Apply[Plus, testValuesNum]], Equal[Fold[Times, testValuesNum], Apply[Times, testValuesNum]], Equal[NonStandard[Fold[Times, testValuesNum]], Total[Apply[Times, DualTuples[testValuesNum], List[1]]]]]
	,
	List[True, True, True]	
	,
	TestID->"26171df0-0069-4981-8d67-5e2e2efd3d0e"
]

VerificationTest[(* 11 *)
	CompoundExpression[Set[largeTestArray, RandomSample[Join[UnpackDualArray[Apply[Dual, RandomReal[List[0.99`, 1.01`], List[2, 10000]]]], RandomReal[List[0.99`, 1.01`], List[10000]]]]], List[TimeConstrained[Less[Abs[Plus[Apply[Plus, largeTestArray], Times[-1, Fold[Plus, largeTestArray]]]], Power[10, -8]], 1], TimeConstrained[Less[Abs[Plus[Apply[Times, largeTestArray], Times[-1, Fold[Times, largeTestArray]]]], Power[10, -10]], 1]]]
	,
	List[True, True]	
	,
	TestID->"1f06c788-1bed-49b6-b76f-c018c57a1320"
]

VerificationTest[(* 12 *)
	List[Apply[Plus, RandomSample[Flatten[List[Dual[1.`, 2.`], RandomReal[1, 10000]]]]], Apply[Times, RandomSample[Flatten[List[Dual[1.`, 2.`], RandomReal[List[0.99`, 1.01`], 10000]]]]]]
	,
	List[Repeated[Dual[PatternTest[Blank[], NumericQ], PatternTest[Blank[], NumericQ]]]]	
	,
	SameTest->MatchQ, TimeConstraint->2, TestID->"d58e656f-2418-4036-bc3a-887fb3715006"
]

EndTestSection[]

BeginTestSection["Arrays"]

BeginTestSection["MatrixPower"]

VerificationTest[(* 13 *)
	CompoundExpression[Set[angle, Times[Pi, Power[7, -1]]], Set[mat, RotationMatrix[angle]], List[MatrixPower[N[mat], 1000], MatrixPower[N[mat, 20], 1000], MatrixPower[N[mat, 50], 1000]]]
	,
	ConstantArray[RotationMatrix[Times[1000, angle]], 3]	
	,
	SameTest->Function[Max[Abs[#1 - #2]]<10^(-10)], TimeConstraint->2, TestID->"7175ef12-fab3-41c8-9090-8d7b0294c2db"
]

EndTestSection[]

BeginTestSection["Fold"]

VerificationTest[(* 14 *)
	List[DualApply[Expand][Fold[Plus, Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Times, Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Plus, \[FormalX], Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Times, \[FormalX], Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]]]
	,
	List[Dual[Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5]]], Dual[Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[Times[\[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][1]], Times[\[FormalA][1], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][4], \[FormalA][5], \[FormalB][3]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][5], \[FormalB][4]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalB][5]]]], Dual[Plus[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5]]], Dual[Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][4], \[FormalA][5], \[FormalB][3]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][5], \[FormalB][4]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalB][5]]]]]	
	,
	TestID->"9113ab76-b211-4f2b-b03b-085cd044cdb3"
]

VerificationTest[(* 15 *)
	List[DualApply[Expand][FoldList[Plus, Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Times, Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Plus, \[FormalX], Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Times, \[FormalX], Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]]]
	,
	List[Dual[List[\[FormalA][1], Plus[\[FormalA][1], \[FormalA][2]], Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[\[FormalB][1], Plus[\[FormalB][1], \[FormalB][2]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3]]]], Dual[List[\[FormalA][1], Times[\[FormalA][1], \[FormalA][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[\[FormalB][1], Plus[Times[\[FormalA][2], \[FormalB][1]], Times[\[FormalA][1], \[FormalB][2]]], Plus[Times[\[FormalA][2], \[FormalA][3], \[FormalB][1]], Times[\[FormalA][1], \[FormalA][3], \[FormalB][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalB][3]]]]], Dual[List[\[FormalX], Plus[\[FormalX], \[FormalA][1]], Plus[\[FormalX], \[FormalA][1], \[FormalA][2]], Plus[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[0, \[FormalB][1], Plus[\[FormalB][1], \[FormalB][2]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3]]]], Dual[List[\[FormalX], Times[\[FormalX], \[FormalA][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[0, Times[\[FormalX], \[FormalB][1]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalB][2]]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalA][3], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][3], \[FormalB][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalB][3]]]]]]	
	,
	TestID->"4335f9a5-34b4-4e16-a2a0-85b44a4495bf"
]

VerificationTest[(* 16 *)
	Fold[Times, Dual[1, 2]]
	,
	2
	,
	{Dual::arrayOp}
	,
	TestID->"365ecee2-bd31-40d4-87d2-f63e4feab3df"
]

VerificationTest[(* 17 *)
	Fold[Times, 1, Dual[1, 2]]
	,
	2
	,
	{Dual::arrayOp}
	,
	TestID->"122151f9-4073-4704-8520-10568d9ea09e"
]

VerificationTest[(* 18 *)
	FoldList[Times, Dual[1, 2]]
	,
	Dual[1, 2]
	,
	{Dual::arrayOp}
	,
	TestID->"c4f5161c-13e4-44fb-ac74-15322b838465"
]

VerificationTest[(* 19 *)
	FoldList[Times, 1, Dual[1, 2]]
	,
	Dual[1, 1, 2]
	,
	{Dual::arrayOp, Dual::argt}
	,
	TestID->"a1372ded-f932-44b8-bb40-1e9d41c179e4"
]

VerificationTest[(* 20 *)
	List[Fold[Times, Dual[1, 2], Range[5]], FoldList[Times, Dual[1, 2], Range[5]]]
	,
	List[Dual[120, 240], List[Dual[1, 2], Dual[1, 2], Dual[2, 4], Dual[6, 12], Dual[24, 48], Dual[120, 240]]]	
	,
	TestID->"7bb97bb8-1617-4e4f-9035-0802f2dda2ec"
]

EndTestSection[]

BeginTestSection["Dot"]

VerificationTest[(* 21 *)
	List[Dot[Dual[a, b]], Dot[Dual[List[1, 2], List[3, 4]]]]
	,
	List[Dual[a, b], Dual[List[1, 2], List[3, 4]]]	
	,
	TestID->"6aec3a2d-1c2c-40c1-bb64-1f551439b190"
]

VerificationTest[(* 22 *)
	List[Dot[Dual[List[], List[]], List[]], Dot[Dual[List[a1], List[b1]], List[a2]], Dot[Dual[Array[a1, 2], Array[b1, 2]], Array[a2, 2]], Dot[Dual[Array[a1, List[2, 2]], Array[b1, List[2, 2]]], Array[a2, List[2, 2]]]]
	,
	List[Dual[0, 0], Dual[Times[a1, a2], Times[a2, b1]], Dual[Plus[Times[a1[1], a2[1]], Times[a1[2], a2[2]]], Plus[Times[a2[1], b1[1]], Times[a2[2], b1[2]]]], Dual[List[List[Plus[Times[a1[1, 1], a2[1, 1]], Times[a1[1, 2], a2[2, 1]]], Plus[Times[a1[1, 1], a2[1, 2]], Times[a1[1, 2], a2[2, 2]]]], List[Plus[Times[a1[2, 1], a2[1, 1]], Times[a1[2, 2], a2[2, 1]]], Plus[Times[a1[2, 1], a2[1, 2]], Times[a1[2, 2], a2[2, 2]]]]], List[List[Plus[Times[a2[1, 1], b1[1, 1]], Times[a2[2, 1], b1[1, 2]]], Plus[Times[a2[1, 2], b1[1, 1]], Times[a2[2, 2], b1[1, 2]]]], List[Plus[Times[a2[1, 1], b1[2, 1]], Times[a2[2, 1], b1[2, 2]]], Plus[Times[a2[1, 2], b1[2, 1]], Times[a2[2, 2], b1[2, 2]]]]]]]	
	,
	TestID->"13b47a3e-c1a1-48e9-859f-5be87a636b0a"
]

VerificationTest[(* 23 *)
	List[Dot[List[], Dual[List[], List[]]], Dot[List[a2], Dual[List[a1], List[b1]]], Dot[Array[a2, 2], Dual[Array[a1, 2], Array[b1, 2]]], Dot[Array[a2, List[2, 2]], Dual[Array[a1, List[2, 2]], Array[b1, List[2, 2]]]]]
	,
	List[Dual[0, 0], Dual[Times[a1, a2], Times[a2, b1]], Dual[Plus[Times[a1[1], a2[1]], Times[a1[2], a2[2]]], Plus[Times[a2[1], b1[1]], Times[a2[2], b1[2]]]], Dual[List[List[Plus[Times[a1[1, 1], a2[1, 1]], Times[a1[2, 1], a2[1, 2]]], Plus[Times[a1[1, 2], a2[1, 1]], Times[a1[2, 2], a2[1, 2]]]], List[Plus[Times[a1[1, 1], a2[2, 1]], Times[a1[2, 1], a2[2, 2]]], Plus[Times[a1[1, 2], a2[2, 1]], Times[a1[2, 2], a2[2, 2]]]]], List[List[Plus[Times[a2[1, 1], b1[1, 1]], Times[a2[1, 2], b1[2, 1]]], Plus[Times[a2[1, 1], b1[1, 2]], Times[a2[1, 2], b1[2, 2]]]], List[Plus[Times[a2[2, 1], b1[1, 1]], Times[a2[2, 2], b1[2, 1]]], Plus[Times[a2[2, 1], b1[1, 2]], Times[a2[2, 2], b1[2, 2]]]]]]]	
	,
	TestID->"c686d623-4510-4a1b-8183-f763bfb2527a"
]

VerificationTest[(* 24 *)
	List[Dot[Dual[List[], List[]], Dual[List[], List[]]], Dot[Dual[List[a1], List[b1]], Dual[List[a2], List[b2]]], Dot[Dual[Array[a1, 2], Array[b1, 2]], Dual[Array[a2, 2], Array[b2, 2]]]]
	,
	List[Dual[0, 0], Dual[Times[a1, a2], Plus[Times[a2, b1], Times[a1, b2]]], Dual[Plus[Times[a1[1], a2[1]], Times[a1[2], a2[2]]], Plus[Times[a2[1], b1[1]], Times[a2[2], b1[2]], Times[a1[1], b2[1]], Times[a1[2], b2[2]]]]]	
	,
	TestID->"46b86cb7-cffd-4708-a130-6e39d76cc346"
]

VerificationTest[(* 25 *)
	List[Dot[Dual[1, 2], List[1, 2]], Dot[List[1, 2], Dual[1, 2]], Dot[Dual[1, 2], Dual[1, 2]], Dot[Dual[List[1], List[2]], 3], Dot[3, Dual[List[1], List[2]]], Dot[Dual[a, b], x, y, z], Apply[Dot, Flatten[List[Dual[a, b], Array[c, 20]]]]]
	,
	List[Repeated[Dual[Blank[], Blank[]]]]	
	,
	TestID->"c5b5445e-2a00-4eef-b536-58381696d908", TimeConstraint->2, SameTest-> MatchQ
]

VerificationTest[(* 26 *)
	With[List[Set[manyDuals, RandomSample[Join[Map[PackDualArray, Apply[Dual, RandomReal[1, List[1000, 2, 3, 3]], List[1]]], RandomReal[1, List[1000, 3, 3]]]]]], Through[List[DualArrayQ, Dimensions][Apply[Dot, manyDuals]]]]
	,
	List[True, List[3, 3]]	
	,
	TimeConstraint->5, TestID->"a69d21a2-abad-4299-8cd2-a73d6e46f276"
]

VerificationTest[(* 27 *)
	With[List[Set[manyDuals, RandomSample[Join[Map[PackDualArray, Apply[Dual, RandomInteger[10, List[20, 2, 5, 5]], List[1]]], RandomInteger[10, List[20, 5, 5]]]]]], SameQ[Apply[Dot, manyDuals], Fold[Dot, manyDuals]]]
	,
	True	
	,
	TestID->"1cf05627-d07a-45b7-affb-a6e048698d41", TimeConstraint->5
]

EndTestSection[]

BeginTestSection["Selecting"]

VerificationTest[(* 28 *)
	CompoundExpression[Set[arr, Dual[Array[a, List[3, 2]], Array[b, List[3, 2]]]], List[Part[arr, List[1, 2]], Take[arr, 2], Drop[arr, 2], Extract[arr, List[List[1], List[2]]], Extract[arr, List[List[1, 1], List[2, 2]]]]]
	,
	List[Dual[List[List[a[1, 1], a[1, 2]], List[a[2, 1], a[2, 2]]], List[List[b[1, 1], b[1, 2]], List[b[2, 1], b[2, 2]]]], Dual[List[List[a[1, 1], a[1, 2]], List[a[2, 1], a[2, 2]]], List[List[b[1, 1], b[1, 2]], List[b[2, 1], b[2, 2]]]], Dual[List[List[a[3, 1], a[3, 2]]], List[List[b[3, 1], b[3, 2]]]], Dual[List[List[a[1, 1], a[1, 2]], List[a[2, 1], a[2, 2]]], List[List[b[1, 1], b[1, 2]], List[b[2, 1], b[2, 2]]]], Dual[List[a[1, 1], a[2, 2]], List[b[1, 1], b[2, 2]]]]	
	,
	TestID->"56931f4e-6eaf-494c-a315-14a7d8c0c2d7"
]

VerificationTest[(* 29 *)
	Through[List[First, Rest, Most, Last][arr]]
	,
	List[Dual[List[a[1, 1], a[1, 2]], List[b[1, 1], b[1, 2]]], Dual[List[List[a[2, 1], a[2, 2]], List[a[3, 1], a[3, 2]]], List[List[b[2, 1], b[2, 2]], List[b[3, 1], b[3, 2]]]], Dual[List[List[a[1, 1], a[1, 2]], List[a[2, 1], a[2, 2]]], List[List[b[1, 1], b[1, 2]], List[b[2, 1], b[2, 2]]]], Dual[List[a[3, 1], a[3, 2]], List[b[3, 1], b[3, 2]]]]	
	,
	TestID->"3ea728f1-5171-4f9b-96c7-8e0e046b6361"
]

VerificationTest[(* 30 *)
	List[SameQ[Join[Part[arr, List[1]], Part[arr, Span[2, All]]], arr], SameQ[Join[Part[arr, All, List[1]], Part[arr, All, Span[2, All]], 2], arr]]
	,
	List[True, True]	
	,
	TestID->"083afab6-6a66-4298-abd7-5aaf57544739"
]

VerificationTest[(* 31 *)
	Through[List[Transpose, Mean, Total, Flatten][arr]]
	,
	List[Dual[List[List[a[1, 1], a[2, 1], a[3, 1]], List[a[1, 2], a[2, 2], a[3, 2]]], List[List[b[1, 1], b[2, 1], b[3, 1]], List[b[1, 2], b[2, 2], b[3, 2]]]], Dual[List[Times[Times[1, Power[3, -1]], Plus[a[1, 1], a[2, 1], a[3, 1]]], Times[Times[1, Power[3, -1]], Plus[a[1, 2], a[2, 2], a[3, 2]]]], List[Times[Times[1, Power[3, -1]], Plus[b[1, 1], b[2, 1], b[3, 1]]], Times[Times[1, Power[3, -1]], Plus[b[1, 2], b[2, 2], b[3, 2]]]]], Dual[List[Plus[a[1, 1], a[2, 1], a[3, 1]], Plus[a[1, 2], a[2, 2], a[3, 2]]], List[Plus[b[1, 1], b[2, 1], b[3, 1]], Plus[b[1, 2], b[2, 2], b[3, 2]]]], Dual[List[a[1, 1], a[1, 2], a[2, 1], a[2, 2], a[3, 1], a[3, 2]], List[b[1, 1], b[1, 2], b[2, 1], b[2, 2], b[3, 1], b[3, 2]]]]	
	,
	TestID->"2e63fc1c-c822-4ca4-94e0-2dfd7937906f"
]

VerificationTest[(* 32 *)
	List[GroupBy[arr, First], GroupBy[arr, Function[EvenQ[Part[Slot[1], 1, 1]]]], GroupBy[arr, Rule[First, Sin]], GroupBy[arr, Rule[First, Sin], Function[Total[Slot[1], 2]]]]
	,
	List[Association[Rule[a[1, 1], Dual[List[List[a[1, 1], a[1, 2]]], List[List[b[1, 1], b[1, 2]]]]], Rule[a[2, 1], Dual[List[List[a[2, 1], a[2, 2]]], List[List[b[2, 1], b[2, 2]]]]], Rule[a[3, 1], Dual[List[List[a[3, 1], a[3, 2]]], List[List[b[3, 1], b[3, 2]]]]]], Association[Rule[False, Dual[List[List[a[1, 1], a[1, 2]], List[a[3, 1], a[3, 2]]], List[List[b[1, 1], b[1, 2]], List[b[3, 1], b[3, 2]]]]], Rule[True, Dual[List[List[a[2, 1], a[2, 2]]], List[List[b[2, 1], b[2, 2]]]]]], Association[Rule[a[1, 1], Dual[List[List[Sin[a[1, 1]], Sin[a[1, 2]]]], List[List[Times[b[1, 1], Cos[a[1, 1]]], Times[b[1, 2], Cos[a[1, 2]]]]]]], Rule[a[2, 1], Dual[List[List[Sin[a[2, 1]], Sin[a[2, 2]]]], List[List[Times[b[2, 1], Cos[a[2, 1]]], Times[b[2, 2], Cos[a[2, 2]]]]]]], Rule[a[3, 1], Dual[List[List[Sin[a[3, 1]], Sin[a[3, 2]]]], List[List[Times[b[3, 1], Cos[a[3, 1]]], Times[b[3, 2], Cos[a[3, 2]]]]]]]], Association[Rule[a[1, 1], Dual[Plus[Sin[a[1, 1]], Sin[a[1, 2]]], Plus[Times[b[1, 1], Cos[a[1, 1]]], Times[b[1, 2], Cos[a[1, 2]]]]]], Rule[a[2, 1], Dual[Plus[Sin[a[2, 1]], Sin[a[2, 2]]], Plus[Times[b[2, 1], Cos[a[2, 1]]], Times[b[2, 2], Cos[a[2, 2]]]]]], Rule[a[3, 1], Dual[Plus[Sin[a[3, 1]], Sin[a[3, 2]]], Plus[Times[b[3, 1], Cos[a[3, 1]]], Times[b[3, 2], Cos[a[3, 2]]]]]]]]	
	,
	TestID->"e4067233-cde4-458e-988c-82e17b75cb70"
]

VerificationTest[(* 33 *)
	GroupBy[Dual[1, 2], f]
	,
	GroupBy[Dual[1, 2], f]
	,
	{Dual::arrayOp, GroupBy::list1}
	,
	TestID->"5ac38c7c-bda0-4821-9361-a5629eb09265"
]

VerificationTest[(* 34 *)
	GroupBy[Dual[1, 2], f, g]
	,
	GroupBy[Dual[1, 2], f, g]
	,
	{Dual::arrayOp, GroupBy::list1}
	,
	TestID->"ad7e9d62-cc10-4fb1-82cf-5db2d37450b8"
]

VerificationTest[(* 35 *)
	GroupBy[Dual[1, 2], List[f]]
	,
	GroupBy[Dual[1, 2], List[f]]
	,
	{Dual::arrayOp, GroupBy::list1}
	,
	TestID->"3cc02ffc-c1a5-41a1-bbfd-631cd3f546f0"
]

VerificationTest[(* 36 *)
	GroupBy[Dual[List[1], List[2]], List[f]]
	,
	GroupBy[Dual[List[1], List[2]], List[f]]
	,
	{Dual::groupbyfun, GroupBy::list1}
	,
	TestID->"03f34999-0633-4cd6-93d8-580c9dedb0a1"
]

VerificationTest[(* 37 *)
	GroupBy[Dual[List[1], List[2]], List[f], g]
	,
	GroupBy[Dual[List[1], List[2]], List[f], g]
	,
	{Dual::groupbyfun, GroupBy::list1}
	,
	TestID->"ab3ce908-750e-4e32-b377-a022621be77e"
]

VerificationTest[(* 38 *)
	Module[List[res], CompoundExpression[On["Packing"], Set[res, List[Through[Map[Map, List[First, Last, Most, Rest]][arr]], GroupBy[arr, Rule[First, Rest]]]], Off["Packing"], res]]
	,
	List[List[Dual[List[a[1, 1], a[2, 1], a[3, 1]], List[b[1, 1], b[2, 1], b[3, 1]]], Dual[List[a[1, 2], a[2, 2], a[3, 2]], List[b[1, 2], b[2, 2], b[3, 2]]], Dual[List[List[a[1, 1]], List[a[2, 1]], List[a[3, 1]]], List[List[b[1, 1]], List[b[2, 1]], List[b[3, 1]]]], Dual[List[List[a[1, 2]], List[a[2, 2]], List[a[3, 2]]], List[List[b[1, 2]], List[b[2, 2]], List[b[3, 2]]]]], Association[Rule[a[1, 1], Dual[List[List[a[1, 2]]], List[List[b[1, 2]]]]], Rule[a[2, 1], Dual[List[List[a[2, 2]]], List[List[b[2, 2]]]]], Rule[a[3, 1], Dual[List[List[a[3, 2]]], List[List[b[3, 2]]]]]]]	
	,
	TestID->"8c51c238-f95d-4a48-87ab-c2af2b64984c"
]

VerificationTest[(* 39 *)
	List[Select[Dual[List[], List[]], EvenQ], Select[Dual[List[1, 2], List[a, b]], Function[False]], Select[Dual[List[1, 2], List[a, b]], Function[True]], Select[Dual[List[1, 2], List[a, b]], EvenQ], Select[UnpackDualArray[Dual[List[1, 2], List[a, b]]], EvenQ]]
	,
	List[Dual[List[], List[]], Dual[List[], List[]], Dual[List[1, 2], List[a, b]], Dual[List[2], List[b]], List[Dual[2, b]]]	
	,
	TestID->"5a4fbafa-117c-41e4-821a-aa4e30b7a195"
]

VerificationTest[(* 40 *)
	Select[Dual[1, 2], EvenQ]
	,
	Dual[2, 0]
	,
	{Dual::arrayOp}
	,
	TestID->"3af53e2f-f4df-466b-a0fe-8cc3800c82ad"
]

VerificationTest[(* 41 *)
	List[Pick[Dual[List[], List[]], List[]], Pick[List[], Dual[List[], List[]]], Pick[Dual[List[], List[]], Dual[List[], List[]]], Pick[Dual[List[a1, a2], List[b1, b2]], List[True, False]], Pick[List[a1, a2], Dual[List[0, 1], List[b1, b2]], 1], Pick[Dual[List[a1, a2], List[b1, b2]], Dual[List[0, 1], List[5, 6]], 1]]
	,
	List[Dual[List[], List[]], List[], Dual[List[], List[]], Dual[List[a1], List[b1]], List[a2], Dual[List[a2], List[b2]]]	
	,
	TestID->"b4e494d1-0f16-40bb-a66b-e1ad69a55efc"
]

VerificationTest[(* 42 *)
	Pick[Dual[1, 2], List[]]
	,
	Pick[Dual[1, 2], List[]]
	,
	{Dual::arrayOp, Pick::incomp}
	,
	TestID->"2ff42615-1afa-4625-8307-ea5a628bf23d"
]

VerificationTest[(* 43 *)
	Pick[List[], Dual[1, 2]]
	,
	Pick[List[], Dual[1, 2]]
	,
	{Dual::arrayOp, Pick::incomp}
	,
	TestID->"4781a4f6-e1eb-47e0-a94d-9ae6f6d84d4d"
]

VerificationTest[(* 44 *)
	List[Position[Dual[List[], List[]], 1], Position[Dual[List[1], List[2]], 1]]
	,
	List[List[], List[List[1]]]	
	,
	TestID->"4f21f8bb-a42e-4795-af1e-9f9730e8c620"
]

EndTestSection[]

EndTestSection[]

BeginTestSection["End"]

EndTestSection[]

EndTestSection[]
