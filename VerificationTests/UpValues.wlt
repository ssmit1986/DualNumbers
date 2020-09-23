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
	CompoundExpression[Set[testValuesSym, Join[List[\[FormalX][1], \[FormalX][2]], UnpackDualArray[Dual[Array[\[FormalA], List[10]], Array[\[FormalB], List[10]]]]]], Set[testValuesNum, BlockRandom[CompoundExpression[SeedRandom[1], UnpackDualArray[Dual[RandomReal[List[-2, 2], 50], RandomReal[List[-2, 2], 50]]]]]], List[Apply[Plus, testValuesSym], Apply[Plus, testValuesNum], DualApply[Expand][Apply[Times, Part[testValuesSym, Span[1, 4]]]], Apply[Times, testValuesNum]]]
	,
	List[Dual[Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalA][6], \[FormalA][7], \[FormalA][8], \[FormalA][9], \[FormalA][10], \[FormalX][1], \[FormalX][2]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5], \[FormalB][6], \[FormalB][7], \[FormalB][8], \[FormalB][9], \[FormalB][10]]], Dual[-8.42866398437041`, -5.969132875714167`], Dual[Times[\[FormalA][1], \[FormalA][2], \[FormalX][1], \[FormalX][2]], Plus[Times[\[FormalA][2], \[FormalB][1], \[FormalX][1], \[FormalX][2]], Times[\[FormalA][1], \[FormalB][2], \[FormalX][1], \[FormalX][2]]]], Dual[-4.023358697120797`*^-9, 5.5480408466810513`*^-8]]	
	,
	TestID->"b1dc5aa4-58a9-4f26-8a74-f453ada0ee3f"
]

VerificationTest[(* 5 *)
	List[Equal[Fold[Plus, testValuesNum], Apply[Plus, testValuesNum]], Equal[Fold[Times, testValuesNum], Apply[Times, testValuesNum]], Equal[NonStandard[Fold[Times, testValuesNum]], Total[Apply[Times, DualTuples[testValuesNum], List[1]]]]]
	,
	List[True, True, True]	
	,
	TestID->"26171df0-0069-4981-8d67-5e2e2efd3d0e"
]

VerificationTest[(* 6 *)
	CompoundExpression[Set[largeTestArray, Join[UnpackDualArray[Apply[Dual, RandomReal[List[0.99`, 1.01`], List[2, 10000]]]], RandomReal[List[0.99`, 1.01`], List[10000]]]], List[TimeConstrained[Equal[Apply[Plus, largeTestArray], Fold[Plus, largeTestArray]], 1], TimeConstrained[Equal[Apply[Times, largeTestArray], Fold[Times, largeTestArray]], 1]]]
	,
	List[True, True]	
	,
	TestID->"1f06c788-1bed-49b6-b76f-c018c57a1320"
]

EndTestSection[]

BeginTestSection["Arrays"]

BeginTestSection["MatrixPower"]

VerificationTest[(* 7 *)
	CompoundExpression[Set[angle, Times[Pi, Power[7, -1]]], Set[mat, RotationMatrix[angle]], List[MatrixPower[N[mat], 1000], MatrixPower[N[mat, 20], 1000], MatrixPower[N[mat, 50], 1000]]]
	,
	ConstantArray[RotationMatrix[Times[1000, angle]], 3]	
	,
	SameTest->Function[Max[Abs[#1 - #2]]<10^(-10)], TimeConstraint->2, TestID->"7175ef12-fab3-41c8-9090-8d7b0294c2db"
]

EndTestSection[]

BeginTestSection["Fold"]

VerificationTest[(* 8 *)
	List[DualApply[Expand][Fold[Plus, Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Times, Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Plus, \[FormalX], Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Times, \[FormalX], Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]]]
	,
	List[Dual[Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5]]], Dual[Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[Times[\[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][1]], Times[\[FormalA][1], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][4], \[FormalA][5], \[FormalB][3]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][5], \[FormalB][4]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalB][5]]]], Dual[Plus[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5]]], Dual[Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][4], \[FormalA][5], \[FormalB][3]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][5], \[FormalB][4]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalB][5]]]]]	
	,
	TestID->"9113ab76-b211-4f2b-b03b-085cd044cdb3"
]

VerificationTest[(* 9 *)
	List[DualApply[Expand][FoldList[Plus, Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Times, Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Plus, \[FormalX], Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Times, \[FormalX], Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]]]
	,
	List[Dual[List[\[FormalA][1], Plus[\[FormalA][1], \[FormalA][2]], Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[\[FormalB][1], Plus[\[FormalB][1], \[FormalB][2]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3]]]], Dual[List[\[FormalA][1], Times[\[FormalA][1], \[FormalA][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[\[FormalB][1], Plus[Times[\[FormalA][2], \[FormalB][1]], Times[\[FormalA][1], \[FormalB][2]]], Plus[Times[\[FormalA][2], \[FormalA][3], \[FormalB][1]], Times[\[FormalA][1], \[FormalA][3], \[FormalB][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalB][3]]]]], Dual[List[\[FormalX], Plus[\[FormalX], \[FormalA][1]], Plus[\[FormalX], \[FormalA][1], \[FormalA][2]], Plus[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[0, \[FormalB][1], Plus[\[FormalB][1], \[FormalB][2]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3]]]], Dual[List[\[FormalX], Times[\[FormalX], \[FormalA][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[0, Times[\[FormalX], \[FormalB][1]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalB][2]]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalA][3], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][3], \[FormalB][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalB][3]]]]]]	
	,
	TestID->"4335f9a5-34b4-4e16-a2a0-85b44a4495bf"
]

VerificationTest[(* 10 *)
	Fold[Times, Dual[1, 2]]
	,
	2
	,
	{Dual::arrayOp}
	,
	TestID->"365ecee2-bd31-40d4-87d2-f63e4feab3df"
]

VerificationTest[(* 11 *)
	Fold[Times, 1, Dual[1, 2]]
	,
	2
	,
	{Dual::arrayOp}
	,
	TestID->"122151f9-4073-4704-8520-10568d9ea09e"
]

VerificationTest[(* 12 *)
	FoldList[Times, Dual[1, 2]]
	,
	Dual[1, 2]
	,
	{Dual::arrayOp}
	,
	TestID->"c4f5161c-13e4-44fb-ac74-15322b838465"
]

VerificationTest[(* 13 *)
	FoldList[Times, 1, Dual[1, 2]]
	,
	Dual[1, 1, 2]
	,
	{Dual::arrayOp, Dual::argt}
	,
	TestID->"a1372ded-f932-44b8-bb40-1e9d41c179e4"
]

VerificationTest[(* 14 *)
	List[Fold[Times, Dual[1, 2], Range[5]], FoldList[Times, Dual[1, 2], Range[5]]]
	,
	List[Dual[120, 240], List[Dual[1, 2], Dual[1, 2], Dual[2, 4], Dual[6, 12], Dual[24, 48], Dual[120, 240]]]	
	,
	TestID->"7bb97bb8-1617-4e4f-9035-0802f2dda2ec"
]

EndTestSection[]

EndTestSection[]

EndTestSection[]
