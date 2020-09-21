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
	CompoundExpression[Set[testValuesSym, UnpackDualArray[Dual[Array[\[FormalA], List[10]], Array[\[FormalB], List[10]]]]], Set[testValuesNum, BlockRandom[CompoundExpression[SeedRandom[1], UnpackDualArray[Dual[RandomReal[List[-2, 2], 50], RandomReal[List[-2, 2], 50]]]]]], List[Apply[Plus, testValuesSym], Apply[Plus, testValuesNum], DualApply[Expand][Apply[Times, Part[testValuesSym, Span[1, 3]]]], Apply[Times, testValuesNum]]]
	,
	List[Dual[Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalA][6], \[FormalA][7], \[FormalA][8], \[FormalA][9], \[FormalA][10]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5], \[FormalB][6], \[FormalB][7], \[FormalB][8], \[FormalB][9], \[FormalB][10]]], Dual[-8.42866398437041`, -5.969132875714167`], Dual[Times[\[FormalA][1], \[FormalA][2], \[FormalA][3]], Plus[Times[\[FormalA][2], \[FormalA][3], \[FormalB][1]], Times[\[FormalA][1], \[FormalA][3], \[FormalB][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalB][3]]]], Dual[-4.0233586971207944`*^-9, 5.548040846681049`*^-8]]	
	,
	TestID->"552e05b9-e03b-481e-b5db-3943808f093a", SameTest->Equal
]

VerificationTest[(* 3 *)
	List[Equal[Fold[Plus, testValuesNum], Apply[Plus, testValuesNum]], Equal[Fold[Times, testValuesNum], Apply[Times, testValuesNum]], Equal[NonStandard[Fold[Times, testValuesNum]], Total[Apply[Times, DualTuples[testValuesNum], List[1]]]]]
	,
	List[True, True, True]	
	,
	TestID->"26171df0-0069-4981-8d67-5e2e2efd3d0e"
]

EndTestSection[]

BeginTestSection["Arrays"]

BeginTestSection["MatrixPower"]

VerificationTest[(* 4 *)
	CompoundExpression[Set[angle, Times[Pi, Power[7, -1]]], Set[mat, RotationMatrix[angle]], List[MatrixPower[N[mat], 1000], MatrixPower[N[mat, 20], 1000], MatrixPower[N[mat, 50], 1000]]]
	,
	ConstantArray[RotationMatrix[Times[1000, angle]], 3]	
	,
	SameTest->Function[Max[Abs[#1 - #2]]<10^(-10)], TimeConstraint->2, TestID->"7175ef12-fab3-41c8-9090-8d7b0294c2db"
]

EndTestSection[]

BeginTestSection["Fold"]

VerificationTest[(* 5 *)
	List[DualApply[Expand][Fold[Plus, Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Times, Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Plus, \[FormalX], Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Times, \[FormalX], Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]]]
	,
	List[Dual[Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5]]], Dual[Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[Times[\[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][1]], Times[\[FormalA][1], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][4], \[FormalA][5], \[FormalB][3]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][5], \[FormalB][4]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalB][5]]]], Dual[Plus[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3], \[FormalB][4], \[FormalB][5]]], Dual[Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][3], \[FormalA][4], \[FormalA][5], \[FormalB][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][4], \[FormalA][5], \[FormalB][3]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][5], \[FormalB][4]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3], \[FormalA][4], \[FormalB][5]]]]]	
	,
	TestID->"9113ab76-b211-4f2b-b03b-085cd044cdb3"
]

VerificationTest[(* 6 *)
	List[DualApply[Expand][FoldList[Plus, Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Times, Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Plus, \[FormalX], Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Times, \[FormalX], Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]]]
	,
	List[Dual[List[\[FormalA][1], Plus[\[FormalA][1], \[FormalA][2]], Plus[\[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[\[FormalB][1], Plus[\[FormalB][1], \[FormalB][2]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3]]]], Dual[List[\[FormalA][1], Times[\[FormalA][1], \[FormalA][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[\[FormalB][1], Plus[Times[\[FormalA][2], \[FormalB][1]], Times[\[FormalA][1], \[FormalB][2]]], Plus[Times[\[FormalA][2], \[FormalA][3], \[FormalB][1]], Times[\[FormalA][1], \[FormalA][3], \[FormalB][2]], Times[\[FormalA][1], \[FormalA][2], \[FormalB][3]]]]], Dual[List[\[FormalX], Plus[\[FormalX], \[FormalA][1]], Plus[\[FormalX], \[FormalA][1], \[FormalA][2]], Plus[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[0, \[FormalB][1], Plus[\[FormalB][1], \[FormalB][2]], Plus[\[FormalB][1], \[FormalB][2], \[FormalB][3]]]], Dual[List[\[FormalX], Times[\[FormalX], \[FormalA][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalA][3]]], List[0, Times[\[FormalX], \[FormalB][1]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalB][2]]], Plus[Times[\[FormalX], \[FormalA][2], \[FormalA][3], \[FormalB][1]], Times[\[FormalX], \[FormalA][1], \[FormalA][3], \[FormalB][2]], Times[\[FormalX], \[FormalA][1], \[FormalA][2], \[FormalB][3]]]]]]	
	,
	TestID->"4335f9a5-34b4-4e16-a2a0-85b44a4495bf"
]

VerificationTest[(* 7 *)
	Fold[Times, Dual[1, 2]]
	,
	2
	,
	{Dual::arrayOp}
	,
	TestID->"365ecee2-bd31-40d4-87d2-f63e4feab3df"
]

VerificationTest[(* 8 *)
	Fold[Times, 1, Dual[1, 2]]
	,
	2
	,
	{Dual::arrayOp}
	,
	TestID->"122151f9-4073-4704-8520-10568d9ea09e"
]

VerificationTest[(* 9 *)
	FoldList[Times, Dual[1, 2]]
	,
	Dual[1, 2]
	,
	{Dual::arrayOp}
	,
	TestID->"c4f5161c-13e4-44fb-ac74-15322b838465"
]

VerificationTest[(* 10 *)
	FoldList[Times, 1, Dual[1, 2]]
	,
	Dual[1, 1, 2]
	,
	{Dual::arrayOp, Dual::argt}
	,
	TestID->"a1372ded-f932-44b8-bb40-1e9d41c179e4"
]

VerificationTest[(* 11 *)
	List[Fold[Times, Dual[1, 2], Range[5]], FoldList[Times, Dual[1, 2], Range[5]]]
	,
	List[Dual[120, 240], List[Dual[1, 2], Dual[1, 2], Dual[2, 4], Dual[6, 12], Dual[24, 48], Dual[120, 240]]]	
	,
	TestID->"7bb97bb8-1617-4e4f-9035-0802f2dda2ec"
]

EndTestSection[]

EndTestSection[]

EndTestSection[]
