BeginTestSection["UpValues"]

BeginTestSection["Initialisation"]

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
	TestID->"552e05b9-e03b-481e-b5db-3943808f093a"
]

VerificationTest[(* 3 *)
	List[Equal[Fold[Plus, testValuesNum], Apply[Plus, testValuesNum]], Equal[Fold[Times, testValuesNum], Apply[Times, testValuesNum]]]
	,
	List[True, True]	
	,
	TestID->"26171df0-0069-4981-8d67-5e2e2efd3d0e"
]

EndTestSection[]

EndTestSection[]
