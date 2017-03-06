(* ::Package:: *)

readHistogram[fn_]:=Module[
{rawDat,names,nVec,maxM,countDat,row,patternS,countS,pattern,count},
rawDat=ReadList[fn,String];
names=StringSplit[rawDat[[1]],"="][[2]]//StringSplit[#,","]&;
nVec=StringSplit[rawDat[[2]],"="][[2]]//StringSplit[#,","]&//ToExpression;
maxM=StringSplit[rawDat[[3]],"="][[2]]//ToExpression;
countDat=Table[
{patternS,countS}=StringSplit[row];
If[patternS=="HIGHER",Nothing,
pattern=ToExpression@StringSplit[patternS,","];
count=ToExpression@countS;
Association@@Append[MapThread[(#1->#2)&,{names,pattern}],"Count"->count]
]
,
{row,rawDat[[4;;]]}
];
Dataset[
<|"nVec"->Association@@MapThread[(#1->#2)&,{names,nVec}],"maxM"->maxM,"countDat"->countDat|>
]
]
