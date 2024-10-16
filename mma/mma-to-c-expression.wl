(* ::Package:: *)

(* ::Chapter:: *)
(*MMA-> C++ expression*)


(*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
a: matrix name;
k: matrix dimension;
bkt: \:6392\:7248\:4f7f\:7528, \:751f\:6210\:7c7b\:4f3c a[1] \:8fd9\:79cd\:5f62\:5f0f
*)
aList[a_,k_]:=Array[bkt[a,#]&,k,0];
(*name a; dim k; element [i,j]*)
b[a_,k_,i_,j_]:=a[i*k+j];
(*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx \:751f\:6210\:77e9\:9635, name a, \:7ef4\:6570k*)
mat[a_,k_]:=Module[{},
ArrayReshape[aList[a,k*k],{k,k}]];


(*-------------------------\:5b57\:7b26\:4e32 representation *)
(*\:7279\:6b8a\:89c4\:5219*)
rPlus[a_]:=ToString@a;
rTimes[b_]:=ToString@b;
rTimes[-1,b__]:=StringTemplate["(-1.0)*`1`"][rTimes@b];
(* a^-1 => 1.0/(a) *)
rPower[a_,-1]:=StringTemplate["1.0/(`1`)"][a];
(*\:4e00\:822c\:89c4\:5219, \:9012\:5f52 repr*)
rList[a__]:=ToString[list@a]<>"\n";
rPower[a_,b_]:=StringTemplate["pow(`1`,`2`)"][a,b];
rPlus[a_,b__]:=StringTemplate["`1`+`2`"][a,rPlus@b];
rTimes[a_,b__]:=StringTemplate["`1`*`2`"][a,rTimes@b];
rbkt[a_,b_]:=StringTemplate["`1`[`2`]"][a,b];
rules={Plus->rPlus,Times->rTimes,bkt->rbkt
,(*List->rList,*)Power->rPower
};


(*-----------------\:8ba1\:7b97\:884c\:5217\:5f0f
name: \:77e9\:9635\:540d\:79f0
k: \:77e9\:9635\:7ef4\:6570
*)
calcDet[name_,k_]:=StringTemplate[
"auto calcDet = [](scalar* `name`){
	scalar det = `detExp`;
	return det;
};"
][
<|
"name"->ToString@name,
"detExp"->(Det@mat[name,k])/.rules
|>
];
(*----------------- \:8ba1\:7b97\:9006\:77e9\:9635
name: \:77e9\:9635\:540d\:79f0
k: \:77e9\:9635\:7ef4\:6570
iFace, \:9006\:77e9\:9635\:4e58\:5b50
*)
calcInvMat[name_,k_,iFac_]:=Block[{
iMat,iDet
},
iMat=mat[name,k];
iDet=Det@iMat;
StringTemplate[
"auto calcInvMat = [](scalar* `name`){
	// det of matrix
	const scalar iDet = `iDet`;
	// inverse factor
	const scalar iFac = 1.0 / iDet;
	std::vector<scalar> invMat `detExp`;
	for(auto& x: invMat) x *= iFac;
	return invMat;
};"
][
<|
"name"->ToString@name,
"iDet"->iDet/.rules,
"detExp"->(Flatten@Simplify@(Inverse@iMat*iDet*iFac))/.rules
|>]
]


(* ::Section:: *)
(*test*)


(* ::Input:: *)
(*calcDet[a,2]*)


(* ::Input:: *)
(*calcInvMat[a,3,iFac]*)
