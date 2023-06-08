(* ::Package:: *)

(* ::Title:: *)
(*quadrilateral*)


(* ::Chapter:: *)
(*quad4*)


(*\:8f93\:5165\:8282\:70b9\:5750\:6807\:77e9\:9635 {X1,X2,...}, \:8f93\:51fa\:7cfb\:6570\:77e9\:9635 {A1,A2,...}*)
(* A1:= {a1,b1,c1}... *)
q4Coes[nds_]:=Module[{coe1},
coe1[nds1_,a_,b_]:={
nds1[[a,1]]*nds1[[b,2]]-nds1[[b,1]]*nds1[[a,2]],nds1[[a,2]]-nds1[[b,2]],
nds1[[b,1]]-nds1[[a,1]]};
(*generate coes*)
{
coe1[nds,2,3],
coe1[nds,3,4],
coe1[nds,4,1],
coe1[nds,1,2]}
]


(*\:8ba1\:7b97 quad4 \:5355\:5143\:7684\:9762\:79ef*)
q4Area[coe_]:=1/2*1/2 (
(coe[[1,2]]-coe[[3,2]])*(coe[[2,3]]-coe[[4,3]])-
(coe[[2,2]]-coe[[4,2]])*(coe[[1,3]]-coe[[3,3]]))


(*\:8f93\:5165\:7cfb\:6570\:77e9\:9635 {A1,A2,A3}\:ff0c\:7ed9\:51fa \:7279\:5f81\:53c2\:6570, {g1,g2}*)
q4Trait[coe_]:=Module[{area},
area=q4Area[coe];
{1/(2*area) (coe[[3,2]]*coe[[4,3]]-coe[[4,2]]*coe[[3,3]]),
1/(2*area) (coe[[4,2]]*coe[[1,3]]-coe[[1,2]]*coe[[4,3]])}]


(*\:8f93\:5165 \:8282\:70b9\:5750\:6807\:77e9\:9635 nds, \:5355\:5143\:5185\:5750\:6807 cds,  \:8ba1\:7b97\:5bf9\:5e94\:7684 natural coordinates*)
q4NatCoord[nds_,cds_]:=Module[{coe,area,cd3},
coe=q4Coes[nds];
area=q4Area[coe];
cd3=Prepend[cds,1];
1/(2area)*(coe . cd3)]


(* \:4f7f\:7528 g1, g2 \:751f\:6210\:7684\:7279\:6b8a\:53c2\:6570\:7ec4\:5408*)
q4Vec1[trait_]:=Module[{g1,g2},
g1=trait[[1]];g2=trait[[2]];

{g1(1-g1),-g1*g2,g2(1-g1),-(1-g1)(1-g2)}
]


(* ::Section:: *)
(*example*)


(* ::Input:: *)
(*nds={*)
(*{0,0},*)
(*{1,0},*)
(*{0.8,0.8},{0.1,0.5}};*)
(*{Polygon[nds],Area@Polygon@nds}*)
(*(*-------------------------*)*)
(*coe=q4Coes[nds]*)
(*trait=q4Trait[coe]*)
(*vec1=q4Vec1[trait];*)
(*area=q4Area[coe]*)
(*ncs=q4NatCoord[nds,{1,0}]*)
(**)
(*ncs . vec1*)
(*ncs//Total*)


(* ::Section:: *)
(*test*)


(* ::Input:: *)
(**)


(*
nds: \:8282\:70b9\:5750\:6807\:77e9\:9635; { {x1,y1}, ...};
cds: \:5185\:70b9\:5750\:6807 x,y;
expo: \:5e42\:6b21\:5411\:91cf {i,j,k,l}
*)
fnTest[nds_,cds_,expo_]:=Module[{ncs},
ncs=q4NatCoord[nds,cds];
Times@@MapThread[Power,{ncs,expo}]]


(*---------------------*)
fnAnly[nds_,expo_]:=Module[{m,n,p,q,coe,area,g1,g2,g1b,g2b},
{m,n,p,q}=expo;
coe=q4Coes@nds;
area=q4Area@coe;
{g1,g2}=q4Trait@coe;
g1b=1-g1;g2b=1-g2;

(m!*n!p!*q!)/(m+n+p+q+2)!*2area*(
g1b^(m+n+1)*Sum[
Binomial[m+q-k,m]*Binomial[n+p-j,n]*
Binomial[k+j,k]*g2^k*g2b^j*g1^(p+q-k-j)
,{k,0,q},{j,0,p}]+
(*--------------*)
g1^(p+q+1)*Sum[
Binomial[m+q-k,q]*Binomial[n+p-j,p]*
Binomial[k+j,k]*g2^k*g2b^j*g1b^(m+n-k-j) 
,{k,0,m},{j,0,n}])]


(* ::Input:: *)
(*(*-----------------------*)*)
(*nds={*)
(*{0,0},{1,0},*)
(*{0.8,0.8},{0.1,0.5}};*)
(*cds={0.1,0.1};*)
(*EchoLabel["region"]@{rg=Polygon[nds],Area@rg};*)
(*(*--\:5e42\:6b21\:5217\:8868--*)*)
(*expo={1,1,1,1};*)
(**)
(*ans1=EchoLabel["ans1"]@NIntegrate[*)
(*fnTest[nds,{x,y},expo],{x,y}\[Element]rg,AccuracyGoal->10];*)
(*ans2=EchoLabel["ans2"]@fnAnly[nds,expo];*)



