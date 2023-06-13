(* ::Package:: *)

(*\:4e00\:7ef4\:6269\:6563\:6a21\:578b*)
il=0.01;
diff=5*10^-8;
usol=NDSolveValue[{
D[u[t,x],{t,1}] ==D[diff*u[t,x],{x,2}]+ NeumannValue[-0.0006, x == il]
,u[0,x]==20}
, u,{t,0,60},{x,0,il}
,AccuracyGoal->14,PrecisionGoal->14
]
(*solution*)
usol[1,il]
usol[30,il]


(*figure*)
Plot3D[usol[t,x],{t,0,60},{x,0,il}
,PlotRange->All
,AxesLabel->Automatic]


(*figure*)
Plot[usol[1,x],{x,0.007,il}
,PlotRange->All
,AxesLabel->Automatic]
