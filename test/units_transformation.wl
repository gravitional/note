(* ::Package:: *)

(* ::Title:: *)
(*units-transformation.nb*)


(* ::Input:: *)
(*NotebookFileName[]*)


(* ::Text:: *)
(*\:8fd9\:4e2a\:7b14\:8bb0\:672c\:7528\:6765\:8bb0\:5f55\:5355\:4f4d\:53d8\:6362*)


(* ::Text:: *)
(*\:4ece\:516c\:5236\:6362\:7b97\:5230\:81ea\:7136\:5355\:4f4d\:5236\:ff0c\:516c\:5236\:548c\:81ea\:7136\:5355\:4f4d\:5236\:7684\:6240\:6709\:7269\:7406\:91cf\:7684\:5355\:4f4d\:662f\:4e00\:4e00\:5bf9\:5e94\:7684\:ff0c*)


(* ::Text:: *)
(*\:5e76\:4e14\:4e24\:4e2a\:4f53\:7cfb\:90fd\:662f\:81ea\:6d3d\:7684\:ff0c\:6240\:4ee5\:5728\:516c\:5236\:4e2d\:7684\:5355\:4f4d\:53d8\:6362\:53ef\:4ee5\:6620\:5c04\:5230\:81ea\:7136\:5355\:4f4d\:5236\:4e2d\:ff0c\:5e76\:4e14\:662f\:591a\:4e00\:6620\:5c04\:ff0c*)


(* ::Text:: *)
(*\:56e0\:4e3a\:81ea\:7136\:5355\:4f4d\:5236\:53ea\:6709\:4e00\:4e2a\:57fa\:672c\:91cf\:7eb2*)


(* ::EquationNumbered:: *)
(*E==M==1/L==1/T*)


(* ::Text:: *)
(*\:6240\:4ee5\:4e3a\:4e86\:5728\:4e24\:5957\:4f53\:7cfb\:4e4b\:95f4\:53d8\:5316\:ff0c\:53ef\:4ee5\:628a\:516c\:5236\:4e2d\:7684\:6240\:6709\:7684\:5355\:4f4d\:6620\:5c04\:5230\:81ea\:7136\:5355\:4f4d\:5236\:5236\:4e2d\:8d28\:91cf\:7684n\:6b21\:5e42\:ff0c*)


(* ::Text:: *)
(*\:7136\:540e\:4ece\:81ea\:7136\:5355\:4f4d\:5236\:7684\:8d28\:91cf\:6362\:7b97\:5230\:516c\:5236\:7684\:957f\:5ea6\:ff08\:6216\:8005\:53cd\:4e4b\:ff09*)


(* ::Text:: *)
(*\:8fd9\:4e00\:6838\:5fc3\:7684\:5173\:7cfb\:662f*)


(* ::EquationNumbered:: *)
(*\[HBar]c==197.326 MeV\[CenterDot]fm==1*)


(* ::Section:: *)
(*MeV and fm*)


(* ::EquationNumbered:: *)
(*\[HBar]c==197.326 MeV\[CenterDot]fm==1*)


(* ::Text:: *)
(*in natural units, so*)


(* ::EquationNumbered:: *)
(*fm=1/(197.326 MeV)==0.00506791/MeV==0.00506791/(GeV/1000)==5.06791/GeV*)


(* ::EquationNumbered:: *)
(*MeV^-1=197.326fm,GeV^-1=0.197326fm*)


(* ::EquationNumbered:: *)
(*1/GeV^2=0.0389376/fm^2*)


(* ::Text:: *)
(*********************)


(* ::Text:: *)
(*\:5e26\:7535\:7c92\:5b50\:548c\:4e2d\:6027\:7c92\:5b50\:7684\:534a\:5f84\:5e73\:65b9\:7684\:91cf\:7eb2\:4e00\:6837*)


(* ::Text:: *)
(*\:56e0\:4e3a\:5f62\:72b6\:56e0\:5b50\:6ca1\:6709\:91cf\:7eb2*)


(* ::Text:: *)
(*\:5e26\:7535\:91cd\:5b50*)


(* ::EquationNumbered:: *)
(*\[LeftAngleBracket]rE^2\[RightAngleBracket]=-6/Ge[0]*(\[DifferentialD]Ge[Q2]/\[DifferentialD]Q2)[Q2==0],GeV^-2=0.0389376fm^-2*)


(* ::Text:: *)
(*\:4e2d\:6027\:91cd\:5b50*)


(* ::EquationNumbered:: *)
(*\[LeftAngleBracket]rE^2\[RightAngleBracket]=(-6)*(\[DifferentialD]Ge[Q2]/\[DifferentialD]Q2)[Q2==0],GeV^-2=0.0389376fm^-2*)


(* ::Text:: *)
(****************************************)


(* ::Text:: *)
(*E M L T*)


(* ::EquationNumbered:: *)
(*c=L/T==1*)


(* ::Text:: *)
(*\:5149\:901f\:4e3a1,\:6240\:4ee5*)


(* ::EquationNumbered:: *)
(*1 s=299792458 m*)


(* ::EquationNumbered:: *)
(*1/s==1/(299792458 m)*)


(* ::Text:: *)
(*********************************)


(* ::Text:: *)
(*\:7269\:7406\:91cf\:ff0c\:786e\:5b9a\:503c\:ff0c\:8bef\:5dee\:ff0c\:5355\:4f4d*)


(* ::Text:: *)
(*\:771f\:7a7a\:5149\:901f, 299792458 	(\:786e\:5207), m s^-1*)


(* ::Text:: *)
(*\:6838\:78c1\:77e9, 5.05078343 e(-27) + 0.00000043e(-27), J.T^-1, T is Tesla*)


(* ::Text:: *)
(*\:57fa\:672c\:7535\:8377, 1.60217653 e(-19)+ 0.00000014e(-19), C*)


(* ::Section:: *)
(*form factor*)


(* ::Text:: *)
(*set E or M \:7684\:91cf\:7eb2\:4e3a1*)


(* ::Text:: *)
(*\[ScriptCapitalL] is Lagrangian density.*)


(* ::EquationNumbered:: *)
(*S=\[Integral]\[DifferentialD]tL=\[Integral]\[DifferentialD]^4x\[ScriptCapitalL][\[Phi],\[PartialD]\[Mu] . \[Phi]]*)


(* ::Text:: *)
(*\:91cf\:7eb2\:4e3a0\:ff0cso \[ScriptCapitalL] \:7684\:91cf\:7eb2\:4e3a4*)


(* ::EquationNumbered:: *)
(*H=\[Integral]\[DifferentialD]^3x(\[Pi] . \[PartialD]t . \[Phi]-\[ScriptCapitalL])\[Congruent]\[Integral]\[DifferentialD]^3x\[ScriptCapitalH]*)


(* ::Text:: *)
(*\:91cf\:7eb2\:4e3a1*)


(* ::EquationNumbered:: *)
(*\[ScriptCapitalL]=\!\(\*OverscriptBox[\(\[Psi]\), \(_\)]\) . \!\(TraditionalForm\`\((I \**)
(*OverlayBox[{"/", "\[PartialD]"}] - m)\) . \[Psi]\)*)


(* ::Text:: *)
(*\:91cf\:7eb2\:4e3a4*)


(* ::Text:: *)
(************************)


(* ::Text:: *)
(*\[Psi] \:7684\:91cf\:7eb2*)


(* ::EquationNumbered:: *)
(*1+2*dim[\[Psi]]=4,dim[\[Psi]]==3/2*)


(* ::EquationNumbered:: *)
(*[a[k],a[p]\[ConjugateTranspose]]==(2\[Pi])^3 (\[Delta]^3)[p-k]*)


(* ::Text:: *)
(*(2\[Pi])^3 (\[Delta]^3)[p-k]==\[Integral]\[DifferentialD]^3xExp[\[PlusMinus]I(p-k)\[CenterDot]x]*)


(* ::Text:: *)
(*\:6240\:4ee5\:ff0ca[k] \:7684\:91cf\:7eb2\:4e3a-(3/2)*)


(* ::Text:: *)
(*********************************************)


(* ::Text:: *)
(*\[Psi]=\[Integral](\[DifferentialD]^3p/((2\[Pi])^3))\[Conjugate] 1/Sqrt[2Subscript[E, p]] \!\(\*UnderscriptBox[\(\[Sum]\), \(s\)]\)(\!\( *)
(*\*SubsuperscriptBox[\(a\), \(p\), \(s\)] \( *)
(*\(\*SuperscriptBox[\(u\), \(s\)]\)[p]\) \(Exp[\(-I\) p\[CenterDot]x]\)\)+\!\( *)
(*\*SubsuperscriptBox[\(b\), \(p\), \(s\[Dagger]\)] \( *)
(*\(\*SuperscriptBox[\(v\), \(s\)]\)[p]\) \(Exp[I  p\[CenterDot]x]\)\))*)


(* ::EquationNumbered:: *)
(*(3-1/2)+dim[u]-3/2=3/2*)


(* ::EquationNumbered:: *)
(*so,dim[u]==1/2*)


(* ::Text:: *)
(*************************)


(* ::Text:: *)
(*\[Psi]\:91cf\:7eb2\:4e3a3/2\:ff0cu[s,p]\:91cf\:7eb2\:4e3a1/2\:ff0ca[k] \:7684\:91cf\:7eb2\:4e3a-(3/2)*)


(* ::Text:: *)
(*\:5355\:7c92\:5b50\:6001*)


(* ::Equation:: *)
(*TextCell[RawBoxes[Cell[BoxData[FormBox[RowBox[{TemplateBox[{RowBox[{"p",",","s"}]},"Ket"],"=",RowBox[{SqrtBox[RowBox[{"2","Ep"}]],RowBox[{RowBox[{"a","[",RowBox[{"p",",","s"}],"]"}],"\[ConjugateTranspose]"}],TemplateBox[{"0"},"Ket"]}]}],TraditionalForm]]]]] \:ff0c (\:91cf\:7eb2\:662f TextCell[RawBoxes[Cell[BoxData[FormBox[RowBox[{"-","1"}],TraditionalForm]]]]])*)


(* ::EquationNumbered:: *)
(*BraKet[{q,r},{p,s}]\[Congruent]2E[p](2\[Pi])^3 (\[Delta]^3)[p-q]\[Delta][r,s]*)


(* ::EquationNumbered:: *)
(*J\[Mu]=\!\(\*OverscriptBox[\(\[Psi]\), \(_\)]\) . \!\(TraditionalForm\`\[Gamma]\[Mu] . \[Psi]\),\:91cf\:7eb2\:4e3a 3*)


(* ::Text:: *)
(*Bra[N[p\[Prime]]]J\[Mu] Ket[N[p]]=\!\(\*OverscriptBox[\(u\), \(_\)]\)[p\[Prime]](\[Gamma]\[Mu]*F1[Q2]+(I*\[Sigma]\[Mu]\[Nu]*q\[Nu])/(2mN) F2[Q2])u[p]*)


(* ::EquationNumbered:: *)
(*-1-1+3==1/2+1/2+dim[F1]*)


(* ::EquationNumbered:: *)
(*dim[F1]==0*)


(* ::Section:: *)
(*others*)


(* ::Text:: *)
(***********************************)


(* ::EquationNumbered:: *)
(*MeV=(10^6 1.60217653 J)/10^19=(1.60217653 J)/10^13*)


(* ::Text:: *)
(*****************)


(* ::EquationNumbered:: *)
(*1 T==Wb/m^2=N/(A\[CenterDot]m)=N . s/C . m=Kg/(A\[CenterDot]s^2)=Kg/(C\[CenterDot]s)*)


(* ::Text:: *)
(*\:6240\:4ee5\:6838\:78c1\:77e9,*)


(* ::EquationNumbered:: *)
(*J . (1/T)==dim[f] . dim[l] . (1/T)==(Kg . m . (1/s^2)) . m . (Kg/(C\[CenterDot]s))=kg^2 . m^2 . (1/s^3) . C*)


(* ::Text:: *)
(********************\:5982\:679c\:7528\:81ea\:7136\:5355\:4f4d\:5236\:ff0c*)


(* ::EquationNumbered:: *)
(*[\:6838\:78c1\:77e9]==J . T^-1==[M]^2*[L]^2*[T]^-3==[M]^2*[L]^-1==[M]^1*)


(* ::Text:: *)
(***************************)


(* ::EquationNumbered:: *)
(*\[Mu]N==(5.05078343 J . (1/T) Kg)/(10^27 C s)*)


(* ::EquationNumbered:: *)
(*==5.05078343*10^-27*(1.60217653*10^-13)^-1*MeV*s^-1*Kg*C^-1*)


(* ::EquationNumbered:: *)
(*==5.05078343*10^-27*(1.60217653*10^-13)^-1*MeV*(1/299792458)*m^-1*Kg*C^-1*)


(* ::EquationNumbered:: *)
(*==5.05078343*10^-27*(1.60217653*10^-13)^-1*MeV*(1/299792458)*(10^-15*fm)^-1*Kg*C^-1*)


(* ::EquationNumbered:: *)
(*==5.05078343*10^-27*(1.60217653*10^-13)^-1*MeV*(1/299792458)*10^15*fm^-1*Kg*C^-1*)


(* ::EquationNumbered:: *)
(*==1.05154*10^-7 MeV*fm^-1*Kg*C^-1*)


(* ::EquationNumbered:: *)
(*==1.05154*10^-10 GeV*fm^-1*Kg*C^-1*)
