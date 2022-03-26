# leetcode

## 不重复子串

```mathematica
cst[sym_Symbol] := Module[{},
  sym["winL"] = 1;
  sym["winR"] = 1;
  sym["maxLen"] = 1;
  sym["posList"] = <||>;
  
  sym["eat"][char_] := (
    (*如果出现重复元素，需要更新左边界的位置到 pair 的左元素-----------*)
    If[
     KeyExistsQ[sym["posList"], char] && 
      sym["posList"]@char > sym@"winL",
     sym["winL"] = sym["posList"]@char
     ];
    (*录入新的字符串，存入或更新它的位置 -------------*)
    
    AppendTo[sym["posList"], char -> sym["winR"]];
    (*不重复子串长度，等于 距离 delta=p2-p1 *)
    
    sym["maxLen"] = Max[sym["maxLen"], sym["winR"] - sym["winL"]];
    (*更新现在消耗的字符数量,即右边界的位置*)
    sym["winR"]++;
    )
  ]
(*测试------------------*)
tea = 
 Characters["qwertasdfgqwerttyuiozxcvbnm"]
cst[fn]; count = 1;
Do[fn["eat"]@tea[[count++]], Length@tea]
?? fn
```
