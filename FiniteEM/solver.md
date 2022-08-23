# Solver

`model()`; 单例模式

```cpp
//通过id 获取某类存储组件的指针.
FieldConstraintEF* GetComponent<FieldConstraintEF>(int id);
// 获取单元并转换类型
CompElementEF* GetElement<CompElementEF>(int id);
// 取构件 T 及子类的指针向量，存储到 ptrs 中
void GetCompPtrs<PostAnalysisBaseEF>(std::vector<PostAnalysisBaseEF*> &ptrs)
```
