# DenseBasse Matrix Vector

## Vector 构造, 自动转置

Vector 的拷贝/移动构造函数,
在创建新的 Vector 时, 会自动调整 vector 的行列指标次序,
让矢量的变动指标是 `指标0`(`row`).

```cpp
VectorBase<T>::VectorBase(DenseBase<T>&& other):DenseBase<T>(std::move(other)){
    assert(this->Rows()==1||this->Cols()==1);
    if(this->Cols()>1) swap(this->_rows,this->_cols);
}
```

如果在表达式中, `[n,1]` 与 `[1,n]` 矩阵相加会报错,
但是如果用 中间变量 `Vector tmp` 保存 `[1,n]`,
则会调用构造函数自动转置, 转换成 `[n,1]` 矢量, 再相加就不会报错.
