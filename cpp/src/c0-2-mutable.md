# c++ mutable

[const真理大讨论(二)mutable](https://zhuanlan.zhihu.com/p/272240421)
[C++的mutable和volatile](https://blog.csdn.net/AAA123524457/article/details/80967330)

```cpp
class Mesh {
  std::vector<Vertex> vertices;    //模型顶点的集合
  mutable std::mutex mtx;
  mutable double volume;              //计算出来的体积
  mutable bool volumeCalculated;      //当前体积是否已计算的标识

public:
  Mesh(std::vector<Vertex> vxs = {}): volume{0}, volumeCalculated{false}, vertices(std::move(vxs)) {}
  double getVolume() const {
    std::scoped_lock lock{mtx};                     //OK
    if(volumeCalculated) {
      return volume;
    }
    volume = geometry::calculateVolume(vertices);   //OK
    volumeCalculated = true;                        //OK
    return volume;
  }

  void addVertex(Vertex const& v) {
    std::scoped_lock lock{mtx};
    vertices.push_back(v);
    volumeCalculated = false;
  }
     //...
};
```
