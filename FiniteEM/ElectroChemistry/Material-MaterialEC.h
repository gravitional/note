#include "CompMaterial.h"

// 电化学材料

class MaterialEC : public CompMaterial
{
public:
    // 根据温度，更新材料属性
    virtual void UpdateByTemperature(double Temp);

private:
    ConductivityBaseEC *_Conductivity; // 电导率
    DiffusionBaseEC *_Diffusion;       // 扩散系数
    MigrationBaseEC *_Migration;       // 迁移系数
}
