// 电磁材料

class EMBASE_API MaterialEM : public CompMaterial
{
public:
    // 克隆材料
    virtual MaterialEM *Clone()
    {
        return new MaterialEM(*this);
    }
    //
    void Create(ControlReader &data);
    void UpdateByTemperature(double Temp);

    // 磁导率
    PermeabilityBase *GetPermeability() { return _Permeability; }
    // 电导率
    ConductivityBase *GetConductivity() { return _Conductivity; }
    // 介电常数, permittivity
    DielectricBase *GetDielectric() { return _Dielectric; }
    // xxx
    ElectricalSteelBase *GetElectricalSteel() { return _ElectricalSteel; }

protected:
    PermeabilityBase *__Permeability;
    ConductivityBase *_Conductivity;
    DielectricBase *_Dielectric;
    ElectricalSteelBase *_ElectricalSteel;
}