# mesh 格式

## dat

```json
// mesh.dat by IBE4.0
Stat {
    Nodes 68
    Elements 183
    Surfaces 124
}

Node {
    // 编号，x,y,z 坐标
    0 [  -5.0000000000000001e-03  -5.0000000000000001e-03   5.0000000000000001e-03]
    1 [  -5.0000000000000001e-03  -5.0000000000000001e-03  -5.0000000000000001e-03]
    2 [  -5.0000000000000001e-03   5.0000000000000001e-03   5.0000000000000001e-03]
    ...
}

Element { // volume 单元
    Tet4 { // 单元类型
        0 [2,1,4,0] // 构成节点编号
        1 [7,2,1,4]
        2 [2,4,7,6]
        ...
    }
}

Surface {  // surface 单元
    Tri3 { // 单元类型
        183 [7,3,2,3] // 构成节点编号, 所属实体单元编号
        184 [6,7,2,2]
        185 [9,10,15,21]
        ...
    }
}

Set { // set info
    Node {
    }
    Element {
        assignment-1 [0:4]
        Component1-References [0:4]
        FloatingConductor-References [0:4]
        Charge1-References [0:4]
        ElectroMagneticForce-References [0:4]
        Torque-References [0:4]
    }
    Surface {
        DFluxNormal-References [183,184]
        OpenBoundaryCondition-References [185:296]
        PeriodicBoundary-References [297,298]
        PeriodicBoundary-References2 [299,300]
        Voltage-References [301,302]
    }
}

Part {
    Box_Translation-Solid1 [0:4]
    Region-Solid1 [5:182]
}
```
