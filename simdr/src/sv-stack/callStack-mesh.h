// 求解器调用栈,创建网格部分
//solver/main.cpp

main(int argc, char* argv[]);
    system("chcp 65001"); // cmd encoding
    common::Control().Execute(argc, argv, __DATE__, __TIME__); // 总流程控制类, 管理 Impl 资源, /Common/Control/Control.cpp
        args()->Parse(argc,argv); //解析命令行参数
        _pIml->Initialize(argc, argv); // 初始化 Blas, mpi, cuda, thread, hypre, petsc, profiler(分析器)
        anlsCtrl()->TestLicense(); //检查许可证
        _pIml->SetWorkingPath(); //设置工作路径
        _pIml->PrintHeader(); //打印日志头, 成刷版本, 进程数目等.
        _pIml->CreateModel(); //创建模型
            auto reader=controlReader(); // read control and solver, 从 control.json
            reader->ParseFile(globalInfo()->GetProjectPath()+"/control.json");
            anlsCtrl()->TestLicense(solver); //
            lib()->Load(Str::ToLower(solver)); // load solver dll

            memTracker()->SnapShot("CreateModel"); //分析内存
            unique_ptr<ModelCreator> creator(ModelCreator::New(solver));// 创建 solver model
            creator->Create(); // 创建网格和模型, /Common/Model/ModelCreator.cpp
                FeModelCreator::CreateMesh(); // virtual; 创建网格; 从 projectPath/mesh.h5 生成网格
                    FeMeshCreator meshCreator; // 有限元网格生成及分解类; /common/FiniteElement/FiniteElement/Mesh/FeMeshCreator.h
                    _mesh=meshCreator.Create(); // common/FiniteElement/FiniteElement/Mesh/FeMeshCreator.cpp
                        Read(); // 读取网格数据; path 网格文件路径. 默认为空, 读本工程网格, 并读取集合; 否则只读网格.
                            _reader->ReadNodes(_nodeCount,coords); // 读取节点数量，坐标
                            // /common/Common/Common/Mesh/MeshReader.cpp
                            _reader->MeshReader::CreateSet(); //创建集合, from mesh.h5; 非 隐式结构 电磁
                            _reader->CreatePart(); // 为 ensight 创建 Part 数据, from mesh.h5
                        Partition(); // 网格区域分解, 封装 metis
                        AdjusMesh(); // 根据配置, 调整网格信息
                        auto mesh=BuilMesh(); //
                            mesh->SetNode(move(_nodeCoord)); // 设置节点
                            mesh->SetElement(move(_eleTypes),move(_eleNodes),move(_surfOwners)); //设置单元
                        MockParaMesh(mesh, MPIUtil::GetSize()); // ensight mock parallel mesh
                        FeMeshCreator::CreateSet(mesh); // 创建 FeSet; 电磁，隐式结构
