# 求解器调用栈,静电场

```cpp
//solver/main.cpp
main(int argc, char* argv[]);
    common::Control().Execute(argc, argv); // 总流程控制类, 管理 Impl 资源, /Common/Control/Control.cpp
        args(); //解析命令行参数
        _pIml->Initialize(argc, argv); // 初始化 Blas, mpi, cuda, thread, hypre, petsc, profiler(分析器)
        anlsCtrl()->TestLicense(); //检查许可证
        _pIml->SetWorkingPath(); //设置工作路径
        _pIml->PrintHeader(); //打印日志头, 成刷版本, 进程数目等.
        _pIml->CreateModel(); //创建模型
            reader=controlReader(); // read control and solver, 从 control.json
            anlsCtrl()->TestLicense(solver); //
            lib()->Load(Str::ToLower(solver)); // load solver dll

            memTracker()->SnapShot("CreateModel"); //分析内存
            unique_ptr<ModelCreator> creator(ModelCreator::New(solver));// 创建 solver model
            creator->Create(); // 创建网格和模型, /Common/Model/ModelCreator.cpp
                CreateMesh(); // 创建网格
                CreateModel(); //创建模型 /FiniteElement/Model/FeModelCreator.cpp
                    CreateFunction(); //自定义函数
                    CreateCoordSys(); //自定义坐标系
                    CreateMonitor(); //
                    CreateNode();
                    CreateElement(data);
                    CreateMaterial(data);
                    CreateInitialField(data);
                    CreateMisc(data); //创建各场的特有数据, /Electrics/Model/ModelCreatorEF.cpp
                        CreateInfo(data);
                            data.ReadValue("Thickness2D"...) //读取厚度
                            data.ReadValue("Sector"...) //读取模型分数
                        CreateConstraint(data) // 创建约束, 即边界条件
                    CreateAnalysis(); // 创建分析, /FiniteElement/Model/FeModelCreator.cpp
                        for(分析类型){
                            AnalysisBase *analysis =NewAnalysis(type);
                            analysis->AnalysisFe::Read(); // 动态多态, 转向 AnalysisFe::Read()
                                auto reader=controlReader(); // control.json 单例
                                auto job =NewJob(); //任务工厂方法
                                _jobID=model()->AddComponentGenID(job); //分配 jobID, 即 工况号
                                _name=reader->GetCurrentNode(); // 获取有限元分析名称
                                // 设置 jobID 对应的名称, 存入 unordered_map<string, int> _nameMap, 不同 reader 共享
                                reader->SetIDByName(_name,_jobID);
                                job->Create(*reader); //virtual, 创建 new job
                                    NonlinearPara.Create(data) //非线性迭代; /Electrics/Analysis/LoadJobEF.cpp
                                    couple // 耦合
                                    load // 创建载荷
                                    int id=model()->AddComponentGenID(_PostAnalysis); // 添加后处理分析任务
                            anlsCtrl()->AddAnalysis(analysis) // append 分析到分析队列末尾
                        }

                    Initialize(); // 各个场自己的特定初始化, /Electrics/Model/ModelCreatorEF.cpp
                        model()->CopyThreadsMaterial(); //复制线程私有材料对象
                        ApplyConstraint(); // 施加约束
                            ExpandCons2Ele();//施加约束到单元上
                            BuildMstSlvDofConnection();// 建立主从约束
                        ArrangeEquationNo(); // 数据自由度排序
                        PartitionMesh(); // 剖分网格
                        AdjustRankComponent(); // 调整构件所属MPI 节点
                        CreateElementList(); // 生成单元列表
                        RemoveOtherRankComponent(); //删除其他MPI 节点上的对象
                        globalInfo()->SetDouble("Penalty",1e60)// 罚系数确定
                        WriteGeoInfo(); //写出网格信息

            memTracker()->SnapShot("CreateModel"); //分析内存,

        _pIml->AnalyzeModel(); //运行模型分析, /Common/Control/Control.cpp
            //迭代每个分析
            while(true){
                auto analysis = anlsCtrl()->GetNextAnalysis();
                memTracker();timeTracker(); // 记录内存, 时间占用
                analysis->Run(); // 运行每个分析. /Common/Analysis/AnalysisBase.cpp
                    Analyze(); // 虚函数, /Electrics/Analysis/AnalysisStaticEF.cpp
                        InitiDyMesh();  // 初始化动网格 和 后验误差分析 组件
                        InitBeforeJobLoop(); // 静态电场分析初始化
                            ResultManipEM* p_resultHandle=resultManip(); //初始化计算结果存储指针
                            RecordRestrictedEqNo(); // 记录受到约束的自由度
                            _load->Inital(); // 激励初始化
                        time()->Clear(); // 初始化 AMR 时间步
                        analyze_EM_Field_Nonlinear(); // 静电场分析 非线性迭代
                            AssembleGlobleFv(p_GFv_T); //组装总体载荷列向量(力矢量)
                                cons->AssembleFv(p_GFv_T); // 计算约束对应的激励
                            CalculateTotalForce(); //计算total force, 存入 p_GRt
                                cons->AssembleFin(p_GFinAccum); //计算约束对应的内力 Fin
                            AssembleGlobleK(K); //组装总体刚度矩阵
                                CalcuElementMatrixKe(Ke);//组装单元上的局域刚度矩阵, 迭代所有单元, 包括体单元, 面单元, 线单元.
                                cons->AssembleKe(mat); //组装约束对应的刚度矩阵,对于Floating,Open,Period,Volt,Scalar1st,Scalar3rd约束节点
                                    ElePt->CalcEleMatrixKeOpen(ke);//开放边界的贡献
                                    ElePt->CalcEleMatrixKeShield(ke);//介电屏蔽的贡献
                            CallSolver_T(K,p_GRt.data(),p_Solu_T.data());//求解方程
                                resultManip()->SetSolution(_jobID, gX); // 存储自由度解X, 到 _jobID steroNO
                            LineSearch();//回溯线搜索
                                p_resultHadle->CopySoluData(STP_SEARCH, _jobID, aStep); // 复制 自由度X 到 STP_SEARCH SeteroNO
                            p_resultHadle->CopySoluData(STP_SEARCH,_jobID,1); // 复制 自由度X 到 STP_SEARCH SeteroNO
                            p_resultHadle->AccumSoluData(STEPBEG_ACCUM,STP_SEARCH); // 累加 自由度
                        isConverged=AMRCheck(); // 后验误差分析 check
                            auto refnStatus=ConvergenceCheck(); // 后验误差分析
                            UpdateWriter(); // 更新 field writer
                            PostAnls_Dump(); // post 输出, Field value
                                AlgPostAnalysisEF analysis; // 新建Post分析对象
                                analysis.Run(); // 运行后处理分析, 电磁力, 力矩, 电容
                                    PostProcessElementData(jobID);//计算场量如 V,E,D,Energy 则高斯点和节点的值
                                    PostProcessElementNodeForce(jobID);//计算节点上受的电磁力
                                    _PostAnalysis->Analysis();// 依次运行特定的后处理任务
                                    auto io=common::resultWriter()->GetID(); // 输出结果到 h5
                                    Print2TxtFile();//输出结果到 txt 文件
                                FieldWriterEF* writer=objects()->GetUniPtr<FieldWriterEF>();
                                writer->Write(_job, "Static"); // 写入场量到 result.h5
                                    _time=timer()->GetTotalTime(); // 获取 Field Value 的时刻
                                    FieldWriterEF::WriteField(); // ov; 写入到 hdf5
                                        FieldWriterEM::WriteField();
                                            GetFieldDataGenTest();
                                    SetTestFieldData(); // 写入到 ensight
                            isConverged= _dyMesh->RunAMR(move(cellRefnOrNot)); // 网格自适应
                            if(!isConverged) ReInitForDyMesh(); // FieldValue, DOF 刷新
                        time()->StepForward(); // AMR 时间步 forward

                memTracker();timeTracker(); // 记录内存, 时间占用
            }
        _pIml->Finalize(); // 计算收尾, /Common/Control/Control.cpp
            ThreadUtil::WaitAll(); // async
            timeTracker();memTracker(); //profiler
            objects()->Clear(); // objects
            Pyutil::Finalize(); //python
            PetscUtil::Finalize(); //petsc
            HypreUtil::Finalize(); //hypre
            AmgxUtil::Finalize(); //amgx
            MPIUtil::Finalize(); //mpi
            CUDAUtil::Finalize(); //gpu
```
