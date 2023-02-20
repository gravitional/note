// 求解器调用栈,电流场, 瞬态
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
                    CreateMonitor(); // 创建监视
                    CreateNode(); // 创建节点
                        CompNode* node=NewNode(); //节点工厂方法，调用具体实现
                    CreateElement(data); // 创建单元
                    CreateMaterial(data); //创建材料
                    CreateInitialField(data);
                    CreateMisc(data); //创建各场的特有数据, /Current/Model/ModelCreatorCF.cpp
                        CreateInfo(data);
                            data.ReadValue("Thickness2D"...) //读取厚度
                            data.ReadValue("Sector"...) //读取模型分数
                        CreateConstraint(data) // 创建约束, 即边界条件, 
                        // Volt, Current, Floating, Open, Periodic, Contact, Scalar1st, Scalar3rd, Nonbdr
                    CreateAnalysis(); // 创建分析, /FiniteElement/Model/FeModelCreator.cpp
                        //迭代分析类型
                        analysis->Read(); // 动态多态, 转向 AnalysisFe::Read()
                            auto reader=controlReader(); // control.json 单例
                            auto job =NewJob(); //任务工厂方法
                            _jobID=model()->AddComponentGenID(job); //分配 jobID, 即 工况号
                            _name=reader->GetCurrentNode(); // 获取有限元分析名称
                            reader->SetIDByName(_name,_jobID);// 设置 jobID 对应的名称,
                                                            //存入unordered_map<string, int> _nameMap, 不同 reader 共享
                            job->Create(*reader); //virtual, 创建 new job
                                NonlinearPara.Create(data) //非线性迭代; /Current/Analysis/LoadJobCF.cpp
                                couple // 耦合
                                load // 创建载荷
                                int id=model()->AddComponentGenID(_PostAnalysis); // 添加后处理分析任务
                        anlsCtrl()->AddAnalysis(analysis) // append 分析到分析队列末尾
                            //非续算时清空独立计时器
                            //清空当前分析的标记 _flags
                            //设置新的变量输出

                    Initialize(); // 各个场自己的特定初始化, /Current/Model/ModelCreatorCF.cpp
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
                    Analyze(); // 虚函数, /Current/Analysis/AnalysisDynaCF.cpp
                        InitBeforeJobLoop(); // 电流场分析初始化
                            ResultManipEM* p_resultHandle=resultManip(); //初始化计算结果存储指针
                            // 初始化 p_GFinAccum, p_GRt, p_GFv_T, p_Solu_T, P_GFv_Pre
                            RecordRestrictedEqNo(); // 记录受到约束的自由度
                            cons->SetValue(); // 设置边界条件，约束值
                        // 根据设置, 运行不同分析算法, 
                        analyze_EM_Field_Fixed(); // 非线性迭代, 固定时间步长
                            //---------------- 时间循环开始
                            // ---- 结果导入, 续算
                            // ---- 零时刻计算
                            UpdateExcitationsConstraint(_timer->GetTime());//更新激励和约束
                            SolveOneTimeStep(_jobID); //
                                CalculateTotalForce(); //计算total force, 存入 p_GRt
                                    ele->CalcuElementFin(internalFv); //单元对内力的贡献
                                    cons->AssembleFin(p_GFinAccum); //计算约束对应的内力
                            AssembleGlobleK(Sparse::GetMatrix<double>("K")); //组装总体刚度矩阵
                                ele->CalcuElementMatrixKe(Ke);//组装单元上的局域刚度矩阵, 迭代所有单元, 包括体单元, 面单元, 线单元.
                                cons->AssembleKe(mat); //组装约束对应的刚度矩阵
                                    ElePt->CalcEleMatrixKeOpen(ke);//开放边界的贡献
                                    ElePt->CalcEleMatrixKeShield(ke);//介电屏蔽的贡献
                            CallSolver_T(K,p_GRt.data(),p_Solu_T.data());//求解方程
                            LineSearch();//回溯线搜索, 广义牛顿法
                            p_resultHadle->CopySoluData(STP_SEARCH,_jobID,1);//复制解到 线搜索存储空间
                            p_resultHadle->AccumSoluData(STEPBEG_ACCUM,STP_SEARCH); // 将结果累加
                            bool converged= false;//判断解是否收敛
                            PostProcessStepFiledData(); // 后处理, 当前时间步场数据
                                AlgPostAnalysisEF analysis; // 新建Post分析对象
                                analysis.Run(); // 运行后处理分析, 电磁力, 力矩, 电容
                                    PostProcessElementData(jobID);//计算场量如 V,E,D,Energy 则高斯点和节点的值
                                    PostProcessElementNodeForce(jobID);//计算节点上受的电磁力
                                    _PostAnalysis->Analysis();// 依次运行特定的后处理任务
                                    auto io=common::resultWriter()->GetID(); // 输出结果到 h5
                                    Print2TxtFile();//输出结果到 txt 文件
                                FieldWriterEF* writer=objects()->GetUniPtr<FieldWriterEF>();
                                writer->Write(_job, "Static"); // 写入场量到 result.h5
                                    WriteField(); // 写入到 hdf5
                                    SetTestFieldData(); // 写入到 ensight
                        
                        analyze_Coupled_Field(); // 耦合场分析
                        analyze_EM_Field_Adapt(); // 非线性迭代, 自适应时间步长


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
