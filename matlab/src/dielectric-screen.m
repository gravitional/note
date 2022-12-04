clearvars;clc;%初始化
% close all;
tic

offset_c2ML = 1; % C++数组从0开始，MatLab 从1开始
%电磁常数
er0 = 8.854187817e-12;
mu0 = 4 * pi * 1e-7;
penalty = 1e10; %罚系数

J = 1e6; %source
eps_m = 40; %静电屏蔽壳层的相对介电常数

eps_in = 1; %内部区域的介电常数
eps_out = 1; %外部区域的介电常数
screen_d = 1e-5; %壳层的厚度
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%++++导入数据
NodeCoord = readmatrix('Node.txt'); %节点坐标
PlaneNdID = readmatrix('Element.txt'); %每个单元的 节点编号
PlaneNdID = PlaneNdID + offset_c2ML; % 编号偏移
offset_plane = 1; %单元的节点数据在 2 3 4列, 偏移量=1
offset_surface = 1; % surface 单元中节点列的偏移, 从第2行开始
%++++SurfaceNdID: 线单元编号 节点1ID 节点2ID 线所属单元ID 罚系数
SurfaceNdID = readmatrix('Surface.txt'); % 边界单元
SurfaceNdID = SurfaceNdID + offset_c2ML; % 编号偏移

%++++ 将单元划分到不同的 Region
region(1, :) = [0 395] + offset_c2ML; %外部区域
region(2, :) = [396 522] + offset_c2ML; %外部区域

%++++第一类边界条件上的单元，准备罚系数
voltRefID(1, :) = [533 542] + offset_c2ML; %第二组边界
voltRefID(2, :) = [523 532] + offset_c2ML; %第一组边界

%++++介电屏蔽条件所在的 surface ID
screenRID = [543 571] + offset_c2ML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%++++增加单元的 区域ID
regionID = zeros(length(PlaneNdID), 1);
%不同的区域，赋予不同的ID
for i = region(1, 1):region(1, 2)
    regionID(i) = 2; % 外部
end

for i = region(2, 1):region(2, 2)
    regionID(i) = 1; %内部
end

PlaneNdID = [PlaneNdID, regionID]; % 附上 Region ID.

%++++扩充边界节点ID矩阵, 填充罚系数的系数
tmp = zeros(length(SurfaceNdID), 1);
SurfaceNdID = [SurfaceNdID, tmp];
%第一类边界条件
for i = 1:length(SurfaceNdID)

    if (voltRefID(1, 1) <= SurfaceNdID(i, 1)) && (SurfaceNdID(i, 1) <= voltRefID(1, 2))
        SurfaceNdID(i, 5) = 10; %10V电压
    end

    if (voltRefID(2, 1) <= SurfaceNdID(i, 1)) && (SurfaceNdID(i, 1) <= voltRefID(2, 2))
        SurfaceNdID(i, 5) = -10; %-10V电压
    end

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 初始化 刚度矩阵 和 力矩阵
GlobKe = zeros(length(NodeCoord));
GlobBe = zeros(length(NodeCoord), 1);

NodeNum = size(NodeCoord); %节点数目
PlaneSize = size(PlaneNdID); %单元数目

% 网格区域作图
figure
subplot(1, 2, 1)
hold on
nodex = zeros(3, 1);
nodey = zeros(3, 1);
%对三角形单元循环
for ne = 1:PlaneSize(1)

    for np = 1:3
        nodeID = PlaneNdID(ne, np + offset_plane); %节点ID
        nodex(np) = NodeCoord(nodeID, 2); %第ne个单元的第np个节点的x坐标
        nodey(np) = NodeCoord(nodeID, 3); %第ne个单元的第np个节点的y坐标
    end

    % 计算中常用到的系数
    be(1) = nodey(2) - nodey(3);
    be(2) = nodey(3) - nodey(1);
    be(3) = nodey(1) - nodey(2);

    ce(1) = -nodex(2) + nodex(3);
    ce(2) = -nodex(3) + nodex(1);
    ce(3) = -nodex(1) + nodex(2);

    plot([nodex(1) nodex(2) nodex(3) nodex(1)], [nodey(1) nodey(2) nodey(3) nodey(1)])

    Delta_e = (be(1) * ce(2) - be(2) * ce(1)) / 2; %单元三角形面积
    %每个单元内的刚度矩阵
    Ke_ij = zeros(3, 3);

    for ni = 1:3

        for nj = 1:3

            switch PlaneNdID(ne, 5)
                case 1 %如果是球壳内部单元
                    er = eps_in;
                case 2 %如果是球壳外部单元
                    er = eps_out;
                otherwise
                    er = eps_out;
            end

            %%单元内的Ke矩阵
            Ke_ij(ni, nj) = er * (be(ni) * be(nj) + ce(ni) * ce(nj)) / (4 * Delta_e);
            %累加，组装 global 刚度矩阵
            refi = PlaneNdID(ne, ni + 1); refj = PlaneNdID(ne, nj + 1); %单元Ke在全局Ke中的位置
            GlobKe(refi, refj) = GlobKe(refi, refj) + Ke_ij(ni, nj);
        end

    end

    bei = zeros(1, 3);

    for ni = 1:3
        ref = PlaneNdID(ne, ni + 1);
        GlobBe(ref) = GlobBe(ref) + bei(ni);
    end

end

%边界单元数目
SurfaceSize = size(SurfaceNdID);
NdFlag = zeros(NodeNum(1), 1); %标记已经处理过

%对边界元(第一类边条件)循环
nodex = zeros(2, 1);
nodey = zeros(2, 1);
%边界单元刚度矩阵初始化
const_ij = [1 -1;
        -1 1];

for nl = 1:SurfaceSize(1)

    for np = 1:2
        refID = SurfaceNdID(nl, np + offset_surface);
        nodex(np) = NodeCoord(refID, 2); %第n个表面元的第np节点的x坐标
        nodey(np) = NodeCoord(refID, 3); %第n个表面元的第np节点的y坐标
    end

    plot([nodex(1) nodex(2) nodex(1)], [nodey(1) nodey(2) nodey(1)], 'r', 'linewidth', 2)

    %处理边界线元上的节点1,2
    refID = SurfaceNdID(nl, 2:3); % 边界上的节点编号
    %判断是否在 介电屏蔽 边界条件 引用ID范围内
    inRange = all((screenRID(1, 1) <= refID) .* (refID <= screenRID(1, 2)));

    if (inRange) %
        %行差分,即 x2-x1, y2-y1
        tmp_vec = diff([nodex nodey]);
        Delta_l = norm(tmp_vec); %边界线单元的长度
        %%单元内的Ke矩阵
        Ke_ij = -screen_d * eps_out / Delta_l * const_ij;

        for ni = 1:2

            for nj = 1:2
                %单元Ke在全局Ke中的位置
                refi = PlaneNdID(nl, ni + 1); refj = PlaneNdID(nl, nj + 1);
                %累加，组装 global 刚度矩阵
                GlobKe(refi, refj) = GlobKe(refi, refj) + Ke_ij(ni, nj);
            end

        end

    end

    %%第一类边界条件
    for surf_node = 2:3
        refID = SurfaceNdID(nl, surf_node); % 边界上的节点编号

        if (NdFlag(refID) == 0) %避免重复累加
            GlobKe(refID, refID) = penalty; %罚系数
            GlobBe(refID) = SurfaceNdID(nl, 5) * penalty; %0*penalty
            NdFlag(refID) = 1;
        end

    end

end

axis equal

NdFlag = zeros(NodeNum(1), 1);
phi = GlobKe \ GlobBe; %解方程

X = NodeCoord(:, 2);
Y = NodeCoord(:, 3);
%对电势作图
subplot(1, 2, 2)
tri0 = delaunay(X, Y);
trisurf(tri0, X, Y, phi)
axis tight

Bx = zeros(length(NodeCoord), 1);
By = zeros(length(NodeCoord), 1);
Ex = zeros(length(NodeCoord), 1);
Ey = zeros(length(NodeCoord), 1);

for ne = 1:length(PlaneNdID) %对三角形单元做循环

    for np = 1:3
        refID = PlaneNdID(ne, np + offset_plane);
        nodex(np) = NodeCoord(refID, 2);
        nodey(np) = NodeCoord(refID, 3);
    end

    %%计算有限元方程系数
    be(1) = nodey(2) - nodey(3);
    be(2) = nodey(3) - nodey(1);
    be(3) = nodey(1) - nodey(2);

    ce(1) = -nodex(2) + nodex(3);
    ce(2) = -nodex(3) + nodex(1);
    ce(3) = -nodex(1) + nodex(2);

    Delta_e = (be(1) * ce(2) - be(2) * ce(1)) / 2;

    for ni = 1:3

        for np = 1:3
            refi = PlaneNdID(ne, 1 + ni); refp = PlaneNdID(ne, np + 1);
            Bx(refi) = Bx(refi) + ce(np) * phi(refp) / 2 / Delta_e;
            By(refi) = By(refi) - be(np) * phi(refp) / 2 / Delta_e;

            Ex(refi) = Ex(refi) + be(np) * phi(refp) / 2 / Delta_e;
            Ey(refi) = Ey(refi) + ce(np) * phi(refp) / 2 / Delta_e;
        end

        NdFlag(refi) = NdFlag(refi) + 1;
    end

end

% Bx = Bx./NdFlag;
% By = By./NdFlag;
Ex = Ex ./ NdFlag;
Ey = Ey ./ NdFlag;

%quiver(X,Y,Bx,By)
figure
quiver(X, Y, Ex, Ey)
axis tight
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
toc
