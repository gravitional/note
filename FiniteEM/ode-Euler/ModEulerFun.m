function T=ModEulerFun(fun,x0,y0,x_end,order)
% Gjeulerfun.m函数为用改进的Euler法求解微分方程
% fun为一阶微分方程的函数
% x0,y0为初始条件
% xn为取值范围的一个端点
% h为区间步长
% N为区间的个数
% x为Xn构成的向量
% y为Yn构成的向量
x=zeros(1,order+1);
y=zeros(1,order+1);
x(1)=x0;y(1)=y0;
h=(x_end-x0)/order;
for k=1:order
x(k+1)=x(k)+h;
z0=y(k)+h*feval(fun,x(k),y(k));
y(k+1)=y(k)+h/2*(feval(fun,x(k),y(k))+feval(fun,x(k+1),z0));
end
T=[x',y'];