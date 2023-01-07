% 调用 改进Euler方法
x0=1;y0=3;
x_end=-1.5; order=15;
res=ModEulerFun('fun',x0,y0,x_end,order);
plot(res(:,1),res(:,2))