# matlab 脚本examples

彭芳麟 p349;  Laplace 方程

```matlab
clearvars;
omega=1.5;
pX_tot=30; pY_tot=30;
lenX=1;lenY=1;
x=linspace(0,lenX,pX_tot); y=linspace(0,lenY,pY_tot);
amp=1;
phi(:,pY_tot)=amp.*sin(3*pi/lenY.*y)';
phi(pX_tot,:)=amp.*(sin(3*pi/lenX.*x).*cos(pi/lenX.*x));
for N=1:100
    for ni=2:pX_tot-1
        for nj=2:pY_tot-1
            ph=(phi(ni+1,nj)+phi(ni-1,nj)+phi(ni,nj+1)+phi(ni,nj-1));
            phi(ni,nj)=(1-omega)*phi(ni,nj)+0.25*omega*(ph);
        end
    end
end
% colormap([0.5,0.5,0.5]);
surfc(phi)
```
