#include <iostream>
using namespace std;

class Vehicle
{
public:
    Vehicle() : weight(0.0) {}       //默认构造函数
    Vehicle(double w) : weight(w) {} //含参构造函数
    //虚析构函数必须存在，
    //所有子类对象在析构时都是以Vehicle*的方式调用析构函数的，
    //以虚析构函数调用才能调用到正确的析构函数  才不会导致内存泄露
    virtual ~Vehicle() {}
    //复制自己
    virtual Vehicle *copy() const
    {
        return new Vehicle(*this);
    }
    //读 weight
    double
    get_weight() const
    {
        return weight;
    }
    //写 weight
    Vehicle *set_weight(double w)
    {
        weight = w;
        return this;
    }

private:
    double weight;
};
//定义陆地上的车辆
class RoadVehicle : public Vehicle
{
public:
    //默认构造函数，容许声明RoadVehicle的数组
    RoadVehicle() {}
    RoadVehicle(double w) : Vehicle(w) {}
    //拷贝构造函数
    RoadVehicle(const RoadVehicle &RV) : Vehicle(RV.get_weight()) {}
    //复制自己
    Vehicle *copy() const
    {
        return new RoadVehicle(*this);
    }
    virtual ~RoadVehicle() {}
};
//定义飞机
class AirCraft : public Vehicle
{
public:
    //默认构造函数，容许声明AirCraft的数组
    AirCraft() {}
    AirCraft(double w) : Vehicle(w) {}
    //拷贝构造函数
    AirCraft(const AirCraft &AC) : Vehicle(AC.get_weight()) {}

    Vehicle *copy() const
    {
        return new AirCraft(*this);
    }
    virtual ~AirCraft() {}
};

//---------------------------------------------智能指针类定义
class VehicleSurrogate
{
public:
    //默认构造函数，使得可以声明VehicleSurrogate的数组
    VehicleSurrogate() : vp(new Vehicle()), num(new int(1)) {}
    //拷贝构造函数，可发现此时它所代理的实际对象并未复制
    VehicleSurrogate(const VehicleSurrogate &VS) : vp(VS.vp), num(VS.num)
    {
        ++(*num);
    }
    //以它所代理的Vehicle对象初始化
    VehicleSurrogate(const Vehicle &Ve) : vp(Ve.copy()), num(new int(1)) {}
    //重载赋值，可发现此时它所代理的实际对象并未复制
    VehicleSurrogate &operator=(const VehicleSurrogate &VS)
    {
        if (this != &VS) //检测自我赋值
        {
            //删除原来的旧的关联对象
            if (--(*num) == 0)
            {
                delete vp;
                delete num;
            }
            //赋值新的关联对象
            vp = VS.vp;
            num = VS.num; //同步 VehicleSurrogate 对象数目

            ++(*num);
        }
        return *this;
    };
    ~VehicleSurrogate()
    {
        if (--(*num) == 0) // 智能指针
        {
            delete vp;
            delete num;
        }
    }

    int get_num() const
    {
        return *num;
    }
    //代理Vehicle类行为的函数，读操作无需复制所代理的实际对象
    double get_weight() const
    {
        return vp->get_weight();
    }
    //写时复制策略，写时必须复制所代理的实际对象
    VehicleSurrogate &set_weight(double w)
    {
        if ((*num) == 1)
        {
            vp->set_weight(w);
        }
        else
        {
            --(*num);

            vp = vp->copy(); //真正的复制发生在这里
            num = new int(1);

            vp->set_weight(w);
        }
        return *this;
    }

private:
    Vehicle *vp; // vehicle pointer, 指向被代理实体的指针
    int *num;    // 代理的实体数目
};

int main()
{
    //测试上述智能指针
    VehicleSurrogate parking_lot[100];
    RoadVehicle x(10);
    parking_lot[0] = RoadVehicle(x);
    parking_lot[1] = parking_lot[0];
    parking_lot[0].set_weight(5.0);
    cout << parking_lot[0].get_weight() << endl
         << parking_lot[0].get_num() << endl;
    cout << parking_lot[1].get_weight() << endl
         << parking_lot[1].get_num() << endl;
}