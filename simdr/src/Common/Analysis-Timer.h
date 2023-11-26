#pragma once
#include "CommonHeader.h"
#include <set>

using namespace std;

namespace common
{
    class DataIO;
}

// 观察者接口类
class IObserverTimer
{
public:
    virtual void TimeUpdate(scalar time) = 0; // 根据时刻更新
    virtual void IterUpdate(int iter) = 0;    // 根据 iter 更新, iter 无需和 step 相同
}

// 时间序列控制类
class COMMON_API Timer:
{
public:
    Timer();
    ~Timer();

    // 导入导出接口,  control.json/Dump/Time 节点
    // Time, LastTotalTime, LastStep, CurrnetStep, NextStep, SubTime, SubStep, SpecialTime, StepNo
    void Read();
    void Write();

    //============== 分析
    // 开始新的分析步; 更新 _lastTotalTime; 然后 SetTime(0);
    void NewAnalysis()
    {
        _lastTotalTime += _time;
        SetTime(0);
    }

    // 设置时间, _time, _subTime 为零, _iterNo 为零; TimeUpdateObserver();
    void SetTime(double time)
    {
        _time = time;
        _subTime = time;
        _iterNo = 0;
        TimeUpdateObserver();
    }

    // 设置flag, 用于记录 特殊总时间
    void SetSpecial(bool isSpecial) { _isSpecial = isSpecial; }

    //=============== 时间
    // 设置步长
    void SetStep(double dt)
    {
        _nextStep = dt;
    }
    //+++++++++++++ 前进
    // 使用 _nextStep 前进;
    void StepForward()
    {
        SetTime(_time + _nextStep);
        if (_isSpecial)
            _specialTime += _nextStep;
        _lastStep = _currentStep;
        _currentStep = _nextStep;
        _stepNo++;
        _restartStepNo++;
    }

    // 显式 step 前进
    void StepForward(double dt)
    {
        SetStep(dt);
        StepForward();
    }

    // 时间回退
    void StepBack()
    {
        SetTime(_time - _currentStep);
        if (_isSpecial)
            _specialTime -= _currentStep;
        _currentStep = _lastStep;
        _stepNo--;
        _restartStepNo--;
    }

    // 前进到特定分析步时刻
    void StepTo(double analysisTime)
    {
        double dt = analysisTime - _time;
        StepForward(dt);
    }

    // 是否零时刻
    bool IsZeroTime() const { return GetTotalTime() == 0.0; }
    // 取当前步长;
    double GetStep() const { return _currentStep; }
    // 取当前步长倒数;
    double GetStepInv() const { return Util::IsZero(_currentStep) ? 0 : 1 / _currentStep; }
    // 取上一个步长
    double GetLastStep() const { return _lastStep; }
    // 取当前分析时刻
    double GetTime() const { return _time; }
    // 取总时刻
    double GetTotalTime() const { return _lastTotalTime + _time; }
    // 取当前存储 的特殊时刻
    double GetSpecialTime() const { return _specialTime; }

    int GetStepNo() const { return _stepNo; }               // 取步数
    int GetRestartStepNo() const { return _restartStepNo; } // 取重启对应的时间步数

    //========================= 子时刻
    // 在每一个主时刻 t0 停驻期间, 子时刻可以独立变化; 担当主时刻变动, 子时刻也被更新;
    // 子时刻 前进
    void SubStepForward(double subdt)
    {
        _subStep = subdt;
        _subTime += subdt;
    }

    // 取子时间步长
    double GetSubStep() const
    {
        return _subStep > 0 ? _subStep : GetStep();
    }

    double GetSubTime() const { return _subTime; } // 取子时刻

    //================== 迭代
    // 累加迭代次数
    void IncIter()
    {
        ++_iterNo;
        IterUpdateObserver();
    }

    int GetIterNo() const { return _iterNo; }        // 取迭代次数
    void SetIterNo(int iterNo) { _iterNo = iterNo; } // 设置迭代次数

    //========================= 清空
    void Clear()
    {
        SetTime(0);
        _lastTotalTime = 0;
        _currentStep = 0;
        _nextStep = 0;
        _lastStep = 0;
        _subStep = 0;
        _specialTime = 0;
        _isSpecial = false;
        _stepNo = 0;
        _restartStepNo = 0;
    }

    //====================== 观察者模式
    // 添加观察者
    void AddObserver(IObserverTimer *observer)
    {
        _observers.emplace(observer);
    }
    // 移除观察者
    void RemoveObserver(IObserverTimer *observer)
    {
        _observers.erase(observer);
    }

    // 更新观察者
    void TimeUpdateObserver()
    {
        for (auto ob : _observers)
        {
            ob->TimeUpdate(GetTime());
        }
    }
    void IterUpdateObserver()
    {
        for (auto ob : _observers)
        {
            ob->IterUpdate(GetIterNo());
        }
    }

private:
    // 设置时间
    void SetTime(double time)
    {
        _time = time;
        _subTime = time;
        _iterNo = 0;
        TimeUpdateObserver();
    }

protected:
    double _time;          // 当前分析时刻
    double _lastTotalTime; // 分析步总开始时刻

    // 循环更新
    double _lastStep;    // 上一个增量步长
    double _currentStep; // 当前增量步长
    double _nextStep;    // 下一个增量步长

    double _subTime; // 子时刻
    double _subStep; // 子时间步长

    double _specialTime; // 特殊总时刻
    bool _isSpecial;     // flag, 是否记录特殊总时刻

    int _stepNo;        // 时间步数
    int _restartStepNo; // 重启时刻对应的 时间步数
    int _iterNo;        // 本时间步迭代数

    std::set<IObserverTimer *> _observers; // 观察者
}