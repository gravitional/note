#include <iostream>
#include <string>
using namespace std;

class ExamInfo
{
private:
    string name; //课程名称
    enum
    {
        GRADE,
        PASS,
        PERCENTAGE
    } mode; // 计分方式
    union
    {
        char grade;  //等级制成绩
        bool pass;   //是否通过
        int percent; //百分制成绩
    };

public:
    ExamInfo(string name, char grade) : name(name), mode(GRADE), grade(grade) {}
    ExamInfo(string name, bool pass) : name(name), mode(PASS), pass(pass) {}
    ExamInfo(string name, int percentage) : name(name), mode(PERCENTAGE), percent(percent) {}
    void show();
};

void ExamInfo::show()
{
    cout << name << " : ";
    switch (mode)
    {
    case GRADE:
        cout << grade <<endl;
        break;
    case PASS:
        cout << (pass ? "PASS" : "FAIL")<<endl;
        break;
    case PERCENTAGE:
        cout << percent<<endl;
        break;
    }
}

int main()
{
    ExamInfo course1("English", 'B');
    ExamInfo course2("Calculus", true);
    ExamInfo course3("C++ programming", 85);
    course1.show();
    course2.show();
    course3.show();
    return 0;
}