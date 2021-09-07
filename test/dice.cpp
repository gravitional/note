#include <iostream>
#include <cstdlib>
using namespace std;

enum GameStatus
{
    WIN,
    LOSE,
    PLAYING
};
int main()
{
    int sum, myPoint;
    GameStatus status;
    unsigned seed;
    int rollDice();
    cout << "Please enter an unsigned integer: ";
    cin >> seed;
    srand(seed);
    sum = rollDice();
    switch (sum)
    {
    case 7: //如果和为7或11 则为胜，状态为 WIN
    case 11:
        status = WIN;
        break;
    case 2: //如果和为2,3,12 则为负，状态为 LOSE
    case 3:
    case 12:
        status = LOSE;
        break;
    default:
        status = PLAYING;
        myPoint = sum;
        cout << "point is " << myPoint << endl;
        break;
    }
    while (status == PLAYING)
    { // 状态为 PLAYING,继续
        sum = rollDice();
        if (sum == myPoint) //某轮的和数等于点数
            status = WIN;
        else if (sum == 7) //出现和数为7则为负
            status = LOSE;
    }
    // 当状态不为  PLAYING 时循环结束，输出结果
    if (status == WIN)
        cout << "player wins" << endl;
    else
        cout << "player loss" << endl;
    return 0;
}

int rollDice()
{
    int die1 = 1 + rand() % 6;
    int die2 = 1 + rand() % 6;
    int sum = die1 + die2;
    cout << "player rolled " << die1 << " + " << die2 << " = " << sum << endl;
    return sum;
}
