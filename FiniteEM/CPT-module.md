# CPT 模块

带电粒子追踪

调研功能需求, 基本构成元素
数学公式模型,
离散模型, 数值表示
有限元求解

## 电磁场项目

+ 情况1: 静电场 静磁场 叠加

+ 情况2: 电磁场耦合.瞬态电磁场求解模块.

## 带电粒子项目

### 初始设定

粒子数目 N

### 单粒子的状态参数

+ 运动学变量, 位移, 速度
+ 电磁参数, 电荷

### 求解时间依赖

+ 动力学方程->运动学方程

相对论情形,
非相对论情形

### 边界处理

+ 约束条件

+ 壁面条件: 吸收, 反射,  穿过,  等等.

+ 判断是否 接近/越过  求解区域边界,  如果越过, 是 `0+`, 还是 `0-` 状态.

通过 比较区域最大 `坐标值`, 比较粒子与区域的 `垂直水平截线` 判断.