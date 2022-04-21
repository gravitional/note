# mathematica Cuba

[library for multidimensional numerical integration](http://www.feynarts.de/cuba/)

`Cuba` 库为多维数值积分提供了四个独立的程序选择. 
`Vegas`, `Suave`, `Divonne`, 和 `Cuhre`.

它们的工作方法非常不同, 总结在下表中.

<table cellspacing="2" cellpadding="5">
<tbody><tr>
    <th>Routine</th>
    <th>基本积分方法</th>
    <th>算法类型</th>
    <th>Variance reduction</th></tr>
<tr>
<td>Vegas</td>
    <td>Sobol 准随机采样<br>
        <i>或</i> Mersenne Twister 伪随机采样<br>
        <i>或</i> Ranlux 伪随机采样</td>
    <td>Monte Carlo<br>
        Monte Carlo<br>
        Monte Carlo</td>
    <td>importance sampling</td></tr>
<tr>
    <td>Suave</td>
    <td>Sobol 准随机采样<br>
        <i>或</i> Mersenne Twister 伪随机采样<br>
        <i>或</i> Ranlux 伪随机采样</td>
    <td>Monte Carlo<br>
        Monte Carlo<br>
        Monte Carlo</td>
    <td>globally adaptive subdivision<br>
        + importance sampling</td></tr>
<tr>
    <td>Divonne</td>
    <td>Korobov 准随机采样<br>
        <i>或</i> Sobol 准随机采样<br>
        <i>或</i> Mersenne Twister 伪随机采样<br>
        <i>或</i> Ranlux 伪随机采样<br>
        <i>或</i> cubature rules</td>
    <td>Monte Carlo<br>
        Monte Carlo<br>
        Monte Carlo<br>
        Monte Carlo<br>
        deterministic</td>
    <td>stratified sampling,<br>
        aided by methods from<br>
        numerical optimization</td></tr>
<tr>
    <td>Cuhre</td>
    <td>cubature rules</td>
    <td>deterministic</td>
    <td>globally adaptive subdivision</td></tr>
</tbody></table>
