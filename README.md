# R4Bass-Model（解法1.传统运算）
<a href='https://github.com/rockpanda666/R4Bass-Model/blob/main/README.md#r4bass-model%E8%A7%A3%E6%B3%952%E4%BD%BF%E7%94%A8%E6%A8%A1%E5%9D%97'>点击跳转第二段<a/>
 
``` R
q# exampleT79<-1:10Tdelt<-(1:100)/10Sales<-c(840,1470,2110,4000,7590,10950,10530,9470,7790,5890)Cusales<-cumsum(Sales)Bass.nls<-nls(Sales~M*(((P+Q)^2/P)*exp(-(P+Q)*T79))/(1+(Q/P)*exp(-(P+Q)*T79))^2,start=list(M=60630,P=0.03,Q=0.38))summary(Bass.nls)
 
##
 
## Formula: Sales ~ M * (((P + Q)^2/P) * exp(-(P + Q) * T79))/(1 + (Q/P) *
 
## exp(-(P + Q) * T79))^2
 
##
 
## Parameters:
 
## Estimate Std. Error t value Pr(>|t|)
 
## M 6.80e+04 3.13e+03 21.74 1.1e-07 ***
 
## P 6.59e-03 1.43e-03 4.61 0.0025 **
 
## Q 6.38e-01 4.14e-02 15.41 1.2e-06 ***
 
## ---
 
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
 
##
 
## Residual standard error: 727 on 7 degrees of freedom
 
##
 
## Number of iterations to convergence: 8
 
## Achieved convergence tolerance: 7.32e-06
 
# get coefficient
 
Bcoef <- coef(Bass.nls)
 
m <- Bcoef[1]
 
p <- Bcoef[2]
 
q <- Bcoef[3]

```

### 将M的起始值设置为记录的总销售额

``` R

ngete <- exp(-(p + q) * Tdelt)
 
# plot pdf
 
Bpdf <- m * ((p + q)^2/p) * ngete/(1 + (q/p) * ngete)^2
 
plot(Tdelt, Bpdf, xlab = "Year from 1979", ylab = "Sales per year", type = "l")
 
points(T79, Sales)

```
![avatar](https://github.com/rockpanda666/R4Bass-Model/blob/main/Sales%20per%20year.png)

``` R
# plot cdfBcdf <- m * (1 - ngete)/(1 + (q/p) * ngete)plot(Tdelt, Bcdf, xlab = "Year from 1979", ylab = "Cumulative sales", type = "l")points(T79, Cusales)
 
```
![avatar](https://github.com/rockpanda666/R4Bass-Model/blob/main/Year%20from%201979.png)


``` R
＃当q = 0时，只有创新者没有模仿者。
 
Ipdf <- m * ((p + 0)^2/p) * exp(-(p + 0) * Tdelt)/(1 + (0/p) * exp(-(p + 0) *
 
Tdelt))^2# plot(Tdelt, Ipdf, xlab = 'Year from 1979',ylab = 'Isales per year',# type='l')Impdf <- Bpdf - Ipdfplot(Tdelt, Bpdf, xlab = "Year from 1979", ylab = "Sales per year", type = "l",
 
col = "red")lines(Tdelt, Impdf, col = "green")lines(Tdelt, Ipdf, col = "blue")
```
![avatar](https://github.com/rockpanda666/R4Bass-Model/blob/main/Isales%20per%20year.png)

``` R
#当q = 0时
 
Icdf <- m * (1 - exp(-(p + 0) * Tdelt))/(1 + (0/p) * exp(-(p + 0) * Tdelt))
 
# plot(Tdelt, Icdf, xlab = 'Year from 1979',ylab = 'ICumulative sales',
 
# type='l')
 
Imcdf <- m * (1 - ngete)/(1 + (q/p) * ngete) - Icdf
 
plot(Tdelt, Imcdf, xlab = "Year from 1979", ylab = "Cumulative sales", type = "l",
 
col = "red")
 
lines(Tdelt, Bcdf, col = "green")
 
lines(Tdelt, Icdf, col = "blue")

```
![avatar](https://github.com/rockpanda666/R4Bass-Model/blob/main/Year%20from%201979_2.png)

# R4Bass-Model（解法2.使用模块）

首先安装diffusion包，用其中自带Safari数据和BASS模型判断生命周期
``` R
library(diffusion)
#tsSafari: different version of Safari browser from JAN 2009 to Feb 2017

dataSafari<-ts(tsSafari[,-1],frequency = 12,start=c(2009,1)) #ts is used to create time-series objects.
```
不同于平常的data.frame，时间序列的数据一般使用ts结构
其中设置开始年份为2009年1月，步长等于一年的12个月，frequency=12

``` R
View(dataSafari)
plot(dataSafari[,1:10])
```
👆运行到上面应该就出图了

重点来分析Safari6.0的产品周期
讲Safari6.0的一列单独拿出来命名为safari6

```R
safari6<-dataSafari[,8]
plot(safari6)
time(safari6)[which(safari6!=0)[1]]
safari6<-window(safari6,start=time(safari6)[which(safari6!=0)[1]])
#从7.0上市的时候开始作图，time函数定位从哪个时间点开始

safari6Cumulative<-cumsum(safari6)
#从和safari同样的时间点开始
safari6Cumulative<-ts(safari6Cumulative,start = start(safari6),frequency = frequency(safari6))
```

搭建Safari6.0的总占据市场比的线性回归模型，即到时间t时采用者的累计比例

``` R
safari6Data<-as.data.frame(cbind(safari6,safari6Cumulative))
colnames(safari6Data)<-c("y","Y")
safari6Model<-lm(y~Y+I(Y^2),data = safari6Data)
safari6Modelcoef<-coef(safari6Model)
```
将在时间t时的采用者数量占总的潜在采用者数量比例的概率密度函数定义为“y"
将到时间t时采用者的累计比例(即dF/dt=f)定义为"Y“
一一对应Bass model 的具体参数

``` R
m<-max(Re(polyroot(safari6Modelcoef)))
p<-safari6Modelcoef[1]/m
q<-safari6Modelcoef[2]+p
```
根据线性模型计算参数，其中：

M – 市场总潜力（最终采用者总数）
p – 创新参数
q – 模仿参数
设置safari6Fitted为预测模型，运用ts重新搭建时间序列如下：
``` R
safari6Fitted<-ts(fitted(safari6Model),start=start(safari6),frequency = frequency(safari6))
safari6Innovators<-p*(m - safari6Cumulative)#计算创新人群购买量
safari6Imitators<-q*safari6Cumulative/m*(m - safari6Cumulative)#计算模仿人群购买量
plot(safari6)#画出原数据
lines(safari6Fitted,col="red")#添加预测线
lines(safari6Innovators,col="blue")#添加蓝色线代表创新人群购买变化曲线
lines(safari6Imitators,col="darkgreen")#添加绿色线为模仿人群购买变化曲线
```
👆运行完得到模型
接下来运用已有参数进行对未来市场的预估看看大概趋势
越深的线代表的版本越新

``` R
palette(gray(seq(0,0.9,len=nSeries)))
plot(dataSafari[,1],ylim=range(dataSafari),col=1) 
for (i in 2:nSeries) {
  lines(dataSafari[,i],col=i)
}

safari7Model<-diffusion(dataSafari[,6])
safari7Model
plot(safari7Model)
```

运用diffusion函数包，简单暴力直接！
