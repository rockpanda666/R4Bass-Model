# R4Bass-Model

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
