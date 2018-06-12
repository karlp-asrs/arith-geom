# Estimating multi-period geometric portfolio returns

The convention among actuaries is to use multi-period geometric returns for the purpose of discounting liabilities.  So, the question is how, exactly, are they calculated.

Let's say we have a portfolio with the following properties on a 30 year horizon.



```r
asset_names=c("BbgAgg","LrgUSEq","SmlUSEq","EAFE","EAFESml","EM","PrivCredit","RE")
geom_returns=c(4,7.5,7.75,7.75,9.5,9.5,8,6.5)/100
volatility=c(6.03,17.5,21,21,28,13,9.29,15)/100
weights=c(10,17,5,16,4,8,20,20)/100
correlation=matrix(nrow=8,data=
                     c(100,0,0,0,0,0,0,10,
                       0,100,90,70,60,70,60,40,
                       0,90,100,60,65,75,65,40,
                       0,70,60,100,70,60,75,35,
                       0,60,65,70,100,45,80,30,
                       0,70,75,60,45,100,65,50,
                       0,60,65,75,80,65,100,40,
                       10,40,40,35,30,50,40,100))/100
names(geom_returns)=names(volatility)=names(weights)=asset_names
rownames(correlation)=asset_names
colnames(correlation)=asset_names
```


```r
geom_returns
```

```
##     BbgAgg    LrgUSEq    SmlUSEq       EAFE    EAFESml         EM 
##     0.0400     0.0750     0.0775     0.0775     0.0950     0.0950 
## PrivCredit         RE 
##     0.0800     0.0650
```

```r
volatility
```

```
##     BbgAgg    LrgUSEq    SmlUSEq       EAFE    EAFESml         EM 
##     0.0603     0.1750     0.2100     0.2100     0.2800     0.1300 
## PrivCredit         RE 
##     0.0929     0.1500
```

```r
weights
```

```
##     BbgAgg    LrgUSEq    SmlUSEq       EAFE    EAFESml         EM 
##       0.10       0.17       0.05       0.16       0.04       0.08 
## PrivCredit         RE 
##       0.20       0.20
```

```r
correlation
```

```
##            BbgAgg LrgUSEq SmlUSEq EAFE EAFESml   EM PrivCredit   RE
## BbgAgg        1.0     0.0    0.00 0.00    0.00 0.00       0.00 0.10
## LrgUSEq       0.0     1.0    0.90 0.70    0.60 0.70       0.60 0.40
## SmlUSEq       0.0     0.9    1.00 0.60    0.65 0.75       0.65 0.40
## EAFE          0.0     0.7    0.60 1.00    0.70 0.60       0.75 0.35
## EAFESml       0.0     0.6    0.65 0.70    1.00 0.45       0.80 0.30
## EM            0.0     0.7    0.75 0.60    0.45 1.00       0.65 0.50
## PrivCredit    0.0     0.6    0.65 0.75    0.80 0.65       1.00 0.40
## RE            0.1     0.4    0.40 0.35    0.30 0.50       0.40 1.00
```

We now calculate a covariance matrix from this information.


```r
Dsqrt=0*correlation
diag(Dsqrt)=volatility
covariance=Dsqrt%*%correlation%*%Dsqrt
round(covariance,2)
```

```
##            BbgAgg LrgUSEq SmlUSEq EAFE EAFESml   EM PrivCredit   RE
## BbgAgg          0    0.00    0.00 0.00    0.00 0.00       0.00 0.00
## LrgUSEq         0    0.03    0.03 0.03    0.03 0.02       0.01 0.01
## SmlUSEq         0    0.03    0.04 0.03    0.04 0.02       0.01 0.01
## EAFE            0    0.03    0.03 0.04    0.04 0.02       0.01 0.01
## EAFESml         0    0.03    0.04 0.04    0.08 0.02       0.02 0.01
## EM              0    0.02    0.02 0.02    0.02 0.02       0.01 0.01
## PrivCredit      0    0.01    0.01 0.01    0.02 0.01       0.01 0.01
## RE              0    0.01    0.01 0.01    0.01 0.01       0.01 0.02
```

The portfolio volatility is calculated as follows.


```r
portvol=sqrt(t(weights)%*%covariance%*%weights)
```

So, your portfolio volatility is 11.55%.  This compares to the weighted average asset volatility of 15.01%.

Now let's calculate the expected geometric return of the portfolio.  To do this, we calculate the weighted average arithmetic return of the portfolio which we then adjust for the portfolio volatility.  These calculations are performed using a 30 year horizon to be consistent with the return assumptions.   

First we create functions to do the conversion.  The formula to convert between arithmetic and geometric returns is from <http://www.treasury.govt.nz/publications/research-policy/wp/2003/03-28/twp03-28.pdf>


```r
# function to convert from geom return to arith return
# geo is the geometric return (may be a vector)
# vol is the volatility of the asset (may be a vecor)
# n is the number of years
geom_to_arith=function(geo,vol,n) {
  var=vol^2
  factor=(1+var/((1+geo)^2))^((1-n)/(2*n))
  arith=-1+(1+geo)/factor
  return(arith)
}
# function to convert from arith return to geom return
# arith is the arithmetic return (may be a vector)
# vol is the volatility of the asset (may be a vecor)
# n is the number of years
arith_to_geom=function(arith,vol,n) {
  var=vol^2
  factor=(1+var/((1+arith)^2))^((1-n)/(2*n))
  geo=-1+(1+arith)*factor
  return(geo)
}
```

Convert the geometric returns to arithmetic returns.


```r
n=30
arith_returns=geom_to_arith(geom_returns,volatility,n)
round(arith_returns,4)
```

```
##     BbgAgg    LrgUSEq    SmlUSEq       EAFE    EAFESml         EM 
##     0.0417     0.0887     0.0971     0.0971     0.1290     0.1024 
## PrivCredit         RE 
##     0.0839     0.0752
```

Next we calculate the portfolio arithmetic return (which is the weighted average of the arithmetic returns).


```r
port_arith_return=arith_returns%*%weights
port_arith_return
```

```
##            [,1]
## [1,] 0.08479212
```

Finally, we calculate the portfolio geometric return.


```r
port_geom_return=arith_to_geom(port_arith_return,portvol,n)
wavg_geom_return=geom_returns%*%weights
port_geom_return
```

```
##            [,1]
## [1,] 0.07890231
```

So, the expected geometric return of the portfolio is 7.89%.  This is 55bp greater than the weighted average geometric return of 7.34%.


