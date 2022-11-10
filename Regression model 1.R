
> library(readxl)
> library(readxl)
> coal_production_by_country <- read_excel("C:/Users/Tanmay Shah/Desktop/Project/coal-production-by-country.xlsx", 
                                           +     col_types = c("skip", "skip", "numeric", 
                                                               +         "numeric"))
> View(coal_production_by_country)
> library(readxl)
> co2_emission <- read_excel("C:/Users/Tanmay Shah/Desktop/Project/co2 emission.xlsx", 
                             +     col_types = c("skip", "skip", "numeric", 
                                                 +         "numeric"))
> View(co2_emission)

> y=coal_production_by_country$`Coal production (TWh)`
> y
[1]  841.709  905.426  992.484 1055.136 1147.427 1236.167 1321.824 1395.098
[9] 1441.369 1466.095 1547.903 1658.954 1713.905 1703.446 1666.204 1771.192
[17] 1793.757 1873.272 1954.734 2110.972 2207.160 2303.685 2446.755 2647.513
[25] 2857.771 2937.109 2915.217 2966.210 2974.117 3133.724 3268.103 3301.831
[33] 3329.753 3555.345 3499.012 3522.966
> x=co2_emission$`Annual CO2 emissions`
> x
[1]  397952960  426698557  455758841  492144025  541134880  578518195  615924923
[8]  656033132  677916275  716917563  762120642  825918856  859686238  877698067
[15]  951748674  978919215  992560136 1023027203 1059616164 1125471265 1185953364
[22] 1259744276 1358152218 1462814598 1612816599 1677887585 1780129996 1963586045
[29] 2036937081 2185855918 2268567478 2382223220 2433855754 2599805717 2625968148
[36] 2441792313

> a=lm(y~x)
> a

Call:
  lm(formula = y ~ x)

Coefficients:
  (Intercept)            x  
6.218e+02    1.190e-06  

> summary(a)

Call:
  lm(formula = y ~ x)

Residuals:
  Min      1Q  Median      3Q     Max 
-253.56  -96.19   -7.97   69.99  318.93 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 6.218e+02  5.430e+01   11.45 3.26e-13 ***
  x           1.190e-06  3.714e-08   32.04  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 155.1 on 34 degrees of freedom
Multiple R-squared:  0.9679,	Adjusted R-squared:  0.967 
F-statistic:  1026 on 1 and 34 DF,  p-value: < 2.2e-16
> par(mfrow=c(2,2))
> plot(a)
> shapiro.test(x)

Shapiro-Wilk normality test

data:  x
W = 0.90208, p-value = 0.003893

> View(coal_production_new)
> View(co2_emission_new)
> p=coal_production_new$`Coal production (TWh)`
> q=co2_emission_new$`Annual CO2 emissions`
> b=lm(p~q+I(q^2))
> summary(b)

Call:
  lm(formula = p ~ q + I(q^2))

Residuals:
  Min       1Q   Median       3Q      Max 
-144.018  -50.474    0.294   49.202  131.751 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.378e+02  5.512e+01   2.500   0.0181 *  
  q            2.029e-06  9.132e-08  22.213  < 2e-16 ***
  I(q^2)      -2.851e-16  2.995e-17  -9.519 1.42e-10 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 64.6 on 30 degrees of freedom
Multiple R-squared:  0.9948,	Adjusted R-squared:  0.9944 
F-statistic:  2864 on 2 and 30 DF,  p-value: < 2.2e-16

> plot(b)
>par(mfrow=c(1,1))
> plot(q,p)
> lines(smooth.spline(q,predict(b)),col="red",lwd=3)