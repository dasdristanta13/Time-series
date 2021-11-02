

## Code And Workings

### Imports

<details>
  <summary>Click to expand!</summary>
  
  ```r
library(prophet)
library(quantmod)
library(forecast)
library(tseries)
library(timeSeries)
library(dplyr)
library(fGarch)
library(gridExtra)
library(readxl)
library(astsa)
library(urca)
library(rugarch)
library(quantmod)
library(dplyr)
library(tidyverse)
library(tseries)
library(rugarch)
library(xts)
library(PerformanceAnalytics)
library(lubridate) 

  ```
  </details>


### Data Loading, Cleaning and Data Preparation
<details>
  <summary>Click to expand!</summary>
  
  ```r
nifty <- read_excel("nifty.xlsx")
niftyw<-nifty[857:1234,]

  ```
  </details>
  
  
## Nifty 50

  <details>
  <summary>Click to expand!</summary>
  
  ```r
niftyxts<-xts(x=as.numeric(niftyw$Close),order.by = ymd(niftyw$Date))

chartSeries(niftyxts)

  ```
  </details>
  
  
 <img src="Plots/Nifty_act.jpeg" alt="drawing" width="500"/>

  
  <details>
  <summary>Click to expand!</summary>
  
  ```r
acf2(niftyxts)

  ```
  </details>
<img src="Plots/Nifty_acf.jpeg" alt="drawing" width="500"/> 
  
  

  <details>
  <summary>Click to expand!</summary>
  
  ```r
  summary(ur.df(niftyxts, type="none",lag=1))

  ```
  </details>
  
  
  ```r
  ############################################### 
  # Augmented Dickey-Fuller Test Unit Root Test # 
  ############################################### 

  Test regression none 


  Call:
  lm(formula = z.diff ~ z.lag.1 - 1 + z.diff.lag)

  Residuals:
      Min      1Q  Median      3Q     Max 
  -596.75  -66.97    6.01   92.08  696.39 

  Coefficients:
              Estimate Std. Error t value Pr(>|t|)   
  z.lag.1    0.0017829  0.0005885   3.030  0.00262 **
  z.diff.lag 0.0141279  0.0517674   0.273  0.78507   
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Residual standard error: 150.8 on 374 degrees of freedom
  Multiple R-squared:  0.02565,	Adjusted R-squared:  0.02044 
  F-statistic: 4.922 on 2 and 374 DF,  p-value: 0.00776


  Value of test-statistic is: 3.0297 

  Critical values for test statistics: 
        1pct  5pct 10pct
  tau1 -2.58 -1.95 -1.62
  ```
  
  
  <details>
  <summary>Click to expand!</summary>
  
  ```r
  niftyreturn = CalculateReturns(log(niftyxts))
  niftyreturn=niftyreturn[-c(1),]

  chartSeries(niftyreturn)
  chartSeries(niftyreturn**2)

  ```
  </details>
  
 <img src="Plots/Nifty_logret.jpeg" alt="drawing" width="400"/> <img src="Plots/Nifty_sq_logret.jpeg" alt="drawing" width="400"/>
  

  
  <details>
  <summary>Click to expand!</summary>
  
  ```r
  summary(ur.df(niftyreturn, type="none",lag=1))
  ```
  </details>
  
  ```r
  ############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression none 


Call:
lm(formula = z.diff ~ z.lag.1 - 1 + z.diff.lag)

Residuals:
       Min         1Q     Median         3Q        Max 
-0.0063091 -0.0003262  0.0002651  0.0009271  0.0091561 

Coefficients:
           Estimate Std. Error t value Pr(>|t|)    
z.lag.1    -1.04889    0.07036 -14.909   <2e-16 ***
z.diff.lag  0.03057    0.04980   0.614     0.54    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.001401 on 373 degrees of freedom
Multiple R-squared:  0.5265,	Adjusted R-squared:  0.5239 
F-statistic: 207.4 on 2 and 373 DF,  p-value: < 2.2e-16


Value of test-statistic is: -14.9086 

Critical values for test statistics: 
      1pct  5pct 10pct
tau1 -2.58 -1.95 -1.62
  ```
  

  <details>
  <summary>Click to expand!</summary>
  
  ```r
  chart.Histogram(niftyreturn,methods = c("add.density","add.normal"),colorset = c("grey","red","blue"))
  legend("topright",legend=c("return","kernel","normal dist."),fill=c("grey","red","blue"))
  ```
  </details>
  
  
<img src="Plots/Nifty_ret_hist.jpeg" alt="drawing" width="500"/>
  
  
  
  
  
  
  
  
  
  
  
  
 
  
  <details>
  <summary>Click to expand!</summary>
  
  ```r


  ```
  </details>
