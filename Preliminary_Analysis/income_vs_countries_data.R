data <- read.csv("/Users/Philipp/Desktop/SurveyResultnew.csv")

countries=c(data$Q3)
income=c(data$Q5)
korea <-length(which(data$Q2=="Kor"))
intl <-length(which(data$Q2=="Int"))

#Income vs number of countries visited
plot(income,countries)

sd(income)=NA
sd(income,na.rm)=244.1804
sd(countries)=NA
sd(countries,na.rm=TRUE)=7.307213

Because standard deviation of income and countries does not exist (only works with na.rm=TRUE), 
the correlation coefficient does not exist as well.

rt <- cor.test(income,countries,na.rm=TRUE)
  t = -0.044924, df = 55, p-value = 0.9643
  alternative hypothesis: true correlation is not equal to 0
  95 percent confidence interval:
    -0.2662050  0.2549125
  sample estimates:
    cor 
  -0.006057503 

m <- lm(countries~income)
abline(m)
summary(m)
a <- m$coefficients[1]
b <- m$coefficients[2]

Min      1Q  Median      3Q     Max 
-459.34 -158.53  -58.93  241.68  345.72 
 
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 509.3365    45.7715  11.128 1.06e-15 ***
  countries    -0.2024     4.5058  -0.045    0.964    
---
  Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1

Residual standard error: 246.4 on 55 degrees of freedom
(1 observation deleted due to missingness)
Multiple R-squared:  3.669e-05,	Adjusted R-squared:  -0.01814 
F-statistic: 0.002018 on 1 and 55 DF,  p-value: 0.9643