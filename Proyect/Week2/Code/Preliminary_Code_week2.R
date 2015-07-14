
#Dataset needs to be adjusted to the root of each computer
data <- read.csv("/ClassResearch/Preliminary_Analisis/SurveyResultnew.csv")


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


#get means of every percentage of spending

mean_l <- mean(studentdata$Living)
mean_t <- mean(studentdata$Trans)
mean_h <- mean(studentdata$Hobby)
mean_f <- mean(studentdata$Food)
mean_c <- mean(studentdata$Clothes)
mean_p <- mean(studentdata$Phonebill)
mean_i <- mean(studentdata$Insurance)
mean_o <- mean(studentdata$Others)

#take a pie graph from those means

mean_total <- c(mean_l, mean_t, mean_h, mean_f, mean_c, mean_p, mean_i, mean_o)
pie(mean_total, c("Living", "Transportation", "Hobby", "Food", "Clothes",
"Phonebill", "Insurance", "Others"),main="Spending Categories of All Students",
col=c("red","yellow","green","blue","purple","brown","orange","navy"))

#(1) Pie Graph of spending categories for all males

#indexing male data from studentdata

male <- which(studentdata$Q1=="Male")
length(male)            #We have 41 male students
data_male <- studentdata[male,]

#take means of every percentage of spending from male data

m_mean_l <- mean(data_male$Living)
m_mean_t <- mean(data_male$Trans)
m_mean_h <- mean(data_male$Hobby)
m_mean_f <- mean(data_male$Food)
m_mean_c <- mean(data_male$Clothes)
m_mean_p <- mean(data_male$Phonebill)
m_mean_i <- mean(data_male$Insurance)
m_mean_o <- mean(data_male$Others)

#get a pie graph from those means

m_mean_total <- c(m_mean_l, m_mean_t, m_mean_h, m_mean_f, m_mean_c, m_mean_p, m_mean_i, m_mean_o)
pie(m_mean_total, c("Living", "Transportation", "Hobby", "Food", "Clothes",
"Phonebill", "Insurance", "Others"),main="Spending Categories of Male Students",
col=c("red","yellow","green","blue","purple","brown","orange","navy"))

#(2) Pie Graph of spending categories for all females

#indexing female data from studentdata

female <- which(studentdata$Q1=="Female")
length(female)            #We have 16 female students
data_female <- studentdata[female,]

#take means of every percentage of spending from female data

f_mean_l <- mean(data_female$Living)
f_mean_t <- mean(data_female$Trans)
f_mean_h <- mean(data_female$Hobby)
f_mean_f <- mean(data_female$Food)
f_mean_c <- mean(data_female$Clothes)
f_mean_p <- mean(data_female$Phonebill)
f_mean_i <- mean(data_female$Insurance)
f_mean_o <- mean(data_female$Others)

#get a pie graph from those means

f_mean_total <- c(f_mean_l, f_mean_t, f_mean_h, f_mean_f, f_mean_c, f_mean_p, f_mean_i, f_mean_o)
pie(f_mean_total, c("Living", "Transportation", "Hobby", "Food", "Clothes",
"Phonebill", "Insurance", "Others"),main="Spending Categories of Female Students",
col=c("red","yellow","green","blue","purple","brown","orange","navy"))

#(3) Pie Graph of spending categories for all international students

#indexing intl students data from studentdata

int <- which(studentdata$Q2=="Int")
length(int)           #We have 11 intl students
data_int <- studentdata[int,]

#take means of every percentage of spending from data of int students

i_mean_l <- mean(data_int$Living)
i_mean_t <- mean(data_int$Trans)
i_mean_h <- mean(data_int$Hobby)
i_mean_f <- mean(data_int$Food)
i_mean_c <- mean(data_int$Clothes)
i_mean_p <- mean(data_int$Phonebill)
i_mean_i <- mean(data_int$Insurance)
i_mean_o <- mean(data_int$Others)

#get a pie graph from those means

i_mean_total <- c(i_mean_l, i_mean_t, i_mean_h, i_mean_f, i_mean_c, i_mean_p, i_mean_i, i_mean_o)
pie(i_mean_total, c("Living", "Transportation", "Hobby", "Food", "Clothes",
"Phonebill", "Insurance", "Others"),main="Spending Categories of International Students",
col=c("red","yellow","green","blue","purple","brown","orange","navy"))

#(4) Pie Graph of spending categories for all Korean students

#indexing Korean students data from studentdata

kor <- which(studentdata$Q2=="Kor")
length(kor)           #We have 46 Korean students
data_kor <- studentdata[kor,]

#take means of every percentage of spending from data of Korean students

k_mean_l <- mean(data_kor$Living)
k_mean_t <- mean(data_kor$Trans)
k_mean_h <- mean(data_kor$Hobby)
k_mean_f <- mean(data_kor$Food)
k_mean_c <- mean(data_kor$Clothes)
k_mean_p <- mean(data_kor$Phonebill)
k_mean_i <- mean(data_kor$Insurance)
k_mean_o <- mean(data_kor$Others)

#get a pie graph from those means

k_mean_total <- c(k_mean_l, k_mean_t, k_mean_h, k_mean_f, k_mean_c, k_mean_p, k_mean_i, k_mean_o)
pie(k_mean_total, c("Living", "Transportation", "Hobby", "Food", "Clothes",
"Phonebill", "Insurance", "Others"),main="Spending Categories of Korean Students",
col=c("red","yellow","green","blue","purple","brown","orange","navy"))


#(5) Hypothesis Test of percentage of spending for clothes between males and females

# H0: The percentages are the same in males and females 
# H1: The percentages are not the same

male_c <- data_male$Clothes
female_c <- data_female$Clothes

hist(male_c)
hist(female_c)

mean(male_c)
mean(female_c)
sd(male_c)
sd(female_c)

t.test(male_c,female_c)

# p-value for this test is 0.1189, so we can't reject null hypothesis.
# Therefore, the percentage of spending for clothes between males and females are the same.

data <- read.csv("/Users/Administrator/Documents/GitHub/ClassResearch/SurveyResult.csv")

income_1 <- which(data$Q5=="0-100")
income_2 <- which(data$Q5=="100-200")
income_3 <- which(data$Q5=="200-300")
income_4 <- which(data$Q5=="300-400")
income_5 <- which(data$Q5=="400-500")
income_6 <- which(data$Q5=="500-600")
income_7 <- which(data$Q5=="600-700")
income_8 <- which(data$Q5=="700-800")
income_9 <- which(data$Q5=="800-900")
income_10 <- which(data$Q5=="800-")

income <- c(income_1,income_2,income_3,income_4,income_5,income_6,income_7,income_8,income_9,income_10)

ma <- which(data$Q4=="0")
mb <- which(data$Q4=="1")
mc <- which(data$Q4=="2")
md <- which(data$Q4=="3")
me <- which(data$Q4=="4")
mf <- which(data$Q4=="5")
mg <- which(data$Q4=="6")
mh <- which(data$Q4=="7")

movie <- c(ma,mb,mc,md,me,mf,mg,mh)

hist(income)
hist(movie)

length(income)=length(movie)=57
plot(income,movie)
cor(income, movie)=0.07188229
rt <- cor.test(income, movie)

#There is no correlation between income and movies
#As the plot graph shows, there's no particular correlation between income and frequency of watching movie.


