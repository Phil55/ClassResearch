studentdata <- read.csv("/Users/VAIO/Desktop/GitHub/ClassResearch/SurveyResult.csv")


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
length(male)						#We have 41 male students
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
length(female)						#We have 16 female students
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
length(int)						#We have 11 intl students
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
length(kor)						#We have 46 Korean students
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

#----------------------------------------------------

income=c(studentdata$Q5)

#Barplot of the income of all students
lnbar <- barplot(table(income) ,xlim= c(0,13), ylim= c(0,14), legend = c("0-100", "200-300", "300-400", "400-500", "500-600", "600-700", "700-800", "800+"), 
                 main="Income of Student", xlab= "Income", ylab= "Number of students",
                 col = c("aquamarine4", "red", "cadetblue2", "chartreuse3", "darkblue", "darkgoldenrod2", "darkgreen", "darkorchid2"),
                 args.legend = list(x = "topright", bty = "n", inset=0))

#Adding Line and points to the barplot
lines(x = lnbar, y = table(income)/10, lwd = 2)
points(x = lnbar, y = table(income)/10, lwd = 3)

#----------------------------------------------------
