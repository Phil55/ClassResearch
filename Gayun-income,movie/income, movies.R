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

