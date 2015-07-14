data <- read.csv("/Users/Administrator/Documents/GitHub/ClassResearch/SurveyResult.csv")


income =c(data$Q5)
movie =c(data$Q4)

hist(income)
hist(movie)

plot(income,movie)

rt <- cor.test(income, movie)
#cor=0.1762094
#There is no correlation between income and movies
#As the plot graph shows, there's no particular correlation between income and frequency of watching movie.

