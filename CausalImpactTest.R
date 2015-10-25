library(devtools)
devtools::install_github("google/CausalImpact")
library(CausalImpact)

## data load
data <- read.csv("gdp_from_duke_edu.txt", header = T, sep = "\t")
capita <- data[,c(6, 3)]
colnames(capita)<- c("x","y")
capita[is.na(capita)]<-0 ## null handling

matplot(capita, type = "l")
pre.period <- c(1, 25)
post.period <- c(26, 34)
impact <- CausalImpact(capita, pre.period, post.period)
plot(impact)
summary(impact)

?CausalImpact
View(data)
View(capita)

set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)

View(data)
matplot(data, type = "l")
pre.period <- c(1, 70)
post.period <- c(71, 100)
