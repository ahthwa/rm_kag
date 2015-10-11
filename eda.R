library("data.table")
library("ggplot2")

#https://www.kaggle.com/thie1e/rossmann-store-sales/exploratory-analysis-rossmann/notebook 세세한 팁. 이거 보고 배울 것.

train_data <- fread("data/train.csv", sep=",", header = T)
test_data <- fread("data/test.csv", sep=",", header = T)
store_data <- fread("data/store.csv", sep=",", header = T)
train_data = cbind(train_data, poxis_dt  = strptime(train_data$Date, "%Y-%m-%d"))

summary(train_data)
summary(test_data)
summary(store_data)

head(train_data)
head(store_data)

# Sales 분포
ggplot(data = train_data) + geom_histogram(aes(x = Sales), binwidth=500)
ggplot(data = train_data[Sales < 500,]) + geom_histogram(aes(x = Sales), binwidth=10) # Sale 0인 상점-날짜가 꽤 있음.

# DayOfWeek 분포
# Day of week == 7 일때 Sales median ~= 0 임.
ggplot(data = train_data) + geom_boxplot(aes(x = as.factor(DayOfWeek), y = Sales))

# Date min/max
min(train_data$Date) # 2013-01-01
max(train_data$Date) # 2013-07-31

# Customer 분포
ggplot(data = train_data) + geom_histogram(aes(x = Customers), binwidth=100)

# Customer ~ Sales
# 우상향 plot. 종모양으로 흩뿌려짐.
ggplot(data = train_data, aes(x = Customers, y = Sales, colours(SchoolHoliday))) + geom_point()

# Open, Promo, StateHoliday, SchoolHoliday
open_summary = train_data[, .(sales = sum(as.numeric(Sales)), mean_sales = mean(as.numeric(Sales)), customers = sum(as.numeric(Customers)), mean_customers = mean(as.numeric(Customers))), by = (open = factor(Open))]
ggplot(open_summary, aes(x=factor(1), y=sales, fill = factor(open))) + 
  geom_bar(width = 1, stat="identity")

promo_summary = train_data[, .(sales = sum(as.numeric(Sales)), mean_sales = mean(as.numeric(Sales)), customers = sum(as.numeric(Customers)), mean_customers = mean(as.numeric(Customers))), by = (promo = factor(Promo))]
ggplot(promo_summary, aes(x=factor(1), y=sales, fill = promo)) + 
  geom_bar(width = 1, stat="identity") + coord_polar(theta="y")

# Holiday sales의 절대양은 적지만, mean_sales는 상대적으로 높다. 그렇다고 평일보다 평균이 높다는 것은 아니다.
stateholiday_summary = train_data[, .(sales = sum(as.numeric(Sales)), mean_sales = mean(as.numeric(Sales)), customers = sum(as.numeric(Customers)), mean_customers = mean(as.numeric(Customers))), by = (stateholiday = factor(StateHoliday))]
ggplot(stateholiday_summary, aes(x=factor(1), y=sales, fill = stateholiday)) + 
  geom_bar(width = 1, stat="identity") + coord_polar(theta="y")
ggplot(stateholiday_summary, aes(x=factor(1), y=mean_sales, fill = stateholiday)) + 
  geom_bar(width = 1, stat="identity") + coord_polar(theta="y")
stateholiday_summary

# School Holiday는 mean_sales가 높다.
schoolholiday_summary = train_data[, .(sales = sum(as.numeric(Sales)), mean_sales = mean(as.numeric(Sales)), customers = sum(as.numeric(Customers)), mean_customers = mean(as.numeric(Customers))), by = (schoolholiday = factor(SchoolHoliday))]
ggplot(schoolholiday_summary, aes(x=factor(1), y=sales, fill = schoolholiday)) + 
  geom_bar(width = 1, stat="identity") + coord_polar(theta="y")
ggplot(schoolholiday_summary, aes(x=factor(1), y=mean_sales, fill = schoolholiday)) + 
  geom_bar(width = 1, stat="identity") + coord_polar(theta="y")
schoolholiday_summary

# Store 정보와 결합.
# Competition year/month, Promo2 year/week, PromoInterval을 join data의 각 row에 적용하도록 코딩할 것.


