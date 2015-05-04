#数据描述
#The data set consists of daily records of several meteorological parameters, measured in the city ofPorto over the year of 2014. We have, then, 365 observations for each of the following 14 variables:
#day.count – number of days passed since the beginning of the year
#day – day of the month
#month – month of the year
#season – season of the year
#l.temp, h.temp, ave.temp – lowest, highest and average temperature for the day (in ºC)
#l.temp.time, h.temp.time – hour of the day when l.temp and h.temp occurred
#rain – amount of precipitation (in mm)
#ave.wind – average wind speed for the day (in km/h)
#gust.wind – maximum wind speed for the day (in km/h)
#gust.wind.time – hour of the day when gust.wind occurred
#dir.wind – dominant wind direction for the day

#数据准备及清洗

weather <- read.csv("data/weather_2014.csv",sep=";",stringsAsFactors=FALSE)
dim(weather)
names(weather)
head(weather)
str(weather)
#缺失值检查
sum(is.na(weather))
nrow(weather)
sum(complete.cases(weather))
nrow(weather) == sum(complete.cases(weather))

#创建因子
class(weather$season)
summary(weather$season)
weather$season <- factor(weather$season,levels = c("Spring","Summer","Autumn","Winter"))
summary(weather$season)
weather$day <- as.factor(weather$day)
weather$month <- as.factor(weather$month)
weather$dir.wind <- as.factor(weather$dir.wind)

#风向重编码
length(unique(weather$dir.wind))
table(weather$dir.wind)
rel <- round(prop.table(table(weather$dir.wind))*100,1)
sort(rel,decreasing = TRUE)
weather$dir.wind.8 <- weather$dir.wind 
weather$dir.wind.8 <- ifelse(weather$dir.wind %in%  c("NNE","ENE"),"NE",as.character(weather$dir.wind.8)) 
weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("NNW","WNW"), "NW",as.character(weather$dir.wind.8)) 
weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("WSW","SSW"), "SW",as.character(weather$dir.wind.8)) 
weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("ESE","SSE"), "SE",as.character(weather$dir.wind.8)) 
weather$dir.wind.8 <- factor(weather$dir.wind.8, levels = c("N","NE","E","SE","S","SW","W","NW"))
length(unique(weather$dir.wind.8))
round(prop.table(table(weather$dir.wind.8,weather$season),margin = 2)*100,1)

#创建时间序列


first.day <- "2014-01-01"
class(first.day)
first.day <- as.Date(first.day)
class(first.day)
weather$date  <- first.day + weather$day.count - 1 
head(weather$day.count)
#创建日期时间
l.temp.time.date <- as.POSIXlt(paste(weather$date,weather$l.temp.time))
head(l.temp.time.date)
l.temp.time.date <- round(l.temp.time.date,"hours")
head(l.temp.time.date)
weather$l.temp.hour <- l.temp.time.date [["hour"]]
weather$l.temp.hour <- as.factor(weather$l.temp.hour)
head(weather$l.temp.hour)

h.temp.time.date <- as.POSIXlt(paste(weather$date,weather$h.temp.time))
head(h.temp.time.date)
h.temp.time.date <- round(h.temp.time.date,"hours")
head(h.temp.time.date)
weather$h.temp.hour <- h.temp.time.date [["hour"]]
weather$h.temp.hour <- as.factor(weather$h.temp.hour)
head(weather$h.temp.hour)

gust.wind.time.date <- as.POSIXlt(paste(weather$date,weather$gust.wind.time))
head(gust.wind.time.date)
gust.wind.time.date <- round(gust.wind.time.date,"hours")
head(gust.wind.time.date)
weather$gust.wind.hour <- gust.wind.time.date [["hour"]]
weather$gust.wind.hour <- as.factor(weather$gust.wind.hour)
head(weather$gust.wind.hour)


#准备好的新数据集
str(weather)


#探索性数据分析

#日平均气温时序图
library(ggplot2)
ggplot(weather,aes(x = date,y = ave.temp)) +
  geom_point(colour = "blue") +
  geom_smooth(colour = "red",size = 1) +
  scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Daily average temperature") +
  xlab("Date") +  ylab ("Average Temperature ( ºC )")
#增加温度颜色渐变效果
ggplot(weather,aes(x = date,y = ave.temp)) + 
  geom_point(aes(colour = ave.temp)) +
  scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = 16) + 
  geom_smooth(color = "red",size = 1) +
  scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Daily average temperature") +
  xlab("Date") +  ylab ("Average Temperature ( ºC )")


#按季节绘制气温概率密度图
ggplot(weather,aes(x = ave.temp, colour = season)) +
  geom_density() +
  scale_x_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Temperature distribution by season") +
  xlab("Average temperature ( ºC )") +  ylab ("Probability")

#月度气温时序
weather$month = factor(weather$month,
                       labels = c("Jan","Fev","Mar","Apr",
                                  "May","Jun","Jul","Aug","Sep",
                                  "Oct","Nov","Dec"))
ggplot(weather,aes(x = month, y = ave.temp)) +
  geom_violin(fill = "orange") +
  geom_point(aes(size = rain), colour = "blue", position = "jitter") +
  ggtitle ("Temperature distribution by month") +
  xlab("Month") +  ylab ("Average temperature ( ºC )")

#高温低温相关性
ggplot(weather,aes(x = l.temp, y = h.temp)) +
  geom_point(colour = "firebrick", alpha = 0.3) + 
  geom_smooth(aes(colour = season),se= F, size = 1.1) +
  ggtitle ("Daily low and high temperatures") +
  xlab("Daily low temperature ( ºC )") +  ylab ("Daily high temperature ( ºC )") 

#每天最高最低气温分布图
library(reshape2) 
temperatures <- weather[c("day.count","h.temp.hour","l.temp.hour")] 
head(temperatures)

dim(temperatures)
temperatures <- melt(temperatures,id.vars = "day.count",
                     variable.name = "l.h.temp", value.name = "hour")
head(temperatures)
tail(temperatures)
dim(temperatures)
temperatures$hour <- factor(temperatures$hour,levels=0:23)
ggplot(temperatures) +
  geom_bar(aes(x = hour, fill = l.h.temp)) +
  scale_fill_discrete(name= "", labels = c("Daily high","Daily low")) +
  scale_y_continuous(limits = c(0,100)) +
  ggtitle ("Low and high temperatures - time of the day") +
  xlab("Hour") +  ylab ("Frequency")

#分析降雨量的影响因素

#降雨量变量分析
#降雨量时序图
ggplot(weather, aes(date,rain)) +
  geom_point(aes(colour = rain)) +
  geom_smooth(colour = "blue", size = 1) +
  scale_colour_gradient2(low = "green", mid = "orange",high = "red", midpoint = 20) +
  scale_y_continuous(breaks = seq(0,80,20)) +
  xlab("Date") +
  ylab("Rain (mm)") +
  ggtitle("Daily rain amount")

#降雨量频数图
ggplot(weather,aes(rain)) + 
  geom_histogram(binwidth = 1,colour = "blue", fill = "darkgrey") +
  scale_x_continuous(breaks = seq(0,80,5)) +
  scale_y_continuous(breaks = seq(0,225,25)) +
  xlab("Rain (mm)") +
  ylab ("Frequency (days)") +
  ggtitle("Daily rain amount distribution")

#描述统计分析
summary(weather$rain)
summary(subset(weather, rain > 0)$rain)
library(e1071) 
skewness(weather$rain)
skewness(subset(weather, rain >0)$rain)
nrow(subset(weather, rain == 0))
nrow(subset(weather, rain <1 & rain >0))
weather$rained <- ifelse(weather$rain >= 1, "Yes", "No")
table(rained = weather$rained) 
prop.table(table(rained = weather$rained)) 

#添加季节分类变量

ggplot(weather, aes(season,rain)) +
  geom_jitter(aes(colour=rain), position = position_jitter(width = 0.2)) +
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = 30) +
  scale_y_continuous(breaks = seq(0,80,20)) +
  xlab("Season") +
  ylab ("Rain (mm)") +
  ggtitle("Daily rain amount by season")

tapply(weather$rain,weather$season,summary) 

#季节降雨天数比例
ggplot(weather,aes(season)) +
  geom_bar(aes(fill = rained), position = "fill") +
  geom_hline(aes(yintercept = prop.table(table(weather$rained))["No"]),
             colour = "blue",linetype = "dashed", size = 1) +
  annotate("text", x = 1, y = 0.65, label = "yr. w/o = 0.60", colour = "blue") +
  xlab("Season") +
  ylab ("Proportion") +
  ggtitle("Proportion of days without and with rain, by season")
round(prop.table(table(season = weather$season, rained= weather$rained),1),2) 

#降雨和其他数值变量的关系

weather.num <- weather[c("rain","l.temp","h.temp","ave.temp","ave.wind","gust.wind",
                         "l.temp.hour","h.temp.hour","gust.wind.hour")] 

#将因子变量转换为数值变量
weather.num$l.temp.hour <- as.numeric(weather.num$l.temp.hour)
weather.num$h.temp.hour <- as.numeric(weather.num$h.temp.hour)
weather.num$gust.wind.hour <- as.numeric(weather.num$gust.wind.hour) 
round(cor(weather.num),2)[1,]


weather.num.season <- split(weather.num,weather$season) 
class(weather.num.season)
length(weather.num.season)
summary(weather.num.season)
attributes(weather.num.season)
sapply(weather.num.season, function (x) round(cor(x)["rain",],2))

#风量和雨量的相关分析，增加季节变量
ggplot(weather,aes(gust.wind,rain)) +
  geom_point(colour = "firebrick") +
  geom_smooth(size = 0.75, se = F) +
  facet_wrap(~season) +
  xlab("Maximum wind speed (km/h)") +
  ylab ("Rain (mm)") +
  ggtitle("Amount of rain vs. maximum wind speed, by season")

#增加更多的控制变量
quantile(weather$h.temp)
weather$h.temp.quant <- cut(weather$h.temp, breaks = quantile(weather$h.temp),
                            labels = c("Cool","Mild","Warm","Hot"),include.lowest = T)
table(weather$h.temp.quant)

ggplot(weather,aes(rained,gust.wind)) +
  geom_boxplot(aes(colour=rained)) +
  facet_grid(h.temp.quant~season) +
  xlab("Occurrence of rain") +
  ylab ("Maximum wind speed (km/h)") +
  ggtitle("Occurrence of rain, by season and daily high temperature")
