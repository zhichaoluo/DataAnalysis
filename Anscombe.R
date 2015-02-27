#anscombe dataset
library(dplyr)
library(ggplot2)
library(reshape2)

anscombe
# 改变数据结构
#方法一
d1<-NULL
d1$x<-anscombe[,2]
d1$y<-anscombe[,6]
d1$type<-"b"
d1<-as.data.frame(d1)
#....
d<-rbind(d1,d2,d3,d4)

#方法二
d1<-data_frame(
  x<-anscombe[,1],
  y<-anscombe[,5],
  type<-"a"
  )
d2<-data_frame(
  x<-anscombe[,2],
  y<-anscombe[,6],
  type<-"b"
)
d3<-data_frame(
  x<-anscombe[,3],
  y<-anscombe[,7],
  type<-"c"
)
d4<-data_frame(
  x<-anscombe[,4],
  y<-anscombe[,8],
  type<-"d"
)
d<-rbind(d1,d2,d3,d4)

#方法三

t<-c("a","b","c","d")

for (i in 1:4){
d<-paste("d",i,sep="")  
  data=data_frame(
    x=anscombe[,i],
    y=anscombe[,i+4],
    type=t[i]
    )
  assign(d,data)
}
d<-rbind(d1,d2,d3,d4)

#思考下下面赋值方法为什么会出错
t<-c("a","b","c","d")
ds<-paste("d",1:4,sep="")
for (i in 1:4){
  ds[i]<-data_frame(
    x<-anscombe[,i],
    y<-anscombe[,i+4],
    type<-t[i]
  )
}

#方法四
library(reshape2)

#绘图

d%>%
  group_by(type) %>%
  summarize(mean(x), sd(x), mean(y), sd(y), cor(x,y))

p <- ggplot(d, aes(x, y)) + geom_point()
p <- p + geom_smooth(method = lm, se = FALSE)
p <- p + facet_wrap(~type)
p

#参考R语言自带的anscombe绘图
?anscombe
