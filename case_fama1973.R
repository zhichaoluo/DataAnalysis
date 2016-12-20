#load library
library(haven)
library(lubridate)
library(lme4)
library(dplyr)


#raw data download link: http://pan.baidu.com/s/1pLGMsC7
#data preparation
df.raw<-read_sas("data/stockreturn.sas7bdat")
df<-df.raw
df$DATE<-as.Date(df$DATE, origin="1960-01-01")
df$YEAR<-year(df$DATE)
df$MONTH<-month(df$DATE)#duplicate month problem
df2<-subset(df,YEAR>1926 & YEAR<1968,select = c(PERMNO,DATE,YEAR,MONTH,RET,EWRETD))
df2<-na.omit(df2)
df2$stockcode<-as.factor(df2$PERMNO)

pplist<-list(c(1926:1929),c(1927:1933),c(1931:1937),
             c(1936:1941),c(1939:1945),c(1943:1949),
             c(1947:1953),c(1951:1957),c(1955:1961))
iplist<-list(c(1930:1934),c(1934:1938),c(1938:1942),
             c(1942:1946),c(1946:1950),c(1950:1954),
             c(1954:1958),c(1958:1962),c(1962:1966))
tplist<-list(c(1935:1938),c(1939:1942),c(1943:1946),
             c(1947:1950),c(1951:1954),c(1955:1958),
             c(1959:1962),c(1963:1966),c(1967:1968))
#load self define functions
source("case_fama1973_functions.R")

#Step1
#regression by each period and stock, 
#then sort the beta of EWRETD
#cut the beta dataset into 20 group
#collect all the groups in different period into df.step1


df.step1<-list()
for (i in 1:length(pplist)){
  df.step1[[i]]<-reg.pp(pplist[[i]])
}
df.step1<- do.call(rbind,df.step1)
row.names(df.step1)<-NULL

#step2

#regression each period and month
#get main regression result

df.final<-list()
for (i in 1:length(tplist)){
  ml.tp<-monthlist(tplist[[i]])
    for (j in 1:length(ml.tp)){
    reg.3(i,j)
      }
}
res<-as.data.frame(do.call(rbind,df.final),stringsAsFactors = F)

#Bug list

