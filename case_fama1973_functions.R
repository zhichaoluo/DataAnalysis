clear.reg<-function(df,n){
  #make sure each regression has at least n obs
  d<-df %>% group_by(PERMNO)%>% summarise(nr=n())
  c<-subset(d,nr>=n,select=c(PERMNO,nr))
  df<-merge(df,c,by="PERMNO")
}


reg.pp<-function(timeRange){
  df.t<-subset(df2,YEAR %in% timeRange)
  df.t<-clear.reg(df.t,30) 
  lm.1<-lmList(RET ~ EWRETD | PERMNO , data=df.t)
  beta<-coef(lm.1)
  beta<-na.omit(beta)
  beta$PERMNO<-rownames(beta)
  beta<-beta[order(beta$EWRETD),]
  #beta系数分组
  x<-seq(from=0,to=nrow(beta),by=ceiling(nrow(beta)/20))[2:20]#1486/20=74.3
  x<-c(x,nrow(beta))
  beta$group<-cut(1:nrow(beta), c(0,x),labels = paste0("g",1:20))
  beta$pp<-paste(timeRange[1],timeRange[length(timeRange)],sep="~")
  return(beta)
} 
monthlist<-function(timeRange){
  #create second loop series by each month
  st<-as.Date(paste0(timeRange[1],"-01-01"))
  et<-as.Date(paste0(timeRange[length(timeRange)],"-12-01"))
  loopmonth<-seq.Date(from=st,to=et,by="months")
  loopmonth<-format(loopmonth,"%Y%m")  
}
calcultY<-function(i,j){
  a<-pplist[[i]]
  p<-paste(a[1],a[length(a)],sep="~")
  ds.x<-subset(df2,YEAR==substr(ml.tp[j],1,4) & MONTH==as.numeric(substr(ml.tp[j],5,6)))
  ds.y<-subset(df.step1,pp==p,select=c(PERMNO,group))
  df.xy<-merge(ds.x,ds.y,by="PERMNO",all.x=TRUE)
  df.xy<-na.omit(df.xy)
  df.xy.m<- df.xy[,c("RET","group")] %>% group_by(group) %>% summarise(RET_m=mean(RET,na.rm=TRUE))
  return(df.xy.m)
}
calcultX<-function(i,j){
  
  ml.ip<-monthlist(iplist[[i]])
  st<-as.Date(paste0(ml.ip[j],"01"),"%Y%m%d")
  et<-st+months(60)
  ds.x<-subset(df2,DATE>=st & DATE<=et)
  
  a<-pplist[[i]]
  p<-paste(a[1],a[length(a)],sep="~")
  ds.y<-subset(df.step1,pp==p,select=c(PERMNO,group))
  
  df.xy<-merge(ds.x,ds.y,by="PERMNO",all.x=TRUE)
  df.xy<-na.omit(df.xy)
  df.xy<-clear.reg(df.xy,20)
  lm.2<-lmList(RET ~ EWRETD | PERMNO , data=df.xy)
  
  #calculate every group sd of residuals
  sd_resid<-lapply(1:length(lm.2),
                   function(x) sd(lm.2[[x]]$residuals) )
  sd_resid<-do.call(rbind,sd_resid)
  sd_resid<-data.frame(sd_resid,labels(lm.2))
  names(sd_resid)<-c("sd_resid","PERMNO")
  
  #calculate beta
  beta.2<-coef(lm.2)
  beta.2<-na.omit(beta.2)
  beta.2$PERMNO<-rownames(beta.2)
  #merge beta data with sd_residual data
  beta.2<-merge(beta.2,sd_resid,by="PERMNO",all.x=TRUE)
  
  #merge beta data with ds.y
  beta.2<-merge(beta.2,ds.y,by="PERMNO",all.x=TRUE)
  beta.2.m<- beta.2[,c("EWRETD","sd_resid","group")] %>% 
    group_by(group) %>% 
    summarise(EWRETD_m=mean(EWRETD,na.rm=TRUE),
              sd_resid_m=mean(sd_resid,na.rm=TRUE))
  return(beta.2.m)
  
}
reg.3<-function(i,j){
  #main regression
  y<-calcultY(i,j)
  x<-calcultX(i,j)
  if (nrow(y)==0 | nrow(x) == 0 ) {return(print(paste0(ml.tp[j],"test without data!"))) } 
  else {
    z<-merge(x,y,by="group")
    z$tp<-ml.tp[j]
    z<-na.omit(z)
    lm.3<-lm(RET_m~EWRETD_m+sd_resid_m,data=z)
    tpid=ml.tp[j]
    res.lm3<-c(coef(lm.3),tpid)
    #return result is factor
    df.final[[tpid]]<<-res.lm3
    print(res.lm3)
  }
}
