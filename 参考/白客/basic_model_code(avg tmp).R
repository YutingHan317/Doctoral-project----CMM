setwd("D:/graduate/data")
rm(list = ls())
library(lme4)
library(splines)
library(magrittr)
load("hyp dta with labs v1.RData")
load("hyp dta v3 (for analysis).RData")
dta$id1 <- 1:nrow(dta)
dta$city2=as.character(as.numeric(as.factor(dta$city)))
vs <- c("s1","s2","s3","d1","d2","d3")
for (v in vs) {dta[which(dta[,v]>180),v]=NA}
dta$med <- apply(dta[,c("med_c","med_w","med_o")],1,function(x)any(!is.na(x)))
dta$agec <- cut(dta$ages,breaks = c(-Inf,seq(40,80,10),Inf),right = F)
dta$sp <- apply(dta[,c("s1","s2","s3")],1,mean,na.rm=T)
dta$dp <- apply(dta[,c("d1","d2","d3")],1,mean,na.rm=T)
dta$TMP=apply(TMPlag[,1:12],1,mean)
fs=c("y~PM25+med+
     city2+(1|HID)+(1|ID2)",
     "y~PM25+med+ns(TMP,3)+agec+
     city2+(1|HID)+(1|ID2)",
     "y~PM25+med+ns(TMP,3)+agec+urban_nbs+sex+edu+married+smoke+drink+
     city2+(1|HID)+(1|ID2)",
     "y~PM25+med+ns(TMP,3)+agec+urban_nbs+sex+edu+married+smoke+drink+incold+heat+multistory+rent+untidy+tele+
     city2+(1|HID)+(1|ID2)")
names(fs)=c("unadjusted","simple","standard","fulladjusted")
#
for (y in c("sp","dp")) 
 {
  dta$y <- dta[,y]
  for(lags in 1:4)
  {
  dta$PM25=apply(PM25lag[,1:(6*lags)],1,mean)
  dta %>%  subset(!is.na(y)) %>% subset(ages>=40)->dta1
  dta1 %>% subset(ID2%in%names(which(table(dta1$ID2)>1))) ->dta2
  
  for(f in fs)
   {
    m<-lmer(as.formula(f),data=dta2)
    tmp=as.data.frame(summary(m)$coef)
    id<-which(rownames(tmp)%in%c("PM25"))
    tmp=tmp[id,]
    tmp$lags <- lags
    tmp$y <- y
    tmp$model=names(which(fs==f))
    print(paste(y,lags,tmp$model,collapse = "",sep = " "))
    if(y=="sp"&lags==1&f==fs[1]) coef=tmp else coef=rbind(coef,tmp)
    
   }
  }
 }
coef$lo <- coef$Estimate-coef$`Std. Error`*1.96
coef$up <- coef$Estimate+coef$`Std. Error`*1.96
setwd("E:/graduate/paper1/report5/data results")
write.csv(coef,"basic model coef(avg temp).csv")
save(coef,file = "basic model coef(avg tmp).RData")

