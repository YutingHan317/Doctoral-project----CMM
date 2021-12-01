# 已剔除基线患有重大疾病者，bmi缺失者

# 设置工作路径
setwd("D:/HanYT/2019-06 Multimorbidity")
library(mstate)
library(plyr)

# 载入数据
load("D:/HanYT/2019-06 Multimorbidity/1. Database/project4.rdata")
tempfile <- project4
project4 <- tempfile

# 时间换算为年龄（岁）
project4$study_date <- as.numeric(project4$study_date)
project4$first_cmd_inc_date <- as.numeric(project4$first_cmd_inc_date)
project4$mltmbd_inc_date <- as.numeric(project4$mltmbd_inc_date)
project4$du_ep0001_date <- as.numeric(project4$du_ep0001_date)
project4$dob_anon <- as.numeric(project4$dob_anon)

# 一、某些研究对象首次心血管代谢性疾病发生于2017年12月31日，在上述基础上加上0.5天
attach(project4)
project4$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5
project4$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5 
detach(project4)

# 计算年龄
project4[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")] <- (project4[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")]-project4$dob_anon)/365.25
summary(project4[, c("dob_anon","study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")])
class(project4$dob_anon)

# # # gen original database
# 选取需要的变量进行分析，避免数据库过大
covariates <- c("studyid","age_strata","region_code","region_is_urban","is_female","highest_education","occupation",
                "marital_status","household_income","parental_fh_3groups", "chronic_fh", "mltmbd_fh")
phrases <- c("study_date","first_cmd_inc","first_cmd_inc_date","mltmbd_inc","mltmbd_inc_date","du_ep0001","du_ep0001_date")
lifestyles <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","healthy_score_5groups","healthy_score")
varlist <- c(covariates,phrases,lifestyles)

# 生成第一次疾病和死亡同时发生的研究对象（即死于首发疾病）05/01
project4$fcmd.death[project4$du_ep0001_date==project4$first_cmd_inc_date & project4$du_ep0001==1 & project4$first_cmd_inc==1] <- 1
project4$fcmd.death[is.na(project4$fcmd.death)] <- 0
table(project4$fcmd.death)

# 生成并剔除第一次和第二次疾病同时发生的研究对象  4/30
project4$fcmd.mltmbd[project4$mltmbd_inc_date==project4$first_cmd_inc_date & project4$mltmbd_inc==1 & project4$first_cmd_inc==1] <- 1
project4$fcmd.mltmbd[is.na(project4$fcmd.mltmbd)] <- 0
sum(is.na(project4$fcmd.mltmbd))
mltstate.origin <- subset(project4, fcmd.mltmbd == 0, select = c(varlist,"fcmd.death"))  # 04-08 不再先剔除有相同日期的人
mltstate.origin$enrollment <- 1

# # # 根据死亡时间向前计算“健康到首发疾病”的发病，根据不同间隔考虑生成多个数据库检验不同
mltstate.origin <- mutate(mltstate.origin, first_cmd_inc_date1 = first_cmd_inc_date,
                          first_cmd_inc_date2 = first_cmd_inc_date,
                          first_cmd_inc_date3 = first_cmd_inc_date,
                          first_cmd_inc_date4 = first_cmd_inc_date,
                          first_cmd_inc_date5 = first_cmd_inc_date)
fcmd.list <- paste("first_cmd_inc_date",1:5,sep = "")
summary(mltstate.origin[fcmd.list])

# 0.5天：最小为1天，减去0.5没有影响，无需剔除任何研究对象
mltstate.origin$first_cmd_inc_date1[mltstate.origin$fcmd.death==1] <- mltstate.origin$first_cmd_inc_date1[mltstate.origin$fcmd.death==1] - 0.5/365.25  
summary(a[c("first_cmd_inc_date1","first_cmd_inc_date")])
mltstate.origin1 <- mltstate.origin
summary(mltstate.origin1[c("first_cmd_inc_date1","first_cmd_inc_date")])

# 半年：剔除随访半年内死亡的研究对象
mltstate.origin$first_cmd_inc_date2[mltstate.origin$fcmd.death==1] <- mltstate.origin$first_cmd_inc_date2[mltstate.origin$fcmd.death==1] - 0.5  
mltstate.origin2 <- subset(mltstate.origin, study_date<first_cmd_inc_date2)  # 死亡往前推半年，若超过基线，就剔除
summary(mltstate.origin2[c("first_cmd_inc_date2","first_cmd_inc_date")])

# 1年：剔除随访1年内死亡的研究对象
mltstate.origin$first_cmd_inc_date3[mltstate.origin$fcmd.death==1] <- mltstate.origin$first_cmd_inc_date3[mltstate.origin$fcmd.death==1] - 1  
mltstate.origin3 <- subset(mltstate.origin, study_date<first_cmd_inc_date3)  # 死亡往前推一年，若超过基线，就剔除
summary(mltstate.origin3[c("first_cmd_inc_date3","first_cmd_inc_date")])

# 3年：剔除随访3年内死亡的研究对象
mltstate.origin$first_cmd_inc_date4[mltstate.origin$fcmd.death==1] <- mltstate.origin$first_cmd_inc_date4[mltstate.origin$fcmd.death==1] - 3 
mltstate.origin4 <- subset(mltstate.origin, study_date<first_cmd_inc_date4)  # 死亡往前推三年，若超过基线，就剔除
summary(mltstate.origin4[c("first_cmd_inc_date4","first_cmd_inc_date")])

# 5年：剔除随访5年内死亡的研究对象
mltstate.origin$first_cmd_inc_date5[mltstate.origin$fcmd.death==1] <- mltstate.origin$first_cmd_inc_date5[mltstate.origin$fcmd.death==1] - 5 
mltstate.origin5 <- subset(mltstate.origin, study_date<first_cmd_inc_date5)  # 死亡往前推五年，若超过基线，就剔除
summary(mltstate.origin5[c("first_cmd_inc_date5","first_cmd_inc_date")])

# 设置路径
tmat <- transMat(x = list(c(2, 4), c(3,4), c(4), c()), names = c("healthy", "fcmd", "mltmbd", "Death"))

# # # 大循环使用多个数据库
originlist <- list(mltstate.origin1,mltstate.origin2,mltstate.origin3,
                   mltstate.origin4,mltstate.origin5)
fcmd_datevector <- paste("first_cmd_inc_date",1:5,sep = "")
timevector <- c("0.5d","0.5y","1y","3y","5y")

#建立空 long&full&final list
longlist <- list()
fulllist <- list()
finallist <- list()

# Wide to long
for (i in 1:5){
    longlist[[i]] <- msprep(data = originlist[[i]], trans = tmat,
                            time = c(NA, fcmd_datevector[i], "mltmbd_inc_date","du_ep0001_date"),
                            status = c(NA, "first_cmd_inc", "mltmbd_inc", "du_ep0001"),
                            keep = c(covariates,lifestyles,"study_date"),
                            start = list(state = originlist[[i]]$enrollment,time = originlist[[i]]$study_date))
    
    # set lifestyle score as category
    longlist[[i]]$hscore <- longlist[[i]]$healthy_score_5groups
    longlist[[i]]$hscore.cat <- as.factor(longlist[[i]]$hscore)
    
    # Transition-specific covariates
    covs <- c("healthy_smoking", "healthy_alcohol", "healthy_diet", "healthy_bmi", "healthy_WC", "healthy_PA", "healthy_sleep",
              "hscore","hscore.cat")
    fulllist[[i]] <- expand.covs(longlist[[i]], covs, longnames = FALSE)  # 均为哑变量
    head(fulllist[[i]])
    
    # Drop observation of which time = 0
    message(sum(fulllist[[i]]$time!=0)," records was droped")
    finallist[[i]] <- subset(fulllist[[i]],time != 0)
    
    ###  Calculate HR for single lifestyle
    # Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
    single2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                         is_female + 
                         as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                         healthy_smoking.1 + healthy_smoking.2 + healthy_smoking.3 + healthy_smoking.4 + healthy_smoking.5 +
                         healthy_alcohol.1 + healthy_alcohol.2 + healthy_alcohol.3 + healthy_alcohol.4 + healthy_alcohol.5 +
                         healthy_diet.1 + healthy_diet.2  + healthy_diet.3 + healthy_diet.4 + healthy_diet.5 + 
                         healthy_bmi.1 + healthy_bmi.2  + healthy_bmi.3 + healthy_bmi.4 + healthy_bmi.5 +
                         healthy_WC.1 + healthy_WC.2  + healthy_WC.3 + healthy_WC.4 + healthy_WC.5 +
                         healthy_PA.1 + healthy_PA.2  + healthy_PA.3 + healthy_PA.4 + healthy_PA.5 +
                         healthy_sleep.1 + healthy_sleep.2 + healthy_sleep.3 +healthy_sleep.4 + healthy_sleep.5,
                     data = finallist[[i]], method = "breslow")
    single.hr2 <- round(summary(single2)$conf.int,2)
    write.csv(single.hr2, paste0("3. Result/multistate/single model2 ",timevector[i],Sys.Date(),".csv"))
    
    ### Calculate HR for lifestyle score (ordinal) ###
    # Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
    scoreord2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                           is_female + 
                           as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                           hscore.1 + hscore.2 + hscore.3 + hscore.4 + hscore.5,
                       data = finallist[[i]], method = "breslow")
    scoreord.hr2 <- round(summary(scoreord2)$conf.int,2)
    write.csv(scoreord.hr2, paste0("3. Result/multistate/ordinal score model2 ",timevector[i],Sys.Date(),".csv"))
    
    print(i)
    
}






# Drop observation of which time = 0 
nrow(mltstate.full[,"time"!=0])
sum(mltstate.full$time!=0)
mltstate.final <- subset(mltstate.full,time != 0)

# Number of transition(暂无法完成)
event <- events(mltstate.full)
Freq <- event$Frequencies[,1:5]
Prop <- event$Proportions
Freq.Prop <- rbind(Freq,Prop)
write.csv(Freq.Prop, paste0("3. Result/multistate/Number of transitions ",substr(Sys.Date(),5,10),".csv"))

###  Calculate HR for single lifestyle  ###
# Model 1 age, region code, sex
single1 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     is_female + 
                     healthy_smoking.1 + healthy_smoking.2 + healthy_smoking.3 + healthy_smoking.4 + healthy_smoking.5 +
                     healthy_alcohol.1 + healthy_alcohol.2 + healthy_alcohol.3 + healthy_alcohol.4 + healthy_alcohol.5 +
                     healthy_diet.1 + healthy_diet.2  + healthy_diet.3 + healthy_diet.4 + healthy_diet.5 + 
                     healthy_obesity.1 + healthy_obesity.2  + healthy_obesity.3 + healthy_obesity.4 + healthy_obesity.5 +
                     healthy_PA.1 + healthy_PA.2  + healthy_PA.3 + healthy_PA.4 + healthy_PA.5 +
                     healthy_sleep.1 + healthy_sleep.2 + healthy_sleep.3 +healthy_sleep.4 + healthy_sleep.5,
                 data = mltstate.final, method = "breslow")
single.hr1 <- round(summary(single1)$conf.int,2)
write.csv(single.hr1, paste0("3. Result/multistate/single model1 ","i",substr(Sys.Date(),5,10),".csv"))

# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
single2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     is_female + 
                     as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                     healthy_smoking.1 + healthy_smoking.2 + healthy_smoking.3 + healthy_smoking.4 + healthy_smoking.5 +
                     healthy_alcohol.1 + healthy_alcohol.2 + healthy_alcohol.3 + healthy_alcohol.4 + healthy_alcohol.5 +
                     healthy_diet.1 + healthy_diet.2  + healthy_diet.3 + healthy_diet.4 + healthy_diet.5 + 
                     healthy_obesity.1 + healthy_obesity.2  + healthy_obesity.3 + healthy_obesity.4 + healthy_obesity.5 +
                     healthy_PA.1 + healthy_PA.2  + healthy_PA.3 + healthy_PA.4 + healthy_PA.5 +
                     healthy_sleep.1 + healthy_sleep.2 + healthy_sleep.3 +healthy_sleep.4 + healthy_sleep.5,
                 data = mltstate.final, method = "breslow")
single.hr2 <- round(summary(single2)$conf.int,2)
write.csv(single.hr2, paste0("3. Result/multistate/single model2 ",substr(Sys.Date(),5,10),".csv"))

### Calculate HR for lifestyle score (category) ###
# Model 1 age, region code, sex
scorecat1 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans) + strata(region_code) + strata(age_strata) +
                       is_female +
                       hscore.cat1.1 + hscore.cat2.1 + hscore.cat3.1 + hscore.cat4.1 + hscore.cat5.1 +
                       hscore.cat1.2 + hscore.cat2.2 + hscore.cat3.2 + hscore.cat4.2 + hscore.cat5.2 +
                       hscore.cat1.3 + hscore.cat2.3 + hscore.cat3.3 + hscore.cat4.3 + hscore.cat5.3 +
                       hscore.cat1.4 + hscore.cat2.4 + hscore.cat3.4 + hscore.cat4.4 + hscore.cat5.4 +
                       hscore.cat1.5 + hscore.cat2.5 + hscore.cat3.5 + hscore.cat4.5 + hscore.cat5.5 ,
                   data = mltstate.final, method = "breslow")
scorecat.hr1 <- round(summary(scorecat1)$conf.int,2)
write.csv(scorecat.hr1, paste0("3. Result/multistate/categorical score model1 ",substr(Sys.Date(),5,10),".csv"))

# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scorecat2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       is_female + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.cat1.1 + hscore.cat2.1 + hscore.cat3.1 + hscore.cat4.1 + hscore.cat5.1 +
                       hscore.cat1.2 + hscore.cat2.2 + hscore.cat3.2 + hscore.cat4.2 + hscore.cat5.2 +
                       hscore.cat1.3 + hscore.cat2.3 + hscore.cat3.3 + hscore.cat4.3 + hscore.cat5.3 +
                       hscore.cat1.4 + hscore.cat2.4 + hscore.cat3.4 + hscore.cat4.4 + hscore.cat5.4 +
                       hscore.cat1.5 + hscore.cat2.5 + hscore.cat3.5 + hscore.cat4.5 + hscore.cat5.5 ,
                   data = mltstate.final, method = "breslow")
scorecat.hr2 <- round(summary(scorecat2)$conf.int,2)
write.csv(scorecat.hr2, paste0("3. Result/multistate/categorical score model2 ",substr(Sys.Date(),5,10),".csv"))

### Calculate HR for lifestyle score (ordinal) ###
# Model 1 age, region code, sex
scoreord1 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans) + strata(region_code) + strata(age_strata) +
                       is_female +
                       hscore.1 + hscore.2 + hscore.3 + hscore.4 + hscore.5,
                   data = mltstate.final, method = "breslow")
scoreord.hr1 <- round(summary(scoreord1)$conf.int,2)
write.csv(scoreord.hr1, paste0("3. Result/multistate/ordinal score model1 ",substr(Sys.Date(),5,10),".csv"))

# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scoreord2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       is_female + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.1 + hscore.2 + hscore.3 + hscore.4 + hscore.5,
                   data = mltstate.final, method = "breslow")
scoreord.hr2 <- round(summary(scoreord2)$conf.int,2)
write.csv(scoreord.hr2, paste0("3. Result/multistate/ordinal score model2 ",substr(Sys.Date(),5,10),".csv"))
