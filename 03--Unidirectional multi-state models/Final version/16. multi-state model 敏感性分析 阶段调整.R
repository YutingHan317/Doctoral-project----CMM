# 已剔除基线患有肿瘤和糖尿病，脑卒中和缺血性心脏病
# 调整变量删除职业 8/31
# 所有协变量均设置为trans-specific
#######  2021/2/23
# 分四个阶段调整 （1）主模型无教育程度

# 设置工作路径
rm(list=ls())
setwd("D:/HanYT/2019-06 Multimorbidity")
library(mstate)
library(plyr)
library(lmtest)
library(stringr)
library(tidyverse)
library(reshape2)

################################################################################################################################################
###################################                                                           ##################################################
###################################                    gen dataset                            ##################################################
###################################                                                           ##################################################
################################################################################################################################################

# 载入数据
load("./1. Database/project4.rdata")

tempfile <- project4
project4 <- subset(tempfile,study_date != du_ep0001_date)

# 提取子集，增加运算速度;待调整好之后删除
# project4 <- subset(project4,region_is_urban==0)

# 计算同一时间抵达多个阶段，用于之后的剔除同时抵达不同阶段的研究对象
# project4$sametime[project4$mltmbd_inc_date==project4$first_cmd_inc_date & project4$mltmbd_inc==1 & project4$first_cmd_inc==1] <- T
# project4$sametime[project4$du_ep0001_date==project4$first_cmd_inc_date & project4$du_ep0001==1 & project4$first_cmd_inc==1] <- T
# project4$sametime[project4$du_ep0001_date==project4$mltmbd_inc_date & project4$du_ep0001==1 & project4$mltmbd_inc==1] <- T
# project4$sametime[is.na(project4$sametime)] <- F
# tr(project4$sametime)
# sum(is.na(project4$sametime))·
# table(project4$sametime)

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
                "marital_status_2groups","household_income","parental_fh_3groups","parental_fh_2groups", "chronic_fh", "menopause_2groups",
                "age_3groups","region_is_urban", "education_3groups","has_hypertension","has_hyperlipidemia")
phrases <- c("study_date","first_cmd_inc","first_cmd_inc_date","mltmbd_inc","mltmbd_inc_date","du_ep0001","du_ep0001_date")
lifestyles <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","healthy_score_5groups","healthy_score",
                "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_PA","risky_obesity","risky_score_5groups","risky_score")
varlist <- c(covariates,phrases,lifestyles)

# # 数据库中存在不同阶段记录时间相同的个体，生成对应变量指代其类型 
attach(project4)
project4$sametime[first_cmd_inc==1 & du_ep0001 == 1 & du_ep0001_date==first_cmd_inc_date] <- 1      # 死于首次CMD
project4$sametime[first_cmd_inc==1 & mltmbd_inc == 1 & mltmbd_inc_date==first_cmd_inc_date] <- 2      # 首次心血管代谢性疾病时间与共病记录日期相同
project4$sametime[mltmbd_inc == 1 & du_ep0001 == 1 & mltmbd_inc_date==du_ep0001_date] <- 3      # 死于首次发生共病
project4$sametime[first_cmd_inc == 1 &  mltmbd_inc == 1 & du_ep0001 == 1 & mltmbd_inc_date==du_ep0001_date & first_cmd_inc_date==mltmbd_inc_date] <- 4      # 首次CMD、共病和死亡日期相同
project4$sametime[is.na(project4$sametime)] <- 0
table(project4$sametime)
detach(project4)

# # 对时间相同的不同阶段进行处理
# 死于首次CMD
project4$first_cmd_inc_date[project4$sametime==1] <- project4$du_ep0001_date[project4$sametime==1] - 0.5/365.25
# 首次心血管代谢性疾病时间与共病记录日期相同
project4$first_cmd_inc_date[project4$sametime==2] <- project4$mltmbd_inc_date[project4$sametime==2] - 0.5/365.25
# 死于首次发生共病
project4$mltmbd_inc_date[project4$sametime==3] <- project4$du_ep0001_date[project4$sametime==3] - 0.5/365.25
# 首次CMD、共病和死亡日期相同
project4$mltmbd_inc_date[project4$sametime==4] <- project4$du_ep0001_date[project4$sametime==4] - 0.5/365.25
project4$first_cmd_inc_date[project4$sametime==4] <- project4$mltmbd_inc_date[project4$sametime==4] - 0.5/365.25


# 仅保留需要的变量
mltstate.origin <- subset(project4, select = varlist)  
# 生成代表进入研究的阶段
mltstate.origin$enrollment <- 1

# 设置路径
tmat <- transMat(x = list(c(2, 4), c(3,4), c(4), c()), names = c("healthy", "fcmd", "mltmbd", "Death"))

# Wide to long
mltstate.long <- msprep(data = mltstate.origin, trans = tmat,
                        time = c(NA, "first_cmd_inc_date", "mltmbd_inc_date","du_ep0001_date"),
                        status = c(NA, "first_cmd_inc", "mltmbd_inc", "du_ep0001"),
                        keep = c(covariates,lifestyles),
                        start = list(state = mltstate.origin$enrollment,time = mltstate.origin$study_date))

# Transitions
events(mltstate.long)
sum(mltstate.long$time==0)
events.primary <- events(mltstate.long)
freq_trans <- data.frame(events.primary$Frequencies)
freq_trans <- dcast(freq_trans,from~to,value.var = "Freq")
prop_trans <- data.frame(events.primary$Proportions)
prop_trans <- dcast(prop_trans,from~to,value.var = "Freq")
for (i in 2:5){
    freq_trans[,i] <- paste(freq_trans[,i]," (",round(prop_trans[,i],3),")")
}
write.csv(freq_trans, paste("3. Result/multistate/primary analyses/transitions",".csv",sep = ""))

# Set lifestyle score as category
mltstate.long$hscore <- mltstate.long$healthy_score
mltstate.long$hscore.cat <- as.factor(mltstate.long$healthy_score_5groups)

mltstate.long$rscore <- mltstate.long$risky_score
mltstate.long$rscore.cat <- as.factor(mltstate.long$risky_score_5groups)

# Transition-specific covariates
long_covariates <- c("is_female","highest_education","occupation","marital_status_2groups","menopause_2groups","has_hyperlipidemia",
                     "age_3groups","region_is_urban", "education_3groups","has_hypertension","region_code","parental_fh_3groups","parental_fh_2groups")
long_lf <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","hscore","hscore.cat",
             "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_obesity","risky_PA","rscore","rscore.cat")
mltstate.full <- expand.covs(mltstate.long, c(long_covariates,long_lf), longnames = FALSE)  # 均为哑变量
head(mltstate.full)
names(mltstate.full)

# Drop observation of which time = 0 
message(sum(mltstate.full$time==0)," records was droped")
mltstate.final <- subset(mltstate.full,time != 0)

################################################################################################################################################
###################################                                                           ##################################################
###################################                    primary analyses                       ##################################################
###################################                                                           ##################################################
################################################################################################################################################
path_adjust <- paste0("3. Result/multistate/sensitivity analyses adjust","/",Sys.Date())
dir.create(path_adjust)

#####################################################   原模型
# # # # dichotomous single lifestyle;model 1 和 model2 差别很小，下面仅罗列了model2;家族史变量调一个和两个没有大的区别（05-24）
single2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 + 
                     risky_obesity.1 + risky_obesity.2  + risky_obesity.3 + risky_obesity.4 + risky_obesity.5,
                 data = mltstate.final, method = "breslow")
single.hr2 <- data.frame(round(summary(single2)$conf.int,2))
single.hr2[,5] <- paste0(sprintf(single.hr2[,1],fmt="%.2f")," (",sprintf(single.hr2[,3],fmt="%.2f"),"-",sprintf(single.hr2[,4],fmt="%.2f"),")")
#single.hr2[,6] <- rownames(single.hr2)
#single.hr2[,6] <- substr(single.hr2[,6],1,nchar(single.hr2[,6])-2)
#single.hr2[,7] <- 1:5
#single.hr2 <- single.hr2[(nrow(single.hr2)-29):nrow(single.hr2),]
#names(single.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
#single.hr2 <- dcast(single.hr2,Variable~single.hr2$Transition,value.var = "HR_95CI")
write.csv(single.hr2, paste0(path_adjust,"/dichotomous single model2 primary",".csv"))

### Calculate HR for lifestyle score (category) 
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scorecat2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                       rscore.cat1.1 + rscore.cat2.1 + rscore.cat3.1 + rscore.cat4.1 + 
                       rscore.cat1.2 + rscore.cat2.2 + rscore.cat3.2 + rscore.cat4.2 + 
                       rscore.cat1.3 + rscore.cat2.3 + rscore.cat3.3 + rscore.cat4.3 + 
                       rscore.cat1.4 + rscore.cat2.4 + rscore.cat3.4 + rscore.cat4.4 + 
                       rscore.cat1.5 + rscore.cat2.5 + rscore.cat3.5 + rscore.cat4.5  ,
                   data = mltstate.final, method = "breslow")
scorecat.hr2 <- data.frame(round(summary(scorecat2)$conf.int,2))
scorecat.hr2[,5] <- paste0(sprintf(scorecat.hr2[,1],fmt="%.2f")," (",sprintf(scorecat.hr2[,3],fmt="%.2f"),"-",sprintf(scorecat.hr2[,4],fmt="%.2f"),")")
#scorecat.hr2[,6] <- rownames(scorecat.hr2)
#scorecat.hr2[,6] <- substr(scorecat.hr2[,6],1,nchar(scorecat.hr2[,6])-2)
#scorecat.hr2[,7] <- str_sub(rownames(scorecat.hr2),-1)
#scorecat.hr2 <- scorecat.hr2[(nrow(scorecat.hr2)-19):nrow(scorecat.hr2),]
#names(scorecat.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
#scorecat.hr2 <- dcast(scorecat.hr2,Variable~scorecat.hr2$Transition,value.var = "HR_95CI")
write.csv(scorecat.hr2, paste0(path_adjust,"/category score model2 primary",".csv"))

### Calculate HR for lifestyle score (ordinal) （各阶段协变量系数相同）
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scoreord2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                       rscore.1 + rscore.2 + rscore.3 + rscore.4 + rscore.5,
                   data = mltstate.final, method = "breslow")
scoreord.hr2 <- data.frame(round(summary(scoreord2)$conf.int,2))
scoreord.hr2[,5] <- paste0(sprintf(scoreord.hr2[,1],fmt="%.2f")," (",sprintf(scoreord.hr2[,3],fmt="%.2f"),"-",sprintf(scoreord.hr2[,4],fmt="%.2f"),")")
write.csv(scoreord.hr2, paste0(path_adjust,"/ordinal score model2 primary",".csv"))


#####################################################   调整血压
# # # # dichotomous single lifestyle;model 1 和 model2 差别很小，下面仅罗列了model2;家族史变量调一个和两个没有大的区别（05-24）
single2.ht <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     as.factor(has_hypertension.1) + as.factor(has_hypertension.2) + as.factor(has_hypertension.3) + as.factor(has_hypertension.4) + as.factor(has_hypertension.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 + 
                     risky_obesity.1 + risky_obesity.2  + risky_obesity.3 + risky_obesity.4 + risky_obesity.5,
                 data = mltstate.final, method = "breslow")
single.ht.hr2 <- data.frame(round(summary(single2.ht)$conf.int,2))
single.ht.hr2[,5] <- paste0(sprintf(single.ht.hr2[,1],fmt="%.2f")," (",sprintf(single.ht.hr2[,3],fmt="%.2f"),"-",sprintf(single.ht.hr2[,4],fmt="%.2f"),")")
#single.ht.hr2[,6] <- rownames(single.ht.hr2)
#single.ht.hr2[,6] <- substr(single.ht.hr2[,6],1,nchar(single.ht.hr2[,6])-2)
#single.ht.hr2[,7] <- 1:5
#single.ht.hr2 <- single.ht.hr2[(nrow(single.ht.hr2)-29):nrow(single.ht.hr2),]
#names(single.ht.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
#single.ht.hr2 <- dcast(single.ht.hr2,Variable~single.ht.hr2$Transition,value.var = "HR_95CI")
write.csv(single.ht.hr2, paste0(path_adjust,"/dichotomous single model2 血压",".csv"))

### Calculate HR for lifestyle score (category) 
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scorecat2.ht <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                       as.factor(has_hypertension.1) + as.factor(has_hypertension.2) + as.factor(has_hypertension.3) + as.factor(has_hypertension.4) + as.factor(has_hypertension.5) + 
                       rscore.cat1.1 + rscore.cat2.1 + rscore.cat3.1 + rscore.cat4.1 + 
                       rscore.cat1.2 + rscore.cat2.2 + rscore.cat3.2 + rscore.cat4.2 + 
                       rscore.cat1.3 + rscore.cat2.3 + rscore.cat3.3 + rscore.cat4.3 + 
                       rscore.cat1.4 + rscore.cat2.4 + rscore.cat3.4 + rscore.cat4.4 + 
                       rscore.cat1.5 + rscore.cat2.5 + rscore.cat3.5 + rscore.cat4.5  ,
                   data = mltstate.final, method = "breslow")
scorecat.ht.hr2 <- data.frame(round(summary(scorecat2.ht)$conf.int,2))
scorecat.ht.hr2[,5] <- paste0(sprintf(scorecat.ht.hr2[,1],fmt="%.2f")," (",sprintf(scorecat.ht.hr2[,3],fmt="%.2f"),"-",sprintf(scorecat.ht.hr2[,4],fmt="%.2f"),")")
#scorecat.ht.hr2[,6] <- rownames(scorecat.ht.hr2)
#scorecat.ht.hr2[,6] <- substr(scorecat.ht.hr2[,6],1,nchar(scorecat.ht.hr2[,6])-2)
#scorecat.ht.hr2[,7] <- str_sub(rownames(scorecat.ht.hr2),-1)
#scorecat.ht.hr2 <- scorecat.ht.hr2[(nrow(scorecat.ht.hr2)-19):nrow(scorecat.ht.hr2),]
#names(scorecat.ht.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
#scorecat.ht.hr2 <- dcast(scorecat.ht.hr2,Variable~scorecat.ht.hr2$Transition,value.var = "HR_95CI")
write.csv(scorecat.ht.hr2, paste0(path_adjust,"/category score model2 血压",".csv"))

### Calculate HR for lifestyle score (ordinal) （各阶段协变量系数相同）
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scoreord2.ht <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                       as.factor(has_hypertension.1) + as.factor(has_hypertension.2) + as.factor(has_hypertension.3) + as.factor(has_hypertension.4) + as.factor(has_hypertension.5) + 
                       rscore.1 + rscore.2 + rscore.3 + rscore.4 + rscore.5,
                   data = mltstate.final, method = "breslow")
scoreord.ht.hr2 <- data.frame(round(summary(scoreord2.ht)$conf.int,2))
scoreord.ht.hr2[,5] <- paste0(sprintf(scoreord.ht.hr2[,1],fmt="%.2f")," (",sprintf(scoreord.ht.hr2[,3],fmt="%.2f"),"-",sprintf(scoreord.ht.hr2[,4],fmt="%.2f"),")")
write.csv(scoreord.ht.hr2, paste0(path_adjust,"/ordinal score model2 model2 血压",".csv"))

#####################################################   调整血压和血脂
# # # # dichotomous single lifestyle;model 1 和 model2 差别很小，下面仅罗列了model2;家族史变量调一个和两个没有大的区别（05-24）
single2.hthl <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     as.factor(has_hypertension.1) + as.factor(has_hypertension.2) + as.factor(has_hypertension.3) + as.factor(has_hypertension.4) + as.factor(has_hypertension.5) + 
                     as.factor(has_hyperlipidemia.1) + as.factor(has_hyperlipidemia.2) + as.factor(has_hyperlipidemia.3) + as.factor(has_hyperlipidemia.4) + as.factor(has_hyperlipidemia.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 + 
                     risky_obesity.1 + risky_obesity.2  + risky_obesity.3 + risky_obesity.4 + risky_obesity.5,
                 data = mltstate.final, method = "breslow")
single.hthl.hr2 <- data.frame(round(summary(single2.hthl)$conf.int,2))
single.hthl.hr2[,5] <- paste0(sprintf(single.hthl.hr2[,1],fmt="%.2f")," (",sprintf(single.hthl.hr2[,3],fmt="%.2f"),"-",sprintf(single.hthl.hr2[,4],fmt="%.2f"),")")
#single.hthl.hr2[,6] <- rownames(single.hthl.hr2)
#single.hthl.hr2[,6] <- substr(single.hthl.hr2[,6],1,nchar(single.hthl.hr2[,6])-2)
#single.hthl.hr2[,7] <- 1:5
#single.hthl.hr2 <- single.hthl.hr2[(nrow(single.hthl.hr2)-29):nrow(single.hthl.hr2),]
#names(single.hthl.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
#single.hthl.hr2 <- dcast(single.hthl.hr2,Variable~single.hthl.hr2$Transition,value.var = "HR_95CI")
write.csv(single.hthl.hr2, paste0(path_adjust,"/dichotomous single model2 血压血脂",".csv"))

### Calculate HR for lifestyle score (category) 
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scorecat2.hthl <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                       as.factor(has_hypertension.1) + as.factor(has_hypertension.2) + as.factor(has_hypertension.3) + as.factor(has_hypertension.4) + as.factor(has_hypertension.5) + 
                       as.factor(has_hyperlipidemia.1) + as.factor(has_hyperlipidemia.2) + as.factor(has_hyperlipidemia.3) + as.factor(has_hyperlipidemia.4) + as.factor(has_hyperlipidemia.5) + 
                       rscore.cat1.1 + rscore.cat2.1 + rscore.cat3.1 + rscore.cat4.1 + 
                       rscore.cat1.2 + rscore.cat2.2 + rscore.cat3.2 + rscore.cat4.2 + 
                       rscore.cat1.3 + rscore.cat2.3 + rscore.cat3.3 + rscore.cat4.3 + 
                       rscore.cat1.4 + rscore.cat2.4 + rscore.cat3.4 + rscore.cat4.4 + 
                       rscore.cat1.5 + rscore.cat2.5 + rscore.cat3.5 + rscore.cat4.5  ,
                   data = mltstate.final, method = "breslow")
scorecat.hthl.hr2 <- data.frame(round(summary(scorecat2.hthl)$conf.int,2))
scorecat.hthl.hr2[,5] <- paste0(sprintf(scorecat.hthl.hr2[,1],fmt="%.2f")," (",sprintf(scorecat.hthl.hr2[,3],fmt="%.2f"),"-",sprintf(scorecat.hthl.hr2[,4],fmt="%.2f"),")")
#scorecat.hthl.hr2[,6] <- rownames(scorecat.hthl.hr2)
#scorecat.hthl.hr2[,6] <- substr(scorecat.hthl.hr2[,6],1,nchar(scorecat.hthl.hr2[,6])-2)
#scorecat.hthl.hr2[,7] <- str_sub(rownames(scorecat.hthl.hr2),-1)
#scorecat.hthl.hr2 <- scorecat.hthl.hr2[(nrow(scorecat.hthl.hr2)-19):nrow(scorecat.hthl.hr2),]
#names(scorecat.hthl.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
#scorecat.hthl.hr2 <- dcast(scorecat.hthl.hr2,Variable~scorecat.hthl.hr2$Transition,value.var = "HR_95CI")
write.csv(scorecat.hthl.hr2, paste0(path_adjust,"/category score model2 血压血脂",".csv"))

### Calculate HR for lifestyle score (ordinal) （各阶段协变量系数相同）
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scoreord2.hthl <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                       as.factor(has_hypertension.1) + as.factor(has_hypertension.2) + as.factor(has_hypertension.3) + as.factor(has_hypertension.4) + as.factor(has_hypertension.5) + 
                       as.factor(has_hyperlipidemia.1) + as.factor(has_hyperlipidemia.2) + as.factor(has_hyperlipidemia.3) + as.factor(has_hyperlipidemia.4) + as.factor(has_hyperlipidemia.5) + 
                       rscore.1 + rscore.2 + rscore.3 + rscore.4 + rscore.5,
                   data = mltstate.final, method = "breslow")
scoreord.hthl.hr2 <- data.frame(round(summary(scoreord2.hthl)$conf.int,2))
scoreord.hthl.hr2[,5] <- paste0(sprintf(scoreord.hthl.hr2[,1],fmt="%.2f")," (",sprintf(scoreord.hthl.hr2[,3],fmt="%.2f"),"-",sprintf(scoreord.hthl.hr2[,4],fmt="%.2f"),")")
write.csv(scoreord.hthl.hr2, paste0(path_adjust,"/ordinal score model2 model2 血压血脂",".csv"))


#####################################################   原模型+不调整教育程度
# # # # dichotomous single lifestyle;model 1 和 model2 差别很小，下面仅罗列了model2;家族史变量调一个和两个没有大的区别（05-24）
single2.noedu <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 + 
                     risky_obesity.1 + risky_obesity.2  + risky_obesity.3 + risky_obesity.4 + risky_obesity.5,
                 data = mltstate.final, method = "breslow")
single.hr2.noedu <- data.frame(round(summary(single2.noedu)$conf.int,2))
single.hr2.noedu[,5] <- paste0(sprintf(single.hr2.noedu[,1],fmt="%.2f")," (",sprintf(single.hr2.noedu[,3],fmt="%.2f"),"-",sprintf(single.hr2.noedu[,4],fmt="%.2f"),")")
#single.hr2.noedu[,6] <- rownames(single.hr2.noedu)
#single.hr2.noedu[,6] <- substr(single.hr2.noedu[,6],1,nchar(single.hr2.noedu[,6])-2)
#single.hr2.noedu[,7] <- 1:5
#single.hr2.noedu <- single.hr2.noedu[(nrow(single.hr2.noedu)-29):nrow(single.hr2.noedu),]
#names(single.hr2.noedu)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
#single.hr2.noedu <- dcast(single.hr2.noedu,Variable~single.hr2.noedu$Transition,value.var = "HR_95CI")
write.csv(single.hr2.noedu, paste0(path_adjust,"/dichotomous single model2 no edu",".csv"))

### Calculate HR for lifestyle score (category) 
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scorecat2.noedu <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                       rscore.cat1.1 + rscore.cat2.1 + rscore.cat3.1 + rscore.cat4.1 + 
                       rscore.cat1.2 + rscore.cat2.2 + rscore.cat3.2 + rscore.cat4.2 + 
                       rscore.cat1.3 + rscore.cat2.3 + rscore.cat3.3 + rscore.cat4.3 + 
                       rscore.cat1.4 + rscore.cat2.4 + rscore.cat3.4 + rscore.cat4.4 + 
                       rscore.cat1.5 + rscore.cat2.5 + rscore.cat3.5 + rscore.cat4.5  ,
                   data = mltstate.final, method = "breslow")
scorecat.hr2.noedu <- data.frame(round(summary(scorecat2.noedu)$conf.int,2))
scorecat.hr2.noedu[,5] <- paste0(sprintf(scorecat.hr2.noedu[,1],fmt="%.2f")," (",sprintf(scorecat.hr2.noedu[,3],fmt="%.2f"),"-",sprintf(scorecat.hr2.noedu[,4],fmt="%.2f"),")")
#scorecat.hr2.noedu[,6] <- rownames(scorecat.hr2.noedu)
#scorecat.hr2.noedu[,6] <- substr(scorecat.hr2.noedu[,6],1,nchar(scorecat.hr2.noedu[,6])-2)
#scorecat.hr2.noedu[,7] <- str_sub(rownames(scorecat.hr2.noedu),-1)
#scorecat.hr2.noedu <- scorecat.hr2.noedu[(nrow(scorecat.hr2.noedu)-19):nrow(scorecat.hr2.noedu),]
#names(scorecat.hr2.noedu)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
#scorecat.hr2.noedu <- dcast(scorecat.hr2.noedu,Variable~scorecat.hr2.noedu$Transition,value.var = "HR_95CI")
write.csv(scorecat.hr2.noedu, paste0(path_adjust,"/category score model2 no edu",".csv"))

### Calculate HR for lifestyle score (ordinal) （各阶段协变量系数相同）
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scoreord2.noedu <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                       rscore.1 + rscore.2 + rscore.3 + rscore.4 + rscore.5,
                   data = mltstate.final, method = "breslow")
scoreord.hr2.edu <- data.frame(round(summary(scoreord2.noedu)$conf.int,2))
scoreord.hr2.edu[,5] <- paste0(sprintf(scoreord.hr2.edu[,1],fmt="%.2f")," (",sprintf(scoreord.hr2.edu[,3],fmt="%.2f"),"-",sprintf(scoreord.hr2.edu[,4],fmt="%.2f"),")")
write.csv(scoreord.hr2.edu, paste0(path_adjust,"/ordinal score model2 no edu",".csv"))