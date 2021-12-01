# 已剔除基线患有肿瘤和糖尿病，脑卒中和缺血性心脏病
# 调整变量删除职业 8/31
# 所有协变量均设置为trans-specific

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
                "marital_status_2groups","household_income","parental_fh_3groups", "chronic_fh", "menopause_2groups",
                "age_3groups","region_is_urban", "education_3groups","has_hypertension")
phrases <- c("study_date","first_cmd_inc","first_cmd_inc_date","mltmbd_inc","mltmbd_inc_date","du_ep0001","du_ep0001_date")
lifestyles <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","healthy_score_5groups","healthy_score",
                "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_PA","risky_obesity","risky_score_5groups","risky_score",
                "risky_bmi2","risky_WC2","risky_WC3","risky_obesity2","bmi_9groups","WC_8groups")
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
long_covariates <- c("is_female","highest_education","occupation","marital_status_2groups","menopause_2groups",
                     "age_3groups","region_is_urban", "education_3groups","has_hypertension","region_code","parental_fh_3groups")
long_lf <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","hscore","hscore.cat",
             "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_obesity","risky_PA","rscore","rscore.cat",
             "risky_bmi2","risky_WC2","risky_WC3","risky_obesity2","bmi_9groups","WC_8groups")
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
path_primary <- paste0("3. Result/multistate/primary analyses","/",Sys.Date())
dir.create(path_primary)

# # # # dichotomous single lifestyle;model 1 和 model2 差别很小，下面仅罗列了model2;家族史变量调一个和两个没有大的区别（05-24）
bmi <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 +
                     risky_bmi.1 + risky_bmi.2  + risky_bmi.3 + risky_bmi.4 + risky_bmi.5 +
                     risky_WC.1 + risky_WC.2  + risky_WC.3 + risky_WC.4 + risky_WC.5,
                 data = mltstate.final, method = "breslow")
bmi.hr2 <- data.frame(round(summary(bmi)$conf.int,2))
bmi.hr2[,5] <- paste0(as.character(bmi.hr2[,1])," (",bmi.hr2[,3],"-",bmi.hr2[,4],")")
bmi.hr2[,6] <- rownames(bmi.hr2)
bmi.hr2[,6] <- substr(bmi.hr2[,6],1,nchar(bmi.hr2[,6])-2)
bmi.hr2[,7] <- 1:5
bmi.hr2 <- bmi.hr2[(nrow(bmi.hr2)-29):nrow(bmi.hr2),]
names(bmi.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
bmi.hr2 <- dcast(bmi.hr2,Variable~bmi.hr2$Transition,value.var = "HR_95CI")
write.csv(bmi.hr2, paste0(path_primary,"/dichotomous bmi model2 primary",".csv"))

bmi2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 +
                     risky_bmi.1 + risky_bmi.2  + risky_bmi.3 + risky_bmi.4 + risky_bmi.5 +
                     risky_WC2.1 + risky_WC2.2  + risky_WC2.3 + risky_WC2.4 + risky_WC2.5,
                 data = mltstate.final, method = "breslow")
bmi2.hr2 <- data.frame(round(summary(bmi2)$conf.int,2))
bmi2.hr2[,5] <- paste0(as.character(bmi2.hr2[,1])," (",bmi2.hr2[,3],"-",bmi2.hr2[,4],")")
bmi2.hr2[,6] <- rownames(bmi2.hr2)
bmi2.hr2[,6] <- substr(bmi2.hr2[,6],1,nchar(bmi2.hr2[,6])-2)
bmi2.hr2[,7] <- 1:5
bmi2.hr2 <- bmi2.hr2[(nrow(bmi2.hr2)-29):nrow(bmi2.hr2),]
names(bmi2.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
bmi2.hr2 <- dcast(bmi2.hr2,Variable~bmi2.hr2$Transition,value.var = "HR_95CI")
write.csv(bmi2.hr2, paste0(path_primary,"/dichotomous bmi2 model2 primary",".csv"))

bmi3 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 +
                     risky_bmi.1 + risky_bmi.2  + risky_bmi.3 + risky_bmi.4 + risky_bmi.5 +
                     risky_WC3.1 + risky_WC3.2  + risky_WC3.3 + risky_WC3.4 + risky_WC3.5,
                 data = mltstate.final, method = "breslow")
bmi3.hr2 <- data.frame(round(summary(bmi3)$conf.int,2))
bmi3.hr2[,5] <- paste0(as.character(bmi3.hr2[,1])," (",bmi3.hr2[,3],"-",bmi3.hr2[,4],")")
bmi3.hr2[,6] <- rownames(bmi3.hr2)
bmi3.hr2[,6] <- substr(bmi3.hr2[,6],1,nchar(bmi3.hr2[,6])-2)
bmi3.hr2[,7] <- 1:5
bmi3.hr2 <- bmi3.hr2[(nrow(bmi3.hr2)-29):nrow(bmi3.hr2),]
names(bmi3.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
bmi3.hr2 <- dcast(bmi3.hr2,Variable~bmi3.hr2$Transition,value.var = "HR_95CI")
write.csv(bmi3.hr2, paste0(path_primary,"/dichotomous bmi3 model2 primary",".csv"))

bmi4 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 +
                     risky_bmi2.1 + risky_bmi2.2  + risky_bmi2.3 + risky_bmi2.4 + risky_bmi2.5 +
                     risky_WC.1 + risky_WC.2  + risky_WC.3 + risky_WC.4 + risky_WC.5,
                 data = mltstate.final, method = "breslow")
bmi4.hr2 <- data.frame(round(summary(bmi4)$conf.int,2))
bmi4.hr2[,5] <- paste0(as.character(bmi4.hr2[,1])," (",bmi4.hr2[,3],"-",bmi4.hr2[,4],")")
bmi4.hr2[,6] <- rownames(bmi4.hr2)
bmi4.hr2[,6] <- substr(bmi4.hr2[,6],1,nchar(bmi4.hr2[,6])-2)
bmi4.hr2[,7] <- 1:5
bmi4.hr2 <- bmi4.hr2[(nrow(bmi4.hr2)-29):nrow(bmi4.hr2),]
names(bmi4.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
bmi4.hr2 <- dcast(bmi4.hr2,Variable~bmi4.hr2$Transition,value.var = "HR_95CI")
write.csv(bmi4.hr2, paste0(path_primary,"/dichotomous bmi4 model2 primary",".csv"))

bmi5 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 +
                     risky_bmi2.1 + risky_bmi2.2  + risky_bmi2.3 + risky_bmi2.4 + risky_bmi2.5 +
                     risky_WC3.1 + risky_WC3.2  + risky_WC3.3 + risky_WC3.4 + risky_WC3.5,
                 data = mltstate.final, method = "breslow")
bmi5.hr2 <- data.frame(round(summary(bmi5)$conf.int,2))
bmi5.hr2[,5] <- paste0(as.character(bmi5.hr2[,1])," (",bmi5.hr2[,3],"-",bmi5.hr2[,4],")")
bmi5.hr2[,6] <- rownames(bmi5.hr2)
bmi5.hr2[,6] <- substr(bmi5.hr2[,6],1,nchar(bmi5.hr2[,6])-2)
bmi5.hr2[,7] <- 1:5
bmi5.hr2 <- bmi5.hr2[(nrow(bmi5.hr2)-29):nrow(bmi5.hr2),]
names(bmi5.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
bmi5.hr2 <- dcast(bmi5.hr2,Variable~bmi5.hr2$Transition,value.var = "HR_95CI")
write.csv(bmi5.hr2, paste0(path_primary,"/dichotomous bmi5 model2 primary",".csv"))

bmi6 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
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
bmi6.hr2 <- data.frame(round(summary(bmi6)$conf.int,2))
bmi6.hr2[,5] <- paste0(as.character(bmi6.hr2[,1])," (",bmi6.hr2[,3],"-",bmi6.hr2[,4],")")
bmi6.hr2[,6] <- rownames(bmi6.hr2)
bmi6.hr2[,6] <- substr(bmi6.hr2[,6],1,nchar(bmi6.hr2[,6])-2)
bmi6.hr2[,7] <- 1:5
bmi6.hr2 <- bmi6.hr2[(nrow(bmi6.hr2)-29):nrow(bmi6.hr2),]
names(bmi6.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
bmi6.hr2 <- dcast(bmi6.hr2,Variable~bmi6.hr2$Transition,value.var = "HR_95CI")
write.csv(bmi6.hr2, paste0(path_primary,"/dichotomous bmi6 model2 primary",".csv"))

bmi7 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 +
                     risky_obesity2.1 + risky_obesity2.2  + risky_obesity2.3 + risky_obesity2.4 + risky_obesity2.5,
                 data = mltstate.final, method = "breslow")
bmi7.hr2 <- data.frame(round(summary(bmi7)$conf.int,2))
bmi7.hr2[,5] <- paste0(as.character(bmi7.hr2[,1])," (",bmi7.hr2[,3],"-",bmi7.hr2[,4],")")
bmi7.hr2[,6] <- rownames(bmi7.hr2)
bmi7.hr2[,6] <- substr(bmi7.hr2[,6],1,nchar(bmi7.hr2[,6])-2)
bmi7.hr2[,7] <- 1:5
bmi7.hr2 <- bmi7.hr2[(nrow(bmi7.hr2)-29):nrow(bmi7.hr2),]
names(bmi7.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
bmi7.hr2 <- dcast(bmi7.hr2,Variable~bmi7.hr2$Transition,value.var = "HR_95CI")
write.csv(bmi7.hr2, paste0(path_primary,"/dichotomous bmi7 model2 primary",".csv"))

bmi8 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 +
                     as.factor(bmi_9groups.1) + as.factor(bmi_9groups.2)  + as.factor(bmi_9groups.3) + as.factor(bmi_9groups.4) + as.factor(bmi_9groups.5) +
                     risky_WC.1 + risky_WC.2  + risky_WC.3 + risky_WC.4 + risky_WC.5,
                 data = mltstate.final, method = "breslow")
bmi8.hr2 <- data.frame(round(summary(bmi8)$conf.int,2))
bmi8.hr2[,5] <- paste0(as.character(bmi8.hr2[,1])," (",bmi8.hr2[,3],"-",bmi8.hr2[,4],")")
bmi8.hr2[,6] <- rownames(bmi8.hr2)
bmi8.hr2[,6] <- substr(bmi8.hr2[,6],1,nchar(bmi8.hr2[,6])-2)
bmi8.hr2[,7] <- 1:5
bmi8.hr2 <- bmi8.hr2[(nrow(bmi8.hr2)-49):nrow(bmi8.hr2),]
names(bmi8.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
bmi8.hr2 <- dcast(bmi8.hr2,Variable~bmi8.hr2$Transition,value.var = "HR_95CI")
write.csv(bmi8.hr2, paste0(path_primary,"/dichotomous bmi8 model2 primary",".csv"))