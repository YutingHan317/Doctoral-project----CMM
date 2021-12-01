# 已剔除基线患有肿瘤和糖尿病，脑卒中和缺血性心脏病
# 调整变量删除职业 8/31
# 所有协变量均设置为trans-specific
# Body weight and fat

# 设置工作路径
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
project4.2y <- subset(tempfile,study_date != du_ep0001_date)

# 提取子集，增加运算速度;待调整好之后删除
# project4.2y <- subset(project4.2y,region_is_urban==0)

# 计算同一时间抵达多个阶段，用于之后的剔除同时抵达不同阶段的研究对象
# project4.2y$sametime[project4.2y$mltmbd_inc_date==project4.2y$first_cmd_inc_date & project4.2y$mltmbd_inc==1 & project4.2y$first_cmd_inc==1] <- T
# project4.2y$sametime[project4.2y$du_ep0001_date==project4.2y$first_cmd_inc_date & project4.2y$du_ep0001==1 & project4.2y$first_cmd_inc==1] <- T
# project4.2y$sametime[project4.2y$du_ep0001_date==project4.2y$mltmbd_inc_date & project4.2y$du_ep0001==1 & project4.2y$mltmbd_inc==1] <- T
# project4.2y$sametime[is.na(project4.2y$sametime)] <- F
# tr(project4.2y$sametime)
# sum(is.na(project4.2y$sametime))·
# table(project4.2y$sametime)

# 时间换算为年龄（岁）
project4.2y$study_date <- as.numeric(project4.2y$study_date)
project4.2y$first_cmd_inc_date <- as.numeric(project4.2y$first_cmd_inc_date1)
project4.2y$mltmbd_inc_date <- as.numeric(project4.2y$mltmbd_inc_date1)
project4.2y$du_ep0001_date <- as.numeric(project4.2y$du_ep0001_date)
project4.2y$dob_anon <- as.numeric(project4.2y$dob_anon)

project4.2y$first_cmd_inc <- as.numeric(project4.2y$first_cmd_inc1)
project4.2y$mltmbd_inc <- as.numeric(project4.2y$mltmbd_inc1)

# 一、某些研究对象首次心血管代谢性疾病发生于2017年12月31日，在上述基础上加上0.5天
attach(project4.2y)
project4.2y$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4.2y$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5
project4.2y$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4.2y$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5 
detach(project4.2y)


# 计算年龄
project4.2y[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")] <- (project4.2y[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")]-project4.2y$dob_anon)/365.25
summary(project4.2y[, c("dob_anon","study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")])
class(project4.2y$dob_anon)

# 剔除随访前两年发病或死亡的研究对象
project4.2y <- transform(project4.2y,fcmd_duration = first_cmd_inc_date - study_date, death_duration = du_ep0001_date - study_date)
summary(project4.2y[, c("fcmd_duration","death_duration")])
project4.2y <- subset(project4.2y,fcmd_duration>=2 & death_duration>=2)

# # # gen original database
# 选取需要的变量进行分析，避免数据库过大
covariates <- c("studyid","age_strata","region_code","region_is_urban","is_female","highest_education","occupation",
                "marital_status_2groups","household_income","parental_fh_3groups", "menopause_2groups",
                "age_3groups","region_is_urban", "education_3groups")
phrases <- c("study_date","first_cmd_inc","first_cmd_inc_date","mltmbd_inc","mltmbd_inc_date","du_ep0001","du_ep0001_date")
lifestyles <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","healthy_score_5groups","healthy_score",
                "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_obesity","risky_PA","risky_score_5groups","risky_score")
varlist <- c(covariates,phrases,lifestyles)

# # 数据库中存在不同阶段记录时间相同的个体，生成对应变量指代其类型 
attach(project4.2y)
project4.2y$sametime[first_cmd_inc==1 & du_ep0001 == 1 & du_ep0001_date==first_cmd_inc_date] <- 1      # 死于首次CMD
project4.2y$sametime[first_cmd_inc==1 & mltmbd_inc == 1 & mltmbd_inc_date==first_cmd_inc_date] <- 2      # 首次心血管代谢性疾病时间与共病记录日期相同
project4.2y$sametime[mltmbd_inc == 1 & du_ep0001 == 1 & mltmbd_inc_date==du_ep0001_date] <- 3      # 死于首次发生共病
project4.2y$sametime[first_cmd_inc == 1 &  mltmbd_inc == 1 & du_ep0001 == 1 & mltmbd_inc_date==du_ep0001_date & first_cmd_inc_date==mltmbd_inc_date] <- 4      # 首次CMD、共病和死亡日期相同
project4.2y$sametime[is.na(project4.2y$sametime)] <- 0
table(project4.2y$sametime)
detach(project4.2y)

# # 对时间相同的不同阶段进行处理
# 死于首次CMD
project4.2y$first_cmd_inc_date[project4.2y$sametime==1] <- project4.2y$du_ep0001_date[project4.2y$sametime==1] - 0.5/365.25
# 首次心血管代谢性疾病时间与共病记录日期相同
project4.2y$first_cmd_inc_date[project4.2y$sametime==2] <- project4.2y$mltmbd_inc_date[project4.2y$sametime==2] - 0.5/365.25
# 死于首次发生共病
project4.2y$mltmbd_inc_date[project4.2y$sametime==3] <- project4.2y$du_ep0001_date[project4.2y$sametime==3] - 0.5/365.25
# 首次CMD、共病和死亡日期相同
project4.2y$mltmbd_inc_date[project4.2y$sametime==4] <- project4.2y$du_ep0001_date[project4.2y$sametime==4] - 0.5/365.25
project4.2y$first_cmd_inc_date[project4.2y$sametime==4] <- project4.2y$mltmbd_inc_date[project4.2y$sametime==4] - 0.5/365.25


# 仅保留需要的变量
mltstate.origin.2y <- subset(project4.2y, select = varlist)  
# 生成代表进入研究的阶段
mltstate.origin.2y$enrollment <- 1

# 设置路径
tmat <- transMat(x = list(c(2, 4), c(3,4), c(4), c()), names = c("healthy", "fcmd", "mltmbd", "Death"))

# Wide to long
mltstate.long.2y <- msprep(data = mltstate.origin.2y, trans = tmat,
                        time = c(NA, "first_cmd_inc_date", "mltmbd_inc_date","du_ep0001_date"),
                        status = c(NA, "first_cmd_inc", "mltmbd_inc", "du_ep0001"),
                        keep = c(covariates,lifestyles),
                        start = list(state = mltstate.origin.2y$enrollment,time = mltstate.origin.2y$study_date))

# Transitions
events(mltstate.long.2y)
sum(mltstate.long.2y$time==0)
events.primary <- events(mltstate.long.2y)
freq_trans <- data.frame(events.primary$Frequencies)
freq_trans <- dcast(freq_trans,from~to,value.var = "Freq")
prop_trans <- data.frame(events.primary$Proportions)
prop_trans <- dcast(prop_trans,from~to,value.var = "Freq")
for (i in 2:5){
    freq_trans[,i] <- paste(freq_trans[,i]," (",round(prop_trans[,i],3),")")
}
write.csv(freq_trans, paste("3. Result/multistate/primary analyses/transitions",".csv",sep = ""))

# Set lifestyle score as category
mltstate.long.2y$hscore <- mltstate.long.2y$healthy_score
mltstate.long.2y$hscore.cat <- as.factor(mltstate.long.2y$healthy_score_5groups)

mltstate.long.2y$rscore <- mltstate.long.2y$risky_score
mltstate.long.2y$rscore.cat <- as.factor(mltstate.long.2y$risky_score_5groups)

# Transition-specific covariates
long_covariates <- c("is_female","highest_education","occupation","marital_status_2groups","parental_fh_3groups","menopause_2groups",
                     "age_3groups","region_is_urban", "education_3groups")
long_lf <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","hscore","hscore.cat",
             "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_obesity","risky_PA","rscore","rscore.cat")
mltstate.full.2y <- expand.covs(mltstate.long.2y, c(long_covariates,long_lf), longnames = FALSE)  # 均为哑变量
head(mltstate.full.2y)
names(mltstate.full.2y)

# Drop observation of which time = 0 
message(sum(mltstate.full.2y$time==0)," records was droped")
mltstate.final.2y <- subset(mltstate.full.2y,time != 0)

################################################################################################################################################
###################################                                                           ##################################################
###################################                    primary analyses                       ##################################################
###################################                                                           ##################################################
################################################################################################################################################
dir.create("3. Result/multistate/sensitivity analyses 2y")
path_2y <- paste0("3. Result/multistate/sensitivity analyses 2y","/",Sys.Date())
dir.create(path_2y)

# # # # dichotomous single lifestyle;model 1 和 model2 差别很小，下面仅罗列了model2;家族史变量调一个和两个没有大的区别（05-24）
single2.2y <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 +
                     risky_obesity.1 + risky_obesity.2  + risky_obesity.3 + risky_obesity.4 + risky_obesity.5,
                 data = mltstate.final.2y, method = "breslow")
single.hr2.2y <- data.frame(round(summary(single2.2y)$conf.int,2))
single.hr2.2y[,5] <- paste0(sprintf(single.hr2.2y[,1],fmt="%.2f")," (",sprintf(single.hr2.2y[,3],fmt="%.2f"),"-",sprintf(single.hr2.2y[,4],fmt="%.2f"),")")
single.hr2.2y[,6] <- rownames(single.hr2.2y)
single.hr2.2y[,6] <- substr(single.hr2.2y[,6],1,nchar(single.hr2.2y[,6])-2)
single.hr2.2y[,7] <- 1:5
single.hr2.2y <- single.hr2.2y[(nrow(single.hr2.2y)-29):nrow(single.hr2.2y),]
names(single.hr2.2y)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
single.hr2.2y <- dcast(single.hr2.2y,Variable~single.hr2.2y$Transition,value.var = "HR_95CI")
write.csv(single.hr2.2y, paste0(path_2y,"/dichotomous single model2 sensitivity 2y",".csv"))

### Calculate HR for lifestyle score (category) 
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scorecat2.2y <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                       rscore.cat1.1 + rscore.cat2.1 + rscore.cat3.1 + rscore.cat4.1 + 
                       rscore.cat1.2 + rscore.cat2.2 + rscore.cat3.2 + rscore.cat4.2 + 
                       rscore.cat1.3 + rscore.cat2.3 + rscore.cat3.3 + rscore.cat4.3 + 
                       rscore.cat1.4 + rscore.cat2.4 + rscore.cat3.4 + rscore.cat4.4 + 
                       rscore.cat1.5 + rscore.cat2.5 + rscore.cat3.5 + rscore.cat4.5  ,
                   data = mltstate.final.2y, method = "breslow")
scorecat.hr2.2y <- data.frame(round(summary(scorecat2.2y)$conf.int,2))
scorecat.hr2.2y[,5] <- paste0(sprintf(scorecat.hr2.2y[,1],fmt="%.2f")," (",sprintf(scorecat.hr2.2y[,3],fmt="%.2f"),"-",sprintf(scorecat.hr2.2y[,4],fmt="%.2f"),")")
scorecat.hr2.2y[,6] <- rownames(scorecat.hr2.2y)
scorecat.hr2.2y[,6] <- substr(scorecat.hr2.2y[,6],1,nchar(scorecat.hr2.2y[,6])-2)
scorecat.hr2.2y[,7] <- str_sub(rownames(scorecat.hr2.2y),-1)
scorecat.hr2.2y <- scorecat.hr2.2y[(nrow(scorecat.hr2.2y)-19):nrow(scorecat.hr2.2y),]
names(scorecat.hr2.2y)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
scorecat.hr2.2y <- dcast(scorecat.hr2.2y,Variable~scorecat.hr2.2y$Transition,value.var = "HR_95CI")
write.csv(scorecat.hr2.2y, paste0(path_2y,"/category score model2 sensitivity 2y",".csv"))

### Calculate HR for lifestyle score (ordinal) （各阶段协变量系数相同）
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scoreord2.2y <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                       rscore.1 + rscore.2 + rscore.3 + rscore.4 + rscore.5,
                   data = mltstate.final.2y, method = "breslow")
scoreord.hr2.2y <- data.frame(round(summary(scoreord2.2y)$conf.int,2))
scoreord.hr2.2y[,5] <- paste0(sprintf(scoreord.hr2.2y[,1],fmt="%.2f")," (",sprintf(scoreord.hr2.2y[,3],fmt="%.2f"),"-",sprintf(scoreord.hr2.2y[,4],fmt="%.2f"),")")
write.csv(scoreord.hr2.2y, paste0(path_2y,"/ordinal score model2 sensitivity 2y",".csv"))

