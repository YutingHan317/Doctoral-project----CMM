# 已剔除基线患有肿瘤和1型糖尿病，bmi缺失者;未剔除患有冠心病、脑卒中、2型糖尿病
# 本部分分析已经尝试了调整时间和从不同阶段进入没有大的区别（仅有部分单一生活方式指标发生改变）

dir.create("3. Result/multistate/sensitivity analyses diff enter stage")
rm(list=ls())

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

path_stage <- paste0("3. Result/multistate/sensitivity analyses diff enter stage/",Sys.Date())
dir.create(path_stage)

# 载入数据
load("./1. Database/project4_full.rdata")

tempfile.diff <- project4_full
project4.diff <- subset(tempfile.diff,study_date != du_ep0001_date)

# 提取子集，增加运算速度;待调整好之后删除
# project4.diff <- subset(project4.diff,region_is_urban==0)

# 计算同一时间抵达多个阶段，用于之后的剔除同时抵达不同阶段的研究对象
# project4.diff$sametime[project4.diff$mltmbd_inc_date==project4.diff$first_cmd_inc_date & project4.diff$mltmbd_inc==1 & project4.diff$first_cmd_inc==1] <- T
# project4.diff$sametime[project4.diff$du_ep0001_date==project4.diff$first_cmd_inc_date & project4.diff$du_ep0001==1 & project4.diff$first_cmd_inc==1] <- T
# project4.diff$sametime[project4.diff$du_ep0001_date==project4.diff$mltmbd_inc_date & project4.diff$du_ep0001==1 & project4.diff$mltmbd_inc==1] <- T
# project4.diff$sametime[is.na(project4.diff$sametime)] <- F
# tr(project4.diff$sametime)
# sum(is.na(project4.diff$sametime))·
# table(project4.diff$sametime)

# 时间换算为年龄（岁）
project4.diff$study_date <- as.numeric(project4.diff$study_date)
project4.diff$first_cmd_inc_date <- as.numeric(project4.diff$first_cmd_inc_date)
project4.diff$mltmbd_inc_date <- as.numeric(project4.diff$mltmbd_inc_date)
project4.diff$du_ep0001_date <- as.numeric(project4.diff$du_ep0001_date)
project4.diff$dob_anon <- as.numeric(project4.diff$dob_anon)

# 一、某些研究对象首次心血管代谢性疾病发生于2017年12月31日，在上述基础上加上0.5天
attach(project4.diff)
project4.diff$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4.diff$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5
project4.diff$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4.diff$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5 
detach(project4.diff)

# 计算年龄
project4.diff[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")] <- (project4.diff[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")]-project4.diff$dob_anon)/365.25
summary(project4.diff[, c("dob_anon","study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")])
class(project4.diff$dob_anon)

# # # gen original database
# 选取需要的变量进行分析，避免数据库过大
covariates <- c("studyid","age_strata","region_code","region_is_urban","is_female","highest_education","occupation",
                "marital_status_2groups","household_income","parental_fh_3groups", "chronic_fh", "mltmbd_fh_3groups", "menopause_2groups",
                "age_3groups","region_is_urban", "education_3groups")
phrases <- c("study_date","first_cmd_inc","first_cmd_inc_date","mltmbd_inc","mltmbd_inc_date","du_ep0001","du_ep0001_date")
lifestyles <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","healthy_score_5groups","healthy_score",
                "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_PA","risky_score_5groups","risky_score")
varlist <- c(covariates,phrases,lifestyles,"d_number_base")

# # 数据库中存在不同阶段记录时间相同的个体，生成对应变量指代其类型 
attach(project4.diff)
project4.diff$sametime[first_cmd_inc==1 & du_ep0001 == 1 & du_ep0001_date==first_cmd_inc_date] <- 1      # 死于首次CMD
project4.diff$sametime[first_cmd_inc==1 & mltmbd_inc == 1 & mltmbd_inc_date==first_cmd_inc_date] <- 2      # 首次心血管代谢性疾病时间与共病记录日期相同
project4.diff$sametime[mltmbd_inc == 1 & du_ep0001 == 1 & mltmbd_inc_date==du_ep0001_date] <- 3      # 死于首次发生共病
project4.diff$sametime[first_cmd_inc == 1 &  mltmbd_inc == 1 & du_ep0001 == 1 & mltmbd_inc_date==du_ep0001_date & first_cmd_inc_date==mltmbd_inc_date] <- 4      # 首次CMD、共病和死亡日期相同
project4.diff$sametime[is.na(project4.diff$sametime)] <- 0
table(project4.diff$sametime)
detach(project4.diff)

# # 部分基线就患基础疾病的研究对象：首发疾病状态为1，首发疾病时间为入组时间（待定：发病时间）
project4.diff$first_cmd_inc[project4.diff$d_number_base==1] <- 1
project4.diff$first_cmd_inc_date[project4.diff$d_number_base==1] <- project4.diff$study_date[project4.diff$d_number_base==1]
project4.diff$mltmbd_inc[project4.diff$d_number_base>1] <- 1
project4.diff$mltmbd_inc_date[project4.diff$d_number_base>1] <- project4.diff$study_date[project4.diff$d_number_base>1]

# # 对时间相同的不同阶段进行处理
# 死于首次CMD
project4.diff$first_cmd_inc_date[project4.diff$sametime==1] <- project4.diff$du_ep0001_date[project4.diff$sametime==1] - 0.5/365.25
# 首次心血管代谢性疾病时间与共病记录日期相同
project4.diff$first_cmd_inc_date[project4.diff$sametime==2] <- project4.diff$mltmbd_inc_date[project4.diff$sametime==2] - 0.5/365.25
# 死于首次发生共病
project4.diff$mltmbd_inc_date[project4.diff$sametime==3] <- project4.diff$du_ep0001_date[project4.diff$sametime==3] - 0.5/365.25
# 首次CMD、共病和死亡日期相同
project4.diff$mltmbd_inc_date[project4.diff$sametime==4] <- project4.diff$du_ep0001_date[project4.diff$sametime==4] - 0.5/365.25
project4.diff$first_cmd_inc_date[project4.diff$sametime==4] <- project4.diff$mltmbd_inc_date[project4.diff$sametime==4] - 0.5/365.25

# 仅保留需要的变量
mltstate.origin.diff <- subset(project4.diff, select = varlist)  
# 生成代表进入研究的阶段
mltstate.origin.diff$enrollment[mltstate.origin.diff$d_number_base==0] <- 1
mltstate.origin.diff$enrollment[mltstate.origin.diff$d_number_base==1] <- 2
mltstate.origin.diff$enrollment[mltstate.origin.diff$d_number_base> 1] <- 3
table(mltstate.origin.diff$enrollment)

# Wide to long
mltstate.long.diff <- msprep(data = mltstate.origin.diff, trans = tmat,
                        time = c(NA, "first_cmd_inc_date", "mltmbd_inc_date","du_ep0001_date"),
                        status = c(NA, "first_cmd_inc", "mltmbd_inc", "du_ep0001"),
                        keep = c(covariates,lifestyles),
                        start = list(state = mltstate.origin.diff$enrollment,time = mltstate.origin.diff$study_date))
events(mltstate.long.diff)
sum(mltstate.long.diff$time==0)


# Set lifestyle score as category
mltstate.long.diff$hscore <- mltstate.long.diff$healthy_score
mltstate.long.diff$hscore.cat <- as.factor(mltstate.long.diff$healthy_score_5groups)

mltstate.long.diff$rscore <- mltstate.long.diff$risky_score
mltstate.long.diff$rscore.cat <- as.factor(mltstate.long.diff$risky_score_5groups)

# Transition-specific covariates
long_covariates <- c("is_female","highest_education","occupation","marital_status_2groups","mltmbd_fh_3groups","menopause_2groups",
                     "age_3groups","region_is_urban", "education_3groups")
long_lf <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","hscore","hscore.cat",
             "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_PA","rscore","rscore.cat")
mltstate.full.diff <- expand.covs(mltstate.long.diff, c(long_covariates,long_lf), longnames = FALSE)  # 均为哑变量
head(mltstate.full.diff)
names(mltstate.full.diff)

# Drop observation of which time = 0 
message(sum(mltstate.full.diff$time==0)," records was droped")
mltstate.final.diff <- subset(mltstate.full.diff,time != 0)

###################################################     分析     ######################################################
# # # # dichotomous single lifestyle;model 1 和 model2 差别很小，下面仅罗列了model2;家族史变量调一个和两个没有大的区别（05-24）
single2.diff <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                     as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                     as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                     as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                     as.factor(mltmbd_fh_3groups.1) + as.factor(mltmbd_fh_3groups.2) + as.factor(mltmbd_fh_3groups.3) + as.factor(mltmbd_fh_3groups.4) + as.factor(mltmbd_fh_3groups.5) + 
                     risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                     risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                     risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                     risky_bmi.1 + risky_bmi.2  + risky_bmi.3 + risky_bmi.4 + risky_bmi.5 +
                     risky_WC.1 + risky_WC.2  + risky_WC.3 + risky_WC.4 + risky_WC.5 +
                     risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5,
                 data = mltstate.final.diff, method = "breslow")
singlediff.hr2 <- data.frame(round(summary(single2.diff)$conf.int,2))
singlediff.hr2[,5] <- paste0(as.character(singlediff.hr2[,1])," (",singlediff.hr2[,3],"-",singlediff.hr2[,4],")")
singlediff.hr2[,6] <- rownames(singlediff.hr2)
singlediff.hr2[,6] <- substr(singlediff.hr2[,6],1,nchar(singlediff.hr2[,6])-2)
singlediff.hr2[,7] <- 1:5
singlediff.hr2 <- singlediff.hr2[(nrow(singlediff.hr2)-29):nrow(singlediff.hr2),]
names(singlediff.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
singlediff.hr2 <- dcast(singlediff.hr2,Variable~singlediff.hr2$Transition,value.var = "HR_95CI")
write.csv(singlediff.hr2, paste0(path_stage,"/dichotomous single",Sys.Date(),".csv",sep = " "))

### Calculate HR for lifestyle score (category) 
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scorecat2.diff <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(mltmbd_fh_3groups.1) + as.factor(mltmbd_fh_3groups.2) + as.factor(mltmbd_fh_3groups.3) + as.factor(mltmbd_fh_3groups.4) + as.factor(mltmbd_fh_3groups.5) + 
                       rscore.cat1.1 + rscore.cat2.1 + rscore.cat3.1 + rscore.cat4.1 + 
                       rscore.cat1.2 + rscore.cat2.2 + rscore.cat3.2 + rscore.cat4.2 + 
                       rscore.cat1.3 + rscore.cat2.3 + rscore.cat3.3 + rscore.cat4.3 + 
                       rscore.cat1.4 + rscore.cat2.4 + rscore.cat3.4 + rscore.cat4.4 + 
                       rscore.cat1.5 + rscore.cat2.5 + rscore.cat3.5 + rscore.cat4.5  ,
                   data = mltstate.final.diff, method = "breslow")
scorecatdiff.hr2 <- data.frame(round(summary(scorecat2.diff)$conf.int,2))
scorecatdiff.hr2[,5] <- paste0(as.character(scorecatdiff.hr2[,1])," (",scorecatdiff.hr2[,3],"-",scorecatdiff.hr2[,4],")")
scorecatdiff.hr2[,6] <- rownames(scorecatdiff.hr2)
scorecatdiff.hr2[,6] <- substr(scorecatdiff.hr2[,6],1,nchar(scorecatdiff.hr2[,6])-2)
scorecatdiff.hr2[,7] <- str_sub(rownames(scorecatdiff.hr2),-1)
scorecatdiff.hr2 <- scorecatdiff.hr2[(nrow(scorecatdiff.hr2)-19):nrow(scorecatdiff.hr2),]
names(scorecatdiff.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
scorecatdiff.hr2 <- dcast(scorecatdiff.hr2,Variable~scorecatdiff.hr2$Transition,value.var = "HR_95CI")
write.csv(scorecatdiff.hr2, paste0(path_stage,"/category score",Sys.Date(),".csv",sep = " "))

### Calculate HR for lifestyle score (ordinal) （各阶段协变量系数相同）
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scoreord2.diff <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(mltmbd_fh_3groups.1) + as.factor(mltmbd_fh_3groups.2) + as.factor(mltmbd_fh_3groups.3) + as.factor(mltmbd_fh_3groups.4) + as.factor(mltmbd_fh_3groups.5) + 
                       rscore.1 + rscore.2 + rscore.3 + rscore.4 + rscore.5,
                   data = mltstate.final.diff, method = "breslow")
scoreorddiff.hr2 <- data.frame(round(summary(scoreord2.diff)$conf.int,2))
scoreorddiff.hr2[,5] <- paste0(as.character(scoreorddiff.hr2[,1])," (",scoreorddiff.hr2[,3],"-",scoreorddiff.hr2[,4],")")
write.csv(scoreorddiff.hr2, paste0(path_stage,"/ordinal score",Sys.Date(),".csv",sep = " "))



