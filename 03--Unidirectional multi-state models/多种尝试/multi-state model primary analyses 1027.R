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
                "marital_status_2groups","household_income","parental_fh_3groups","parental_fh_2groups", "chronic_fh", "menopause_2groups",
                "age_3groups","region_is_urban", "education_3groups","has_hypertension","has_hyperlipidemia")
phrases <- c("study_date","first_cmd_inc","first_cmd_inc_date","mltmbd_inc","mltmbd_inc_date","du_ep0001","du_ep0001_date")
lifestyles <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","healthy_score_5groups","healthy_score",
                "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_PA","risky_obesity","risky_score_5groups","risky_score_4groups","risky_score")
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
                        keep = c(covariates,lifestyles,"first_cmd_inc","mltmbd_inc"),
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
mltstate.long$risky_score_5groups[mltstate.long$risky_score_5groups==5] <- 4 
mltstate.long$rscore.cat <- as.factor(mltstate.long$risky_score_5groups)

mltstate.long$number_cmd <- 0
mltstate.long$number_cmd[mltstate.long$first_cmd_inc==1] <- 1
mltstate.long$number_cmd[mltstate.long$mltmbd_inc==1] <- 2
# Transition-specific covariates
long_covariates <- c("is_female","highest_education","occupation","marital_status_2groups","menopause_2groups","has_hyperlipidemia","number_cmd",
                     "age_3groups","region_is_urban", "education_3groups","has_hypertension","region_code","parental_fh_3groups","parental_fh_2groups")
long_lf <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","hscore","hscore.cat",
             "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_obesity","risky_PA","rscore","rscore.cat","risky_score_4groups","risky_score_5groups")
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
single.hr2$exp.coef. <- sprintf(single.hr2$exp.coef.,fmt = "%.2f")
single.hr2$lower..95 <- sprintf(single.hr2$lower..95,fmt = "%.2f")
single.hr2$upper..95 <- sprintf(single.hr2$upper..95,fmt = "%.2f")
#single.hr2[,5] <- paste0(sprintf(single.hr2[,1],fmt="%.2f")," (",sprintf(single.hr2[,3],fmt="%.2f"),"-",sprintf(single.hr2[,4],fmt="%.2f"),")")
#single.hr2[,6] <- rownames(single.hr2)
#single.hr2[,6] <- substr(single.hr2[,6],1,nchar(single.hr2[,6])-2)
#single.hr2[,7] <- 1:5
#single.hr2 <- single.hr2[(nrow(single.hr2)-29):nrow(single.hr2),]
#names(single.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
#single.hr2 <- dcast(single.hr2,Variable~single.hr2$Transition,value.var = "HR_95CI")
write.csv(single.hr2, paste0(path_primary,"/dichotomous single model2 primary",".csv"))

### Calculate HR for lifestyle score (category) 
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scorecat2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                       rscore.cat1.1 + rscore.cat2.1 + rscore.cat3.1 + 
                       rscore.cat1.2 + rscore.cat2.2 + rscore.cat3.2 + 
                       rscore.cat1.3 + rscore.cat2.3 + rscore.cat3.3 + 
                       rscore.cat1.4 + rscore.cat2.4 + rscore.cat3.4 + 
                       rscore.cat1.5 + rscore.cat2.5 + rscore.cat3.5  ,
                   data = mltstate.final, method = "breslow")
scorecat.hr2 <- data.frame(round(summary(scorecat2)$conf.int,2))
scorecat.hr2$exp.coef. <- sprintf(scorecat.hr2$exp.coef.,fmt = "%.2f")
scorecat.hr2$lower..95 <- sprintf(scorecat.hr2$lower..95,fmt = "%.2f")
scorecat.hr2$upper..95 <- sprintf(scorecat.hr2$upper..95,fmt = "%.2f")
#scorecat.hr2[,5] <- paste0(sprintf(scorecat.hr2[,1],fmt="%.2f")," (",sprintf(scorecat.hr2[,3],fmt="%.2f"),"-",sprintf(scorecat.hr2[,4],fmt="%.2f"),")")
#scorecat.hr2[,6] <- rownames(scorecat.hr2)
#scorecat.hr2[,6] <- substr(scorecat.hr2[,6],1,nchar(scorecat.hr2[,6])-2)
#scorecat.hr2[,7] <- str_sub(rownames(scorecat.hr2),-1)
#scorecat.hr2 <- scorecat.hr2[(nrow(scorecat.hr2)-19):nrow(scorecat.hr2),]
#names(scorecat.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
#scorecat.hr2 <- dcast(scorecat.hr2,Variable~scorecat.hr2$Transition,value.var = "HR_95CI")
write.csv(scorecat.hr2, paste0(path_primary,"/category score model2 primary",".csv"))

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
scoreord.hr2$exp.coef. <- sprintf(scoreord.hr2$exp.coef.,fmt = "%.2f")
scoreord.hr2$lower..95 <- sprintf(scoreord.hr2$lower..95,fmt = "%.2f")
scoreord.hr2$upper..95 <- sprintf(scoreord.hr2$upper..95,fmt = "%.2f")
#scoreord.hr2[,5] <- paste0(sprintf(scoreord.hr2[,1],fmt="%.2f")," (",sprintf(scoreord.hr2[,3],fmt="%.2f"),"-",sprintf(scoreord.hr2[,4],fmt="%.2f"),")")
write.csv(scoreord.hr2, paste0(path_primary,"/ordinal score model2 primary",".csv"))

#### Calculate HR for lifestyle score (ordinal) 考虑经过首发疾病和共病的死亡风险
## Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
#scoreord2.cmd <- coxph(Surv(Tstart, Tstop, status) ~ strata(to)+ strata(region_code) + strata(age_strata) +
#                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
#                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
#                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
#                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
#                       rscore.1 + rscore.2 + rscore.3 + rscore.4 + rscore.5 + number_cmd,
#                   data = mltstate.final, method = "breslow")
#scoreord.hr2.cmd <- data.frame(round(summary(scoreord2.cmd)$conf.int,2))
#scoreord.hr2.cmd$exp.coef. <- sprintf(scoreord.hr2.cmd$exp.coef.,fmt = "%.2f")
#scoreord.hr2.cmd$lower..95 <- sprintf(scoreord.hr2.cmd$lower..95,fmt = "%.2f")
#scoreord.hr2.cmd$upper..95 <- sprintf(scoreord.hr2.cmd$upper..95,fmt = "%.2f")
##scoreord.hr2.cmd[,5] <- paste0(sprintf(scoreord.hr2.cmd[,1],fmt="%.2f")," (",sprintf(scoreord.hr2.cmd[,3],fmt="%.2f"),"-",sprintf(scoreord.hr2.cmd[,4],fmt="%.2f"),")")
#write.csv(scoreord.hr2.cmd, paste0(path_primary,"/ordinal score model2 primary",".csv"))

### No. of cases
ddply(mltstate.final,c("risky_smoking","trans"),summarise,a=sum(status))
ddply(mltstate.final,c("risky_alcohol","trans"),summarise,a=sum(status))
ddply(mltstate.final,c("risky_diet","trans"),summarise,a=sum(status))
ddply(mltstate.final,c("risky_obesity","trans"),summarise,a=sum(status))
ddply(mltstate.final,c("risky_PA","trans"),summarise,a=sum(status))
ddply(mltstate.final,c("trans","rscore.cat"),summarise,a=sum(status))
# for subgroup analysis
ddply(mltstate.final,c("trans","age_3groups"),summarise,a=sum(status))
ddply(mltstate.final,c("trans","is_female"),summarise,a=sum(status))
ddply(mltstate.final,c("trans","region_is_urban"),summarise,a=sum(status))
ddply(mltstate.final,c("trans","parental_fh_2groups"),summarise,a=sum(status))
ddply(mltstate.final,c("trans","has_hypertension"),summarise,a=sum(status))
# Incidence rate
ddply(mltstate.final,"trans",summarise,a=sum(status)/sum(time))

################################################################################################################################################
###################################                                                           ##################################################
###################################                    Subgroup analyses                      ##################################################
###################################                                                           ##################################################
################################################################################################################################################
path <- paste0("3. Result/multistate/Subgroup analyses/",Sys.Date())
dir.create(path)
##########################################          无交互项基础模型        ##########################################
# scorecat2

##########################################          sex        ##########################################
# men
mltstate.men <- subset(mltstate.final, is_female == 0)
mltstate.women <- subset(mltstate.final, is_female == 1)
scoreord.men <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                          as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                          as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                          as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                          rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                          rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                          rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                      data = mltstate.men, method = "breslow")
scoreord.men.hr <- data.frame(round(summary(scoreord.men)$conf.int,2))
scoreord.men.hr[,5] <- paste0(sprintf(scoreord.men.hr[,1],fmt="%.2f")," (",sprintf(scoreord.men.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.men.hr[,4],fmt="%.2f"),")")
scoreord.men.hr[,6] <- rownames(scoreord.men.hr)
scoreord.men.hr[,6] <- substr(scoreord.men.hr[,6],1,nchar(scoreord.men.hr[,6])-2)
scoreord.men.hr[,7] <- str_sub(rownames(scoreord.men.hr),-1)
scoreord.men.hr[,8] <- "men"
scoreord.men.hr <- scoreord.men.hr[(nrow(scoreord.men.hr)-4):nrow(scoreord.men.hr),]
names(scoreord.men.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.men.hr <- dcast(scoreord.men.hr,Subgroups + Variable~scoreord.men.hr$Transition,value.var = "HR_95CI")
# 增加新行代表分组变量
new_row <- c("Sex","","","","","","")
scoreord.men.hr <- rbind(new_row,scoreord.men.hr)

# women
scoreord.women <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                            as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                            as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                            as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                            as.factor(menopause_2groups.1) + as.factor(menopause_2groups.2) + as.factor(menopause_2groups.3) + as.factor(menopause_2groups.4) + as.factor(menopause_2groups.5) + 
                            rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                            rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                            rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                        data = mltstate.women, method = "breslow")
scoreord.women.hr <- data.frame(round(summary(scoreord.women)$conf.int,2))
scoreord.women.hr[,5] <- paste0(sprintf(scoreord.women.hr[,1],fmt="%.2f")," (",sprintf(scoreord.women.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.women.hr[,4],fmt="%.2f"),")")
scoreord.women.hr[,6] <- rownames(scoreord.women.hr)
scoreord.women.hr[,6] <- substr(scoreord.women.hr[,6],1,nchar(scoreord.women.hr[,6])-2)
scoreord.women.hr[,7] <- str_sub(rownames(scoreord.women.hr),-1)
scoreord.women.hr[,8] <- "women"
scoreord.women.hr <- scoreord.women.hr[(nrow(scoreord.women.hr)-4):nrow(scoreord.women.hr),]
names(scoreord.women.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.women.hr <- dcast(scoreord.women.hr,Subgroups + Variable~scoreord.women.hr$Transition,value.var = "HR_95CI")


# interaction
score.vector <- paste0("risky_score_5groups",".",1:5)
sex.vector <- paste0("is_female",".",1:5)
scoreord.sexlist <- list()
scoreord.sex.interaction <- data.frame()
for (i in 1:5){
    
    scoreord.sexlist[[i]] <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                                       rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                                       rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                                       rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5 + 
                                       mltstate.final[,sex.vector[i]]:as.factor(mltstate.final[,score.vector[i]]),
                                   data = mltstate.final, method = "breslow")
    scoreord.sex.hr <- data.frame(round(summary(scoreord.sexlist[[i]])$conf.int,2))
    scoreord.sex.hr <- scoreord.sex.hr[nrow(scoreord.sex.hr),]
    interaction <- lrtest(scorecat2,scoreord.sexlist[[i]])
    scoreord.sex.hr[1,5] <- interaction[2,5]
    scoreord.sex.hr[,6] <- i
    names(scoreord.sex.hr)[c(5,6)] <- c("pvalue","transitions")
    scoreord.sex.interaction <- rbind(scoreord.sex.interaction,scoreord.sex.hr)
    print(i)
    
}



##########################################          age gruops        ##########################################
mltstate.lt50 <- subset(mltstate.final, age_3groups == 0)
mltstate.5059 <- subset(mltstate.final, age_3groups == 1)
mltstate.ge60 <- subset(mltstate.final, age_3groups == 2)

# aged < 50y
scoreord.lt50 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                           as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                           as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                           as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                           as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                           rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                           rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                           rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                       data = mltstate.lt50, method = "breslow")
scoreord.lt50.hr <- data.frame(round(summary(scoreord.lt50)$conf.int,2))
scoreord.lt50.hr[,5] <- paste0(sprintf(scoreord.lt50.hr[,1],fmt="%.2f")," (",sprintf(scoreord.lt50.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.lt50.hr[,4],fmt="%.2f"),")")
scoreord.lt50.hr[,6] <- rownames(scoreord.lt50.hr)
scoreord.lt50.hr[,6] <- substr(scoreord.lt50.hr[,6],1,nchar(scoreord.lt50.hr[,6])-2)
scoreord.lt50.hr[,7] <- str_sub(rownames(scoreord.lt50.hr),-1)
scoreord.lt50.hr[,8] <- "agedlt50"
scoreord.lt50.hr <- scoreord.lt50.hr[(nrow(scoreord.lt50.hr)-4):nrow(scoreord.lt50.hr),]
names(scoreord.lt50.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.lt50.hr <- dcast(scoreord.lt50.hr,Subgroups + Variable~scoreord.lt50.hr$Transition,value.var = "HR_95CI")
# 增加新行代表分组变量
new_row <- c("Age","","","","","","")
scoreord.lt50.hr <- rbind(new_row,scoreord.lt50.hr)


# aged 50-59y
scoreord.5059 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                           as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                           as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                           as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                           as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                           rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                           rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                           rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                       data = mltstate.5059, method = "breslow")
scoreord.5059.hr <- data.frame(round(summary(scoreord.5059)$conf.int,2))
scoreord.5059.hr[,5] <- paste0(sprintf(scoreord.5059.hr[,1],fmt="%.2f")," (",sprintf(scoreord.5059.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.5059.hr[,4],fmt="%.2f"),")")
scoreord.5059.hr[,6] <- rownames(scoreord.5059.hr)
scoreord.5059.hr[,6] <- substr(scoreord.5059.hr[,6],1,nchar(scoreord.5059.hr[,6])-2)
scoreord.5059.hr[,7] <- str_sub(rownames(scoreord.5059.hr),-1)
scoreord.5059.hr[,8] <- "aged5059"
scoreord.5059.hr <- scoreord.5059.hr[(nrow(scoreord.5059.hr)-4):nrow(scoreord.5059.hr),]
names(scoreord.5059.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.5059.hr <- dcast(scoreord.5059.hr,Subgroups + Variable~scoreord.5059.hr$Transition,value.var = "HR_95CI")

# aged >=60y
scoreord.ge60 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                           as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                           as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                           as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                           as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                           rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                           rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                           rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                       data = mltstate.ge60, method = "breslow")
scoreord.ge60.hr <- data.frame(round(summary(scoreord.ge60)$conf.int,2))
scoreord.ge60.hr[,5] <- paste0(sprintf(scoreord.ge60.hr[,1],fmt="%.2f")," (",sprintf(scoreord.ge60.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.ge60.hr[,4],fmt="%.2f"),")")
scoreord.ge60.hr[,6] <- rownames(scoreord.ge60.hr)
scoreord.ge60.hr[,6] <- substr(scoreord.ge60.hr[,6],1,nchar(scoreord.ge60.hr[,6])-2)
scoreord.ge60.hr[,7] <- str_sub(rownames(scoreord.ge60.hr),-1)
scoreord.ge60.hr[,8] <- "agedge60"
scoreord.ge60.hr <- scoreord.ge60.hr[(nrow(scoreord.ge60.hr)-4):nrow(scoreord.ge60.hr),]
names(scoreord.ge60.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.ge60.hr <- dcast(scoreord.ge60.hr,Subgroups + Variable~scoreord.ge60.hr$Transition,value.var = "HR_95CI")


# interaction
age.vector <- paste0("age_3groups",".",1:5)
scoreord.agelist <- list()
scoreord.age.interaction <- data.frame()
for (i in 1:5){
    
    scoreord.agelist[[i]] <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                                       rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                                       rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                                       rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5 +
                                       as.factor(mltstate.final[,age.vector[i]]):as.factor(mltstate.final[,score.vector[i]]),
                                   data = mltstate.final, method = "breslow")
    scoreord.age.hr <- data.frame(round(summary(scoreord.agelist[[i]])$conf.int,2))
    scoreord.age.hr <- scoreord.age.hr[nrow(scoreord.age.hr),]
    interaction <- lrtest(scorecat2,scoreord.agelist[[i]])
    scoreord.age.hr[1,5] <- interaction[2,5]
    scoreord.age.hr[,6] <- i
    names(scoreord.age.hr)[c(5,6)] <- c("pvalue","transitions")
    scoreord.age.interaction <- rbind(scoreord.age.interaction,scoreord.age.hr)
    print(i)
    
}


##########################################          residence area       ##########################################
mltstate.rural <- subset(mltstate.final, region_is_urban == 0)
mltstate.urban <- subset(mltstate.final, region_is_urban == 1)

# rural
scoreord.rural <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                            as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                            as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                            as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                            as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                            rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                            rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                            rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                        data = mltstate.rural, method = "breslow")
scoreord.rural.hr <- data.frame(round(summary(scoreord.rural)$conf.int,2))
scoreord.rural.hr[,5] <- paste0(sprintf(scoreord.rural.hr[,1],fmt="%.2f")," (",sprintf(scoreord.rural.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.rural.hr[,4],fmt="%.2f"),")")
scoreord.rural.hr[,6] <- rownames(scoreord.rural.hr)
scoreord.rural.hr[,6] <- substr(scoreord.rural.hr[,6],1,nchar(scoreord.rural.hr[,6])-2)
scoreord.rural.hr[,7] <- str_sub(rownames(scoreord.rural.hr),-1)
scoreord.rural.hr[,8] <- "rural"
scoreord.rural.hr <- scoreord.rural.hr[(nrow(scoreord.rural.hr)-4):nrow(scoreord.rural.hr),]
names(scoreord.rural.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.rural.hr <- dcast(scoreord.rural.hr,Subgroups + Variable~scoreord.rural.hr$Transition,value.var = "HR_95CI")
# 增加新行代表分组变量
new_row <- c("Residence area","","","","","","")
scoreord.rural.hr <- rbind(new_row,scoreord.rural.hr)


# urban
scoreord.urban <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                            as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                            as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                            as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                            as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                            rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                            rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                            rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                        data = mltstate.urban, method = "breslow")
scoreord.urban.hr <- data.frame(round(summary(scoreord.urban)$conf.int,2))
scoreord.urban.hr[,5] <- paste0(sprintf(scoreord.urban.hr[,1],fmt="%.2f")," (",sprintf(scoreord.urban.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.urban.hr[,4],fmt="%.2f"),")")
scoreord.urban.hr[,6] <- rownames(scoreord.urban.hr)
scoreord.urban.hr[,6] <- substr(scoreord.urban.hr[,6],1,nchar(scoreord.urban.hr[,6])-2)
scoreord.urban.hr[,7] <- str_sub(rownames(scoreord.urban.hr),-1)
scoreord.urban.hr[,8] <- "urban"
scoreord.urban.hr <- scoreord.urban.hr[(nrow(scoreord.urban.hr)-4):nrow(scoreord.urban.hr),]
names(scoreord.urban.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.urban.hr <- dcast(scoreord.urban.hr,Subgroups + Variable~scoreord.urban.hr$Transition,value.var = "HR_95CI")


# interaction
residence.vector <- paste0("region_is_urban",".",1:5)
scoreord.residencelist <- list()
scoreord.residence.interaction <- data.frame()
for (i in 1:5){
    
    scoreord.residencelist[[i]] <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                                             as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                                             as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                                             as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                                             as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                                             rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                                             rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                                             rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5 + 
                                             as.factor(mltstate.final[,residence.vector[i]]):as.factor(mltstate.final[,score.vector[i]]),
                                         data = mltstate.final, method = "breslow")
    scoreord.residence.hr <- data.frame(round(summary(scoreord.residencelist[[i]])$conf.int,2))
    scoreord.residence.hr <- scoreord.residence.hr[nrow(scoreord.residence.hr),]
    interaction <- lrtest(scorecat2,scoreord.residencelist[[i]])
    scoreord.residence.hr[1,5] <- interaction[2,5]
    scoreord.residence.hr[,6] <- i
    names(scoreord.residence.hr)[c(5,6)] <- c("pvalue","transitions")
    scoreord.residence.interaction <- rbind(scoreord.residence.interaction,scoreord.residence.hr)
    print(i)
    
}


##########################################          education        ##########################################
mltstate.primary <- subset(mltstate.final, education_3groups == 0)
mltstate.middle <- subset(mltstate.final, education_3groups == 1)
mltstate.college <- subset(mltstate.final, education_3groups == 2)

# Illiterate and primary school
scoreord.primary <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                              as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                              as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                              as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) +                            
                              rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                              rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                              rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                          data = mltstate.primary, method = "breslow")
scoreord.primary.hr <- data.frame(round(summary(scoreord.primary)$conf.int,2))
scoreord.primary.hr[,5] <- paste0(sprintf(scoreord.primary.hr[,1],fmt="%.2f")," (",sprintf(scoreord.primary.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.primary.hr[,4],fmt="%.2f"),")")
scoreord.primary.hr[,6] <- rownames(scoreord.primary.hr)
scoreord.primary.hr[,6] <- substr(scoreord.primary.hr[,6],1,nchar(scoreord.primary.hr[,6])-2)
scoreord.primary.hr[,7] <- str_sub(rownames(scoreord.primary.hr),-1)
scoreord.primary.hr[,8] <- "primary"
scoreord.primary.hr <- scoreord.primary.hr[(nrow(scoreord.primary.hr)-4):nrow(scoreord.primary.hr),]
names(scoreord.primary.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.primary.hr <- dcast(scoreord.primary.hr,Subgroups + Variable~scoreord.primary.hr$Transition,value.var = "HR_95CI")
# 增加新行代表分组变量
new_row <- c("Highest education","","","","","","")
scoreord.primary.hr <- rbind(new_row,scoreord.primary.hr)


# Middle and high school
scoreord.middle <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                             as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                             as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                             as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) +                                                       
                             rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                             rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                             rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                         data = mltstate.middle, method = "breslow")
scoreord.middle.hr <- data.frame(round(summary(scoreord.middle)$conf.int,2))
scoreord.middle.hr[,5] <- paste0(sprintf(scoreord.middle.hr[,1],fmt="%.2f")," (",sprintf(scoreord.middle.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.middle.hr[,4],fmt="%.2f"),")")
scoreord.middle.hr[,6] <- rownames(scoreord.middle.hr)
scoreord.middle.hr[,6] <- substr(scoreord.middle.hr[,6],1,nchar(scoreord.middle.hr[,6])-2)
scoreord.middle.hr[,7] <- str_sub(rownames(scoreord.middle.hr),-1)
scoreord.middle.hr[,8] <- "middle"
scoreord.middle.hr <- scoreord.middle.hr[(nrow(scoreord.middle.hr)-4):nrow(scoreord.middle.hr),]
names(scoreord.middle.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.middle.hr <- dcast(scoreord.middle.hr,Subgroups + Variable~scoreord.middle.hr$Transition,value.var = "HR_95CI")


# College/university
scoreord.college <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                              as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                              as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                              as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) +                            
                              rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                              rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                              rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                          data = mltstate.college, method = "breslow")
scoreord.college.hr <- data.frame(round(summary(scoreord.college)$conf.int,2))
scoreord.college.hr[,5] <- paste0(sprintf(scoreord.college.hr[,1],fmt="%.2f")," (",sprintf(scoreord.college.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.college.hr[,4],fmt="%.2f"),")")
scoreord.college.hr[,6] <- rownames(scoreord.college.hr)
scoreord.college.hr[,6] <- substr(scoreord.college.hr[,6],1,nchar(scoreord.college.hr[,6])-2)
scoreord.college.hr[,7] <- str_sub(rownames(scoreord.college.hr),-1)
scoreord.college.hr[,8] <- "college"
scoreord.college.hr <- scoreord.college.hr[(nrow(scoreord.college.hr)-4):nrow(scoreord.college.hr),]
names(scoreord.college.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.college.hr <- dcast(scoreord.college.hr,Subgroups + Variable~scoreord.college.hr$Transition,value.var = "HR_95CI")


# interaction
edu.vector <- paste0("education_3groups",".",1:5)
scoreord.edulist <- list()
scoreord.edu.interaction <- data.frame()
for (i in 1:5){
    
    scoreord.edulist[[i]] <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                                       as.factor(education_3groups.1) + as.factor(education_3groups.2) + as.factor(education_3groups.3) + as.factor(education_3groups.4) + as.factor(education_3groups.5) + 
                                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                                       as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                                       rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                                       rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                                       rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5 +
                                       as.factor(mltstate.final[,edu.vector[i]]):as.factor(mltstate.final[,score.vector[i]]),
                                   data = mltstate.final, method = "breslow")
    scoreord.edu.hr <- data.frame(round(summary(scoreord.edulist[[i]])$conf.int,2))
    scoreord.edu.hr <- scoreord.edu.hr[nrow(scoreord.edu.hr),]
    interaction <- lrtest(scorecat2,scoreord.edulist[[i]])
    scoreord.edu.hr[1,5] <- interaction[2,5]
    scoreord.edu.hr[,6] <- i
    names(scoreord.edu.hr)[c(5,6)] <- c("pvalue","transitions")
    scoreord.edu.interaction <- rbind(scoreord.edu.interaction,scoreord.edu.hr)
    print(i)
    
}


##########################################          family history of multimorbidity        ##########################################

mltstate.none <- subset(mltstate.final, parental_fh_2groups == 0)
mltstate.one <- subset(mltstate.final, parental_fh_2groups == 1)


# None
scoreord.none <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                           as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                           as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                           as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                           rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                           rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                           rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                       data = mltstate.none, method = "breslow")
scoreord.none.hr <- data.frame(round(summary(scoreord.none)$conf.int,2))
scoreord.none.hr[,5] <- paste0(sprintf(scoreord.none.hr[,1],fmt="%.2f")," (",sprintf(scoreord.none.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.none.hr[,4],fmt="%.2f"),")")
scoreord.none.hr[,6] <- rownames(scoreord.none.hr)
scoreord.none.hr[,6] <- substr(scoreord.none.hr[,6],1,nchar(scoreord.none.hr[,6])-2)
scoreord.none.hr[,7] <- str_sub(rownames(scoreord.none.hr),-1)
scoreord.none.hr[,8] <- "None"
scoreord.none.hr <- scoreord.none.hr[(nrow(scoreord.none.hr)-4):nrow(scoreord.none.hr),]
names(scoreord.none.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.none.hr <- dcast(scoreord.none.hr,Subgroups + Variable~scoreord.none.hr$Transition,value.var = "HR_95CI")
# 增加新行代表分组变量
new_row <- c("Family history of multimorbidity","","","","","","")
scoreord.none.hr <- rbind(new_row,scoreord.none.hr)


# one
scoreord.one <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                          as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                          as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                          as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                          rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                          rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                          rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                      data = mltstate.one, method = "breslow")
scoreord.one.hr <- data.frame(round(summary(scoreord.one)$conf.int,2))
scoreord.one.hr[,5] <- paste0(sprintf(scoreord.one.hr[,1],fmt="%.2f")," (",sprintf(scoreord.one.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.one.hr[,4],fmt="%.2f"),")")
scoreord.one.hr[,6] <- rownames(scoreord.one.hr)
scoreord.one.hr[,6] <- substr(scoreord.one.hr[,6],1,nchar(scoreord.one.hr[,6])-2)
scoreord.one.hr[,7] <- str_sub(rownames(scoreord.one.hr),-1)
scoreord.one.hr[,8] <- "One"
scoreord.one.hr <- scoreord.one.hr[(nrow(scoreord.one.hr)-4):nrow(scoreord.one.hr),]
names(scoreord.one.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.one.hr <- dcast(scoreord.one.hr,Subgroups + Variable~scoreord.one.hr$Transition,value.var = "HR_95CI")



# interaction
fhd.vector <- paste0("parental_fh_2groups",".",1:5)
scoreord.fhdlist <- list()
scoreord.fhd.interaction <- data.frame()
for (i in 1:5){
    
    scoreord.fhdlist[[i]] <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                                       as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                                       as.factor(parental_fh_2groups.1) + as.factor(parental_fh_2groups.2) + as.factor(parental_fh_2groups.3) + as.factor(parental_fh_2groups.4) + as.factor(parental_fh_2groups.5) + 
                                       rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                                       rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                                       rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5 +
                                       as.factor(mltstate.final[,fhd.vector[i]]):as.factor(mltstate.final[,score.vector[i]]),
                                   data = mltstate.final, method = "breslow")
    scoreord.fhd.hr <- data.frame(round(summary(scoreord.fhdlist[[i]])$conf.int,2))
    scoreord.fhd.hr <- scoreord.fhd.hr[nrow(scoreord.fhd.hr),]
    interaction <- lrtest(scorecat2,scoreord.fhdlist[[i]])
    scoreord.fhd.hr[1,5] <- interaction[2,5]
    scoreord.fhd.hr[,6] <- i
    names(scoreord.fhd.hr)[c(5,6)] <- c("pvalue","transitions")
    scoreord.fhd.interaction <- rbind(scoreord.fhd.interaction,scoreord.fhd.hr)
    print(i)
    
}

##########################################          hypertension       ##########################################
mltstate.ht1 <- subset(mltstate.final, has_hypertension == 1)
mltstate.ht0 <- subset(mltstate.final, has_hypertension == 0)

# ht1
scoreord.ht1 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                          as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                          as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                          as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                          as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                          rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                          rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                          rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                      data = mltstate.ht1, method = "breslow")
scoreord.ht1.hr <- data.frame(round(summary(scoreord.ht1)$conf.int,2))
scoreord.ht1.hr[,5] <- paste0(sprintf(scoreord.ht1.hr[,1],fmt="%.2f")," (",sprintf(scoreord.ht1.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.ht1.hr[,4],fmt="%.2f"),")")
scoreord.ht1.hr[,6] <- rownames(scoreord.ht1.hr)
scoreord.ht1.hr[,6] <- substr(scoreord.ht1.hr[,6],1,nchar(scoreord.ht1.hr[,6])-2)
scoreord.ht1.hr[,7] <- str_sub(rownames(scoreord.ht1.hr),-1)
scoreord.ht1.hr[,8] <- "ht1"
scoreord.ht1.hr <- scoreord.ht1.hr[(nrow(scoreord.ht1.hr)-4):nrow(scoreord.ht1.hr),]
names(scoreord.ht1.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.ht1.hr <- dcast(scoreord.ht1.hr,Subgroups + Variable~scoreord.ht1.hr$Transition,value.var = "HR_95CI")
# 增加新行代表分组变量
new_row <- c("Hypertension","","","","","","")
scoreord.ht1.hr <- rbind(new_row,scoreord.ht1.hr)


# ht0
scoreord.ht0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                          as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                          as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                          as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                          as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                          rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                          rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                          rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5,
                      data = mltstate.ht0, method = "breslow")
scoreord.ht0.hr <- data.frame(round(summary(scoreord.ht0)$conf.int,2))
scoreord.ht0.hr[,5] <- paste0(sprintf(scoreord.ht0.hr[,1],fmt="%.2f")," (",sprintf(scoreord.ht0.hr[,3],fmt="%.2f"),"-",sprintf(scoreord.ht0.hr[,4],fmt="%.2f"),")")
scoreord.ht0.hr[,6] <- rownames(scoreord.ht0.hr)
scoreord.ht0.hr[,6] <- substr(scoreord.ht0.hr[,6],1,nchar(scoreord.ht0.hr[,6])-2)
scoreord.ht0.hr[,7] <- str_sub(rownames(scoreord.ht0.hr),-1)
scoreord.ht0.hr[,8] <- "ht0"
scoreord.ht0.hr <- scoreord.ht0.hr[(nrow(scoreord.ht0.hr)-4):nrow(scoreord.ht0.hr),]
names(scoreord.ht0.hr)[c(1,3,4,5,6,7,8)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition","Subgroups")
#scoreord.ht0.hr <- dcast(scoreord.ht0.hr,Subgroups + Variable~scoreord.ht0.hr$Transition,value.var = "HR_95CI")


# interaction
ht.vector <- paste0("has_hypertension",".",1:5)
scoreord.htlist <- list()
scoreord.ht.interaction <- data.frame()
for (i in 1:5){
    
    scoreord.htlist[[i]] <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                                      as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                                      as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                                      as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                                      as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                                      rscore.cat1.1 + rscore.cat1.2 + rscore.cat1.3 + rscore.cat1.4 + rscore.cat1.5 +  
                                      rscore.cat2.1 + rscore.cat2.2 + rscore.cat2.3 + rscore.cat2.4 + rscore.cat2.5 +  
                                      rscore.cat3.1 + rscore.cat3.2 + rscore.cat3.3 + rscore.cat3.4 + rscore.cat3.5 +
                                      as.factor(mltstate.final[,ht.vector[i]]):as.factor(mltstate.final[,score.vector[i]]),
                                  data = mltstate.final, method = "breslow")
    scoreord.ht.hr <- data.frame(round(summary(scoreord.htlist[[i]])$conf.int,2))
    scoreord.ht.hr <- scoreord.ht.hr[nrow(scoreord.ht.hr),]
    interaction <- lrtest(scorecat2,scoreord.htlist[[i]])
    scoreord.ht.hr[1,5] <- interaction[2,5]
    scoreord.ht.hr[,6] <- i
    names(scoreord.ht.hr)[c(5,6)] <- c("pvalue","transitions")
    scoreord.ht.interaction <- rbind(scoreord.ht.interaction,scoreord.ht.hr)
    print(i)
    
}


##############################################    亚组分析汇总   ############################################
# for HR
mltstate.subgroups <- rbind(scoreord.men.hr,scoreord.women.hr,scoreord.lt50.hr,scoreord.5059.hr,scoreord.ge60.hr,
                            scoreord.rural.hr,scoreord.urban.hr,scoreord.primary.hr,scoreord.middle.hr,scoreord.college.hr,
                            scoreord.none.hr,scoreord.one.hr,scoreord.ht1.hr,scoreord.ht0.hr)
write.csv(mltstate.subgroups, paste0(path,"/ordinal score subgroups",Sys.Date(),".csv"))

# for P value of interaction term
subgroup.interaction <- rbind(scoreord.sex.interaction,scoreord.age.interaction,scoreord.residence.interaction,scoreord.edu.interaction,
                              scoreord.fhd.interaction,scoreord.ht.interaction)
write.csv(subgroup.interaction, paste0(path,"/interaciton subgroups",Sys.Date(),".csv"))




