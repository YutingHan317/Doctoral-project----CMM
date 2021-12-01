# 已剔除基线患有肿瘤和1型糖尿病，bmi缺失者;未剔除患有冠心病、脑卒中、2型糖尿病
# 本部分分析已经尝试了调整时间和从不同阶段进入没有大的区别（仅有部分单一生活方式指标发生改变）

# 设置工作路径
setwd("D:/HanYT/2019-06 Multimorbidity")
library(mstate)
library(plyr)

# 载入数据
load("D:/HanYT/2019-06 Multimorbidity/1. Database/project4_full.rdata")

# project4 <- subset(project4_full, chd_diag != 1 & stroke_or_tia_diag != 1 | has_diabetes != 1)
project4 <- project4_full
project4 <- subset(project4,studyid!=5517590)

# 提取子集，增加运算速度;待调整好之后删除
# project4 <- subset(project4,region_is_urban==0)

# 计算同一时间抵达多个阶段，用于之后的剔除同时抵达不同阶段的研究对象
# project4$sametime[project4$mltmbd_inc_date==project4$first_cmd_inc_date & project4$mltmbd_inc==1 & project4$first_cmd_inc==1] <- T
# project4$sametime[project4$du_ep0001_date==project4$first_cmd_inc_date & project4$du_ep0001==1 & project4$first_cmd_inc==1] <- T
# project4$sametime[project4$du_ep0001_date==project4$mltmbd_inc_date & project4$du_ep0001==1 & project4$mltmbd_inc==1] <- T
# project4$sametime[is.na(project4$sametime)] <- F
# tr(project4$sametime)
# sum(is.na(project4$sametime))
# table(project4$sametime)

# 时间换算为年龄（岁）
project4$study_date <- as.numeric(project4$study_date)
project4$first_cmd_inc_date <- as.numeric(project4$first_cmd_inc_date)
project4$mltmbd_inc_date <- as.numeric(project4$mltmbd_inc_date)
project4$du_ep0001_date <- as.numeric(project4$du_ep0001_date)
project4$dob_anon <- as.numeric(project4$dob_anon)
attach(project4)
project4$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5/365.25 
project4$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5/365.25 
detach(project4)
project4[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")] <- (project4[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")]-project4$dob_anon)/365.25
summary(project4[, c("dob_anon","study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")])
class(project4$dob_anon)
# # # gen original database
covariates <- c("studyid","age_strata","region_code","region_is_urban","is_female","highest_education","occupation",
                "marital_status","household_income","parental_fh_3groups", "chronic_fh", "mltmbd_fh")
phrases <- c("study_date","first_cmd_inc","first_cmd_inc_date","mltmbd_inc","mltmbd_inc_date","du_ep0001","du_ep0001_date")
lifestyles <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_obesity","healthy_sleep","healthy_sitting","healthy_PA","healthy_score_6groups","healthy_score")
varlist <- c(covariates,phrases,lifestyles)

# 部分基线就患基础疾病的研究对象：首发疾病状态为1，首发疾病时间为入组时间（待定：发病时间）
project4$first_cmd_inc[project4$d_number_base==1] <- 1
project4$first_cmd_inc_date[project4$d_number_base==1] <- project4$study_date[project4$d_number_base==1]
project4$mltmbd_inc[project4$d_number_base>1] <- 1
project4$mltmbd_inc_date[project4$d_number_base>1] <- project4$study_date[project4$d_number_base>1]

# 生成第一次疾病和死亡同时发生的研究对象（即死于首发疾病）05/01
project4$fcmd.death[project4$du_ep0001_date==project4$first_cmd_inc_date & project4$du_ep0001==1 & project4$first_cmd_inc==1] <- 1
project4$fcmd.death[is.na(project4$fcmd.death)] <- 0
table(project4$fcmd.death)

# 生成并剔除第一次和第二次疾病同时发生的研究对象  4/30
project4$fcmd.mltmbd[project4$mltmbd_inc_date==project4$first_cmd_inc_date & project4$mltmbd_inc==1 & project4$first_cmd_inc==1] <- 1
project4$fcmd.mltmbd[is.na(project4$fcmd.mltmbd)] <- 0
sum(is.na(project4$fcmd.mltmbd))
mltstate.origin <- subset(project4, fcmd.mltmbd == 0, 
                          select = c(varlist,"fcmd.death","d_number_base","fcmd.mltmbd","studyid"))  # 04-08 不再剔除先剔除有相同日期的人

mltstate.origin$enrollment[mltstate.origin$d_number_base==0] <- 1
mltstate.origin$enrollment[mltstate.origin$d_number_base==1] <- 2
mltstate.origin$enrollment[mltstate.origin$d_number_base> 1] <- 3
table(mltstate.origin$enrollment)

# 0.5天：最小为1天，减去0.5没有影响，无需剔除任何研究对象
mltstate.origin$first_cmd_inc_date[mltstate.origin$fcmd.death==1] <- mltstate.origin$first_cmd_inc_date[mltstate.origin$fcmd.death==1] - 0.5/365.25  

# 设置路径
tmat <- transMat(x = list(c(2, 4), c(3,4), c(4), c()), names = c("healthy", "fcmd", "mltmbd", "Death"))

# Wide to long
mltstate.long <- msprep(data = mltstate.origin, trans = tmat,
                        time = c(NA, "first_cmd_inc_date", "mltmbd_inc_date","du_ep0001_date"),
                        status = c(NA, "first_cmd_inc", "mltmbd_inc", "du_ep0001"),
                        keep = c(covariates,lifestyles,"study_date","d_number_base"),
                        start = list(state = mltstate.origin$enrollment,time = mltstate.origin$study_date))
events(mltstate.long)
ddply(mltstate.long,c("d_number_base","trans"),summarise, Min = min(time))
table(mltstate.long$fcmd.mltmbd) 
sum(mltstate.long$time==0)
a <- subset(mltstate.long, d_number_base == 1 & trans == 3 & time == 0)
b <- subset(mltstate.long,id == 445976)
# set as category
mltstate.long$hscore <- mltstate.long$healthy_score_6groups
mltstate.long$hscore.cat <- as.factor(mltstate.long$hscore)

# Transition-specific covariates
covs <- c("healthy_smoking", "healthy_alcohol", "healthy_diet", "healthy_obesity", "healthy_PA", "healthy_sleep",
          "hscore","hscore.cat")
mltstate.full <- expand.covs(mltstate.long, covs, longnames = FALSE)  # 均为哑变量
head(mltstate.full)
names(mltstate.full)

# Drop observation of which time = 0 
message(sum(mltstate.full$time==0)," records was droped")
mltstate.final <- subset(mltstate.full,time != 0)

#
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
write.csv(single.hr2, paste0("3. Result/multistate/single model2 no drop ",Sys.Date(),".csv"))

### Calculate HR for lifestyle score (ordinal) 
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scoreord2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       is_female + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.1 + hscore.2 + hscore.3 + hscore.4 + hscore.5,
                   data = mltstate.final, method = "breslow")
scoreord.hr2 <- round(summary(scoreord2)$conf.int,2)
write.csv(scoreord.hr2, paste0("3. Result/multistate/ordinal score model2 no drop ",Sys.Date(),".csv"))
