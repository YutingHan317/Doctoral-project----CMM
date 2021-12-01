# 已剔除基线患有肿瘤和糖尿病，脑卒中和缺血性心脏病
# 调整变量删除职业

# 设置工作路径
setwd("D:/HanYT/2019-06 Multimorbidity")
library(mstate)
library(plyr)
library(lmtest)

# 载入数据
load("./1. Database/project4.rdata")

tempfile <- project4
project4 <- subset(tempfile,study_date != du_ep0001_date)

# 提取子集，增加运算速度;待调整好之后删除
project4 <- subset(project4,region_is_urban==0)

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
                "marital_status","household_income","parental_fh_3groups", "chronic_fh", "mltmbd_fh")
phrases <- c("study_date","first_cmd_inc","first_cmd_inc_date","mltmbd_inc","mltmbd_inc_date","du_ep0001","du_ep0001_date")
lifestyles <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","healthy_score_5groups1","healthy_score")
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
events(mltstate.long)
sum(mltstate.long$time==0)

# set as category
mltstate.long$hscore.cat <- as.factor(mltstate.long$hscore)

# Set lifestyle score as category
mltstate.long$hscore <- mltstate.long$healthy_score_5groups
mltstate.long$hscore.cat <- as.factor(mltstate.long$hscore)

# Transition-specific covariates
long_covariates <- c("is_female","highest_education","occupation","marital_status","mltmbd_fh")
long_lf <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi",
          "healthy_WC","healthy_sleep","healthy_PA","hscore","hscore.cat")
mltstate.full <- expand.covs(mltstate.long, c(long_covariates,long_lf), longnames = FALSE)  # 均为哑变量
head(mltstate.full)
names(mltstate.full)

# Drop observation of which time = 0 
message(sum(mltstate.full$time==0)," records was droped")
mltstate.final <- subset(mltstate.full,time != 0)

# # # # dichotomous single lifestyle;model 1 和 model2 差别很小，下面仅罗列了model2;家族史变量调一个和两个没有大的区别（05-24）
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
                 data = mltstate.final, method = "breslow")
single.hr2 <- round(summary(single2)$conf.int,2)
write.csv(single.hr2, paste0("3. Result/multistate/dichotomous single model2 primary",Sys.Date(),".csv"))

###### 比较一下是否使调整因素每个阶段系数不同
#### Calculate HR for lifestyle score (ordinal) （各阶段协变量系数不同）(计算不容易收敛)
coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                       as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                       as.factor(occupation.1) + as.factor(occupation.2) + as.factor(occupation.3) + as.factor(occupation.4) + as.factor(occupation.5) + 
                       as.factor(marital_status.1) + as.factor(marital_status.2) + as.factor(marital_status.3) + as.factor(marital_status.4) + as.factor(marital_status.5) + 
                       as.factor(mltmbd_fh.1) + as.factor(mltmbd_fh.2) + as.factor(mltmbd_fh.3) + as.factor(mltmbd_fh.4) + as.factor(mltmbd_fh.5) + 
                       hscore.1 + hscore.2 + hscore.3 + hscore.4 + hscore.5,
                   data = mltstate.final, method = "breslow")

### Calculate HR for lifestyle score (ordinal) （各阶段协变量系数相同）
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scoreord2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       is_female + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.1 + hscore.2 + hscore.3 + hscore.4 + hscore.5,
                   data = mltstate.final, method = "breslow")
scoreord.hr2 <- round(summary(scoreord2)$conf.int,2)
write.csv(scoreord.hr2, paste0("3. Result/multistate/ordinal score model2 primary",Sys.Date(),".csv"))

### Calculate HR for lifestyle score (category) 
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scorecat2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       is_female + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.cat1.1 + hscore.cat2.1 + hscore.cat3.1 + hscore.cat4.1 + 
                       hscore.cat1.2 + hscore.cat2.2 + hscore.cat3.2 + hscore.cat4.2 + 
                       hscore.cat1.3 + hscore.cat2.3 + hscore.cat3.3 + hscore.cat4.3 + 
                       hscore.cat1.4 + hscore.cat2.4 + hscore.cat3.4 + hscore.cat4.4 + 
                       hscore.cat1.5 + hscore.cat2.5 + hscore.cat3.5 + hscore.cat4.5  ,
                   data = mltstate.final, method = "breslow")
scorecat.hr2 <- round(summary(scorecat2)$conf.int,2)
write.csv(scorecat.hr2, paste0("3. Result/multistate/category score model2 primary",Sys.Date(),".csv"))

#################################################################   检验性别分层 ###########################################################
a <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       is_female + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.1 + hscore.2 + hscore.3 + hscore.4 + hscore.5,
                   data = mltstate.final, method = "breslow")

b <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       strata(is_female) + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.1 + hscore.2 + hscore.3 + hscore.4 + hscore.5,
                   data = mltstate.final, method = "breslow")
test.a <- cox.zph(a)
ggcoxzph(test.a)
test.b <- cox.zph(b)
ggcoxzph(test.b)
lrtest(a,b)

a.1 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       is_female.1 + is_female.2 + is_female.3 + is_female.4 + is_female.5 + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.1 + hscore.2 + hscore.3 + hscore.4 + hscore.5,
                   data = mltstate.final, method = "breslow")

c <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       is_female + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.cat1.1 + hscore.cat2.1 + hscore.cat3.1 + hscore.cat4.1 + 
                       hscore.cat1.2 + hscore.cat2.2 + hscore.cat3.2 + hscore.cat4.2 + 
                       hscore.cat1.3 + hscore.cat2.3 + hscore.cat3.3 + hscore.cat4.3 + 
                       hscore.cat1.4 + hscore.cat2.4 + hscore.cat3.4 + hscore.cat4.4 + 
                       hscore.cat1.5 + hscore.cat2.5 + hscore.cat3.5 + hscore.cat4.5  ,
                   data = mltstate.final, method = "breslow")

d <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       strata(is_female) + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.cat1.1 + hscore.cat2.1 + hscore.cat3.1 + hscore.cat4.1 + 
                       hscore.cat1.2 + hscore.cat2.2 + hscore.cat3.2 + hscore.cat4.2 + 
                       hscore.cat1.3 + hscore.cat2.3 + hscore.cat3.3 + hscore.cat4.3 + 
                       hscore.cat1.4 + hscore.cat2.4 + hscore.cat3.4 + hscore.cat4.4 + 
                       hscore.cat1.5 + hscore.cat2.5 + hscore.cat3.5 + hscore.cat4.5  ,
                   data = mltstate.final, method = "breslow")
test.c <- cox.zph(c)
ggcoxzph(test.c)
test.d <- cox.zph(d)
ggcoxzph(test.d)

lrtest(c,d)






##################################################################    亚组分析   ###########################################################
### Calculate HR for lifestyle score (ordinal) （各阶段协变量系数相同）
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity （无交互项）
scoreord2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       is_female + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.1 + hscore.2 + hscore.3 + hscore.4 + hscore.5,
                   data = mltstate.final, method = "breslow")
scoreord.hr2 <- round(summary(scoreord2)$conf.int,2)
write.csv(scoreord.hr2, paste0("3. Result/multistate/ordinal score model2 primary",Sys.Date(),".csv"))

# 加入性别交互项
scoreord2.sex <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       is_female + 
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       hscore.1 + hscore.2 + hscore.3 + hscore.4 + hscore.5 +
                       as.factor(is_female):hscore.1 + as.factor(is_female):hscore.2 + as.factor(is_female):hscore.3 + 
                       as.factor(is_female):hscore.4 + as.factor(is_female):hscore.5,
                   data = mltstate.final, method = "breslow")

scoreord2.sex.trans <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                       as.factor(highest_education) + as.factor(occupation) + as.factor(marital_status) + as.factor(mltmbd_fh) +
                       is_female.1*hscore.1 + is_female.2*hscore.2 + is_female.3*hscore.3 + is_female.4*hscore.4 + is_female.5*hscore.5 ,
                   data = mltstate.final, method = "breslow")


lrtest(scoreord2,scoreord2.sex)
