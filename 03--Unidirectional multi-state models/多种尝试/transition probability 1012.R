# 已剔除基线患有肿瘤和糖尿病，脑卒中和缺血性心脏病
# 调整变量删除职业 8/31
# 所有协变量均设置为trans-specific
# 地区变量挪到factor里面
# 时间轴改为以time on study为时间轴
# 计算transition probability
# Body weight and fat
# rm(list=ls())
# 设置工作路径
setwd("D:/HanYT/2019-06 Multimorbidity")
library(mstate)
library(plyr)
library(lmtest)
library(stringr)
library(tidyverse)
library(reshape2)
library(cowplot)
library(ggplot2)

################################################################################################################################################
###################################                                                           ##################################################
###################################                    gen dataset                            ##################################################
###################################                                                           ##################################################
################################################################################################################################################

# 载入数据
load("./1. Database/project4.rdata")

tempfile <- project4
project4.graph <- subset(tempfile,study_date != du_ep0001_date)

# 提取子集，增加运算速度;待调整好之后删除
# project4.graph <- subset(project4.graph,region_is_urban==0)

# 计算同一时间抵达多个阶段，用于之后的剔除同时抵达不同阶段的研究对象
# project4.graph$sametime[project4.graph$mltmbd_inc_date==project4.graph$first_cmd_inc_date & project4.graph$mltmbd_inc==1 & project4.graph$first_cmd_inc==1] <- T
# project4.graph$sametime[project4.graph$du_ep0001_date==project4.graph$first_cmd_inc_date & project4.graph$du_ep0001==1 & project4.graph$first_cmd_inc==1] <- T
# project4.graph$sametime[project4.graph$du_ep0001_date==project4.graph$mltmbd_inc_date & project4.graph$du_ep0001==1 & project4.graph$mltmbd_inc==1] <- T
# project4.graph$sametime[is.na(project4.graph$sametime)] <- F
# tr(project4.graph$sametime)
# sum(is.na(project4.graph$sametime))·
# table(project4.graph$sametime)

# 时间换算为年龄（岁）
project4.graph$study_date <- as.numeric(project4.graph$study_date)
project4.graph$first_cmd_inc_date <- as.numeric(project4.graph$first_cmd_inc_date)
project4.graph$mltmbd_inc_date <- as.numeric(project4.graph$mltmbd_inc_date)
project4.graph$du_ep0001_date <- as.numeric(project4.graph$du_ep0001_date)
project4.graph$dob_anon <- as.numeric(project4.graph$dob_anon)

# 一、某些研究对象首次心血管代谢性疾病发生于2017年12月31日，在上述基础上加上0.5天
attach(project4.graph)
project4.graph$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4.graph$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5
project4.graph$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4.graph$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5 
detach(project4.graph)

# 计算各个对应阶段的随访时间
project4.graph[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")] <- (project4.graph[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")]-project4.graph$study_date)/365.25
summary(project4.graph[, c("dob_anon","study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")])
class(project4.graph$dob_anon)

# # # gen original database
# # 数据库中存在不同阶段记录时间相同的个体，生成对应变量指代其类型 
attach(project4.graph)
project4.graph$sametime[first_cmd_inc==1 & du_ep0001 == 1 & du_ep0001_date==first_cmd_inc_date] <- 1      # 死于首次CMD
project4.graph$sametime[first_cmd_inc==1 & mltmbd_inc == 1 & mltmbd_inc_date==first_cmd_inc_date] <- 2      # 首次心血管代谢性疾病时间与共病记录日期相同
project4.graph$sametime[mltmbd_inc == 1 & du_ep0001 == 1 & mltmbd_inc_date==du_ep0001_date] <- 3      # 死于首次发生共病
project4.graph$sametime[first_cmd_inc == 1 &  mltmbd_inc == 1 & du_ep0001 == 1 & mltmbd_inc_date==du_ep0001_date & first_cmd_inc_date==mltmbd_inc_date] <- 4      # 首次CMD、共病和死亡日期相同
project4.graph$sametime[is.na(project4.graph$sametime)] <- 0
table(project4.graph$sametime)
detach(project4.graph)

# # 对时间相同的不同阶段进行处理
# 死于首次CMD
project4.graph$first_cmd_inc_date[project4.graph$sametime==1] <- project4.graph$du_ep0001_date[project4.graph$sametime==1] - 0.5/365.25
# 首次心血管代谢性疾病时间与共病记录日期相同
project4.graph$first_cmd_inc_date[project4.graph$sametime==2] <- project4.graph$mltmbd_inc_date[project4.graph$sametime==2] - 0.5/365.25
# 死于首次发生共病
project4.graph$mltmbd_inc_date[project4.graph$sametime==3] <- project4.graph$du_ep0001_date[project4.graph$sametime==3] - 0.5/365.25
# 首次CMD、共病和死亡日期相同
project4.graph$mltmbd_inc_date[project4.graph$sametime==4] <- project4.graph$du_ep0001_date[project4.graph$sametime==4] - 0.5/365.25
project4.graph$first_cmd_inc_date[project4.graph$sametime==4] <- project4.graph$mltmbd_inc_date[project4.graph$sametime==4] - 0.5/365.25


# 仅保留需要的变量
mltstate.origin.graph <- project4.graph
# 生成代表进入研究的阶段
mltstate.origin.graph$enrollment <- 1

# 设置路径
tmat <- transMat(x = list(c(2, 4), c(3,4), c(4), c()), names = c("healthy", "fcmd", "mltmbd", "Death"))

# Set lifestyle score as category
mltstate.origin.graph$hscore <- mltstate.origin.graph$healthy_score
mltstate.origin.graph$hscore.cat <- as.factor(mltstate.origin.graph$healthy_score_5groups)

mltstate.origin.graph$rscore <- mltstate.origin.graph$risky_score
mltstate.origin.graph$rscore.cat <- as.factor(mltstate.origin.graph$risky_score_5groups)

# set all categorical variable to factors
mltstate.origin.graph$region_code1 <- factor(mltstate.origin.graph$region_code,levels = c(12,16,26,36,46,52,58,68,78,88),
                                             labels = c("qingdao","harbin","haikou","suzhou","liuzhou","sichuan","gansu","henan","zhejiang","hunan")) 
mltstate.origin.graph$age_strata1 <- factor(mltstate.origin.graph$age_strata,levels = c(0,1,2,3,4,5,6,7,8,9),
                                            labels = c("30","35","40","45","50","55","60","65","70","75")) 
mltstate.origin.graph$highest_education1 <- factor(mltstate.origin.graph$highest_education,levels = c(0,1,2,3,4,5),
                                                   labels = c("noformal","primary","middle","high","tech","uni"))
mltstate.origin.graph$parental_fh_3groups1 <- factor(mltstate.origin.graph$parental_fh_3groups,levels = c(0,1,2),
                                                   labels = c("none","one","two"))
mltstate.origin.graph$rscore.cat1 <- mltstate.origin.graph$rscore.cat
factor.vector <- c("age_strata1","region_code1","highest_education1","parental_fh_3groups1","rscore.cat1")
dummy.matrix <- model.matrix(~age_strata1+region_code1+highest_education1+parental_fh_3groups1+rscore.cat1,mltstate.origin.graph)
dummy.name <- colnames(dummy.matrix[,2:ncol(dummy.matrix)])
mltstate.origin.graph <- cbind(mltstate.origin.graph,dummy.matrix[,2:ncol(dummy.matrix)])

# 选取需要的变量进行分析，避免数据库过大
covariates <- c("studyid","age_strata","region_code","region_is_urban","is_female","highest_education","occupation",
                "marital_status_2groups","household_income","parental_fh_3groups", "chronic_fh", "menopause_2groups",
                "age_3groups","region_is_urban", "education_3groups","has_hypertension")
phrases <- c("study_date","first_cmd_inc","first_cmd_inc_date","mltmbd_inc","mltmbd_inc_date","du_ep0001","du_ep0001_date")
lifestyles <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","healthy_score_5groups","healthy_score",
                "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_obesity","risky_PA","risky_score_5groups","risky_score",
                "hscore","hscore.cat","rscore","rscore.cat")
varlist <- c(covariates,phrases,lifestyles,dummy.name)

# Wide to long
mltstate.long.graph <- msprep(data = mltstate.origin.graph, trans = tmat,
                              time = c(NA, "first_cmd_inc_date", "mltmbd_inc_date","du_ep0001_date"),
                              status = c(NA, "first_cmd_inc", "mltmbd_inc", "du_ep0001"),
                              keep = c(covariates,lifestyles,dummy.name),
                              start = list(state = mltstate.origin.graph$enrollment,time = mltstate.origin.graph$study_date))

# Transitions
events(mltstate.long.graph)
sum(mltstate.long.graph$time==0)

# Transition-specific covariates
long_covariates <- c("is_female","age_strata","highest_education","occupation","marital_status_2groups","parental_fh_3groups","menopause_2groups",
                     "age_3groups","region_is_urban", "education_3groups","has_hypertension","region_code")
long_lf <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","hscore","hscore.cat",
             "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_obesity","risky_PA","rscore","rscore.cat")
mltstate.full.graph <- expand.covs(mltstate.long.graph, c(long_covariates,long_lf,dummy.name), longnames = FALSE)  # 均为哑变量
head(mltstate.full.graph)
names(mltstate.full.graph)

# Drop observation of which time = 0 
message(sum(mltstate.full.graph$time==0)," records was droped")
mltstate.final.graph <- subset(mltstate.full.graph,time != 0)

################################################################################################################################################
###################################                                                           ##################################################
###################################                    primary analyses                       ##################################################
###################################                                                           ##################################################
################################################################################################################################################
# Generate formula
covariates_dummy <- c("is_female","marital_status_2groups",dummy.name)
cov_fomula <- paste0("Surv(Tstart, Tstop, status)","~","strata(trans)")

for (i in 1:length(covariates_dummy)) {
    
    cov_fomula <- paste(cov_fomula,"+",paste(covariates_dummy[i],".",1:5,sep = "", collapse = " + "))
}
as.formula(cov_fomula)

### Calculate HR for lifestyle score (category) 
# Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
scorecat2.graph1 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)  +
                              as.factor(age_strata.1) + as.factor(age_strata.2) + as.factor(age_strata.3) + as.factor(age_strata.4) + as.factor(age_strata.5) + 
                              as.factor(region_code.1) + as.factor(region_code.2) + as.factor(region_code.3) + as.factor(region_code.4) + as.factor(region_code.5) + 
                              as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                              as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                              as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                              as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                              rscore.cat1.1 + rscore.cat2.1 + rscore.cat3.1 + rscore.cat4.1 + 
                              rscore.cat1.2 + rscore.cat2.2 + rscore.cat3.2 + rscore.cat4.2 + 
                              rscore.cat1.3 + rscore.cat2.3 + rscore.cat3.3 + rscore.cat4.3 + 
                              rscore.cat1.4 + rscore.cat2.4 + rscore.cat3.4 + rscore.cat4.4 + 
                              rscore.cat1.5 + rscore.cat2.5 + rscore.cat3.5 + rscore.cat4.5 ,
                          data = mltstate.final.graph, method = "breslow")
round(summary(scorecat2.graph1)$conf.int,2)

scorecat2.graph2 <- coxph(as.formula(cov_fomula), data = mltstate.final.graph, method = "breslow")
round(summary(scorecat2.graph2)$conf.int,2)

################################################################################################################################################
###################################                                                           ##################################################
###################################                      prediction                           ##################################################
###################################                                                           ##################################################
################################################################################################################################################
### 计算所有哑变量均值
d_name_vector <- c("is_female","marital_status_2groups",dummy.name)
sapply(mltstate.origin.graph[d_name_vector], mean, na.rm=TRUE)
##################################################################  生成转移概率
#######################  具有0或1个危险生活方式
no_trans <- 5
newd1 <- data.frame(is_female = rep(0.5898,no_trans), marital_status_2groups = rep(0.0891,no_trans),
                    age_strata135 = rep(0.1445,no_trans), age_strata140 = rep(0.1760,no_trans), age_strata145 = rep(0.1391,no_trans), 
                    age_strata150 = rep(0.1724,no_trans), age_strata155 = rep(0.1315,no_trans), age_strata160 = rep(0.0894,no_trans),
                    age_strata165 = rep(0.0711,no_trans), age_strata170 = rep(0.0493,no_trans), age_strata175 = rep(0.0058,no_trans),
                    region_code1harbin = rep(0.0979,no_trans), region_code1haikou = rep(0.0586,no_trans), region_code1suzhou = rep(0.1069,no_trans),
                    region_code1liuzhou = rep(0.0931,no_trans), region_code1sichuan = rep(0.1147,no_trans), region_code1gansu = rep(0.1018,no_trans),
                    region_code1henan = rep(0.1242,no_trans), region_code1zhejiang = rep(0.1169,no_trans), region_code1hunan = rep(0.1197,no_trans),
                    highest_education1primary = rep(0.3215,no_trans), highest_education1middle = rep(0.2860,no_trans), highest_education1high = rep(0.1514,no_trans),
                    highest_education1tech = rep(0.0349,no_trans), highest_education1uni = rep(0.0215,no_trans), parental_fh_3groups1one = rep(0.2044,no_trans),
                    parental_fh_3groups1two = rep(0.0094,no_trans), rscore.cat12 = rep(0,no_trans), rscore.cat13 = rep(0,no_trans),
                    rscore.cat14 = rep(0,no_trans), rscore.cat15 = rep(0,no_trans), trans = 1:no_trans)
attr(newd1, "trans") <- tmat
class(newd1) <- c("msdata", "data.frame")
newd1 <- expand.covs(newd1, d_name_vector, longnames = FALSE)
newd1$strata = 1:no_trans
msf1 <- msfit(scorecat2.graph2, newdata = newd1, trans = tmat)
head(msf1)
tail(msf1)
summary(msf1)

#######################  具有5或6个危险生活方式
newd2 <- data.frame(is_female = rep(0.5898,no_trans), marital_status_2groups = rep(0.0891,no_trans),
                    age_strata135 = rep(0.1445,no_trans), age_strata140 = rep(0.1760,no_trans), age_strata145 = rep(0.1391,no_trans), 
                    age_strata150 = rep(0.1724,no_trans), age_strata155 = rep(0.1315,no_trans), age_strata160 = rep(0.0894,no_trans),
                    age_strata165 = rep(0.0711,no_trans), age_strata170 = rep(0.0493,no_trans), age_strata175 = rep(0.0058,no_trans),
                    region_code1harbin = rep(0.0979,no_trans), region_code1haikou = rep(0.0586,no_trans), region_code1suzhou = rep(0.1069,no_trans),
                    region_code1liuzhou = rep(0.0931,no_trans), region_code1sichuan = rep(0.1147,no_trans), region_code1gansu = rep(0.1018,no_trans),
                    region_code1henan = rep(0.1242,no_trans), region_code1zhejiang = rep(0.1169,no_trans), region_code1hunan = rep(0.1197,no_trans),
                    highest_education1primary = rep(0.3215,no_trans), highest_education1middle = rep(0.2860,no_trans), highest_education1high = rep(0.1514,no_trans),
                    highest_education1tech = rep(0.0349,no_trans), highest_education1uni = rep(0.0215,no_trans), parental_fh_3groups1one = rep(0.2044,no_trans),
                    parental_fh_3groups1two = rep(0.0094,no_trans), rscore.cat12 = rep(0,no_trans), rscore.cat13 = rep(0,no_trans),
                    rscore.cat14 = rep(0,no_trans), rscore.cat15 = rep(1,no_trans), trans = 1:no_trans)
attr(newd2, "trans") <- tmat
class(newd2) <- c("msdata", "data.frame")
newd2 <- expand.covs(newd2, d_name_vector, longnames = FALSE)
newd2$strata = 1:no_trans
msf2 <- msfit(scorecat2.graph2, newdata = newd2, trans = tmat)

##################################################################  画图
path_transplot <- paste0("3. Result/multistate/Figure","/",Sys.Date())
dir.create(path_transplot)
####################### 不考虑途经阶段
# # Transition probability for low risk
pt1 <- probtrans(msf1, predt = 0)

# 宽转长
trans1 <- melt(pt1[[1]],
               id.vars="time",
               measure.vars = c("pstate2","pstate3","pstate4"),
               variable.name="state",
               value.name="probability")
trans1$state <- factor(trans1$state,levels = rev(levels(trans1$state)))
levels(trans1$state)    
p1 <- ggplot(trans1,aes(x=trans1$time,y=trans1$probability,fill=state)) + geom_area() + 
    scale_fill_brewer(name="State",palette="Reds", direction = -1, breaks = c("pstate4","pstate3","pstate2"),
                      labels=c("Death","Cardiometabolic multimorbidity","First caridiometabolic disease"))+ 
    xlab("Years since enrollment") + ylab("Probability") +
    scale_x_continuous(breaks = c(0,2.5,5,7.5,10,12.5,15))+ 
    scale_y_continuous(limits = c(0,0.40),breaks = seq(0,0.40,by=0.05)) +
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
          axis.title =element_text(face="plain",size=14),
          axis.text = element_text(face="plain",size = 12),
          legend.title=element_text(face="plain",size=12),
          legend.position=c(0.1,1), legend.justification=c(0.1,1),
          legend.text=element_text(size=14))

# # Transition probability for low risk
pt2 <- probtrans(msf2, predt = 0)

## Graph
trans2 <- melt(pt2[[1]],
               id.vars="time",
               measure.vars = c("pstate2","pstate3","pstate4"),
               variable.name="state",
               value.name="probability")
trans2$state <- factor(trans2$state,levels = rev(levels(trans2$state)))
levels(trans2$state)    
p2 <- ggplot(trans2,aes(x=trans2$time,y=trans2$probability,fill=state)) + geom_area() + 
    scale_fill_brewer(name="State",palette="Reds", direction = -1, breaks = c("pstate4","pstate3","pstate2"),
                      labels=c("Death","Cardiometabolic multimorbidity","First caridiometabolic disease"))+ 
    xlab("Years since enrollment") + ylab("Probability") +
    scale_x_continuous(breaks = c(0,2.5,5,7.5,10,12.5,15))+ 
    scale_y_continuous(limits = c(0,0.40),breaks = seq(0,0.40,by=0.05)) +
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
          axis.title.x = element_text(face="plain",size=14),
          axis.title.y = element_blank(),
          axis.text.x = element_text(face="plain",size = 12),
          axis.text.y = element_blank(),
          legend.title=element_text(face="plain",size=12),
          legend.position=c(0.1,1), legend.justification=c(0.1,1),
          legend.text=element_text(size=14))
# # 合并图像
plot_grid(p1,p2)
ggsave("Transition plot1.png",path = path_transplot,width = 9,height = 9)

####################### 考虑途经阶段
tmat2 <- transMat(x = list(c(2, 4), c(3,5), c(6), c(), c(), c()), 
                  names = c("healthy", "fcmd", "mltmbd", "Death no cmd","Death fcmd","Death mbd"))
# # Transition probability for low risk
msf3 <- msf1
msf3$trans <- tmat2
pt3 <- probtrans(msf3,predt = 0)

trans3 <- melt(pt3[[1]],
               id.vars="time",
               measure.vars = c("pstate2","pstate3","pstate4","pstate5","pstate6"),
               variable.name="state",
               value.name="probability")
trans3$state <- factor(trans3$state,levels = rev(levels(trans3$state)))
levels(trans3$state) 

p3 <- ggplot(trans3,aes(x=trans3$time,y=trans3$probability,fill=state)) + geom_area() + 
    scale_fill_brewer(name="State",palette="Reds", direction = -1, 
                      breaks = c("pstate6","pstate5","pstate4","pstate3","pstate2"),
                      labels=c("Dead with CMM","Dead with FCMD","Dead without CMD","CMM survivor","FCMD survivor"))+ 
    xlab("Years since enrollment") + ylab("Probability") +
    scale_x_continuous(breaks = c(0,2.5,5,7.5,10,12.5,15))+ 
    scale_y_continuous(limits = c(0,0.40),breaks = seq(0,0.40,by=0.05)) +
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
          axis.title =element_text(face="plain",size=14),
          axis.text = element_text(face="plain",size = 12),
          legend.title=element_text(face="plain",size=12),
          legend.position=c(0.1,1), legend.justification=c(0.1,1),
          legend.text=element_text(size=14))

# # Transition probability for high risk
msf4 <- msf2
msf4$trans <- tmat2
pt4 <- probtrans(msf4,predt = 0)

trans4 <- melt(pt4[[1]],
               id.vars="time",
               measure.vars = c("pstate2","pstate3","pstate4","pstate5","pstate6"),
               variable.name="state",
               value.name="probability")
trans4$state <- factor(trans4$state,levels = rev(levels(trans4$state)))
levels(trans4$state) 

p4 <- ggplot(trans4,aes(x=trans4$time,y=trans4$probability,fill=state)) + geom_area() + 
    scale_fill_brewer(name="State",palette="Reds", direction = -1, 
                      breaks = c("pstate6","pstate5","pstate4","pstate3","pstate2"),
                      labels=c("Dead with CMM","Dead with FCMD","Dead without CMD","CMM survivor","FCMD survivor"))+ 
    xlab("Years since enrollment") + ylab("Probability") +
    scale_x_continuous(breaks = c(0,2.5,5,7.5,10,12.5,15))+ 
    scale_y_continuous(limits = c(0,0.40),breaks = seq(0,0.40,by=0.05)) +
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
          axis.title.x = element_text(face="plain",size=14),
          axis.title.y = element_blank(),
          axis.text.x = element_text(face="plain",size = 12),
          axis.text.y = element_blank(),
          legend.title=element_text(face="plain",size=12),
          legend.position=c(0.1,1), legend.justification=c(0.1,1),
          legend.text=element_text(size=14))

plot_grid(p3,p4)
ggsave("Transition plot2.png",path = path_transplot,width = 9,height = 9)









