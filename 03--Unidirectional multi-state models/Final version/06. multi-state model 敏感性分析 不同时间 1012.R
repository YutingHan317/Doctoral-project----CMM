# 已剔除基线患有重大疾病者，bmi缺失者
# Created by HanYT, test whether different duraitons to substract will affect the result   0901
# Family history name: parental_fh_3groups
# Body weight and fat 10/12

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
path_duration <- paste0("3. Result/multistate/sensitivity analyses duration/",Sys.Date())
dir.create(path_duration)

# 载入数据
load("./1. Database/project4.rdata")

tempfile <- project4
project4sens <- subset(tempfile,study_date != du_ep0001_date)

# 提取子集，增加运算速度;待调整好之后删除
# project4sens <- subset(project4sens,region_is_urban==0)

# 计算同一时间抵达多个阶段，用于之后的剔除同时抵达不同阶段的研究对象
# project4sens$sametime[project4sens$mltmbd_inc_date==project4sens$first_cmd_inc_date & project4sens$mltmbd_inc==1 & project4sens$first_cmd_inc==1] <- T
# project4sens$sametime[project4sens$du_ep0001_date==project4sens$first_cmd_inc_date & project4sens$du_ep0001==1 & project4sens$first_cmd_inc==1] <- T
# project4sens$sametime[project4sens$du_ep0001_date==project4sens$mltmbd_inc_date & project4sens$du_ep0001==1 & project4sens$mltmbd_inc==1] <- T
# project4sens$sametime[is.na(project4sens$sametime)] <- F
# tr(project4sens$sametime)
# sum(is.na(project4sens$sametime))
# table(project4sens$sametime)

# 时间换算为年龄（岁）
project4sens$study_date <- as.numeric(project4sens$study_date)
project4sens$first_cmd_inc_date <- as.numeric(project4sens$first_cmd_inc_date)
project4sens$mltmbd_inc_date <- as.numeric(project4sens$mltmbd_inc_date)
project4sens$du_ep0001_date <- as.numeric(project4sens$du_ep0001_date)
project4sens$dob_anon <- as.numeric(project4sens$dob_anon)

# 一、某些研究对象首次心血管代谢性疾病发生于2017年12月31日，在上述基础上加上0.5天
attach(project4sens)
project4sens$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4sens$mltmbd_inc_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5
project4sens$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] <- project4sens$du_ep0001_date[first_cmd_inc==1&mltmbd_inc==0&du_ep0001==0&first_cmd_inc_date==as.numeric(as.Date("31/12/17", "%d/%m/%y"))] + 0.5 
detach(project4sens)

# 计算年龄
project4sens[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")] <- (project4sens[, c("study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")]-project4sens$dob_anon)/365.25
summary(project4sens[, c("dob_anon","study_date", "first_cmd_inc_date", "mltmbd_inc_date", "du_ep0001_date")])
class(project4sens$dob_anon)

# # # gen original database
# 选取需要的变量进行分析，避免数据库过大
covariates <- c("studyid","age_strata","region_code","region_is_urban","is_female","highest_education","occupation",
                "marital_status_2groups","household_income","parental_fh_3groups", "menopause_2groups",
                "age_3groups","region_is_urban", "education_3groups")
phrases <- c("study_date","first_cmd_inc","first_cmd_inc_date","mltmbd_inc","mltmbd_inc_date","du_ep0001","du_ep0001_date")
lifestyles <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA","healthy_score_5groups","healthy_score",
                "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_PA","risky_obesity","risky_score_5groups","risky_score")
varlist <- c(covariates,phrases,lifestyles)

# # 数据库中存在不同阶段记录时间相同的个体，生成对应变量指代其类型 
attach(project4sens)
project4sens$sametime[first_cmd_inc==1 & du_ep0001 == 1 & du_ep0001_date==first_cmd_inc_date] <- 1      # 死于首次CMD
project4sens$sametime[first_cmd_inc==1 & mltmbd_inc == 1 & mltmbd_inc_date==first_cmd_inc_date] <- 2      # 首次心血管代谢性疾病时间与共病记录日期相同
project4sens$sametime[mltmbd_inc == 1 & du_ep0001 == 1 & mltmbd_inc_date==du_ep0001_date] <- 3      # 死于首次发生共病
project4sens$sametime[first_cmd_inc == 1 &  mltmbd_inc == 1 & du_ep0001 == 1 & mltmbd_inc_date==du_ep0001_date & first_cmd_inc_date==mltmbd_inc_date] <- 4      # 首次CMD、共病和死亡日期相同
project4sens$sametime[is.na(project4sens$sametime)] <- 0
table(project4sens$sametime)
detach(project4sens)

# 设置路径
tmat <- transMat(x = list(c(2, 4), c(3,4), c(4), c()), names = c("healthy", "fcmd", "mltmbd", "Death"))

# # # 大循环使用多个数据库
project4.1 <- project4sens
project4.2 <- project4sens
project4.3 <- project4sens
project4.4 <- project4sens
project4.5 <- project4sens

project4list <- list(project4.1,project4.2,project4.3,project4.4,project4.5)

#fcmd_datevector <- paste("first_cmd_inc_date",1:5,sep = "")
timevalue_vector <- c(0.5/365.25,0.5,1,3,5)
timevector <- c("0.5d","0.5y","1y","3y","5y")

#建立空 origin&long&full&final list
originlist <- list()
longlist <- list()
fulllist <- list()
finallist <- list()

for (i in 1:5){
    
    # # 对时间相同的不同阶段进行处理
    # 死于首次CMD
    project4list[[i]]$first_cmd_inc_date[project4list[[i]]$sametime==1] <- project4list[[i]]$du_ep0001_date[project4list[[i]]$sametime==1] - timevalue_vector[i]
    # 首次心血管代谢性疾病时间与共病记录日期相同
    project4list[[i]]$first_cmd_inc_date[project4list[[i]]$sametime==2] <- project4list[[i]]$mltmbd_inc_date[project4list[[i]]$sametime==2] - timevalue_vector[i]
    # 死于首次发生共病
    project4list[[i]]$mltmbd_inc_date[project4list[[i]]$sametime==3] <- project4list[[i]]$du_ep0001_date[project4list[[i]]$sametime==3] - timevalue_vector[i]
    project4list[[i]]$first_cmd_inc_date[project4list[[i]]$sametime==3&project4list[[i]]$first_cmd_inc_date>=project4list[[i]]$mltmbd_inc_date] <- 
        project4list[[i]]$mltmbd_inc_date[project4list[[i]]$sametime==3&project4list[[i]]$first_cmd_inc_date>=project4list[[i]]$mltmbd_inc_date] - timevalue_vector[i]

    # 首次CMD、共病和死亡日期相同
    project4list[[i]]$mltmbd_inc_date[project4list[[i]]$sametime==4] <- project4list[[i]]$du_ep0001_date[project4list[[i]]$sametime==4] - timevalue_vector[i]
    project4list[[i]]$first_cmd_inc_date[project4list[[i]]$sametime==4] <- project4list[[i]]$mltmbd_inc_date[project4list[[i]]$sametime==4] - timevalue_vector[i]
    
    # 仅保留需要的变量
    originlist[[i]] <- subset(project4list[[i]],first_cmd_inc_date>study_date, select = varlist)
    originlist[[i]]$enrollment <- 1
    
    # Wide to long
    longlist[[i]] <- msprep(data = originlist[[i]], trans = tmat,
                            time = c(NA, "first_cmd_inc_date", "mltmbd_inc_date","du_ep0001_date"),
                            status = c(NA, "first_cmd_inc", "mltmbd_inc", "du_ep0001"),
                            keep = c(covariates,lifestyles),
                            start = list(state = originlist[[i]]$enrollment,time = originlist[[i]]$study_date))
    sum(longlist[[i]]$time==0)
    
    # Set lifestyle score as category
    longlist[[i]]$rscore <- longlist[[i]]$risky_score
    longlist[[i]]$rscore.cat <- as.factor(longlist[[i]]$risky_score_5groups)
    
    # Transition-specific covariates
    long_covariates <- c("is_female","highest_education","occupation","marital_status_2groups","parental_fh_3groups","menopause_2groups",
                         "age_3groups","region_is_urban", "education_3groups")
    long_lf <- c("healthy_smoking","healthy_alcohol","healthy_diet","healthy_bmi","healthy_WC","healthy_sleep","healthy_PA",
                 "risky_smoking","risky_alcohol","risky_diet","risky_bmi","risky_WC","risky_sleep","risky_PA","rscore","rscore.cat","risky_obesity")
    fulllist[[i]] <- expand.covs(longlist[[i]], c(long_covariates,long_lf), longnames = FALSE)  # 均为哑变量
    
    # Drop observation of which time = 0 
    message(sum(fulllist[[i]]$time==0)," records was droped")
    finallist[[i]] <- subset(fulllist[[i]],time != 0)
    
    
    # # # # dichotomous single lifestyle;model 1 和 model2 差别很小，下面仅罗列了model2;家族史变量调一个和两个没有大的区别（05-24）
    single.sen <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                            as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) +
                            as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) +
                            as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                            as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                            risky_smoking.1 + risky_smoking.2 + risky_smoking.3 + risky_smoking.4 + risky_smoking.5 +
                            risky_alcohol.1 + risky_alcohol.2 + risky_alcohol.3 + risky_alcohol.4 + risky_alcohol.5 +
                            risky_diet.1 + risky_diet.2  + risky_diet.3 + risky_diet.4 + risky_diet.5 + 
                            risky_PA.1 + risky_PA.2  + risky_PA.3 + risky_PA.4 + risky_PA.5 +
                            risky_obesity.1 + risky_obesity.2  + risky_obesity.3 + risky_obesity.4 + risky_obesity.5,
                        data = finallist[[i]], method = "breslow")
    singlesen.hr2 <- data.frame(round(summary(single.sen)$conf.int,2))
    singlesen.hr2[,5] <- paste0(sprintf(singlesen.hr2[,1],fmt="%.2f")," (",sprintf(singlesen.hr2[,3],fmt="%.2f"),"-",sprintf(singlesen.hr2[,4],fmt="%.2f"),")")
    singlesen.hr2[,6] <- rownames(singlesen.hr2)
    singlesen.hr2[,6] <- substr(singlesen.hr2[,6],1,nchar(singlesen.hr2[,6])-2)
    singlesen.hr2[,7] <- 1:5
    singlesen.hr2 <- singlesen.hr2[(nrow(singlesen.hr2)-29):nrow(singlesen.hr2),]
    names(singlesen.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
    singlesen.hr2 <- dcast(singlesen.hr2,Variable~singlesen.hr2$Transition,value.var = "HR_95CI")
    write.csv(singlesen.hr2, paste0(path_duration,"/dichotomous single sensitivity",timevector[i],Sys.Date(),".csv",sep=" "))
    
    ### Calculate HR for lifestyle score (category) 
    # Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
    scorecat.sen <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                              as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                              as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                              as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                              as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                              rscore.cat1.1 + rscore.cat2.1 + rscore.cat3.1 + rscore.cat4.1 + 
                              rscore.cat1.2 + rscore.cat2.2 + rscore.cat3.2 + rscore.cat4.2 + 
                              rscore.cat1.3 + rscore.cat2.3 + rscore.cat3.3 + rscore.cat4.3 + 
                              rscore.cat1.4 + rscore.cat2.4 + rscore.cat3.4 + rscore.cat4.4 + 
                              rscore.cat1.5 + rscore.cat2.5 + rscore.cat3.5 + rscore.cat4.5  ,
                          data = finallist[[i]], method = "breslow")
    scorecatsen.hr2 <- data.frame(round(summary(scorecat.sen)$conf.int,2))
    scorecatsen.hr2[,5] <- paste0(sprintf(scorecatsen.hr2[,1],fmt="%.2f")," (",sprintf(scorecatsen.hr2[,3],fmt="%.2f"),"-",sprintf(scorecatsen.hr2[,4],fmt="%.2f"),")")
    scorecatsen.hr2[,6] <- rownames(scorecatsen.hr2)
    scorecatsen.hr2[,6] <- substr(scorecatsen.hr2[,6],1,nchar(scorecatsen.hr2[,6])-2)
    scorecatsen.hr2[,7] <- str_sub(rownames(scorecatsen.hr2),-1)
    scorecatsen.hr2 <- scorecatsen.hr2[(nrow(scorecatsen.hr2)-19):nrow(scorecatsen.hr2),]
    names(scorecatsen.hr2)[c(1,3,4,5,6,7)] <- c("HR","Lower_CI","Upper_CI","HR_95CI","Variable","Transition")
    scorecatsen.hr2 <- dcast(scorecatsen.hr2,Variable~scorecatsen.hr2$Transition,value.var = "HR_95CI")
    write.csv(scorecatsen.hr2, paste0(path_duration,"/category score sensitivity",timevector[i],Sys.Date(),".csv",sep=" "))
    
    ### Calculate HR for lifestyle score (ordinal) （各阶段协变量系数相同）
    # Model 2: model 1 + education,occupation, marital status, family history of multimorbidity
    scoreord.sen <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+ strata(region_code) + strata(age_strata) +
                              as.factor(is_female.1) + as.factor(is_female.2) + as.factor(is_female.3) + as.factor(is_female.4) + as.factor(is_female.5) + 
                              as.factor(highest_education.1) + as.factor(highest_education.2) + as.factor(highest_education.3) + as.factor(highest_education.4) + as.factor(highest_education.5) + 
                              as.factor(marital_status_2groups.1) + as.factor(marital_status_2groups.2) + as.factor(marital_status_2groups.3) + as.factor(marital_status_2groups.4) + as.factor(marital_status_2groups.5) + 
                              as.factor(parental_fh_3groups.1) + as.factor(parental_fh_3groups.2) + as.factor(parental_fh_3groups.3) + as.factor(parental_fh_3groups.4) + as.factor(parental_fh_3groups.5) + 
                              rscore.1 + rscore.2 + rscore.3 + rscore.4 + rscore.5,
                          data = finallist[[i]], method = "breslow")
    scoreordsen.hr2 <- data.frame(round(summary(scoreord.sen)$conf.int,2))
    scoreordsen.hr2[,5] <- paste0(sprintf(scoreordsen.hr2[,1],fmt="%.2f")," (",sprintf(scoreordsen.hr2[,3],fmt="%.2f"),"-",sprintf(scoreordsen.hr2[,4],fmt="%.2f"),")")
    write.csv(scoreordsen.hr2, paste0(path_duration,"/ordinal score sensitivity",timevector[i],Sys.Date(),".csv",sep=" "))
    
    print(i)
    
}
