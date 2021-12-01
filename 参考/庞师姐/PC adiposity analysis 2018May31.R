## Housekeeping: call library and set limit
library(Epi)
library(survival)

## Housekeeping: directory and import dataset
setwd('K:/kadoorie/Staff_Folders/YuanjieP/CKB dataset/Release 13')
load('K:/kadoorie/Staff_Folders/YuanjieP/CKB dataset/Release 13/R13_cancer_excl_17Dec08.RData')

## Housekeeping: check dataset
dim(ckb.data)
ckb <- ckb.data

## Housekeeping: check exclusions and outcomes - R13
dim(ckb)
summary(as.factor(ckb$cancer_diag)) # 510,132
summary(as.factor(ckb$c_ep0020.y)) # 796
summary(as.factor(ckb$c_ep0019.y)) # 2847
summary(as.factor(ckb$c_ep0018.y)) # 3024
summary(as.factor(ckb$c_ep4032.y)) # 1745
summary(as.factor(ckb$c_ep4033.y)) # 1716
summary(as.factor(ckb$c_ep0014.y)) # 26,594

## S2: outcome variables
## Housekeeping: rename outcome variables
ckb$c_ep0001 <- ckb$c_ep0020.y # pancreas 
ckb$c_ep0002 <- ckb$c_ep0019.y # liver 
ckb$c_ep0003 <- ckb$c_ep0018.y # colorectal
ckb$c_ep0004 <- ckb$c_ep4032.y # colon 
ckb$c_ep0005 <- ckb$c_ep4033.y # rectal 
ckb$c_ep0000 <- ckb$c_ep0014.y # total

ckb$c_ep0001_date <- ckb$c_ep0020_date.y 
ckb$c_ep0002_date <- ckb$c_ep0019_date.y 
ckb$c_ep0003_date <- ckb$c_ep0018_date.y 
ckb$c_ep0004_date <- ckb$c_ep4032_date.y 
ckb$c_ep0005_date <- ckb$c_ep4033_date.y 
ckb$c_ep0000_date <- ckb$c_ep0014_date.y 

## Housekeeping: time since birth
ckb$c_ep0001_time <- as.numeric(as.Date(substr(ckb$c_ep0001_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
ckb$c_ep0002_time <- as.numeric(as.Date(substr(ckb$c_ep0002_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
ckb$c_ep0003_time <- as.numeric(as.Date(substr(ckb$c_ep0003_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
ckb$c_ep0004_time <- as.numeric(as.Date(substr(ckb$c_ep0004_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
ckb$c_ep0005_time <- as.numeric(as.Date(substr(ckb$c_ep0005_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
ckb$c_ep0000_time <- as.numeric(as.Date(substr(ckb$c_ep0000_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
summary(ckb$c_ep0001_time)
summary(ckb$c_ep0002_time)
summary(ckb$c_ep0003_time)
summary(ckb$c_ep0004_time)
summary(ckb$c_ep0005_time)
summary(ckb$c_ep0000_time)

## time since baseline  ???
ckb$c_ep0001_time_baseline <- as.numeric(as.Date(substr(ckb$c_ep0001_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$study_date, 1, 9), "%d%b%Y")) / 365.25
ckb$c_ep0002_time_baseline <- as.numeric(as.Date(substr(ckb$c_ep0002_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$study_date, 1, 9), "%d%b%Y")) / 365.25
ckb$c_ep0003_time_baseline <- as.numeric(as.Date(substr(ckb$c_ep0003_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$study_date, 1, 9), "%d%b%Y")) / 365.25
ckb$c_ep0004_time_baseline <- as.numeric(as.Date(substr(ckb$c_ep0004_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$study_date, 1, 9), "%d%b%Y")) / 365.25
ckb$c_ep0005_time_baseline <- as.numeric(as.Date(substr(ckb$c_ep0005_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$study_date, 1, 9), "%d%b%Y")) / 365.25
ckb$c_ep0000_time_baseline <- as.numeric(as.Date(substr(ckb$c_ep0000_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$study_date, 1, 9), "%d%b%Y")) / 365.25
summary(ckb$c_ep0001_time_baseline)
summary(ckb$c_ep0002_time_baseline)
summary(ckb$c_ep0003_time_baseline)
summary(ckb$c_ep0004_time_baseline)
summary(ckb$c_ep0005_time_baseline)
summary(ckb$c_ep0000_time_baseline)

## time since birth for undelying death
ckb$du_ep0001 <- ckb$du_ep0020.y # pancreas 
ckb$du_ep0002 <- ckb$du_ep0019.y # liver 
ckb$du_ep0003 <- ckb$du_ep0018.y # colorectal
ckb$du_ep0004 <- ckb$du_ep4032 # colon 
ckb$du_ep0005 <- ckb$du_ep4033 # rectal 
ckb$du_ep0000 <- ckb$du_ep0014.y # total

ckb$du_ep0001_date <- ckb$du_ep0020_date.y 
ckb$du_ep0002_date <- ckb$du_ep0019_date.y 
ckb$du_ep0003_date <- ckb$du_ep0018_date.y 
ckb$du_ep0004_date <- ckb$du_ep4032_date 
ckb$du_ep0005_date <- ckb$du_ep4033_date
ckb$du_ep0000_date <- ckb$du_ep0014_date.y

summary(as.factor(ckb$du_ep0020.y)) # 569
summary(as.factor(ckb$du_ep0019.y)) # 2021
summary(as.factor(ckb$du_ep0018.y)) # 961
summary(as.factor(ckb$du_ep4032)) # 429
summary(as.factor(ckb$du_ep4033)) # 518
summary(as.factor(ckb$du_ep0014.y)) # 13,305

ckb$du_ep0001_time <- as.numeric(as.Date(substr(ckb$du_ep0001_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
ckb$du_ep0002_time <- as.numeric(as.Date(substr(ckb$du_ep0002_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
ckb$du_ep0003_time <- as.numeric(as.Date(substr(ckb$du_ep0001_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
ckb$du_ep0004_time <- as.numeric(as.Date(substr(ckb$du_ep0004_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
ckb$du_ep0005_time <- as.numeric(as.Date(substr(ckb$du_ep0005_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
ckb$du_ep0000_time <- as.numeric(as.Date(substr(ckb$du_ep0000_date, 1, 7), "%d%b%y") - as.Date(substr(ckb$dob, 1, 9), "%d%b%Y")) / 365.25
summary(ckb$du_ep0001_time)
summary(ckb$du_ep0002_time)
summary(ckb$du_ep0001_time)
summary(ckb$du_ep0004_time)
summary(ckb$du_ep0005_time)
summary(ckb$du_ep0000_time)

## S3: exposure variables
## 1. adiposity categories
## BMI baseline - 5 categories
ckb$bmi_5cat[ckb$bmi_calc<20 & !is.na(ckb$bmi_calc)] <- 1
ckb$bmi_5cat[ckb$bmi_calc>=20 & ckb$bmi_calc<22.5 & !is.na(ckb$bmi_calc)] <- 0
ckb$bmi_5cat[ckb$bmi_calc>=22.5 & ckb$bmi_calc<25 & !is.na(ckb$bmi_calc)] <- 2
ckb$bmi_5cat[ckb$bmi_calc>=25 & ckb$bmi_calc<27 & !is.na(ckb$bmi_calc)] <- 3
ckb$bmi_5cat[ckb$bmi_calc>=27 & !is.na(ckb$bmi_calc)] <- 4
summary(as.factor(ckb$bmi_5cat))

## BMI age 25 - 5 categories
ckb$bmi_25 <- NA
ckb$bmi_25 <- (0.5*ckb$age_25_weight_jin)/((0.001*ckb$standing_height_mm)*(0.001*ckb$standing_height_mm))
summary(ckb$bmi_25) # missing 82,023

ckb$bmi25_5cat[ckb$bmi_25<20 & !is.na(ckb$bmi_25)] <- 1
ckb$bmi25_5cat[ckb$bmi_25>=20 & ckb$bmi_25<22.5 & !is.na(ckb$bmi_25)] <- 0
ckb$bmi25_5cat[ckb$bmi_25>=22.5 & ckb$bmi_25<25 & !is.na(ckb$bmi_25)] <- 2
ckb$bmi25_5cat[ckb$bmi_25>=25 & ckb$bmi_25<27 & !is.na(ckb$bmi_25)] <- 3
ckb$bmi25_5cat[ckb$bmi_25>=27& !is.na(ckb$bmi_25)] <- 4
summary(as.factor(ckb$bmi25_5cat))

## adiposity variables
ckb$fat_percent <- 0.1*ckb$fat_percent_x10
ckb$weight_kg <- 0.1*ckb$weight_kg_x10
ckb$wt_change_kg <- ckb$weight_kg - 0.5*ckb$age_25_weight_jin
ckb$leg_mm <- ckb$standing_height_mm - ckb$sitting_height_mm
ckb$fat_kg <- ckb$weight_kg*ckb$fat_percent*0.01
ckb$lean_kg <- ckb$weight_kg - ckb$fat_kg
ckb$waist_height_ratio <- ckb$waist_mm/ckb$standing_height_mm

## WHO classification for BMI 
ckb$bmi_who0[ckb$bmi_calc<18.5 & !is.na(ckb$bmi_calc)] <- 1
ckb$bmi_who0[ckb$bmi_calc>=18.5 & ckb$bmi_calc<22.5 & !is.na(ckb$bmi_calc)] <- 0
ckb$bmi_who0[ckb$bmi_calc>=22.5 & ckb$bmi_calc<25 & !is.na(ckb$bmi_calc)] <- 2
ckb$bmi_who0[ckb$bmi_calc>=25 & ckb$bmi_calc<27.5 & !is.na(ckb$bmi_calc)] <- 3
ckb$bmi_who0[ckb$bmi_calc>=27.5 & ckb$bmi_calc<30 & !is.na(ckb$bmi_calc)] <- 4
ckb$bmi_who0[ckb$bmi_calc>=30 & !is.na(ckb$bmi_calc)] <- 5
summary(as.factor(ckb$bmi_who0))

ckb$bmi_who1[ckb$bmi_calc<18.5 & !is.na(ckb$bmi_calc)] <- 1
ckb$bmi_who1[ckb$bmi_calc>=18.5 & ckb$bmi_calc<25 & !is.na(ckb$bmi_calc)] <- 0
ckb$bmi_who1[ckb$bmi_calc>=25 & ckb$bmi_calc<30 & !is.na(ckb$bmi_calc)] <- 2
ckb$bmi_who1[ckb$bmi_calc>=30 & !is.na(ckb$bmi_calc)] <- 3
summary(as.factor(ckb$bmi_who1))

## extreme categories for WC
ckb$wc_5cat <- NA
ckb$wc_5cat[ckb$waist_mm<700 & !is.na(ckb$waist_mm)] <- 1
ckb$wc_5cat[ckb$waist_mm>=700 & ckb$waist_mm<800 & !is.na(ckb$waist_mm)] <- 0
ckb$wc_5cat[ckb$waist_mm>=800 & ckb$waist_mm<900 & !is.na(ckb$waist_mm)] <- 2
ckb$wc_5cat[ckb$waist_mm>=900 & ckb$waist_mm<1000 & !is.na(ckb$waist_mm)] <- 3
ckb$wc_5cat[ckb$waist_mm>=1000 & !is.na(ckb$waist_mm)] <- 4
summary(as.factor(ckb$wc_5cat))

ckb$wc_6cat <- NA
ckb$wc_6cat[ckb$waist_mm<700 & !is.na(ckb$waist_mm)] <- 1
ckb$wc_6cat[ckb$waist_mm>=700 & ckb$waist_mm<800 & !is.na(ckb$waist_mm)] <- 0
ckb$wc_6cat[ckb$waist_mm>=800 & ckb$waist_mm<850 & !is.na(ckb$waist_mm)] <- 2
ckb$wc_6cat[ckb$waist_mm>=850 & ckb$waist_mm<900 & !is.na(ckb$waist_mm)] <- 3
ckb$wc_6cat[ckb$waist_mm>=900 & ckb$waist_mm<1000 & !is.na(ckb$waist_mm)] <- 4
ckb$wc_6cat[ckb$waist_mm>=1000 & !is.na(ckb$waist_mm)] <- 5
summary(as.factor(ckb$wc_6cat))

## 2. diabetes and RPG
## 3-category for diabetes
ckb$diabts_3cat[ckb$has_diabetes==0] <- 0
ckb$diabts_3cat[ckb$has_diabetes==1 & ckb$diabetes_diag==0] <- 1  # screen-detected
ckb$diabts_3cat[ckb$diabetes_diag==1] <- 2  # self-reported
summary(as.factor(ckb$diabts_3cat))

## glucose
summary(ckb$random_glucose_x10) # missing 8121
ckb$random_glucose <- NA
ckb$random_glucose[ckb$random_glucose_x10>0 & !is.na(ckb$random_glucose_x10)] <- 0.1*ckb$random_glucose_x10[ckb$random_glucose_x10>0 & !is.na(ckb$random_glucose_x10)]
ckb$random_glucose[ckb$diabetes_diag==1] <- NA
summary(ckb$random_glucose) # among participants without self-reported DM
summary(as.factor(ckb$c_ep0001[!is.na(ckb$random_glucose)]))

## glucose 4 cat in non-DM (cut-offs as in the IJC paper)
ckb$glucose_4cat <- NA
ckb$glucose_4cat[ckb$random_glucose>0 & ckb$random_glucose<=5.5 & ckb$diabetes_diag==0] <- 0
ckb$glucose_4cat[ckb$random_glucose>5.5 & ckb$random_glucose<6.8 & ckb$diabetes_diag==0] <- 1
ckb$glucose_4cat[ckb$random_glucose>=6.8 & ckb$random_glucose<7.8 & ckb$diabetes_diag==0] <- 2
ckb$glucose_4cat[ckb$random_glucose>=7.8 & ckb$diabetes_diag==0] <- 3
ckb$glucose_4cat[ckb$diabetes_diag==1] <- NA
summary(as.factor(ckb$glucose_4cat))

## 3. PA and SLT
## total PA - quintiles
ckb$pa_q <- NA
ckb$pa_q[ckb$met<8.72] <- 0
ckb$pa_q[ckb$met>=8.72 & ckb$met<14.2] <- 1
ckb$pa_q[ckb$met>=14.2 & ckb$met<21.8] <- 2
ckb$pa_q[ckb$met>=21.8 & ckb$met<33.2] <- 3
ckb$pa_q[ckb$met>=33.2] <- 4
summary(as.factor(ckb$pa_q))

## sedentary leisure time - 5 categories
ckb$sitting_hours_day <- 1/7*ckb$tv_reading_hours
summary(ckb$sitting_hours_day)
ckb$slt_5cat <- NA
ckb$slt_5cat[ckb$sitting_hours_day<2] <- 0
ckb$slt_5cat[ckb$sitting_hours_day>=2 & ckb$sitting_hours_day<3] <- 1
ckb$slt_5cat[ckb$sitting_hours_day>=3 & ckb$sitting_hours_day<4] <- 2
ckb$slt_5cat[ckb$sitting_hours_day>=4 & ckb$sitting_hours_day<5] <- 3
ckb$slt_5cat[ckb$sitting_hours_day>=5] <- 4
summary(as.factor(ckb$slt_5cat))


## 5. linear and z-score
## adiposity SD
ckb$bmi_z <- scale(ckb$bmi_calc, center = TRUE, scale = TRUE)
summary(ckb$bmi_z)
ckb$bmi25_z <- scale(ckb$bmi_25, center = TRUE, scale = TRUE)
summary(ckb$bmi25_z)
ckb$wc_z <- scale(ckb$waist_mm, center = TRUE, scale = TRUE)
summary(ckb$wc_z)
ckb$whr_z <- scale(ckb$waist_hip_ratio, center = TRUE, scale = TRUE)
summary(ckb$whr_z)
ckb$whtr_z <- scale(ckb$waist_height_ratio, center = TRUE, scale = TRUE)
summary(ckb$whtr_z)
ckb$hc_z <- scale(ckb$hip_mm, center = TRUE, scale = TRUE)
summary(ckb$hc_z)
ckb$fat_z <- scale(ckb$fat_percent, center = TRUE, scale = TRUE)
summary(ckb$fat_z)
ckb$wt_z <- scale(ckb$weight_kg, center = TRUE, scale = TRUE)
summary(ckb$wt_z)
ckb$change_z <- scale(ckb$wt_change_kg, center = TRUE, scale = TRUE)
summary(ckb$change_z)
ckb$ht_z <- scale(ckb$standing_height_mm, center = TRUE, scale = TRUE)
summary(ckb$ht_z)
ckb$leg_z <- scale(ckb$leg_mm, center = TRUE, scale = TRUE)
summary(ckb$leg_z)
ckb$leanm_z <- scale(ckb$lean_kg, center = TRUE, scale = TRUE)
ckb$fatm_z <- scale(ckb$fat_kg, center = TRUE, scale = TRUE)
summary(ckb$leanm_z)
summary(ckb$fatm_z)

## adiposity RDR
ckb$bmi_in5 <- 0.2*ckb$bmi_calc
ckb$bmi25_in5 <- 0.2*ckb$bmi_25
ckb$bmi_z_rdr <- 0.9348807*ckb$bmi_z
ckb$wc_z_rdr <- 0.8418681*ckb$wc_z
ckb$whtr_z_rdr <- 0.8418681*ckb$whtr_z
ckb$waist_unit <- 0.8418681*0.01*ckb$waist_mm
ckb$hip_unit <- 0.8157891*0.01*ckb$hip_mm
ckb$whr_unit <- 0.6784283*ckb$waist_hip_ratio*10
ckb$hc_z_rdr <- 0.8157891*ckb$hc_z
ckb$whr_z_rdr <- 0.6784283*ckb$whr_z
ckb$fat_z_rdr <- 0.8988488*ckb$fat_z
ckb$wt_z_rdr <- 0.9601658*ckb$wt_z
ckb$ht_z_rdr <- 0.9601658*ckb$ht_z  ## ??
ckb$ht_unit <- 0.9601658*0.01*ckb$standing_height_mm  ## ??
ckb$leg_z_rdr <- 0.9601658*ckb$leg_z  ## ??
ckb$leg_unit <- 0.9601658*0.01*ckb$leg_mm  ## ??
ckb$fatm_z_rdr <- 0.9086061*ckb$fatm_z
ckb$leanm_z_rdr <- 0.9600969*ckb$leanm_z
ckb$fat_in10 <- 0.8988488*0.1*ckb$fat_percent

## RPG RDR
ckb$rpg_unit <- ckb$random_glucose
ckb$rpg_rdr <- 0.56*ckb$rpg_unit
summary(ckb$rpg_unit)
summary(ckb$rpg_rdr)

## PA and SLT RDR
ckb$met_sd <- 1/14*0.52*ckb$met
ckb$slt_sd <- 2/3*0.31*ckb$sitting_hours_day

## S4: other variables
## smoking (1:never, 2:occasional, 3:ex-regular, 4:regular)
ckb$smoking_3cat[ckb$smoking_category==1 | ckb$smoking_category==2] <- 0
ckb$smoking_3cat[ckb$smoking_category==3] <- 1
ckb$smoking_3cat[ckb$smoking_category==4 | ckb$smoking_stopped_reason==0] <- 2
summary(as.factor(ckb$smoking_3cat)) 

## cigeratte category 
ckb$cig_4cat <- NA
ckb$cig_4cat[ckb$smoking_3cat==0] <- 0
ckb$cig_4cat[ckb$cig_equiv_day>0.01 & ckb$cig_equiv_day<20] <- 1
ckb$cig_4cat[ckb$cig_equiv_day>=20 & ckb$cig_equiv_day<25] <- 2
ckb$cig_4cat[ckb$cig_equiv_day>=25] <- 3
summary(as.factor(ckb$cig_4cat)) 

## smoking in adjustment
ckb$smoking_adj[ckb$smoking_category==1] <- 0
ckb$smoking_adj[ckb$smoking_category==2] <- 1
ckb$smoking_adj[ckb$smoking_category==3] <- 2
ckb$smoking_adj[ckb$smoking_category==4 & ckb$cig_equiv_day>0.01 & ckb$cig_equiv_day<20] <- 3
ckb$smoking_adj[ckb$smoking_category==4 & ckb$cig_equiv_day>=20 & ckb$cig_equiv_day<25] <- 4
ckb$smoking_adj[ckb$smoking_category==4 & ckb$cig_equiv_day>=25] <- 5
summary(as.factor(ckb$smoking_adj)) 

## alcohol
summary(ckb$total_alc_av_week_g[ckb$alcohol_category==6])
summary(ckb$total_alc_av_week_g)
ckb$alc_in10 <- 0.1*ckb$total_alc_av_week_g
summary(ckb$alc_in10)
ckb$alc_cat <-NA
ckb$alc_cat[ckb$alcohol_category==1] <- 0
ckb$alc_cat[ckb$alcohol_category==6 & ckb$total_alc_av_week_g<140] <- 1
ckb$alc_cat[ckb$alcohol_category==6 & ckb$total_alc_av_week_g>=140 & ckb$total_alc_av_week_g<420] <- 2
ckb$alc_cat[ckb$alcohol_category==6 & ckb$total_alc_av_week_g>=420] <- 3
summary(as.factor(ckb$alc_cat))

## alcohol in adjustment
ckb$alcohol_adj[ckb$alcohol_category==1] <- 0
ckb$alcohol_adj[ckb$alcohol_category==2] <- 1
ckb$alcohol_adj[ckb$alcohol_category==3] <- 2
ckb$alcohol_adj[ckb$alcohol_category==4] <- 3
ckb$alcohol_adj[ckb$alcohol_category==5] <- 4
ckb$alcohol_adj[ckb$alcohol_category==6 & ckb$total_alc_av_week_g<140] <- 5
ckb$alcohol_adj[ckb$alcohol_category==6 & ckb$total_alc_av_week_g>=140 & ckb$total_alc_av_week_g<420] <- 6
ckb$alcohol_adj[ckb$alcohol_category==6 & ckb$total_alc_av_week_g>=420] <- 7
summary(as.factor(ckb$alcohol_adj))

## heavy alcohol typical
ckb$alc_heavy <- NA
ckb$alc_heavy[ckb$alcohol_category==6] <- 0
ckb$alc_heavy[(ckb$total_alc_typ_day_g>60 & ckb$is_female==0) | (ckb$total_alc_typ_day_g>40 & ckb$is_female==1)] <- 1
ckb$alc_heavy2 <- 0 # heavy alcohol all occasions
ckb$alc_heavy2[(ckb$total_alc_typ_day_g>60 & ckb$is_female==0) | (ckb$total_alc_typ_day_g>40 & ckb$is_female==1)] <- 1

## family cancer
summary(as.factor(ckb$siblings_cancer))
summary(as.factor(ckb$children_cancer))
summary(as.factor(ckb$father_cancer))
summary(as.factor(ckb$mother_cancer))
ckb$family_cancer <- 0
ckb$family_cancer[ckb$father_cancer==1 | ckb$mother_cancer==1] <- 1
ckb$family_cancer[ckb$siblings_cancer>1] <- 2
summary(as.factor(ckb$family_cancer))

## HBV
summary(ckb$hep_b)
ckb$hbv <- 0
ckb$hbv[ckb$hep_b>=1 & ckb$hep_b<6] <- 1
ckb$hbv[is.na(ckb$hep_b)] <- 2
summary(as.factor(ckb$hbv))


##################################################################################

echo <- ckb

############### TABLE - categories ###############

##########################################################
### Table 1: 5 key & 5 other traits - all participants ###

# model 1: basic model 
# model 2: add diabetes
outputpath <- "J:/T1_category_2model.csv"
paradigit <- 2
expolist <- c("bmi_5cat","wc_q","whr_q","fat_q","bmi25_5cat",
              "hc_q","whtr_q")

for (exponame in expolist) {
  eval(parse(text=paste0("echo$exposure <- echo$",exponame)))
  restable <- NULL
  
  for (modelid in 1:2) {
    if (modelid == 1) {
      modelf <- float(with(echo,coxph(Surv(age_at_study_date, c_ep0001_time, c_ep0001) ~ as.factor(exposure) + age_at_study_date + strata(is_female) + strata(region_code)  
                                      + as.factor(smoking_adj) + as.factor(alcohol_adj) + as.factor(family_cancer) + as.factor(self_rated_health) + as.factor(highest_education) + as.factor(household_income))))
    } else if (modelid == 2) {
      modelf <- float(with(echo,coxph(Surv(age_at_study_date, c_ep0001_time, c_ep0001) ~ as.factor(exposure) + age_at_study_date + strata(is_female) + strata(region_code) 
                                      + as.factor(smoking_adj) + as.factor(alcohol_adj) + as.factor(family_cancer) + as.factor(self_rated_health) + as.factor(highest_education) + as.factor(household_income) + has_diabetes)))
    }
    
    modelcoef <- sprintf(paste0("%.",paradigit,"f"), exp(modelf$coef))
    modellb <- exp(modelf$coef-1.96*sqrt(modelf$var))
    modelub <- exp(modelf$coef+1.96*sqrt(modelf$var))
    modelci <- matrix(sprintf(paste0("%.",paradigit,"f"), c(modellb, modelub)), ncol=2)
    case <- table(echo$exposure, echo$c_ep0001)[,2]
    
    coefinfo <- NULL
    for (singlecoefid in 1:length(modelcoef)) {
      modelsinglecoef <- paste0(modelcoef[singlecoefid], " (", modelci[singlecoefid,1], ", ", modelci[singlecoefid,2], ")")
      coefinfo <- c(coefinfo,modelsinglecoef)
    }
    
    restable <- rbind(restable,c(coefinfo),case)
    
  }
  
  row.names(restable) <- paste0(c("Model 1", "Case", "Model 2", "Case"))   
  cat(paste("Exposure:",exponame,"\n\n"), file=outputpath, append=T)
  cat(",Model 1,Case,Model 2,Case\n", file=outputpath, append=T)
  write.table(t(restable), file=outputpath, append=T, sep=",",col.names = F)
  cat("\n\n", file=outputpath, append=T)
}

###############  add height  ###############
outputpath <- "J:/T1_category_ht_2model.csv"
paradigit <- 2
expolist <- c("weight_q","fatm_q","leanm_q")

for (exponame in expolist) {
  eval(parse(text=paste0("echo$exposure <- echo$",exponame)))
  restable <- NULL
  
  for (modelid in 1:2) {
    if (modelid == 1) {
      modelf <- float(with(echo,coxph(Surv(age_at_study_date, c_ep0001_time, c_ep0001) ~ as.factor(exposure) + age_at_study_date + strata(is_female) + strata(region_code) 
                                      + as.factor(smoking_adj) + as.factor(alcohol_adj) + as.factor(family_cancer) + as.factor(self_rated_health) + as.factor(highest_education) + as.factor(household_income) + standing_height_mm)))
    } else if (modelid == 2) {
      modelf <- float(with(echo,coxph(Surv(age_at_study_date, c_ep0001_time, c_ep0001) ~ as.factor(exposure) + age_at_study_date + strata(is_female) + strata(region_code) 
                                      + as.factor(smoking_adj) + as.factor(alcohol_adj) + as.factor(family_cancer) + as.factor(self_rated_health) + as.factor(highest_education) + as.factor(household_income) + standing_height_mm + has_diabetes)))
    }
    
    modelcoef <- sprintf(paste0("%.",paradigit,"f"), exp(modelf$coef))
    modellb <- exp(modelf$coef-1.96*sqrt(modelf$var))
    modelub <- exp(modelf$coef+1.96*sqrt(modelf$var))
    modelci <- matrix(sprintf(paste0("%.",paradigit,"f"), c(modellb, modelub)), ncol=2)
    case <- table(echo$exposure, echo$c_ep0001)[,2]
    
    coefinfo <- NULL 
    for (singlecoefid in 1:length(modelcoef)) {
      modelsinglecoef <- paste0(modelcoef[singlecoefid], " (", modelci[singlecoefid,1], ", ", modelci[singlecoefid,2], ")")
      coefinfo <- c(coefinfo,modelsinglecoef)
    }
    
    restable <- rbind(restable,c(coefinfo),case)
    
  }
  
  row.names(restable) <- paste0(c("Model 1", "Case", "Model 2", "Case"))   
  cat(paste("Exposure:",exponame,"\n\n"), file=outputpath, append=T)
  cat(",Model 1,Case,Model 2,Case\n", file=outputpath, append=T)
  write.table(t(restable), file=outputpath, append=T, sep=",",col.names = F)
  cat("\n\n", file=outputpath, append=T)
}

############### linear 1 ###############
outputpath <- "J:/T1_linear_2model.csv"
paradigit <- 2
expolist <- c("hip_unit", "whtr_z_rdr","bmi_z_rdr","wc_z_rdr","whr_z_rdr","fat_z_rdr","bmi25_z","hc_z_rdr")
#expolist <- c("bmi_in5","waist_unit","whr_unit","fat_in10","bmi25_in5",
#              "hip_unit", "whtr_z_rdr","bmi_z_rdr","wc_z_rdr","whr_z_rdr","fat_z_rdr","bmi25_z","hc_z_rdr")

for (exponame in expolist) {
  eval(parse(text=paste0("echo$exposure <- echo$",exponame)))
  restable <- NULL
  
  for (modelid in 1:2) {
    if (modelid == 1) {
      model <- with(echo,coxph(Surv(age_at_study_date, c_ep0001_time, c_ep0001) ~ age_at_study_date + strata(is_female) + strata(region_code)  
                               + as.factor(smoking_adj) + as.factor(alcohol_adj) + as.factor(family_cancer) + as.factor(self_rated_health) + as.factor(highest_education) + as.factor(household_income) + exposure))
    } else  if (modelid == 2) {
      model <- with(echo,coxph(Surv(age_at_study_date, c_ep0001_time, c_ep0001) ~ age_at_study_date + strata(is_female) + strata(region_code)  
                               + as.factor(smoking_adj) + as.factor(alcohol_adj) + as.factor(family_cancer) + as.factor(self_rated_health) + as.factor(highest_education) + as.factor(household_income) + has_diabetes + exposure))
    }      
    modelcoef <- sprintf(paste0("%.",paradigit,"f"),exp(model$coefficients[(length(model$coefficients)-length(levels(echo$exposure))):length(model$coefficients)]))
    modelci <- matrix(sprintf(paste0("%.",paradigit,"f"),exp(confint(model)[(length(model$coefficients)-length(levels(echo$exposure))):length(model$coefficients),])),ncol=2)
    modelpval <- sprintf(paste0("%.",paradigit,"f"),summary(model)[[7]][(nrow(summary(model)[[7]])-length(levels(echo$exposure))):(nrow(summary(model)[[7]])),5])
    
    modelQ2 <- paste0(modelcoef[1], " (", modelci[1,1], ", ", modelci[1,2], ")")
    if (modelpval[1] < 0.001) {
      modelQ2pval <- "<0.001"
    } else {
      modelQ2pval <- modelpval[1]
    }            
    
    
    restable <- rbind(restable, c(modelQ2,modelQ2pval))
    
  }
  
  
  row.names(restable) <- paste0("Model", 1:2)      
  cat(paste("Exposure:",exponame,"\n\n"), file=outputpath, append=T)
  cat(",Q2,p value\n", file=outputpath, append=T)
  write.table(restable, file=outputpath, append=T, sep=",",col.names = F)
  cat("\n\n", file=outputpath, append=T)
}
