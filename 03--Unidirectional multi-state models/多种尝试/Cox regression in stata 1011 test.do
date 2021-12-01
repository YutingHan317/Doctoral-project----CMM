***************************************************************************************************************
*****************************                                       *******************************************
*****************************           COX regression              *******************************************
*****************************                                       *******************************************
***************************************************************************************************************
*** analyse risk effects instead of protective effects

cap mkdir "D:\HanYT\2019-06 Multimorbidity\3. Result\multistate\COX regression"
log using "D:\HanYT\2019-06 Multimorbidity\3. Result\multistate\COX regression\cox regression.log",replace 
*****************************************      单因素      *******************************

/* 设置全局宏 */
global adjusted_cox i.is_female i.highest_education i.marital_status_2groups i.parental_fh_3groups 

/* Stset */
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta", clear
stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001 == 1)

di "*********bmi 18.5-24; WC 85/90"
stcox risky_bmi risky_WC risky_smoking risky_alcohol risky_diet risky_PA $adjusted_cox, strata(age_strata region_code)

di "*********bmi 18.5-24; WC 80/85"
stcox risky_bmi risky_WC2 risky_smoking risky_alcohol risky_diet risky_PA $adjusted_cox, strata(age_strata region_code)

di "*********bmi 18.5-24; WC 85"
stcox risky_bmi risky_WC3 risky_smoking risky_alcohol risky_diet risky_PA $adjusted_cox, strata(age_strata region_code)

di "*********bmi 18.5-28; WC 85/90"
stcox risky_bmi2 risky_WC risky_smoking risky_alcohol risky_diet risky_PA $adjusted_cox, strata(age_strata region_code)

di "*********bmi 18.5-28; WC 85"
stcox risky_bmi2 risky_WC3 risky_smoking risky_alcohol risky_diet risky_PA $adjusted_cox, strata(age_strata region_code)

di "*********Combined bmi 18.5-28; WC 85/90"
stcox risky_obesity risky_smoking risky_alcohol risky_diet risky_PA $adjusted_cox, strata(age_strata region_code)

di "*********Combined bmi 18.5-28; WC 80/85"
stcox risky_obesity2 risky_smoking risky_alcohol risky_diet risky_PA $adjusted_cox, strata(age_strata region_code)

di "*********bmi 9groups; WC 8groups"
stcox i.bmi_9groups i.WC_8groups risky_smoking risky_alcohol risky_diet risky_PA $adjusted_cox, strata(age_strata region_code)

di "*********bmi 9groups; WC 2groups"
stcox i.bmi_9groups i.risky_WC risky_smoking risky_alcohol risky_diet risky_PA $adjusted_cox, strata(age_strata region_code)
