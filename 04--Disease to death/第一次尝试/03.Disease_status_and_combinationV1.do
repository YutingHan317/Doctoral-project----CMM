********************************************


**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                 Part 3: Disease status and combination                         *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
/* 目前的调整逻辑 */
* 模型1： 性别、教育程度、收入、婚姻状态、共病家族史、吸烟五分组、饮酒七分组、饮食五分组、BMI4分组、WC三分组、体力活动五分组
* 模型2： 模型1+高血压、慢性肾病、风湿性心脏病
* 模型3： 模型2基础上调整药物（降压药（分开还是合并、阿司匹林、降脂药（statin）


/* 已经考虑的问题 */
* 生活方式按照多分组调整和两分组调整结果基本一致

/* 变量特征 */
cd "D:/HanYT/2021-02_CMMtodeath"

***************************************************************************************************************
*****************************                                       *******************************************
*****************************           Exploring analyses          *******************************************
*****************************                                       *******************************************
***************************************************************************************************************
log using "2.Result/Log/test 0714.log", append


********************************************************************************************************************************************************************************
di "********************************** baseline CMDs"
use "1.Data/project2.dta", clear
/* 设置全局宏 */
*** Covariates for adjustment
global adjusted_model1 i.is_female 
global adjusted_model2 $adjusted_model1 i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups
global adjusted_model3 $adjusted_model2 i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups
global adjusted_model4 $adjusted_model3 i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001 == 1)
*** 先依次调整社会经济学指标和生活方式
* 这个调整没意义
stcox i.d_number_base $adjusted_model1, strata(age_strata region_code) cformat(%9.3f) 
stcox i.d_number_base $adjusted_model2, strata(age_strata region_code) cformat(%9.3f) 
* 至少应该调整到生活方式变量
stcox i.d_number_base $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 

*** 在考虑基线同时存在的疾病
* 变化大
stcox i.d_number_base $adjusted_model3 i.has_hypertension, strata(age_strata region_code) cformat(%9.3f)
* 变化很小
stcox i.d_number_base $adjusted_model3 i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.3f)
* 变化很小
stcox i.d_number_base $adjusted_model3 i.rheum_heart_dis_diag, strata(age_strata region_code) cformat(%9.3f) 

* 同时考虑三种 效应值跟调整高血压的差不多
stcox i.d_number_base $adjusted_model3 i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag, strata(age_strata region_code) cformat(%9.3f)

*** 再考虑药物使用
* taking_aspirin taking_ace_i taking_beta_blocker taking_statins taking_diuretics taking_ca_antagonist taking_chlor_metaformin taking_insulin
* 变化很小
stcox i.d_number_base i.taking_aspirin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f)
* 变化较小
stcox i.d_number_base i.taking_statins_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较小
stcox i.d_number_base i.taking_ace_i_2groups i.taking_beta_blocker_2groups taking_diuretics_2groups taking_ca_antagonist_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较小
stcox i.d_number_base i.hypertension_lowering_2g $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较大
stcox i.d_number_base i.taking_chlor_metaformin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较大，跟口服降糖药类似
stcox i.d_number_base i.taking_insulin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化大
stcox i.d_number_base i.glucose_lowering_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 



********************************************************************************************************************************************************************************
di "********************************** updated CMDs"
use "1.Data/project2_updated_du_ep0001.dta", clear
/* 设置全局宏 */
*** Covariates for adjustment
global adjusted_model1 i.is_female 
global adjusted_model2 $adjusted_model1 i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups
global adjusted_model3 $adjusted_model2 i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups
global adjusted_model4 $adjusted_model3 i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001 == 1)
*** 先依次调整社会经济学指标和生活方式
stcox i.d_number_updated $adjusted_model1, strata(age_strata region_code) cformat(%9.3f) 
stcox i.d_number_updated $adjusted_model2, strata(age_strata region_code) cformat(%9.3f) 
stcox i.d_number_updated $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 

*** 在考虑基线同时存在的疾病
* 变化较大
stcox i.d_number_updated $adjusted_model3 i.has_hypertension, strata(age_strata region_code) cformat(%9.3f) 
* 变化较小
stcox i.d_number_updated $adjusted_model3 i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.3f) 
* 变化较小
stcox i.d_number_updated $adjusted_model3 i.rheum_heart_dis_diag, strata(age_strata region_code) cformat(%9.3f) 

* 同时考虑三种 效应值跟调整高血压的差不多
stcox i.d_number_updated $adjusted_model3 i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag, strata(age_strata region_code) cformat(%9.3f)

*** 再考虑药物使用
* taking_aspirin taking_ace_i taking_beta_blocker taking_statins taking_diuretics taking_ca_antagonist taking_chlor_metaformin taking_insulin
* 变化较小
stcox i.d_number_updated i.taking_aspirin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较小
stcox i.d_number_updated i.taking_statins_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较小
stcox i.d_number_updated i.taking_ace_i_2groups i.taking_beta_blocker_2groups taking_diuretics_2groups taking_ca_antagonist_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) // 变化较小
* 变化较小 跟单独放所有抗高血压药物类似
stcox i.d_number_updated i.hypertension_lowering_2g $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较大
stcox i.d_number_updated i.taking_chlor_metaformin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较大
stcox i.d_number_updated i.taking_insulin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化大
stcox i.d_number_updated i.glucose_lowering_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 


********************************************************************************************************************************************************************************
di "********************************** Baseline CMDs combination"
use "1.Data/project2.dta", clear
/* 设置全局宏 */
*** Covariates for adjustment
global adjusted_model1 i.is_female 
global adjusted_model2 $adjusted_model1 i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups
global adjusted_model3 $adjusted_model2 i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups
global adjusted_model4 $adjusted_model3 i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001 == 1)
*** 先依次调整社会经济学指标和生活方式
stcox i.combin_base $adjusted_model1, strata(age_strata region_code) cformat(%9.3f)
stcox i.combin_base $adjusted_model2, strata(age_strata region_code) cformat(%9.3f) 
stcox i.combin_base $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 

*** 在考虑基线同时存在的疾病
* 变化较大
stcox i.combin_base $adjusted_model3 i.has_hypertension, strata(age_strata region_code) cformat(%9.3f) 
* 变化较小
stcox i.combin_base $adjusted_model3 i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.3f) 
* 变化较小
stcox i.combin_base $adjusted_model3 i.rheum_heart_dis_diag, strata(age_strata region_code) cformat(%9.3f) 

* 同时考虑三种 效应值跟调整高血压的差不多
stcox i.combin_base $adjusted_model3 i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag, strata(age_strata region_code) cformat(%9.3f)

*** 再考虑药物使用
* taking_aspirin taking_ace_i taking_beta_blocker taking_statins taking_diuretics taking_ca_antagonist taking_chlor_metaformin taking_insulin
* 变化较小
stcox i.combin_base i.taking_aspirin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较小
stcox i.combin_base i.taking_statins_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较大，主要是共患三种疾病的较大
stcox i.combin_base i.taking_ace_i_2groups i.taking_beta_blocker_2groups taking_diuretics_2groups taking_ca_antagonist_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较大，主要是共患三种疾病的较大
stcox i.combin_base i.hypertension_lowering_2g $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较大，比降高血压药物大
stcox i.combin_base i.taking_chlor_metaformin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化较大，高于二甲双胍
stcox i.combin_base i.taking_insulin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
* 变化大
stcox i.combin_base i.glucose_lowering_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 



********************************************************************************************************************************************************************************
di "********************************** Updated CMDs combination"
use "1.Data/project2_updated_du_ep0001.dta", clear
/* 设置全局宏 */
*** Covariates for adjustment
global adjusted_model1 i.is_female 
global adjusted_model2 $adjusted_model1 i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups 
global adjusted_model3 $adjusted_model2 i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups
global adjusted_model4 $adjusted_model3 i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001 == 1)
*** 先依次调整社会经济学指标和生活方式
stcox i.combin_updated $adjusted_model1, strata(age_strata region_code) cformat(%9.3f)
stcox i.combin_updated $adjusted_model2, strata(age_strata region_code) cformat(%9.3f) 
stcox i.combin_updated $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 

*** 在考虑基线同时存在的疾病
* 变化较大
stcox i.combin_updated $adjusted_model3 i.has_hypertension, strata(age_strata region_code) cformat(%9.3f) 
* 变化较小
stcox i.combin_updated $adjusted_model3 i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.3f) 
* 变化较大
stcox i.combin_updated $adjusted_model3 i.rheum_heart_dis_diag, strata(age_strata region_code) cformat(%9.3f) 

* 同时考虑三种 效应值跟调整高血压的差不多
stcox i.combin_updated $adjusted_model3 i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag, strata(age_strata region_code) cformat(%9.3f)

*** 再考虑药物使用，变化风格之前类似
* taking_aspirin taking_ace_i taking_beta_blocker taking_statins taking_diuretics taking_ca_antagonist taking_chlor_metaformin taking_insulin
stcox i.combin_updated i.taking_aspirin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
stcox i.combin_updated i.taking_statins_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
stcox i.combin_updated i.taking_ace_i_2groups i.taking_beta_blocker_2groups taking_diuretics_2groups taking_ca_antagonist_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
stcox i.combin_updated i.hypertension_lowering_2g $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
stcox i.combin_updated i.taking_chlor_metaformin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
stcox i.combin_updated i.taking_insulin_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 
stcox i.combin_updated i.glucose_lowering_2groups $adjusted_model4, strata(age_strata region_code) cformat(%9.3f) 

*************************************************************************************
/* Analytic strategy */
* Model 1: 性别、教育程度、家庭收入、婚姻状况、共病家族史、吸烟、饮酒、饮食、BMI、腰围、体力活动
* Model 2: Model 1 + 高血压、慢性肾病、风湿性心脏病
* Model 3: Model 2 + 降脂药（他汀）、降压药（放敏感性分析）
* 后续尝试性分析主要使用model 2


*************************************************************************************
****** 查看三种疾病定义的差异： 1. 基线自报疾病； 2.考虑updated 结局； 3. 仅考虑新发结局

*************************************************************************************
********* disease number
use "1.Data/project2_updated_du_ep0001.dta", clear
stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001 == 1)
* Baseline
stcox i.d_number_base $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 
* Updated
stcox i.d_number_updated $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 
* Only incident
keep if d_number_base == 0
stcox i.d_number_updated $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 


********* Single disease:has_diabetes chd_diag stroke_or_tia_diag
use "1.Data/project2_updated_du_ep0001.dta", clear
stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001 == 1)
* Baseline
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 
* Updated
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 
* Only incident
keep if d_number_base == 0
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 

********* disease number
use "1.Data/project2_updated_du_ep0001.dta", clear
stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001 == 1)
* Baseline
stcox i.combin_base $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 
* Updated
stcox i.combin_updated $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 
* Only incident
keep if d_number_base == 0
stcox i.combin_updated $adjusted_model3, strata(age_strata region_code) cformat(%9.3f) 









log close
