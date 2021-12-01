********************************************
* Objective: Subgroups for disease duration
* Created date: 2021/09/15
* Updating log:

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                                  Exploration                                   *************
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
*cd "D:/HanYT/2021-02_CMMtodeath"

/* 暴露变量名 */
* i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g
* i.duration_chd_updated_8g i.duration_stroke_updated_8g i.duration_diabetes_updated_8g

local ep "`1'"

log using "2.Result/Log/subgroups duration `ep' $S_DATE.log", replace

***************************************************************************************************************
*****************************                                       *******************************************
*****************************               1.Baseline              *******************************************
*****************************                                       *******************************************
***************************************************************************************************************

***************************************************************************************************************
*****************************            1.1 Single disease         *******************************************
***************************************************************************************************************

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* 设置全局宏 */
*** Covariates for adjustment
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_education i.is_female i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag


/* Load dataset */
use "1.Data/project2_updated_`ep'.dta", clear

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* Full model */
stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* Subgroups */
* Sex
bysort is_female: stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)

* Residence area
bysort region_is_urban: stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)

* Education
bysort education_3groups: stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model_education}, strata(age_strata region_code) cformat(%9.2f)
bysort education_2groups: stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model_education}, strata(age_strata region_code) cformat(%9.2f)

* Age groups
bysort age_3groups: stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
bysort age_2groups: stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)


***************************************************************************************************************
*****************************                                       *******************************************
*****************************                1.Updated              *******************************************
*****************************                                       *******************************************
***************************************************************************************************************

***************************************************************************************************************
*****************************            1.1 Single disease         *******************************************
***************************************************************************************************************

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* 设置全局宏 */
*** Covariates for adjustment
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_education i.is_female i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag


/* Load dataset */
use "1.Data/project2_updated_`ep'.dta", clear

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* Full model */
stcox i.duration_chd_updated_8g i.duration_stroke_updated_8g i.duration_diabetes_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* Subgroups */
* Sex
bysort is_female: stcox i.duration_chd_updated_8g i.duration_stroke_updated_8g i.duration_diabetes_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)

* Residence area
bysort region_is_urban: stcox i.duration_chd_updated_8g i.duration_stroke_updated_8g i.duration_diabetes_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)

* Education
bysort education_3groups: stcox i.duration_chd_updated_8g i.duration_stroke_updated_8g i.duration_diabetes_updated_8g ${adjusted_model_education}, strata(age_strata region_code) cformat(%9.2f)
bysort education_2groups: stcox i.duration_chd_updated_8g i.duration_stroke_updated_8g i.duration_diabetes_updated_8g ${adjusted_model_education}, strata(age_strata region_code) cformat(%9.2f)

* Age groups
bysort age_3groups: stcox i.duration_chd_updated_8g i.duration_stroke_updated_8g i.duration_diabetes_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
bysort age_2groups: stcox i.duration_chd_updated_8g i.duration_stroke_updated_8g i.duration_diabetes_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)


log close
