********************************************
* Usage: generating table for subgroup analysis of duration of three CMD diseases
* Version 1
* Created date: 2021/12/02
* Updating log:
* 2021/12/10 replace age_3groups to age_2groups

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                     Table for subgroup analyses duration                       *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
/* 目前的调整逻辑 */
* 模型1： 性别、教育程度、收入、婚姻状态、共病家族史、吸烟五分组、饮酒七分组、饮食五分组、BMI4分组、WC三分组、体力活动五分组
* 模型2： 模型1+高血压、慢性肾病、风湿性心脏病
* 模型3： 模型2基础上调整药物（降压药（分开还是合并、阿司匹林、降脂药（statin）
* set trace on
* set tracedepth 1

/* 已经考虑的问题 */
* 生活方式按照多分组调整和两分组调整结果基本一致

/* 变量特征 */
*cd "D:/HanYT/2021-02_CMMtodeath"
*duration_chd_groups duration_stroke_groups duration_diabetes_groups duration_chd_7g duration_stroke_7g duration_diabetes_7g
*duration_chd_updated_groups duration_stroke_updated_groups duration_diabetes_updated_groups duration_chd_updated_7g duration_stroke_updated_7g duration_diabetes_updated_7g

***************************************************************************************************************
*****************************                                       *******************************************
*****************************             Whole population          *******************************************
*****************************                                       *******************************************
***************************************************************************************************************

********************************************************************************************************************************************************************************
/* 设置用于Linux循环的宏 */
*** Endpoints
local ep "`1'"
*** Exposure
local exposure "`2'"

/* 设置宏 */
*** Covariates for adjustment Updated on 11/29/2021
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_region_is_urban i.is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_education_3groups i.is_female i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_age_2groups i.is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

*** Variables defining subgroups
local varlist is_female region_is_urban education_3groups age_2groups


/* Generate table */
*** Exporsure
di _dup(30) "*" "`ep'"  _dup(30) "*"

*** Output file	
tempname coef
tempfile coef_data
save `coef_data', emptyok
postfile `coef' str50 name str50 hr str50 lb str50 ub str50 hr_ci str50 p using `coef_data', replace

*** Load database
use "1.Data/project2_updated_`ep'_duration3_nochg.dta", clear


*** TIME-VARING STATIFIED Variables
if ustrregexm("`exposure'","diabetes")==1{

	local stratified_var fcmd_notdm_updated
}
else if ustrregexm("`exposure'","chd")==1{

	local stratified_var fcmd_notchd_updated
}
else {

	local stratified_var fcmd_notstroke_updated
}

*** Original
stcox i.`exposure' i.`exposure'#i.`stratified_var' ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimate store original

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

foreach g_var of local varlist{


	di _dup(30) "*" "`g_var'"  _dup(30) "*"

	/* LRTEST */
	*** Original + interaction
	stcox i.`exposure' i.`exposure'#i.`stratified_var'#i.`g_var' i.`exposure'#i.`g_var' ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimate store interaction
	lrtest original interaction
	local p = string(r(p),"%9.2f")


	/* Subgroup analysis */
	levelsof `g_var', local(group_level) clean
	foreach g_level of local group_level{

		post `coef' ("`g_var'=`g_level'") ("") ("") ("") ("") ("`p'")
		stcox i.`exposure' i.`exposure'#i.`stratified_var' ${adjusted_model_`g_var'} if `g_var' == `g_level', strata(age_strata region_code) cformat(%9.2f)
			mat mat_hr = r(table)				
		*** Import HRs to the tables
		levelsof `exposure', local(ex_level) clean

		*** Without CMM
		local var_name: variable label `exposure'
		post `coef' ("Without CMM: `var_name'") ("") ("") ("") ("") ("`p'")

		foreach l of local ex_level{
			local n = `l' + 1
			local label_name: label(`exposure') `l'
			local hr = string(mat_hr[1,`n'], "%9.2f")
			if "`hr'" == "1.00"{
				local ci = "Reference"
				local lb = string(mat_hr[5,`n'],"%9.2f") 
				local ub = string(mat_hr[6,`n'],"%9.2f") 
				local hr_ci = "Reference"	
			}
			else {
				local lb = string(mat_hr[5,`n'],"%9.2f") 
				local ub = string(mat_hr[6,`n'],"%9.2f") 
				local hr_ci = "`hr'" + " (" + "`lb'" + "-" + "`ub'" + ")"
			}
			post `coef'  ("`label_name'") ("`hr'") ("`lb'") ("`ub'") ("`hr_ci'") ("`p'")	
		}

		*** With CMM
		post `coef' ("With CMM: `var_name'") ("") ("") ("") ("") ("`p'")
		foreach l of local ex_level{

			local label_name: label(`exposure') `l'
			lincom `l'.`exposure' + `l'.`exposure '#1.`stratified_var',eform
			local hr = string(r(estimate), "%9.2f")
			local lb = string(r(lb), "%9.2f")
			local ub = string(r(ub), "%9.2f")
			local hr_ci = "`hr'" + " (" + "`lb'" + "-" + "`ub'" + ")"
			* Output
			post `coef'  ("`label_name'") ("`hr'") ("`lb'") ("`ub'") ("`hr_ci'") ("`p'")
		}



	}


}


postclose `coef'
use  `coef_data', clear 
*** Store
cap mkdir "2.Result/HRs" 
cap mkdir "2.Result/HRs//$S_DATE" 
export delim using "2.Result/HRs//$S_DATE/Subgroup `ep' `exposure' $S_DATE.csv", replace
