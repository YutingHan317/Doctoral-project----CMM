********************************************
* Usage: Sensitivity analysis generating table for duration of three CMD diseases
* Version 1
* Created date: 2021/12/05
* Updating log:
/// Version 1



**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                 Different type of three type of exposure                       *************
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

/* 设置全局宏 */
*** Covariates for adjustment Updated on 11/29/2021
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

/* 循环 */
*** Exporsure
di _dup(30) "*" "`ep'"  _dup(30) "*"

*** Output file	
tempname coef
tempfile coef_data
save `coef_data', emptyok
postfile `coef' str50 name str50 hr str50 lb str50 ub str50 hr_ci using `coef_data', replace

*** Load database
use "1.Data/project2_updated_`ep'_duration3_nochg.dta", clear
drop if drop2y == 1
* keep if region_is_urban == 1

*** stcox
if ustrregexm("`exposure'","diabetes")==1{

	local stratified_var fcmd_notdm_updated
}
else if ustrregexm("`exposure'","chd")==1{

	local stratified_var fcmd_notchd_updated
}
else {

	local stratified_var fcmd_notstroke_updated
}

stcox i.`exposure' i.`exposure'#i.`stratified_var' ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	mat mat_hr = r(table)				
*** Import HRs to the tables
levelsof `exposure', local(ex_level) clean

*** Without CMM
local var_name: variable label `exposure'
post `coef' ("Without CMM: `var_name'") ("") ("") ("") ("")

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
	post `coef'  ("`label_name'") ("`hr'") ("`lb'") ("`ub'") ("`hr_ci'")	
}

*** With CMM
post `coef' ("With CMM: `var_name'") ("") ("") ("") ("")
foreach l of local ex_level{

	local label_name: label(`exposure') `l'
	lincom `l'.`exposure' + `l'.`exposure '#1.`stratified_var',eform
	local hr = string(r(estimate), "%9.2f")
	local lb = string(r(lb), "%9.2f")
	local ub = string(r(ub), "%9.2f")
	local hr_ci = "`hr'" + " (" + "`lb'" + "-" + "`ub'" + ")"
	* Output
	post `coef'  ("`label_name'") ("`hr'") ("`lb'") ("`ub'") ("`hr_ci'")
}


postclose `coef'
use  `coef_data', clear 
save `coef_data', replace 


/* IR */
use "1.Data/project2_updated_`ep'_duration3_nochg.dta", clear
drop if drop2y == 1
gen py = _t - _t0
tempname IR
tempfile IR_data
save `IR_data', emptyok
postfile `IR' str50 name number_cases str50 IR using `IR_data', replace

*** Without CMM
post `IR' ("Without CMM: `var_name'") (.) ("") 
foreach l of local ex_level{

	local level_label: label(`exposure') `l'
	sum py if `exposure' == `l' & `stratified_var' == 0
	local py_sum = r(sum)
	sum _d if `exposure' == `l' & `stratified_var' == 0
	local ep_count = r(sum)
	local ir = string(`ep_count'/`py_sum'*10000,"%9.2f")
	post `IR' ("`level_label'")  (`ep_count') ("`ir'")
}

*** With CMM
post `IR' ("With CMM: `var_name'") (.) ("") 
foreach l of local ex_level{

	local level_label: label(`exposure') `l'
	sum py if `exposure' == `l' & `stratified_var' == 1
	local py_sum = r(sum)
	sum _d if `exposure' == `l' & `stratified_var' == 1
	local ep_count = r(sum)
	local ir = string(`ep_count'/`py_sum'*10000,"%9.2f")
	post `IR' ("`level_label'")  (`ep_count') ("`ir'")
}


postclose `IR'

*** Merge all the data
use `IR_data',clear
merge 1:1 _n using `coef_data', nogen keepusing(hr lb ub hr_ci)

*** Store
cap mkdir "2.Result/Sensitivity//$S_DATE" 
export delim using "2.Result/Sensitivity//$S_DATE/Sens `ep' `exposure' $S_DATE.csv", replace
