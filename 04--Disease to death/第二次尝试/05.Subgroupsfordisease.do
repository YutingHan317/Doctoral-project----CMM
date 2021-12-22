*******************************************
*** Objective: Subgroups for diseases
*** Created date: 2021/10/25
*** Version: 3.1
*** Updating log:
* Version 2: Automatically generate table
* Version 3: Adding the Calculation using incident cases 
* 2021-12-02: Only calculate incident outcome
* 2021-12-10: replace age_2groups to age_2groups

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
* has_diabetes chd_diag stroke_or_tia_diag combin_base
* diabetes_diag_updated chd_diag_updated stroke_diag_updated combin_updated

/* 设置全局宏 */
loc ep "`1'"
di "`ep'"
*set trace on
*set tracedepth 1

/* Log file */
log using "2.Result/Log/subgroups `ep' $S_DATE.log", replace

/* 设置全局宏 */
*** Covariates for adjustment
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_region_is_urban i.is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_education_3groups i.is_female i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_age_2groups i.is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

/* Postfile setting */
tempname stats
tempfile table_sbg
save `table_sbg', replace emptyok
postfile `stats' str50 (name disease1 disease2 disease3 total1 combin1 combin2 combin3 combin4 combin5 combin6 combin7 total2) using `table_sbg', replace


***************************************************************************************************************
*****************************                                       *******************************************
*****************************               1.Baseline              *******************************************
*****************************                                       *******************************************
***************************************************************************************************************

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* Import dataset */
use "1.Data/project2_updated_`ep'_nochg.dta", clear

post `stats' ("Baseline") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")  


/* Overall */

******* Single disease
*** Original model
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store original_single

*** Extracting coef
mat a = r(table)
	loc disease1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
	loc disease2 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
	loc disease3 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"


******* Disease combination
*** Original model
stcox i.combin_base ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store original_combin

*** Extracting coef
mat a = r(table)
	loc combin1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
	loc combin2 = string(a[1,3],"%9.2f") + " (" + string(a[5,3],"%9.2f") + "," + string(a[6,3],"%9.2f") + ")"
	loc combin3 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
	loc combin4 = string(a[1,5],"%9.2f") + " (" + string(a[5,5],"%9.2f") + "," + string(a[6,5],"%9.2f") + ")"
	loc combin5 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"
	loc combin6 = string(a[1,7],"%9.2f") + " (" + string(a[5,7],"%9.2f") + "," + string(a[6,7],"%9.2f") + ")"
	loc combin7 = string(a[1,8],"%9.2f") + " (" + string(a[5,8],"%9.2f") + "," + string(a[6,8],"%9.2f") + ")"

post `stats' ("Overall") ("`disease1'") ("`disease2'") ("`disease3'") ("") ///
	("`combin1'") ("`combin2'") ("`combin3'") ("`combin4'") ("`combin5'") ("`combin6'") ("`combin7'") ("")

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* Subgroups */

*** Subgroup variable list
local varlist is_female region_is_urban education_3groups age_2groups

*** 
foreach var of local varlist{

	di "*****************Subgroup is `var'"
	levelsof `var', local(level) clean
	local name: variable label `var'
	post `stats' ("`name'") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")  

	*** Calculate and extract overall lrtest estimates
	* Single disease
	stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag i.has_diabetes#i.`var' i.chd_diag#i.`var' i.stroke_or_tia_diag#i.`var' ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
		estimates store interaction_single
		lrtest original_single interaction_single
		local total1 = r(p)

	* Combination
	stcox i.combin_base i.combin_base#i.`var' ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
		estimates store interaction_combin
		lrtest original_combin interaction_combin
		local total2 = r(p)

	foreach l of local level{

		*** Extract model
		local name: label(`var') `l'	

		*** Single disease
		stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag ${adjusted_model_`var'} if `var' == `l', strata(age_strata region_code) cformat(%9.2f)
			mat a = r(table)

			* Extracting
			loc disease1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
			loc disease2 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
			loc disease3 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"

		*** Disease combination
		stcox i.combin_base ${adjusted_model_`var'} if `var' == `l', strata(age_strata region_code) cformat(%9.2f)
			mat a = r(table)

			*** Extracting coef
			loc combin1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
			loc combin2 = string(a[1,3],"%9.2f") + " (" + string(a[5,3],"%9.2f") + "," + string(a[6,3],"%9.2f") + ")"
			loc combin3 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
			loc combin4 = string(a[1,5],"%9.2f") + " (" + string(a[5,5],"%9.2f") + "," + string(a[6,5],"%9.2f") + ")"
			loc combin5 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"
			loc combin6 = string(a[1,7],"%9.2f") + " (" + string(a[5,7],"%9.2f") + "," + string(a[6,7],"%9.2f") + ")"
			loc combin7 = string(a[1,8],"%9.2f") + " (" + string(a[5,8],"%9.2f") + "," + string(a[6,8],"%9.2f") + ")"

		post `stats' ("`name'") ("`disease1'") ("`disease2'") ("`disease3'") ("`total1'") ///
			("`combin1'") ("`combin2'") ("`combin3'") ("`combin4'") ("`combin5'") ("`combin6'") ("`combin7'") ("`total2'")

	}

}



***************************************************************************************************************
*****************************                                       *******************************************
*****************************               2.Updated               *******************************************
*****************************                                       *******************************************
***************************************************************************************************************

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
post `stats' ("Updated") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")  

/* Overall */

******* Single disease
*** Original model
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store original_single

*** Extracting coef
mat a = r(table)
	loc disease1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
	loc disease2 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
	loc disease3 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"


******* Disease combination
*** Original model
stcox i.combin_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store original_combin

*** Extracting coef
mat a = r(table)
	loc combin1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
	loc combin2 = string(a[1,3],"%9.2f") + " (" + string(a[5,3],"%9.2f") + "," + string(a[6,3],"%9.2f") + ")"
	loc combin3 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
	loc combin4 = string(a[1,5],"%9.2f") + " (" + string(a[5,5],"%9.2f") + "," + string(a[6,5],"%9.2f") + ")"
	loc combin5 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"
	loc combin6 = string(a[1,7],"%9.2f") + " (" + string(a[5,7],"%9.2f") + "," + string(a[6,7],"%9.2f") + ")"
	loc combin7 = string(a[1,8],"%9.2f") + " (" + string(a[5,8],"%9.2f") + "," + string(a[6,8],"%9.2f") + ")"

post `stats' ("Overall") ("`disease1'") ("`disease2'") ("`disease3'") ("") ///
	("`combin1'") ("`combin2'") ("`combin3'") ("`combin4'") ("`combin5'") ("`combin6'") ("`combin7'") ("")

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* Subgroups */

*** Subgroup variable list
local varlist is_female region_is_urban education_3groups age_2groups

*** 
foreach var of local varlist{

	di "*****************Subgroup is `var'"
	levelsof `var', local(level) clean
	local name: variable label `var'
	post `stats' ("`name'") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")  

	*** Calculate and extract overall lrtest estimates
	* Single disease
	stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.diabetes_diag_updated#i.`var' i.chd_diag_updated#i.`var' i.stroke_diag_updated#i.`var' ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
		estimates store interaction_single
		lrtest original_single interaction_single
		local total1 = r(p)

	* Combination
	stcox i.combin_updated i.combin_updated#i.`var' ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
		estimates store interaction_combin
		lrtest original_combin interaction_combin
		local total2 = r(p)

	foreach l of local level{

		*** Extract model
		local name: label(`var') `l'	

		*** Single disease
		stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model_`var'} if `var' == `l', strata(age_strata region_code) cformat(%9.2f)
			mat a = r(table)

			* Extracting
			loc disease1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
			loc disease2 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
			loc disease3 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"

		*** Disease combination
		stcox i.combin_updated ${adjusted_model_`var'} if `var' == `l', strata(age_strata region_code) cformat(%9.2f)
			mat a = r(table)

			*** Extracting coef
			loc combin1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
			loc combin2 = string(a[1,3],"%9.2f") + " (" + string(a[5,3],"%9.2f") + "," + string(a[6,3],"%9.2f") + ")"
			loc combin3 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
			loc combin4 = string(a[1,5],"%9.2f") + " (" + string(a[5,5],"%9.2f") + "," + string(a[6,5],"%9.2f") + ")"
			loc combin5 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"
			loc combin6 = string(a[1,7],"%9.2f") + " (" + string(a[5,7],"%9.2f") + "," + string(a[6,7],"%9.2f") + ")"
			loc combin7 = string(a[1,8],"%9.2f") + " (" + string(a[5,8],"%9.2f") + "," + string(a[6,8],"%9.2f") + ")"

		post `stats' ("`name'") ("`disease1'") ("`disease2'") ("`disease3'") ("`total1'") ///
			("`combin1'") ("`combin2'") ("`combin3'") ("`combin4'") ("`combin5'") ("`combin6'") ("`combin7'") ("`total2'")

	}

}



***************************************************************************************************************
*****************************                                       *******************************************
*****************************              3.Incidnet               *******************************************
*****************************                                       *******************************************
***************************************************************************************************************

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
post `stats' ("Incident") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")  
keep if d_number_base == 0
/* Overall */

******* Single disease
*** Original model
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store original_single

*** Extracting coef
mat a = r(table)
	loc disease1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
	loc disease2 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
	loc disease3 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"


******* Disease combination
*** Original model
stcox i.combin_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store original_combin

*** Extracting coef
mat a = r(table)
	loc combin1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
	loc combin2 = string(a[1,3],"%9.2f") + " (" + string(a[5,3],"%9.2f") + "," + string(a[6,3],"%9.2f") + ")"
	loc combin3 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
	loc combin4 = string(a[1,5],"%9.2f") + " (" + string(a[5,5],"%9.2f") + "," + string(a[6,5],"%9.2f") + ")"
	loc combin5 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"
	loc combin6 = string(a[1,7],"%9.2f") + " (" + string(a[5,7],"%9.2f") + "," + string(a[6,7],"%9.2f") + ")"
	loc combin7 = string(a[1,8],"%9.2f") + " (" + string(a[5,8],"%9.2f") + "," + string(a[6,8],"%9.2f") + ")"

post `stats' ("Overall") ("`disease1'") ("`disease2'") ("`disease3'") ("") ///
	("`combin1'") ("`combin2'") ("`combin3'") ("`combin4'") ("`combin5'") ("`combin6'") ("`combin7'") ("")

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* Subgroups */

*** Subgroup variable list
local varlist is_female region_is_urban education_3groups age_2groups

*** 
foreach var of local varlist{

	di "*****************Subgroup is `var'"
	levelsof `var', local(level) clean
	local name: variable label `var'
	post `stats' ("`name'") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")  

	*** Calculate and extract overall lrtest estimates
	* Single disease
	stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.diabetes_diag_updated#i.`var' i.chd_diag_updated#i.`var' i.stroke_diag_updated#i.`var' ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
		estimates store interaction_single
		lrtest original_single interaction_single
		local total1 = r(p)

	* Combination
	stcox i.combin_updated i.combin_updated#i.`var' ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
		estimates store interaction_combin
		lrtest original_combin interaction_combin
		local total2 = r(p)

	foreach l of local level{

		*** Extract model
		local name: label(`var') `l'	

		*** Single disease
		stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model_`var'} if `var' == `l', strata(age_strata region_code) cformat(%9.2f)
			mat a = r(table)

			* Extracting
			loc disease1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
			loc disease2 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
			loc disease3 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"

		*** Disease combination
		stcox i.combin_updated ${adjusted_model_`var'} if `var' == `l', strata(age_strata region_code) cformat(%9.2f)
			mat a = r(table)

			*** Extracting coef
			loc combin1 = string(a[1,2],"%9.2f") + " (" + string(a[5,2],"%9.2f") + "," + string(a[6,2],"%9.2f") + ")"
			loc combin2 = string(a[1,3],"%9.2f") + " (" + string(a[5,3],"%9.2f") + "," + string(a[6,3],"%9.2f") + ")"
			loc combin3 = string(a[1,4],"%9.2f") + " (" + string(a[5,4],"%9.2f") + "," + string(a[6,4],"%9.2f") + ")"
			loc combin4 = string(a[1,5],"%9.2f") + " (" + string(a[5,5],"%9.2f") + "," + string(a[6,5],"%9.2f") + ")"
			loc combin5 = string(a[1,6],"%9.2f") + " (" + string(a[5,6],"%9.2f") + "," + string(a[6,6],"%9.2f") + ")"
			loc combin6 = string(a[1,7],"%9.2f") + " (" + string(a[5,7],"%9.2f") + "," + string(a[6,7],"%9.2f") + ")"
			loc combin7 = string(a[1,8],"%9.2f") + " (" + string(a[5,8],"%9.2f") + "," + string(a[6,8],"%9.2f") + ")"

		post `stats' ("`name'") ("`disease1'") ("`disease2'") ("`disease3'") ("`total1'") ///
			("`combin1'") ("`combin2'") ("`combin3'") ("`combin4'") ("`combin5'") ("`combin6'") ("`combin7'") ("`total2'")

	}

}


postclose `stats'
use `table_sbg', clear
cap mkdir "2.Result/HRs//$S_DATE" 
export delim using "2.Result/HRs//$S_DATE//Subgroups dicho `ep' $S_DATE.csv", replace
