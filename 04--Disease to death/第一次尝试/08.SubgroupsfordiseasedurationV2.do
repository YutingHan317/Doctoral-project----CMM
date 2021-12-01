********************************************
* Objective: Subgroups for disease duration
* Created date: 2021/09/15
* Version 2
* Updating log:
// version 2
* 2021/11/12 automatically generate table

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
set trace on
set tracedepth 1
/* 设置用于Linux循环的宏 */
*** Endpoints
local ep "`1'"

***************************************************************************************************************
*****************************                                       *******************************************
*****************************            Incident analyses          *******************************************
*****************************                                       *******************************************
***************************************************************************************************************
global adjusted_model_is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_region_is_urban i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_education_2groups i.is_female i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_age_2groups i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

global exposure_var1 duration_chd_updated_8g duration_stroke_updated_8g duration_diabetes_updated_8g


local varlist is_female region_is_urban education_2groups age_2groups
foreach var of local varlist{

	use "1.Data/project2.dta", clear
	levelsof `var',local(group_level) clean

	foreach l of local group_level{

		di _dup(30) "*" "`ep'"  _dup(30) "*"
					
		/* Stset */
		*use "D:/HanYT/2021-02_CMMtodeath/1.Data/project2_updated_`ep'.dta", clear
		use "1.Data/project2_updated_`ep'_duration3.dta", clear
		keep if `var' == `l'
		keep if d_number_base == 0
		****** Output file	
		tempname coef
		tempfile coef_data
		save `coef_data', replace
		postfile `coef' str50 name str50 hr str50 lb str50 ub str50 hr_ci using `coef_data', replace
		forvalues n = 1/1{	

			di _dup(30) "#" "`ep' ${exposure_var`n'}"  _dup(30) "*#"
			local exposure_list`n'  ""
			foreach exp_var of global exposure_var`n'{

				local exposure_list`n' `exposure_list`n'' i.`exp_var'

			}
			* COX regression
			stcox `exposure_list`n'' ${adjusted_model_`var'} , strata(age_strata region_code)
			mat mat_hr = r(table)				
			* Import HRs to the tables
			local i = 0
			foreach exp_var of global exposure_var`n'{	

				local var_name: variable label `exp_var'
				post `coef' ("`var_name'") ("") ("") ("") ("")
				levelsof `exp_var', local(var_level) clean
				foreach c of local var_level{			
					local ++i
					local label_name: label(`exp_var') `c'
					local hr = string(mat_hr[1,`i'],"%9.2f")
					if "`hr'" == "1.00"{

						local ci = "Reference"
						local lb = ""
						local ub = ""
						local hr_ci = "Reference"	

					}
					else{		

						local lb = string(mat_hr[5,`i'],"%9.2f") 
						local ub = string(mat_hr[6,`i'],"%9.2f") 
						local hr_ci = "`hr'" + " (" + "`lb'" + "-" + "`ub'" + ") "

					}		

					post `coef'  ("`label_name'") ("`hr'") ("`lb'") ("`ub'") ("`hr_ci'")	

				}			
			}
		}
		postclose `coef'
		use  `coef_data', clear 
		save `coef_data', replace 

			
		/* IR */
		use "1.Data/project2_updated_`ep'_duration3.dta", clear
		keep if `var' == `l'
		keep if d_number_base == 0
		stset `ep'_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(`ep' == 1)
		gen py = _t - _t0
		tempname IR
		tempfile IR_data
		save `IR_data'
		postfile `IR' str50 name number_cases str50 IR using `IR_data', replace
		forvalues n = 1/1{

			foreach vn of global exposure_var`n'{

				local name: variable label `vn'
				post `IR' ("`name'") (.) ("") 
				levelsof `vn', local(var_level) clean
				foreach c of local var_level{

					local level_label: label(`vn')`c'
					sum py if `vn' == `c'
					local py_sum = r(sum)
					sum _d if `vn' == `c' & `ep' == 1
					local ep_count = r(N)
					local ir = string(`ep_count'/`py_sum'*10000,"%9.2f")
					post `IR' ("`level_label'")  (`ep_count') ("`ir'")
				}

			}
		}
		postclose `IR'

		*** Merge all the data
		use `IR_data',clear
		merge 1:1 _n using `coef_data', nogen keepusing(hr lb ub hr_ci)
		cap mkdir "D:/HanYT/2021-02_CMMtodeath/2.Result/Hazard ratios//$S_DATE" 
		cap mkdir "2.Result/HRs" 
		cap mkdir "2.Result/HRs//$S_DATE" 
		export delim using "2.Result/HRs//$S_DATE//`ep' `var'=`l' incident duration $S_DATE.csv", replace
			

	}


}




***************************************************************************************************************
*****************************                                       *******************************************
*****************************            Updated analyses           *******************************************
*****************************                                       *******************************************
***************************************************************************************************************
global adjusted_model_is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_region_is_urban i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_education_2groups i.is_female i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_age_2groups i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

global exposure_var1 duration_chd_8g duration_stroke_8g duration_diabetes_8g
global exposure_var2 duration_chd_updated_8g duration_stroke_updated_8g duration_diabetes_updated_8g


local varlist is_female region_is_urban education_2groups age_2groups
foreach var of local varlist{

	use "1.Data/project2.dta", clear
	levelsof `var',local(group_level) clean

	foreach l of local group_level{

		di _dup(30) "*" "`ep'"  _dup(30) "*"
					
		/* Stset */
		*use "D:/HanYT/2021-02_CMMtodeath/1.Data/project2_updated_`ep'.dta", clear
		use "1.Data/project2_updated_`ep'_duration3.dta", clear
		keep if `var' == `l'
		****** Output file	
		tempname coef
		tempfile coef_data
		save `coef_data', replace
		postfile `coef' str50 name str50 hr str50 lb str50 ub str50 hr_ci using `coef_data', replace
		forvalues n = 1/2{	

			di _dup(30) "#" "`ep' ${exposure_var`n'}"  _dup(30) "*#"
			local exposure_list`n'  ""
			foreach exp_var of global exposure_var`n'{

				local exposure_list`n' `exposure_list`n'' i.`exp_var'

			}
			* COX regression
			stcox `exposure_list`n'' ${adjusted_model_`var'} , strata(age_strata region_code)
			mat mat_hr = r(table)				
			* Import HRs to the tables
			local i = 0
			foreach exp_var of global exposure_var`n'{	

				local var_name: variable label `exp_var'
				post `coef' ("`var_name'") ("") ("") ("") ("")
				levelsof `exp_var', local(var_level) clean
				foreach c of local var_level{			
					local ++i
					local label_name: label(`exp_var') `c'
					local hr = string(mat_hr[1,`i'],"%9.2f")
					if "`hr'" == "1.00"{

						local ci = "Reference"
						local lb = ""
						local ub = ""
						local hr_ci = "Reference"	

					}
					else{		

						local lb = string(mat_hr[5,`i'],"%9.2f") 
						local ub = string(mat_hr[6,`i'],"%9.2f") 
						local hr_ci = "`hr'" + " (" + "`lb'" + "-" + "`ub'" + ") "

					}		

					post `coef'  ("`label_name'") ("`hr'") ("`lb'") ("`ub'") ("`hr_ci'")	

				}			
			}
		}
		postclose `coef'
		use  `coef_data', clear 
		save `coef_data', replace 

			
		/* IR */
		use "1.Data/project2_updated_`ep'_duration3.dta", clear
		keep if `var' == `l'
		gen py = _t - _t0
		tempname IR
		tempfile IR_data
		save `IR_data'
		postfile `IR' str50 name number_cases str50 IR using `IR_data', replace
		forvalues n = 1/2{

			foreach vn of global exposure_var`n'{

				local name: variable label `vn'
				post `IR' ("`name'") (.) ("") 
				levelsof `vn', local(var_level) clean
				foreach c of local var_level{

					local level_label: label(`vn')`c'
					sum py if `vn' == `c'
					local py_sum = r(sum)
					sum _d if `vn' == `c' & `ep' == 1
					local ep_count = r(N)
					local ir = string(`ep_count'/`py_sum'*10000,"%9.2f")
					post `IR' ("`level_label'")  (`ep_count') ("`ir'")
				}

			}
		}
		postclose `IR'

		*** Merge all the data
		use `IR_data',clear
		merge 1:1 _n using `coef_data', nogen keepusing(hr lb ub hr_ci)
		cap mkdir "D:/HanYT/2021-02_CMMtodeath/2.Result/Hazard ratios//$S_DATE" 
		cap mkdir "2.Result/HRs" 
		cap mkdir "2.Result/HRs//$S_DATE" 
		export delim using "2.Result/HRs//$S_DATE//`ep' `var'=`l' baseline_updated duration $S_DATE.csv", replace
			

	}


}
