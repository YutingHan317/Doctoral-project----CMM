****** Created on 10/12/2020
*Body weight and fat
***************************************************************************************************************
*****************************                                       *******************************************
*****************************           COX regression              *******************************************
*****************************                                       *******************************************
***************************************************************************************************************
global adjusted_cox i.is_female i.highest_education i.marital_status_2groups i.parental_fh_3groups 
global single_var risky_smoking risky_alcohol risky_diet risky_PA risky_obesity

********************************* 仅以未发生任何疾病的作为参照组，剔除随访期间仅发生一个疾病的研究对象
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta", clear
drop if first_cmd_inc == 1 & mltmbd_inc == 0
stset mltmbd_inc_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(mltmbd_inc == 1)
local lifestyle_list ""
foreach var of global single_var{

	local lifestyle_list `lifestyle_list' i.`var'

}
stcox `lifestyle_list' $adjusted_cox, strata(age_strata region_code)
mat mat_hr = r(table)

local i = 0
tempname coef
tempfile coef_data
save `coef_data', replace
postfile `coef' str50 hr str50 ci str50 hr_ci using `coef_data', replace	
foreach var of global single_var{

	levelsof `var', local(level) clean
	foreach l of local level{

		local ++i
		local hr = string(mat_hr[1,`i'],"%9.2f")
		if "`hr'" == "1.00"{

			local ci = "Reference"
			local hr_ci = "Reference"

		}
		else{

			local ci = "(" + string(mat_hr[5,`i'],"%9.2f") + "-" + string(mat_hr[6,`i'],"%9.2f") + ")"
			local hr_ci = "`hr'" + " " + "`ci'"

		}

		post `coef'  ("`hr'") ("`ci'") ("`hr_ci'")

	}

}
postclose `coef'

/* IR */
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta", clear
drop if first_cmd_inc == 1 & mltmbd_inc == 0
gen py = (mltmbd_inc_date - study_date)/365.25
tempname IR
tempfile IR_data
save `IR_data'
postfile `IR' str50 name str50 label number_cases str50 IR using `IR_data', replace
foreach vn of global single_var{

	local name: variable label `vn'
	levelsof `vn', local(level) clean
	foreach l of local level{

		local level_label: label(`vn')`l'
		sum py if `vn' == `l'
		local py_sum = r(sum)
		count if `vn' == `l' & mltmbd_inc == 1
		local ep_count = r(N)
		local ir = string(`ep_count'/`py_sum'*10000,"%9.2f")
		post `IR' ("`name'") ("`level_label'") (`ep_count') ("`ir'")

	}

}
postclose `IR'


/* Merge */	
use `IR_data', clear
merge 1:1 _n using `coef_data',nogen
cap mkdir "D:\HanYT\2019-06 Multimorbidity\3. Result\multistate\COX regression\\$S_DATE"
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\multistate\COX regression\\$S_DATE\\single drop one CMD.xlsx", replace firstrow(variables)


*****************************  分析局限于基线有病患者
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4_full.dta",replace
keep if chd_diag == 1 | stroke_or_tia_diag == 1|  has_diabetes == 1   
drop if d_number_base > 1 
gen diabetes_cmm = 1 if (c_ep0003 == 1 | c_ep0009 == 1) & has_diabetes == 1
	replace diabetes_cmm = 0 if mi(diabetes_cmm)
gen stroke_cmm = 1 if (c_ep0003 == 1 | c_ep0088 == 1) & stroke_or_tia_diag == 1
	replace stroke_cmm = 0 if mi(stroke_cmm)
gen ihd_cmm = 1 if (c_ep0009 == 1 | c_ep0088 == 1) & chd_diag == 1
	replace ihd_cmm = 0 if mi(ihd_cmm)
gen patient_cmm = 1 if diabetes_cmm == 1 | stroke_cmm == 1 | ihd_cmm == 1
gen patient_cmm_date = min(c_ep0003_date, c_ep0009_date) if diabetes_cmm == 1
	replace patient_cmm_date = min(c_ep0003_date, c_ep0088_date) if stroke_cmm == 1
	replace patient_cmm_date = min(c_ep0009_date, c_ep0088_date) if ihd_cmm == 1
	replace patient_cmm_date = du_ep0001_date if mi(patient_cmm_date)
stset patient_cmm_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(patient_cmm == 1)
local lifestyle_list ""
foreach var of global single_var{

	local lifestyle_list `lifestyle_list' i.`var'

}
stcox `lifestyle_list' $adjusted_cox, strata(age_strata region_code)
mat mat_hr = r(table)

local i = 0
tempname coef
tempfile coef_data
save `coef_data', replace
postfile `coef' str50 hr str50 ci str50 hr_ci using `coef_data', replace	
foreach var of global single_var{

	levelsof `var', local(level) clean
	foreach l of local level{

		local ++i
		local hr = string(mat_hr[1,`i'],"%9.2f")
		if "`hr'" == "1.00"{

			local ci = "Reference"
			local hr_ci = "Reference"

		}
		else{

			local ci = "(" + string(mat_hr[5,`i'],"%9.2f") + "-" + string(mat_hr[6,`i'],"%9.2f") + ")"
			local hr_ci = "`hr'" + " " + "`ci'"

		}

		post `coef'  ("`hr'") ("`ci'") ("`hr_ci'")

	}

}
postclose `coef'

/* IR */
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4_full.dta",replace
keep if chd_diag == 1 | stroke_or_tia_diag == 1|  has_diabetes == 1   
drop if d_number_base > 1 
gen diabetes_cmm = 1 if (c_ep0003 == 1 | c_ep0009 == 1) & has_diabetes == 1
	replace diabetes_cmm = 0 if mi(diabetes_cmm)
gen stroke_cmm = 1 if (c_ep0003 == 1 | c_ep0088 == 1) & stroke_or_tia_diag == 1
	replace stroke_cmm = 0 if mi(stroke_cmm)
gen ihd_cmm = 1 if (c_ep0009 == 1 | c_ep0088 == 1) & chd_diag == 1
	replace ihd_cmm = 0 if mi(ihd_cmm)
gen patient_cmm = 1 if diabetes_cmm == 1 | stroke_cmm == 1 | ihd_cmm == 1
gen patient_cmm_date = min(c_ep0003_date, c_ep0009_date) if diabetes_cmm == 1
	replace patient_cmm_date = min(c_ep0003_date, c_ep0088_date) if stroke_cmm == 1
	replace patient_cmm_date = min(c_ep0009_date, c_ep0088_date) if ihd_cmm == 1
	replace patient_cmm_date = du_ep0001_date if mi(patient_cmm_date)
gen py = (patient_cmm_date - study_date)/365.25
tempname IR
tempfile IR_data
save `IR_data'
postfile `IR' str50 name str50 label number_cases str50 IR using `IR_data', replace
foreach vn of global single_var{

	local name: variable label `vn'
	levelsof `vn', local(level) clean
	foreach l of local level{

		local level_label: label(`vn')`l'
		sum py if `vn' == `l'
		local py_sum = r(sum)
		count if `vn' == `l' & patient_cmm == 1
		local ep_count = r(N)
		local ir = string(`ep_count'/`py_sum'*10000,"%9.2f")
		post `IR' ("`name'") ("`level_label'") (`ep_count') ("`ir'")

	}

}
postclose `IR'


/* Merge */	
use `IR_data', clear
merge 1:1 _n using `coef_data',nogen
cap mkdir "D:\HanYT\2019-06 Multimorbidity\3. Result\multistate\COX regression\\$S_DATE"
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\multistate\COX regression\\$S_DATE\\single baseline to cmm CMD.xlsx", replace firstrow(variables)


*****************************  动态队列 发生首发疾病后入组
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta", clear
replace first_cmd_inc_date = mltmbd_inc_date - 0.5 if first_cmd_inc == 1 & mltmbd_inc == 1 & first_cmd_inc_date == mltmbd_inc_date
stset mltmbd_inc_date, id(studyid) enter(first_cmd_inc_date) origin(time dob_anon) scale(365.25) failure(mltmbd_inc == 1)
local lifestyle_list ""
foreach var of global single_var{

	local lifestyle_list `lifestyle_list' i.`var'

}
stcox `lifestyle_list' $adjusted_cox, strata(age_strata region_code)
mat mat_hr = r(table)

local i = 0
tempname coef
tempfile coef_data
save `coef_data', replace
postfile `coef' str50 hr str50 ci str50 hr_ci using `coef_data', replace	
foreach var of global single_var{

	levelsof `var', local(level) clean
	foreach l of local level{

		local ++i
		local hr = string(mat_hr[1,`i'],"%9.2f")
		if "`hr'" == "1.00"{

			local ci = "Reference"
			local hr_ci = "Reference"

		}
		else{

			local ci = "(" + string(mat_hr[5,`i'],"%9.2f") + "-" + string(mat_hr[6,`i'],"%9.2f") + ")"
			local hr_ci = "`hr'" + " " + "`ci'"

		}

		post `coef'  ("`hr'") ("`ci'") ("`hr_ci'")

	}

}
postclose `coef'

/* IR */
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta", clear
replace first_cmd_inc_date = mltmbd_inc_date - 0.5 if first_cmd_inc == 1 & mltmbd_inc == 1 & first_cmd_inc_date == mltmbd_inc_date
gen py = (mltmbd_inc_date - study_date)/365.25
tempname IR
tempfile IR_data
save `IR_data'
postfile `IR' str50 name str50 label number_cases str50 IR using `IR_data', replace
foreach vn of global single_var{

	local name: variable label `vn'
	levelsof `vn', local(level) clean
	foreach l of local level{

		local level_label: label(`vn')`l'
		sum py if `vn' == `l'
		local py_sum = r(sum)
		count if `vn' == `l' & mltmbd_inc == 1
		local ep_count = r(N)
		local ir = string(`ep_count'/`py_sum'*10000,"%9.2f")
		post `IR' ("`name'") ("`level_label'") (`ep_count') ("`ir'")

	}

}
postclose `IR'


/* Merge */	
use `IR_data', clear
merge 1:1 _n using `coef_data',nogen
cap mkdir "D:\HanYT\2019-06 Multimorbidity\3. Result\multistate\COX regression\\$S_DATE"
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\multistate\COX regression\\$S_DATE\\single dynamic cohort CMD.xlsx", replace firstrow(variables)
