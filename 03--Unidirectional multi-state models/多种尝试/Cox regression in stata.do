***************************************************************************************************************
*****************************                                       *******************************************
*****************************           COX regression              *******************************************
*****************************                                       *******************************************
***************************************************************************************************************

cap mkdir "D:\HanYT\2019-06 Multimorbidity\3. Result\multistate\COX regression\\$S_DATE"
*****************************************      单因素      *******************************

/* 设置全局宏 */
global adjusted_cox i.is_female i.highest_education i.marital_status_2groups i.mltmbd_fh_3groups 
global endpoints first_cmd_inc mltmbd_inc du_ep0001 
global single_var healthy_smoking healthy_alcohol healthy_diet healthy_bmi healthy_WC healthy_PA

foreach ep of global endpoints{


	di _dup(30) "*" "`ep'"  _dup(30) "*"

	/* Stset */
	use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta", clear
	stset `ep'_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(`ep' == 1)
	local lifestyle_list  ""
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
	gen py = (`ep'_date - study_date)/365.25
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
			count if `vn' == `l' & `ep' == 1
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
	export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\multistate\COX regression\\$S_DATE\\single `ep' $S_DATE.xlsx", replace firstrow(variables)

}


*****************************************      生活方式得分（分类）      *******************************

/* 设置全局宏 */
global adjusted_cox i.is_female i.highest_education i.marital_status_2groups i.mltmbd_fh_3groups 
global endpoints first_cmd_inc mltmbd_inc du_ep0001 
global single_var healthy_score_5groups

foreach ep of global endpoints{


	di _dup(30) "*" "`ep'"  _dup(30) "*"

	/* Stset */
	use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta", clear
	stset `ep'_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(`ep' == 1)
	local lifestyle_list  ""
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
	gen py = (`ep'_date - study_date)/365.25
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
			count if `vn' == `l' & `ep' == 1
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
	export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\multistate\COX regression\\$S_DATE\\score `ep' $S_DATE.xlsx", replace firstrow(variables)

}

*****************************************      生活方式得分（等级）      *******************************

foreach ep of global endpoints{

	qui use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta", clear
	qui stset `ep'_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(`ep' == 1)
	stcox healthy_score_5groups $adjusted_cox, strata(age_strata region_code)

}
