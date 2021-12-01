***************************************************************************************************************
*****************************                                       *******************************************
*****************************             Baseline CMDs             *******************************************
*****************************                                       *******************************************
***************************************************************************************************************
*** analyse risk effects instead of protective effects

cap mkdir "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios"
cap mkdir "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE"
* log using "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE\Hazard ratios.log",replace 
*****************************************      单因素      *******************************

/* 设置全局宏 */
*** Covariates for adjustment
global adjusted_model1 i.is_female 
global adjusted_model2 $adjusted_model1 i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups
global adjusted_model3 $adjusted_model2 i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups
global adjusted_model4 $adjusted_model3 i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups
global adjusted_model5 $adjusted_model3 i.has_hypertension i.kidney_dis_diag
global adjusted_model6 $adjusted_model4 i.has_hypertension i.kidney_dis_diag

*** Endpoints
global endpoints du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999

*** Exporsure
global exposure_var1 d_number_base
global exposure_var2 has_diabetes chd_diag stroke_or_tia_diag
global exposure_var3 combin_base


foreach ep of global endpoints{


	di _dup(30) "*" "`ep'"  _dup(30) "*"


	forvalues c = 1/6{
			
		/* Stset */
		use "D:\HanYT\2021-02 CMM to death\1. Data\project2.dta", clear
		stset `ep'_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(`ep' == 1)

		****** Output file	
		tempname coef
		tempfile coef_data`c'
		save `coef_data`c'', replace
		postfile `coef' str50 name str50 hr str50 lb str50 ub str50 hr_ci using `coef_data`c'', replace
		forvalues n = 1/3{
			
			di _dup(30) "#" "`ep' ${exposure_var`n'} Model`c'"  _dup(30) "*#"
			post `coef' ("") ("Model`c'") ("") ("") ("")
			local exposure_list`n'  ""
			foreach var of global exposure_var`n'{

				local exposure_list`n' `exposure_list`n'' i.`var'
			}
			* COX regression
			stcox `exposure_list`n'' ${adjusted_model`c'} , strata(age_strata region_code)
			mat mat_hr`c' = r(table)
				
			* Import HRs to the tables
			local i = 0

			foreach var of global exposure_var`n'{
			
				local var_name: variable label `var'
				post `coef' ("`var_name'") ("") ("") ("") ("")
				levelsof `var', local(level) clean
				foreach l of local level{
			
					local ++i
					local label_name: label(`var') `l'
					local hr = string(mat_hr`c'[1,`i'],"%9.2f")
					if "`hr'" == "1.00"{
			
						local ci = "Reference"
						local lb = ""
						local ub = ""
						local hr_ci = "Reference"
			
					}
					else{
			
						local lb = string(mat_hr`c'[5,`i'],"%9.2f") 
						local ub = string(mat_hr`c'[6,`i'],"%9.2f") 
						local hr_ci = "`hr'" + " (" + "`lb'" + "-" + "`ub'" + ")"
					}
			
					post `coef'  ("`label_name'") ("`hr'") ("`lb'") ("`ub'") ("`hr_ci'")
			
				}
			
			}
		}

		postclose `coef'
		use  `coef_data`c'', clear 
		rename hr hr`c'
		rename lb lb`c'
		rename ub ub`c'
		rename hr_ci hr_ci`c'
		save `coef_data`c'', replace 

		}
	
	/* IR */
	use "D:\HanYT\2021-02 CMM to death\1. Data\project2.dta", clear
	gen py = (`ep'_date - study_date)/365.25
	tempname IR
	tempfile IR_data
	save `IR_data'
	postfile `IR' str50 name number_cases str50 IR using `IR_data', replace
	forvalues n = 1/3{

		post `IR' ("") (.) ("") 
		foreach vn of global exposure_var`n'{

			local name: variable label `vn'
			post `IR' ("`name'") (.) ("") 
			levelsof `vn', local(level) clean
			foreach l of local level{

				local level_label: label(`vn')`l'
				sum py if `vn' == `l'
				local py_sum = r(sum)
				count if `vn' == `l' & `ep' == 1
				local ep_count = r(N)
				local ir = string(`ep_count'/`py_sum'*10000,"%9.2f")
				post `IR' ("`level_label'")  (`ep_count') ("`ir'")
			}

		}
	}
	postclose `IR'

	*** Merge all the data
	use `IR_data',clear
			
	forvalues c = 1/6{
	
		merge 1:1 _n using `coef_data`c'', nogen keepusing(hr`c' lb`c' ub`c' hr_ci`c')

	}
	cap mkdir "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE" 
	export excel using "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE\\`ep' baseline $S_DATE.xlsx", replace firstrow(variables)
	
}








***************************************************************************************************************
*****************************                                       *******************************************
*****************************             Updated CMDs             *******************************************
*****************************                                       *******************************************
***************************************************************************************************************
*** analyse risk effects instead of protective effects

cap mkdir "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios"
cap mkdir "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE"
* log using "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE\Hazard ratios.log",replace 
*****************************************      单因素      *******************************

/* 设置全局宏 */
*** Covariates for adjustment
global adjusted_model1 i.is_female 
global adjusted_model2 $adjusted_model1 i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups
global adjusted_model3 $adjusted_model2 i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups
global adjusted_model4 $adjusted_model3 i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups
global adjusted_model5 $adjusted_model3 i.has_hypertension i.kidney_dis_diag
global adjusted_model6 $adjusted_model4 i.has_hypertension i.kidney_dis_diag

*** Endpoints
global endpoints du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999

*** Exporsure
global exposure_var1 d_number_updated
global exposure_var2 diabetes_diag_updated chd_diag_updated stroke_diag_updated
global exposure_var3 combin_updated


foreach ep of global endpoints{


	di _dup(30) "*" "`ep'"  _dup(30) "*"


	forvalues c = 1/6{
			
		/* Stset */
		use "D:\HanYT\2021-02 CMM to death\1. Data\project2_updated_`ep'.dta", clear

		****** Output file	
		tempname coef
		tempfile coef_data`c'
		save `coef_data`c'', replace
		postfile `coef' str50 name str50 hr str50 lb str50 ub str50 hr_ci using `coef_data`c'', replace
		forvalues n = 1/3{
			
			di _dup(30) "#" "`ep' ${exposure_var`n'} Model`c'"  _dup(30) "*#"
			post `coef' ("") ("Model`c'") ("") ("") ("")
			local exposure_list`n'  ""
			foreach var of global exposure_var`n'{

				local exposure_list`n' `exposure_list`n'' i.`var'
			}
			* COX regression
			stcox `exposure_list`n'' ${adjusted_model`c'} , strata(age_strata region_code)
			mat mat_hr`c' = r(table)
				
			* Import HRs to the tables
			local i = 0

			foreach var of global exposure_var`n'{
			
				local var_name: variable label `var'
				post `coef' ("`var_name'") ("") ("") ("") ("")
				levelsof `var', local(level) clean
				foreach l of local level{
			
					local ++i
					local label_name: label(`var') `l'
					local hr = string(mat_hr`c'[1,`i'],"%9.2f")
					if "`hr'" == "1.00"{
			
						local ci = "Reference"
						local lb = ""
						local ub = ""
						local hr_ci = "Reference"
			
					}
					else{
			
						local lb = string(mat_hr`c'[5,`i'],"%9.2f") 
						local ub = string(mat_hr`c'[6,`i'],"%9.2f") 
						local hr_ci = "`hr'" + " (" + "`lb'" + "-" + "`ub'" + ")"
					}
			
					post `coef'  ("`label_name'") ("`hr'") ("`lb'") ("`ub'") ("`hr_ci'")
			
				}
			
			}
		}

		postclose `coef'
		use  `coef_data`c'', clear 
		rename hr hr`c'
		rename lb lb`c'
		rename ub ub`c'
		rename hr_ci hr_ci`c'
		save `coef_data`c'', replace 

		}
	
	/* IR */
	use "D:\HanYT\2021-02 CMM to death\1. Data\project2_updated_`ep'.dta", clear
	gen py = _t - _t0   // 根据time-varying 需要进行修改
	tempname IR
	tempfile IR_data
	save `IR_data'
	postfile `IR' str50 name number_cases str50 IR using `IR_data', replace
	forvalues n = 1/3{

		post `IR' ("") (.) ("") 
		foreach vn of global exposure_var`n'{

			local name: variable label `vn'
			post `IR' ("`name'") (.) ("") 
			levelsof `vn', local(level) clean
			foreach l of local level{

				local level_label: label(`vn')`l'
				sum py if `vn' == `l'
				local py_sum = r(sum)
				count if `vn' == `l' & _d == 1
				local ep_count = r(N)
				local ir = string(`ep_count'/`py_sum'*10000,"%9.2f")
				post `IR' ("`level_label'")  (`ep_count') ("`ir'")
			}

		}
	}
	postclose `IR'

	*** Merge all the data
	use `IR_data',clear
			
	forvalues c = 1/6{
	
		merge 1:1 _n using `coef_data`c'', nogen keepusing(hr`c' lb`c' ub`c' hr_ci`c')

	}
	cap mkdir "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE" 
	export excel using "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE\\`ep' Updated $S_DATE.xlsx", replace firstrow(variables)
	
}



***************************************************************************************************************
*****************************                                       *******************************************
*****************************            Baseline duration          *******************************************
*****************************                                       *******************************************
***************************************************************************************************************
*** analyse risk effects instead of protective effects

cap mkdir "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios"
cap mkdir "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE"
* log using "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE\Hazard ratios.log",replace 
*****************************************      单因素      *******************************

/* 设置全局宏 */
*** Covariates for adjustment
global adjusted_model1 i.is_female 
global adjusted_model2 $adjusted_model1 i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups
global adjusted_model3 $adjusted_model2 i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups
global adjusted_model4 $adjusted_model3 i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups
global adjusted_model5 $adjusted_model3 i.has_hypertension i.kidney_dis_diag
global adjusted_model6 $adjusted_model4 i.has_hypertension i.kidney_dis_diag

*** Endpoints
global endpoints du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999

*** Exporsure
global exposure_var1 duration_diabetes_groups duration_chd_groups duration_stroke_groups
global exposure_var2 duration_diabetes_updated_groups duration_chd_updated_groups duration_stroke_updated_groups



foreach ep of global endpoints{


	di _dup(30) "*" "`ep'"  _dup(30) "*"


	forvalues c = 1/6{
			
		/* Stset */
		use "D:\HanYT\2021-02 CMM to death\1. Data\project2_updated_`ep'_duration.dta", clear

		****** Output file	
		tempname coef
		tempfile coef_data`c'
		save `coef_data`c'', replace
		postfile `coef' str50 name str50 hr str50 lb str50 ub str50 hr_ci using `coef_data`c'', replace
		forvalues n = 1/2{
			
			di _dup(30) "#" "`ep' ${exposure_var`n'} Model`c'"  _dup(30) "*#"
			post `coef' ("") ("Model`c'") ("") ("") ("")
			local exposure_list`n'  ""
			foreach var of global exposure_var`n'{

				local exposure_list`n' `exposure_list`n'' b99.`var'
			}
			* COX regression
			stcox `exposure_list`n'' ${adjusted_model`c'} , strata(age_strata region_code)
			mat mat_hr`c' = r(table)
				
			* Import HRs to the tables
			local i = 0

			foreach var of global exposure_var`n'{
			
				local var_name: variable label `var'
				post `coef' ("`var_name'") ("") ("") ("") ("")
				levelsof `var', local(level) clean
				foreach l of local level{
			
					local ++i
					local label_name: label(`var') `l'
					local hr = string(mat_hr`c'[1,`i'],"%9.2f")
					if "`hr'" == "1.00"{
			
						local ci = "Reference"
						local lb = ""
						local ub = ""
						local hr_ci = "Reference"
			
					}
					else{
			
						local lb = string(mat_hr`c'[5,`i'],"%9.2f") 
						local ub = string(mat_hr`c'[6,`i'],"%9.2f") 
						local hr_ci = "`hr'" + " (" + "`lb'" + "-" + "`ub'" + ")"
					}
			
					post `coef'  ("`label_name'") ("`hr'") ("`lb'") ("`ub'") ("`hr_ci'")
			
				}
			
			}
		}

		postclose `coef'
		use  `coef_data`c'', clear 
		rename hr hr`c'
		rename lb lb`c'
		rename ub ub`c'
		rename hr_ci hr_ci`c'
		save `coef_data`c'', replace 

		}
	
	/* IR */
	use "D:\HanYT\2021-02 CMM to death\1. Data\project2_updated_`ep'_duration.dta", clear
	gen py = _t - _t0   // 根据time-varying 需要进行修改
	tempname IR
	tempfile IR_data
	save `IR_data'
	postfile `IR' str50 name number_cases str50 IR using `IR_data', replace
	forvalues n = 1/3{

		post `IR' ("") (.) ("") 
		foreach vn of global exposure_var`n'{

			local name: variable label `vn'
			post `IR' ("`name'") (.) ("") 
			levelsof `vn', local(level) clean
			foreach l of local level{

				local level_label: label(`vn')`l'
				sum py if `vn' == `l'
				local py_sum = r(sum)
				count if `vn' == `l' & _d == 1
				local ep_count = r(N)
				local ir = string(`ep_count'/`py_sum'*10000,"%9.2f")
				post `IR' ("`level_label'")  (`ep_count') ("`ir'")
			}

		}
	}
	postclose `IR'

	*** Merge all the data
	use `IR_data',clear
			
	forvalues c = 1/6{
	
		merge 1:1 _n using `coef_data`c'', nogen keepusing(hr`c' lb`c' ub`c' hr_ci`c')

	}
	cap mkdir "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE" 
	export excel using "D:\HanYT\2021-02 CMM to death\2. Result\Hazard ratios\\$S_DATE\\`ep' duration $S_DATE.xlsx", replace firstrow(variables)
	
}
