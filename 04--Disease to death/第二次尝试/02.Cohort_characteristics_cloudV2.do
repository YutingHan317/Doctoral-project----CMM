*********************************************************
* Creator: Hanyuting
* Created date: 2021-06-30
/* Updating log */ 
* 21/07/03: 降糖药不存在预防性用药，因此只要没有糖尿病就不可能服用降糖药物，因此计算这个分布的时候 需要剔除绝对相关的
* 21/10/21: combine male and female
* 21/10/21: change "by baseline disease" to "by follow-up disease"

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                       Part 2: Corhort characteristics                          *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************

*cd "D:\HanYT\2021-02_CMMtodeath"
log using "2.Result\Log\Cohort characteristics $S_DATE.log", replace 
**********************************************************************************************************
*************                                                                                *************
*************                      Part 2: Baseline characteristics                          *************
*************                                                                                *************
**********************************************************************************************************


**********************************************************************************************************
*************                    Part 2.1: Number of CMDs (3groups)                          *************
**********************************************************************************************************

/* Whole */

use "1.Data/project2.dta", clear

***** Postfile
tempname stats
tempfile table1
postfile `stats' str50 (variable category base_number0 base_number1 base_number2 total) using `table1', replace

***** Number of participants
count if d_number2_base == 0
local base_number0 = r(N)
count if d_number2_base == 1
local base_number1 = r(N)
count if d_number2_base == 2
local base_number2 = r(N)
count
local total = r(N)
post `stats' ("No.") ("") ("`base_number0'") ("`base_number1'") ("`base_number2'") ("`total'")  

****** Age, no adjustment
tabstat age_at_study_date, s(mean sd) by(d_number2_base) save
	mat a = r(Stat1)
	local base_number0 = string(a[1,1],"%9.2f") + " (" + string(a[2,1],"%9.2f") + ")"
 	mat b = r(Stat2)
	local base_number1 = string(b[1,1],"%9.2f") + " (" + string(b[2,1],"%9.2f") + ")"
	mat c = r(Stat3)
	local base_number2 = string(c[1,1],"%9.2f") + " (" + string(c[2,1],"%9.2f") + ")"
	mat d = r(StatTotal)
local total = string(d[1,1],"%9.2f") + " (" + string(d[2,1],"%9.2f") + ")"
post `stats' ("`variable'") ("") ("`base_number0'") ("`base_number1'") ("`base_number2'")  ("`total'")

****** Sex
*** proportion
mlogit is_female i.d_number2_base, baseoutcome(0)
	margins d_number2_base, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
	mat a = r(table)
		loc base_number0 = string(a[1,1]*100, "%9.1f") 
		loc base_number1 = string(a[1,2]*100, "%9.1f") 
		loc base_number2 = string(a[1,3]*100, "%9.1f")
	* Total
	count if is_female == 1
	local total = string(r(N)/_N*100, "%9.1f")
post `stats' ("Sex, %") ("") ("`base_number0'") ("`base_number1'") ("`base_number2'") ("`total'") 

****** Residence
*** proportion
mlogit region_is_urban i.d_number2_base age_at_study_date, baseoutcome(0)
	margins d_number2_base, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
	mat a = r(table)
		loc base_number0 = string(a[1,1]*100, "%9.1f") 
		loc base_number1 = string(a[1,2]*100, "%9.1f") 
		loc base_number2 = string(a[1,3]*100, "%9.1f")
	* Total
	count if region_is_urban == 1
	local total = string(r(N)/_N*100, "%9.1f")
post `stats' ("Urban, %") ("") ("`base_number0'") ("`base_number1'") ("`base_number2'") ("`total'") 

******Categorical variables; no menopause status
local varlist education_2groups marital_status_2groups diabetes_fh_2groups chd_fh_2groups stoke_fh_2groups chronic_fh_2groups ///
	healthy_smoking healthy_alcohol healthy_diet healthy_PA bmi_4groups WC_3groups ///
	taking_aspirin_2groups taking_ace_i_2groups taking_beta_blocker_2groups taking_statins_2groups taking_diuretics_2groups taking_ca_antagonist_2groups ///
	taking_chlor_metaformin_2groups taking_insulin_2groups hypertension_lowering_2g glucose_lowering_2groups ///
	has_hypertension has_copd cancer_diag rheum_heart_dis_diag kidney_dis_diag
foreach var of local varlist{
	
	if "`var'" == "taking_chlor_metaformin_2groups" | "`var'" == "taking_insulin_2groups" | "`var'" == "glucose_lowering_2groups" {

		loc variable: variable label `var'
		di "******************************** `variable'"
		post `stats' ("`variable'") ("") ("") ("") ("") ("")  
		sum `var'
		loc min = r(min)
		levelsof `var', local(level) clean
		mlogit `var' i.d_number2_base i.region_code age_at_study_date if d_number2_base != 0, baseoutcome(`min')
		foreach l of local level{
			
			loc variable ""
			loc category:	label(`var') `l'
				margins d_number2_base, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
				mat a = r(table)
				loc base_number1 = string(a[1,1]*100, "%9.1f") 
				loc base_number2 = string(a[1,2]*100, "%9.1f") 

			** total
			count if `var' == `l'
			local total = string(r(N)/_N*100, "%9.1f")

			post `stats' ("`variable'") ("`category'") ("--") ("`base_number1'") ("`base_number2'")  ("`total'")
		}

	}
	else{
		loc variable: variable label `var'
		di "*******************************  `variable'"
		post `stats' ("`variable'") ("") ("") ("") ("") ("")  
		sum `var'
		loc min = r(min)
		levelsof `var', local(level) clean
		mlogit `var' i.d_number2_base i.region_code age_at_study_date, baseoutcome(`min')
		foreach l of local level{
			
			loc variable ""
			loc category:	label(`var') `l'
				margins d_number2_base, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
				mat a = r(table)
				loc base_number0 = string(a[1,1]*100, "%9.1f") 
				loc base_number1 = string(a[1,2]*100, "%9.1f") 
				loc base_number2 = string(a[1,3]*100, "%9.1f") 

			** total
			count if `var' == `l'
			local total = string(r(N)/_N*100, "%9.1f")

			post `stats' ("`variable'") ("`category'") ("`base_number0'") ("`base_number1'") ("`base_number2'")  ("`total'")
		}
	}

}


postclose `stats'
use `table1', clear


cap mkdir "2.Result/Cohort characteristics"
export excel using "2.Result/Cohort characteristics/Baseline characteristics whole disease number $S_DATE.xlsx", firstrow(var) replace


**********************************************************************************************************
*************                    Part 2.2: Combination of CMDs (8groups)                     *************
**********************************************************************************************************

/* Whole */

use "1.Data/project2.dta", clear

***** Postfile
tempname stats
tempfile table1
postfile `stats' str50 (variable category base_comb0 base_comb1 base_comb2 base_comb3 base_comb4 base_comb5 base_comb6 base_comb7 total) using `table1', replace

***** Number of participants
count if combin_base == 0
local base_comb0 = r(N)
count if combin_base == 1
local base_comb1 = r(N)
count if combin_base == 2
local base_comb2 = r(N)
count if combin_base == 3
local base_comb3 = r(N)
count if combin_base == 4
local base_comb4 = r(N)
count if combin_base == 5
local base_comb5 = r(N)
count if combin_base == 6
local base_comb6 = r(N)
count if combin_base == 7
local base_comb7 = r(N)
count
local total = r(N)
post `stats' ("No.") ("") ("`base_comb0'") ("`base_comb1'") ("`base_comb2'") ("`base_comb3'") ("`base_comb4'") ("`base_comb5'") ("`base_comb6'") ("`base_comb7'") ("`total'")

****** Age, no adjustment
tabstat age_at_study_date, s(mean sd) by(combin_base) save
	mat a = r(Stat1)
	local base_comb0 = string(a[1,1],"%9.2f") + " (" + string(a[2,1],"%9.2f") + ")"
	mat b = r(Stat2)
	local base_comb1 = string(b[1,1],"%9.2f") + " (" + string(b[2,1],"%9.2f") + ")"
	mat c = r(Stat3)
	local base_comb2 = string(c[1,1],"%9.2f") + " (" + string(c[2,1],"%9.2f") + ")"
	mat c = r(Stat4)
	local base_comb3 = string(c[1,1],"%9.2f") + " (" + string(c[2,1],"%9.2f") + ")"
	mat c = r(Stat5)
	local base_comb4 = string(c[1,1],"%9.2f") + " (" + string(c[2,1],"%9.2f") + ")"
	mat c = r(Stat6)
	local base_comb5 = string(c[1,1],"%9.2f") + " (" + string(c[2,1],"%9.2f") + ")"
	mat c = r(Stat7)
	local base_comb6 = string(c[1,1],"%9.2f") + " (" + string(c[2,1],"%9.2f") + ")"
	mat c = r(Stat8)
	local base_comb7 = string(c[1,1],"%9.2f") + " (" + string(c[2,1],"%9.2f") + ")"
	mat d = r(StatTotal)
	local total = string(d[1,1],"%9.2f") + " (" + string(d[2,1],"%9.2f") + ")"
post `stats' ("Age, year") ("") ("`base_comb0'") ("`base_comb1'") ("`base_comb2'") ("`base_comb3'") ("`base_comb4'") ("`base_comb5'") ("`base_comb6'") ("`base_comb7'") ("`total'")

****** Sex, no adjustment
mlogit is_female i.combin_base, baseoutcome(0)
	margins combin_base, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
	mat a = r(table)
		loc base_comb0 = string(a[1,1]*100, "%9.1f") 
		loc base_comb1 = string(a[1,2]*100, "%9.1f")
		loc base_comb2 = string(a[1,3]*100, "%9.1f")
		loc base_comb3 = string(a[1,4]*100, "%9.1f")
		loc base_comb4 = string(a[1,5]*100, "%9.1f")
		loc base_comb5 = string(a[1,6]*100, "%9.1f")
		loc base_comb6 = string(a[1,7]*100, "%9.1f")
		loc base_comb7 = string(a[1,8]*100, "%9.1f")
	* Total
	count if is_female == 1
	local total = string(r(N)/_N*100, "%9.1f")
post `stats' ("Female, %") ("") ("`base_comb0'") ("`base_comb1'") ("`base_comb2'") ("`base_comb3'") ("`base_comb4'") ("`base_comb5'") ("`base_comb6'") ("`base_comb7'") ("`total'") 


****** Residence
*** proportion
mlogit region_is_urban i.combin_base age_at_study_date, baseoutcome(0)
	margins combin_base, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
	mat a = r(table)
		loc base_comb0 = string(a[1,1]*100, "%9.1f") 
		loc base_comb1 = string(a[1,2]*100, "%9.1f")
		loc base_comb2 = string(a[1,3]*100, "%9.1f")
		loc base_comb3 = string(a[1,4]*100, "%9.1f")
		loc base_comb4 = string(a[1,5]*100, "%9.1f")
		loc base_comb5 = string(a[1,6]*100, "%9.1f")
		loc base_comb6 = string(a[1,7]*100, "%9.1f")
		loc base_comb7 = string(a[1,8]*100, "%9.1f")
	* Total
	count if region_is_urban == 1
	local total = string(r(N)/_N*100, "%9.1f")
post `stats' ("Urban, %") ("") ("`base_comb0'") ("`base_comb1'") ("`base_comb2'") ("`base_comb3'") ("`base_comb4'") ("`base_comb5'") ("`base_comb6'") ("`base_comb7'") ("`total'") 

******Categorical variables; no menopause status
local varlist education_2groups marital_status_2groups diabetes_fh_2groups chd_fh_2groups stoke_fh_2groups chronic_fh_2groups ///
	healthy_smoking healthy_alcohol healthy_diet healthy_PA bmi_4groups WC_3groups ///
	taking_aspirin_2groups taking_ace_i_2groups taking_beta_blocker_2groups taking_statins_2groups taking_diuretics_2groups taking_ca_antagonist_2groups ///
	taking_chlor_metaformin_2groups taking_insulin_2groups hypertension_lowering_2g glucose_lowering_2groups ///
	has_hypertension has_copd cancer_diag rheum_heart_dis_diag kidney_dis_diag
foreach var of local varlist{
	
	loc variable: variable label `var'
	di "******************    `variable'"
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") 

	*** 考虑糖尿病
	if "`var'" == "taking_chlor_metaformin_2groups" | "`var'" == "taking_insulin_2groups" | "`var'" == "glucose_lowering_2groups" {
	
		sum `var'
		loc min = r(min)
		levelsof `var', local(level) clean
		mlogit `var' i.combin_base i.region_code age_at_study_date if combin_base != 0 & combin_base != 2 & combin_base != 3 & combin_base != 6 , baseoutcome(`min')
		foreach l of local level{
			
			loc variable ""
			loc category:	label(`var') `l'
				margins combin_base, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
				mat a = r(table)
				loc base_comb1 = string(a[1,1]*100, "%9.1f") 
				loc base_comb4 = string(a[1,2]*100, "%9.1f") 
				loc base_comb5 = string(a[1,3]*100, "%9.1f") 
				loc base_comb7 = string(a[1,4]*100, "%9.1f") 

			** total
			count if `var' == `l'
			local total = string(r(N)/_N*100, "%9.1f")

			post `stats' ("`variable'") ("`category'") ("--") ("`base_comb1'") ("--") ("--") ("`base_comb4'") ("`base_comb5'") ("--") ("`base_comb7'") ("`total'")
		}
	}
	else{
	sum `var'
	loc min = r(min)
	levelsof `var', local(level) clean
	mlogit `var' i.combin_base i.region_code age_at_study_date, baseoutcome(`min')
	foreach l of local level{
		
			loc variable ""
			loc category:	label(`var') `l'
				margins combin_base, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
				mat a = r(table)
				loc base_comb0 = string(a[1,1]*100, "%9.1f") 
				loc base_comb1 = string(a[1,2]*100, "%9.1f") 
				loc base_comb2 = string(a[1,3]*100, "%9.1f") 
				loc base_comb3 = string(a[1,4]*100, "%9.1f") 
				loc base_comb4 = string(a[1,5]*100, "%9.1f") 
				loc base_comb5 = string(a[1,6]*100, "%9.1f") 
				loc base_comb6 = string(a[1,7]*100, "%9.1f") 
				loc base_comb7 = string(a[1,8]*100, "%9.1f") 

			** total
			count if `var' == `l'
			local total = string(r(N)/_N*100, "%9.1f")

			post `stats' ("`variable'") ("`category'") ("`base_comb0'") ("`base_comb1'") ("`base_comb2'") ("`base_comb3'") ("`base_comb4'") ("`base_comb5'") ("`base_comb6'") ("`base_comb7'") ("`total'")
		}
	}

}


postclose `stats'
use `table1', clear


cap mkdir "2.Result/Cohort characteristics"
export excel using "2.Result/Cohort characteristics/Baseline characteristics whole disease combination $S_DATE.xlsx", firstrow(var) replace


log close
