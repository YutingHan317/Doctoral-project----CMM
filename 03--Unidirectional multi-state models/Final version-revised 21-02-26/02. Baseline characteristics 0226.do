***************************************************************************************************************
**************************                                                   **********************************
**************************                Baseline Characteristics           **********************************
**************************                                                   **********************************
***************************************************************************************************************
//////////////// Modified on 2021-02-26
* 更新心血管代谢性共病的定义为缺血性脑卒中、《脑卒中（出血和缺血）》、二型糖尿病



/* 不进行任何调整，分六组 */
log using "D:\HanYT\2019-06 Multimorbidity\2. Result\Baseline characteristics.log",replace 

***********************************************************************************************************************************************
***********************************************************************************************************************************************
/* 进行调整，分六组 */

use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta", clear
****** 将对象分为以下6组：1.未发生心血管代谢性疾病未死亡；2.发生一个心血管代谢性疾病未死亡；3.发生两个心血管代谢性疾病未死亡；
****** 4.未发生心血管代谢性疾病死亡；5.发生一个心血管代谢性疾病死亡；6.发生两个心血管代谢性疾病死亡
gen table1_category = 1 if d_number_inc1 == 0 & du_ep0001 == 0
	replace table1_category = 2 if d_number_inc1 == 1 & du_ep0001 == 0
	replace table1_category = 3 if d_number_inc1 >  1 & du_ep0001 == 0
	replace table1_category = 4 if d_number_inc1 == 0 & du_ep0001 == 1
	replace table1_category = 5 if d_number_inc1 == 1 & du_ep0001 == 1
	replace table1_category = 6 if d_number_inc1 >  1 & du_ep0001 == 1
tab table1_category
keep if is_female == 0
tempname stats
tempfile table1
postfile `stats' str50 variable str50 category str50 group1 str50 group2 str50 groups3 ///
	str50 group4 str50 group5 str50 groups6 str50 total using `table1', replace

***No. of participants
qui{

	local variable "No. of participants"
	local category ""
	count if table1_category == 1
	local group1 = r(N)
	count if table1_category == 2
	local group2 = r(N)
	count if table1_category == 3
	local group3 = r(N)
	count if table1_category == 4
	local group4 = r(N)
	count if table1_category == 5
	local group5 = r(N)
	count if table1_category == 6
	local group6 = r(N)
	local total = _N
	post `stats' ("`variable'") ("") ("`group1'") ("`group2'") ("`group3'") ("`group4'") ("`group5'") ("`group6'") ("`total'")

}


***Age at baseline
local variable "Age at baseline"
local category ""

sum age_at_study_date if table1_category == 1
local group1 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date if table1_category == 2
local group2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date if table1_category == 3
local group3 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date if table1_category == 4
local group4 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date if table1_category == 5
local group5 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date if table1_category == 6
local group6 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
*** total
sum age_at_study_date
local total = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
post `stats' ("`variable'") ("") ("`group1'") ("`group2'") ("`group3'") ("`group4'") ("`group5'") ("`group6'") ("`total'")

*** Residence
post `stats' ("`Urban,%'") ("") ("") ("") ("") ("") ("") ("") ("") 
sum region_is_urban
loc min = r(min)
levelsof region_is_urban, local(level) clean
foreach l of local level{
	loc variable ""
	loc category:	label(region_is_urban) `l'
	mlogit region_is_urban i.table1_category age_at_study_date, baseoutcome(`min')
		margins table1_category, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
		mat a = r(table)
		loc group1 = string(a[1,1]*100, "%9.1f") 
		loc group2 = string(a[1,2]*100, "%9.1f") 
		loc group3 = string(a[1,3]*100, "%9.1f") 
		loc group4 = string(a[1,4]*100, "%9.1f") 
		loc group5 = string(a[1,5]*100, "%9.1f") 
		loc group6 = string(a[1,6]*100, "%9.1f") 
	** total
	count if region_is_urban == `l'
	local total = string(r(N)/_N*100, "%9.1f")
	post `stats' ("`variable'") ("`category'") ("`group1'") ("`group2'") ("`group3'") ("`group4'") ("`group5'") ("`group6'") ("`total'")
}


******Categorical variables
local varlist education_2groups marital_status_2groups parental_fh_cmm_2groups has_hypertension ///
	diet_component1 diet_component2 diet_component3 diet_component4 ///
	risky_smoking risky_alcohol risky_diet risky_PA risky_obesity risky_score_5groups
foreach var of local varlist{
	
	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") ("") 
	sum `var'
	loc min = r(min)
	levelsof `var', local(level) clean
	foreach l of local level{
		
		loc variable ""
		loc category:	label(`var') `l'
		mlogit `var' i.table1_category i.region_code age_at_study_date, baseoutcome(`min')
			margins table1_category, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
			mat a = r(table)
			loc group1 = string(a[1,1]*100, "%9.1f") 
			loc group2 = string(a[1,2]*100, "%9.1f") 
			loc group3 = string(a[1,3]*100, "%9.1f") 
			loc group4 = string(a[1,4]*100, "%9.1f") 
			loc group5 = string(a[1,5]*100, "%9.1f") 
			loc group6 = string(a[1,6]*100, "%9.1f") 

		** total
		count if `var' == `l'
		local total = string(r(N)/_N*100, "%9.1f")

		post `stats' ("`variable'") ("`category'") ("`group1'") ("`group2'") ("`group3'") ("`group4'") ("`group5'") ("`group6'") ("`total'")
	}

}

postclose `stats'
use `table1', clear
cap mkdir "D:\HanYT\2019-06 Multimorbidity\2. Result"
export excel using "D:\HanYT\2019-06 Multimorbidity\2. Result\Baseline characteristics adjust men.xlsx", firstrow(var) replace

/* women */
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta", clear
****** 将对象分为以下6组：1.未发生心血管代谢性疾病未死亡；2.发生一个心血管代谢性疾病未死亡；3.发生两个心血管代谢性疾病未死亡；
****** 4.未发生心血管代谢性疾病死亡；5.发生一个心血管代谢性疾病死亡；6.发生两个心血管代谢性疾病死亡
gen table1_category = 1 if d_number_inc1 == 0 & du_ep0001 == 0
	replace table1_category = 2 if d_number_inc1 == 1 & du_ep0001 == 0
	replace table1_category = 3 if d_number_inc1 >  1 & du_ep0001 == 0
	replace table1_category = 4 if d_number_inc1 == 0 & du_ep0001 == 1
	replace table1_category = 5 if d_number_inc1 == 1 & du_ep0001 == 1
	replace table1_category = 6 if d_number_inc1 >  1 & du_ep0001 == 1
tab table1_category
keep if is_female == 1
tempname stats
tempfile table1
postfile `stats' str50 variable str50 category str50 group1 str50 group2 str50 groups3 ///
	str50 group4 str50 group5 str50 groups6 str50 total using `table1', replace

***No. of participants
qui{

	local variable "No. of participants"
	local category ""
	count if table1_category == 1
	local group1 = r(N)
	count if table1_category == 2
	local group2 = r(N)
	count if table1_category == 3
	local group3 = r(N)
	count if table1_category == 4
	local group4 = r(N)
	count if table1_category == 5
	local group5 = r(N)
	count if table1_category == 6
	local group6 = r(N)
	local total = _N
	post `stats' ("`variable'") ("") ("`group1'") ("`group2'") ("`group3'") ("`group4'") ("`group5'") ("`group6'") ("`total'")

}


***Age at baseline
local variable "Age at baseline"
local category ""

sum age_at_study_date if table1_category == 1
local group1 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date if table1_category == 2
local group2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date if table1_category == 3
local group3 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date if table1_category == 4
local group4 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date if table1_category == 5
local group5 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date if table1_category == 6
local group6 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
*** total
sum age_at_study_date
local total = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
post `stats' ("`variable'") ("") ("`group1'") ("`group2'") ("`group3'") ("`group4'") ("`group5'") ("`group6'") ("`total'")

*** Residence
post `stats' ("`Urban,%'") ("") ("") ("") ("") ("") ("") ("") ("") 
sum region_is_urban
loc min = r(min)
levelsof region_is_urban, local(level) clean
foreach l of local level{
	loc variable ""
	loc category:	label(region_is_urban) `l'
	mlogit region_is_urban i.table1_category age_at_study_date, baseoutcome(`min')
		margins table1_category, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
		mat a = r(table)
		loc group1 = string(a[1,1]*100, "%9.1f") 
		loc group2 = string(a[1,2]*100, "%9.1f") 
		loc group3 = string(a[1,3]*100, "%9.1f") 
		loc group4 = string(a[1,4]*100, "%9.1f") 
		loc group5 = string(a[1,5]*100, "%9.1f") 
		loc group6 = string(a[1,6]*100, "%9.1f") 
	** total
	count if region_is_urban == `l'
	local total = string(r(N)/_N*100, "%9.1f")
	post `stats' ("`variable'") ("`category'") ("`group1'") ("`group2'") ("`group3'") ("`group4'") ("`group5'") ("`group6'") ("`total'")
}

******Categorical variables
local varlist education_2groups marital_status_2groups parental_fh_cmm_2groups has_hypertension ///
	diet_component1 diet_component2 diet_component3 diet_component4 ///
	risky_smoking risky_alcohol risky_diet risky_PA risky_obesity risky_score_5groups
foreach var of local varlist{
	
	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") ("") 
	sum `var'
	loc min = r(min)
	levelsof `var', local(level) clean
	foreach l of local level{
		
		loc variable ""
		loc category:	label(`var') `l'
		mlogit `var' i.table1_category i.region_code age_at_study_date, baseoutcome(`min')
			margins table1_category, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
			mat a = r(table)
			loc group1 = string(a[1,1]*100, "%9.1f") 
			loc group2 = string(a[1,2]*100, "%9.1f") 
			loc group3 = string(a[1,3]*100, "%9.1f") 
			loc group4 = string(a[1,4]*100, "%9.1f") 
			loc group5 = string(a[1,5]*100, "%9.1f") 
			loc group6 = string(a[1,6]*100, "%9.1f") 

		** total
		count if `var' == `l'
		local total = string(r(N)/_N*100, "%9.1f")

		post `stats' ("`variable'") ("`category'") ("`group1'") ("`group2'") ("`group3'") ("`group4'") ("`group5'") ("`group6'") ("`total'")
	}

}

postclose `stats'
use `table1', clear
cap mkdir "D:\HanYT\2019-06 Multimorbidity\2. Result"
export excel using "D:\HanYT\2019-06 Multimorbidity\2. Result\Baseline characteristics adjust women.xlsx", firstrow(var) replace


log close


