**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                        Part 1: descriptive analyses                            *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************


**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                TABLE 4: Matched disease compare adjusted for CMD               *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************

***根据以下四组变量进行展示
use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
tempname stats
tempfile table3
postfile `stats' str50 variable str50 category str50 healthy_baseline str50 healthy_resurvey2 str50 fcmd_baseline str50 fcmd_resurvey2 ///
	str50 mltmbd_baseline str50 mltmbd_resurvey2 str50 total_baseline str50 total_resurvey2 using `table3', replace

***No. of participants
qui{

	local variable "No. of participants"
	local category ""
	count if cmd_number_3groups == 0
	local healthy_baseline = r(N)
	local healthy_resurvey2 = r(N)
	count if cmd_number_3groups == 1
	local fcmd_baseline = r(N)
	local fcmd_resurvey2 = r(N)
	count if cmd_number_3groups == 2
	local mltmbd_baseline = r(N)
	local mltmbd_resurvey2 = r(N)
	local total_baseline = _N
	local total_resurvey2 = _N
	post `stats' ("") ("") ("`healthy_baseline'") ("`healthy_resurvey2'") ("`fcmd_baseline'") ("`fcmd_resurvey2'") ("`mltmbd_baseline'") ///
		("`mltmbd_resurvey2'") ("`total_baseline'") ("`total_resurvey2'")

}

***Age
local variable "Age"
local category ""
*** Healhthy
sum age_at_study_date if cmd_number_3groups == 0
local healthy_baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date2 if cmd_number_3groups == 0	
local healthy_resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
*** only on cmd
sum age_at_study_date if cmd_number_3groups == 1
local fcmd_baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date2 if cmd_number_3groups == 1	
local fcmd_resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
*** multimorbidity
sum age_at_study_date if cmd_number_3groups == 2
local mltmbd_baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date2 if cmd_number_3groups == 2	
local mltmbd_resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
*** total
sum age_at_study_date
local total_baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
sum age_at_study_date2	
local total_resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
post `stats' ("`variable'") ("") ("`healthy_baseline'") ("`healthy_resurvey2'") ("`fcmd_baseline'") ("`fcmd_resurvey2'") ("`mltmbd_baseline'") ///
	("`mltmbd_resurvey2'") ("`total_baseline'") ("`total_resurvey2'")

******Continuous variables(adjusted for age, region and sex)
loc varlist met bmi_calc
foreach var of local varlist{

	qui{
		loc variable: variable label `var'
		loc category ""
		**baseline
		sum `var' if cmd_number_3groups == 0
		loc healthy_sd = r(sd)
		sum `var' if cmd_number_3groups == 1
		loc fcmd_sd = r(sd)
		sum `var' if cmd_number_3groups == 2
		loc mltmbd_sd = r(sd)

		anova `var' cmd_number_3groups c.age_at_study_date region_code is_female
		margins cmd_number_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)
		mat a = r(table)
		loc healthy_baseline = string(a[1,1], "%9.1f") + "(" + string(`healthy_sd', "%9.1f") + ")"
		loc fcmd_baseline = string(a[1,2], "%9.1f") + "(" + string(`fcmd_sd', "%9.1f") + ")"
		loc mltmbd_baseline = string(a[1,3], "%9.1f") + "(" + string(`mltmbd_sd', "%9.1f") + ")"
		**resurvey2
		sum `var'2 if cmd_number_3groups == 0
		loc healthy_sd = r(sd)
		sum `var'2 if cmd_number_3groups == 1
		loc fcmd_sd = r(sd)
		sum `var'2 if cmd_number_3groups == 2
		loc mltmbd_sd = r(sd)
		anova `var'2 cmd_number_3groups c.age_at_study_date2 region_code is_female
		margins cmd_number_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)
		mat a = r(table)
		loc healthy_resurvey2 = string(a[1,1], "%9.1f") + "(" + string(`healthy_sd', "%9.1f") + ")"
		loc fcmd_resurvey2 = string(a[1,2], "%9.1f") + "(" + string(`fcmd_sd', "%9.1f") + ")"
		loc mltmbd_resurvey2 = string(a[1,3], "%9.1f") + "(" + string(`mltmbd_sd', "%9.1f") + ")"
		*** total
		sum `var'
		local total_baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
		sum `var'2	
		local total_resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
		post `stats' ("`variable'") ("") ("`healthy_baseline'") ("`healthy_resurvey2'") ("`fcmd_baseline'") ("`fcmd_resurvey2'") ("`mltmbd_baseline'") ///
			("`mltmbd_resurvey2'") ("`total_baseline'") ("`total_resurvey2'")

		}

}

******Categorical variables
local varlist smoking_5groups alcohol_5groups diet_5groups bmi_4groups WC_3groups ///
	risky_smoking risky_alcohol risky_diet risky_bmi risky_WC risky_PA
foreach var of local varlist{
	
	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") ("") ("")
	sum `var'
	loc min = r(min)
	levelsof `var', local(level) clean
	foreach l of local level{
		
		loc variable ""
		loc category:	label(`var') `l'
		* baseline 
		count if `var' == `l' & cmd_number_3groups == 0
		local h_b_number = r(N)
		count if `var' == `l' & cmd_number_3groups == 1
		local fcmd_b_number = r(N)
		count if `var' == `l' & cmd_number_3groups == 2
		local mltmbd_b_number = r(N)
		* resurvey2
		count if `var'2 == `l' & cmd_number_3groups == 0
		local h_r_number = r(N)
		count if `var'2 == `l' & cmd_number_3groups == 1
		local fcmd_r_number = r(N)
		count if `var'2 == `l' & cmd_number_3groups == 2
		local mltmbd_r_number = r(N)
		mlogit `var' i.cmd_number_3groups i.region_code i.is_female age_at_study_date, baseoutcome(`min')
			margins cmd_number_3groups, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
			mat a = r(table)
			loc healthy_baseline = string(`h_b_number') + " (" + string(a[1,1]*100, "%9.1f") + ")"
			loc fcmd_baseline = string(`fcmd_b_number') + " (" + string(a[1,2]*100, "%9.1f") + ")"
			loc mltmbd_baseline = string(`mltmbd_b_number') + " (" + string(a[1,3]*100, "%9.1f") + ")"
		mlogit `var'2 i.cmd_number_3groups i.region_code i.is_female age_at_study_date2, baseoutcome(`min')
			margins cmd_number_3groups, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
			mat a = r(table)
			loc healthy_resurvey2 = string(`h_r_number') + " (" + string(a[1,1]*100, "%9.1f") + ")"
			loc fcmd_resurvey2 = string(`fcmd_r_number') + " (" + string(a[1,2]*100, "%9.1f") + ")"
			loc mltmbd_resurvey2 = string(`mltmbd_r_number') + " (" + string(a[1,3]*100, "%9.1f") + ")"

		** total
		count if `var' == `l'
		local b_number = r(N)
		local total_baseline = string(`b_number') + " (" + string(`b_number'/_N*100, "%9.1f") + ")"
		count if `var'2 == `l'
		local r_number = r(N)
		local total_resurvey2 = string(`r_number') + " (" + string(`r_number'/_N*100, "%9.1f") + ")"
		post `stats' ("`variable'") ("`category'") ("`healthy_baseline'") ("`healthy_resurvey2'") ("`fcmd_baseline'") ("`fcmd_resurvey2'") ("`mltmbd_baseline'") ///
			("`mltmbd_resurvey2'") ("`total_baseline'") ("`total_resurvey2'")
	}

}

postclose `stats'
use `table3', clear
cap mkdir "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE"
export excel using "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE\matched compare cmd_number_3groups.xlsx", firstrow(var) replace

