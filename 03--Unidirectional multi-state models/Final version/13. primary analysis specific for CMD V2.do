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
postfile `stats' str50 variable str50 category str50 healthy str50 fcmd str50 mltmbd str50 total using `table3', replace

***No. of participants
qui{

	local variable "No. of participants"
	local category ""
	count if cmd_number_3groups == 0
	local healthy = r(N)
	count if cmd_number_3groups == 1
	local fcmd = r(N)
	count if cmd_number_3groups == 2
	local mltmbd = r(N)
	local total = _N
	post `stats' ("") ("") ("`healthy'") ("`fcmd'") ("`mltmbd'") ("`total'") 

}


***Age at baseline
local variable "Age at baseline"
local category ""
*** Healhthy
sum age_at_study_date if cmd_number_3groups == 0
local healthy = string(r(mean), "%9.1f") + " (" + string(r(sd), "%9.1f") + ")"
*** only on cmd
sum age_at_study_date if cmd_number_3groups == 1
local fcmd = string(r(mean), "%9.1f") + " (" + string(r(sd), "%9.1f") + ")"
*** multimorbidity
sum age_at_study_date if cmd_number_3groups == 2
local mltmbd = string(r(mean), "%9.1f") + " (" + string(r(sd), "%9.1f") + ")"
*** total
sum age_at_study_date
local total = string(r(mean), "%9.1f") + " (" + string(r(sd), "%9.1f") + ")"
post `stats' ("`variable'") ("") ("`healthy'") ("`fcmd'") ("`mltmbd'") ("`total'")

***Age at resurvey2
local variable "Age at resurvey2"
local category ""
*** Healhthy
sum age_at_study_date2 if cmd_number_3groups == 0
local healthy = string(r(mean), "%9.1f") + " (" + string(r(sd), "%9.1f") + ")"
*** only on cmd
sum age_at_study_date2 if cmd_number_3groups == 1
local fcmd = string(r(mean), "%9.1f") + " (" + string(r(sd), "%9.1f") + ")"
*** multimorbidity
sum age_at_study_date2 if cmd_number_3groups == 2
local mltmbd = string(r(mean), "%9.1f") + " (" + string(r(sd), "%9.1f") + ")"
*** total
sum age_at_study_date2
local total = string(r(mean), "%9.1f") + " (" + string(r(sd), "%9.1f") + ")"
post `stats' ("`variable'") ("") ("`healthy'") ("`fcmd'") ("`mltmbd'") ("`total'")


******Continuous variables(adjusted for age, region and sex)
*** calculate the difference between baseline and resurvey
gen diff_met = met2 - met
loc variable "Physical activity"
loc category ""
**baseline
sum diff_met if cmd_number_3groups == 0
loc healthy_sd = r(sd)
sum diff_met if cmd_number_3groups == 1
loc fcmd_sd = r(sd)
sum diff_met if cmd_number_3groups == 2
loc mltmbd_sd = r(sd)
anova diff_met cmd_number_3groups c.age_at_study_date region_code is_female
margins cmd_number_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)
mat a = r(table)
loc healthy = string(a[1,1], "%9.1f") + " (" + string(`healthy_sd', "%9.1f") + ")"
loc fcmd = string(a[1,2], "%9.1f") + " (" + string(`fcmd_sd', "%9.1f") + ")"
loc mltmbd = string(a[1,3], "%9.1f") + " (" + string(`mltmbd_sd', "%9.1f") + ")"
sum diff_met
local total = string(r(mean), "%9.1f") + " (" + string(r(sd), "%9.1f") + ")"
post `stats' ("`variable'") ("") ("`healthy'") ("`fcmd'") ("`mltmbd'") ("`total'")

******Categorical variables
local varlist healthy_smoking_change healthy_alcohol_change healthy_diet_change healthy_obesity_change
foreach var of local varlist{
	
	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") 
	sum `var'
	loc min = r(min)
	levelsof `var', local(level) clean
	foreach l of local level{
		
		loc variable ""
		loc category:	label(`var') `l'
		mlogit `var' i.cmd_number_3groups i.region_code i.is_female age_at_study_date, baseoutcome(`min')
			margins cmd_number_3groups, cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
			mat a = r(table)
			loc healthy = string(a[1,1]*100, "%9.1f") + " (" + string(a[5,1]*100, "%9.1f") + "-" + string(a[6,1]*100, "%9.1f") + ")"
			loc fcmd = string(a[1,2]*100, "%9.1f") + " (" + string(a[5,2]*100, "%9.1f") + "-" + string(a[6,2]*100, "%9.1f") + ")"
			loc mltmbd = string(a[1,3]*100, "%9.1f") + " (" + string(a[5,3]*100, "%9.1f") + "-" + string(a[6,3]*100, "%9.1f") + ")" 
		** total
		count if `var' == `l'
		local propor = r(N)/_N
		cii proportion r(N) `propor'
		local total = string(r(proportion)*100, "%9.1f") + " (" + string(r(lb)*100, "%9.1f") + "-" + string(r(ub)*100, "%9.1f") + ")"

		post `stats' ("`variable'") ("`category'") ("`healthy'") ("`fcmd'") ("`mltmbd'") ("`total'")
	}

}

postclose `stats'
use `table3', clear
cap mkdir "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE"
export excel using "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE\matched compare cmd_number_3groups V2.xlsx", firstrow(var) replace

