********************************************************
*** Objective: Generate table for updated dsiease interaction 
*** Created date: 2021-08-31
*** Version: 2
*** Updating log:


**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                        Part 5: Disease interaction                             *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************

****** Log file
log using "2.Result/Log/Updated Disease by disease interaction V2.log",replace

local ep "`1'"

tempname stats 
tempfile table_dxd
save `table_dxd', emptyok
postfile `stats' str50 name double (dm_chd_coef dm_chd_p dm_stroke_coef dm_stroke_p chd_stroke_coef chd_stroke_p total_p) using `table_dxd', replace

**********************************************************************************************************
*************                                                                                *************
*************                        Part 5.1: Overall  analysis                             *************
*************                                                                                *************
**********************************************************************************************************

****** Global macro
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

use "1.Data/project2_updated_`ep'.dta", clear

****** Interaction
*** Original model
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store original


*** Three interaction simultaneously
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.diabetes_diag_updated#i.chd_diag_updated i.diabetes_diag_updated#i.stroke_diag_updated i.chd_diag_updated#i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store three_interact
mat a = r(table)
* extracting coef
local dm_chd_coef = a[1,10]
local dm_chd_p = a[4,10]
local dm_stroke_coef = a[1,14]
local dm_stroke_p = a[4,14]
local chd_stroke_coef = a[1,18]
local chd_stroke_p = a[4,18]

* total
lrtest original three_interact
local total_p = r(p)

post `stats' ("Overall") (`dm_chd_coef') (`dm_chd_p') (`dm_stroke_coef') (`dm_stroke_p') (`chd_stroke_coef') (`chd_stroke_p') (`total_p')

**********************************************************************************************************
*************                                                                                *************
*************                        Part 5.2: Subgroup analysis                             *************
*************                                                                                *************
**********************************************************************************************************


**********************************************************************************************************
****************************       Part 5.2.1 Baseline interaction    ************************************
**********************************************************************************************************
****** Global macro
global adjusted_model_is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_region_is_urban i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_education_3groups i.is_female i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_age_3groups i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

*** Local macro
local varlist is_female region_is_urban education_2groups age_2groups

foreach var of local varlist{

	di "#########################  Updated `var'"
	local name: variable label `var'
	levelsof `var',local(level) clean 
	
	post `stats' ("`name'") (.) (.) (.) (.) (.) (.) (.)

	foreach l of local level{
	
	di "******************  Updated `var' == `l'"
	use "1.Data/project2_updated_`ep'.dta", clear
	local name: label(`var') `l'	
	keep if `var' == `l'
	****** Interaction
	*** Original model
	stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store original
	
	*** Three interaction simultaneously
	stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.diabetes_diag_updated#i.chd_diag_updated i.diabetes_diag_updated#i.stroke_diag_updated i.chd_diag_updated#i.stroke_diag_updated ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store three_interact
	mat a = r(table)
	* extracting coef
	local dm_chd_coef = a[1,10]
	local dm_chd_p = a[4,10]
	local dm_stroke_coef = a[1,14]
	local dm_stroke_p = a[4,14]
	local chd_stroke_coef = a[1,18]
	local chd_stroke_p = a[4,18]

	* total
	lrtest original three_interact
	local total_p = r(p)

	post `stats' ("`name'") (`dm_chd_coef') (`dm_chd_p') (`dm_stroke_coef') (`dm_stroke_p') (`chd_stroke_coef') (`chd_stroke_p') (`total_p')
	
	}
}


postclose `stats'

use `table_dxd', clear
cap mkdir "2.Result/HRs//$S_DATE" 
export delim using "2.Result/HRs//$S_DATE//Updated DXD interaction `ep' $S_DATE.csv", replace
	

log close

