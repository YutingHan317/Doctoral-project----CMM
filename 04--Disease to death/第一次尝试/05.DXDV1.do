********************************************************
*** Created date: 2021-08-18
*** Version: 1.1
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
log using "2.Result/Log/Disease by disease interaction V1.log",replace

**********************************************************************************************************
*************                                                                                *************
*************                        Part 5.1: Overall  analysis                             *************
*************                                                                                *************
**********************************************************************************************************

**********************************************************************************************************
****************************       Part 5.1 Baseline interaction      ************************************
**********************************************************************************************************
****** Global macro
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

use "1.Data/project2_updated_du_ep0001.dta", clear

****** Interaction
*** Original model
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store original

*** DM X CHD
stcox i.has_diabetes##i.chd_diag i.stroke_or_tia_diag ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store dm_chd
lrtest original dm_chd

*** DM X Stroke
stcox i.has_diabetes##i.stroke_or_tia_diag i.chd_diag ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store dm_stroke
lrtest original dm_stroke

*** CHD X Stroke
stcox i.has_diabetes i.chd_diag##i.stroke_or_tia_diag ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store chd_stroke
lrtest original chd_stroke

*** Three interaction simultaneously
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag i.has_diabetes#i.chd_diag i.has_diabetes#i.stroke_or_tia_diag i.chd_diag#i.stroke_or_tia_diag ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store three_interact
lrtest original three_interact

*** three-way interaction
stcox i.has_diabetes##i.chd_diag##i.stroke_or_tia_diag ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store dm_chd_stroke
lrtest original dm_chd_stroke

**********************************************************************************************************
****************************       Part 5.2 Updated  interaction      ************************************
**********************************************************************************************************
****** Global macro
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

use "1.Data/project2_updated_du_ep0001.dta", clear

****** Interaction
*** Original model
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store original

*** DM X CHD
stcox i.diabetes_diag_updated##i.chd_diag_updated i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store dm_chd
lrtest original dm_chd

*** DM X Stroke
stcox i.diabetes_diag_updated##i.stroke_diag_updated i.chd_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store dm_stroke
lrtest original dm_stroke

*** CHD X Stroke
stcox i.diabetes_diag_updated i.chd_diag_updated##i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store chd_stroke
lrtest original chd_stroke

*** Three interaction simultaneously
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.diabetes_diag_updated#i.chd_diag_updated i.diabetes_diag_updated#i.stroke_diag_updated i.chd_diag_updated#i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store three_interact
lrtest original three_interact

*** three-way interaction
stcox i.diabetes_diag_updated##i.chd_diag_updated##i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store dm_chd_stroke
lrtest original dm_chd_stroke

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
local varlist is_female region_is_urban education_3groups age_3groups

foreach var of local varlist{

	di "#########################  Baseline `var'"
	levelsof `var',local(level) clean 
	
	foreach l of local level{
	di "******************  Baseline `var' == `l'"
	
	use "1.Data/project2_updated_du_ep0001.dta", clear
	
	keep if `var' == `l'
	****** Interaction
	*** Original model
	stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store original
	
	*** DM X CHD
	stcox i.has_diabetes##i.chd_diag i.stroke_or_tia_diag ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store dm_chd
	lrtest original dm_chd
	
	*** DM X Stroke
	stcox i.has_diabetes##i.stroke_or_tia_diag i.chd_diag ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store dm_stroke
	lrtest original dm_stroke
	
	*** CHD X Stroke
	stcox i.has_diabetes i.chd_diag##i.stroke_or_tia_diag ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store chd_stroke
	lrtest original chd_stroke
	
	*** Three interaction simultaneously
	stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag i.has_diabetes#i.chd_diag i.has_diabetes#i.stroke_or_tia_diag i.chd_diag#i.stroke_or_tia_diag ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store three_interact
	lrtest original three_interact
	
	*** three-way interaction
	stcox i.has_diabetes##i.chd_diag##i.stroke_or_tia_diag ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store dm_chd_stroke
	lrtest original dm_chd_stroke
	
	**********************************************************************************************************
	****************************       Part 5.2 Updated  interaction      ************************************
	**********************************************************************************************************
	****** Global macro
	global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
	
	di "******************  Updated `var' == `l'"
	use "1.Data/project2_updated_du_ep0001.dta", clear
	
	keep if `var' == `l'
	****** Interaction
	*** Original model
	stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store original
	
	*** DM X CHD
	stcox i.diabetes_diag_updated##i.chd_diag_updated i.stroke_diag_updated ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store dm_chd
	lrtest original dm_chd
	
	*** DM X Stroke
	stcox i.diabetes_diag_updated##i.stroke_diag_updated i.chd_diag_updated ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store dm_stroke
	lrtest original dm_stroke
	
	*** CHD X Stroke
	stcox i.diabetes_diag_updated i.chd_diag_updated##i.stroke_diag_updated ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store chd_stroke
	lrtest original chd_stroke
	
	*** Three interaction simultaneously
	stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.diabetes_diag_updated#i.chd_diag_updated i.diabetes_diag_updated#i.stroke_diag_updated i.chd_diag_updated#i.stroke_diag_updated ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store three_interact
	lrtest original three_interact
	
	*** three-way interaction
	stcox i.diabetes_diag_updated##i.chd_diag_updated##i.stroke_diag_updated ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
	estimates store dm_chd_stroke
	lrtest original dm_chd_stroke
	
	}
}

log close




****** Global macro
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

use "1.Data/project2_updated_du_ep0001.dta", clear

****** Interaction
*** Original model
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store original

*** Three interaction simultaneously
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.diabetes_diag_updated#i.chd_diag_updated i.diabetes_diag_updated#i.stroke_diag_updated i.chd_diag_updated#i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store three_interact
lrtest original three_interact


*** Subgroup

bysort is_female:stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.diabetes_diag_updated#i.chd_diag_updated i.diabetes_diag_updated#i.stroke_diag_updated i.chd_diag_updated#i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)



global adjusted_model_is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_region_is_urban i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_education_3groups i.is_female i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_age_3groups i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

use "1.Data/project2_updated_du_ep0001.dta", clear

*** Original model
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
estimates store original

*** Three interaction simultaneously
bysort education_2groups: stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.diabetes_diag_updated#i.chd_diag_updated i.diabetes_diag_updated#i.stroke_diag_updated i.chd_diag_updated#i.stroke_diag_updated ${adjusted_model_education_3groups}, strata(age_strata region_code) cformat(%9.2f)
bysort education_3groups: stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.diabetes_diag_updated#i.chd_diag_updated i.diabetes_diag_updated#i.stroke_diag_updated i.chd_diag_updated#i.stroke_diag_updated ${adjusted_model_education_3groups}, strata(age_strata region_code) cformat(%9.2f)

bysort age_2groups: stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.diabetes_diag_updated#i.chd_diag_updated i.diabetes_diag_updated#i.stroke_diag_updated i.chd_diag_updated#i.stroke_diag_updated ${adjusted_model_age_2groups}, strata(age_strata region_code) cformat(%9.2f)
