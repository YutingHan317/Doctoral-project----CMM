**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                        Project 3: Risk of mortality                            *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
/// Last updated on Mar 27 2021

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                     Project 3.1: Basline characteristics                       *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
log using "D:\HanYT\2021-02 CMM to death\2. Result\Log\baseline characteristics $S_DATE.log", replace
use "D:\HanYT\2021-02 CMM to death\1. Data\project2.dta", clear
tab1 has_diabetes chd_diag stroke_or_tia_diag d_number_base combin_base 
* Age
tabstat age_at_study_date, s(mean median) by(d_number_base)

* Categorical
local covariates is_female highest_education household_income marital_status mltmbd_fh_3groups ///
	smoking_5groups alcohol_7groups diet_5score bmi_4groups WC_3groups PA_5groups

foreach c of local covariates{

	tab `c' d_number_base, row

}

log close



**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                       Project 3.2: baseline morbidity                          *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
log using "D:\HanYT\2021-02 CMM to death\2. Result\Log\baseline to death $S_DATE.log", replace

*** Import dataset
use "D:\HanYT\2021-02 CMM to death\1. Data\project2.dta", clear

*** Set global macro
global endpoints
*** Set survival data
stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001 == 1)

/* ???????????? */

*** Model 1:??????
stcox i.d_number_base i.is_female, strata(age_strata region_code) cformat(%9.2f)

*** Model 2: Model 1 + ?????????????????????????????????????????????????????????????????????
/* stcox i.d_number_base i.is_female i.highest_education i.household_income i.marital_status ///
	i.diabetes_fh_2groups i.stoke_fh_2groups i.chd_fh_2groups, strata(age_strata region_code) cformat(%9.2f) */
stcox i.d_number_base i.is_female i.highest_education i.household_income i.marital_status ///
	i.mltmbd_fh_3groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 3 :Model 2?????????????????????????????????????????????bmi???WC???????????????
stcox i.d_number_base i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 4: Model 3?????????????????????????????????
stcox i.d_number_base i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 5: Model 3??????????????????????????????????????????
stcox i.d_number_base i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)

*** Model 6: Model 3???????????????????????????????????????????????????
stcox i.d_number_base i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)


/* ??????????????????????????? */
*** Model 1:??????
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag, strata(age_strata region_code is_female) cformat(%9.2f)
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag i.is_female, strata(age_strata region_code) cformat(%9.2f)

*** Model 2: Model 1 + ?????????????????????????????????????????????????????????????????????
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag i.is_female i.highest_education i.household_income i.marital_status ///
	i.diabetes_fh_2groups i.stoke_fh_2groups i.chd_fh_2groups, strata(age_strata region_code) cformat(%9.2f)
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag i.is_female i.highest_education i.household_income i.marital_status ///
	i.mltmbd_fh_3groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 3 :Model 2?????????????????????????????????????????????bmi???WC???????????????
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 4: Model 3?????????????????????????????????
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 5: Model 3??????????????????????????????????????????
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)

*** Model 6: Model 3???????????????????????????????????????????????????
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)
estimates store original

*** ????????????
stcox i.has_diabetes##i.chd_diag i.stroke_or_tia_diag i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)
estimates store dm_chd
lrtest original dm_chd

stcox i.has_diabetes##i.stroke_or_tia_diag i.chd_diag i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)
estimates store dm_stroke
lrtest original dm_stroke

stcox i.has_diabetes i.chd_diag##i.stroke_or_tia_diag i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)
estimates store chd_stroke
lrtest original chd_stroke


/* ???????????? */
*** Model 1:??????
stcox i.combin_base, strata(age_strata region_code is_female) cformat(%9.2f)
stcox i.combin_base i.is_female, strata(age_strata region_code) cformat(%9.2f)

*** Model 2: Model 1 + ?????????????????????????????????????????????????????????????????????
stcox i.combin_base i.is_female i.highest_education i.household_income i.marital_status ///
	i.diabetes_fh_2groups i.stoke_fh_2groups i.chd_fh_2groups, strata(age_strata region_code) cformat(%9.2f)
stcox i.combin_base i.is_female i.highest_education i.household_income i.marital_status ///
	i.mltmbd_fh_3groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 3 :Model 2?????????????????????????????????????????????bmi???WC???????????????
stcox i.combin_base i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 4: Model 3?????????????????????????????????
stcox i.combin_base i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 5: Model 3??????????????????????????????????????????
stcox i.combin_base i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)

*** Model 6: Model 3???????????????????????????????????????????????????
stcox i.combin_base i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)

log close


**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                       Project 3.2: updated morbidity                           *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
log using "D:\HanYT\2021-02 CMM to death\2. Result\Log\timevaring to death $S_DATE.log", replace
************** c_ep0003 ?????????????????????????????????c_ep0010  ????????????any stroke I60-61 & I63-64; c_ep0101 Diabetes E10-E14
************** Dataset has been stset
use "D:\HanYT\2021-02 CMM to death\1. Data\project2_updated_du_ep0001.dta", clear

/* Regression using updated varibale */
/* Number of diseases */
*** Model 1: sex
stcox i.d_number_updated, strata(age_strata region_code is_female) cformat(%9.2f)
stcox i.d_number_updated i.is_female, strata(age_strata region_code) cformat(%9.2f)

*** Model 2: Model 1 + ?????????????????????????????????????????????????????????????????????
stcox i.d_number_updated i.is_female i.highest_education i.household_income i.marital_status ///
	i.diabetes_fh_2groups i.stoke_fh_2groups i.chd_fh_2groups, strata(age_strata region_code) cformat(%9.2f)
stcox i.d_number_updated i.is_female i.highest_education i.household_income i.marital_status ///
	i.mltmbd_fh_3groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 3 :Model 2?????????????????????????????????????????????bmi???WC???????????????
stcox i.d_number_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 4: Model 3?????????????????????????????????
stcox i.d_number_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 5: Model 3??????????????????????????????????????????
stcox i.d_number_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)

*** Model 6: Model 3???????????????????????????????????????????????????
stcox i.d_number_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)


/* single CMD */
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated, strata(age_strata region_code is_female)
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.is_female, strata(age_strata region_code) cformat(%9.2f)

*** Model 2: Model 1 + ?????????????????????????????????????????????????????????????????????
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.is_female i.highest_education i.household_income i.marital_status ///
	i.diabetes_fh_2groups i.stoke_fh_2groups i.chd_fh_2groups, strata(age_strata region_code)
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.is_female i.highest_education i.household_income i.marital_status ///
	i.mltmbd_fh_3groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 3 :Model 2?????????????????????????????????????????????bmi???WC???????????????
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 4: Model 3?????????????????????????????????
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 5: Model 3??????????????????????????????????????????
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)

*** Model 6: Model 3???????????????????????????????????????????????????
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)
estimates store original_updated


*** ????????????
stcox i.diabetes_diag_updated##i.chd_diag_updated i.stroke_diag_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)
estimates store dm_chd_updated
lrtest original_updated dm_chd_updated

stcox i.diabetes_diag_updated##i.stroke_diag_updated i.chd_diag_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)
estimates store dm_stroke_diag_updated
lrtest original_updated dm_stroke_diag_updated

stcox i.diabetes_diag_updated i.chd_diag_updated##i.stroke_diag_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)
estimates store chd_stroke_diag_updated
lrtest original_updated chd_stroke_diag_updated


/* Combination */
*** Model 1:??????
stcox i.combin_updated, strata(age_strata region_code is_female) cformat(%9.2f)
stcox i.combin_updated i.is_female, strata(age_strata region_code) cformat(%9.2f)

*** Model 2: Model 1 + ?????????????????????????????????????????????????????????????????????
stcox i.combin_updated i.is_female i.highest_education i.household_income i.marital_status ///
	i.diabetes_fh_2groups i.stoke_fh_2groups i.chd_fh_2groups, strata(age_strata region_code) cformat(%9.2f)
stcox i.combin_updated i.is_female i.highest_education i.household_income i.marital_status ///
	i.mltmbd_fh_3groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 3 :Model 2?????????????????????????????????????????????bmi???WC???????????????
stcox i.combin_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 4: Model 3?????????????????????????????????
stcox i.combin_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups, strata(age_strata region_code) cformat(%9.2f)

*** Model 5: Model 3??????????????????????????????????????????
stcox i.combin_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)

*** Model 6: Model 3???????????????????????????????????????????????????
stcox i.combin_updated i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups ///
	i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.PA_5groups ///
	i.taking_aspirin_2groups i.taking_ace_i_2groups i.taking_beta_blocker_2groups i.taking_statins_2groups i.taking_diuretics_2groups ///
	i.taking_ca_antagonist_2groups i.taking_chlor_metaformin_2groups i.taking_insulin_2groups ///
	i.has_hypertension i.kidney_dis_diag, strata(age_strata region_code) cformat(%9.2f)




log close







list studyid age_at_study_date study_date c_ep0003 c_ep0003_date chd_diag _t0 _t


list studyid c_ep0003_date c_ep0010_date c_ep0101_date du_ep0001_date chd_diag age_chd_diag age_stroke_diag age_diabetes_diag chd_diag_updated stroke_diag_updated diabetes_diag_updated _t0 _t in 1/100
