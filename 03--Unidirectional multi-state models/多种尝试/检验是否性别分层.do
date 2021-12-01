use "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta", clear

stset mltmbd_inc_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(mltmbd_inc == 1)

stcox i.healthy_score_5groups i.highest_education i.household_income ///
	i.occupation i.marital_status, strata(age_strata region_code is_female)

stcox i.healthy_score_5groups i.is_female i.highest_education i.household_income ///
	i.occupation i.marital_status, strata(age_strata region_code)
