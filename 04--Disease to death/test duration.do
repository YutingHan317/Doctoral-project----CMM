global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

****** Loading data
use "1.Data/project2_updated_du_ep0001_duration3.dta", clear

****** Check whether duration variables have too mant unique values; >2500 for diabetes
*codebook duration_diabetes_updated duration_chd_updated duration_stroke_updated

******Find the knots
* 1.257 6.65 15
centile duration_diabetes_updated if duration_diabetes_updated != 0, centile(10 50 90 95)



bysort studyid: gen n = _n
bysort studyid: keep if n == _N







* 0.861 5.666 19
centile duration_chd_updated if duration_chd_updated != 0, centile(10 50 90 95)

* 0.601 3.858 12.5
centile duration_stroke_updated if duration_stroke_updated != 0, centile(10 50 90 95)
