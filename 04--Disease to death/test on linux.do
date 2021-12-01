
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

use "1.Data/project2_updated_du_ep0001.dta", clear
stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001== 1)
stset du_ep0014_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0014== 1)
stset du_ep0002_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0002== 1)

stcox i.combin_base ${adjusted_model} , strata(age_strata region_code)
stcox i.combin_updated ${adjusted_model} , strata(age_strata region_code)


list studyid diabetes_diag_updated chd_diag_updated stroke_diag_updated c_ep0101 c_ep0101_date c_ep0003 c_ep0003_date c_ep0010 c_ep0010_date du_ep0001 du_ep0001_date du_ep0002 du_ep0002_date _d _t0 _t if studyid == 1000033








use "1.Data/project2.dta", clear
	/* 对数据进行切割 */
	*** Set survival data
	stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001 == 1)

	*** 根据缺血性心脏病切割随访时间
	stsplit ihd_split, after(time=c_ep0003_date) at(0)

	*** 根据脑卒中切割随访时间
	stsplit stroke_split, after(time=c_ep0010_date) at(0)

	*** 根据糖尿病切割随访时间
	stsplit diabetes_split, after(time=c_ep0101_date) at(0)



use "1.Data/project2_updated_du_ep0001_duration.dta", clear
stcox i.duration_chd_7g i.duration_stroke_7g ${adjusted_model} , strata(age_strata region_code)
mat a = r(table)
matlist a



stcox b99.duration_chd_7g b99.duration_stroke_7g b99.duration_diabetes_7g ${adjusted_model} , strata(age_strata region_code)
mat a = r(table)
matlist a


use "1.Data/project2_updated_du_ep0001_duration.dta", clear
stcox b99.duration_chd_updated_groups b99.duration_stroke_updated_groups b99.duration_diabetes_updated_groups ${adjusted_model} , strata(age_strata region_code)
stcox b99.duration_chd_updated_7g b99.duration_stroke_updated_7g b99.duration_diabetes_updated_7g ${adjusted_model} , strata(age_strata region_code)


use "1.Data/project2_updated_du_ep0001_duration2.dta", clear
stcox b99.duration_chd_updated_groups b99.duration_stroke_updated_groups b99.duration_diabetes_updated_groups ${adjusted_model} , strata(age_strata region_code)
stcox b99.duration_chd_updated_7g b99.duration_stroke_updated_7g b99.duration_diabetes_updated_7g ${adjusted_model} , strata(age_strata region_code)



*********************************************************************************************************************
****************************                        Duration                  ***************************************
*********************************************************************************************************************
use "1.Data/project2.dta", clear

gen duration_chd1 = age_at_study_date - chd_diag_age
	count if duration_chd1 <= 0
	list duration_chd1 if duration_chd1 <= 0

gen duration_diabetes1 = age_at_study_date - diabetes_diag_age
	count if duration_diabetes1 <= 0
	list duration_diabetes1 if duration_chd1 <= 0

gen duration_stroke1 = age_at_study_date - stroke_or_tia_diag_age
	count if duration_stroke1 <= 0
	list duration_stroke1 if duration_stroke1 <= 0






use "1.Data/project2_updated_du_ep0001.dta", clear

stsplit aage, every(0.5)

* 53
gen duration_chd_updated = _t0 - chd_diag_age if chd_diag == 1
replace duration_chd_updated = _t0 - (c_ep0003_date - dob_anon)/365.25 if chd_diag == 0 & c_ep0003==1 & ((c_ep0003_date - dob_anon)/365.25) <= _t0
count if  duration_chd_updated<=0
codebook studyid if duration_chd_updated <=0

* 35
gen duration_stroke_updated =  _t0 - stroke_or_tia_diag_age if stroke_or_tia_diag == 1
replace duration_stroke_updated = _t0 - (c_ep0010_date - dob_anon)/365.25 if stroke_or_tia_diag == 0 & c_ep0010==1 & ((c_ep0010_date - dob_anon)/365.25) <= _t0
count if  duration_stroke_updated<=0
codebook studyid if duration_stroke_updated <=0



sum duration_chd_updated if duration_chd_updated <=0, detail
sum duration_chd_updated if duration_chd_updated <=0, detail


*********************************************************************************************************************
****************************                      Prediction                  ***************************************
*********************************************************************************************************************
adopath + /public/home/hanyuting/ado

/* Baseline */
use "1.Data/project2.dta", clear
stset du_ep0001_date, id(studyid) origin(time study_date) scale(365.25) failure(du_ep0001 == 1)

gen double age_squar = age_at_study_date^2
stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag i.is_female i.highest_education, strata(age_strata region_code)
	estimate store basemodel1
	predict hr1,hr
	gen double invhr1 = 1/hr1

stcox b99.duration_diabetes_7g b99.duration_chd_7g b99.duration_stroke_7g i.is_female i.highest_education,strata(age_strata region_code)
	estimate store basemodel2
	predict hr2,hr
	gen double invhr2 = 1/hr2
**********************
estimates stats basemodel1 basemodel2
gen censind = 1 - _d if _st == 1
somersd _t invhr1 invhr2 if _st == 1, cenind(censind) tdist transf(c)


/* Updated */
use "1.Data/project2_updated_du_ep0001_duration3.dta", clear 

gen double age_squar = age_at_study_date^2
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated age_at_study_date age_squar i.is_female i.highest_education i.region_code
	estimate store upmodel1
	predict hr1,xb
	gen double invhr1 = 1/hr1
stcox b99.duration_chd_updated_7g b99.duration_stroke_updated_7g b99.duration_diabetes_updated_7g  age_at_study_date age_squar i.is_female i.highest_education i.region_code
	estimate store upmodel2
	predict hr2,xb
	gen double invhr2 = 1/hr2

**********************
estimates stats upmodel1 upmodel2
gen censind = 1 - _d if _st == 1
somersd _t invhr1 invhr2 if _st == 1, cenind(censind) tdist transf(c)




use "1.Data/project2_updated_du_ep0001_duration3.dta", clear 

global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stcox i.duration_diabetes_updated_groups i.duration_chd_updated_groups i.duration_stroke_updated_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 



stcox b99.duration_chd_updated_7g b99.duration_stroke_updated_7g b99.duration_diabetes_updated_7g  age_at_study_date age_squar i.is_female i.highest_education i.region_code
	estimate store upmodel2
	predict hr2,xb
	gen double invhr2 = 1/hr2


*********************************************************************************************************************
****************************                      Prediction                  ***************************************
*********************************************************************************************************************






****************************************************************************************************************************
****************************                      duration subgroup                  ***************************************
****************************************************************************************************************************

global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

global adjusted_model_education_2groups i.is_female i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag


use "1.Data/project2_updated_du_ep0001_duration3.dta", clear

stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model}, strata(age_strata region_code)

bysort is_female: stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model}, strata(age_strata region_code)

bysort region_is_urban: stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model}, strata(age_strata region_code)

bysort education_2groups: stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model}, strata(age_strata region_code)

bysort age_2groups: stcox i.duration_chd_8g i.duration_stroke_8g i.duration_diabetes_8g ${adjusted_model}, strata(age_strata region_code)




****************************************************************************************************************************
****************************                   diff stroke definition                ***************************************
****************************************************************************************************************************
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

*** different exposure
use "1.Data/project2_updated_du_ep0001.dta", clear

stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model} , strata(age_strata region_code)


*** duration
use "1.Data/project2_updated_du_ep0001_duration3.dta", clear
stcox i.dduration_chd_updated_8g i.duration_stroke_updated_8g i.duration_diabetes_updated_8g ${adjusted_model} , strata(age_strata region_code)


****************************************************************************************************************************
****************************                   不同患病时间基线患病占比                ***************************************
****************************************************************************************************************************
use "1.Data/project2_updated_du_ep0001_duration3.dta", clear


tab duration_diabetes_updated_8g has_diabetes if du_ep0001==1, row
tab duration_stroke_updated_8g stroke_or_tia_diag if du_ep0001==1, row
tab duration_chd_updated_8g chd_diabetes if du_ep0001==1, row



****************************************************************************************************************************
****************************                   额外调整基线肿瘤和COPD                ***************************************
****************************************************************************************************************************

global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
use "1.Data/project2_updated_du_ep0001.dta", clear

stcox i.has_diabetes i.chd_diag i.stroke_or_tia_diag ${adjusted_model} i.cancer_diag i.has_copd, strata(age_strata region_code)
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model} i.cancer_diag i.has_copd, strata(age_strata region_code)


****************************************************************************************************************************
****************************             Cases of death by duration groups           ***************************************
****************************************************************************************************************************
use "1.Data/project2_updated_du_ep0001_duration3.dta", clear

bysort studyid: gen n  = 1 if _n == _N

tab1 chd_diag stroke_or_tia_diag has_diabetes if n == 1
tab1 chd_diag_updated MI_diag_updated stroke_diag_updated IS_diag_updated HS_diag_updated diabetes_diag_updated    if n == 1
tab1 chd_diag_updated MI_diag_updated stroke_diag_updated IS_diag_updated HS_diag_updated diabetes_diag_updated    if n == 1 & d_number_base == 0


*** Updated
tab duration_chd_updated_8g du_ep0001

tab duration_MI_updated_8g du_ep0001

tab duration_stroke_updated_8g du_ep0001

tab duration_IS_updated_8g du_ep0001

tab duration_HS_updated_8g du_ep0001

*** Incident
tab duration_chd_updated_8g du_ep0001 if d_number_base == 0

tab duration_MI_updated_8g du_ep0001 if d_number_base == 0

tab duration_stroke_updated_8g du_ep0001 if d_number_base == 0

tab duration_IS_updated_8g du_ep0001 if d_number_base == 0

tab duration_HS_updated_8g du_ep0001 if d_number_base == 0

*** 看性别看一下不同患病时长
* 基线
tab duration_diabetes_8g is_female if n == 1, col

tab duration_diabetes_updated_8g is_female if n == 1, col
tab duration_diabetes_updated_8g is_female if d_number_base == 0 & n == 1, col



****************************************************************************************************************************
****************************                  Analysis of FCMD and CMM               ***************************************
****************************************************************************************************************************
log using "2.Result/Log/Fcmd & cmm $S_DATE.log", replace
*use "1.Data/project2_updated_du_ep0001.dta", clear
use "1.Data/project2_updated_du_ep0001_nochg.dta", clear

global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

/* Baseline */
* Dich exposure
stcox i.fcmd_base ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.cmm_base ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.fcmd_base i.cmm_base ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.fcmd_base##i.cmm_base ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)

* Duration 
/* stcox i.duration_fcmd_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.duration_cmm_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.duration_fcmd_8g i.duration_cmm_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
 */

/* Updated */
stcox i.fcmd_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.fcmd_updated i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.fcmd_updated##i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
* 2021/11/21
stcox i.fcmd_updated i.fcmd_updated#i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)

/* Incident */
keep if d_number_base == 0
stcox i.fcmd_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.fcmd_updated i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.fcmd_updated##i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)

**** d_number2_base d_number2_updated
stcox i.d_number2_base ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.d_number2_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.d_number2_updated ${adjusted_model} if d_number_base == 0 , strata(age_strata region_code) cformat(%9.2f)


********************* Duration
*use "1.Data/project2_updated_du_ep0001_duration3.dta", clear
use "1.Data/project2_updated_du_ep0001_duration3_nochg.dta", clear
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stcox i.duration_fcmd_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.duration_cmm_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.duration_fcmd_updated_8g i.duration_cmm_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
stcox i.duration_fcmd_updated_8g i.duration_fcmd_updated_8g#i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)


lincom 0.duration_fcmd_updated_8g + 0.duration_fcmd_updated_8g#1.cmm_updated,eform
lincom 1.duration_fcmd_updated_8g + 1.duration_fcmd_updated_8g#1.cmm_updated,eform
lincom 2.duration_fcmd_updated_8g + 2.duration_fcmd_updated_8g#1.cmm_updated,eform
lincom 3.duration_fcmd_updated_8g + 3.duration_fcmd_updated_8g#1.cmm_updated,eform
lincom 4.duration_fcmd_updated_8g + 4.duration_fcmd_updated_8g#1.cmm_updated,eform
lincom 5.duration_fcmd_updated_8g + 5.duration_fcmd_updated_8g#1.cmm_updated,eform
lincom 6.duration_fcmd_updated_8g + 6.duration_fcmd_updated_8g#1.cmm_updated,eform
lincom 7.duration_fcmd_updated_8g + 7.duration_fcmd_updated_8g#1.cmm_updated,eform
lincom 8.duration_fcmd_updated_8g + 8.duration_fcmd_updated_8g#1.cmm_updated,eform

log close


****************************************************************************************************************************
****************************               No change in 30d before death             ***************************************
****************************************************************************************************************************
use "1.Data/project2_updated_du_ep0001_duration3_nochg.dta", clear 

global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stcox i.duration_diabetes_updated_8g i.duration_chd_updated_8g i.duration_stroke_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 



****************************************************************************************************************************
****************************               No change in 30d before death             ***************************************
****************************************************************************************************************************
/* Duration without any adjustment */
use "1.Data/project2_updated_du_ep0001_duration3_nochg.dta", clear
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stcox i.duration_diabetes_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 
stcox i.duration_chd_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 
stcox i.duration_stroke_updated_8g ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 



****************************************************************************************************************************
****************************        Diabetes duration acccording to other CMD        ***************************************
****************************************************************************************************************************
use "1.Data/project2_updated_du_ep0001_duration3_nochg.dta", clear
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

/* Diabetes */
stcox i.duration_diabetes_updated_8g i.duration_diabetes_updated_8g#i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 

lincom 0.duration_diabetes_updated_8g + 0.duration_diabetes_updated_8g#1.cmm_updated,eform
lincom 1.duration_diabetes_updated_8g + 1.duration_diabetes_updated_8g#1.cmm_updated,eform
lincom 2.duration_diabetes_updated_8g + 2.duration_diabetes_updated_8g#1.cmm_updated,eform
lincom 3.duration_diabetes_updated_8g + 3.duration_diabetes_updated_8g#1.cmm_updated,eform
lincom 4.duration_diabetes_updated_8g + 4.duration_diabetes_updated_8g#1.cmm_updated,eform
lincom 5.duration_diabetes_updated_8g + 5.duration_diabetes_updated_8g#1.cmm_updated,eform
lincom 6.duration_diabetes_updated_8g + 6.duration_diabetes_updated_8g#1.cmm_updated,eform
lincom 7.duration_diabetes_updated_8g + 7.duration_diabetes_updated_8g#1.cmm_updated,eform
lincom 8.duration_diabetes_updated_8g + 8.duration_diabetes_updated_8g#1.cmm_updated,eform


/* CHD */
stcox i.duration_chd_updated_8g i.duration_chd_updated_8g#i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 

lincom 0.duration_chd_updated_8g + 0.duration_chd_updated_8g#1.cmm_updated,eform
lincom 1.duration_chd_updated_8g + 1.duration_chd_updated_8g#1.cmm_updated,eform
lincom 2.duration_chd_updated_8g + 2.duration_chd_updated_8g#1.cmm_updated,eform
lincom 3.duration_chd_updated_8g + 3.duration_chd_updated_8g#1.cmm_updated,eform
lincom 4.duration_chd_updated_8g + 4.duration_chd_updated_8g#1.cmm_updated,eform
lincom 5.duration_chd_updated_8g + 5.duration_chd_updated_8g#1.cmm_updated,eform
lincom 6.duration_chd_updated_8g + 6.duration_chd_updated_8g#1.cmm_updated,eform
lincom 7.duration_chd_updated_8g + 7.duration_chd_updated_8g#1.cmm_updated,eform
lincom 8.duration_chd_updated_8g + 8.duration_chd_updated_8g#1.cmm_updated,eform



/* ST */
stcox i.duration_stroke_updated_8g i.duration_stroke_updated_8g#i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 

lincom 0.duration_stroke_updated_8g + 0.duration_stroke_updated_8g#1.cmm_updated,eform
lincom 1.duration_stroke_updated_8g + 1.duration_stroke_updated_8g#1.cmm_updated,eform
lincom 2.duration_stroke_updated_8g + 2.duration_stroke_updated_8g#1.cmm_updated,eform
lincom 3.duration_stroke_updated_8g + 3.duration_stroke_updated_8g#1.cmm_updated,eform
lincom 4.duration_stroke_updated_8g + 4.duration_stroke_updated_8g#1.cmm_updated,eform
lincom 5.duration_stroke_updated_8g + 5.duration_stroke_updated_8g#1.cmm_updated,eform
lincom 6.duration_stroke_updated_8g + 6.duration_stroke_updated_8g#1.cmm_updated,eform
lincom 7.duration_stroke_updated_8g + 7.duration_stroke_updated_8g#1.cmm_updated,eform
lincom 8.duration_stroke_updated_8g + 8.duration_stroke_updated_8g#1.cmm_updated,eform

* ======================================
* Using adjustment instead of interaction
stcox i.duration_diabetes_updated_8g i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 
stcox i.duration_chd_updated_8g i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 
stcox i.duration_stroke_updated_8g i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 

* Using both adjustment 
stcox i.duration_diabetes_updated_8g##i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 
stcox i.duration_chd_updated_8g##i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 
stcox i.duration_stroke_updated_8g##i.cmm_updated ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f) 






/* 查看死于某一种CMD的人数 */
global eplist_status c_ep0003 c_ep0068 c_ep0008 c_ep0009  c_ep0010 c_ep0011 c_ep0047 c_ep0048 c_ep0301 c_ep0301 c_ep0302  du_ep0001 du_ep0002 du_ep0014 du_ep0032
global eplist_date c_ep0003_date c_ep0068_date c_ep0008_date c_ep0009_date c_ep0010_date c_ep0011_date c_ep0047_date c_ep0048_date c_ep0301_date c_ep0302_date du_ep0001_date du_ep0002_date du_ep0014_date du_ep0032_date
use "1.Data/baseline.dta", clear

* 收集所有变量
merge 1:1 studyid using "1.Data/endpoints.dta", keep(match) nogen keepusing($eplist_status $eplist_date)

***日期型变量转换
local varlist dob_anon study_date censoring_date $eplist_date
foreach var of local varlist{

	gen `var'1 = substr(`var',1,10)
	gen `var'2 = date(`var'1,"YMD",2050)
	drop `var' `var'1
	rename `var'2 `var'
	format `var' %td
	
}

*** Generate longitudinal DM variable
gen c_ep0101 = 1 if c_ep0047 == 1 | c_ep0048 == 1 | c_ep0301 == 1 | c_ep0302 == 1
	replace c_ep0101 = 0 if c_ep0047 == 0 & c_ep0048 == 0 & c_ep0301 == 0 & c_ep0302 == 0
egen c_ep0101_date = rowmin(c_ep0047_date c_ep0048_date c_ep0301_date c_ep0302_date)

*** Generate longitudinal death from other causes
gen du_ep9999 = 1 if du_ep0001 == 1 & du_ep0002 == 0 & du_ep0014 == 0 & du_ep0032 == 0
	replace du_ep9999 = 0 if ((du_ep0002 == 1 | du_ep0014 == 1 | du_ep0032 == 1) & du_ep0001 == 1) | (du_ep0001 == 0)
egen du_ep9999_date = rowmin(du_ep0001_date du_ep0002_date du_ep0014_date du_ep0032_date)

format c_ep0101_date du_ep9999_date %td

misstable summarize $eplist_status $eplist_date c_ep0101 c_ep0101_date du_ep9999 du_ep9999_date

local eplist c_ep0003 c_ep0008 c_ep0009 c_ep0010 c_ep0011 c_ep0068 c_ep0101
foreach ep of local eplist{

	di "`ep'"
	count if `ep' == 1 & du_ep0001 == 1 & `ep'_date == du_ep0001_date

}





****************************************************************************************************************************
****************************                分辨发病率差别           ***************************************
****************************************************************************************************************************


use "1.Data/project2_updated_du_ep0001_duration3_nochg.dta", clear

bysort studyid: gen n  = 1 if _n == _N

tab diabetes_diag_updated cmm_updated if n ==1

sum _d if diabetes_diag_updated == 0 & cmm_updated == 0, detail
total _d if diabetes_diag_updated == 0 & cmm_updated == 0


stcox i.duration_diabetes_updated_8g i.duration_diabetes_updated_8g#fcmd_updated

gen fcmd_notdm_updated = 1 if chd_diag_updated == 1 | stroke_diag_updated == 1
	replace fcmd_notdm_updated = 0 if fcmd_notdm_updated == . 




use "1.Data/project2_updated_du_ep0001_duration3_nochg.dta", clear

global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stcox i.duration_diabetes_updated_8g i.duration_diabetes_updated_8g#fcmd_notdm_updated ${adjusted_model}, strata(age_strata region_code)

lincom 0.duration_diabetes_updated_8g + 0.duration_diabetes_updated_8g#1.fcmd_notdm_updated,eform
lincom 1.duration_diabetes_updated_8g + 1.duration_diabetes_updated_8g#1.fcmd_notdm_updated,eform
lincom 2.duration_diabetes_updated_8g + 2.duration_diabetes_updated_8g#1.fcmd_notdm_updated,eform
lincom 3.duration_diabetes_updated_8g + 3.duration_diabetes_updated_8g#1.fcmd_notdm_updated,eform
lincom 4.duration_diabetes_updated_8g + 4.duration_diabetes_updated_8g#1.fcmd_notdm_updated,eform
lincom 5.duration_diabetes_updated_8g + 5.duration_diabetes_updated_8g#1.fcmd_notdm_updated,eform
lincom 6.duration_diabetes_updated_8g + 6.duration_diabetes_updated_8g#1.fcmd_notdm_updated,eform
lincom 7.duration_diabetes_updated_8g + 7.duration_diabetes_updated_8g#1.fcmd_notdm_updated,eform
lincom 8.duration_diabetes_updated_8g + 8.duration_diabetes_updated_8g#1.fcmd_notdm_updated,eform


use "1.Data/project2_updated_du_ep0001_duration3_nochg.dta", clear

global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stcox i.duration_chd_updated_8g i.duration_chd_updated_8g#fcmd_notchd_updated ${adjusted_model}, strata(age_strata region_code)

lincom 0.duration_chd_updated_8g + 0.duration_chd_updated_8g#1.fcmd_notchd_updated,eform
lincom 1.duration_chd_updated_8g + 1.duration_chd_updated_8g#1.fcmd_notchd_updated,eform
lincom 2.duration_chd_updated_8g + 2.duration_chd_updated_8g#1.fcmd_notchd_updated,eform
lincom 3.duration_chd_updated_8g + 3.duration_chd_updated_8g#1.fcmd_notchd_updated,eform
lincom 4.duration_chd_updated_8g + 4.duration_chd_updated_8g#1.fcmd_notchd_updated,eform
lincom 5.duration_chd_updated_8g + 5.duration_chd_updated_8g#1.fcmd_notchd_updated,eform
lincom 6.duration_chd_updated_8g + 6.duration_chd_updated_8g#1.fcmd_notchd_updated,eform
lincom 7.duration_chd_updated_8g + 7.duration_chd_updated_8g#1.fcmd_notchd_updated,eform
lincom 8.duration_chd_updated_8g + 8.duration_chd_updated_8g#1.fcmd_notchd_updated,eform


use "1.Data/project2_updated_du_ep0001_duration3_nochg.dta", clear

global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.diabetes_fh_2groups i.chd_fh_2groups i.stroke_fh_2groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stcox i.duration_stroke_updated_8g i.duration_stroke_updated_8g#fcmd_notstroke_updated ${adjusted_model}, strata(age_strata region_code)

lincom 0.duration_stroke_updated_8g + 0.duration_stroke_updated_8g#1.fcmd_notstroke_updated,eform
lincom 1.duration_stroke_updated_8g + 1.duration_stroke_updated_8g#1.fcmd_notstroke_updated,eform
lincom 2.duration_stroke_updated_8g + 2.duration_stroke_updated_8g#1.fcmd_notstroke_updated,eform
lincom 3.duration_stroke_updated_8g + 3.duration_stroke_updated_8g#1.fcmd_notstroke_updated,eform
lincom 4.duration_stroke_updated_8g + 4.duration_stroke_updated_8g#1.fcmd_notstroke_updated,eform
lincom 5.duration_stroke_updated_8g + 5.duration_stroke_updated_8g#1.fcmd_notstroke_updated,eform
lincom 6.duration_stroke_updated_8g + 6.duration_stroke_updated_8g#1.fcmd_notstroke_updated,eform
lincom 7.duration_stroke_updated_8g + 7.duration_stroke_updated_8g#1.fcmd_notstroke_updated,eform
lincom 8.duration_stroke_updated_8g + 8.duration_stroke_updated_8g#1.fcmd_notstroke_updated,eform
