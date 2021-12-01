********************************************
* Created date: 2021-07-15
* Updating log
* 2020/08/09: Try RCS

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                          Part 4: Duration of diseases                          *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
/* 目前的调整逻辑 */
* 模型1： 性别、教育程度、收入、婚姻状态、共病家族史、吸烟五分组、饮酒七分组、饮食五分组、BMI4分组、WC三分组、体力活动五分组
* 模型2： 模型1+高血压、慢性肾病、风湿性心脏病
* 模型3： 模型2基础上调整药物（降压药（分开还是合并、阿司匹林、降脂药（statin）

/* 暴露 */
* duration_diabetes drt_chd drt_stroke
* drt_chd_groups drt_stroke_groups duration_diabetes_groups drt_chd_7g drt_stroke_7g duration_diabetes_7g
* duration_diabetes_updated drt_chd_updated drt_stroke_updated
* drt_chd_updated_groups drt_stroke_updated_groups duration_diabetes_updated_groups drt_chd_updated_7g drt_stroke_updated_7g duration_diabetes_updated_7g
/* 已经考虑的问题 */
* 生活方式按照多分组调整和两分组调整结果基本一致

/* 变量特征 */
* cd "D:/HanYT/2021-02_CMMtodeath"

***************************************************************************************************************
*****************************                                       *******************************************
*****************************           Exploring analyses          *******************************************
*****************************                                       *******************************************
***************************************************************************************************************
/* 设置宏进行linux 循环 */
loc ep "`1'"

log using "2.Result/Log/test_duration_`ep'_0715.log", replace


***************************************************************************************************************
di "********************************** baseline CMDs duration"
use "1.Data/project2_updated_`ep'_duration.dta", clear
/* 设置全局宏 */
*** Covariates for adjustment
global adjusted_model1 i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups
global adjusted_model2 $adjusted_model1 i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

stset `ep'_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(`ep' == 1)

****** Many groups
*** seperately model duration of three diseases
stcox b99.duration_diabetes_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 

*** 同时放
stcox b99.duration_diabetes_groups b99.drt_chd_groups b99.drt_stroke_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_groups b99.drt_chd_groups b99.drt_stroke_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 

****** 7 groups
stcox b99.duration_diabetes_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 

*** 同时放
stcox b99.duration_diabetes_7g b99.drt_chd_7g b99.drt_stroke_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_7g b99.drt_chd_7g b99.drt_stroke_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 


***************************************************************************************************************
di "**********************************  Updated CMDs duration"
****** Many groups
stcox b99.duration_diabetes_updated_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_updated_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_updated_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_updated_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_updated_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_updated_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 

*** 同时放
stcox b99.duration_diabetes_updated_groups b99.drt_chd_updated_groups b99.drt_stroke_updated_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_updated_groups b99.drt_chd_updated_groups b99.drt_stroke_updated_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.3f)

****** 7 groups
stcox b99.duration_diabetes_updated_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_updated_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_updated_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_updated_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_updated_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_updated_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 

*** 同时放
stcox b99.duration_diabetes_updated_7g b99.drt_chd_updated_7g b99.drt_stroke_updated_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_updated_7g b99.drt_chd_updated_7g b99.drt_stroke_updated_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.3f)


***************************************************************************************************************
di "**********************************  incident CMDs duration"
keep if d_number_base == 0
****** Many groups
stcox b99.duration_diabetes_updated_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_updated_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_updated_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_updated_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_updated_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_updated_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 

*** 同时放
stcox b99.duration_diabetes_updated_groups b99.drt_chd_updated_groups b99.drt_stroke_updated_groups $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_updated_groups b99.drt_chd_updated_groups b99.drt_stroke_updated_groups $adjusted_model2, strata(age_strata region_code) cformat(%9.3f)

****** 7 groups
stcox b99.duration_diabetes_updated_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_updated_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_updated_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_chd_updated_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_updated_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.drt_stroke_updated_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.2f) 

*** 同时放
stcox b99.duration_diabetes_updated_7g b99.drt_chd_updated_7g b99.drt_stroke_updated_7g $adjusted_model1, strata(age_strata region_code) cformat(%9.2f) 
stcox b99.duration_diabetes_updated_7g b99.drt_chd_updated_7g b99.drt_stroke_updated_7g $adjusted_model2, strata(age_strata region_code) cformat(%9.3f)

log close

***************************************************************************************************************
*****************************                                       *******************************************
*****************************               RCS plot                *******************************************
*****************************                                       *******************************************
***************************************************************************************************************

***************************************************************************************************************
*****************************               Baseline                *******************************************
***************************************************************************************************************

****** Setting global macro
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

****** Loading data
use "1.Data/project2.dta", clear
stset du_ep0001_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(du_ep0001 == 1)

****** Check whether duration variables have too mant unique values; >2500 for diabetes
codebook duration_diabetes duration_chd duration_stroke

****** Generating rcs variables
* Diabetes: 4 knots is not applicable; 0  0.74  10.33
mkspline rcs_d_dm = duration_diabetes, cubic nknots(3) displayknots
mat knots_dm = r(knots)
* CHD: 0.8 4 9.42 23.81
mkspline rcs_d_chd = duration_chd, cubic nknots(4) displayknots
mat knots_chd = r(knots)
* Stroke: 0.54 2.78 6.1 16.74
mkspline rcs_d_stroke = duration_stroke, cubic nknots(4) displayknots
mat knots_stroke = r(knots)

****** Diabetes
stcox rcs_d_dm* b99.duration_chd_groups b99.duration_stroke_groups  ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
levelsof duration_diabetes,local(level_dm)
*** Graph
capture drop drt_dm hr_dm lb_dm ub_dm
xbrcspline rcs_d_dm, values(`level_dm') ref(0) eform matknots(knots_dm) gen(drt_dm hr_dm lb_dm ub_dm)
twoway (line hr_dm drt_dm, sort) (line lb_dm ub_dm drt_dm, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of diabetes) ytitle(HR) yline(1)
graph export "2.Result/Graph/Baseline_rcs_diabetes.png",replace

****** CHD
stcox rcs_d_chd* b99.duration_diabetes_groups b99.duration_stroke_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
levelsof duration_chd,local(level_chd)
*** Graph
capture drop drt_chd hr_chd lb_chd ub_chd
sum duration_chd
local min = r(min)
local max = r(max)
local interval = (r(max)-r(min))/2400
xbrcspline rcs_d_chd, values(`min'(`interval')`max') ref(0) eform matknots(knots_chd) gen(drt_chd hr_chd lb_chd ub_chd)
twoway (line hr_chd drt_chd, sort) (line lb_chd ub_chd drt_chd, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of diabetes) ytitle(HR) yline(1)
graph export "2.Result/Graph/Baseline_rcs_chd.png",replace

****** Stroke
stcox rcs_d_stroke* b99.duration_diabetes_groups b99.duration_chd_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
levelsof duration_stroke,local(level_stroke)
*** Graph
capture drop drt_stroke hr_stroke lb_stroke ub_stroke
xbrcspline rcs_d_stroke, values(`level_stroke') ref(0) eform matknots(knots_stroke) gen(drt_stroke hr_stroke lb_stroke ub_stroke)
twoway (line hr_stroke drt_stroke, sort) (line lb_stroke ub_stroke drt_stroke, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of diabetes) ytitle(HR) yline(1)
graph export "2.Result/Graph/Baseline_rcs_stroke.png",replace


***************************************************************************************************************
*****************************                Updated                *******************************************
***************************************************************************************************************

****** Setting global macro
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

****** Loading data
use "1.Data/project2_updated_du_ep0001_duration3.dta", clear

****** Check whether duration variables have too mant unique values; >2500 for diabetes
*codebook duration_diabetes_updated duration_chd_updated duration_stroke_updated

******Find the knots
* 1.257 6.65 15; 0.619 4.5 9 18.5
centile duration_diabetes_updated if duration_diabetes_updated != 0, centile(10 50 90)
centile duration_diabetes_updated if duration_diabetes_updated != 0, centile(5 35 65 95)

* 0.861 5.666 19; 0.416 3.51 8.56 24.5
centile duration_chd_updated if duration_chd_updated != 0, centile(10 50 90)
centile duration_chd_updated if duration_chd_updated != 0, centile(5 35 65 95)

* 0.601 3.858 12.5; 0.28 2.46 5.63 16
centile duration_stroke_updated if duration_stroke_updated != 0, centile(10 50 90)
centile duration_stroke_updated if duration_stroke_updated != 0, centile(5 35 65 95)


****** Generating rcs variables
* Diabetes: 4knots: 0.14 4.05 8.5 18(old)
mkspline rcs_d_dm_up = duration_diabetes_updated, cubic nknots(4) knots(0 1.257 6.65 15) displayknots
mat knots_dm = r(knots)
* CHD 4knots: 0 3.05 8.15 24(old)
mkspline rcs_d_chd_up = duration_chd_updated, cubic nknots(4) knots(0 0.861 5.666 19) displayknots
mat knots_chd = r(knots)
* Stroke 4knots: 0 1.99 5.18 15.5(old)
mkspline rcs_d_stroke_up = duration_stroke_updated, cubic nknots(4) knots(0 0.601 3.858 12.5) displayknots
mat knots_stroke = r(knots)


****** Diabetes
stcox rcs_d_dm_up*  i.duration_chd_groups i.duration_stroke_groups  ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
*** Graph
capture drop drt_dm hr_dm lb_dm ub_dm
sum duration_diabetes_updated
local min = r(min)
local max = r(max)
local interval = (r(max)-r(min))/2400
xbrcspline rcs_d_dm_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_dm) gen(drt_dm hr_dm lb_dm ub_dm)
twoway (line hr_dm drt_dm, sort) (line lb_dm ub_dm drt_dm, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of diabetes) ytitle(HR) yline(1)
graph export "2.Result/Graph/Updated_rcs_diabetes.png",replace

****** CHD
stcox rcs_d_chd_up* i.duration_diabetes_groups i.duration_stroke_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
*** Graph
capture drop drt_chd hr_chd lb_chd ub_chd
sum duration_chd_updated
local min = r(min)
local max = r(max)
local interval = (r(max)-r(min))/2400
xbrcspline rcs_d_chd_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_chd) gen(drt_chd hr_chd lb_chd ub_chd)
twoway (line hr_chd drt_chd, sort) (line lb_chd ub_chd drt_chd, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of diabetes) ytitle(HR) yline(1)
graph export "2.Result/Graph/Updated_rcs_chd.png",replace

****** Stroke
stcox rcs_d_stroke_up* i.duration_diabetes_groups i.duration_chd_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
*** Graph
capture drop drt_stroke hr_stroke lb_stroke ub_stroke
sum duration_stroke_updated
local min = r(min)
local max = r(max)
local interval = (r(max)-r(min))/2400
xbrcspline rcs_d_stroke_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_stroke) gen(drt_stroke hr_stroke lb_stroke ub_stroke)
twoway (line hr_stroke drt_stroke, sort) (line lb_stroke ub_stroke drt_stroke, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of diabetes) ytitle(HR) yline(1)
graph export "2.Result/Graph/Updated_rcs_stroke.png",replace


***************************************************************************************************************
*****************************                cutoff choice              ***************************************
***************************************************************************************************************
local ep "`1'"
//// 经过试验，du_ep0001都是五分组比较好

****** Setting global macro
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

****** Loading data
use "1.Data/project2_updated_du_ep0001_duration3.dta", clear

****** Diabetes
* 4knots: 0 1.257 6.65 15
mkspline rcs_d_dm_up1 = duration_diabetes_updated, cubic nknots(4) knots(0 1.257 6.65 15) displayknots

* 5knots: 0 1.257 6.65 15 30
mkspline rcs_d_dm_up2 = duration_diabetes_updated, cubic nknots(5) knots(0 1.257 6.65 15 30) displayknots

* 5knots: 0 0.619 4.5 9 18.5
mkspline rcs_d_dm_up3 = duration_diabetes_updated, cubic nknots(5) knots(0 0.619 4.5 9 18.5) displayknots


stcox rcs_d_dm_up1* i.duration_chd_groups i.duration_stroke_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model1
stcox rcs_d_dm_up2* i.duration_chd_groups i.duration_stroke_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model2
stcox rcs_d_dm_up3* i.duration_chd_groups i.duration_stroke_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model3
estimates stats model1 model2 model3

****** CHD
* 4knots: 0 0.861 5.666 19
mkspline rcs_d_chd_up1 = duration_chd_updated, cubic nknots(4) knots(0 0.861 5.666 19) displayknots

* 5knots: 0 0.861 5.666 19 30
mkspline rcs_d_chd_up2 = duration_chd_updated, cubic nknots(5) knots(0 0.861 5.666 19 30) displayknots

* 5knots: 0.416 3.51 8.56 24.5
mkspline rcs_d_chd_up3 = duration_chd_updated, cubic nknots(5) knots(0 0.416 3.51 8.56 24.5) displayknots

stcox rcs_d_chd_up1* i.duration_diabetes_groups i.duration_stroke_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model4
stcox rcs_d_chd_up2* i.duration_diabetes_groups i.duration_stroke_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model5
stcox rcs_d_chd_up3* i.duration_diabetes_groups i.duration_stroke_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model6
estimates stats model4 model5 model6


****** Stroke
* 4knots: 0 0.601 3.858 12.5
mkspline rcs_d_stroke_up1 = duration_stroke_updated, cubic nknots(4) knots(0 0.601 3.858 12.5) displayknots

* 5knots: 0 0.601 3.858 12.5 30
mkspline rcs_d_stroke_up2 = duration_stroke_updated, cubic nknots(5) knots(0 0.601 3.858 12.5 30) displayknots

* 5knots: 0 0.28 2.46 5.63 16
mkspline rcs_d_stroke_up3 = duration_stroke_updated, cubic nknots(5) knots(0 0.28 2.46 5.63 16) displayknots

stcox rcs_d_stroke_up1* i.duration_diabetes_groups i.duration_chd_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model7
stcox rcs_d_stroke_up2* i.duration_diabetes_groups i.duration_chd_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model8
stcox rcs_d_stroke_up3* i.duration_diabetes_groups i.duration_chd_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model9
estimates stats model7 model8 model9

