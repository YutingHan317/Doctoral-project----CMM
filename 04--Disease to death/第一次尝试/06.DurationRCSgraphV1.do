********************************************
* Created date: 2021-07-15
* Version: 1.1
* Updating log
* 2021/08/09: Try RCS
* 2021/08/23: change 4 knots into 5knots

***************************************************************************************************************
***************************************************************************************************************
*****************************                                       *******************************************
*****************************               RCS plot                *******************************************
*****************************                                       *******************************************
***************************************************************************************************************
***************************************************************************************************************

***************************************************************************************************************
*****************************                Updated                *******************************************
***************************************************************************************************************
/* 设置宏进行linux 循环 */
loc ep "`1'"


****** Setting global macro
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

****** Loading data
use "1.Data/project2_updated_`ep'_duration3.dta", clear

****** Check whether duration variables have too mant unique values; >2500 for diabetes
*codebook duration_diabetes_updated duration_chd_updated duration_stroke_updated

******Find the knots
* 1.257 6.65 15
centile duration_diabetes_updated if duration_diabetes_updated != 0, centile(10 50 90)

* 0.861 5.666 19
centile duration_chd_updated if duration_chd_updated != 0, centile(10 50 90)

* 0.601 3.858 12.5
centile duration_stroke_updated if duration_stroke_updated != 0, centile(10 50 90)


****** Generating rcs variables
* Diabetes: 5knots: 0 0.619 4.5 9 18.5
mkspline rcs_d_dm_up = duration_diabetes_updated, cubic nknots(5) knots(0 0.619 4.5 9 18.5) displayknots
mat knots_dm = r(knots)
* CHD 4knots: 0.416 3.51 8.56 24.5
mkspline rcs_d_chd_up = duration_chd_updated, cubic nknots(5) knots(0 0.416 3.51 8.56 24.5) displayknots
mat knots_chd = r(knots)
* Stroke 4knots:  0 0.28 2.46 5.63 16
mkspline rcs_d_stroke_up = duration_stroke_updated, cubic nknots(5) knots(0 0.28 2.46 5.63 16) displayknots
mat knots_stroke = r(knots)

*************************************************************************************************************
/* Uni analysis */
****** Diabetes
stcox rcs_d_dm_up*  ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
*** Graph
capture drop drt_dm hr_dm lb_dm ub_dm
sum duration_diabetes_updated
local min = r(min)
local max = r(max)
local interval = (r(max)-r(min))/2400
xbrcspline rcs_d_dm_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_dm) gen(drt_dm hr_dm lb_dm ub_dm)
twoway (line hr_dm drt_dm, sort) (line lb_dm ub_dm drt_dm, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of Diabetes) ytitle(HR) yline(1) title("`ep' uni")
graph export "2.Result/Graph/Updated_uni_`ep'_diabetes.png",replace

****** CHD
stcox rcs_d_chd_up* ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
*** Graph
capture drop drt_chd hr_chd lb_chd ub_chd
sum duration_chd_updated
local min = r(min)
local max = r(max)
local interval = (r(max)-r(min))/2400
xbrcspline rcs_d_chd_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_chd) gen(drt_chd hr_chd lb_chd ub_chd)
twoway (line hr_chd drt_chd, sort) (line lb_chd ub_chd drt_chd, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of CHD) ytitle(HR) yline(1) title("`ep' uni")
graph export "2.Result/Graph/Updated_uni_`ep'_chd.png",replace

****** Stroke
stcox rcs_d_stroke_up* ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
*** Graph
capture drop drt_stroke hr_stroke lb_stroke ub_stroke
sum duration_stroke_updated
local min = r(min)
local max = r(max)
local interval = (r(max)-r(min))/2400
xbrcspline rcs_d_stroke_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_stroke) gen(drt_stroke hr_stroke lb_stroke ub_stroke)
twoway (line hr_stroke drt_stroke, sort) (line lb_stroke ub_stroke drt_stroke, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of Stroke) ytitle(HR) yline(1) title("`ep' uni")
graph export "2.Result/Graph/Updated_uni_`ep'_stroke.png",replace


*************************************************************************************************************
/* Adjusted simultaneously analysis */
****** Diabetes
stcox rcs_d_dm_up*  i.duration_chd_groups i.duration_stroke_groups  ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
*** Graph
capture drop drt_dm hr_dm lb_dm ub_dm
sum duration_diabetes_updated
local min = r(min)
local max = r(max)
local interval = (r(max)-r(min))/2400
xbrcspline rcs_d_dm_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_dm) gen(drt_dm hr_dm lb_dm ub_dm)
twoway (line hr_dm drt_dm, sort) (line lb_dm ub_dm drt_dm, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of Diabetes) ytitle(HR) yline(1) title("`ep' adjusted")
graph export "2.Result/Graph/Updated_adj_`ep'_diabetes.png",replace

****** CHD
stcox rcs_d_chd_up* i.duration_diabetes_groups i.duration_stroke_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
*** Graph
capture drop drt_chd hr_chd lb_chd ub_chd
sum duration_chd_updated
local min = r(min)
local max = r(max)
local interval = (r(max)-r(min))/2400
xbrcspline rcs_d_chd_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_chd) gen(drt_chd hr_chd lb_chd ub_chd)
twoway (line hr_chd drt_chd, sort) (line lb_chd ub_chd drt_chd, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of CHD) ytitle(HR) yline(1) title("`ep' adjusted")
graph export "2.Result/Graph/Updated_adj_`ep'_chd.png",replace

****** Stroke
stcox rcs_d_stroke_up* i.duration_diabetes_groups i.duration_chd_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
*** Graph
capture drop drt_stroke hr_stroke lb_stroke ub_stroke
sum duration_stroke_updated
local min = r(min)
local max = r(max)
local interval = (r(max)-r(min))/2400
xbrcspline rcs_d_stroke_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_stroke) gen(drt_stroke hr_stroke lb_stroke ub_stroke)
twoway (line hr_stroke drt_stroke, sort) (line lb_stroke ub_stroke drt_stroke, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of Stroke) ytitle(HR) yline(1) title("`ep' adjusted")
graph export "2.Result/Graph/Updated_adj_`ep'_stroke.png",replace
