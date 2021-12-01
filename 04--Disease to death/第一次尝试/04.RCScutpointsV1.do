********************************************
* Created date: 2021/08/24
* Objective: find a set of cutpoint 
* Updating log
* 2021/08/09: Try RCS
* 2021/08/24；find endpoint-specific cutpoints
* 2021/09/15: try to find another better cutpoint by keep the lastest observation for each participants (first 5 knots）

***************************************************************************************************************
*****************************                cutoff choice              ***************************************
***************************************************************************************************************
local ep "`1'"
//// 经过试验，du_ep0001都是五分组比较好

****** Setting global macro
global adjusted_model i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

log using "2.Result/Log/RCS cutpoint `ep' $S_DATE.log",replace
****** Loading data
use "1.Data/project2_updated_`ep'_duration3.dta", clear

****** Diabetes
* 4knots: 0 1.257 6.65 15
mkspline rcs_d_dm_up1 = duration_diabetes_updated, cubic nknots(5) knots(0 1.257 6.65 15 30) displayknots

* 5knots: 0 0.956 7.540 11.770 21.545
mkspline rcs_d_dm_up2 = duration_diabetes_updated, cubic nknots(5) knots(0 0.956 7.540 11.770 21.545) displayknots

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
mkspline rcs_d_chd_up1 = duration_chd_updated, cubic nknots(5) knots(0 0.861 5.666 19 30) displayknots

* 5knots: 0 0.394 3.595 8.572 24.463
mkspline rcs_d_chd_up2 = duration_chd_updated, cubic nknots(5) knots(0 0.394 3.595 8.572 24.463) displayknots

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
mkspline rcs_d_stroke_up1 = duration_stroke_updated, cubic nknots(5) knots(0 0.601 3.858 12.5 30) displayknots

* 5knots: 0 0.181 2.754 6.229 16.212
mkspline rcs_d_stroke_up2 = duration_stroke_updated, cubic nknots(5) knots(0 0.181 2.754 6.229 16.212) displayknots

* 5knots: 0 0.28 2.46 5.63 16
mkspline rcs_d_stroke_up3 = duration_stroke_updated, cubic nknots(5) knots(0 0.28 2.46 5.63 16) displayknots

stcox rcs_d_stroke_up1* i.duration_diabetes_groups i.duration_chd_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model7
stcox rcs_d_stroke_up2* i.duration_diabetes_groups i.duration_chd_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model8
stcox rcs_d_stroke_up3* i.duration_diabetes_groups i.duration_chd_groups ${adjusted_model}, strata(age_strata region_code) cformat(%9.2f)
	estimates store model9
estimates stats model7 model8 model9


log close



