********************************************
* Created date: 2021-09-17
* Version: 1.1
* Updating log
* 2021/08/09: Try RCS for subgroups analyses
* Updating log:



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
global adjusted_model_is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_region_is_urban i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_education_2groups i.is_female i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag
global adjusted_model_age_2groups i.is_female i.highest_education i.household_income i.marital_status i.mltmbd_fh_3groups i.smoking_5groups i.alcohol_7groups i.diet_5score i.bmi_4groups i.WC_3groups i.PA_5groups i.has_hypertension i.kidney_dis_diag i.rheum_heart_dis_diag

local varlist is_female region_is_urban education_2groups age_2groups

foreach var of local varlist{

	use "1.Data/project2_updated_`ep'_duration3.dta", clear
	levelsof `var', local(level) clean

	foreach l of local level{

		****** Loading data
		use "1.Data/project2_updated_`ep'_duration3.dta", clear
		keep if `var' == `l'

		****** Check whether duration variables have too mant unique values; >2500 for diabetes
		*codebook duration_diabetes_updated duration_chd_updated duration_stroke_updated

		******Find the knots
		* 1.257 6.65 15
		*centile duration_diabetes_updated if duration_diabetes_updated != 0, centile(10 50 90)

		* 0.861 5.666 19
		* centile duration_chd_updated if duration_chd_updated != 0, centile(10 50 90)

		* 0.601 3.858 12.5
		* centile duration_stroke_updated if duration_stroke_updated != 0, centile(10 50 90)


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
		/* Adjusted simultaneously analysis */
		****** Diabetes
		stcox rcs_d_dm_up*  i.duration_chd_groups i.duration_stroke_groups  ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
		*** Graph
		capture drop drt_dm hr_dm lb_dm ub_dm
		sum duration_diabetes_updated
		local min = r(min)
		local max = r(max)
		local interval = (r(max)-r(min))/2400
		xbrcspline rcs_d_dm_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_dm) gen(drt_dm hr_dm lb_dm ub_dm)
		twoway (line hr_dm drt_dm, sort) (line lb_dm ub_dm drt_dm, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of Diabetes) ytitle(HR) yline(1) title("`ep' adjusted")
		capture mkdir "2.Result/Graph//$S_DATE"
		graph export "2.Result/Graph//$S_DATE/Updated_adj_`ep'_diabetes `var'==`l'.png",replace

		****** CHD
		stcox rcs_d_chd_up* i.duration_diabetes_groups i.duration_stroke_groups ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
		*** Graph
		capture drop drt_chd hr_chd lb_chd ub_chd
		sum duration_chd_updated
		local min = r(min)
		local max = r(max)
		local interval = (r(max)-r(min))/2400
		xbrcspline rcs_d_chd_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_chd) gen(drt_chd hr_chd lb_chd ub_chd)
		twoway (line hr_chd drt_chd, sort) (line lb_chd ub_chd drt_chd, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of CHD) ytitle(HR) yline(1) title("`ep' adjusted")
		graph export "2.Result/Graph//$S_DATE/Updated_adj_`ep'_chd `var'==`l'.png",replace

		****** Stroke
		stcox rcs_d_stroke_up* i.duration_diabetes_groups i.duration_chd_groups ${adjusted_model_`var'}, strata(age_strata region_code) cformat(%9.2f)
		*** Graph
		capture drop drt_stroke hr_stroke lb_stroke ub_stroke
		sum duration_stroke_updated
		local min = r(min)
		local max = r(max)
		local interval = (r(max)-r(min))/2400
		xbrcspline rcs_d_stroke_up, values(`min'(`interval')`max') ref(0) eform matknots(knots_stroke) gen(drt_stroke hr_stroke lb_stroke ub_stroke)
		twoway (line hr_stroke drt_stroke, sort) (line lb_stroke ub_stroke drt_stroke, sort lc(black black) lp(- -)), legend(off) yscale(log) xtitle(Duration of Stroke) ytitle(HR) yline(1) title("`ep' adjusted")
		graph export "2.Result/Graph//$S_DATE/Updated_adj_`ep'_stroke `var'==`l'.png",replace

	}
} 



