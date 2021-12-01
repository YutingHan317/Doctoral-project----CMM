**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                               Part 2: regression                               *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
log using "D:\HanYT\03--生活方式变化\3. Result\regression $S_DATE.log", replace

use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
/* 第二次重复调查作为因变量；纳入基线变量和是否发生疾病的主效应和交互项 */
*** 二分类变量
local dichotomous_list healthy_smoking healthy_alcohol healthy_diet healthy_bmi healthy_WC healthy_sleep
foreach var of local dichotomous_list{

	di _dup(30) "*" " `var' " _dup(30) "*"
	di "*model1: no adjustment"
	logistic `var'2 `var'##c_ep0001_d
	di "*model2: age sex regions"
	logistic `var'2 `var'##c_ep0001_d i.is_female i.age_strata i.region_code

}

/* *** 多分类变量
local multi_list smoking_5groups alcohol_5groups diet_5groups sleep_5groups bmi_4groups WC_3groups
foreach var of local multi_list{

	qui sum `var'
	local min = r(min)
	mlogit `var'2 `var'##c_ep0001_d i.is_female i.age_strata, baseoutcome(`min')

} */


/* 变量的变化为因变量；是否发生疾病作为自变量纳入 */
local dichotomous_list healthy_smoking healthy_alcohol healthy_diet healthy_bmi healthy_WC healthy_sleep
foreach var of local dichotomous_list{

	local v = "`var'_change"
	di _dup(30) "*" " `var' " _dup(30) "*"
	di "*model1: no adjustment"
	mlogit `v' i.c_ep0001_d,baseoutcome(0) rr
	di "*model2: age sex regions"
	mlogit `v' i.c_ep0001_d i.is_female i.age_strata i.region_code,baseoutcome(0) rr


}

log close
