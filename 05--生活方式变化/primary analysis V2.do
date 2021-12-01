**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                        Part 1: descriptive analyses                            *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************

**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                TABLE 1: Unadjusted compare two surveys                         *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
cap mkdir "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE"

use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear

/* Table1 比较基线全人群研究对象与第二次重复调查 */
tempname stats
tempfile table1
postfile `stats' str50 variable str50 category str50 baseline str50 resurvey2 using `table1', replace

***No. of participants
qui{
	local variable "No. of participants"
	local category ""
	local baseline = _N
	local resurvey2 = _N
	post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'")
}

***Age

	local variable "Age"
	local category ""
	sum age_at_study_date
	local baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	sum age_at_study_date2
	local resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'")


***
******Continuous variables
loc varlist met sleep_hours bmi_calc
foreach var of local varlist{

	qui{

		loc variable: variable label `var'
		loc category ""
		sum `var' 
		local baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
		**resurvey2
		sum `var'2
		local resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
		post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'")

	}
	
}

******Categorical variables
local varlist smoking_5groups alcohol_5groups diet_5groups sleep_5groups bmi_4groups WC_3groups ///
	healthy_smoking healthy_alcohol healthy_diet healthy_sleep healthy_bmi healthy_WC healthy_PA
foreach var of local varlist{
	
	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("")
	levelsof `var', local(level) clean
	foreach l of local level{
		
		loc variable ""
		loc category:	label(`var') `l' 
		count if `var' == `l'
		loc baseline = string(r(N), "%9.0f") + "(" + string(((r(N)/_N)*100), "%9.1f") + ")"
		count if `var'2 == `l'
		loc resurvey2 = string(r(N), "%9.0f") + "(" + string(((r(N)/_N)*100), "%9.1f") + ")"
		post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'")

	}

}

postclose `stats'
use `table1', clear
export excel using "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE\Unadjusted compare.xlsx", firstrow(var) replace


**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                    TABLE 2: Matched resurvey2 compare                          *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
* 为了使用ttest
cd "D:\HanYT\03--生活方式变化"
use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
/* Table2 比较基线参与两次调查的人员的特征 */
tempname stats
tempfile table2
postfile `stats' str50 variable str50 category str50 baseline str50 resurvey2 str50 pct_agree str50 comp using `table2', replace

***No. of participants
qui{
	local variable "No. of participants"
	local category ""
	local baseline = _N
	local resurvey2 = _N
	post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'") ("") ("")
}

***Age

	local variable "Age"
	local category ""
	sum age_at_study_date
	local baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	sum age_at_study_date2
	local resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	* paired ttest
	ttest age_at_study_date2 == age_at_study_date    // 前者减后者 0802
	local diff = r(mu_1) - r(mu_2)    
	local diff_lc = `diff' - invt(r(df_t),0.975)*r(se)
	local diff_uc = `diff' + invt(r(df_t),0.975)*r(se)
	local comp = string(`diff',"%9.1f") + " (" + string(`diff_lc',"%9.1f") + " - " + string(`diff_uc',"%9.1f") + ")"
	post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'") ("") ("`comp'")


***
******Continuous variables
loc varlist met sleep_hours bmi_calc
foreach var of local varlist{

	qui{
		loc variable: variable label `var'
		loc category ""
		sum `var' 
		local baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
		sum `var'2
		local resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
		* paired ttest
		ttest `var'2 == `var'
		local diff = r(mu_1) - r(mu_2)    
		local diff_lc = `diff' - invt(r(df_t),0.975)*r(se)
		local diff_uc = `diff' + invt(r(df_t),0.975)*r(se)
		local comp = string(`diff',"%9.1f") + " (" + string(`diff_lc',"%9.1f") + " - " + string(`diff_uc',"%9.1f") + ")"
		post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'") ("") ("`comp'")
	}
}

******Categorical variables
local varlist diet_freq_meat diet_freq_eggs diet_freq_fresh_fruit diet_freq_fresh_veg ///
	diet_5groups smoking_5groups alcohol_5groups sleep_5groups bmi_4groups WC_3groups ///
	healthy_smoking healthy_alcohol healthy_diet healthy_sleep healthy_bmi healthy_WC healthy_PA 
foreach var of local varlist{
	
	loc variable: variable label `var'
	* prevalence-adjusted kappa
		kappaetc `var' `var'2, wgt(ordinal)
		mat a = r(table)*100
		local pct_agree = string(a[1,1], "%9.1f") + " (" + string(a[5,1], "%9.1f") + " - " + string(a[6,1], "%9.1f") + ")"
		local comp = string(a[1,2], "%9.1f") + " (" + string(a[5,2], "%9.1f") + " - " + string(a[6,2], "%9.1f") + ")"
	post `stats' ("`variable'") ("") ("") ("") ("`pct_agree'") ("`comp'")
	levelsof `var', local(level) clean
	foreach l of local level{
		
		loc variable ""
		loc category:	label(`var') `l' 
		count if `var' == `l'
		loc baseline = string(r(N), "%9.0f") + "(" + string(((r(N)/_N)*100), "%9.1f") + ")"
		count if `var'2 == `l'
		loc resurvey2 = string(r(N), "%9.0f") + "(" + string(((r(N)/_N)*100), "%9.1f") + ")"
		post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'") ("") ("")

	}

}

postclose `stats'
use `table2', clear
cap mkdir "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE"
export excel using "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE\matched compare.xlsx", firstrow(var) replace


**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                   TABLE 3: Matched disease compare adjusted                    *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************

***根据以下四组变量进行展示
loc endpoints c_ep0001_d four_disease ihd_stroke four_disease1 ihd_stroke1
foreach ep of local endpoints{

	use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
	tempname stats
	tempfile table3
	postfile `stats' str50 variable str50 category str50 disease_baseline str50 disease_resurvey2 str50 disease_pct_agree str50 disease_comp ///
		str50 healthy_baseline str50 healthy_resurvey2 str50 healthy_pct_agree str50 healthy_comp using `table3', replace
	
	***No. of participants
	qui{
		local variable "No. of participants"
		local category ""
		count if `ep' == 1
		local disease_baseline = r(N)
		local disease_resurvey2 = r(N)
		count if `ep' == 0
		local healthy_baseline = r(N)
		local healthy_resurvey2 = r(N)
		post `stats' ("") ("") ("`disease_baseline'") ("`disease_resurvey2'") ("") ("") ("`healthy_baseline'") ("`healthy_resurvey2'") ("") ("")
	}
	
	***Age
	local variable "Age"
	local category ""
	*** disease
	sum age_at_study_date if `ep' == 1
	local disease_baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	sum age_at_study_date2 if `ep' == 1	
	local disease_resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	* paired ttest
	ttest age_at_study_date2 == age_at_study_date if `ep' == 1    // 前者减后者 0802
	local diff = r(mu_1) - r(mu_2)    
	local diff_lc = `diff' - invt(r(df_t),0.975)*r(se)
	local diff_uc = `diff' + invt(r(df_t),0.975)*r(se)
	local disease_comp = string(`diff',"%9.1f") + " (" + string(`diff_lc',"%9.1f") + " - " + string(`diff_uc',"%9.1f") + ")"
	*** Healhthy
	sum age_at_study_date if `ep' == 0
	local healthy_baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	sum age_at_study_date2 if `ep' == 0	
	local healthy_resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	* paired ttest
	ttest age_at_study_date2 == age_at_study_date if `ep' == 0    // 前者减后者 0802
	local diff = r(mu_1) - r(mu_2)    
	local diff_lc = `diff' - invt(r(df_t),0.975)*r(se)
	local diff_uc = `diff' + invt(r(df_t),0.975)*r(se)
	local disease_comp = string(`diff',"%9.1f") + " (" + string(`diff_lc',"%9.1f") + " - " + string(`diff_uc',"%9.1f") + ")"
	post `stats' ("`variable'") ("`category'") ("`disease_baseline'") ("`disease_resurvey2'") ("") ("`disease_comp'") ///
		("`healthy_baseline'") ("`healthy_resurvey2'") ("") ("`healthy_comp'")
	
	******Continuous variables(adjusted for age, region and sex)
	loc varlist met sleep_hours bmi_calc
	foreach var of local varlist{
	
		qui{
			loc variable: variable label `var'
			loc category ""
			**baseline
			sum `var' if `ep' == 1
			loc disease_sd = r(sd)
			sum `var' if `ep' == 0
			loc healthy_sd = r(sd)
			anova `var' `ep' c.age_at_study_date region_code is_female
			margins `ep', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)
			mat a = r(table)
			loc disease_baseline = string(a[1,2], "%9.1f") + "(" + string(`disease_sd', "%9.1f") + ")"
			loc healthy_baseline = string(a[1,1], "%9.1f") + "(" + string(`healthy_sd', "%9.1f") + ")"
			**resurvey2
			sum `var'2 if `ep' == 1
			loc disease_sd = r(sd)
			sum `var'2 if `ep' == 0
			loc healthy_sd = r(sd)
			anova `var'2 `ep' c.age_at_study_date2 region_code is_female
			margins `ep', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)
			mat a = r(table)
			loc disease_resurvey2 = string(a[1,2], "%9.1f") + "(" + string(`disease_sd', "%9.1f") + ")"
			loc healthy_resurvey2 = string(a[1,1], "%9.1f") + "(" + string(`healthy_sd', "%9.1f") + ")"
			** paired ttest
			* disease
			ttest `var'2 == `var' if `ep' == 1
			local diff = r(mu_1) - r(mu_2)    
			local diff_lc = `diff' - invt(r(df_t),0.975)*r(se)
			local diff_uc = `diff' + invt(r(df_t),0.975)*r(se)
			local disease_comp = string(`diff',"%9.1f") + " (" + string(`diff_lc',"%9.1f") + " - " + string(`diff_uc',"%9.1f") + ")"
			* healthy
			ttest `var'2 == `var' if `ep' == 0
			local diff = r(mu_1) - r(mu_2)    
			local diff_lc = `diff' - invt(r(df_t),0.975)*r(se)
			local diff_uc = `diff' + invt(r(df_t),0.975)*r(se)
			local healthy_comp = string(`diff',"%9.1f") + " (" + string(`diff_lc',"%9.1f") + " - " + string(`diff_uc',"%9.1f") + ")"
			post `stats' ("`variable'") ("`category'") ("`disease_baseline'") ("`disease_resurvey2'") ("") ("`disease_comp'") ///
		("`healthy_baseline'") ("`healthy_resurvey2'") ("") ("`healthy_comp'")

		}
	
	}
	
	******Categorical variables
	local varlist smoking_5groups alcohol_5groups diet_5groups sleep_5groups bmi_4groups WC_3groups ///
		healthy_smoking healthy_alcohol healthy_diet healthy_sleep healthy_bmi healthy_WC healthy_PA
	foreach var of local varlist{
		
		loc variable: variable label `var'
		** prevalence-adjusted kappa
		* disease
		kappaetc `var' `var'2 if `ep' == 1, wgt(ordinal)
		mat a = r(table)*100
		local disease_pct_agree = string(a[1,1], "%9.1f") + " (" + string(a[5,1], "%9.1f") + " - " + string(a[6,1], "%9.1f") + ")"
		local disease_comp = string(a[1,2], "%9.1f") + " (" + string(a[5,2], "%9.1f") + " - " + string(a[6,2], "%9.1f") + ")"
		* control
		kappaetc `var' `var'2 if `ep' == 0, wgt(ordinal)
		mat a = r(table)*100
		local healthy_pct_agree = string(a[1,1], "%9.1f") + " (" + string(a[5,1], "%9.1f") + " - " + string(a[6,1], "%9.1f") + ")"
		local healthy_comp = string(a[1,2], "%9.1f") + " (" + string(a[5,2], "%9.1f") + " - " + string(a[6,2], "%9.1f") + ")"

		post `stats' ("`variable'") ("") ("") ("") ("`disease_pct_agree'") ("`disease_comp'") ("") ("") ("`healthy_pct_agree'") ("`healthy_comp'")

		sum `var'
		loc min = r(min)
		levelsof `var', local(level) clean
		foreach l of local level{
			
			loc variable ""
			loc category:	label(`var') `l'
			* baseline 
			count if `var' == `l' & `ep' == 1
			local d_b_number = r(N)
			count if `var' == `l' & `ep' == 0
			local h_b_number = r(N)
			* resurvey2
			count if `var'2 == `l' & `ep' == 1
			local d_r_number = r(N)
			count if `var'2 == `l' & `ep' == 0
			local h_r_number = r(N)
			mlogit `var' i.`ep' i.region_code i.is_female age_at_study_date, baseoutcome(`min')
				margins `ep', cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
				mat a = r(table)
				loc disease_baseline = string(`d_b_number') + " (" + string(a[1,2]*100, "%9.1f") + ")"
				loc healthy_baseline = string(`h_b_number') + " (" + string(a[1,1]*100, "%9.1f") + ")"
			mlogit `var'2 i.`ep' i.region_code i.is_female age_at_study_date2, baseoutcome(`min')
				margins `ep', cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
				mat a = r(table)
				loc disease_resurvey2 = string(`d_r_number') + " (" + string(a[1,2]*100, "%9.1f") + ")"
				loc healthy_resurvey2 = string(`d_r_number') + " (" + string(a[1,1]*100, "%9.1f") + ")"
			post `stats' ("`variable'") ("`category'") ("`disease_baseline'") ("`disease_resurvey2'") ("") ("") ("`healthy_baseline'") ("`healthy_resurvey2'") ("") ("")
	
		}
	
	}
	
	postclose `stats'
	use `table3', clear
	cap mkdir "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE"
	export excel using "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE\matched compare `ep'.xlsx", firstrow(var) replace

}


**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                   TABLE 4: Matched disease adjusted compared twice              *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
*** 考虑进行两组人间的比较
***根据以下四组变量进行展示
loc endpoints c_ep0001_d four_disease ihd_stroke four_disease1 ihd_stroke1
foreach ep of local endpoints{

	use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
	tempname stats
	tempfile table3
	postfile `stats' str50 variable str50 category str50 disease_baseline str50 disease_resurvey2 str50 disease_pct_agree str50 disease_comp ///
		str50 healthy_baseline str50 healthy_resurvey2 str50 healthy_pct_agree str50 healthy_comp str50 test using `table3', replace
	
	***No. of participants
	qui{
		local variable "No. of participants"
		local category ""
		count if `ep' == 1
		local disease_baseline = r(N)
		local disease_resurvey2 = r(N)
		count if `ep' == 0
		local healthy_baseline = r(N)
		local healthy_resurvey2 = r(N)
		post `stats' ("") ("") ("`disease_baseline'") ("`disease_resurvey2'") ("") ("") ("`healthy_baseline'") ("`healthy_resurvey2'") ("") ("") ("")
	}
	
	***Age
	local variable "Age"
	local category ""
	*** disease
	sum age_at_study_date if `ep' == 1
	local disease_baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	sum age_at_study_date2 if `ep' == 1	
	local disease_resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	* paired ttest
	ttest age_at_study_date2 == age_at_study_date if `ep' == 1    // 前者减后者 0802
	local diff = r(mu_1) - r(mu_2)    
	local diff_lc = `diff' - invt(r(df_t),0.975)*r(se)
	local diff_uc = `diff' + invt(r(df_t),0.975)*r(se)
	local disease_comp = string(`diff',"%9.1f") + " (" + string(`diff_lc',"%9.1f") + " - " + string(`diff_uc',"%9.1f") + ")"
	*** Healhthy
	sum age_at_study_date if `ep' == 0
	local healthy_baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	sum age_at_study_date2 if `ep' == 0	
	local healthy_resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	* paired ttest
	ttest age_at_study_date2 == age_at_study_date if `ep' == 0    // 前者减后者 0802
	local diff = r(mu_1) - r(mu_2)    
	local diff_lc = `diff' - invt(r(df_t),0.975)*r(se)
	local diff_uc = `diff' + invt(r(df_t),0.975)*r(se)
	local disease_comp = string(`diff',"%9.1f") + " (" + string(`diff_lc',"%9.1f") + " - " + string(`diff_uc',"%9.1f") + ")"
	post `stats' ("`variable'") ("`category'") ("`disease_baseline'") ("`disease_resurvey2'") ("") ("`disease_comp'") ///
		("`healthy_baseline'") ("`healthy_resurvey2'") ("") ("`healthy_comp'") ("")
	
	******Continuous variables(adjusted for age, region and sex)
	loc varlist met sleep_hours bmi_calc
	foreach var of local varlist{
	
		qui{
			loc variable: variable label `var'
			loc category ""
			**baseline
			sum `var' if `ep' == 1
			loc disease_sd = r(sd)
			sum `var' if `ep' == 0
			loc healthy_sd = r(sd)
			anova `var' `ep' c.age_at_study_date region_code is_female
			margins `ep', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)
			mat a = r(table)
			loc disease_baseline = string(a[1,2], "%9.1f") + "(" + string(`disease_sd', "%9.1f") + ")"
			loc healthy_baseline = string(a[1,1], "%9.1f") + "(" + string(`healthy_sd', "%9.1f") + ")"
			**resurvey2
			sum `var'2 if `ep' == 1
			loc disease_sd = r(sd)
			sum `var'2 if `ep' == 0
			loc healthy_sd = r(sd)
			anova `var'2 `ep' c.age_at_study_date2 region_code is_female
			margins `ep', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)
			mat a = r(table)
			loc disease_resurvey2 = string(a[1,2], "%9.1f") + "(" + string(`disease_sd', "%9.1f") + ")"
			loc healthy_resurvey2 = string(a[1,1], "%9.1f") + "(" + string(`healthy_sd', "%9.1f") + ")"
			
			** paired ttest
			* disease
			ttest `var'2 == `var' if `ep' == 1
			local disease_diff = r(mu_1) - r(mu_2)
			local disease_diff_lc = `disease_diff' - invt(r(df_t),0.975)*r(se)
			local disease_diff_uc = `disease_diff' + invt(r(df_t),0.975)*r(se)
			local disease_comp = string(`disease_diff',"%9.1f") + " (" + string(`disease_diff_lc',"%9.1f") + " - " + string(`disease_diff_uc',"%9.1f") + ")"
			* healthy
			ttest `var'2 == `var' if `ep' == 0
			local healthy_diff = r(mu_1) - r(mu_2)
			local healthy_diff_lc = `healthy_diff' - invt(r(df_t),0.975)*r(se)
			local healthy_diff_uc = `healthy_diff' + invt(r(df_t),0.975)*r(se)
			local healthy_comp = string(`healthy_diff',"%9.1f") + " (" + string(`healthy_diff_lc',"%9.1f") + " - " + string(`healthy_diff_uc',"%9.1f") + ")"

			** two-sample ttest
			tempvar continuous_difference
			gen `continuous_difference' = `var'2 - `var'			
			* disease
			sum `continuous_difference' if `ep' == 1, detail
			local disease_diff_mean = r(mean)
			local disease_diff_sd = r(sd)
			local disease_diff_number = r(N)
			* healthy
			sum `continuous_difference' if `ep' == 0, detail
			local healthy_diff_mean = r(mean)
			local healthy_diff_sd = r(sd)
			local healthy_diff_number = r(N)
			* ttest
			ttesti `disease_diff_number' `disease_diff_mean' `disease_diff_sd' `healthy_diff_number' `healthy_diff_mean' `healthy_diff_sd'
			local diff = r(mu_1) - r(mu_2)
			local diff_lc = `diff' - invt(r(df_t),0.975)*r(se)
			local diff_uc = `diff' + invt(r(df_t),0.975)*r(se)
			local comp = string(`diff',"%9.1f") + " (" + string(`diff_lc',"%9.1f") + " - " + string(`diff_uc',"%9.1f") + ")"
			post `stats' ("`variable'") ("`category'") ("`disease_baseline'") ("`disease_resurvey2'") ("") ("`disease_comp'") ///
		("`healthy_baseline'") ("`healthy_resurvey2'") ("") ("`healthy_comp'") ("`comp'")

		}
	
	}
	
	******Categorical variables
	local varlist smoking_5groups alcohol_5groups diet_5groups sleep_5groups bmi_4groups WC_3groups ///
		healthy_smoking healthy_alcohol healthy_diet healthy_sleep healthy_bmi healthy_WC healthy_PA
	foreach var of local varlist{
		
		loc variable: variable label `var'
		** prevalence-adjusted kappa
		* disease
		kappaetc `var' `var'2 if `ep' == 1, wgt(ordinal)
		mat a = r(table)
		* 比较符合率的差异
		local disease_se = a[2,1]
		local disease_b  = a[1,1]
		local disease_pct_agree = string(a[1,1]*100, "%9.1f") + " (" + string(a[5,1]*100, "%9.1f") + " - " + string(a[6,1]*100, "%9.1f") + ")"
		local disease_comp = string(a[1,2]*100, "%9.1f") + " (" + string(a[5,2]*100, "%9.1f") + " - " + string(a[6,2]*100, "%9.1f") + ")"
		* control
		kappaetc `var' `var'2 if `ep' == 0, wgt(ordinal)
		mat a = r(table)
		local healthy_se = a[2,1]
		local healthy_b  = a[1,1]
		local healthy_pct_agree = string(a[1,1]*100, "%9.1f") + " (" + string(a[5,1]*100, "%9.1f") + " - " + string(a[6,1]*100, "%9.1f") + ")"
		local healthy_comp = string(a[1,2]*100, "%9.1f") + " (" + string(a[5,2]*100, "%9.1f") + " - " + string(a[6,2]*100, "%9.1f") + ")"

		* 比较差值的差异
		local b = `disease_b' - `healthy_b'
		local se = sqrt((`disease_se')^2 + (`healthy_se')^2)
		local b_lc = `b' - invnorm(0.975)*`se'
		local b_uc = `b' + invnorm(0.975)*`se'
		local comp = string(`b'*100, "%9.1f") + " (" + string(`b_lc'*100, "%9.1f") + " - " + string(`b_uc'*100, "%9.1f") + ")"
		post `stats' ("`variable'") ("") ("") ("") ("`disease_pct_agree'") ("`disease_comp'") ("") ("") ("`healthy_pct_agree'") ("`healthy_comp'") ("`comp'")

		sum `var'
		loc min = r(min)
		levelsof `var', local(level) clean
		foreach l of local level{
			
			loc variable ""
			loc category:	label(`var') `l'
			* baseline 
			count if `var' == `l' & `ep' == 1
			local d_b_number = r(N)
			count if `var' == `l' & `ep' == 0
			local h_b_number = r(N)
			* resurvey2
			count if `var'2 == `l' & `ep' == 1
			local d_r_number = r(N)
			count if `var'2 == `l' & `ep' == 0
			local h_r_number = r(N)
			mlogit `var' i.`ep' i.region_code i.is_female age_at_study_date, baseoutcome(`min')
				margins `ep', cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
				mat a = r(table)
				loc disease_baseline = string(`d_b_number') + " (" + string(a[1,2]*100, "%9.1f") + ")"
				loc healthy_baseline = string(`h_b_number') + " (" + string(a[1,1]*100, "%9.1f") + ")"
			mlogit `var'2 i.`ep' i.region_code i.is_female age_at_study_date2, baseoutcome(`min')
				margins `ep', cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`l'))
				mat a = r(table)
				loc disease_resurvey2 = string(`d_r_number') + " (" + string(a[1,2]*100, "%9.1f") + ")"
				loc healthy_resurvey2 = string(`d_r_number') + " (" + string(a[1,1]*100, "%9.1f") + ")"
			post `stats' ("`variable'") ("`category'") ("`disease_baseline'") ("`disease_resurvey2'") ("") ("") ("`healthy_baseline'") ("`healthy_resurvey2'") ("") ("") ("")
	
		}
	
	}
	
	postclose `stats'
	use `table3', clear
	cap mkdir "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE\"
	export excel using "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE\matched compare `ep'.xlsx", firstrow(var) replace

}
