**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                           Part 1: primary analysis                             *************
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

/* Table1 比较基线全人群研究对象与第二次重复调查 */
tempname stats
tempfile table1
postfile `stats' str50 variable str50 category str50 baseline str50 resurvey2 using `table1', replace

***No. of participants
qui{
	local variable "No. of participants"
	local category ""
	use "D:\HanYT\03--生活方式变化\1. Data\merged_baseline_clean.dta", clear
	local baseline = _N
	use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
	local resurvey2 = _N
	post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'")
}

***Age

	local variable "Age"
	local category ""
	use "D:\HanYT\03--生活方式变化\1. Data\merged_baseline_clean.dta", clear
	sum age_at_study_date
	local baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
	sum age_at_study_date2
	local resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'")


***
******Continuous variables
loc varlist met sleep_hours bmi_calc
foreach var of local varlist{

	qui{

		use "D:\HanYT\03--生活方式变化\1. Data\merged_baseline_clean.dta", clear
		loc variable: variable label `var'
		loc category ""
		sum `var' 
		local baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
		**resurvey2
		use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
		sum `var'2
		local resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
		post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'")

	}
	
}

******Categorical variables
use "D:\HanYT\03--生活方式变化\1. Data\merged_baseline_clean.dta", clear
local varlist smoking_5groups alcohol_5groups diet_5groups sleep_5groups bmi_4groups WC_3groups ///
	healthy_smoking healthy_alcohol healthy_diet healthy_sleep healthy_bmi healthy_WC healthy_PA
foreach var of local varlist{
	
	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("")
	levelsof `var', local(level) clean
	foreach l of local level{
		
		loc variable ""
		loc category:	label(`var') `l' 
		use "D:\HanYT\03--生活方式变化\1. Data\merged_baseline_clean.dta", clear
		count if `var' == `l'
		loc baseline = string(r(N), "%9.0f") + "(" + string(((r(N)/_N)*100), "%9.1f") + ")"
		use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
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
use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
/* Table2 比较基线参与两次调查的人员的特征 */
tempname stats
tempfile table2
postfile `stats' str50 variable str50 category str50 baseline str50 resurvey2 using `table2', replace

***No. of participants
qui{
	local variable "No. of participants"
	local category ""
	use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
	local baseline = _N
	local resurvey2 = _N
	post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'")
}

***Age

	local variable "Age"
	local category ""
	use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
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
		use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
		loc variable: variable label `var'
		loc category ""
		sum `var' 
		local baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
		sum `var'2
		local resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
		post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'")
	}
}

******Categorical variables
use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
local varlist smoking_5groups alcohol_5groups diet_5groups sleep_5groups bmi_4groups WC_3groups ///
	healthy_smoking healthy_alcohol healthy_diet healthy_sleep healthy_bmi healthy_WC healthy_PA
foreach var of local varlist{
	
	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("")
	levelsof `var', local(level) clean
	foreach l of local level{
		
		loc variable ""
		loc category:	label(`var') `l' 
		use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
		count if `var' == `l'
		loc baseline = string(r(N), "%9.0f") + "(" + string(((r(N)/_N)*100), "%9.1f") + ")"
		count if `var'2 == `l'
		loc resurvey2 = string(r(N), "%9.0f") + "(" + string(((r(N)/_N)*100), "%9.1f") + ")"
		post `stats' ("`variable'") ("`category'") ("`baseline'") ("`resurvey2'")

	}

}

postclose `stats'
use `table2', clear
export excel using "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE\matched compare.xlsx", firstrow(var) replace


**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                      TABLE 3: Matched disease compare                          *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
***根据以下四组变量进行展示
loc endpoints c_ep0001_d four_disease ihd_stroke four_disease1 ihd_stroke1
foreach ep of local endpoints{

	use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
	tempname stats
	tempfile table3
	postfile `stats' str50 variable str50 category str50 disease_baseline str50 disease_resurvey2 str50 healthy_baseline str50 healthy_resurvey2 using `table3', replace
	
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
		post `stats' ("") ("") ("`disease_baseline'") ("`disease_resurvey2'") ("`healthy_baseline'") ("`healthy_resurvey2'")
	}
	
	***Age
	local variable "Age"
	local category ""
	sum age_at_study_date if `ep' == 1
	local disease_baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	sum age_at_study_date2 if `ep' == 1	
	local disease_resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	sum age_at_study_date if `ep' == 0
	local healthy_baseline = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	sum age_at_study_date2 if `ep' == 0	
	local healthy_resurvey2 = string(r(mean), "%9.1f") + "(" + string(r(sd), "%9.1f") + ")"
	post `stats' ("`variable'") ("`category'") ("`disease_baseline'") ("`disease_resurvey2'") ("`healthy_baseline'") ("`healthy_resurvey2'")
	
	******Continuous variables(adjusted for age, region and sex)
	loc varlist met sleep_hours bmi_calc
	foreach var of local varlist{
	
		qui{
			use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
			loc variable: variable label `var'
			loc category ""
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
			anova `var'2 `ep' c.age_at_study_date2 region_code
			margins `ep', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)
			mat a = r(table)
			loc disease_resurvey2 = string(a[1,2], "%9.1f") + "(" + string(`disease_sd', "%9.1f") + ")"
			loc healthy_resurvey2 = string(a[1,1], "%9.1f") + "(" + string(`healthy_sd', "%9.1f") + ")"
			post `stats' ("`variable'") ("`category'") ("`disease_baseline'") ("`disease_resurvey2'") ("`healthy_baseline'") ("`healthy_resurvey2'")
		}
	
	}
	
	******Categorical variables
	use "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", clear
	local varlist smoking_5groups alcohol_5groups diet_5groups sleep_5groups bmi_4groups WC_3groups ///
		healthy_smoking healthy_alcohol healthy_diet healthy_sleep healthy_bmi healthy_WC healthy_PA
	foreach var of local varlist{
		
		loc variable: variable label `var'
		post `stats' ("`variable'") ("") ("") ("") ("") ("") 
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
			post `stats' ("`variable'") ("`category'") ("`disease_baseline'") ("`disease_resurvey2'") ("`healthy_baseline'") ("`healthy_resurvey2'") 
	
		}
	
	}
	
	postclose `stats'
	use `table3', clear
	export excel using "D:\HanYT\03--生活方式变化\3. Result\\$S_DATE\matched compare `ep'.xlsx", firstrow(var) replace

}
