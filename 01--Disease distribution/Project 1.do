**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                           Project 1: Distribution                              *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
log using "D:\HanYT\2019-06 Multimorbidity\3. Result\Project 1.log", replace

**********************************************************************************************************
*************                                                                                *************
*************              Project 1.1.a: Distribution of multimorbidity                     *************
*************                                                                                *************
**********************************************************************************************************

**********************************************************************************************************
*************                            Project 1.1.a: Baseline                             *************
**********************************************************************************************************
/* 描述不同人群亚组的共病类型构成比 */

/* 基线共病状态 */
***Import dataset without T1DM 
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta"

***Number of participants overall and in subgroups
tabstat mltmbd_base, by(is_female) s(count sum mean)
bysort is_female: tab1 mltmbd_base region_is_urban age_3groups education_3groups income_3groups marital_status_2groups healthy_score


***residence
*male
qui mlogit mltmbd_base i.region_is_urban age_at_study_date if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
*female
qui mlogit mltmbd_base i.region_is_urban age_at_study_date if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))

***age groups
*male
qui mlogit mltmbd_base i.age_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
*female
qui mlogit mltmbd_base i.age_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))

***gen distribution table
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta" ,replace
loc varlist education_3groups income_3groups marital_status_2groups healthy_score
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 non_mltmbd_base_male str50 mltmbd_base_male str50 non_mltmbd_base_female ///
	str50 mltmbd_base_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {
		qui levelsof mltmbd_base, local(disease)
		foreach d of local disease{

			qui{
				mlogit mltmbd_base i.`var' age_at_study_date i.region_code if is_female == `sex', baseoutcome(0)
			margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
			mat `var'_`sex'_`d' = r(table)
			levelsof `var', local(levels)
			foreach l of local levels{
				local category: label(`var')`l'
				local category_`l' = "`category'"
				local i = `l' +1
				local mltmbd_base_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
			}
			}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`mltmbd_base_0_0_`l''") ("`mltmbd_base_0_1_`l''") ("`mltmbd_base_1_0_`l''") ("`mltmbd_base_1_1_`l''") 

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of mltmbd_base.xlsx", firstrow(var) sheetreplace



/* 是否存在CMD */
***Import dataset without T1DM 
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta"

***Number of participants
tabstat free_cmd_base, by(is_female) s(count sum mean)
bysort is_female: tab1 free_cmd_base free_cmd_end 

***residence
*male
qui mlogit free_cmd_base i.region_is_urban age_at_study_date if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
*female
qui mlogit free_cmd_base i.region_is_urban age_at_study_date if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))

***age groups
*male
qui mlogit free_cmd_base i.age_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
*female
qui mlogit free_cmd_base i.age_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))

***gen distribution table
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta" ,replace
loc varlist education_3groups income_3groups marital_status_2groups healthy_score
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 non_free_cmd_base_male str50 free_cmd_base_male str50 non_free_cmd_base_female ///
	str50 free_cmd_base_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {
		qui levelsof free_cmd_base, local(disease)
		foreach d of local disease{

			qui{
				mlogit free_cmd_base i.`var' age_at_study_date i.region_code if is_female == `sex', baseoutcome(0)
				margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
				mat `var'_`sex'_`d' = r(table)
				mat list `var'_`sex'_`d'
				levelsof `var', local(levels)
				foreach l of local levels{
					local category: label(`var')`l'
					local category_`l' = "`category'"
					local i = `l' +1
					local free_cmd_base_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
					di "`free_cmd_base_`sex'_`d'_`l''"
			}
		}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`free_cmd_base_0_0_`l''") ("`free_cmd_base_0_1_`l''") ("`free_cmd_base_1_0_`l''") ("`free_cmd_base_1_1_`l''") 

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of free_cmd_base.xlsx", firstrow(var) sheetreplace



/* 疾病数量 */
***Import dataset without T1DM 
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta"

***Number of participants
tab is_female d_number_base,row
bysort is_female: tab1 d_number_base d_number_end education_3groups income_3groups marital_status_2groups healthy_score

***residence
*male
qui mlogit d_number_base i.region_is_urban age_at_study_date if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
*female
qui mlogit d_number_base i.region_is_urban age_at_study_date if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))

***age groups
*male
qui mlogit d_number_base i.age_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
*female
qui mlogit d_number_base i.age_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))

***gen distribution table
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta" ,replace
loc varlist education_3groups income_3groups marital_status_2groups healthy_score
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 d_number_base_0_male str50 d_number_base_1_male str50 d_number_base_2_male ///
	str50 d_number_base_3_male str50 d_number_base_0_female str50 d_number_base_1_female str50 d_number_base_2_female ///
	str50 d_number_base_3_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {
		qui levelsof d_number_base, local(disease)
		foreach d of local disease{

			qui{
				mlogit d_number_base i.`var' age_at_study_date i.region_code if is_female == `sex', baseoutcome(0)
				margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
				mat `var'_`sex'_`d' = r(table)
				mat list `var'_`sex'_`d'
				levelsof `var', local(levels)
				foreach l of local levels{
					local category: label(`var')`l'
					local category_`l' = "`category'"
					local i = `l' +1
					local d_number_base_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
					di "`d_number_base_`sex'_`d'_`l''"
			}
		}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`d_number_base_0_0_`l''") ("`d_number_base_0_1_`l''") ("`d_number_base_0_2_`l''") ("`d_number_base_0_3_`l''") ///
			("`d_number_base_1_0_`l''") ("`d_number_base_1_1_`l''") ("`d_number_base_1_2_`l''") ("`d_number_base_1_3_`l''") 

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of d_number_base.xlsx", firstrow(var) sheetreplace


/* 疾病数量(3groups) */
***Import dataset without T1DM 
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta"

***Number of participants
tab is_female d_number2_base,row

***residence
*male
qui mlogit d_number2_base i.region_is_urban age_at_study_date if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
*female
qui mlogit d_number2_base i.region_is_urban age_at_study_date if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))

***age groups
*male
qui mlogit d_number2_base i.age_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
*female
qui mlogit d_number2_base i.age_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))

***gen distribution table
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta" ,replace
loc varlist education_3groups income_3groups marital_status_2groups healthy_score
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 d_number2_base_0_male str50 d_number2_base_1_male str50 d_number2_base_2_male ///
	str50 d_number2_base_0_female str50 d_number2_base_1_female str50 d_number2_base_2_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {

		qui levelsof d_number2_base, local(disease)
		foreach d of local disease{

			qui{
				mlogit d_number2_base i.`var' age_at_study_date i.region_code if is_female == `sex', baseoutcome(0)
				margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
				mat `var'_`sex'_`d' = r(table)
				mat list `var'_`sex'_`d'
				levelsof `var', local(levels)
				foreach l of local levels{
					local category: label(`var')`l'
					local category_`l' = "`category'"
					local i = `l' +1
					local d_number2_base_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
					di "`d_number2_base_`sex'_`d'_`l''"

			}
		}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`d_number2_base_0_0_`l''") ("`d_number2_base_0_1_`l''") ("`d_number2_base_0_2_`l''") ///
			("`d_number2_base_1_0_`l''") ("`d_number2_base_1_1_`l''") ("`d_number2_base_1_2_`l''") 

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of d_number2_base.xlsx", firstrow(var) sheetreplace


/* 基线疾病组合 */
***Import dataset without T1DM 
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta"

***Number of participants
tab is_female combin_base,row

***residence
*male
qui mlogit combin_base i.region_is_urban age_at_study_date if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(6))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(7))
*female
qui mlogit combin_base i.region_is_urban age_at_study_date if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(6))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(7))

***age groups
*male
qui mlogit combin_base i.age_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(6))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(7))
*female
qui mlogit combin_base i.age_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(6))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(7))

***gen distribution table
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta" ,replace
loc varlist education_3groups income_3groups marital_status_2groups healthy_score
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 combin_base_0_male str50 combin_base_1_male str50 combin_base_2_male ///
	str50 combin_base_3_male str50 combin_base_4_male str50 combin_base_5_male str50 combin_base_6_male str50 combin_base_7_male ///
	str50 combin_base_0_female str50 combin_base_1_female str50 combin_base_2_female str50 combin_base_3_female ///
	str50 combin_base_4_female str50 combin_base_5_female str50 combin_base_6_female str50 combin_base_7_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {
		qui levelsof combin_base, local(disease)
		foreach d of local disease{

			qui{
				mlogit combin_base i.`var' age_at_study_date i.region_code if is_female == `sex', baseoutcome(0)
				margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
				mat `var'_`sex'_`d' = r(table)
				mat list `var'_`sex'_`d'
				levelsof `var', local(levels)
				foreach l of local levels{
					local category: label(`var')`l'
					local category_`l' = "`category'"
					local i = `l' +1
					local combin_base_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
					di "`combin_base_`sex'_`d'_`l''"
			}
		}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`combin_base_0_0_`l''") ("`combin_base_0_1_`l''") ("`combin_base_0_2_`l''") ("`combin_base_0_3_`l''") ///
			("`combin_base_0_4_`l''") ("`combin_base_0_5_`l''") ("`combin_base_0_6_`l''") ("`combin_base_0_7_`l''") ///
			("`combin_base_1_0_`l''") ("`combin_base_1_1_`l''") ("`combin_base_1_2_`l''") ("`combin_base_1_3_`l''") ///
			("`combin_base_1_4_`l''") ("`combin_base_1_5_`l''") ("`combin_base_1_6_`l''") ("`combin_base_1_7_`l''")

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of combin_base.xlsx", firstrow(var) sheetreplace

**********************************************************************************************************
*************                              Project 1.1.a: End                                *************
**********************************************************************************************************
/* 描述不同人群亚组的共病类型构成比 */

/* 期末共病状态 */
***Import dataset without T1DM 
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta"

***Number of participants
tabstat mltmbd_end, by(is_female) s(count sum mean)

***residence
*male
qui mlogit mltmbd_end i.region_is_urban age_at_end if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
*female
qui mlogit mltmbd_end i.region_is_urban age_at_end if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))

***age groups
*male
qui mlogit mltmbd_end i.age_end_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
*female
qui mlogit mltmbd_end i.age_end_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))

***gen distribution table
loc varlist education_3groups income_3groups marital_status_2groups healthy_score
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 non_mltmbd_end_male str50 mltmbd_end_male str50 non_mltmbd_end_female ///
	str50 mltmbd_end_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {
		qui levelsof mltmbd_end, local(disease)
		foreach d of local disease{

			qui{
				mlogit mltmbd_end i.`var' age_at_end i.region_code if is_female == `sex', baseoutcome(0)
			margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
			mat `var'_`sex'_`d' = r(table)
			levelsof `var', local(levels)
			foreach l of local levels{
				local category: label(`var')`l'
				local category_`l' = "`category'"
				local i = `l' +1
				local mltmbd_end_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
			}
			}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`mltmbd_end_0_0_`l''") ("`mltmbd_end_0_1_`l''") ("`mltmbd_end_1_0_`l''") ("`mltmbd_end_1_1_`l''") 

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of mltmbd_end.xlsx", firstrow(var) sheetreplace


/* 是否存在CMD */
***Import dataset without T1DM 
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta"

***Number of participants
tabstat free_cmd_end, by(is_female) s(count sum mean)

***residence
*male
qui mlogit free_cmd_end i.region_is_urban age_at_end if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
*female
qui mlogit free_cmd_end i.region_is_urban age_at_end if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))

***age groups
*male
qui mlogit free_cmd_end i.age_end_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
*female
qui mlogit free_cmd_end i.age_end_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))

***gen distribution table
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta" ,replace
loc varlist education_3groups income_3groups marital_status_2groups healthy_score
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 non_free_cmd_end_male str50 free_cmd_end_male str50 non_free_cmd_end_female ///
	str50 free_cmd_end_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {
		qui levelsof free_cmd_end, local(disease)
		foreach d of local disease{

			qui{
				mlogit free_cmd_end i.`var' age_at_end i.region_code if is_female == `sex', baseoutcome(0)
				margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
				mat `var'_`sex'_`d' = r(table)
				mat list `var'_`sex'_`d'
				levelsof `var', local(levels)
				foreach l of local levels{
					local category: label(`var')`l'
					local category_`l' = "`category'"
					local i = `l' +1
					local free_cmd_end_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
					di "`free_cmd_end_`sex'_`d'_`l''"
			}
		}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`free_cmd_end_0_0_`l''") ("`free_cmd_end_0_1_`l''") ("`free_cmd_end_1_0_`l''") ("`free_cmd_end_1_1_`l''") 

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of free_cmd_end.xlsx", firstrow(var) sheetreplace


/* 期末疾病数量 */
***Import dataset without T1DM 
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta"

***Number of participants
tab is_female d_number_end,row

***residence
*male
qui mlogit d_number_end i.region_is_urban age_at_end if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
*female
qui mlogit d_number_end i.region_is_urban age_at_end if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))

***age groups
*male
qui mlogit d_number_end i.age_end_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
*female
qui mlogit d_number_end i.age_end_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))

***gen distribution table
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta" ,replace
loc varlist education_3groups income_3groups marital_status_2groups healthy_score
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 d_number_end_0_male str50 d_number_end_1_male str50 d_number_end_2_male ///
	str50 d_number_end_3_male str50 d_number_end_0_female str50 d_number_end_1_female str50 d_number_end_2_female ///
	str50 d_number_end_3_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {
		qui levelsof d_number_end, local(disease)
		foreach d of local disease{

			qui{
				mlogit d_number_end i.`var' age_at_end i.region_code if is_female == `sex', baseoutcome(0)
				margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
				mat `var'_`sex'_`d' = r(table)
				mat list `var'_`sex'_`d'
				levelsof `var', local(levels)
				foreach l of local levels{
					local category: label(`var')`l'
					local category_`l' = "`category'"
					local i = `l' +1
					local d_number_end_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
					di "`d_number_end_`sex'_`d'_`l''"
			}
		}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`d_number_end_0_0_`l''") ("`d_number_end_0_1_`l''") ("`d_number_end_0_2_`l''") ("`d_number_end_0_3_`l''") ///
			("`d_number_end_1_0_`l''") ("`d_number_end_1_1_`l''") ("`d_number_end_1_2_`l''") ("`d_number_end_1_3_`l''") 

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of d_number_end.xlsx", firstrow(var) sheetreplace


/* 期末疾病数量(3groups) */
***Import dataset without T1DM 
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta"

***Number of participants
tab is_female d_number2_end,row
bysort is_female:tab1 combin_base combin_end

***residence
*male
qui mlogit d_number2_end i.region_is_urban age_at_end if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
*female
qui mlogit d_number2_end i.region_is_urban age_at_end if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))

***age groups
*male
qui mlogit d_number2_end i.age_end_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
*female
qui mlogit d_number2_end i.age_end_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))

***gen distribution table
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta" ,replace
loc varlist education_3groups income_3groups marital_status_2groups healthy_score
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 d_number2_end_0_male str50 d_number2_end_1_male str50 d_number2_end_2_male ///
	str50 d_number2_end_0_female str50 d_number2_end_1_female str50 d_number2_end_2_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {

		qui levelsof d_number2_end, local(disease)
		foreach d of local disease{

			qui{
				mlogit d_number2_end i.`var' age_at_end i.region_code if is_female == `sex', baseoutcome(0)
				margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
				mat `var'_`sex'_`d' = r(table)
				mat list `var'_`sex'_`d'
				levelsof `var', local(levels)
				foreach l of local levels{
					local category: label(`var')`l'
					local category_`l' = "`category'"
					local i = `l' +1
					local d_number2_end_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
					di "`d_number2_end_`sex'_`d'_`l''"

			}
		}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`d_number2_end_0_0_`l''") ("`d_number2_end_0_1_`l''") ("`d_number2_end_0_2_`l''") ///
			("`d_number2_end_1_0_`l''") ("`d_number2_end_1_1_`l''") ("`d_number2_end_1_2_`l''") 

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of d_number2_end.xlsx", firstrow(var) sheetreplace


/* 期末疾病组合 */
***Import dataset without T1DM 
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta"

***Number of participants
tab is_female combin_end,row

***residence
*male
qui mlogit combin_end i.region_is_urban age_at_end if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(6))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(7))
*female
qui mlogit combin_end i.region_is_urban age_at_end if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(6))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(7))

***age groups
*male
qui mlogit combin_end i.age_end_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(6))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(7))
*female
qui mlogit combin_end i.age_end_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(6))
margins age_end_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(7))

***gen distribution table
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta" ,replace
loc varlist education_3groups income_3groups marital_status_2groups healthy_score
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 combin_end_0_male str50 combin_end_1_male str50 combin_end_2_male ///
	str50 combin_end_3_male str50 combin_end_4_male str50 combin_end_5_male str50 combin_end_6_male str50 combin_end_7_male ///
	str50 combin_end_0_female str50 combin_end_1_female str50 combin_end_2_female str50 combin_end_3_female ///
	str50 combin_end_4_female str50 combin_end_5_female str50 combin_end_6_female str50 combin_end_7_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {
		qui levelsof combin_end, local(disease)
		foreach d of local disease{

			qui{
				mlogit combin_end i.`var' age_at_end i.region_code if is_female == `sex', baseoutcome(0)
				margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
				mat `var'_`sex'_`d' = r(table)
				mat list `var'_`sex'_`d'
				levelsof `var', local(levels)
				foreach l of local levels{
					local category: label(`var')`l'
					local category_`l' = "`category'"
					local i = `l' +1
					local combin_end_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
					di "`combin_end_`sex'_`d'_`l''"
			}
		}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`combin_end_0_0_`l''") ("`combin_end_0_1_`l''") ("`combin_end_0_2_`l''") ("`combin_end_0_3_`l''") ///
			("`combin_end_0_4_`l''") ("`combin_end_0_5_`l''") ("`combin_end_0_6_`l''") ("`combin_end_0_7_`l''") ///
			("`combin_end_1_0_`l''") ("`combin_end_1_1_`l''") ("`combin_end_1_2_`l''") ("`combin_end_1_3_`l''") ///
			("`combin_end_1_4_`l''") ("`combin_end_1_5_`l''") ("`combin_end_1_6_`l''") ("`combin_end_1_7_`l''")

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of combin_end.xlsx", firstrow(var) sheetreplace


**********************************************************************************************************
*************                           Project 1.1.a: Follow_up                             *************
**********************************************************************************************************
/* 随访期间首发疾病类型 */
***Import dataset without T1DM 
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project2.dta",replace

***exclude participants who have same date on first CMD
tempvar min median
egen `min'    = rowmin(c_ep0003_date c_ep0009_date c_ep0088_date)
egen `median' = rowmedian(c_ep0003_date c_ep0009_date c_ep0088_date)
drop if `min' == `median' & d_number_inc > 1

***generate variable on first manifestation of CMD
gen type_fcmd = 0 if d_number_inc == 0
	replace type_fcmd = 1 if c_ep0088_date == `min' & d_number_inc != 0
	replace type_fcmd = 2 if c_ep0003_date == `min' & d_number_inc != 0
	replace type_fcmd = 3 if c_ep0009_date == `min' & d_number_inc != 0
	label variable type_fcmd "type of first manifestation of CMD"
	label define type_fcmd 0 "healthy" 1 "T2DM" 2 "IHD" 3 "IST"
	label values type_fcmd type_fcmd

***Number of participants
tab is_female type_fcmd,row
bysort is_female: tab1 type_fcmd region_urban age_3groups healthy_smoking healthy_alcohol healthy_PA healthy_diet ///
	healthy_sleep education_3groups income_3groups marital_status_2groups healthy_score

***residence
*male
qui mlogit type_fcmd i.region_is_urban age_at_study_date if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
*female
qui mlogit type_fcmd i.region_is_urban age_at_study_date if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))

***age groups
*male
qui mlogit type_fcmd i.age_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
*female
qui mlogit type_fcmd i.age_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))

***gen distribution table
loc varlist education_3groups income_3groups marital_status_2groups healthy_score ///
	healthy_smoking healthy_alcohol healthy_PA healthy_diet healthy_obesity healthy_sleep
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 type_fcmd_0_male str50 type_fcmd_1_male str50 type_fcmd_2_male ///
	str50 type_fcmd_3_male str50 type_fcmd_0_female str50 type_fcmd_1_female str50 type_fcmd_2_female ///
	str50 type_fcmd_3_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {
		qui levelsof type_fcmd, local(disease)
		foreach d of local disease{

			qui{
				mlogit type_fcmd i.`var' age_at_study_date i.region_code if is_female == `sex', baseoutcome(0)
				margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
				mat `var'_`sex'_`d' = r(table)
				mat list `var'_`sex'_`d'
				levelsof `var', local(levels)
				foreach l of local levels{
					local category: label(`var')`l'
					local category_`l' = "`category'"
					local i = `l' +1
					local type_fcmd_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
					di "`type_fcmd_`sex'_`d'_`l''"
			}
		}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`type_fcmd_0_0_`l''") ("`type_fcmd_0_1_`l''") ("`type_fcmd_0_2_`l''") ("`type_fcmd_0_3_`l''") ///
			("`type_fcmd_1_0_`l''") ("`type_fcmd_1_1_`l''") ("`type_fcmd_1_2_`l''") ("`type_fcmd_1_3_`l''") 

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of type_fcmd.xlsx", firstrow(var) sheetreplace



/* 随访期间首发共病组合 */
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project2.dta",replace

***Number of participants
tab is_female combin_inc,row
bysort is_female:tab1 combin_inc healthy_smoking healthy_alcohol healthy_PA healthy_diet ///
	healthy_sleep education_3groups income_3groups marital_status_2groups healthy_score
***residence
*male
qui mlogit combin_inc i.region_is_urban age_at_study_date if is_female == 0, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))
*female
qui mlogit combin_inc i.region_is_urban age_at_study_date if is_female == 1, baseoutcome(0)
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins region_is_urban, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))
***age groups
*male
qui mlogit combin_inc i.age_3groups i.region_code if is_female == 0, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))
*female
qui mlogit combin_inc i.age_3groups i.region_code if is_female == 1, baseoutcome(0)
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(0))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(1))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(2))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(3))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(4))
margins age_3groups, cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(5))

***gen distribution table;replace healthy_score with healthy_score_6groups because unable to converge
loc varlist education_3groups income_3groups marital_status_2groups healthy_score_6groups ///
	healthy_smoking healthy_alcohol healthy_PA healthy_diet healthy_obesity healthy_sleep
tempname stats
tempfile project1
postfile `stats' str50 variable str50 category str50 combin_inc_0_male str50 combin_inc_1_male str50 combin_inc_2_male ///
	str50 combin_inc_3_male str50 combin_inc_4_male str50 combin_inc_5_male ///
	str50 combin_inc_0_female str50 combin_inc_1_female str50 combin_inc_2_female str50 combin_inc_3_female ///
	str50 combin_inc_4_female str50 combin_inc_5_female using `project1', replace
foreach var of local varlist{

	loc variable: variable label `var'
	post `stats' ("`variable'") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") 
	forvalues sex = 0/1 {
		qui levelsof combin_inc, local(disease)
		foreach d of local disease{

			qui{
				mlogit combin_inc i.`var' age_at_study_date i.region_code if is_female == `sex', baseoutcome(0)
				margins `var', cformat(%9.3f) pformat(%5.3f) sformat(%8.3f) predict(outcome(`d'))
				mat `var'_`sex'_`d' = r(table)
				mat list `var'_`sex'_`d'
				levelsof `var', local(levels)
				foreach l of local levels{
					local category: label(`var')`l'
					local category_`l' = "`category'"
					local i = `l' +1
					local combin_inc_`sex'_`d'_`l' = string(`var'_`sex'_`d'[1,`i']*100, "%9.1f")
					di "`combin_inc_`sex'_`d'_`l''"
			}
		}

		}
	}		
	foreach l of local levels{

		post `stats' ("") ("`category_`l''") ("`combin_inc_0_0_`l''") ("`combin_inc_0_1_`l''") ("`combin_inc_0_2_`l''") ("`combin_inc_0_3_`l''") ///
			("`combin_inc_0_4_`l''") ("`combin_inc_0_5_`l''")  ///
			("`combin_inc_1_0_`l''") ("`combin_inc_1_1_`l''") ("`combin_inc_1_2_`l''") ("`combin_inc_1_3_`l''") ///
			("`combin_inc_1_4_`l''") ("`combin_inc_1_5_`l''") 

	}
}			

postclose `stats'
use `project1', clear
export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\distribution of combin_inc.xlsx", firstrow(var) sheetreplace


log close

**********************************************************************************************************
*************                                                                                *************
*************             Project 1.2: Birth cohort & incidence rate                       *************
*************                                                                                *************
**********************************************************************************************************
log using "D:\HanYT\2019-06 Multimorbidity\3. Result\Project 1.2  10-30.log", replace
/* age-specific incidence rate in whole dataset */
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project2.dta",replace
preserve
stset mltmbd_inc_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(mltmbd_inc == 1)
stsplit age, at(30 40 45(5)85)
/* gen py = _t - _t0
poisson mltmbd_inc i.age, exposure(py)
margins age, predict(ir) cformat(%9.5f) */
strate age, graph
stsum,by(age)


**********************************************************************************************************
*************                           Project 1.2.a: Birth cohort                          *************
**********************************************************************************************************
/* Birth cohort analysis */
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project2.dta",replace
cap drop person_year
***extract year of birth
gen birth_year = year(dob_anon)
egen birth_cohort = cut(birth_year), at(1925 1930(5)1975 1980) label
tab birth_cohort

***split follow-up duration in 5y groups
stset mltmbd_inc_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(mltmbd_inc == 1)
/* **merge two groups: 30-34;35-39
preserve
stsplit age1, at(30 40 45(5)85)
gen person_year = _t-_t0
table age1, c(sum person_year)
restore */
**no merge
stsplit age, at(30(5)85)
gen person_year = _t-_t0
table age, c(sum person_year)
**calculate birth cohort-specific cohort
tempfile birth_cohort
levelsof birth_cohort, local(level)
foreach i of local level{

	local name: label(birth_cohort)`i'
	preserve
	strate age if birth_cohort == `i', output(`birth_cohort',replace) per(10000)
	use `birth_cohort', clear
	export excel using "D:\HanYT\2019-06 Multimorbidity\3. Result\Table of birth cohort\table of birth cohort `name'.xlsx", firstrow(var) sheetreplace
	restore
}

/* Graph using MY PC */
import excel "D:\Onedrive\00. 博士课题\02-结果\01-课题1\Birth cohort\02-Project 1.2 -birth cohort.xlsx", sheet("出生队列") firstrow clear
***convert data from wide to long
reshape long b, i(age) j(birth_cohort)
gen birth_cohort1 = birth_cohort/10
drop birth_cohort
rename birth_cohort1 birth_cohort
rename b incidence
**label variable
generate str var4 = "30-" in 11
replace var4 = "35-" in 20
replace var4 = "40-" in 30
replace var4 = "45-" in 40
replace var4 = "50-" in 50
replace var4 = "55-" in 60
replace var4 = "60-" in 70
replace var4 = "65-" in 80
replace var4 = "70-" in 90
replace var4 = "75-" in 100
replace var4 = "80-" in 111
replace var4 = "85-" in 122
rename var4 agegrp
global connect_options  connect(l) sort  msize(vtiny) mlwidth(none) lwidth(thin) 
global mlabel mlabel(agegrp)  mlabposition(9) mlabsize(tiny)
***graph
twoway scatter incidence birth_cohort if age == "30-", $connect_options $mlabel || ///
	scatter incidence birth_cohort if age == "35-", $connect_options $mlabel || ///
	scatter incidence birth_cohort if age == "40-", $connect_options $mlabel || ///
	scatter incidence birth_cohort if age == "45-", $connect_options $mlabel || ///
	scatter incidence birth_cohort if age == "50-", $connect_options $mlabel || ///
	scatter incidence birth_cohort if age == "55-", $connect_options $mlabel || ///
	scatter incidence birth_cohort if age == "60-", $connect_options $mlabel || ///
	scatter incidence birth_cohort if age == "65-", $connect_options $mlabel || ///
	scatter incidence birth_cohort if age == "70-", $connect_options $mlabel || ///
	scatter incidence birth_cohort if age == "75-", $connect_options $mlabel || ///
	scatter incidence birth_cohort if age == "80-", $connect_options $mlabel || ///
	scatter incidence birth_cohort if age == "85-", $connect_options $mlabel ///
	title("Trend in cardiometabolic multimorbidity") xtitle("Birth cohort") ytitle("Incidence rate (per 10 000)") ///
	xlabel(1925 "1925" 1930 "1930" 1935 "1935" 1940 "1940" 1945 "1945" 1950 "1950" 1955 "1955" 1960 "1960" 1965 "1965" 1970 "1970" 1975 "1975" 1980 "1980" 1985 "1985" 1990 "1990") ///
	ylabel(0 "0" 50 "50" 100 "100" 150 "150" 200 "200" 250 "250" 300 "300" 350 "350" 400 "400") xscale(titlegap(2)) yscale(titlegap(2)) ///
	legend(off) graphregion(color(white)) 

graph export "D:\Onedrive\00. 博士课题\02-结果\01-课题1\Birth cohort\Trend in incidence multimorbidity.png", replace


**********************************************************************************************************
*************                           Project 1.2.b: incidence rate                           *************
**********************************************************************************************************
/* Healthy status →→→→ first CMD */
use "D:\HanYT\2019-06 Multimorbidity\1. Database\project2.dta",replace
***exclude participants diagnosed directly on multimorbidity;1591
drop if first_cmd_inc_date == mltmbd_inc_date & d_number_inc >1

***generate variable on first manifestation of CMD
tempvar min 
egen `min'    = rowmin(c_ep0003_date c_ep0009_date c_ep0088_date)
gen type_fcmd = 0 if d_number_inc == 0
	replace type_fcmd = 1 if c_ep0088_date == `min' & d_number_inc != 0
	replace type_fcmd = 2 if c_ep0003_date == `min' & d_number_inc != 0
	replace type_fcmd = 3 if c_ep0009_date == `min' & d_number_inc != 0
	label variable type_fcmd "type of first manifestation of CMD"
	label define type_fcmd 0 "healthy" 1 "T2DM" 2 "IHD" 3 "IST"
	label values type_fcmd type_fcmd

***gen outcome variable
gen first_T2DM = 1 if type_fcmd == 1
	replace first_T2DM = 0 if mi(first_T2DM)
gen first_IHD = 1 if type_fcmd == 2
	replace first_IHD = 0 if mi(first_IHD)
gen first_IST = 1 if type_fcmd == 3
	replace first_IST = 0 if mi(first_IST)

***description:age at diagnose
*duration between recruiment and first CMD
gen py_fcmd = (first_cmd_inc_date - study_date)/365.25
*
table type_fcmd, c(mean age_at_fcmd median py_fcmd)
table is_female type_fcmd, c(mean age_at_fcmd median py_fcmd)

***incidence of T2DM IHD IST, respectively
local disease T2DM IHD IST
foreach i of local disease{

	di _dup(30) "*" "`i'" _dup(30) "*"
	stset first_cmd_inc_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(first_`i' == 1)
	strate
	strate is_female

}
/* First CMD →→→→ multimorbidity */
***detect logical error: date of mltmbd later than date of FCMD
list is_female if first_cmd_inc_date > mltmbd_inc_date & d_number_inc >1

***exclusion
drop if mltmbd_inc_date <=first_cmd_inc_date & d_number_inc >0   /* exclude participants died from fcmd or no cmd occurred  */
drop if type_fcmd == 0 

***calculate the incidence；participants are excluded because 
stset mltmbd_inc_date, id(studyid) enter(time first_cmd_inc_date) origin(time dob_anon) scale(365.25) failure(mltmbd_inc == 1)
gen py = _t-_t0

***description
**age at first mbd
gen age_at_fmbd = (mltmbd_inc_date - dob_anon)/365.25
table is_female type_fcmd, c(mean age_at_fmbd median py)

*number of case
bysort type_fcmd: tab mltmbd_inc
bysort is_female type_fcmd: tab mltmbd_inc
***sex-specific incidence
forvalues i = 0/1{
**Model 1: age region
poisson mltmbd_inc i.type_fcmd i.age_strata i.region_code if is_female ==`i', exposure(py)
margins type_fcmd, predict(ir) cformat(%9.5f) 

**Model 2: age region i.highest_education i.household_income i.occupation i.marital_status
poisson mltmbd_inc i.type_fcmd i.age_strata i.region_code i.highest_education i.household_income i.occupation i.marital_status if is_female ==`i', exposure(py)
margins type_fcmd, predict(ir) cformat(%9.5f) 

**Model 3: age region i.highest_education i.household_income i.occupation i.marital_status i.alcohol_5groups i.obesity_5groups i.PA_5groups i.smoking_5groups i.diet_5groups
poisson mltmbd_inc i.type_fcmd i.age_strata i.region_code i.highest_education i.household_income i.occupation i.marital_status ///
	i.alcohol_5groups i.obesity_5groups i.PA_5groups i.smoking_5groups i.diet_5groups if is_female ==`i', exposure(py)
margins type_fcmd, predict(ir) cformat(%9.5f)

}
log close

























