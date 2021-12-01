**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                              Part 1: cleaning data                             *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
log using "D:\HanYT\2019-06 Multimorbidity\3. Result\data cleaning.log",replace
**********************************************************************************************************
****************************            Part 1.1 Basic Preparation    ************************************
**********************************************************************************************************
/* Merge covarites and endpoints */
***Endpoints
import delimited "D:\HanYT\00-原始数据\2020-05 Multimorbidity\endpoints.csv", encoding(utf8) clear
rename csid studyid
*ischaemic heart disease
rename ep_12156_combined_ep c_ep0003
rename ep_12156_combined_datedeveloped c_ep0003_date
*ischaemic stroke
rename ep_12157_combined_ep c_ep0009
rename ep_12157_combined_datedeveloped c_ep0009_date
*diabetes
rename ep_12155_combined_ep c_ep0047
rename ep_12155_combined_datedeveloped c_ep0047_date
rename ep_12161_combined_ep c_ep0048
rename ep_12161_combined_datedeveloped c_ep0048_date
rename ep_12162_combined_ep c_ep0301
rename ep_12162_combined_datedeveloped c_ep0301_date
rename ep_12163_combined_ep c_ep0302
rename ep_12163_combined_datedeveloped c_ep0302_date
*All-cause death
rename ep_12160_da_ep du_ep0001
rename ep_12160_da_datedeveloped du_ep0001_date
*test missing
codebook c_ep0003 c_ep0003_date c_ep0048 c_ep0048_date c_ep0302 c_ep0302_date c_ep0301 c_ep0047 c_ep0047_date c_ep0009 ///
	c_ep0009_date du_ep0001_date du_ep0001
save "D:\HanYT\2019-06 Multimorbidity\1. Database\endpoints.dta", replace

***Baseline
import delimited "D:\HanYT\00-原始数据\2020-05 Multimorbidity\data_baseline_questionnaires.csv", encoding(utf8) clear
rename csid studyid
save "D:\HanYT\2019-06 Multimorbidity\1. Database\baseline.dta", replace

***Merge
use "D:\HanYT\2019-06 Multimorbidity\1. Database\baseline.dta", clear
/* merge 1:1 studyid using "D:\HanYT\2019-06 Multimorbidity\1. Database\endpoints.dta", keep(match) nogen keepusing(du_ep0001 du_ep0001_date ///
	c_ep0003 c_ep0003_date c_ep0048 c_ep0048_date c_ep0302 c_ep0302_date c_ep0301 c_ep0301_date c_ep0047 c_ep0047_date c_ep0009 c_ep0009_date) 
 */
 * 收集所有变量
 merge 1:1 studyid using "D:\HanYT\2019-06 Multimorbidity\1. Database\endpoints.dta", keep(match) nogen

***日期型变量转换
local varlist dob_anon study_date censoring_date c_ep0003_date c_ep0048_date c_ep0302_date c_ep0301_date c_ep0047_date du_ep0001_date c_ep0009_date
foreach var of local varlist{

	gen `var'1 = substr(`var',1,10)
	gen `var'2 = date(`var'1,"YMD",2050)
	drop `var' `var'1
	rename `var'2 `var'
	format `var' %td
	
}

***Generate T2DM using lv's code
gen c_ep0088 = 1 if c_ep0048 == 1
replace c_ep0088 = 1 if c_ep0048 == 0 & c_ep0301 == 1 & (c_ep0047 == 0 & c_ep0302 == 0)
replace c_ep0088 = 0 if c_ep0048 == 0 & c_ep0301 == 1 & (c_ep0047 == 1 | c_ep0302 == 1)
replace c_ep0088 = 0 if c_ep0048 == 0 & c_ep0301 == 0


gen c_ep0088_date = c_ep0048_date if c_ep0048 == 1 & c_ep0301 == 0
replace c_ep0088_date = c_ep0048_date if c_ep0048 == 1 & c_ep0301 == 1 & (c_ep0047 == 0 & c_ep0302 == 0) & (c_ep0048_date <= c_ep0301_date)
replace c_ep0088_date = c_ep0301_date if c_ep0048 == 1 & c_ep0301 == 1 & (c_ep0047 == 0 & c_ep0302 == 0) & (c_ep0048_date > c_ep0301_date)
replace c_ep0088_date = c_ep0048_date if c_ep0048 == 1 & c_ep0301 == 1 & (c_ep0047 == 1 | c_ep0302 == 1)
replace c_ep0088_date = c_ep0301_date if c_ep0048 == 0 & c_ep0301 == 1 & (c_ep0047 == 0 & c_ep0302 == 0)
replace c_ep0088_date = du_ep0001_date if c_ep0088 == 0

format c_ep0088_date %td
save "D:\HanYT\2019-06 Multimorbidity\1. Database\merged_database.dta",replace

**********************************************************************************************************
****************************           Part 1.2 Baseline endpoints    ************************************
**********************************************************************************************************
use "D:\HanYT\2019-06 Multimorbidity\1. Database\merged_database.dta",clear
***Number of diseases
egen d_number_base = rowtotal(has_diabetes chd_diag stroke_or_tia_diag)
	label variable d_number_base "Number of incident diseases at basline"

***Number of disease(3groups)
recode d_number_base (0=0 "None") (1=1 "One") (2/3=2 "Multimorbidity"), gen(d_number2_base)
	label variable d_number2_base "Number of CMD at baseline 3groups"

***Multimorbidity
gen mltmbd_base = 1 if d_number_base >=2
	replace mltmbd_base = 0 if mi(mltmbd_base)
	label variable mltmbd_base "Baseline Multimorbidity"
	label define mltmbd 0 "No Multimorbidity" 1 "Multimorbidity"
	label values mltmbd_base mltmbd

***free from CMD
recode d_number_base (0=0 "free from cmd") (1/3=1 "least one"), gen(free_cmd_base)
	label variable free_cmd_base "Baseline free CMD"	

***Mutual Combination
gen combin_base = 0 if has_diabetes == 0 & chd_diag == 0 & stroke_or_tia_diag == 0
	replace combin_base = 1 if has_diabetes == 1 & chd_diag == 0 & stroke_or_tia_diag == 0
	replace combin_base = 2 if has_diabetes == 0 & chd_diag == 1 & stroke_or_tia_diag == 0
	replace combin_base = 3 if has_diabetes == 0 & chd_diag == 0 & stroke_or_tia_diag == 1
	replace combin_base = 4 if has_diabetes == 1 & chd_diag == 1 & stroke_or_tia_diag == 0
	replace combin_base = 5 if has_diabetes == 1 & chd_diag == 0 & stroke_or_tia_diag == 1
	replace combin_base = 6 if has_diabetes == 0 & chd_diag == 1 & stroke_or_tia_diag == 1
	replace combin_base = 7 if has_diabetes == 1 & chd_diag == 1 & stroke_or_tia_diag == 1
	label variable combin_base "Baseline Combination"
	label define baseline_combination 0 "healthy" 1 "only T2D" 2 "only CHD" 3 "only ST" 4 "T2D & CHD" ///
		5 "T2D & ST" 6 "CHD & ST" 7 "T2D & CHD & ST"
	label values combin_base baseline_combination


**********************************************************************************************************
****************************           Part 1.3 incidnet endpoints    ************************************
**********************************************************************************************************
/* Baseline disease status */
label variable has_diabetes "Basline diabetes"
label variable chd_diag "Basline CHD"
label variable stroke_or_tia_diag "Baseline stroke"
label variable has_copd "Baseline COPD"
label variable cancer_diag "Baseline cancer"
label variable rheum_heart_dis_diag "Baseline rheumatic heart disease"
label variable kidney_dis_diag "Baseline chronic kidney disease"
label define baseline_disease 0 "None" 1 "Has"
label values has_diabetes chd_diag stroke_or_tia_diag has_copd cancer_diag ///
		rheum_heart_dis_diag kidney_dis_diag hypertension_diag baseline_disease
***hypertension
gen has_hypertension = 1 if hypertension_diag == 1 | sbp_mean >= 140 | dbp_mean >= 90 | used_blood_pressure_drugs == 1
replace has_hypertension = 0 if hypertension_diag == 0 & sbp_mean < 140 & dbp_mean < 90 & used_blood_pressure_drugs == 0
label variable has_hypertension "Baseline hypertension"
label define yesno 1 "yes" 0 "no"
label values has_hypertension yesno

***number of disease(4groups)
egen d_number_inc = rowtotal(c_ep0003 c_ep0009 c_ep0088)
	label variable d_number_inc "Number of incidence diseases under observation"

***Number of disease(3groups)
recode d_number_inc (0=0 "None") (1=1 "One") (2/3=2 "Multimorbidity"), gen(d_number2_inc)
	label variable d_number2_inc "Number of incidence CMD under observation 3groups"

***Multimorbidity(2groups)
recode d_number_inc (0/1=0 "None") (2/3=1 "multimorbidity"), gen(mltmbd_inc)
	label variable mltmbd_inc "Incidence Multimorbidity"
	label values mltmbd_inc mltmbd
egen mltmbd_inc_date = rowmedian(c_ep0003_date c_ep0009_date c_ep0088_date) if mltmbd_inc == 1
	replace mltmbd_inc_date = max(c_ep0003_date, c_ep0009_date, c_ep0088_date) if mltmbd_inc == 0
	format mltmbd_inc_date %td
	label variable mltmbd_inc_date "Date of Multimorbidity"

/* *check
forvalues n = 0/3 {

	preserve
		keep if d_number_inc == `n'
		list c_ep0003 c_ep0003_date c_ep0009 c_ep0009_date c_ep0088 c_ep0088_date mltmbd_inc mltmbd_inc_date in 1/50
	restore
	
}
 */

***First cardiomebolic disease (不区分首发疾病类型)
recode d_number_inc (0=0 "None") (1/3=1 "1th CMD"), gen(first_cmd_inc)
	label variable first_cmd_inc "First cardiomebolic disease"
egen first_cmd_inc_date = rowmin(c_ep0003_date c_ep0009_date c_ep0088_date)
	label variable first_cmd_inc_date "Date of first cardiomebolic disease"
	format first_cmd_inc_date %td
**age at the first manifestation of CMD
gen age_at_fcmd = (first_cmd_inc_date - dob_anon)/365.25

***区分首发类型待定；于Project 1中设定


***Manifestation of Combination 
**recode differences
egen diff = diff(c_ep0003_date c_ep0009_date c_ep0088_date)
	label variable diff "three dates of diseases"
**recode three time point
egen min    = rowmin(c_ep0003_date c_ep0009_date c_ep0088_date)
egen median = rowmedian(c_ep0003_date c_ep0009_date c_ep0088_date)
egen max    = rowmax(c_ep0003_date c_ep0009_date c_ep0088_date)
**occuring <3 diseases is easy to handle
egen diff1 = diff(c_ep0003_date c_ep0009_date) if d_number_inc == 3
egen diff2 = diff(c_ep0003_date c_ep0088_date) if d_number_inc == 3
egen diff3 = diff(c_ep0088_date c_ep0009_date) if d_number_inc == 3    /* 仅发生三种疾病者有该值 */
gen three_type = 2 if diff1 == 1 & diff2 == 1 & diff3 == 1		/* 三个疾病发病时间都不一样 */
	replace three_type = 1 if diff1 == 0 | diff2 == 0 | diff3 == 0    /* 相同时间在前还是在后 112  122*/
	replace three_type = 0 if diff1 == 0 & diff2 == 0 & diff3 == 0	/* 因为前面限制在发生三种疾病的人，所以本行表示三个疾病同时发生 */
	label variable three_type "differences among dates of diseases for three inc disease"
	label define three_type 0 "three disease:three occured sametime" 1 "three disease:two occured sametime" 2 "three disease:none occured sametime"
	label values three_type three_type

**Generate incident combination (variable, three_type, indicating occured three diseases)
*Method 1
gen combin_inc = 0 if d_number_inc == 0
	replace combin_inc = 1 if d_number_inc == 1 
	replace combin_inc = 2 if (c_ep0088 == 1 & c_ep0009 == 1 & d_number_inc == 2) | ///
			(d_number_inc == 3 & three_type == 1 & c_ep0003_date == max ) | ///
			(d_number_inc == 3 & three_type == 2 & c_ep0003_date == max)
	replace combin_inc = 3 if (c_ep0088 == 1 & c_ep0003 == 1 & d_number_inc == 2) | ///
			(d_number_inc == 3 & three_type == 1 & c_ep0009_date == max ) | ///
			(d_number_inc == 3 & three_type == 2 & c_ep0009_date == max)
	replace combin_inc = 4 if (c_ep0009 == 1 & c_ep0003 == 1 & d_number_inc == 2) | ///
			(d_number_inc == 3 & three_type == 1 & c_ep0088_date == max ) | ///
			(d_number_inc == 3 & three_type == 2 & c_ep0088_date == max)
	replace combin_inc = 5 if (three_type == 1 & max == median) | (three_type == 0)     /* T2D ST CHD */
	label define combin_inc 0 "non incident" 1 "one disease" 2 "T2D & ST" 3 "T2D & IHD" 4 "ST & IHD" 5 "T2D ST CHD"
	label values combin_inc combin_inc

drop min median max diff1 diff2 diff3

*Method 2: validate method 1, yielding same results
/* gen combin_inc1 = 0 if d_number_inc == 0
	replace combin_inc1 = 1 if d_number_inc == 1
	replace combin_inc1 = 2 if (c_ep0088 == 1 & c_ep0009 == 1 & d_number_inc == 2) | ///
			(d_number_inc == 3 & c_ep0003_date > c_ep0088_date & c_ep0003_date > c_ep0009_date)
	replace combin_inc1 = 3 if (c_ep0088 == 1 & c_ep0003 == 1 & d_number_inc == 2) | ///
			(d_number_inc == 3 & c_ep0009_date > c_ep0088_date & c_ep0009_date > c_ep0003_date)
	replace combin_inc1 = 4 if (c_ep0009 == 1 & c_ep0003 == 1 & d_number_inc == 2) | ///
			(d_number_inc == 3 & c_ep0088_date > c_ep0009_date & c_ep0088_date > c_ep0003_date)
	replace combin_inc1 = 5 if (three_type == 1 & max == median) | (three_type == 0)     /* T2D ST CHD */
	label define combin_inc1 0 "non incident" 1 "one disease" 2 "T2D & ST" 3 "T2D & IHD" 4 "ST & IHD" 5 "T2D ST CHD"
	label values combin_inc1 combin_inc */


**********************************************************************************************************
****************************             Part 1.4 end endpoints     **************************************
**********************************************************************************************************
***generate status of diseases at the end of follow-up
gen t2d_end = has_diabetes + c_ep0088         /* 是否可以考虑随访期间的不分亚型的糖尿病 */
	replace t2d_end = 1 if t2d_end >1
	label variable t2d_end "End T2D"
gen st_end = stroke_or_tia_diag + c_ep0009    /* 基线是全部脑卒中，之后仅有缺血的，有点奇怪 */
	replace st_end = 1 if st_end >1
	label variable st_end "End ST"
gen chd_end = chd_diag + c_ep0003
	replace chd_end = 1 if chd_end >1
	label variable chd_end "End CHD"
label define end_disease 0 "None" 1 "Has_disease"
label values t2d_end st_end chd_end end_disease
****number of CMD
egen d_number_end = rowtotal(t2d_end chd_end st_end)
	label variable d_number_end "Number of diseases at end"

***Number of disease(3groups)
recode d_number_end (0=0 "None") (1=1 "One") (2/3=2 "Multimorbidity"), gen(d_number2_end)
	label variable d_number2_end "Number of CMD at end 3groups"

***Multimorbidity
gen mltmbd_end = 1 if d_number_end >=2
	replace mltmbd_end = 0 if mi(mltmbd_end)
	label variable mltmbd_inc "End Multimorbidity"

***free from CMD
recode d_number_end (0=1 "free from cmd") (1/3=0 "least one"), gen(free_cmd_end)
	label variable free_cmd_end "End free cmd "	

***Mutual Combination
gen combin_end = 0 if t2d_end == 0 & chd_end == 0 & st_end == 0
	replace combin_end = 1 if t2d_end == 1 & chd_end == 0 & st_end == 0
	replace combin_end = 2 if t2d_end == 0 & chd_end == 1 & st_end == 0
	replace combin_end = 3 if t2d_end == 0 & chd_end == 0 & st_end == 1
	replace combin_end = 4 if t2d_end == 1 & chd_end == 1 & st_end == 0
	replace combin_end = 5 if t2d_end == 1 & chd_end == 0 & st_end == 1
	replace combin_end = 6 if t2d_end == 0 & chd_end == 1 & st_end == 1
	replace combin_end = 7 if t2d_end == 1 & chd_end == 1 & st_end == 1
	label values combin_end baseline_combination


**********************************************************************************************************
****************************            Part 1.5 Covariates         **************************************
**********************************************************************************************************
/* Sociodemographic variables */
***residence
label variable region_is_urban residence
label define residence 0 "Rural" 1 "Urban"
label values region_is_urban residence

***education
label variable highest_education "highest education"
label define highest_education 0 "no formal school" 1 "primary school" 2 "middle school" 3 "high school" 4 "technical school" 5 "university"
label values highest_education highest_education
recode highest_education (0/1=0 "Illiterate and primary school") (2/3=1 "Middle and high school") (4/5=2 "College/university"), gen(education_3groups)
    label variable education_3groups "highest education three groups"
recode highest_education (0/1=0 "Illiterate and primary school") (2/max=1 "Middle and high school") , gen(education_2groups)
    label variable education_3groups "highest education two groups"

***household income
label variable household_income "household income"
label define household_income 0 "<2500yuan" 1 "2500-4999yuan" 2 "5000-9999yuan" 3 "10000-19999yuan" 4 "20000-34999yuan" 5 ">=35000yuan"
label values household_income household_income
recode household_income (0/2=0 "<10000/year") (3=1 "10000-19999/year") (4/5=2 ">20000/year"), gen(income_3groups)
	label variable income_3groups "Income three groups"

***occupation
label define occupation 0 "agriculture worker" 1 "factory worker" 2 "administrator" 3"professional" 4 "service workers" 5"retired" 6"house wife/husband" 7"self employed" 8"unemployed" 9"other"
label values occupation occupation

***marriage
label variable marital_status "marital status"
label define marital_status 0 "married"  1 "widowed" 2 "separated/divorced" 3 "never married"
label values marital_status marital_status
recode marital_status (0=0 "married") (1/3=1 "unmarried"), gen(marital_status_2groups)
	label variable marital_status_2groups "marital status two groups"

***age at baseline
gen age_at_study_date = age_at_study_date_x100/100
	label variable age_at_study_date "age at study date"
egen age_strata=cut(age_at_study_date), at(30(5)80) label
	tab age_strata
egen age_3groups = cut(age_at_study_date), at(0 50 60 100) icodes
	label variable age_3groups "age groups at baseline"
	label define age_3groups 0 "<50 y" 1 "50~59 y" 2 ">=60 yr"
	label values age_3groups age_3groups	

***age at the end of follow-up & date variable setup
gen age_at_end = (du_ep0001_date - dob_anon)/365.25
	label variable age_at_end "age at end of follow-up" 
egen age_end_3groups = cut(age_at_end), at(0 50 60 100) icodes
	label variable age_end_3groups "age groups at the end"
	label values age_end_3groups age_3groups


/* Family history */
***diabetes family history
gen diabetes_fh = 1 if mother_diabetes == 1 | father_diabetes == 1 | (siblings_diabetes >= 1 & siblings_diabetes != .)
	replace diabetes_fh = 0 if (mother_diabetes == 0 & father_diabetes == 0 & siblings_diabetes == 0)|(mother_diabetes == 0 & father_diabetes == 0 & siblings == 0)
	replace diabetes_fh = 2 if diabetes_fh == .
	label variable diabetes_fh "Family history: diabetes"
	label define diabetes_fh 2 "Don't know" 1 "Yes" 0 "No"
	label values diabetes_fh diabetes_fh
recode diabetes_fh (0 2=0 "No") (1=1 "Yes"), gen(diabetes_fh_2groups)
	label variable diabetes_fh_2groups "Diabetes family history 2groups"

***stroke family history
gen stroke_fh = 1 if mother_stroke == 1 | father_stroke == 1 | (siblings_stroke >= 1 & siblings_stroke != .)
	replace stroke_fh = 0 if (mother_stroke == 0 & father_stroke == 0 & siblings_stroke == 0)|(mother_stroke == 0 & father_stroke == 0 & siblings == 0)
	replace stroke_fh = 2 if stroke_fh == .
label variable stroke_fh "Family history: stroke"
label define stroke_fh 2 "Don't know" 1 "Yes" 0 "No"
label values stroke_fh stroke_fh
recode stroke_fh (0 2=0 "No") (1=1 "Yes"), gen(stoke_fh_2groups)
	label variable stoke_fh_2groups "Stroke family history 2groups"

***heart attack family history
gen heart_attack_fh = 1 if mother_heart_attack == 1 | father_heart_attack == 1 | (siblings_heart_attack >= 1 & siblings_heart_attack != .)
	replace heart_attack_fh = 0 if (mother_heart_attack == 0 & father_heart_attack == 0 & siblings_heart_attack == 0)|(mother_heart_attack == 0 & father_heart_attack == 0 & siblings == 0)
	replace heart_attack_fh = 2 if heart_attack_fh == .
label variable heart_attack_fh "Family history: heart attack"
label define heart_attack_fh 2 "Don't know" 1 "Yes" 0 "No"
label values heart_attack_fh heart_attack_fh
recode heart_attack_fh (0 2=0 "No") (1=1 "Yes"), gen(chd_fh_2groups)
	label variable chd_fh_2groups "heart attack family history 2groups"

/* Parental history of cardiomebolic multimorbidity(定义方式可能需要改变) */
***双亲家族史
egen mother_multimbd_fh = rowtotal(mother_stroke mother_diabetes mother_heart_attack)
egen father_multimbd_fh = rowtotal(father_stroke father_diabetes father_heart_attack)
gen parental_fh_4groups = 2 if mother_multimbd_fh > 1 | father_multimbd_fh > 1
	replace parental_fh_4groups = 3 if mother_multimbd_fh > 1 & father_multimbd_fh > 1
	replace parental_fh_4groups = 0 if mother_multimbd_fh == 0 & father_multimbd_fh == 0
	replace parental_fh_4groups = 1 if mi(parental_fh_4groups)
	label variable parental_fh_4groups "parental history of cardiomebolic multimorbidity"
	label define parental_fh_4groups 0 "None" 1 "no multimbd has fh" 2 "only one multimbd" 3 "two multimormbd"
	label values parental_fh_4groups parental_fh_4groups
clonevar parental_fh_3groups = parental_fh_4groups
	replace parental_fh_3groups = 2 if parental_fh_3groups == 3
	label variable parental_fh_3groups "parental history of cardiomebolic multimorbidity"
	label define parental_fh_3groups 0 "None" 1 "no multimbd has fh" 2 "least one multimbd"
	label values parental_fh_3groups parental_fh_3groups
recode parental_fh_3groups (0=0 "No") (1/2=1 "Yes"), gen(parental_fh_2groups)
	label variable parental_fh_2groups "Parental family history of CMD 2groups"
recode parental_fh_3groups (0/1=0 "No") (2=1 "Yes"), gen(parental_fh_cmm_2groups)
	label variable parental_fh_2groups "Parental family history of CMM 2groups"

***慢性病家族史（二分类：有或无）
gen chronic_fh = 1 if stroke_fh == 1 | heart_attack_fh == 1 | diabetes_fh == 1
	replace chronic_fh = 0 if stroke_fh == 0 & heart_attack_fh == 0 & diabetes_fh == 0
	replace chronic_fh = 2 if chronic_fh == .
label variable chronic_fh "Chronic disease family history"
label define chronic_fh 2 "Don't know" 1 "Yes" 0 "No"
label values chronic_fh chronic_fh
recode chronic_fh (0 2=0 "No") (1=1 "Yes"), gen(chronic_fh_2groups)
	label variable chronic_fh_2groups "Chronic family history 2groups"

***共病家族史（无，仅有单病家族史，不仅只有一种疾病的家族史）
egen mltmbd_fh = rowtotal(diabetes_fh_2groups stoke_fh_2groups chd_fh_2groups)
recode mltmbd_fh (0=0 "None") (1=1 "Single disease") (2/max=2 "more than one disease") , gen(mltmbd_fh_3groups)
	label variable mltmbd_fh "family history of multimbd three groups"
recode mltmbd_fh (0/1=0 "None/one") (2/max=1 "more than one disease") , gen(mltmbd_fh_2groups)
	label variable mltmbd_fh "family history of multimbd two groups"

/* 女性绝经史 */
recode menopause_status (. 0 1 = 1 "premenopause") (2 = 0 "postmenopause"), gen(menopause_2groups)
	replace menopause_2groups = . if is_female == 0

/* 高血脂 */
gen has_hyperlipidemia = (taking_statins == 1)
	replace has_hyperlipidemia = 0 if mi(has_hyperlipidemia)
/* ***detect missing values
codebook d_number_base mltmbd_base free_cmd_base combin_base d_number_inc mltmbd_inc mltmbd_inc_date ///
	first_cmd_inc first_cmd_inc_date combin_inc t2d_end st_end chd_end d_number_end mltmbd_end free_cmd_end ///
	combin_end education_3groups income_3groups age_at_study_date age_3groups age_strata diabetes_fh stroke_fh ///
	heart_attack_fh parental_fh_4groups parental_fh_3groups chronic_fh mltmbd_fh */


**********************************************************************************************************
****************************            Part 1.6 Lifestyles         **************************************
**********************************************************************************************************
/* traditional variables */
***smoking
gen smoking_5groups = 1 if smoking_category == 1 | smoking_category == 2     
	replace smoking_5groups = 2 if smoking_category == 3 & (smoking_stopped_reason > 0 & smoking_stopped_reason <= 4)
	replace smoking_5groups = 3 if (smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day < 15.00
	replace smoking_5groups = 4 if (smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day >= 15.00 & cig_equiv_day < 25.00
	replace smoking_5groups = 5 if (smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day >= 25.00
recode smoking_category (1=0 "never smoker")  (2/5=1 "ever smoker"), gen (smoking_2groups)
recode smoking_5groups (1/2=1 "healthy")  (3/5=0 "not healthy"), gen (healthy_smoking)
	label variable healthy_smoking "healthy smoking"

***alcohol
*2groups
codebook alcohol_category   /* 无缺失 */
gen healthy_alcohol = 0 if alcohol_category == 6 & alc_weekly == 2 &  total_alc_typ_day_g >=30 & is_female == 0
	replace healthy_alcohol = 0 if alcohol_category == 6 & alc_weekly == 2 &  total_alc_typ_day_g >=15 & is_female == 1
	replace healthy_alcohol = 1 if mi(healthy_alcohol)
label variable healthy_alcohol "Healthy alcohol"
label define healthy_alcohol 1 "Healthy: M<30/F<15 g/d" 0 "Unhealthy"
label values healthy_alcohol healthy_alcohol
*5groups
gen alcohol_7groups = 1 if alcohol_category == 1 | alcohol_category == 3 | alcohol_category == 4
	replace alcohol_7groups = 2 if alcohol_category == 2 | alcohol_category == 5
	replace alcohol_7groups = 3 if alcohol_category == 6 & (alc_weekly == 0 | alc_weekly == 1)
	replace alcohol_7groups = 4 if alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g < 15.00
	replace alcohol_7groups = 5 if alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 15.00 & total_alc_typ_day_g < 30.00)
	replace alcohol_7groups = 6 if alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 30.00 & total_alc_typ_day_g < 60.00)
	replace alcohol_7groups = 7 if alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g >= 60.00
recode alcohol_7groups (1/3=1 "Not daily") (4=2 "Daily 1-14g/d") (5=3 "Daily 15-29g/d") (6=4 "Daily 30-59g/d") (7=5 "Daily >=60g/d"), gen(alcohol_5groups)
recode alcohol_7groups (1=1 "No drinker") (2=2 "quit drinking") (3=3 "weekly drinker") (4/5=4 "daily <30g/d") (6/7=5 "daily >=30g/d"),gen(alcohol_5groups1)
recode alcohol_7groups (1 2 3 6 7=1 "alcohol abstension or heavy drinking") (4 5=2 "daily >0 <30g/d"),gen(healthy_alcohol1)
recode alcohol_7groups (1 2 6 7=1 "No drinker") (3 4 5=2 "weekly & daily >0 <30g/d"),gen(healthy_alcohol2)
recode alcohol_7groups (2 6 7=0 "quit & heavy drinking") (1 3 4 5=1 "None & weekly & daily >0 <30g/d"),gen(healthy_alcohol3)
***diet
/* *需要单独分析饮食指标，在理论基础上选择有意义的；暂按照楠波师兄定义10-25
gen diet_component1 = 1 if diet_freq_fresh_veg == 0
	replace diet_component1 = 0 if mi(diet_component1)
gen diet_component2 = 1 if diet_freq_fresh_fruit == 0
	replace diet_component2 = 0 if mi(diet_component2)
gen diet_component3 = 1 if diet_freq_meat > 0 & diet_freq_meat < 4
	replace diet_component3 = 0 if mi(diet_component3)
gen diet_component4 = 1 if diet_freq_soybean <= 1
	replace diet_component4 = 0 if mi(diet_component4)
egen diet_5score = rowtotal(diet_component1 diet_component2 diet_component3 diet_component4)
tab diet_5score
drop diet_component*
clonevar diet_5groups = diet_5score
	label variable diet_5groups "Diet 5groups"
gen healthy_diet = 1 if diet_5score == 4
	replace healthy_diet = 0 if mi(healthy_diet)
label define healthy_diet 1 "Healthy diet" 0 "Unhealthy diet"
label values healthy_diet healthy_diet */

* 新分析中分析了饮食与6种结局之间的关联，决定纳入红肉、蛋类、新鲜蔬菜、新鲜水果  05-09
gen diet_component1 = 1 if diet_freq_fresh_veg == 0
	replace diet_component1 = 0 if mi(diet_component1)
	label variable diet_component1 "Daily eating vegetable"
	label values diet_component1 yesno
gen diet_component2 = 1 if diet_freq_fresh_fruit == 0
	replace diet_component2 = 0 if mi(diet_component2)
	label variable diet_component2 "Daily eating fruit"
	label values diet_component2 yesno
gen diet_component3 = 1 if diet_freq_meat > 0 & diet_freq_meat < 4
	replace diet_component3 = 0 if mi(diet_component3)
	label variable diet_component3 "weekly not daily meat"
	label values diet_component3 yesno
gen diet_component4 = 1 if diet_freq_eggs == 0 
	replace diet_component4 = 0 if mi(diet_component4)
	label variable diet_component4 "daily eating egg"
	label values diet_component4 yesno
egen diet_5score = rowtotal(diet_component1 diet_component2 diet_component3 diet_component4)
tab diet_5score
clonevar diet_5groups = diet_5score
	label variable diet_5groups "Diet 5groups"
gen healthy_diet = 1 if diet_5score == 4
	replace healthy_diet = 0 if mi(healthy_diet)
label variable healthy_diet "Healthy diet"
label define healthy_diet 1 "Healthy diet" 0 "Unhealthy diet"
label values healthy_diet healthy_diet


***obesity
** BMI
recode bmi_calc (min/18.499=1 "Underweight <18.5") (18.5/23.999=2 "Normal <24.0") (24.0/27.9=3 "Overweight <28.0") (28.0/60.0=4 "Obesity >=28.0"), gen(bmi_4groups)
recode bmi_4groups (2=1 "Healthy BMI") (1 3 4=0 "Unhealthy BMI"), gen(healthy_bmi)
	label variable healthy_bmi "Healthy BMI"
* cutoff 28
recode bmi_4groups (2/3=1 "normal&overweight") (1 4=0 "Underweight&obesity"), gen(healthy_bmi2)
* 9 groups
egen bmi_9groups = cut(bmi_calc), at(0 18.5 20.5 22.5 24.0 26.0 28.0 30.0 35.0 99) label

***waist
gen WC_3groups = 1 if is_female == 0 & waist_mm < 850
	replace WC_3groups = 1 if is_female == 1 & waist_mm < 800
	replace WC_3groups = 2 if is_female == 0 & waist_mm >= 850
	replace WC_3groups = 2 if is_female == 1 & waist_mm >= 800
	replace WC_3groups = 3 if is_female == 0 & waist_mm >= 900
	replace WC_3groups = 3 if is_female == 1 & waist_mm >= 850
	label variable WC_3groups "Central obesity based on WC: men-85/90; women-80/85"
label define WC_3groups 1 "Non-central obesity <85/80" 2 "Pre-central obesity >=85/80" 3 "Central obesity >=90/85"
label values WC_3groups WC_3groups
recode WC_3groups (1 2=1 "Healthy WC") (3=0 "Unealthy WC"),gen(healthy_WC) 
	label variable healthy_WC "Healthy WC"
recode WC_3groups (1=1 "Healthy WC") (2 3=0 "Unealthy WC"),gen(healthy_WC2)
recode waist_mm (min/849.99 = 1 "Healthy WC 85") (850/max = 0 "Unealthy WC 85"), gen(healthy_WC3)
** 8 groups
egen WC_8groups = cut(waist_mm), at(0 650 700 750 800 850 900 950 5000) label

gen obesity_5groups = 1 if bmi_4groups == 1
	replace obesity_5groups = 2 if (bmi_4groups == 2 | bmi_4groups == 3) & WC_3groups < 3
	replace obesity_5groups = 3 if (bmi_4groups == 2 | bmi_4groups == 3) & WC_3groups == 3
	replace obesity_5groups = 4 if bmi_4groups == 4 & WC_3groups < 3
	replace obesity_5groups = 5 if bmi_4groups == 4 & WC_3groups == 3
	label variable obesity_5groups "Obesity based on BMI & WC"
label define obesity_5groups 1 "Underweight" 2 "Normal/overweight with normal WC" ///
	3 "Normal/overweight with central obesity" 4 "General obesity with normal WC" ///
	5 "General & central obesity"
label values obesity_5groups obesity_5groups

gen healthy_obesity = 1 if obesity_5groups == 2
	replace healthy_obesity = 0 if healthy_obesity == .
	label variable healthy_obesity "healthy obesity"
	label define healthy_obesity 1 "Normal & overweight/normal obesity" 0 "Underweight or obesity"
	label values healthy_obesity healthy_obesity

** 10/11/2020 WC 80/85
gen obesity_5groups2 = 1 if bmi_4groups == 1
	replace obesity_5groups2 = 2 if (bmi_4groups == 2 | bmi_4groups == 3) & WC_3groups < 2
	replace obesity_5groups2 = 3 if (bmi_4groups == 2 | bmi_4groups == 3) & WC_3groups == 2
	replace obesity_5groups2 = 4 if bmi_4groups == 4 & WC_3groups < 2
	replace obesity_5groups2 = 5 if bmi_4groups == 4 & WC_3groups == 2
	label variable obesity_5groups2 "Obesity based on BMI & WC"
label define obesity_5groups2 1 "Underweight" 2 "Normal/overweight with WC <80/85" ///
	3 "Normal/overweight with WC>=80/85" 4 "General obesity with WC <80/85" ///
	5 "General & WC>=80/85"
label values obesity_5groups2 obesity_5groups2

gen healthy_obesity2 = 1 if obesity_5groups2 == 2
	replace healthy_obesity2 = 0 if healthy_obesity2 == .
	label variable healthy_obesity2 "healthy obesity"
	label define healthy_obesity2 1 "Normal & overweight/normal obesity" 0 "Underweight or obesity"
	label values healthy_obesity2 healthy_obesity2

/* Novel lifestyles */
sum sleep_hours, detail
***sleep duration
recode sleep_hours (min/6=1 "<=6h") (7=2 "7h") (8=3 "8h") (9=4 "9h") (10/max=5 ">=10h"), gen(sleep_5groups)
	label variable sleep_5groups "sleep duration 5groups"
gen healthy_sleep = 1 if sleep_5groups == 2 | sleep_5groups == 3 | sleep_5groups == 4
	replace healthy_sleep = 0 if mi(healthy_sleep)
	label variable healthy_sleep "healthy sleep"
	label define healthy_sleep 1 "Healthy:7-9h" 0 "Unhealthy" 
	label values healthy_sleep healthy_sleep
tab1 sleep_5groups healthy_sleep

***leisure sitting time
/* recode tv_reading_hours (min/1.99=1 "<2") (2/3.99=2 "2-3.99h") (3/5.99=3 "3-5.99h") (6/7.99=4 "6-7.99h") (8/max=5 ">=8h"), gen(sitting_5groups)
	label variable sitting_5groups "sitting hours 5groups"
gen healthy_sitting = 1 if sitting_5groups == 1 | sitting_5groups ==2
	replace healthy_sitting = 0 if mi(healthy_sitting)
	label variable healthy_sitting "healthy sitting"
	label define healthy_sitting 1 "Healthy:<4h" 0 "Unhealthy"
	label values healthy_sitting healthy_sitting
tab1 sitting_5groups healthy_sitting */

/* ***detect missing values
codebook smoking_5groups smoking_2groups healthy_smoking alcohol_7groups alcohol_5groups healthy_alcohol ///
	diet_5score healthy_diet WC_3groups obesity_5groups healthy_obesity sleep_5groups healthy_sleep */

save "D:\HanYT\2019-06 Multimorbidity\1. Database\basic database.dta",replace


**********************************************************************************************************
****************************           Part 1.7 gen databases       **************************************
**********************************************************************************************************

/* 课题1.1：仅排除BMI缺失和入组即失访者 */
use "D:\HanYT\2019-06 Multimorbidity\1. Database\basic database.dta", clear
drop if mi(bmi_calc)
drop if study_date == du_ep0001_date

***Physical activity
*2groups
xtile healthy_PA1 = met if is_female == 0 & age_3groups == 0, nq(2)
xtile healthy_PA2 = met if is_female == 0 & age_3groups == 1, nq(2)
xtile healthy_PA3 = met if is_female == 0 & age_3groups == 2, nq(2)
xtile healthy_PA4 = met if is_female == 1 & age_3groups == 0, nq(2)
xtile healthy_PA5 = met if is_female == 1 & age_3groups == 1, nq(2)
xtile healthy_PA6 = met if is_female == 1 & age_3groups == 2, nq(2)
egen healthy_PA = rowmin(healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6)
		 replace healthy_PA = healthy_PA - 1
	label variable healthy_PA "healthy PA"
	label define healthy_PA 1 "healthy PA:higher half" 0 "healthy PA:lower half"
	label values healthy_PA healthy_PA
drop healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6
*5groups
xtile PA_5groups1 = met if is_female == 0 & age_3groups == 0, nq(5)
xtile PA_5groups2 = met if is_female == 0 & age_3groups == 1, nq(5)
xtile PA_5groups3 = met if is_female == 0 & age_3groups == 2, nq(5)
xtile PA_5groups4 = met if is_female == 1 & age_3groups == 0, nq(5)
xtile PA_5groups5 = met if is_female == 1 & age_3groups == 1, nq(5)
xtile PA_5groups6 = met if is_female == 1 & age_3groups == 2, nq(5)
egen PA_5groups = rowmax(PA_5groups1 PA_5groups2 PA_5groups3 PA_5groups4 PA_5groups5 PA_5groups6)
	labe variable PA_5groups
drop PA_5groups1 PA_5groups2 PA_5groups3 PA_5groups4 PA_5groups5 PA_5groups6
***Numbers of healthy lifestyle
egen healthy_score = rowtotal(healthy_smoking healthy_alcohol healthy_PA healthy_diet healthy_bmi healthy_WC healthy_sleep)
recode healthy_score (0/2=0) (3=1) (4=2) (5=3) (6/7=4), gen(healthy_score_5groups)

* Risky lifestyle
gen risky_smoking = (healthy_smoking == 0)
gen risky_alcohol = (healthy_alcohol == 0)
gen risky_diet = (healthy_diet == 0)
gen risky_PA = (healthy_PA == 0)
gen risky_bmi = (healthy_bmi == 0)
gen risky_WC = (healthy_WC == 0)
gen risky_sleep = (healthy_sleep == 0)
/* gen risky_sitting = (healthy_sitting == 0) */
cap drop __00*
save "D:\HanYT\2019-06 Multimorbidity\1. Database\project1.dta",replace

/* 课题1.2：仅排除BMI缺失、入组即失访者和基线1型糖尿病 */
******额外剔除1型糖尿病
use "D:\HanYT\2019-06 Multimorbidity\1. Database\basic database.dta", clear
drop if mi(bmi_calc)
drop if study_date == du_ep0001_date
drop if taking_insulin ==1 & diabetes_diag_age <= 20   /*  diabetic, using insuline and diagnosed <=20y, was defined type1 */

***Physical activity
*2groups
xtile healthy_PA1 = met if is_female == 0 & age_3groups == 0, nq(2)
xtile healthy_PA2 = met if is_female == 0 & age_3groups == 1, nq(2)
xtile healthy_PA3 = met if is_female == 0 & age_3groups == 2, nq(2)
xtile healthy_PA4 = met if is_female == 1 & age_3groups == 0, nq(2)
xtile healthy_PA5 = met if is_female == 1 & age_3groups == 1, nq(2)
xtile healthy_PA6 = met if is_female == 1 & age_3groups == 2, nq(2)
egen healthy_PA = rowmin(healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6)
		 replace healthy_PA = healthy_PA - 1
	label variable healthy_PA "healthy PA"
	label define healthy_PA 1 "healthy PA:higher half" 0 "healthy PA:lower half"
	label values healthy_PA healthy_PA
drop healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6
*5groups
xtile PA_5groups1 = met if is_female == 0 & age_3groups == 0, nq(5)
xtile PA_5groups2 = met if is_female == 0 & age_3groups == 1, nq(5)
xtile PA_5groups3 = met if is_female == 0 & age_3groups == 2, nq(5)
xtile PA_5groups4 = met if is_female == 1 & age_3groups == 0, nq(5)
xtile PA_5groups5 = met if is_female == 1 & age_3groups == 1, nq(5)
xtile PA_5groups6 = met if is_female == 1 & age_3groups == 2, nq(5)
egen PA_5groups = rowmax(PA_5groups1 PA_5groups2 PA_5groups3 PA_5groups4 PA_5groups5 PA_5groups6)
	labe variable PA_5groups
drop PA_5groups1 PA_5groups2 PA_5groups3 PA_5groups4 PA_5groups5 PA_5groups6
***Numbers of healthy lifestyle
egen healthy_score = rowtotal(healthy_smoking healthy_alcohol healthy_PA healthy_diet healthy_bmi healthy_WC healthy_sleep)
recode healthy_score (0/2=0) (3=1) (4=2) (5=3) (6/7=4), gen(healthy_score_5groups)

* Risky lifestyle
gen risky_smoking = (healthy_smoking == 0)
gen risky_alcohol = (healthy_alcohol == 0)
gen risky_diet = (healthy_diet == 0)
gen risky_PA = (healthy_PA == 0)
gen risky_bmi = (healthy_bmi == 0)
gen risky_WC = (healthy_WC == 0)
gen risky_sleep = (healthy_sleep == 0)
cap drop __00*
***仅2型糖尿病
save "D:\HanYT\2019-06 Multimorbidity\1. Database\project1_T2D.dta",replace


/* 课题2： */
******Exclusion
use "D:\HanYT\2019-06 Multimorbidity\1. Database\basic database.dta", clear
drop if mi(bmi_calc)
drop if study_date == du_ep0001_date    /* 0509 入组即失访者 */
drop if chd_diag == 1 | stroke_or_tia_diag == 1| cancer_diag == 1 | has_diabetes == 1

***Physical activity
*2groups
xtile healthy_PA1 = met if is_female == 0 & age_3groups == 0, nq(2)
xtile healthy_PA2 = met if is_female == 0 & age_3groups == 1, nq(2)
xtile healthy_PA3 = met if is_female == 0 & age_3groups == 2, nq(2)
xtile healthy_PA4 = met if is_female == 1 & age_3groups == 0, nq(2)
xtile healthy_PA5 = met if is_female == 1 & age_3groups == 1, nq(2)
xtile healthy_PA6 = met if is_female == 1 & age_3groups == 2, nq(2)
egen healthy_PA = rowmin(healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6)
		 replace healthy_PA = healthy_PA - 1
	label variable healthy_PA "healthy PA"
	label define healthy_PA 1 "healthy PA:higher half" 0 "healthy PA:lower half"
	label values healthy_PA healthy_PA
drop healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6

*5groups
xtile PA_5groups1 = met if is_female == 0 & age_3groups == 0, nq(5)
xtile PA_5groups2 = met if is_female == 0 & age_3groups == 1, nq(5)
xtile PA_5groups3 = met if is_female == 0 & age_3groups == 2, nq(5)
xtile PA_5groups4 = met if is_female == 1 & age_3groups == 0, nq(5)
xtile PA_5groups5 = met if is_female == 1 & age_3groups == 1, nq(5)
xtile PA_5groups6 = met if is_female == 1 & age_3groups == 2, nq(5)
egen PA_5groups = rowmax(PA_5groups1 PA_5groups2 PA_5groups3 PA_5groups4 PA_5groups5 PA_5groups6)
	labe variable PA_5groups
drop PA_5groups1 PA_5groups2 PA_5groups3 PA_5groups4 PA_5groups5 PA_5groups6
***Numbers of healthy lifestyle
egen healthy_score = rowtotal(healthy_smoking healthy_alcohol healthy_PA healthy_diet healthy_bmi healthy_WC healthy_sleep)
recode healthy_score (0/2=0) (3=1) (4=2) (5=3) (6/7=4), gen(healthy_score_5groups)

* Risky lifestyle
gen risky_smoking = (healthy_smoking == 0)
gen risky_alcohol = (healthy_alcohol == 0)
gen risky_diet = (healthy_diet == 0)
gen risky_PA = (healthy_PA == 0)
gen risky_bmi = (healthy_bmi == 0)
gen risky_WC = (healthy_WC == 0)
gen risky_sleep = (healthy_sleep == 0)
cap drop __00*
save "D:\HanYT\2019-06 Multimorbidity\1. Database\project2.dta",replace


/* 课题4：multimorbidity */
******Exclusion:糖尿病 重大疾病
use "D:\HanYT\2019-06 Multimorbidity\1. Database\basic database.dta", clear
drop if mi(bmi_calc)
drop if study_date == du_ep0001_date
drop if chd_diag == 1 | stroke_or_tia_diag == 1| cancer_diag == 1 | has_diabetes == 1   

***Physical activity
**sex- and age-specific 
*2groups
xtile healthy_PA1 = met if is_female == 0 & age_3groups == 0, nq(2)
xtile healthy_PA2 = met if is_female == 0 & age_3groups == 1, nq(2)
xtile healthy_PA3 = met if is_female == 0 & age_3groups == 2, nq(2)
xtile healthy_PA4 = met if is_female == 1 & age_3groups == 0, nq(2)
xtile healthy_PA5 = met if is_female == 1 & age_3groups == 1, nq(2)
xtile healthy_PA6 = met if is_female == 1 & age_3groups == 2, nq(2)
egen healthy_PA = rowmin(healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6)
		 replace healthy_PA = healthy_PA - 1
	label variable healthy_PA "healthy PA"
	label define healthy_PA 1 "healthy PA:higher half" 0 "healthy PA:lower half"
	label values healthy_PA healthy_PA
drop healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6
*5groups
xtile PA_5groups1 = met if is_female == 0 & age_3groups == 0, nq(5)
xtile PA_5groups2 = met if is_female == 0 & age_3groups == 1, nq(5)
xtile PA_5groups3 = met if is_female == 0 & age_3groups == 2, nq(5)
xtile PA_5groups4 = met if is_female == 1 & age_3groups == 0, nq(5)
xtile PA_5groups5 = met if is_female == 1 & age_3groups == 1, nq(5)
xtile PA_5groups6 = met if is_female == 1 & age_3groups == 2, nq(5)
egen PA_5groups = rowmax(PA_5groups1 PA_5groups2 PA_5groups3 PA_5groups4 PA_5groups5 PA_5groups6)
	labe variable PA_5groups
drop PA_5groups1 PA_5groups2 PA_5groups3 PA_5groups4 PA_5groups5 PA_5groups6
***Numbers of healthy lifestyle // 新的分析中不将睡眠时间纳入生活方式得分
egen healthy_score = rowtotal(healthy_smoking healthy_alcohol healthy_PA healthy_diet healthy_bmi healthy_WC)    // 2020/08/28 尝试比较不要睡眠的评分
recode healthy_score (0/1=1) (2=2) (3=3) (4=4) (5/6=5), gen(healthy_score_5groups)

**sex-specific 
*2groups
xtile healthy_PA1 = met if is_female == 0, nq(2)
xtile healthy_PA2 = met if is_female == 1, nq(2)
egen healthy_PAa = rowmin(healthy_PA1 healthy_PA2)
		 replace healthy_PAa = healthy_PAa - 1
drop healthy_PA1 healthy_PA2

* Risky lifestyle
gen risky_smoking = (healthy_smoking == 0)
gen risky_alcohol = (healthy_alcohol3 == 0)
gen risky_diet = (healthy_diet == 0)
gen risky_PA = (healthy_PA == 0)
gen risky_bmi = (healthy_bmi == 0)
gen risky_bmi2 = (healthy_bmi2 == 0)
gen risky_WC = (healthy_WC == 0)
gen risky_WC2 = (healthy_WC2 == 0)
gen risky_WC3 = (healthy_WC3 == 0)
gen risky_sleep = (healthy_sleep == 0)
gen risky_obesity = (healthy_obesity == 0)
gen risky_obesity2 = (healthy_obesity2 == 0)
cap drop __00*
label variable risky_smoking "Risky smoking"
label variable risky_alcohol "Risky alcohol"
label variable risky_diet "Risky diet"
label variable risky_bmi "Risky bmi"
label variable risky_WC "Risky WC"
label variable risky_obesity "Risky obesity"
label values risky_smoking risky_alcohol risky_PA risky_diet risky_bmi risky_WC risky_obesity yesno

* Risky lifestyle score
egen risky_score = rowtotal(risky_smoking risky_alcohol risky_PA risky_diet risky_obesity)
	recode risky_score (0/1=1) (2=2) (3=3) (4=4) (5=5), gen(risky_score_5groups)

save "D:\HanYT\2019-06 Multimorbidity\1. Database\project4.dta",replace

******Exclusion:肿瘤和1型糖尿病     冠心病、脑卒中和2型糖尿病保留作为起始阶段   05/06
use "D:\HanYT\2019-06 Multimorbidity\1. Database\basic database.dta", clear
drop if mi(bmi_calc)
drop if study_date == du_ep0001_date
drop if taking_insulin ==1 & diabetes_diag_age <= 20   /*  diabetic, using insuline and diagnosed <=20y, was defined type1 */ 
drop if cancer_diag == 1

***Physical activity
*2groups
xtile healthy_PA1 = met if is_female == 0 & age_3groups == 0, nq(2)
xtile healthy_PA2 = met if is_female == 0 & age_3groups == 1, nq(2)
xtile healthy_PA3 = met if is_female == 0 & age_3groups == 2, nq(2)
xtile healthy_PA4 = met if is_female == 1 & age_3groups == 0, nq(2)
xtile healthy_PA5 = met if is_female == 1 & age_3groups == 1, nq(2)
xtile healthy_PA6 = met if is_female == 1 & age_3groups == 2, nq(2)
egen healthy_PA = rowmin(healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6)
		 replace healthy_PA = healthy_PA - 1
	label variable healthy_PA "healthy PA"
	label define healthy_PA 1 "healthy PA:higher half" 0 "healthy PA:lower half"
	label values healthy_PA healthy_PA
drop healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6
*5groups
xtile PA_5groups1 = met if is_female == 0 & age_3groups == 0, nq(5)
xtile PA_5groups2 = met if is_female == 0 & age_3groups == 1, nq(5)
xtile PA_5groups3 = met if is_female == 0 & age_3groups == 2, nq(5)
xtile PA_5groups4 = met if is_female == 1 & age_3groups == 0, nq(5)
xtile PA_5groups5 = met if is_female == 1 & age_3groups == 1, nq(5)
xtile PA_5groups6 = met if is_female == 1 & age_3groups == 2, nq(5)
egen PA_5groups = rowmax(PA_5groups1 PA_5groups2 PA_5groups3 PA_5groups4 PA_5groups5 PA_5groups6)
	labe variable PA_5groups
drop PA_5groups1 PA_5groups2 PA_5groups3 PA_5groups4 PA_5groups5 PA_5groups6
***Numbers of healthy lifestyle // 新的分析中不将睡眠时间纳入生活方式得分
egen healthy_score = rowtotal(healthy_smoking healthy_alcohol healthy_PA healthy_diet healthy_bmi healthy_WC)    // 2020/08/28 尝试比较不要睡眠的评分
recode healthy_score (0/1=1) (2=2) (3=3) (4=4) (5/6=5), gen(healthy_score_5groups)

* Risky lifestyle
gen risky_smoking = (healthy_smoking == 0)
gen risky_alcohol = (healthy_alcohol3 == 0)
gen risky_diet = (healthy_diet == 0) 
gen risky_PA = (healthy_PA == 0)
gen risky_bmi = (healthy_bmi == 0)
gen risky_bmi2 = (healthy_bmi2 == 0)
gen risky_WC = (healthy_WC == 0)
gen risky_WC2 = (healthy_WC2 == 0)
gen risky_WC3 = (healthy_WC3 == 0)
gen risky_sleep = (healthy_sleep == 0)
gen risky_obesity = (healthy_obesity == 0)
gen risky_obesity2 = (healthy_obesity2 == 0)
cap drop __00*
label variable risky_smoking "Risky smoking"
label variable risky_alcohol "Risky alcohol"
label variable risky_diet "Risky diet"
label variable risky_bmi "Risky bmi"
label variable risky_WC "Risky WC"
label variable risky_obesity "Risky obesity"
label values risky_smoking risky_alcohol risky_PA risky_diet risky_bmi risky_WC risky_obesity yesno

* Risky lifestyle score
egen risky_score = rowtotal(risky_smoking risky_alcohol risky_PA risky_diet risky_obesity)
	recode risky_score (0/1=1) (2=2) (3=3) (4=4) (5=5), gen(risky_score_5groups)

cap drop __00*
save "D:\HanYT\2019-06 Multimorbidity\1. Database\project4_full.dta",replace

log close




