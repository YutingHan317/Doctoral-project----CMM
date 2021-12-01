********************************************************
* Created date: 2021-07-13
* Updating log:
* 2021/7/13 using cd to facilatate the usage of CKB-cloud
* 2021/7/15 adding 7groups of duration of diease
* 2021/07/18 correct the method to define  updated disease status
* 2021/08/09 adding "st split" by 

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                              Part 1: cleaning data                             *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*****
log using "2.Result/Log/data cleaning multi to death V3.log",replace
**********************************************************************************************************
****************************            Part 1.1 Basic Preparation    ************************************
**********************************************************************************************************
/* Merge covarites and endpoints */
***Endpoints
import delimited "/public/home/hanyuting/00-原始数据/2021-02_Multimorbidity/endpoints.csv", encoding(utf8) clear
rename csid studyid

*** Incidence cases
* ischaemic heart disease
rename ep_16523_combined_ep c_ep0003
rename ep_16523_combined_datedeveloped c_ep0003_date

* haemorrhagic stroke
rename ep_16526_combined_ep c_ep0008
rename ep_16526_combined_datedeveloped c_ep0008_date

*ischaemic stroke
rename ep_16536_combined_ep c_ep0009
rename ep_16536_combined_datedeveloped c_ep0009_date

* Any stroke
rename ep_16527_combined_ep c_ep0010
rename ep_16527_combined_datedeveloped c_ep0010_date

*diabetes
rename ep_16532_combined_ep c_ep0047
rename ep_16532_combined_datedeveloped c_ep0047_date
rename ep_16529_combined_ep c_ep0048
rename ep_16529_combined_datedeveloped c_ep0048_date
rename ep_16522_combined_ep c_ep0301
rename ep_16522_combined_datedeveloped c_ep0301_date
rename ep_16524_combined_ep c_ep0302
rename ep_16524_combined_datedeveloped c_ep0302_date


*** Death cases
* All-cause death
rename ep_16521_da_ep du_ep0001
rename ep_16521_da_datedeveloped du_ep0001_date

* Vascular death 
rename ep_16530_du_ep du_ep0002
rename ep_16530_du_datedeveloped du_ep0002_date

* Cancer death
rename ep_16531_du_ep du_ep0014
rename ep_16531_du_datedeveloped du_ep0014_date

* Respiratory disease death
rename ep_16528_du_ep du_ep0032
rename ep_16528_du_datedeveloped du_ep0032_date

* Others (defined after transforming date variable)


*test missing
global eplist_status c_ep0003 c_ep0008 c_ep0009  c_ep0010 c_ep0047  ///
	c_ep0048 c_ep0301 c_ep0301 c_ep0302  du_ep0001 du_ep0002 du_ep0014 du_ep0032
global eplist_date c_ep0003_date c_ep0008_date c_ep0009_date c_ep0010_date c_ep0047_date c_ep0048_date c_ep0301_date c_ep0302_date ///
	du_ep0001_date du_ep0002_date du_ep0014_date du_ep0032_date
misstable summarize $eplist_status $eplist_status
save "1.Data/endpoints.dta", replace

***Baseline
import delimited "/public/home/hanyuting/00-原始数据/2021-02_Multimorbidity/data_baseline_questionnaires.csv", encoding(utf8) clear
rename csid studyid
save "1.Data/baseline.dta", replace

***Merge
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

misstable summarize $eplist_status $eplist_status c_ep0101 c_ep0101_date du_ep9999 du_ep9999_date
save "1.Data/merged_database.dta",replace

**********************************************************************************************************
****************************           Part 1.2 Baseline endpoints    ************************************
**********************************************************************************************************
*** Import dataset
use "1.Data/merged_database.dta",clear

/* Label Baseline disease status */
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

*** Number of diseases
egen d_number_base = rowtotal(has_diabetes chd_diag stroke_or_tia_diag)
	label variable d_number_base "Number of CMDs at basline"

*** Number of disease(3groups)
recode d_number_base (0=0 "None") (1=1 "One") (2/3=2 "Multimorbidity"), gen(d_number2_base)
	label variable d_number2_base "Number of CMD at baseline 3groups"

*** Multimorbidity
gen mltmbd_base = 1 if d_number_base >=2
	replace mltmbd_base = 0 if mi(mltmbd_base)
	label variable mltmbd_base "Baseline Multimorbidity 2 groups"
	label define mltmbd 0 "No Multimorbidity" 1 "Multimorbidity"
	label values mltmbd_base mltmbd

*** free from CMD
recode d_number_base (0=0 "free from cmd") (1/3=1 "least one"), gen(free_cmd_base)
	label variable free_cmd_base "Baseline free CMD"	

*** Mutual Combination
gen combin_base = 0 if has_diabetes == 0 & chd_diag == 0 & stroke_or_tia_diag == 0
	replace combin_base = 1 if has_diabetes == 1 & chd_diag == 0 & stroke_or_tia_diag == 0
	replace combin_base = 2 if has_diabetes == 0 & chd_diag == 1 & stroke_or_tia_diag == 0
	replace combin_base = 3 if has_diabetes == 0 & chd_diag == 0 & stroke_or_tia_diag == 1
	replace combin_base = 4 if has_diabetes == 1 & chd_diag == 1 & stroke_or_tia_diag == 0
	replace combin_base = 5 if has_diabetes == 1 & chd_diag == 0 & stroke_or_tia_diag == 1
	replace combin_base = 6 if has_diabetes == 0 & chd_diag == 1 & stroke_or_tia_diag == 1
	replace combin_base = 7 if has_diabetes == 1 & chd_diag == 1 & stroke_or_tia_diag == 1
	label variable combin_base "Baseline Combination"
	label define baseline_combination 0 "healthy" 1 "only DM" 2 "only CHD" 3 "only ST" 4 "T2D & CHD" ///
		5 "T2D & ST" 6 "CHD & ST" 7 "T2D & CHD & ST"
	label values combin_base baseline_combination

/* duration of disease */
*** Duration
gen duration_chd = age_at_study_date/100 - chd_diag_age
	replace duration_chd = 0 if duration_chd < 0
	label variable duration_chd "Duration of chronic heart disease" 

gen duration_diabetes = age_at_study_date/100 - diabetes_diag_age
	replace duration_diabetes = 0 if has_diabetes == 1 & diabetes_diag == 0
	replace duration_diabetes = 0 if duration_diabetes < 0 
	label variable duration_diabetes "Duration of diabetes"

gen duration_stroke = age_at_study_date/100 - stroke_or_tia_diag_age
	replace duration_stroke = 0 if duration_stroke < 0
	label variable duration_stroke "Duration of stroke or tia"

*** duration groups
egen duration_chd_groups = cut(duration_chd), at(0 0.5 1 2(2)30 90) label
	replace duration_chd_groups = 99 if duration_chd ==.
	label variable duration_chd_groups "Baseline duration of CHD"
egen duration_stroke_groups = cut(duration_stroke), at(0 0.5 1 2(2)30 90) label
	replace duration_stroke_groups = 99 if duration_stroke ==.
	label variable duration_stroke_groups "Baseline duration of stroke"

egen duration_diabetes_groups = cut(duration_diabetes), at(0 0.5 1 2(2)30 90) label
	replace duration_diabetes_groups = 99 if duration_diabetes == .
	label variable duration_diabetes_groups "Baseline duration of diabetes"


*** duration 7 groups
egen duration_chd_7g = cut(duration_chd), at(0(5)30 90) label
	replace duration_chd_7g = 99 if duration_chd ==.
	label variable duration_chd_7g "Baseline duration of CHD 7groups"

egen duration_stroke_7g = cut(duration_stroke), at(0(5)30 90) label
	replace duration_stroke_7g = 99 if duration_stroke ==.
	label variable duration_stroke_7g "Baseline duration of stroke 7groups"
egen duration_diabetes_7g = cut(duration_diabetes), at(0(5)30 90) label
	replace duration_diabetes_7g = 99 if duration_diabetes == .
	label variable duration_diabetes_7g "Baseline duration of diabetes 7groups"

**********************************************************************************************************
****************************           Part 1.3 incidnet endpoints    ************************************
**********************************************************************************************************

***number of disease(4groups)
* c_ep0003 随访期间缺血性心脏病；c_ep0010  随访期间any stroke I60-61 & I63-64; c_ep0101 Diabetes E10-E14
egen d_number_inc = rowtotal(c_ep0003 c_ep0010 c_ep0101)
	label variable d_number_inc "Number of incidence diseases under observation"

***Number of disease(3groups)
recode d_number_inc (0=0 "None") (1=1 "One") (2/3=2 "Multimorbidity"), gen(d_number2_inc)
	label variable d_number2_inc "Number of incidence CMD under observation 3groups"


*** Age at diagnosis; participants without any disease 
gen double age_chd_diag = (c_ep0003_date - dob_anon)/365.25 if c_ep0003 == 1 
gen double age_stroke_diag = (c_ep0010_date - dob_anon)/365.25 if c_ep0010 == 1
gen double age_diabetes_diag = (c_ep0101_date - dob_anon)/365.25 if c_ep0101 == 1

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
    label variable education_2groups "highest education two groups"

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

/* Medication */
/* 高血脂 */
/* gen has_hyperlipidemia = (taking_statins == 1)
	replace has_hyperlipidemia = 0 if mi(has_hyperlipidemia) */

label variable taking_aspirin "Aspirin"
label variable taking_ace_i "ACE-I"
label variable taking_beta_blocker "beta blocker"
label variable taking_statins "statins"
label variable taking_diuretics "Diuretics"
label variable taking_ca_antagonist "Ca++ antagonist"
label variable taking_chlor_metaformin "Chlorpropamide or metformin"
label variable taking_insulin "Insulin"

label define taking_medicine 0 "No" 1 "Yes"

local medicinelist taking_aspirin taking_ace_i taking_beta_blocker taking_statins taking_diuretics taking_ca_antagonist taking_chlor_metaformin taking_insulin
foreach med of local medicinelist{

	clonevar `med'_2groups = `med'
		replace `med'_2groups = 0 if mi(`med'_2groups)
		local varlable :  variable label `med'
		label variable `med'_2groups "`varlable'"
		label values `med'_2groups taking_medicine

}
*** 合并降压药   used_blood_pressure_drugs
gen hypertension_lowering_2g = 1 if used_blood_pressure_drugs == 1 | taking_ace_i == 1 | taking_beta_blocker == 1 | taking_diuretics == 1 | taking_ca_antagonist == 1
	replace hypertension_lowering_2g = 0 if  mi(hypertension_lowering_2g)
	label variable hypertension_lowering_2g "Lower blood pressure medicine"

*** 合并降糖药和胰岛素
gen glucose_lowering_2groups = 1 if taking_chlor_metaformin_2groups == 1 | taking_insulin_2groups == 1
	replace glucose_lowering_2groups = 0 if mi(glucose_lowering_2groups)
	label variable glucose_lowering_2groups "Lower blood glucose medicine"

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
* recode alcohol_7groups (1=1 "No drinker") (2=2 "quit drinking") (3=3 "weekly drinker") (4/5=4 "daily <30g/d") (6/7=5 "daily >=30g/d"),gen(alcohol_5groups1)
* recode alcohol_7groups (1 2 3 6 7=1 "alcohol abstension or heavy drinking") (4 5=2 "daily >0 <30g/d"),gen(healthy_alcohol1)
* recode alcohol_7groups (1 2 6 7=1 "No drinker") (3 4 5=2 "weekly & daily >0 <30g/d"),gen(healthy_alcohol2)
* recode alcohol_7groups (2 6 7=0 "quit & heavy drinking") (1 3 4 5=1 "None & weekly & daily >0 <30g/d"),gen(healthy_alcohol3) /// 
***diet
*需要单独分析饮食指标，在理论基础上选择有意义的；暂按照楠波师兄定义20-10-25
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
label values healthy_diet healthy_diet

/* * 共病分析所用的定义：新分析中分析了饮食与6种结局之间的关联，决定纳入红肉、蛋类、新鲜蔬菜、新鲜水果  
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
 */

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


save "1.Data/basic database.dta",replace


**********************************************************************************************************
****************************           Part 1.7 gen databases       **************************************
**********************************************************************************************************
******Exclusion
use "1.Data/basic database.dta", clear
drop if mi(bmi_calc)
drop if study_date == du_ep0001_date    /* 200509 入组即失访者 */


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

***detect missing values
misstable summarize has_hypertension - PA_5groups

save "1.Data/project2.dta",replace


*********************************************************************************************************************
****************************             Part 1.8 gen updated databases       ***************************************
*********************************************************************************************************************
************** c_ep0003 随访期间缺血性心脏病；c_ep0010  随访期间any stroke I60-61 & I63-64; c_ep0101 Diabetes E10-E14
************** Dataset has been stset
local elist du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999
foreach e of local elist{

	use "1.Data/project2.dta", clear
	/* 对数据进行切割 */
	*** Set survival data
	stset `e'_date, id(studyid) enter(study_date) origin(time dob_anon) scale(365.25) failure(`e' == 1)

	*** 根据缺血性心脏病切割随访时间
	stsplit ihd_split, after(time=c_ep0003_date) at(0)

	*** 根据脑卒中切割随访时间
	stsplit stroke_split, after(time=c_ep0010_date) at(0)

	*** 根据糖尿病切割随访时间
	stsplit diabetes_split, after(time=c_ep0101_date) at(0)


	/* 生成Updated variables */
	*** Updated single CMD   2021-04-03
	gen chd_diag_updated = 0 if chd_diag == 0 & c_ep0003 == 0
		replace chd_diag_updated = 1 if chd_diag == 1 
		replace chd_diag_updated = 1 if chd_diag == 0 & c_ep0003 == 1 & age_chd_diag >= _t0 & age_chd_diag < _t
		replace chd_diag_updated = 1 if chd_diag == 0 & c_ep0003 == 1 & age_chd_diag < _t0
		replace chd_diag_updated = 0 if chd_diag == 0 & c_ep0003 == 1 & age_chd_diag >= _t
		label variable chd_diag_updated "Updated ischemis heart disease status"
		label values chd_diag_updated baseline_disease

	gen stroke_diag_updated = 0 if stroke_or_tia_diag == 0 & c_ep0010 == 0
		replace stroke_diag_updated = 1 if stroke_or_tia_diag == 1 
		replace stroke_diag_updated = 1 if stroke_or_tia_diag == 0 & c_ep0010 == 1 & age_stroke_diag >= _t0 & age_stroke_diag < _t
		replace stroke_diag_updated = 1 if stroke_or_tia_diag == 0 & c_ep0010 == 1 & age_stroke_diag < _t0 
		replace stroke_diag_updated = 0 if stroke_or_tia_diag == 0 & c_ep0010 == 1 & age_stroke_diag >= _t
		label variable stroke_diag_updated "Updated stroke status"
		label values stroke_diag_updated baseline_disease

	gen diabetes_diag_updated = 0 if has_diabetes == 0 & c_ep0101 == 0
		replace diabetes_diag_updated = 1 if has_diabetes == 1 
		replace diabetes_diag_updated = 1 if has_diabetes == 0 & c_ep0101 == 1 & age_diabetes_diag >= _t0 & age_diabetes_diag < _t
		replace diabetes_diag_updated = 1 if has_diabetes == 0 & c_ep0101 == 1 & age_diabetes_diag < _t0
		replace diabetes_diag_updated = 0 if has_diabetes == 0 & c_ep0101 == 1 & age_diabetes_diag >= _t
		label variable diabetes_diag_updated "Updated diabetes status"
		label values diabetes_diag_updated baseline_disease


	*** Updated No. of CMD
	egen d_number_updated = rowtotal(diabetes_diag_updated chd_diag_updated stroke_diag_updated)
		label variable d_number_updated "Updated No. of CMD"

	*** Updated combinatioin of CMDs
	gen combin_updated = 0 if diabetes_diag_updated == 0 & chd_diag_updated == 0 & stroke_diag_updated == 0
		replace combin_updated = 1 if diabetes_diag_updated == 1 & chd_diag_updated == 0 & stroke_diag_updated == 0
		replace combin_updated = 2 if diabetes_diag_updated == 0 & chd_diag_updated == 1 & stroke_diag_updated == 0
		replace combin_updated = 3 if diabetes_diag_updated == 0 & chd_diag_updated == 0 & stroke_diag_updated == 1
		replace combin_updated = 4 if diabetes_diag_updated == 1 & chd_diag_updated == 1 & stroke_diag_updated == 0
		replace combin_updated = 5 if diabetes_diag_updated == 1 & chd_diag_updated == 0 & stroke_diag_updated == 1
		replace combin_updated = 6 if diabetes_diag_updated == 0 & chd_diag_updated == 1 & stroke_diag_updated == 1
		replace combin_updated = 7 if diabetes_diag_updated == 1 & chd_diag_updated == 1 & stroke_diag_updated == 1
		label variable combin_updated "Updated Combination"
		label values combin_updated baseline_combination

	*** Description
	tab1 diabetes_diag_updated chd_diag_updated stroke_diag_updated d_number_updated combin_updated
	tab chd_diag chd_diag_updated
	tab stroke_or_tia_diag stroke_diag_updated
	tab has_diabetes diabetes_diag_updated
	tab d_number_updated
	tab d_number_base d_number_updated
	tab combin_updated
	tab combin_base combin_updated


	save "1.Data/project2_updated_`e'.dta",replace

}




*********************************************************************************************************************
****************************        Part 1.9 gen updated disease duration     ***************************************
*********************************************************************************************************************
*** Bseline variables: duration_diabetes duration_chd duration_stroke duration_diabetes_groups duration_chd_groups duration_stroke_groups
/* 方法一: 先按照发生CMD时间切割，再切割每年，计算duration的参考时间点是_t0 */
* 按照1年切割
local elist du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999
foreach e of local elist{

	use "1.Data/project2_updated_`e'.dta", clear
	stsplit aage, every(1)


	*** Generate Updated duration of disease
	* Duration
	gen duration_chd_updated = _t0 - chd_diag_age if chd_diag == 1    // 对于基线就已经患病的人
		replace duration_chd_updated = _t0 - (c_ep0003_date - dob_anon)/365.25 if chd_diag == 0 & c_ep0003==1 & ((c_ep0003_date - dob_anon)/365.25) <= _t0
		replace duration_chd_updated  = 0 if duration_chd_updated < 0 
		label variable duration_chd_updated "Updated Duration of chronic heart disease" 

	gen duration_diabetes_updated = _t0 - diabetes_diag_age if diabetes_diag == 1 & diabetes_diag == 1
		replace duration_diabetes_updated = _t0 - age_at_study_date if has_diabetes == 1 & diabetes_diag == 0
		replace duration_diabetes_updated = _t0 - (c_ep0101_date - dob_anon)/365.25 if has_diabetes == 0 & c_ep0101 == 1 & (c_ep0101_date - dob_anon)/365.25 <= _t0
		replace duration_diabetes_updated = 0 if duration_diabetes_updated < 0 
		label variable duration_diabetes_updated "Updated Duration of diabetes"

	gen duration_stroke_updated =  _t0 - stroke_or_tia_diag_age if stroke_or_tia_diag == 1 
		replace duration_stroke_updated = _t0 - (c_ep0010_date - dob_anon)/365.25 if stroke_or_tia_diag == 0 & c_ep0010==1 & ((c_ep0010_date - dob_anon)/365.25) <= _t0
		replace duration_stroke_updated = 0 if duration_stroke_updated < 0
		label variable duration_stroke_updated "Updated Duration of stroke or tia"

	*** Updated duration groups
	*** duration groups
	egen duration_chd_updated_groups = cut(duration_chd_updated), at(0 0.5 1 2(2)30 90) label
		replace duration_chd_updated_groups = 99 if duration_chd_updated ==.
		label variable duration_chd_updated_groups "Updated duration of chd"

	egen duration_stroke_updated_groups = cut(duration_stroke_updated), at(0 0.5 1 2(2)30 90) label
		replace duration_stroke_updated_groups = 99 if duration_stroke_updated ==.
		label variable duration_stroke_updated_groups "Updated duration of stroke"

	egen duration_diabetes_updated_groups = cut(duration_diabetes_updated), at(0 0.5 1 2(2)30 90) label
		replace duration_diabetes_updated_groups = 99 if duration_diabetes_updated ==.
		label variable duration_diabetes_updated_groups "Updated duration of diabetes"


	*** Duration 7 groups
	egen duration_chd_updated_7g = cut(duration_chd_updated), at(0(5)30 90) label
		replace duration_chd_updated_7g = 99 if duration_chd_updated ==.
		label variable duration_chd_updated_7g "Updated duration of chd 7groups"

	egen duration_stroke_updated_7g = cut(duration_stroke_updated), at(0(5)30 90) label
		replace duration_stroke_updated_7g = 99 if duration_stroke_updated ==.
		label variable duration_stroke_updated_7g "Updated duration of stroke 7groups"

	egen duration_diabetes_updated_7g = cut(duration_diabetes_updated), at(0(5)30 90) label
		replace duration_diabetes_updated_7g = 99 if duration_diabetes_updated ==.
		label variable duration_diabetes_updated_7g "Updated duration of diabetes 7groups"


	save "1.Data/project2_updated_`e'_duration.dta", replace 
}


* 按照0.5年切割
local elist du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999
foreach e of local elist{

	use "1.Data/project2_updated_`e'.dta", clear
	stsplit aage, every(0.5)


	*** Generate Updated duration of disease
	* Duration
	gen duration_chd_updated = _t0 - chd_diag_age if chd_diag == 1    // 对于基线就已经患病的人
		replace duration_chd_updated = _t0 - (c_ep0003_date - dob_anon)/365.25 if chd_diag == 0 & c_ep0003==1 & ((c_ep0003_date - dob_anon)/365.25) <= _t0
		replace duration_chd_updated  = 0 if duration_chd_updated < 0 
		label variable duration_chd_updated "Updated Duration of chronic heart disease" 

	gen duration_diabetes_updated = _t0 - diabetes_diag_age if diabetes_diag == 1 & diabetes_diag == 1
		replace duration_diabetes_updated = _t0 - age_at_study_date if has_diabetes == 1 & diabetes_diag == 0
		replace duration_diabetes_updated = _t0 - (c_ep0101_date - dob_anon)/365.25 if has_diabetes == 0 & c_ep0101 == 1 & (c_ep0101_date - dob_anon)/365.25 <= _t0
		replace duration_diabetes_updated = 0 if duration_diabetes_updated < 0 
		label variable duration_diabetes_updated "Updated Duration of diabetes"

	gen duration_stroke_updated =  _t0 - stroke_or_tia_diag_age if stroke_or_tia_diag == 1 
		replace duration_stroke_updated = _t0 - (c_ep0010_date - dob_anon)/365.25 if stroke_or_tia_diag == 0 & c_ep0010==1 & ((c_ep0010_date - dob_anon)/365.25) <= _t0
		replace duration_stroke_updated = 0 if duration_stroke_updated < 0
		label variable duration_stroke_updated "Updated Duration of stroke or tia"

	*** Updated duration groups
	*** duration groups
	egen duration_chd_updated_groups = cut(duration_chd_updated), at(0 0.5 1 2(2)30 90) label
		replace duration_chd_updated_groups = 99 if duration_chd_updated ==.
		label variable duration_chd_updated_groups "Updated duration of chd"

	egen duration_stroke_updated_groups = cut(duration_stroke_updated), at(0 0.5 1 2(2)30 90) label
		replace duration_stroke_updated_groups = 99 if duration_stroke_updated ==.
		label variable duration_stroke_updated_groups "Updated duration of stroke"

	egen duration_diabetes_updated_groups = cut(duration_diabetes_updated), at(0 0.5 1 2(2)30 90) label
		replace duration_diabetes_updated_groups = 99 if duration_diabetes_updated ==.
		label variable duration_diabetes_updated_groups "Updated duration of diabetes"


	*** Duration 7 groups
	egen duration_chd_updated_7g = cut(duration_chd_updated), at(0(5)30 90) label
		replace duration_chd_updated_7g = 99 if duration_chd_updated ==.
		label variable duration_chd_updated_7g "Updated duration of chd 7groups"

	egen duration_stroke_updated_7g = cut(duration_stroke_updated), at(0(5)30 90) label
		replace duration_stroke_updated_7g = 99 if duration_stroke_updated ==.
		label variable duration_stroke_updated_7g "Updated duration of stroke 7groups"

	egen duration_diabetes_updated_7g = cut(duration_diabetes_updated), at(0(5)30 90) label
		replace duration_diabetes_updated_7g = 99 if duration_diabetes_updated ==.
		label variable duration_diabetes_updated_7g "Updated duration of diabetes 7groups"


	save "1.Data/project2_updated_`e'_duration2.dta", replace 
}



/* 方法二：直接在project2 的基础上 stsplit, at(failure) ,但内存不足以进行上述处理*/

log close
