**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
*************                                                                                *************
*************                              Part 1: cleaning data                             *************
*************                                                                                *************
**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
log using "D:\HanYT\03--生活方式变化\3. Result\data cleaning $S_DATE.log",replace

**********************************************************************************************************
****************************           Part 1.1 dataset Preparation   ************************************
**********************************************************************************************************
/* .csv 转换为 .dta */
***第一次重复调查
import delimited "D:\HanYT\00-原始数据\2020-05 Lifestyle\data_resurvey1_questionnaires.csv", encoding(utf8) clear
rename csid studyid
save "D:\HanYT\03--生活方式变化\1. Data\data_resurvey1_questionnaires.dta", replace

***第二次重复调查
import delimited "D:\HanYT\00-原始数据\2020-05 Lifestyle\data_resurvey2_questionnaires.csv", encoding(utf8) clear
rename csid studyid
save "D:\HanYT\03--生活方式变化\1. Data\data_resurvey2_questionnaires.dta", replace

***基线调查
import delimited "D:\HanYT\00-原始数据\2020-05 Lifestyle\data_baseline_questionnaires.csv", encoding(utf8) clear
rename csid studyid
save "D:\HanYT\03--生活方式变化\1. Data\data_baseline_questionnaires.dta", replace

***结局数据库
import delimited "D:\HanYT\00-原始数据\2020-05 Lifestyle\endpoints.csv", encoding(utf8) clear
rename csid studyid

**rename endpoints
*Vascular disease
rename ep_12487_combined_ep c_ep0002
rename ep_12487_combined_datedeveloped c_ep0002_date

*ischaemic heart disease
rename ep_12494_combined_ep c_ep0003
rename ep_12494_combined_datedeveloped c_ep0003_date

*Any stroke
rename ep_12495_combined_ep c_ep0070
rename ep_12495_combined_datedeveloped c_ep0070_date

*Hemorrhagic stroke
rename ep_12498_combined_ep c_ep0008
rename ep_12498_combined_datedeveloped c_ep0008_date

*ischaemic stroke
rename ep_12489_combined_ep c_ep0009
rename ep_12489_combined_datedeveloped c_ep0009_date

*Cancer
rename ep_12491_combined_ep c_ep0014
rename ep_12491_combined_datedeveloped c_ep0014_date

*COPD
rename ep_12488_combined_ep c_ep0033
rename ep_12488_combined_datedeveloped c_ep0033_date

*diabetes
rename ep_12500_combined_ep c_ep0047
rename ep_12500_combined_datedeveloped c_ep0047_date
rename ep_12493_combined_ep c_ep0048
rename ep_12493_combined_datedeveloped c_ep0048_date
rename ep_12496_combined_ep c_ep0301
rename ep_12496_combined_datedeveloped c_ep0301_date
rename ep_12492_combined_ep c_ep0302
rename ep_12492_combined_datedeveloped c_ep0302_date

*All-cause death
rename ep_12499_da_ep du_ep0001
rename ep_12499_da_datedeveloped du_ep0001_date
*All-cause incident cases
rename ep_12499_combined_ep c_ep0001
rename ep_12499_combined_datedeveloped c_ep0001_date

save "D:\HanYT\03--生活方式变化\1. Data\endpoints.dta", replace


**********************************************************************************************************
****************************           Part 1.2 baseline database     ************************************
**********************************************************************************************************
use "D:\HanYT\03--生活方式变化\1. Data\data_baseline_questionnaires.dta", clear

**********************************************************************************************************
****************************               Part 1.2.1 Covarites       ************************************
**********************************************************************************************************
/* Sociodemographic variables */
***sex
label variable is_female "Sex"
label define is_female 0 "male" 1 "female"
label values is_female is_female 

***residence
label variable region_is_urban "residence"
label define residence 0 "Rural" 1 "Urban"
label values region_is_urban residence

***education
label variable highest_education "highest education"
label define highest_education 0 "no formal school" 1 "primary school" 2 "middle school" 3 "high school" 4 "technical school" 5 "university"
label values highest_education highest_education
recode highest_education (0/1=0 "Illiterate and primary school") (2/3=1 "Middle school") (4/5=2 "College/university"), gen(education_3groups)
    label variable education_3groups "highest education three groups"

***household income
label variable household_income "household income"
label define household_income 0 "<2500yuan" 1 "2500-4999yuan" 2 "5000-9999yuan" 3 "10000-19999yuan" 4 "20000-34999yuan" 5 ">=35000yuan"
label values household_income household_income
recode household_income (0/2=0 "<10000/year") (3=1 "10000-19999/year") (4=2 "20000-34999/year") (4=3 ">35000/year"), gen(income_4groups)
	label variable income_4groups "Income four groups"

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

/* ***hypertension
gen has_hypertension = 1 if hypertension_diag == 1 | sbp_mean >= 140 | dbp_mean >= 90 | used_blood_pressure_drugs == 1
replace has_hypertension = 0 if hypertension_diag == 0 & sbp_mean < 140 & dbp_mean < 90 & used_blood_pressure_drugs == 0
label variable has_hypertension "Baseline hypertension"
label define yesno 1 "yes" 0 "no"
label values has_hypertension yesno */

*** Using statin
gen has_hyperlipidemia = (taking_statins == 1)
	replace has_hyperlipidemia = 0 if mi(has_hyperlipidemia)

 
**********************************************************************************************************
****************************         Part 1.2.2 Lifestyle factors     ************************************
**********************************************************************************************************
***smoking
gen smoking_5groups = 1 if smoking_category == 1 | smoking_category == 2     
	replace smoking_5groups = 2 if smoking_category == 3 & (smoking_stopped_reason > 0 & smoking_stopped_reason <= 4)
	replace smoking_5groups = 3 if (smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day < 15.00
	replace smoking_5groups = 4 if (smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day >= 15.00 & cig_equiv_day < 25.00
	replace smoking_5groups = 5 if (smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day >= 25.00
label variable smoking_5groups "Smoking 5groups"
label define smoking_5groups 1 "Non-smoker" 2 "exsmoker" 3 "current <15 cig/d" 4 "current 15-24.9 cig/d" 5 "current >=25 cig/d"  
label values smoking_5groups smoking_5groups
recode smoking_category (1=0 "never smoker")  (2/5=1 "ever smoker"), gen (smoking_2groups)
recode smoking_5groups (1/2=1 "healthy")  (3/5=0 "not healthy"), gen (healthy_smoking)
	label variable healthy_smoking "Healthy smoking"

***alcohol
/* *2groups
gen healthy_alcohol = 0 if alcohol_category == 6 & alc_weekly == 2 &  total_alc_typ_day_g >=30 & is_female == 0
	replace healthy_alcohol = 0 if alcohol_category == 6 & alc_weekly == 2 &  total_alc_typ_day_g >=15 & is_female == 1
	replace healthy_alcohol = 1 if mi(healthy_alcohol)
label variable healthy_alcohol "Healthy alcohol"
label define healthy_alcohol 1 "Healthy: M<30/F<15 g/d" 0 "Unhealthy"
label values healthy_alcohol healthy_alcohol */
*5groups
gen alcohol_7groups = 1 if alcohol_category == 1 | alcohol_category == 3 | alcohol_category == 4
	replace alcohol_7groups = 2 if alcohol_category == 2 | alcohol_category == 5
	replace alcohol_7groups = 3 if alcohol_category == 6 & (alc_weekly == 0 | alc_weekly == 1)
	replace alcohol_7groups = 4 if alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g < 15.00
	replace alcohol_7groups = 5 if alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 15.00 & total_alc_typ_day_g < 30.00)
	replace alcohol_7groups = 6 if alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 30.00 & total_alc_typ_day_g < 60.00)
	replace alcohol_7groups = 7 if alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g >= 60.00
recode alcohol_7groups (1/3=1 "Not daily") (4=2 "Daily 1-14g/d") (5=3 "Daily 15-29g/d") (6=4 "Daily 30-59g/d") (7=5 "Daily >=60g/d"), gen(alcohol_5groups)
label variable alcohol_5groups "Alcohol 5groups"
recode alcohol_7groups (2 6 7=0 "quit & heavy drinking") (1 3 4 5=1 "None & weekly & daily >0 <30g/d"),gen(healthy_alcohol)
label variable healthy_alcohol "Healthy alcohol"

***diet： 新分析中分析了饮食与6种结局之间的关联，决定纳入红肉、蛋类、新鲜蔬菜、新鲜水果  05-09
gen diet_component1 = 1 if diet_freq_fresh_veg == 0
	replace diet_component1 = 0 if mi(diet_component1)
gen diet_component2 = 1 if diet_freq_fresh_fruit == 0
	replace diet_component2 = 0 if mi(diet_component2)
gen diet_component3 = 1 if diet_freq_meat > 0 & diet_freq_meat < 4
	replace diet_component3 = 0 if mi(diet_component3)
gen diet_component4 = 1 if diet_freq_eggs == 0 
	replace diet_component4 = 0 if mi(diet_component4)
egen diet_5score = rowtotal(diet_component1 diet_component2 diet_component3 diet_component4)
tab diet_5score
drop diet_component*
clonevar diet_5groups = diet_5score
	label variable diet_5groups "Diet 5groups"
gen healthy_diet = 1 if diet_5score == 4
	replace healthy_diet = 0 if mi(healthy_diet)
label variable healthy_diet "Healthy diet"
label define healthy_diet 1 "Healthy diet" 0 "Unhealthy diet"
label values healthy_diet healthy_diet

***obesity
*BMI
recode bmi_calc (min/18.499=1 "Underweight <18.5") (18.5/23.999=2 "Normal <24.0") (24.0/27.999=3 "Overweight <28.0") (28.0/60.0=4 "Obesity >=28.0"), gen(bmi_4groups)
label variable bmi_4groups "BMI 4groups"
recode bmi_4groups (2=1 "Healthy BMI") (1 3 4=0 "Unhealthy BMI"), gen(healthy_bmi)
label variable healthy_bmi "Healthy BMI"
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
	***sleep duration
recode sleep_hours (min/6=1 "<=6h") (7=2 "7h") (8=3 "8h") (9=4 "9h") (10/max=5 ">=10h"), gen(sleep_5groups)
	label variable sleep_5groups "Sleep duration 5groups"
gen healthy_sleep = 1 if sleep_5groups == 2 | sleep_5groups == 3 | sleep_5groups == 4
	replace healthy_sleep = 0 if mi(healthy_sleep)
	label variable healthy_sleep "healthy sleep"
	label define healthy_sleep 1 "Healthy:7-9h" 0 "Unhealthy" 
	label values healthy_sleep healthy_sleep
tab1 sleep_5groups healthy_sleep
	
***Physical activity(剔除后计算)


***部分变量label
label variable met "Physical activity(Continuous)"
label variable sleep_hours "sleep hours(Continuous)"
label variable bmi_calc "BMI(Continuous)"

save "D:\HanYT\03--生活方式变化\1. Data\dataset_baseline.dta", replace


**********************************************************************************************************
****************************               Part 1.3 resurvey2         ************************************
**********************************************************************************************************
use "D:\HanYT\03--生活方式变化\1. Data\data_resurvey2_questionnaires.dta", clear
merge 1:1 studyid using "D:\HanYT\03--生活方式变化\1. Data\dataset_baseline.dta",keep(match) keepusing(is_female) nogen
label drop _all
drop if smoking_category == .    
drop if waist_mm == .
**********************************************************************************************************
****************************               Part 1.2.1 Covarites       ************************************
**********************************************************************************************************
/* Sociodemographic variables */
capture{

	***sex
	label variable is_female "Sex"
	label define is_female 0 "male" 1 "female"
	label values is_female is_female 

	***residence
	label variable region_is_urban "residence"
	label define residence 0 "Rural" 1 "Urban"
	label values region_is_urban residence

}


***education
label variable highest_education "highest education"
label define highest_education 0 "no formal school" 1 "primary school" 2 "middle school" 3 "high school" 4 "technical school" 5 "university"
label values highest_education highest_education
recode highest_education (0/1=0 "Illiterate and primary school") (2/3=1 "Middle school") (4/5=2 "College/university"), gen(education_3groups)
    label variable education_3groups "highest education three groups"

***household income
label variable household_income2 "household income"
label define household_income2 0 "<2500yuan" 1 "2500-4999yuan" 2 "5000-9999yuan" 3 "10000-19999yuan" 4 "20000-34999yuan" 5 "35000-49,999yuan" 6 "50000-74999yuan" 7 "75000-99999yuan" 8 ">=100000yuan"
label values household_income2 household_income2
recode household_income2 (0/2=0 "<10000/year") (3=1 "10000-19999/year") (4=2 ">20000/year") (5/max=3 ">=35000/year"), gen(income_4groups)
	label variable income_4groups "Income four groups"

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
egen age_strata=cut(age_at_study_date), at(30(5)90) label
	tab age_strata
egen age_3groups = cut(age_at_study_date), at(0 50 60 100) icodes
	label variable age_3groups "age groups at baseline"
	label define age_3groups 0 "<50 y" 1 "50~59 y" 2 ">=60 yr"
	label values age_3groups age_3groups	

***hypertension
/* gen has_hypertension = 1 if hypertension_diag == 1 | sbp_mean >= 140 | dbp_mean >= 90 | used_blood_pressure_drugs == 1
replace has_hypertension = 0 if hypertension_diag == 0 & sbp_mean < 140 & dbp_mean < 90 & used_blood_pressure_drugs == 0
label variable has_hypertension "Baseline hypertension"
label define yesno 1 "yes" 0 "no"
label values has_hypertension yesno */

*** Using statin
gen has_hyperlipidemia = (taking_statins == 1)
	replace has_hyperlipidemia = 0 if mi(has_hyperlipidemia)

**********************************************************************************************************
****************************         Part 1.2.2 Lifestyle factors     ************************************
**********************************************************************************************************
***smoking
gen smoking_5groups = 1 if smoking_category == 1 | smoking_category == 2     
	replace smoking_5groups = 2 if smoking_category == 3 & (smoking_stopped_reason2 > 0 & smoking_stopped_reason2 <= 5)
	replace smoking_5groups = 3 if (smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason2 == 0)) & cig_equiv_day < 15.00
	replace smoking_5groups = 4 if (smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason2 == 0)) & cig_equiv_day >= 15.00 & cig_equiv_day < 25.00
	replace smoking_5groups = 5 if (smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason2 == 0)) & cig_equiv_day >= 25.00
label variable smoking_5groups "Smoking 5groups"
label define smoking_5groups 1 "Non-smoker" 2 "exsmoker" 3 "current <15 cig/d" 4 "current 15-24.9 cig/d" 5 "current >=25 cig/d"  
label values smoking_5groups smoking_5groups
recode smoking_category (1=0 "never smoker")  (2/5=1 "ever smoker"), gen (smoking_2groups)
recode smoking_5groups (1/2=1 "healthy")  (3/5=0 "not healthy"), gen (healthy_smoking)
	label variable healthy_smoking "Healthy smoking"

***alcohol
/* *2groups
gen healthy_alcohol = 0 if alcohol_category == 6 & alc_weekly == 2 &  total_alc_typ_day_g >=30 & is_female == 0
	replace healthy_alcohol = 0 if alcohol_category == 6 & alc_weekly == 2 &  total_alc_typ_day_g >=15 & is_female == 1
	replace healthy_alcohol = 1 if mi(healthy_alcohol)
label variable healthy_alcohol "Healthy alcohol"
label define healthy_alcohol 1 "Healthy: M<30/F<15 g/d" 0 "Unhealthy"
label values healthy_alcohol healthy_alcohol */
*5groups
gen alcohol_7groups = 1 if alcohol_category == 1 | alcohol_category == 3 | alcohol_category == 4
	replace alcohol_7groups = 2 if alcohol_category == 2 | alcohol_category == 5
	replace alcohol_7groups = 3 if alcohol_category == 6 & (alc_weekly == 0 | alc_weekly == 1)
	replace alcohol_7groups = 4 if alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g < 15.00
	replace alcohol_7groups = 5 if alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 15.00 & total_alc_typ_day_g < 30.00)
	replace alcohol_7groups = 6 if alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 30.00 & total_alc_typ_day_g < 60.00)
	replace alcohol_7groups = 7 if alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g >= 60.00
recode alcohol_7groups (1/3=1 "Not daily") (4=2 "Daily 1-14g/d") (5=3 "Daily 15-29g/d") (6=4 "Daily 30-59g/d") (7=5 "Daily >=60g/d"), gen(alcohol_5groups)
label variable alcohol_5groups "Alcohol 5groups"
recode alcohol_7groups (2 6 7=0 "quit & heavy drinking") (1 3 4 5=1 "None & weekly & daily >0 <30g/d"),gen(healthy_alcohol)
label variable healthy_alcohol "Healthy alcohol"

***diet： 新分析中分析了饮食与6种结局之间的关联，决定纳入红肉、蛋类、新鲜蔬菜、新鲜水果  05-09
gen diet_component1 = 1 if diet_freq_fresh_veg == 0
	replace diet_component1 = 0 if mi(diet_component1)
gen diet_component2 = 1 if diet_freq_fresh_fruit == 0
	replace diet_component2 = 0 if mi(diet_component2)
gen diet_component3 = 1 if diet_freq_meat > 0 & diet_freq_meat < 4
	replace diet_component3 = 0 if mi(diet_component3)
gen diet_component4 = 1 if diet_freq_eggs == 0 
	replace diet_component4 = 0 if mi(diet_component4)
egen diet_5score = rowtotal(diet_component1 diet_component2 diet_component3 diet_component4)
tab diet_5score
drop diet_component*
clonevar diet_5groups = diet_5score
	label variable diet_5groups "Diet 5groups"
gen healthy_diet = 1 if diet_5score == 4
	replace healthy_diet = 0 if mi(healthy_diet)
label variable healthy_diet "Healthy diet"
label define healthy_diet 1 "Healthy diet" 0 "Unhealthy diet"
label values healthy_diet healthy_diet

***obesity
*BMI
recode bmi_calc (min/18.499=1 "Underweight <18.5") (18.5/23.999=2 "Normal <24.0") (24.0/27.99=3 "Overweight <28.0") (28.0/60.0=4 "Obesity >=28.0"), gen(bmi_4groups)
recode bmi_4groups (2=1 "Healthy BMI") (1 3 4=0 "Unhealthy BMI"), gen(healthy_bmi)
label variable healthy_bmi "Healthy bmi"
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
label variable healthy_WC "Healthy WC"
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
	***sleep duration
recode sleep_hours (min/6=1 "<=6h") (7=2 "7h") (8=3 "8h") (9=4 "9h") (10/max=5 ">=10h"), gen(sleep_5groups)
	label variable sleep_5groups "Sleep duration 5groups"
gen healthy_sleep = 1 if sleep_5groups == 2 | sleep_5groups == 3 | sleep_5groups == 4
	replace healthy_sleep = 0 if mi(healthy_sleep)
	label variable healthy_sleep "Healthy sleep"
	label define healthy_sleep 1 "Healthy:7-9h" 0 "Unhealthy" 
	label values healthy_sleep healthy_sleep
tab1 sleep_5groups healthy_sleep
	
***Physical activity(剔除后计算)


***部分变量label
label variable met "Physical activity(Continuous)"
label variable sleep_hours "sleep hours(Continuous)"
label variable bmi_calc "BMI(Continuous)"


***Rename
local varlist study_date highest_education education_3groups income_4groups occupation marital_status ///
	marital_status_2groups age_at_study_date age_strata age_3groups smoking_category cig_equiv_day smoking_5groups healthy_smoking ///
		alcohol_category alc_weekly total_alc_typ_day_g alcohol_7groups alcohol_5groups healthy_alcohol ///
		diet_freq_meat diet_freq_eggs diet_freq_fresh_fruit diet_freq_fresh_veg diet_5score diet_5groups healthy_diet ///
		bmi_calc bmi_4groups healthy_bmi waist_mm WC_3groups healthy_WC healthy_obesity ///
		sleep_hours sleep_5groups healthy_sleep met has_diseases has_diabetes has_copd acute_mi_diag angina_diag ///
		other_ihd_diag stroke_or_tia_diag hypertension_diag pul_heart_dis_diag rheum_heart_dis_diag tb_diag emphysema_diag ///
		bronchitis_diag copd_diag asthma_diag cirrhosis_hep_diag peptic_ulcer_diag gall_diag kidney_dis_diag osteoporosis_diag ///
		fracture_diag rheum_arthritis_diag depression_diag anxiety_diag neurasthenia_diag other_psych_dis_diag head_injury_diag cancer_diag ///
		has_hyperlipidemia

local varlist1 ""
foreach var of local varlist{

	rename `var' `var'2
	local varlist1 `varlist1' `var'2

}

***keep variable
keep studyid household_income2 smoking_stopped_reason2 `varlist1'
misstable sum studyid household_income2 smoking_stopped_reason2 `varlist1'

save "D:\HanYT\03--生活方式变化\1. Data\dataset_resurvey2.dta",replace

**********************************************************************************************************
****************************           Part 1.3 merge datasets        ************************************
**********************************************************************************************************
/* 基线与结局数据库合并 */
use "D:\HanYT\03--生活方式变化\1. Data\dataset_baseline.dta", clear
merge 1:1 studyid using "D:\HanYT\03--生活方式变化\1. Data\endpoints.dta", keep(match) keepusing(c_ep0002 c_ep0002_date ///
	c_ep0003 c_ep0003_date c_ep0070 c_ep0070_date c_ep0008 c_ep0008_date c_ep0009 c_ep0009_date c_ep0014 c_ep0014_date c_ep0033 c_ep0033_date ///
	c_ep0001 c_ep0001_date du_ep0001 du_ep0001_date c_ep0047 c_ep0047_date c_ep0048 c_ep0048_date c_ep0301 c_ep0301_date ///
	c_ep0302 c_ep0302_date) nogen

*transform date variables
local varlist dob_anon study_date censoring_date c_ep0002_date c_ep0003_date c_ep0070_date c_ep0008_date c_ep0009_date c_ep0014_date ///
	c_ep0033_date c_ep0001_date du_ep0001_date c_ep0047_date c_ep0048_date c_ep0301_date c_ep0302_date
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

***Generate IS+HS (I61、I63)
gen c_ep0011 = 1 if c_ep0008 == 1 | c_ep0009 == 1
	replace c_ep0011 = 0 if c_ep0008 == 0 & c_ep0009 == 0

egen c_ep0011_date = rowmin(c_ep0008_date c_ep0009_date)

format c_ep0088_date c_ep0011_date %td



/* 生成基线数据库 */
preserve
***Physical activity
*baseline
xtile healthy_PA1 = met if is_female == 0 & age_3groups == 0, nq(2)
xtile healthy_PA2 = met if is_female == 0 & age_3groups == 1, nq(2)
xtile healthy_PA3 = met if is_female == 0 & age_3groups == 2, nq(2)
xtile healthy_PA4 = met if is_female == 1 & age_3groups == 0, nq(2)
xtile healthy_PA5 = met if is_female == 1 & age_3groups == 1, nq(2)
xtile healthy_PA6 = met if is_female == 1 & age_3groups == 2, nq(2)
egen healthy_PA = rowmin(healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6)
		 replace healthy_PA = healthy_PA - 1
	label variable healthy_PA "Healthy PA"
	label define healthy_PA 1 "healthy PA:higher half" 0 "healthy PA:lower half"
	label values healthy_PA healthy_PA
drop healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6

***healthy lifestyle score
egen heatlhy_score = rowtotal(healthy_smoking healthy_alcohol healthy_diet healthy_bmi healthy_WC healthy_sleep healthy_PA)

save "D:\HanYT\03--生活方式变化\1. Data\merged_baseline.dta", replace
restore

/* ***首次心血管代谢性疾病
**number of disease during follow-up
egen d_number_inc = rowtotal(c_ep0003 c_ep0009 c_ep0088)
	label variable d_number_inc "Number of incidence diseases under observation"
**first_cmd_inc
recode d_number_inc (0=0 "None") (1/3=1 "1th CMD"), gen(first_cmd_inc)
	label variable first_cmd_inc "First cardiomebolic disease"
egen first_cmd_inc_date = rowmin(c_ep0003_date c_ep0009_date c_ep0088_date)
	label variable first_cmd_inc_date "Date of first cardiomebolic disease"
	format first_cmd_inc_date %td */

/* 再与第二次重复调查数据合并 */
merge 1:1 studyid using "D:\HanYT\03--生活方式变化\1. Data\dataset_resurvey2.dta",keep(match)

***study date of 2resurvey
gen study_date21 = substr(study_date2,1,10)
gen study_date22 = date(study_date21,"YMD",2050)
drop study_date2 study_date21
rename study_date22 study_date2
format study_date2 %td

***生成基线与第二次随访期间出现的结局
local endpoints c_ep0001 c_ep0002 c_ep0003 c_ep0070 c_ep0009 c_ep0014 c_ep0033 c_ep0088 c_ep0011
foreach var of local endpoints{

	gen `var'_d = 1 if `var' == 1 & `var'_date <= study_date2
		replace `var'_d = 0 if `var' == 0 | (`var' == 1 & `var'_date>study_date2)

}

**基线到第二次重复调查中出现四大慢病(没有任何疾病的作为参照)
gen four_disease = 1 if c_ep0002_d == 1 | c_ep0014_d == 1 | c_ep0033_d == 1 | c_ep0088_d == 1
	replace four_disease = 0 if c_ep0001_d == 0
gen ihd_stroke = 1 if c_ep0003_d == 1 | c_ep0070_d == 1
	replace ihd_stroke = 0 if c_ep0001_d == 0
* 出现心血管代谢性共病(2型糖尿病、缺血性脑卒中和缺血性脑卒中、缺血性心脏病)
egen cmd_number_d = rowtotal(c_ep0003_d c_ep0011_d c_ep0088_d)
recode cmd_number_d (0=0 "None") (1=1 "one CMD") (2/3=2 "cardiomebolic multimorbidity"), gen(cmd_number_3groups)
recode cmd_number_d (0=0 "None") (1/3=1 "at least one CMD"), gen(cmd_number_2groups)


**基线到第二次重复调查中出现四大慢病（1：没有四种主要疾病的作为参照）
gen four_disease1 = 1 if c_ep0002_d == 1 | c_ep0014_d == 1 | c_ep0033_d == 1 | c_ep0088_d == 1
	replace four_disease1 = 0 if (c_ep0002_d == 0 & c_ep0014_d == 0 & c_ep0033_d == 0 & c_ep0088_d == 0) 

gen ihd_stroke1 = 1 if c_ep0003_d == 1 | c_ep0070_d == 1
	replace ihd_stroke1 = 0 if c_ep0003_d == 0 & c_ep0070_d == 0


***Physical activity
*baseline
xtile healthy_PA1 = met if is_female == 0 & age_3groups == 0, nq(2)
xtile healthy_PA2 = met if is_female == 0 & age_3groups == 1, nq(2)
xtile healthy_PA3 = met if is_female == 0 & age_3groups == 2, nq(2)
xtile healthy_PA4 = met if is_female == 1 & age_3groups == 0, nq(2)
xtile healthy_PA5 = met if is_female == 1 & age_3groups == 1, nq(2)
xtile healthy_PA6 = met if is_female == 1 & age_3groups == 2, nq(2)
egen healthy_PA = rowmin(healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6)
		 replace healthy_PA = healthy_PA - 1
	label variable healthy_PA "Healthy PA"
	label define healthy_PA 1 "healthy PA:higher half" 0 "healthy PA:lower half"
	label values healthy_PA healthy_PA
drop healthy_PA1 healthy_PA2 healthy_PA3 healthy_PA4 healthy_PA5 healthy_PA6
gen risky_PA = (healthy_PA == 0)
*resurvey2
xtile PA_2groups1 = met2 if is_female == 0 & age_3groups == 0, nq(2)
xtile PA_2groups2 = met2 if is_female == 0 & age_3groups == 1, nq(2)
xtile PA_2groups3 = met2 if is_female == 0 & age_3groups == 2, nq(2)
xtile PA_2groups4 = met2 if is_female == 1 & age_3groups == 0, nq(2)
xtile PA_2groups5 = met2 if is_female == 1 & age_3groups == 1, nq(2)
xtile PA_2groups6 = met2 if is_female == 1 & age_3groups == 2, nq(2)
egen healthy_PA2 = rowmin(PA_2groups1 PA_2groups2 PA_2groups3 PA_2groups4 PA_2groups5 PA_2groups6)
		 replace healthy_PA2 = healthy_PA2 - 1
	label variable healthy_PA2 "Healthy PA"
	label values healthy_PA2 healthy_PA
drop PA_2groups1 PA_2groups2 PA_2groups3 PA_2groups4 PA_2groups5 PA_2groups6
gen risky_PA2 = (healthy_PA2 == 0)

***healthy lifestyle score
egen healthy_score = rowtotal(healthy_smoking healthy_alcohol healthy_diet healthy_bmi healthy_WC healthy_sleep healthy_PA)
egen healthy_score2 = rowtotal(healthy_smoking2 healthy_alcohol2 healthy_diet2 healthy_bmi2 healthy_WC2 healthy_sleep2 healthy_PA2)

gen risky_smoking = (healthy_smoking == 0)
gen risky_alcohol = (healthy_alcohol == 0)
gen risky_diet = (healthy_diet == 0)
gen risky_bmi = (healthy_bmi == 0)
gen risky_WC = (healthy_WC == 0)
gen risky_sleep = (healthy_sleep == 0)

gen risky_smoking2 = (healthy_smoking2 == 0)
gen risky_alcohol2 = (healthy_alcohol2 == 0)
gen risky_diet2 = (healthy_diet2 == 0)
gen risky_bmi2 = (healthy_bmi2 == 0)
gen risky_WC2 = (healthy_WC2 == 0)
gen risky_sleep2 = (healthy_sleep2 == 0)

**********************************************************************************************************
****************************       Part 1.2.2 Change of lifestyles    ************************************
**********************************************************************************************************
/* Single lifestyle */
local lflist healthy_smoking healthy_alcohol healthy_diet healthy_bmi healthy_WC healthy_sleep healthy_PA healthy_obesity
label define change 0 "Stable" 1 "Worsen" 2 "Better"
foreach lf of local lflist{

	gen `lf'_change = 0 if `lf' == `lf'2
		replace `lf'_change = 1 if `lf' == 1 & `lf'2 == 0
		replace `lf'_change = 2 if `lf' == 0 & `lf'2 == 1
	local name: variable label `lf'
	label variable `lf'_change "`name' change"
	label values `lf'_change change

}

/* Lifestyle score */
gen lfscore_change = healthy_score2 - healthy_score
	label variable lfscore_change "Healthy score change"
recode lfscore_change (min/-1=0 "Worsen") (0=1 "Stable") (1/max=2 "Better"), gen(lfscore_change_3groups)
	label variable lfscore_change_3groups "Healthy score change 3groups"

save "D:\HanYT\03--生活方式变化\1. Data\merged_database.dta",replace

***数据库中剔除基线患有四种重大慢性病的人
use "D:\HanYT\03--生活方式变化\1. Data\merged_database.dta", clear
drop if chd_diag == 1
drop if stroke_or_tia_diag == 1
drop if cancer_diag== 1
drop if has_diabetes == 1
save "D:\HanYT\03--生活方式变化\1. Data\merged_database_clean.dta", replace

log close










