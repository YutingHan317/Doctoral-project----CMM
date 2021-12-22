
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/* Do not change the status of CMDs if the CMDs occured 0.5 year before death */

local elist du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999
foreach e of local elist{

	use "1.Data/project2.dta", clear
	* 编号4711627的研究对象，基线患有三种疾病，但都没有调对应的时间
	drop if (chd_diag == 1 & chd_diag_age ==.) |  (stroke_or_tia_diag == 1 & stroke_or_tia_diag_age ==.) | (diabetes_diag == 1 & diabetes_diag_age == .) 

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
		* 2021-10-16 exlude the change in disease status; all-cause death; 789
		replace chd_diag_updated = 0 if chd_diag_updated == 1 & `e' == 1 & age_chd_diag >=  (_t - 0.5) & age_chd_diag < _t
		label variable chd_diag_updated "Updated ischemis heart disease status"
		label values chd_diag_updated baseline_disease

	* MI
	gen MI_diag_updated = 0 if chd_diag == 0 & c_ep0068 == 0
		replace MI_diag_updated = 1 if chd_diag == 1 
		replace MI_diag_updated = 1 if chd_diag == 0 & c_ep0068 == 1 & age_MI_diag >= _t0 & age_MI_diag < _t
		replace MI_diag_updated = 1 if chd_diag == 0 & c_ep0068 == 1 & age_MI_diag < _t0
		replace MI_diag_updated = 0 if chd_diag == 0 & c_ep0068 == 1 & age_MI_diag >= _t
		* 2021-10-16 exlude the change in disease status; all-cause death; 789
		replace MI_diag_updated = 0 if MI_diag_updated == 1 & `e' == 1 & age_MI_diag >=  (_t - 0.5) & age_MI_diag < _t
		label variable MI_diag_updated "Updated Myocardial infarction status"
		label values MI_diag_updated baseline_disease

	gen stroke_diag_updated = 0 if stroke_or_tia_diag == 0 & c_ep0010 == 0
		replace stroke_diag_updated = 1 if stroke_or_tia_diag == 1 
		replace stroke_diag_updated = 1 if stroke_or_tia_diag == 0 & c_ep0010 == 1 & age_stroke_diag >= _t0 & age_stroke_diag < _t
		replace stroke_diag_updated = 1 if stroke_or_tia_diag == 0 & c_ep0010 == 1 & age_stroke_diag < _t0 
		replace stroke_diag_updated = 0 if stroke_or_tia_diag == 0 & c_ep0010 == 1 & age_stroke_diag >= _t
		* 2021-10-16 exlude the change in disease status; all-cause death; 1,780
		replace stroke_diag_updated = 0 if stroke_diag_updated == 1 & `e' == 1 & age_stroke_diag >= (_t - 0.5) & age_stroke_diag < _t
		label variable stroke_diag_updated "Updated stroke status"
		label values stroke_diag_updated baseline_disease

	* IS
	gen IS_diag_updated = 0 if stroke_or_tia_diag == 0 & c_ep0009 == 0
		replace IS_diag_updated = 1 if stroke_or_tia_diag == 1 
		replace IS_diag_updated = 1 if stroke_or_tia_diag == 0 & c_ep0009 == 1 & age_IS_diag >= _t0 & age_IS_diag < _t
		replace IS_diag_updated = 1 if stroke_or_tia_diag == 0 & c_ep0009 == 1 & age_IS_diag < _t0 
		replace IS_diag_updated = 0 if stroke_or_tia_diag == 0 & c_ep0009 == 1 & age_IS_diag >= _t
		* 2021-10-16 exlude the change in disease status; all-cause death; 1,780
		replace IS_diag_updated = 0 if IS_diag_updated == 1 & `e' == 1 & age_IS_diag >= (_t - 0.5) & age_IS_diag < _t
		label variable IS_diag_updated "Updated ischemic stroke status"
		label values IS_diag_updated baseline_disease

	* HS
	gen HS_diag_updated = 0 if stroke_or_tia_diag == 0 & c_ep0008 == 0
		replace HS_diag_updated = 1 if stroke_or_tia_diag == 1 
		replace HS_diag_updated = 1 if stroke_or_tia_diag == 0 & c_ep0008 == 1 & age_HS_diag >= _t0 & age_HS_diag < _t
		replace HS_diag_updated = 1 if stroke_or_tia_diag == 0 & c_ep0008 == 1 & age_HS_diag < _t0 
		replace HS_diag_updated = 0 if stroke_or_tia_diag == 0 & c_ep0008 == 1 & age_HS_diag >= _t
		* 2021-10-16 exlude the change in disease status; all-cause death; 1,780
		replace HS_diag_updated = 0 if HS_diag_updated == 1 & `e' == 1 & age_HS_diag >= (_t - 0.5) & age_HS_diag < _t
		label variable HS_diag_updated "Updated hemorrhagic stroke status"
		label values HS_diag_updated baseline_disease

	gen diabetes_diag_updated = 0 if has_diabetes == 0 & c_ep0101 == 0
		replace diabetes_diag_updated = 1 if has_diabetes == 1 
		replace diabetes_diag_updated = 1 if has_diabetes == 0 & c_ep0101 == 1 & age_diabetes_diag >= _t0 & age_diabetes_diag < _t
		replace diabetes_diag_updated = 1 if has_diabetes == 0 & c_ep0101 == 1 & age_diabetes_diag < _t0
		replace diabetes_diag_updated = 0 if has_diabetes == 0 & c_ep0101 == 1 & age_diabetes_diag >= _t
		* 2021-10-16 exlude the change in disease status; all-cause death; 150
		replace diabetes_diag_updated = 0 if diabetes_diag_updated == 1 & `e' == 1 & age_diabetes_diag >= (_t - 0.5) & age_diabetes_diag < _t
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

	*** Multimorbidity 2021/11/17
	gen cmm_updated = 1 if d_number_updated >=2
		replace cmm_updated = 0 if mi(cmm_updated)
		label variable cmm_updated "Updated Multimorbidity 2 groups"
		label values cmm_updated mltmbd

	*** free from CMD 2021/11/17
	recode d_number_updated (0=0 "free from cmd") (1/3=1 "least one"), gen(fcmd_updated)
		label variable fcmd_updated "Updated first CMD"	

	*** 0 1 >=2 2021/11/23
	recode d_number_updated (0=0 "Free from CMDs") (1=1 "Only one CMD") (2/max = 2 "CMM"), gen(d_number2_updated)
		label variable d_number2_updated "Updated disease status"

	*** FCMD other than a CMD
	gen fcmd_notdm_updated = 1 if chd_diag_updated == 1 | stroke_diag_updated == 1
		replace fcmd_notdm_updated = 0 if mi(fcmd_notdm_updated)

	gen fcmd_notchd_updated = 1 if diabetes_diag_updated == 1 | stroke_diag_updated == 1
		replace fcmd_notchd_updated = 0 if mi(fcmd_notchd_updated)

	gen fcmd_notstroke_updated = 1 if diabetes_diag_updated == 1 | chd_diag_updated == 1
		replace fcmd_notstroke_updated = 0 if mi(fcmd_notstroke_updated)


	*** Description
	tab1 diabetes_diag_updated chd_diag_updated stroke_diag_updated d_number_updated combin_updated
	tab chd_diag chd_diag_updated
	tab stroke_or_tia_diag stroke_diag_updated
	tab has_diabetes diabetes_diag_updated
	tab d_number_updated
	tab d_number_base d_number_updated
	tab combin_updated
	tab combin_base combin_updated


	save "1.Data/project2_updated_`e'_nochghalfy",replace

}



///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/* Do not change the status of CMDs if the CMDs occured <= 30 days before death */

/* 方法一: 先按照发生CMD时间切割，再切割每年，计算duration的参考时间点是_t */

* 按照0.5年切割
local elist du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999
foreach e of local elist{

	use "1.Data/project2_updated_`e'_nochghalfy.dta", clear

	stsplit aage, every(0.5)


	*** Generate Updated duration of disease
	* Duration
	gen duration_chd_updated = _t - chd_diag_age if chd_diag == 1 & (_t - chd_diag_age) > 0    // 对于基线就已经患病的人;且日期纪录合理的
		replace duration_chd_updated = _t - age_at_study_date if chd_diag == 1 & (_t - chd_diag_age) <= 0 // 对于基线就已经患病，但纪录发病日期早于调查开始天数
		replace duration_chd_updated = 1/365.25 if duration_chd_updated == 0 // 上述两步处理仍有0的人，chd为0
		replace duration_chd_updated = _t - age_chd_diag if chd_diag == 0 & chd_diag_updated == 1 & age_chd_diag <= _t0
		replace duration_chd_updated  = 0 if chd_diag_updated == 0 
		label variable duration_chd_updated "Updated Duration of chronic heart disease" 

	gen duration_MI_updated = _t - chd_diag_age if chd_diag == 1 & (_t - chd_diag_age) > 0    // 对于基线就已经患病的人;且日期纪录合理的
		replace duration_MI_updated = _t - age_at_study_date if chd_diag == 1 & (_t - chd_diag_age) <= 0 // 对于基线就已经患病，但纪录发病日期早于调查开始天数
		replace duration_MI_updated = 1/365.25 if duration_MI_updated == 0 // 上述两步处理仍有0的人，chd为0
		replace duration_MI_updated = _t - age_MI_diag if chd_diag == 0 & MI_diag_updated == 1 & age_MI_diag <= _t0
		replace duration_MI_updated  = 0 if MI_diag_updated == 0 
		label variable duration_MI_updated "Updated Duration of MI" 

	gen duration_diabetes_updated = _t - diabetes_diag_age if diabetes_diag == 1 & (_t - diabetes_diag_age) > 0
		replace duration_diabetes_updated = _t - age_at_study_date if diabetes_diag == 1 & (_t - diabetes_diag_age) <= 0
		replace duration_diabetes_updated = _t - age_at_study_date if has_diabetes == 1 & diabetes_diag == 0
		replace duration_diabetes_updated = 1/365.25 if duration_diabetes_updated == 0 //上述三步处理仍有0的人，diabetes 为24人
		replace duration_diabetes_updated = _t - age_diabetes_diag if has_diabetes == 0 & diabetes_diag_updated == 1 & age_diabetes_diag <= _t0
		replace duration_diabetes_updated = 0 if diabetes_diag_updated == 0 
		label variable duration_diabetes_updated "Updated Duration of diabetes"

	gen duration_stroke_updated =  _t - stroke_or_tia_diag_age if stroke_or_tia_diag == 1 & (_t - stroke_or_tia_diag_age) > 0 
		replace duration_stroke_updated =  _t - age_at_study_date if stroke_or_tia_diag == 1 & (_t - stroke_or_tia_diag_age) <= 0 
		replace duration_stroke_updated =  1/365.25 if duration_stroke_updated == 0 //  上述两步处理仍有0的人，diabetes 为1人
		replace duration_stroke_updated = _t - age_stroke_diag if stroke_or_tia_diag == 0 & stroke_diag_updated==1 & age_stroke_diag <= _t0
		replace duration_stroke_updated = 0 if stroke_diag_updated== 0
		label variable duration_stroke_updated "Updated Duration of stroke or tia"

	gen duration_IS_updated =  _t - stroke_or_tia_diag_age if stroke_or_tia_diag == 1 & (_t - stroke_or_tia_diag_age) > 0 
		replace duration_IS_updated =  _t - age_at_study_date if stroke_or_tia_diag == 1 & (_t - stroke_or_tia_diag_age) <= 0 
		replace duration_IS_updated =  1/365.25 if duration_IS_updated == 0 //  上述两步处理仍有0的人，diabetes 为1人
		replace duration_IS_updated = _t - age_IS_diag if stroke_or_tia_diag == 0 & IS_diag_updated==1 & age_IS_diag <= _t0
		replace duration_IS_updated = 0 if IS_diag_updated== 0
		label variable duration_IS_updated "Updated Duration of ischemic stroke"

	gen duration_HS_updated =  _t - stroke_or_tia_diag_age if stroke_or_tia_diag == 1 & (_t - stroke_or_tia_diag_age) > 0 
		replace duration_HS_updated =  _t - age_at_study_date if stroke_or_tia_diag == 1 & (_t - stroke_or_tia_diag_age) <= 0 
		replace duration_HS_updated =  1/365.25 if duration_HS_updated == 0 //  上述两步处理仍有0的人，diabetes 为1人
		replace duration_HS_updated = _t - age_HS_diag if stroke_or_tia_diag == 0 & HS_diag_updated==1 & age_HS_diag <= _t0
		replace duration_HS_updated = 0 if HS_diag_updated== 0
		label variable duration_HS_updated "Updated Duration of hemorrhagic stroke"
	
	* 2021/11/17
	gen double duration_fcmd_updated = max(duration_diabetes_updated, duration_chd_updated, duration_stroke_updated)
		label variable duration_fcmd_updated "Updated duration of First CMD"

	* 2021/11/17
	egen double duration_cmm_updated = rowmedian(duration_diabetes_updated duration_chd_updated duration_stroke_updated) if d_number_updated == 3
		replace duration_cmm_updated = max(duration_diabetes_updated, duration_chd_updated, duration_stroke_updated) if d_number_updated == 2
		replace duration_cmm_updated = 0 if d_number_updated < 2
		label variable duration_cmm_updated "Updated duration of CMM" 
	

	*** Updated duration groups
	*** duration groups
	egen duration_chd_updated_groups = cut(duration_chd_updated), at(0 0.00001 0.5 1 2(2)30 90) label
		label variable duration_chd_updated_groups "Updated duration of chd"

	egen duration_stroke_updated_groups = cut(duration_stroke_updated), at(0 0.00001 0.5 1 2(2)30 90) label
		label variable duration_stroke_updated_groups "Updated duration of stroke"

	egen duration_diabetes_updated_groups = cut(duration_diabetes_updated), at(0 0.00001 0.5 1 2(2)30 90) label
		label variable duration_diabetes_updated_groups "Updated duration of diabetes"


	*** Duration 7 groups
	egen duration_chd_updated_7g = cut(duration_chd_updated), at(0 0.00001 5(5)30 90) label
		label variable duration_chd_updated_7g "Updated duration of chd 7groups"

	egen duration_stroke_updated_7g = cut(duration_stroke_updated), at(0 0.00001 5(5)30 90) label
		label variable duration_stroke_updated_7g "Updated duration of stroke 7groups"

	egen duration_diabetes_updated_7g = cut(duration_diabetes_updated), at(0 0.00001 5(5)30 90) label
		label variable duration_diabetes_updated_7g "Updated duration of diabetes 7groups"

	*** Duration 8 groups  20210913
	egen duration_chd_updated_8g = cut(duration_chd_updated), at(0 0.00001 1 5(5)30 90) label
		label variable duration_chd_updated_8g "Updated duration of chd 8groups"

	egen duration_MI_updated_8g = cut(duration_MI_updated), at(0 0.00001 1 5(5)30 90) label
		label variable duration_MI_updated_8g "Updated duration of MI 8groups"

	egen duration_stroke_updated_8g = cut(duration_stroke_updated), at(0 0.00001 1 5(5)30 90) label
		label variable duration_stroke_updated_8g "Updated duration of stroke 8groups"

	egen duration_IS_updated_8g = cut(duration_IS_updated), at(0 0.00001 1 5(5)30 90) label
		label variable duration_IS_updated_8g "Updated duration of ischemic stroke 8groups"

	egen duration_HS_updated_8g = cut(duration_HS_updated), at(0 0.00001 1 5(5)30 90) label
		label variable duration_HS_updated_8g "Updated duration of hemorrhagic stroke 8groups"

	egen duration_diabetes_updated_8g = cut(duration_diabetes_updated), at(0 0.00001 1 5(5)30 90) label
		label variable duration_diabetes_updated_8g "Updated duration of diabetes 8groups"

	* 2021/11/17
	egen duration_fcmd_updated_8g = cut(duration_fcmd_updated), at(0 0.00001 1 5(5)30 90) label
		label variable duration_fcmd_updated_8g "Updated duration of fcmd 8groups"

	* 2021/11/17
	egen duration_cmm_updated_8g = cut(duration_cmm_updated), at(0 0.00001 1 5(5)30 90) label
		label variable duration_cmm_updated_8g "Updated duration of CMM 8groups"


	save "1.Data/project2_updated_`e'_duration3_nochghalfy.dta", replace 
}
