use 1.Data/project2_updated_du_ep0002_duration3.dta

list c_ep0010 age_stroke_diag du_ep0001 stroke_diag_updated _t _t0 if c_ep0010 == 1 & du_ep0001 == 1 & c_ep0010_date == du_ep0001_date in 1/5000
list studyid c_ep0010 age_stroke_diag du_ep0001 stroke_diag_updated duration_stroke_updated duration_stroke_updated_8g _t _t0 if c_ep0010 == 1 & du_ep0001 == 1 & c_ep0010_date == du_ep0001_date in 1/5000



stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model} , strata(age_strata region_code)
stcox i.diabetes_diag_updated i.chd_diag_updated i.stroke_diag_updated ${adjusted_model} if d_number_base == 0, strata(age_strata region_code)


stcox i.duration_chd_updated_8g i.duration_stroke_updated_8g i.duration_diabetes_updated_8g ${adjusted_model} , strata(age_strata region_code)
stcox i.duration_chd_updated_8g i.duration_stroke_updated_8g i.duration_diabetes_updated_8g ${adjusted_model} if d_number_base == 0, strata(age_strata region_code)
