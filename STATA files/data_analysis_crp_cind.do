use "/Users/cesarhiggins/Documents/CRP_Dementia_8-9-23/STATA/crp_data_cind_incidence_analysis.dta", clear 


/*Dichotomize Baseline CRP > 75th percentile */
/* THE 75TH PERCENTILE FOR THIS GROUP CUT-OFF POINT WAS 4.98*/

local varslist baseline_crp 
foreach v in `varslist' {
sum `v', detail
foreach n of numlist 75 {
gen byte `v'_`n' = `v' >= `r(p`n')' 
}
}	


tab baseline_crp_75 /* CRP > 75th then N=1,288, this is equal to the N in supplemental table 4*/

/*Standardize continous variables age, bmi, and alcohol consumption*/

local vars age bmi alcohol 
foreach v in `vars' {
sum `v'
gen double z_`v' = (`v'-r(mean))/r(sd)
}
sum z_*

* CRP Models at the 75th percentile for overall sample with CIND cases 

* Univariate 
	
	xi: poisson cind baseline_crp_75, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_1
	
* CIND and demographics 
	
	xi: poisson cind baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_2
	
* CIND and Behaviorals 

	xi: poisson cind baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_3
	
* CIND and Chronic Conditions 

	xi: poisson cind baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_4
	
	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Irace_1 "Black-NH"
	lab var _Irace_2 "Hispanic"
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoke_1 "Former/Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"
	
	esttab model_* using crp_75_cind.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for CIND") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	
	
	
* CRP Models at the 75th percentile for African Americans 

* Univariate

	xi: poisson cind baseline_crp_75 if race==1, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_1aa 
	
* CIND and demographics 

	xi: poisson cind baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin if race==1, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_2aa 
	
* CIND and Behaviorals

	xi: poisson cind baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke if race==1, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_3aa
	
* CIND and Chronic Conditions 

	xi: poisson cind baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic if race==1, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_4aa
	
	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoke_1 "Former/Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"
	
	esttab model_* using crp_75_cind_aa.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for CIND") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	
	
* CRP Models at the 75th percentile for Hispanics 

* Univariate

	xi: poisson cind baseline_crp_75 if race==2, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_1h
	
* Dementia and demographics 

	xi: poisson cind baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin if race==2, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_2h 
	
* Dementia and Behaviorals

	xi: poisson cind baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke if race==2, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_3h
	
* Dementia and Chronic Conditions 

	xi: poisson cind baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic if race==2, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	
	eststo model_4h
	
	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoke_1 "Former/Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"
	
	esttab model_* using crp_75_cind_h.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for CIND") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	
	
* CRP Models at the 75th percentile for Whites

* Univariate

	xi: poisson cind baseline_crp_75 if race==0, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_1w
	
* Dementia and demographics 

	xi: poisson cind baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin if race==0, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_2w 
	
* Dementia and Behaviorals

	xi: poisson cind baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke if race==0, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_3w
	
* Dementia and Chronic Conditions 

	xi: poisson cind baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic if race==0, offset(ln_time_cind) irr cformat(%6.2f) vce(robust)
	eststo model_4w
	
	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoke_1 "Former/Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"
	
	esttab model_* using crp_75_cind_w.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for CIND") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
