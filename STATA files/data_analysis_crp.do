use "/Users/cesarhiggins/Documents/Race_CRP_Dementia/STATA/crp_data_incidence_analysis.dta", clear 

/*Dichotomize Baseline CRP > 75th percentile */

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




* CRP Models at the 75th percentile for overall sample 

* Univariate 
	
	xi: poisson dementia baseline_crp_75, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_1
	
* Dementia and demographics 
	
	xi: poisson dementia baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_2
	
* Dementia and Behaviorals 

	xi: poisson dementia baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_3
	
* Dementia and Chronic Conditions 

	xi: poisson dementia baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic, offset(ln_time) irr cformat(%6.2f) vce(robust)
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
	
	esttab model_* using crp_75_dementia.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	

	
* CRP Models at the 75th percentile for African Americans 

* Univariate

	xi: poisson dementia baseline_crp_75 if race==1, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_1aa 
	
* Dementia and demographics 

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin if race==1, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_2aa 
	
* Dementia and Behaviorals

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke if race==1, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_3aa
	
* Dementia and Chronic Conditions 

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic if race==1, offset(ln_time) irr cformat(%6.2f) vce(robust)
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
	
	esttab model_* using crp_75_dementia_aa.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	
	
* CRP Models at the 75th percentile for Hispanics 

* Univariate

	xi: poisson dementia baseline_crp_75 if race==2, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_1h
	
* Dementia and demographics 

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin if race==2, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_2h 
	
* Dementia and Behaviorals

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke if race==2, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_3h
	
* Dementia and Chronic Conditions 

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic if race==2, offset(ln_time) irr cformat(%6.2f) vce(robust)
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
	
	esttab model_* using crp_75_dementia_h.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	
* CRP Models at the 75th percentile for Whites

* Univariate

	xi: poisson dementia baseline_crp_75 if race==0, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_1w
	
* Dementia and demographics 

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin if race==0, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_2w 
	
* Dementia and Behaviorals

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke if race==0, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_3w
	
* Dementia and Chronic Conditions 

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic if race==0, offset(ln_time) irr cformat(%6.2f) vce(robust)
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
	
	esttab model_* using crp_75_dementia_w.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	
	

* CRP Models at the 75th percentile for Minoritized (Blacks and Hispanics together)
** create minoritized variable 

gen minoritized= .
replace minoritized=1 if race==1 | race==2
replace minoritized=0 if race==0
tab minoritized

* Univariate

	xi: poisson dementia baseline_crp_75 if minoritized==1, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_1m
	
* Dementia and demographics 

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin if minoritized==1, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_2m 
	
* Dementia and Behaviorals

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke if minoritized==1, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_3m
	
* Dementia and Chronic Conditions 

	xi: poisson dementia baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic if minoritized==1, offset(ln_time) irr cformat(%6.2f) vce(robust)
	eststo model_4m
	
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
	
	esttab model_* using crp_75_dementia_m.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	
	

	
** PREDICTING HIGH LEVELS OF THE MEDIATOR 	
** MODELS FOR BLACKS predicting High hsCRP	

* Univariate 

	xi: logistic baseline_crp_75 i.black, cformat(%6.2f) vce(robust)
	eststo model_1
	
* Dementia and demographics 
	
	xi: logistic baseline_crp_75 i.black z_age i.sex i.wave i.edu_cat i.APOE2010_bin, cformat(%6.2f) vce(robust)
	eststo model_2
	
* Dementia and Behaviorals 

	xi: logistic baseline_crp_75 i.black z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke, cformat(%6.2f) vce(robust)
	eststo model_3
	
* Dementia and Chronic Conditions 

	xi: logistic baseline_crp_75 i.black z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic, cformat(%6.2f) vce(robust)
	eststo model_4
	
	lab var baseline_crp_75 "CRP at 75th"
	lab var _Iblack_1 "Black NH"
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
	
	esttab model_* using crp_75_black.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for CRP") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 

** Models for Hispanic predicting High hsCRP	


* Univariate 

	xi: logistic baseline_crp_75 i.hispanic, cformat(%6.2f) vce(robust)
	eststo model_1
	
* Dementia and demographics 
	
	xi: logistic baseline_crp_75 i.hispanic z_age i.sex i.wave i.edu_cat i.APOE2010_bin, cformat(%6.2f) vce(robust)
	eststo model_2
	
* Dementia and Behaviorals 

	xi: logistic baseline_crp_75 i.hispanic z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke, cformat(%6.2f) vce(robust)
	eststo model_3
	
* Dementia and Chronic Conditions 

	xi: logistic baseline_crp_75 i.hispanic z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic, cformat(%6.2f) vce(robust)
	eststo model_4
	
	lab var baseline_crp_75 "CRP at 75th"
	lab var _Ihispanic_1 "Hispanic"
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
	
	esttab model_* using crp_75_hispanic.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for CRP") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	
	
** Models for Minoritized Social group predicting High hsCRP	
	
	gen minority=0 if black==0 
	replace minority=1 if black==1 | hispanic==1
	tab minority

* Univariate 

	xi: logistic baseline_crp_75 i.minority, cformat(%6.2f) vce(robust)
	eststo model_1
	
* Dementia and demographics 
	
	xi: logistic baseline_crp_75 i.minority z_age i.sex i.wave i.edu_cat i.APOE2010_bin, cformat(%6.2f) vce(robust)
	eststo model_2
	
* Dementia and Behaviorals 

	xi: logistic baseline_crp_75 i.minority z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke, cformat(%6.2f) vce(robust)
	eststo model_3
	
* Dementia and Chronic Conditions 

	xi: logistic baseline_crp_75 i.minority z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoke chronic, cformat(%6.2f) vce(robust)
	eststo model_4
	
	lab var baseline_crp_75 "CRP at 75th"
	lab var _Iminority_1 "Minoritized"
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
	
	esttab model_* using crp_75_minority.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for CRP") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	
	
	
/*Save this file for mediation analysis later */

/*Generate first a dummy for education */

tab edu_cat
tab edu_cat, nolabel 

tab edu_cat , nolabel 
gen edu_cat_1 = 1 if edu_cat==2
replace edu_cat_1=0 if edu_cat==3 | edu_cat==1
tab edu_cat_1

gen edu_cat_2 = 1 if edu_cat==3 
replace edu_cat_2=0 if edu_cat== 2 | edu_cat==1
tab edu_cat_2

/*For mediation models we used chronic conditions dichotomized as 0 no chronic or >1, but we also did so for the regression models*/

gen chronic_2 =. 
replace chronic_2=0 if chronic==0
replace chronic_2=1 if chronic>=1
tab chronic_2


drop _Iminority_1 _Isex_2 _Iwave_2008 _Iedu_cat_2 _Iedu_cat_3 _IAPOE2010__1 _Ismoke_1 chronic

rename chronic_2 chronic

*save "/Users/cesarhiggins/Documents/CRP Dementia/STATA/crp_data_incidence_mediation.dta", replace


