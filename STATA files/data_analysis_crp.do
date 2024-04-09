use "/Users/cesarhiggins/Documents/CRP_Dementia_8-9-23/STATA/crp_data_incidence_analysis_8-30-23.dta", clear 


/*This is the sensitivity analysis */

/*Dichotomize Baseline CRP > 75th percentile, we use the distribution of crp at the sample of the study dementia_2 */


sum baseline_crp if dementia_2!=., detail 




/* because this is the distribution of crp in the new sample of 6,908. We use this as the new cut_off point*/

gen baseline_crp_75 = .
replace baseline_crp_75=1 if baseline_crp>=4.73
replace baseline_crp_75=0 if baseline_crp<4.73
tab baseline_crp_75

tab baseline_crp_75 dementia         /* CRP>75 N = 1,257  */
tab baseline_crp_75 dementia_2		 /* CRP>75 N = 1,748  */
tab baseline_crp_75 dementia_3 		 /* CRP>75 N = 1,904  */

drop baseline_crp_75

/*Seeing if the loop creates the same results */

local varslist baseline_crp 
foreach v in `varslist' {
sum `v' if dementia_2!=., detail
foreach n of numlist 75 {
gen byte `v'_`n' = `v' >= `r(p`n')' 
}
}	

tab baseline_crp_75 dementia         /* CRP>75 N = 1,257  */
tab baseline_crp_75 dementia_2		 /* CRP>75 N = 1,748  */
tab baseline_crp_75 dementia_3 		 /* CRP>75 N = 1,904  */

/* I optain the same results just by making the cutoff of CRP the 75th percentile of the sample at 6,908 which is dementia_2 meaning including cind in normal category */

* this baseline crp dichotomization is made at the distribution of the sample of 6,908 which became the new sample size other sample sizes are used as sensitivity models 

tab baseline_crp_75 /* CRP > 75th then N=2,197 this is # changed since the sample size for the entire sample changed but it is equal to the numbers we got in R for the descriptives tables */

/*Standardize continous variables age, bmi, and alcohol consumption*/

local vars age bmi alcohol 
foreach v in `vars' {
sum `v' 
gen double z_`v' = (`v'-r(mean))/r(sd) 
}
sum z_*

sum z_* if dementia_2!=. /*very much still standardized mean virtually 0, and SD virtually 1*/

* CRP Models at the 75th percentile for overall sample (N = 5,143) treating dementia excluding CIND "in reference to normal"


***********************************************************
***** OVERALL MODELS, DEMENTIA OUTCOME EXCLUDING CIND *****
***********************************************************


xi: poisson dementia baseline_crp_75, offset(ln_time) irr cformat(%6.2f) vce(robust)
xi: poisson dementia baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin, offset(ln_time) irr cformat(%6.2f) vce(robust)
xi: poisson dementia baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking, offset(ln_time) irr cformat(%6.2f) vce(robust)
xi: poisson dementia baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking c.chronic, offset(ln_time) irr cformat(%6.2f) vce(robust)


* Define the list of covariate sets 
local unadjusted_varlist baseline_crp_75
local demographic_varlist baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin
local behavioral_varlist baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking
local chronic_varlist baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic" 

* Specify your dependent variable
local dependent_var dementia


* Loop through each variable group
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'" 
    if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
    if "`var_group'" == "behavioral" local current_varlist "`behavioral_varlist'"
    if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
	eststo model_`var_group': xi: poisson `dependent_var' `current_varlist', offset(ln_time) irr cformat(%6.2f) vce(robust) ///

}

	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Irace_1 "Black-NH"
	lab var _Irace_2 "Hispanic"
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_1.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 


**********************************************************************************
***** OVERALL MODELS, DEMENTIA OUTCOME INCLUDING CIND IN THE NORMAL CATEGORY *****
**********************************************************************************
	
	
* CRP model overall sample including CIND cases in the normal category 

xi: poisson dementia_2 baseline_crp_75, offset(ln_time_2) irr cformat(%6.2f) vce(robust)
xi: poisson dementia_2 baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin, offset(ln_time_2) irr cformat(%6.2f) vce(robust)
xi: poisson dementia_2 baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking, offset(ln_time_2) irr cformat(%6.2f) vce(robust)
xi: poisson dementia_2 baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking c.chronic, offset(ln_time_2) irr cformat(%6.2f) vce(robust)
	

* Define the list of covariate sets 
local unadjusted_varlist baseline_crp_75
local demographic_varlist baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin
local behavioral_varlist baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking
local chronic_varlist baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic" 

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia_2 that includes the cind cases in the normal category ****

local dependent_var dementia_2


* Loop through each variable group
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'" 
    if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
    if "`var_group'" == "behavioral" local current_varlist "`behavioral_varlist'"
    if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
	eststo model_`var_group': xi: poisson `dependent_var' `current_varlist', offset(ln_time_2) irr cformat(%6.2f) vce(robust) ///

}

	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Irace_1 "Black-NH"
	lab var _Irace_2 "Hispanic"
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_2.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	
	
	
************************************************************************************
***** OVERALL MODELS, DEMENTIA OUTCOME INCLUDING CIND IN THE DEMENTIA CATEGORY *****
************************************************************************************
	
	
* CRP model overall sample including CIND cases in the dementia category 	
	
xi: poisson dementia_3 baseline_crp_75, offset(ln_time_3) irr cformat(%6.2f) vce(robust)
xi: poisson dementia_3 baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin, offset(ln_time_3) irr cformat(%6.2f) vce(robust)
xi: poisson dementia_3 baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking, offset(ln_time_3) irr cformat(%6.2f) vce(robust)
xi: poisson dementia_3 baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking c.chronic, offset(ln_time_3) irr cformat(%6.2f) vce(robust)

* Define the list of covariate sets 
local unadjusted_varlist baseline_crp_75
local demographic_varlist baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin
local behavioral_varlist baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking
local chronic_varlist baseline_crp_75 z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic" 

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia_2 that includes the cind cases in the normal category ****

local dependent_var dementia_3

* Loop through each variable group

foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'" 
    if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
    if "`var_group'" == "behavioral" local current_varlist "`behavioral_varlist'"
    if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
	eststo model_`var_group': xi: poisson `dependent_var' `current_varlist', offset(ln_time_3) irr cformat(%6.2f) vce(robust) ///

}

	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Irace_1 "Black-NH"
	lab var _Irace_2 "Hispanic"
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_3.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 	

	
	
**********************************************************************************************************
***** OVERALL MODELS, DEMENTIA OUTCOME INCLUDING CIND IN THE NORMAL CATEGORY AND CRP-LOG TRANSFORMED *****
**********************************************************************************************************

gen baseline_crp_log = log(baseline_crp)
histogram baseline_crp_log
histogram baseline_crp	

sum baseline_crp_log baseline_crp

local vars baseline_crp_log
foreach v in `vars' {
sum `v' 
gen double z_`v' = (`v'-r(mean))/r(sd) 
}
sum z_*

* CRP model overall sample including CIND cases in the normal category 

xi: poisson dementia_2 z_baseline_crp_log, offset(ln_time_2) irr cformat(%6.2f) vce(robust)
xi: poisson dementia_2 z_baseline_crp_log z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin, offset(ln_time_2) irr cformat(%6.2f) vce(robust)
xi: poisson dementia_2 z_baseline_crp_log z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking, offset(ln_time_2) irr cformat(%6.2f) vce(robust)
xi: poisson dementia_2 z_baseline_crp_log z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking c.chronic, offset(ln_time_2) irr cformat(%6.2f) vce(robust)
	

* Define the list of covariate sets 
local unadjusted_varlist z_baseline_crp_log
local demographic_varlist z_baseline_crp_log z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin
local behavioral_varlist z_baseline_crp_log z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking
local chronic_varlist z_baseline_crp_log z_age i.sex i.race i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic" 

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia_2 that includes the cind cases in the normal category ****

local dependent_var dementia_2


* Loop through each variable group
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'" 
    if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
    if "`var_group'" == "behavioral" local current_varlist "`behavioral_varlist'"
    if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
	eststo model_`var_group': xi: poisson `dependent_var' `current_varlist', offset(ln_time_2) irr cformat(%6.2f) vce(robust) ///

}

	lab var z_baseline_crp_log "Z - Log CRP"
	lab var _Isex_2 "Male" 
	lab var _Irace_1 "Black-NH"
	lab var _Irace_2 "Hispanic"
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_log.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic + APOE-e4" "Risk Factors" "Chronic Conditions")
	
	
	eststo clear 
	
	
	
	
	
******************************************************************************************************************	
**** CRP Models at the 75th percentile BY RACE (STRATIFIED MODELS) these models are excluding cind cases *********
******************************************************************************************************************


* Define the list of covariate sets 
local unadjusted_varlist baseline_crp_75
local demographic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin 
local behavioral_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking 
local chronic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic"

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia that includes the cind cases in the normal category ****

local dependent_var dementia

* Loop through each variable group and racial group 
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'"
	if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
	if "`var_group'" == "behavioral" local current_varlist	"`behavioral_varlist'"
	if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
foreach i in 0 1 2 {
	eststo model_`var_group'_`i': xi: poisson `dependent_var' `current_varlist' if race == `i', offset(ln_time) irr cformat(%6.2f) vce(robust) 
}
}

	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_4.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic")
	
	
	eststo clear 
	

	

***************************************************************************************************************************************************	
**** CRP Models at the 75th percentile BY RACE (STRATIFIED MODELS) these models are including cind cases in the normal cognition category *********
***************************************************************************************************************************************************


* Define the list of covariate sets 
local unadjusted_varlist baseline_crp_75
local demographic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin 
local behavioral_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking 
local chronic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic"

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia that includes the cind cases in the normal category ****

local dependent_var dementia_2

* Loop through each variable group and racial group 
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'"
	if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
	if "`var_group'" == "behavioral" local current_varlist	"`behavioral_varlist'"
	if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
foreach i in 0 1 2 {
	eststo model_`var_group'_`i': xi: poisson `dependent_var' `current_varlist' if race == `i', offset(ln_time_2) irr cformat(%6.2f) vce(robust) 
}
}

	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_5.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic")
	
	
	eststo clear 
	

***************************************************************************************************************************************************	
**** CRP Models at the 75th percentile BY RACE (STRATIFIED MODELS) these models are including cind cases in the dementia category *****************
***************************************************************************************************************************************************


* Define the list of covariate sets 
local unadjusted_varlist baseline_crp_75
local demographic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin 
local behavioral_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking 
local chronic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic"

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia that includes the cind cases in the normal category ****

local dependent_var dementia_3

* Loop through each variable group and racial group 
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'"
	if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
	if "`var_group'" == "behavioral" local current_varlist	"`behavioral_varlist'"
	if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
foreach i in 0 1 2 {
	eststo model_`var_group'_`i': xi: poisson `dependent_var' `current_varlist' if race == `i', offset(ln_time_3) irr cformat(%6.2f) vce(robust) 
}
}

	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_6.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic")
	
	
	eststo clear 
	

	
***************************************************************************************************************************************************	
**** CRP Models at the 75th percentile BY RACE (STRATIFIED MODELS) these models /// ***************************************************************
*************************** are including cind cases in the normal cognition category AND LOG C-REACTIVE PROTEN  **********************************
***************************************************************************************************************************************************


* Define the list of covariate sets 
local unadjusted_varlist z_baseline_crp_log
local demographic_varlist z_baseline_crp_log z_age i.sex i.wave i.edu_cat i.APOE2010_bin 
local behavioral_varlist z_baseline_crp_log z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking 
local chronic_varlist z_baseline_crp_log z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic"

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia that includes the cind cases in the normal category ****

local dependent_var dementia_2

* Loop through each variable group and racial group 
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'"
	if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
	if "`var_group'" == "behavioral" local current_varlist	"`behavioral_varlist'"
	if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
foreach i in 0 1 2 {
	eststo model_`var_group'_`i': xi: poisson `dependent_var' `current_varlist' if race == `i', offset(ln_time_2) irr cformat(%6.2f) vce(robust) 
}
}

	lab var z_baseline_crp_log "Z log CRP"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_log_race.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic" "NH-White" "NH-Black" "Hispanic")
	
	
	eststo clear 
	

**************************************************************************************************
***************************** MINORITIZED MODELS, DEMENTIA ONLY **********************************
**************************************************************************************************



** Models for Minoritized Social group predicting High hsCRP	
	
	gen minority=0 if black==0 
	replace minority=1 if black==1 | hispanic==1
	tab minority

	
* Define the list of covariate sets 
local unadjusted_varlist baseline_crp_75
local demographic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin 
local behavioral_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking 
local chronic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic"

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia that includes the cind cases in the normal category ****

local dependent_var dementia

* Loop through each variable group and racial group 
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'"
	if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
	if "`var_group'" == "behavioral" local current_varlist	"`behavioral_varlist'"
	if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
	eststo model_`var_group'_`i': xi: poisson `dependent_var' `current_varlist' if minority == 1 , offset(ln_time) irr cformat(%6.2f) vce(robust) 
}


	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_7.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic" "Behavioral" "Chronic")
	
	
	eststo clear 
	

***************************************************************************************************************************
***************************** MINORITIZED MODELS, CIND CASES INCLUDED IN NORMAL CATEGORY **********************************
***************************************************************************************************************************
	
* Define the list of covariate sets 
local unadjusted_varlist baseline_crp_75
local demographic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin 
local behavioral_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking 
local chronic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic"

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia that includes the cind cases in the normal category ****

local dependent_var dementia_2

* Loop through each variable group and racial group 
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'"
	if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
	if "`var_group'" == "behavioral" local current_varlist	"`behavioral_varlist'"
	if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
	eststo model_`var_group'_`i': xi: poisson `dependent_var' `current_varlist' if minority == 1 , offset(ln_time_2) irr cformat(%6.2f) vce(robust) 
}


	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_8.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic" "Behavioral" "Chronic")
	
	
	eststo clear 
	

	
*****************************************************************************************************************************
***************************** MINORITIZED MODELS, CIND CASES INCLUDED IN DEMENTIA CATEGORY **********************************
*****************************************************************************************************************************
	
* Define the list of covariate sets 
local unadjusted_varlist baseline_crp_75
local demographic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin 
local behavioral_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking 
local chronic_varlist baseline_crp_75 z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic"

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia that includes the cind cases in the normal category ****

local dependent_var dementia_3

* Loop through each variable group and racial group 
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'"
	if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
	if "`var_group'" == "behavioral" local current_varlist	"`behavioral_varlist'"
	if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
	eststo model_`var_group'_`i': xi: poisson `dependent_var' `current_varlist' if minority == 1 , offset(ln_time_3) irr cformat(%6.2f) vce(robust) 
}


	lab var baseline_crp_75 "CRP at 75th"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_9.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic" "Behavioral" "Chronic")
	
	
	eststo clear 
	
	
***************************************************************************************************************************
***************************** MINORITIZED MODELS, CIND CASES INCLUDED IN NORMAL CATEGORY **********************************
***************************************************************************************************************************
	
* Define the list of covariate sets 
local unadjusted_varlist z_baseline_crp_log
local demographic_varlist z_baseline_crp_log z_age i.sex i.wave i.edu_cat i.APOE2010_bin 
local behavioral_varlist z_baseline_crp_log z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking 
local chronic_varlist z_baseline_crp_log z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic"

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia that includes the cind cases in the normal category ****

local dependent_var dementia_2

* Loop through each variable group and racial group 
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'"
	if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
	if "`var_group'" == "behavioral" local current_varlist	"`behavioral_varlist'"
	if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
	eststo model_`var_group'_`i': xi: poisson `dependent_var' `current_varlist' if minority == 1 , offset(ln_time_2) irr cformat(%6.2f) vce(robust) 
}


	lab var z_baseline_crp_log "Z log CRP"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using dementia_log_minority.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for Dementia") mtitle("Unadjusted" "Demographic" "Behavioral" "Chronic")
	
	
	eststo clear 
	

*******************************************************************************************************************************************************************	
************** PREDICTING HIGH LEVELS OF THE MEDIATOR AMONG IN BLACK, HISPANIC AND MINORITIZED PARTICIPANTS IN COMPARISON TO WHITE PARTICIPANTS *******************
*******************************************************************************************************************************************************************


********************************************************************
****** BLACK PARTICIPANTS IN COMPARISON TO WHITE PARTICIPANTS ******
********************************************************************



* Define the list of covariate sets 
local unadjusted_varlist black
local demographic_varlist black z_age i.sex i.wave i.edu_cat i.APOE2010_bin 
local behavioral_varlist black z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking 
local chronic_varlist black z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic"

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia that includes the cind cases in the normal category ****

local dependent_var baseline_crp_75

* Loop through each variable group and racial group 
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'"
	if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
	if "`var_group'" == "behavioral" local current_varlist	"`behavioral_varlist'"
	if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
	eststo model_`var_group'_`i': xi: logistic `dependent_var' `current_varlist' if dementia_2!=., cformat(%6.2f) vce(robust) 
}


	lab var black "NH-Black"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using crp_black.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for CRP") mtitle("Unadjusted" "Demographic" "Behavioral" "Chronic")
	
	
	eststo clear 
	
	
**********************************************************************************
****** MODELS FOR HISPANIC PARTICIPANTS IN COMPARISON TO WHITE PARTICIPANTS ******
**********************************************************************************



* Define the list of covariate sets 
local unadjusted_varlist hispanic
local demographic_varlist hispanic z_age i.sex i.wave i.edu_cat i.APOE2010_bin 
local behavioral_varlist hispanic z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking 
local chronic_varlist hispanic z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic"

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia that includes the cind cases in the normal category ****

local dependent_var baseline_crp_75

* Loop through each variable group and racial group 
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'"
	if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
	if "`var_group'" == "behavioral" local current_varlist	"`behavioral_varlist'"
	if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
	eststo model_`var_group'_`i': xi: logistic `dependent_var' `current_varlist' if dementia_2!=., cformat(%6.2f) vce(robust) 
}


	lab var hispanic "Hispanic"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using crp_hispanic.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for CRP") mtitle("Unadjusted" "Demographic" "Behavioral" "Chronic")
	
	
	eststo clear 

	
	
	
*************************************************************************************
****** MODELS FOR MINORITIZED PARTICIPANTS IN COMPARISON TO WHITE PARTICIPANTS ******
*************************************************************************************



* Define the list of covariate sets 
local unadjusted_varlist minority
local demographic_varlist minority z_age i.sex i.wave i.edu_cat i.APOE2010_bin 
local behavioral_varlist minority z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking 
local chronic_varlist minority z_age i.sex i.wave i.edu_cat i.APOE2010_bin z_bmi z_alcohol i.smoking chronic 

local var_groups "unadjusted demographic behavioral chronic"

* Specify your dependent variable 
***** NOTE here the dependent variable is dementia that includes the cind cases in the normal category ****

local dependent_var baseline_crp_75

* Loop through each variable group and racial group 
foreach var_group in `var_groups' {
	if "`var_group'" == "unadjusted" local current_varlist "`unadjusted_varlist'"
	if "`var_group'" == "demographic" local current_varlist "`demographic_varlist'"
	if "`var_group'" == "behavioral" local current_varlist	"`behavioral_varlist'"
	if "`var_group'" == "chronic" local current_varlist "`chronic_varlist'"
	eststo model_`var_group'_`i': xi: logistic `dependent_var' `current_varlist' if dementia_2!=., cformat(%6.2f) vce(robust) 
}


	lab var minority "Minoritized"
	lab var _Isex_2 "Male" 
	lab var _Iedu_cat_2 "College/Some" 
	lab var _Iedu_cat_3 "HS or <" 
	lab var _Ismoking_1 "Former" 
	lab var _Ismoking_2 "Current"
	lab var _IAPOE2010__1 "APOE-e4"
	lab var z_age "Z age"
	lab var z_bmi "Z BMI"
	lab var z_alcohol "Z Alcohol"
	lab var _Iwave_2008 "Wave"
	lab var chronic "Chronic"


	esttab model_* using crp_minoritized.rtf, replace eform ci obslast scalar(F) b(%9.2f) t(%9.2f) wide ///
	label nonumber title("Models for CRP") mtitle("Unadjusted" "Demographic" "Behavioral" "Chronic")
	
	
	eststo clear 
	

	


	
/*Save this file for mediation analysis later */

/*Generate first a dummy for education because CMAverse will require dummies for mediation */

tab edu_cat
tab edu_cat, nolabel 

tab edu_cat , nolabel 
gen edu_cat_1 = 1 if edu_cat==2
replace edu_cat_1=0 if edu_cat==3 | edu_cat==1
tab edu_cat_1

gen edu_cat_2 = 1 if edu_cat==3 
replace edu_cat_2=0 if edu_cat== 2 | edu_cat==1
tab edu_cat_2

/*Generate first a dummy for smoking because CMAverse will require dummies for mediation */

tab smoking 

gen smoking_1=1 if smoking==1 
replace smoking_1=0 if smoking==2 | smoking==0 
tab smoking_1 

gen smoking_2=1 if smoking==2
replace smoking_2=0 if smoking==1 | smoking==0
tab smoking_2 



/* We no longer need chronic diseases dichotomized because the reviewer asked for it to be in its continous form, but we will keep it in the data set  */


drop _Isex_2 _Iwave_2008 _Iedu_cat_2 _Iedu_cat_3 _IAPOE2010__1 _Ismoking_1 _Ismoking_2 


*save "/Users/cesarhiggins/Documents/CRP_Dementia_8-9-23/STATA/crp_data_incidence_mediation_9-5-23.dta", replace


