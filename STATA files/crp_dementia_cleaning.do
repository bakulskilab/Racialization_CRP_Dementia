use "/Users/cesarhiggins/Documents/Dementia project /NIH grant/hsCRP project /Multiple Mediation/crp_data_incidence.dta", clear 


/*Generate Variabels for Dementia at different time points*/
/*Baseline*/

gen AD_0 = 1 if dementia_b==1 
replace AD_0 = 2 if cind_b==1
replace AD_0 = 3 if dementia_b==0 & cind_b==0
tab AD_0, m 

/*Two years follow-up*/
gen AD_2 = AD08 if wave==2006
replace AD_2 = AD10 if wave==2008 
tab AD_2, m 

/*Four years follow-up*/
gen AD_4 = AD10 if wave==2006 
replace AD_4 = AD12 if wave==2008 
tab AD_4, m  

/*Six years follow-up*/
gen AD_6 = AD12 if wave==2006 
replace AD_6 = AD14 if wave==2008 
tab AD_6, m  


/*Keep Variables relevant for our analysis */

keep HHID PN race_ff gender_f edu education wave age alcohol bmi smoking baseline_crp baseline_hdl baseline_tc baseline_cysc baseline_a1c demdx APOE2010_bin R8CONDE R9CONDE AD_0 AD_2 AD_4 AD_6 

/*generate a unique ID variable for each participant*/

gen id = _n

/*Convert data to long format */
 
reshape long AD_ , i(id) j(year)

/*Declare data to be panel data*/

xtset id year

tab AD_, m /*only 801 (3,204 follow-up records) participants or approximately < than 10% of the sample had missing cogntive data over time*/


/*generating dementia outcome overtime, n=1790, same as AD_==1 in previous tab*/
gen dementia=. 
replace dementia=0 if AD_==3
replace dementia=1 if AD_==1 
replace dementia=. if AD_==. | AD_==2
tab dementia , m 


/*generating cind outcome overtime, n = 5239, same as AD_==2 in previous tab */
gen cind=. 
replace cind=0 if AD_==3
replace cind=1 if AD_==2 
replace cind=. if AD_==. | AD_==1
tab cind , m 

* Generate year of first dementia diagnosis and first year of cind 


by id, sort: egen firsttimedem = min(cond(AD_ == 1, year, .)) /*first year diagnosed with dementia*/
by id, sort: egen firsttimecind = min(cond(AD_ == 2, year, .)) /*first year diagnosed with CIND*/
by id, sort: egen lasttimeseen_2 = max(cond(AD_!=., year, .)) /*last year that the participant was followed-up or seen in the 6-year follow-up period*/


/*Making an indicator of when was the last time seen, for example if participant is last time seen in year 4, the indicator will be 0,0,1,0 */

gen byte last_2 = year == lasttimeseen_2


/*generates complete record of last year seen per participants id and time observed*/
by id: egen lasttime_2 = total(last_2*year)

/*these two are the same variable*/
tab lasttimeseen_2
tab lasttime_2


/* generate time to dementia diagnosis, it will be = to first time of dementia diagnosis if the participant had dementia ever in the last 6 years, or last time seen if participant did not have dementia */
gen t_dementia=.
replace t_dementia=lasttime_2 if firsttimedem==. 
replace t_dementia=firsttimedem if firsttimedem!=.
tab t_dementia dementia, m 

/*Time to CIND, same as dementia but this time using the cind category*/
gen t_cind=.
replace t_cind=lasttime_2 if firsttimecind==. 
replace t_cind=firsttimecind if firsttimecind!=.
tab t_cind cind, m 

/*droping variables to be able to shape the data in wide format */

drop dementia cind firsttimedem firsttimecind lasttimeseen_2 last_2

/*shape the data in wide format */

reshape wide  /*this data set has the same 8,781 observations as the original one*/

/*create the dementia variable across the six year period, excluding CIND cases*/

gen dementia=1 if AD_0==1 | AD_2==1 | AD_4==1 | AD_6==1
replace dementia=0 if AD_0==3 & AD_2==3 & AD_4==3 & AD_6==3
tab dementia 

/*create the CIND variable across the six year period, excluding dementia cases*/

gen cind=1 if AD_0==2 | AD_2==2 | AD_4==2 | AD_6==2
replace cind=0 if AD_0==3 & AD_2==3 & AD_4==3 & AD_6==3
tab cind 

/*tabulate participants time with cind and dementia*/

tab lasttime_2 dementia
tab lasttime_2 cind

/*Generate race variable and drop participatns with Other-NH*/

gen race=0 if race_ff=="White-NH"
replace race=1 if race_ff=="Black-NH"
replace race=2 if race_ff=="Hispanic"
replace race=. if race_ff=="Other-NH"
tab race, m 

/*dropping participants with other-NH race*/
drop if race==.

/*recode sex, education, and smoking status*/
encode gender_f, gen(sex)
encode edu, gen(edu_cat)

gen smoke=0 if smoking==0
replace smoke=1 if smoking==1 | smoking==2 
tab smoke 

/*generate chronic conditions variables for baseline*/
gen chronic=. 
replace chronic=R8CONDE if wave==2006 
replace chronic=R9CONDE if wave==2008 
summarize chronic 
	
gen chronic_2 = .
replace chronic_2=0 if chronic==0 
replace chronic_2=1 if chronic>0 
replace chronic_2=. if chronic==. 
tab chronic_2

drop chronic 
rename chronic_2 chronic	

* Dichotomizng the race variable into black vs whites and hispanic vs whites

gen black=.
replace black=1 if race==1
replace black=0 if race==0
tab black
	
gen hispanic=.
replace hispanic=1 if race==2
replace hispanic=0 if race==0
tab hispanic

/*generate the log of time to dementia*/

gen ln_time = ln(t_dementia)


* dropping prevalent dementia cases N = 272 cases
drop if t_dementia==0 & dementia==1 

* dropping CIND cases N = 2299 that were never dementia
drop if cind==1 & dementia==.

* drop no follow-up dementia status 
drop if dementia==.

/*Final N = 5,143*/


/*SAVE FILE FOR ANALYSIS*/

*save "/Users/cesarhiggins/Documents/CRP Dementia/STATA/crp_data_incidence_analysis.dta"
*file /Users/cesarhiggins/Documents/CRP Dementia/STATA/crp_data_incidence_analysis.dta saved




