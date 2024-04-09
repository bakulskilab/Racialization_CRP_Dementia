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


/*Reviewer from communications medicine suggested to include participants with 
cognitive impairment in the analyses by including them in the group of participants who do not have dementia. This could be followed by a sensitivity analysis in which the participants with CIND are included in the dementia group instead. */

/*generating dementia outcome overtime, this time including CIND participants in the normal cognition category */

gen dementia_2=.
replace dementia_2=0 if AD_==3 | AD==2 /*including cind in the normal category */
replace dementia_2=1 if AD_==1 
replace dementia_2=. if AD_==. /*This category should be 3,204*/
tab dementia_2, m 

/*generating dementia outcome overtime, this time including CIND participants in the dementia category */

gen dementia_3=.
replace dementia_3=0 if AD_==3  
replace dementia_3=1 if AD_==1 | AD_==2 /*including cind in the normal category */
replace dementia_3=. if AD_==. /*This category should be 3,204*/
tab dementia_3, m 
tab AD_,  m 


* Generate year of first dementia diagnosis 

by id, sort: egen firsttimedem = min(cond(AD_ == 1, year, .)) /*first year diagnosed with dementia*/
by id, sort: egen lasttimeseen_2 = max(cond(AD_!=., year, .)) /*last year that the participant was followed-up or seen in the 6-year follow-up period*/


* Generate year of first dementia using the dementia variable and see if we get same results 

by id, sort: egen firstdementia = min(cond(dementia == 1, year, .)) 

tab firstdementia 
tab firsttimedem 

/* it generated the same for me as shown in the tabulations below, so we can use this variable to create first time of outcome assessment even with the sensitivity variables dementia_2 and dementia_3 */

* Generate year of first dementia using the dementia variable dementia_2, which include cind cases in the normal category so this variable should be equal to the variable firstdementia and firsttimedem because the #s of cases of dementia remains the same  

by id, sort: egen firstdementia_2 = min(cond(dementia_2 == 1, year, .)) 

tab firstdementia_2
tab firsttimedem  
tab firstdementia

* tabulate with lastimeseen_2 with dementia and dementia_2 and see what the last time of follow-up is 

tab lasttimeseen_2 firstdementia 
tab lasttimeseen_2 firstdementia_2

** this two variables generate the same time of follow-up #s of participants 

* Generate year of first dementia using the dementia variable dementia_3, which include cind cases in the dementia category so this variable should be different to the variable firstdementia and firsttimedem because the #s of cases of dementia changes when including the cind cases as being dementia 


by id, sort: egen firstdementia_3 = min(cond(dementia_3 == 1, year, .)) 

tab firstdementia_3
tab firstdementia_2
tab firsttimedem  
tab firstdementia

* tabulate with lastimeseen_2 with dementia and dementia_2 and dementia_3 and see what the last time of follow-up is 

tab lasttimeseen_2 firsttimedem
tab lasttimeseen_2 firstdementia 
tab lasttimeseen_2 firstdementia_2
tab lasttimeseen_2 firstdementia_3
tab lasttimeseen_2 AD_


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
tab t_dementia, m
tab t_dementia dementia, m 

/*generate time to dementia diagnosis using the firstdementia this should be equal to firsttimedem */

gen t_dementia_a=. 

replace t_dementia_a=lasttime_2 if firstdementia==.
replace t_dementia_a=firstdementia if firstdementia!=.
tab t_dementia_a, m 
tab t_dementia_a dementia, m 

** this led to the same tabulations 

/*generate time to dementia diagnosis using the firstdementia_2 meaning when cind cases where included as normal, this should be equal to firsttimedem because time to dementia should not change since cind is what is added to the normal category */

gen t_dementia_2=. 

replace t_dementia_2=lasttime_2 if firstdementia_2==.
replace t_dementia_2=firstdementia_2 if firstdementia_2!=.
tab t_dementia_2, m 
tab t_dementia_2 dementia_2, m 

tab t_dementia_2 AD_, m 
tab t_dementia_2 dementia_2, m  

/* this tabulations perfectly show that the variable dementia_2 when is tabulated with t_dementia_2, gives the 
same results that the variable AD_ if we add the cind + normal impairment categories */


/* generate time to dementia diagnosis using the firstdementia_3 meaning when cind cases are included as dementia */

gen t_dementia_3=. 

replace t_dementia_3=lasttime_2 if firstdementia_3==.
replace t_dementia_3=firstdementia_3 if firstdementia_3!=.
tab t_dementia_3, m 
tab t_dementia_3 dementia_3, m 

tab t_dementia_3 AD_, m 
tab t_dementia_3 dementia_3, m  

/* this tabulations perfectly show that the variable dementia_3 when is tabulated with t_dementia_3, gives the 
same results that the variable AD_ if we add the cind + dementia categories */


/*droping variables to be able to shape the data in wide format */

drop dementia dementia_2 dementia_3 firsttimedem lasttimeseen_2 last_2
* drop cind firsttimecind

/*shape the data in wide format */

reshape wide  /*this data set has the same 8,781 observations as the original one*/

tab1 AD_*

/*create the dementia variable across the six year period, excluding CIND cases*/

gen dementia=1 if AD_0==1 | AD_2==1 | AD_4==1 | AD_6==1
replace dementia=0 if AD_0==3 & AD_2==3 & AD_4==3 & AD_6==3
tab dementia 


/*Create the dementia variable across the six year period, including CIND cases in the normal category */

gen dementia_2=1 if AD_0==1 | AD_2==1 | AD_4==1 | AD_6==1
replace dementia_2=0 if (AD_0==3 | AD_0==2) & (AD_2==3 | AD_2==2) & (AD_4==3 | AD_4==2) & (AD_6==3 | AD_6==2) 
tab dementia_2

tab dementia_2, m  
tab dementia, m 

/* We can observe that, there were 1,815 CIND cases added to the normal category, I do know this because 6,248 ("normal cases") in dementia_2 - 4,433 "normal cases" in 
 dementia equals 1815 cases */ 

/*Create the dementia variable across the six year period, including CIND cases in the dementia category */


gen dementia_3=1 if (AD_0==1 | AD_0==2) | (AD_2==1 | AD_2==2) | (AD_4==1 | AD_4==2) | (AD_6==1 | AD_6==2)
replace dementia_3=0 if AD_0==3 & AD_2==3  & AD_4==3 & AD_6==3 
tab dementia_3

tab dementia_3, m  
tab dementia, m 

/* We can observe that, there were 1,815 CIND cases added to the normal category, I do know this because 6,248 ("normal cases") in dementia_2 - 4,433 "normal cases" in 
 dementia equals 1815 cases */ 


tab1 dementia dementia_2 dementia_3, m 


tab dementia t_dementia, m 
tab dementia_2 t_dementia_2, m 
tab dementia_3 t_dementia_3, m 




/*tabulate participants time with cind and dementia*/

tab lasttime_2 dementia, m 
tab lasttime_2 dementia_2, m 
tab lasttime_2 dementia_3, m  

/*Generate race variable and drop participatns with Other-NH*/

gen race=0 if race_ff=="White-NH"
replace race=1 if race_ff=="Black-NH"
replace race=2 if race_ff=="Hispanic"
replace race=. if race_ff=="Other-NH"
tab race, m 

/*dropping participants with other-NH race which is equal to 189*/
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
gen ln_time_2 = ln(t_dementia_2)
gen ln_time_3 = ln(t_dementia_3)


* dropping prevalent dementia cases N = 272 cases
drop if t_dementia==0 & dementia==1 

/* Note: we do not need to drop CIND cases anymore. This data set has 8,320 after eliminating prevalent dementia cases. But if we do a crosstab between t_dementia dementia then we get the original sample size of our study of 5,143 */

/*Final N = 5,143*/

/* This new file with the analysis that the reviewers asked to add is saved in a new folder   */

* save "/Users/cesarhiggins/Documents/CRP_Dementia_8-9-23/STATA/crp_data_incidence_analysis_8-30-23.dta"


/*SAVE FILE FOR ANALYSIS*/

*save "/Users/cesarhiggins/Documents/CRP Dementia/STATA/crp_data_incidence_analysis.dta"
*file /Users/cesarhiggins/Documents/CRP Dementia/STATA/crp_data_incidence_analysis.dta saved




