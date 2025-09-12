clear all
set more off
set maxvar 10000
set matsize 2000
set varabbrev on
global REPLICATIONroot "/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change"

cd "$REPLICATIONroot"
adopath + "$REPLICATIONroot/ADO_FILES/"
adopath + "$REPLICATIONroot/ADO_FILES/esta/"


*******************************************************************************************************************************
** The Wind of Change: Maritime Technology, Trade and Economic Development
** American Economic Review 2017
** Luigi Pascali

*This do-file generates all the descriptive statistics, regressions results, and scatter plot associated with the country-level analysis:
*Table 1 (Panels D and E), Tables 6-10, Table A.3, Tables A.7-A.10, Figures A.5-A.10
*This do-file also produces an excel file named “Data_for_Figure10.xls”, which is used to create Figure 10 in ArcGis. 
*******************************************************************************************************************************

************************************************************************************************************************
****************** INSTALL RELEVANT ADO-FILES
ssc install latab, replace
net sj 13-3 st0143_4
net install st0143_4, replace


*****************************************************************************
******************PROGRAMS TO COMPUTE CORRECTED STANDARD ERRORS IN COUNTRY-YEAR REGRESSIONS
*****************************************************************************

cap program drop correction_se_red
program define correction_se_red		/*it create a program, called correction_red, that corrects the standard errors in OLS regressions (reported in tables) to take into account that the instruments depends on estimated parameters from the bilateral trade equation  */
	args Dep_var Weight	outfile	/*this states that the program has 3 arguments (variables to be defined every time we run the program: Dep_var Trade Weight*/
      ivreg2 `Dep_var'  lpred_TOTAL_trad_5ys  dummy_C* dummy_Y*   						    `Weight' if  ln_exportGDP~=., cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto a1
 scalar def coef_trade =_b[lpred_TOTAL_trad_5ys] 
   foreach YR_Tech in 60sa 65sa 70sa 75sa 80sa 85sa 90sa 95sa 00sa  60st 65st 70st 75st 80st 85st 90st 95st 00st  {
  ivreg2 `Dep_var'  lpred_TOTAL_trad_5ys_aug_`YR_Tech'  dummy_C* dummy_Y*   			`Weight' if  ln_exportGDP~=., cluster(country year) partial(dummy_C* dummy_Y*) noc
  scalar def coef_`YR_Tech' =_b[lpred_TOTAL_trad_5ys_aug_`YR_Tech'] 
  scalar def der_`YR_Tech'=(coef_`YR_Tech'-coef_trade)/0.001
  }
 matrix der_b_a= (der_60sa, der_60st,	der_65sa, der_65st, der_70sa, der_70st, der_75sa, der_75st, der_80sa, der_80st, der_85sa, der_85st, der_90sa, der_90st, der_95sa, der_95st, der_00sa, der_00st)		   
 matrix err_cor2=der_b_a*rel_var_cov2*der_b_a'
 matrix list err_cor2
 ivreg2 `Dep_var'  lpred_TOTAL_trad_5ys  dummy_C* dummy_Y*   						    `Weight' if  ln_exportGDP~=., cluster(country year) partial(dummy_C* dummy_Y*) noc
 newcov
 outreg2 using `outfile', append  e(N) se dec(3) nonotes  excel tex

 end

 
cap program drop correction_se_IV
program define correction_se_IV		/*it create a program, called correction_se_IV, that corrects the standard errors in IV regressions (reported in tables) to take into account that the instruments depends on estimated parameters from the bilateral trade equation  */
	args Dep_var Trade Weight outfile		/*this states that the program has 3 arguments (variables to be defined every time we run the program: Dep_var Trade Weight*/
 ivreg2 `Dep_var' (`Trade'= lpred_TOTAL_trad_5ys)  dummy_C* dummy_Y*   						    `Weight' if  ln_exportGDP~=., cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto a1
 scalar def coef_trade =_b[`Trade'] 
   foreach YR_Tech in 60sa 65sa 70sa 75sa 80sa 85sa 90sa 95sa 00sa  60st 65st 70st 75st 80st 85st 90st 95st 00st  {
   ivreg2 `Dep_var' (`Trade'= lpred_TOTAL_trad_5ys_aug_`YR_Tech')  dummy_C* dummy_Y*   			`Weight' if  ln_exportGDP~=., cluster(country year) partial(dummy_C* dummy_Y*) noc
   scalar def coef_`YR_Tech' =_b[`Trade'] 
   scalar def der_`YR_Tech'=(coef_`YR_Tech'-coef_trade)/0.001
   }
 matrix der_b_a= (der_60sa, der_60st,	der_65sa, der_65st, der_70sa, der_70st, der_75sa, der_75st, der_80sa, der_80st, der_85sa, der_85st, der_90sa, der_90st, der_95sa, der_95st, der_00sa, der_00st)		   
 matrix err_cor2=der_b_a*rel_var_cov2*der_b_a'
 matrix list err_cor2
 ivreg2 `Dep_var' (`Trade'= lpred_TOTAL_trad_5ys)  dummy_C* dummy_Y*   						    `Weight' if  ln_exportGDP~=., cluster(country year) partial(dummy_C* dummy_Y*) noc
 newcov
 outreg2 using `outfile', append  e(widstat N) se dec(3) nonotes  excel tex
 end
 
 
 
capture program drop newcov
 program define newcov, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=V_corrected[1,1]+err_cor2
 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with cov_dep */ 
end



*****************************************************************************
******************CONSTRUCT INSTRUMENT
*****************************************************************************

***CONSTRUCT SHARES OF WORLD TRADE FOR EACH COUNTRY IN 1870  
use "PUBLIC_DATA/COUNTRY_LEVEL_PUBLIC.dta", clear  
keep if year==1870
gen TOTAL_TRADE=IMPORT+EXPORT
replace TOTAL_TRADE=2*IMPORT if EXPORT==.  /*Assume trade is 2*imports for those countries for which export data are not available*/
replace TOTAL_TRADE=2*EXPORT if IMPORT==.  /*Assume trade is 2*imports for those countries for which export data are not available*/					

gen mean_trade_usable= TOTAL_TRADE if country~="Australia"     /*This is to avoid double counting of countries*/
keep if mean_trade_usable~=.
gsort - mean_trade_usable

gen large_country_top1=1 	  if _n<=1 & mean_trade_usable~=.
replace large_country_top1=0  if _n>1  & mean_trade_usable~=.
gen large_country_top3=1 	  if _n<=3 & mean_trade_usable~=.
replace large_country_top3=0  if _n>3  & mean_trade_usable~=.
gen large_country_top5=1 	  if _n<=5 & mean_trade_usable~=.
replace large_country_top5=0  if _n>5  & mean_trade_usable~=.
save "TEMP/TEMP1", replace



***ESTIMATE GRAVITY
use "PUBLIC_DATA/BILATERAL_TRADE_PUBLIC2.dta", clear

reg lexpr  c.d6_60#c.lsail2 c.d6_60#c.lsteam c.d6_65#c.lsail2 c.d6_65#c.lsteam c.d6_70#c.lsail2 c.d6_70#c.lsteam c.d6_75#c.lsail2 c.d6_75#c.lsteam c.d6_80#c.lsail2 c.d6_80#c.lsteam c.d6_85#c.lsail2 c.d6_85#c.lsteam  c.d6_90#c.lsail2 c.d6_90#c.lsteam c.d6_95#c.lsail2  c.d6_95#c.lsteam   c.d6_00#c.lsail2 c.d6_00#c.lsteam   dummy_Y* dummy_ORI* dummy_DES*   ,noc
scalar def b_60sa =_b[c.d6_60#c.lsail2]                
scalar def b_60st =_b[c.d6_60#c.lsteam]                
scalar def b_65sa =_b[c.d6_65#c.lsail2]
scalar def b_65st =_b[c.d6_65#c.lsteam] 
scalar def b_70sa =_b[c.d6_70#c.lsail2] 
scalar def b_70st =_b[c.d6_70#c.lsteam] 
scalar def b_75sa =_b[c.d6_75#c.lsail2]
scalar def b_75st =_b[c.d6_75#c.lsteam]
scalar def b_80sa =_b[c.d6_80#c.lsail2] 
scalar def b_80st =_b[c.d6_80#c.lsteam] 
scalar def b_85sa =_b[c.d6_85#c.lsail2] 
scalar def b_85st =_b[c.d6_85#c.lsteam]
scalar def b_90sa =_b[c.d6_90#c.lsail2] 
scalar def b_90st =_b[c.d6_90#c.lsteam] 
scalar def b_95sa =_b[c.d6_95#c.lsail2] 
scalar def b_95st =_b[c.d6_95#c.lsteam] 
scalar def b_00sa =_b[c.d6_00#c.lsail2]
scalar def b_00st =_b[c.d6_00#c.lsteam]

foreach lname of numlist  1(1)56    {
scalar def y_`lname'= _b[dummy_Y`lname']
}

*** NECESSARY TO CORRECT STANDARD ERRORS IN 2SLS COUNTRY-YEAR REGRESSIONS
scalar def b_60sa_aug =_b[c.d6_60#c.lsail2]+0.001                
scalar def b_60st_aug =_b[c.d6_60#c.lsteam]+0.001                 
scalar def b_65sa_aug =_b[c.d6_65#c.lsail2]+0.001 
scalar def b_65st_aug =_b[c.d6_65#c.lsteam]+0.001  
scalar def b_70sa_aug =_b[c.d6_70#c.lsail2]+0.001  
scalar def b_70st_aug =_b[c.d6_70#c.lsteam]+0.001  
scalar def b_75sa_aug =_b[c.d6_75#c.lsail2]+0.001 
scalar def b_75st_aug =_b[c.d6_75#c.lsteam]+0.001 
scalar def b_80sa_aug =_b[c.d6_80#c.lsail2]+0.001  
scalar def b_80st_aug =_b[c.d6_80#c.lsteam]+0.001  
scalar def b_85sa_aug =_b[c.d6_85#c.lsail2]+0.001  
scalar def b_85st_aug =_b[c.d6_85#c.lsteam]+0.001 
scalar def b_90sa_aug =_b[c.d6_90#c.lsail2]+0.001  
scalar def b_90st_aug =_b[c.d6_90#c.lsteam]+0.001  
scalar def b_95sa_aug =_b[c.d6_95#c.lsail2]+0.001  
scalar def b_95st_aug =_b[c.d6_95#c.lsteam]+0.001  
scalar def b_00sa_aug =_b[c.d6_00#c.lsail2]+0.001 
scalar def b_00st_aug =_b[c.d6_00#c.lsteam]+0.001 
cgmreg  lexpr  dd6_60lsail2 dd6_60lsteam dd6_b65lsail2 dd6_b65lsteam dd6_b70lsail2 dd6_b70lsteam dd6_b75lsail2 dd6_b75lsteam dd6_b80lsail2 dd6_b80lsteam dd6_b85lsail2 dd6_b85lsteam dd6_b90lsail2 dd6_b90lsteam dd6_b95lsail2 dd6_b95lsteam dd6_b00lsail2  dd6_b00lsteam  dummy_Y* dummy_ORI* dummy_DES*  ,cluster(country_o country_d year)
matrix var_cov2=e(V)
matrix rel_var_cov2=var_cov2[1..18,1..18]
***


use "PUBLIC_DATA/BILATERAL_DISTANCES_PUBLIC.dta", clear			 
expand 69
sort country_o country_d 
by country_o country_d: gen year=1845 - 1 +_n
gen lsteam=log(TIME_4_2) if year<1870
replace lsteam=log(TIME_4_1) if year>=1870
gen lsail2=log(TIME_5_2_5) if year<1870
replace lsail2=log(TIME_5_1_5) if year>=1870
keep country_o country_d year lsteam lsail2 
gen country=country_d
merge m:1 country using "TEMP/TEMP1"
drop _merge
drop if country_o==""
drop country
drop if year>1905

gen dummy_year=.
foreach lname of numlist  1(1)56    {
replace dummy_year=y_`lname' if year==1844+`lname'
}
foreach lname of numlist  1901(1)1905    {       /*since we don't have a value for the year dummies for 1901-1905, we assign the value of the year dummy in 1900*/
replace dummy_year=y_56 if year==`lname'
}

***CONSTRUCT PREDICTED BILATERAL TRADE
gen     lpred_trad_5ys = b_60sa*lsail2+b_60st*lsteam    if             year<=1860 
replace lpred_trad_5ys = b_65sa*lsail2+b_65st*lsteam    if year>1860 & year<=1865 
replace lpred_trad_5ys = b_70sa*lsail2+b_70st*lsteam    if year>1865 & year<=1870 
replace lpred_trad_5ys = b_75sa*lsail2+b_75st*lsteam    if year>1870 & year<=1875 
replace lpred_trad_5ys = b_80sa*lsail2+b_80st*lsteam    if year>1875 & year<=1880 
replace lpred_trad_5ys = b_85sa*lsail2+b_85st*lsteam    if year>1880 & year<=1885 
replace lpred_trad_5ys = b_90sa*lsail2+b_90st*lsteam    if year>1885 & year<=1890 
replace lpred_trad_5ys = b_95sa*lsail2+b_95st*lsteam    if year>1890 & year<=1895 
replace lpred_trad_5ys = b_00sa*lsail2+b_00st*lsteam    if year>1895 


*** NECESSARY TO CORRECT STANDARD ERRORS IN 2SLS COUNTRY-YEAR REGRESSIONS
gen      lpred_trad_5ys_aug_60sa = lpred_trad_5ys
replace  lpred_trad_5ys_aug_60sa = b_60sa_aug*lsail2+b_60st*lsteam    if             year<=1860 
gen      lpred_trad_5ys_aug_65sa = lpred_trad_5ys
replace  lpred_trad_5ys_aug_65sa = b_65sa_aug*lsail2+b_65st*lsteam    if year>1860 & year<=1865 
gen      lpred_trad_5ys_aug_70sa = lpred_trad_5ys
replace  lpred_trad_5ys_aug_70sa = b_70sa_aug*lsail2+b_70st*lsteam    if year>1865 & year<=1870
gen      lpred_trad_5ys_aug_75sa = lpred_trad_5ys
replace  lpred_trad_5ys_aug_75sa = b_75sa_aug*lsail2+b_75st*lsteam    if year>1870 & year<=1875 
gen      lpred_trad_5ys_aug_80sa = lpred_trad_5ys
replace  lpred_trad_5ys_aug_80sa = b_80sa_aug*lsail2+b_80st*lsteam    if year>1875 & year<=1880 
gen      lpred_trad_5ys_aug_85sa = lpred_trad_5ys
replace  lpred_trad_5ys_aug_85sa = b_85sa_aug*lsail2+b_85st*lsteam    if year>1880 & year<=1885 
gen      lpred_trad_5ys_aug_90sa = lpred_trad_5ys
replace  lpred_trad_5ys_aug_90sa = b_90sa_aug*lsail2+b_90st*lsteam    if year>1885 & year<=1890
gen      lpred_trad_5ys_aug_95sa = lpred_trad_5ys
replace  lpred_trad_5ys_aug_95sa = b_95sa_aug*lsail2+b_95st*lsteam    if year>1890 & year<=1895 
gen      lpred_trad_5ys_aug_00sa = lpred_trad_5ys
replace  lpred_trad_5ys_aug_00sa = b_00sa_aug*lsail2+b_00st*lsteam    if year>1895 

gen      lpred_trad_5ys_aug_60st = lpred_trad_5ys
replace  lpred_trad_5ys_aug_60st = b_60sa*lsail2+b_60st_aug*lsteam    if             year<=1860 
gen      lpred_trad_5ys_aug_65st = lpred_trad_5ys
replace  lpred_trad_5ys_aug_65st = b_65sa*lsail2+b_65st_aug*lsteam    if year>1860 & year<=1865 
gen      lpred_trad_5ys_aug_70st = lpred_trad_5ys
replace  lpred_trad_5ys_aug_70st = b_70sa*lsail2+b_70st_aug*lsteam    if year>1865 & year<=1870
gen      lpred_trad_5ys_aug_75st = lpred_trad_5ys
replace  lpred_trad_5ys_aug_75st = b_75sa*lsail2+b_75st_aug*lsteam    if year>1870 & year<=1875 
gen      lpred_trad_5ys_aug_80st = lpred_trad_5ys
replace  lpred_trad_5ys_aug_80st = b_80sa*lsail2+b_80st_aug*lsteam    if year>1875 & year<=1880 
gen      lpred_trad_5ys_aug_85st = lpred_trad_5ys
replace  lpred_trad_5ys_aug_85st = b_85sa*lsail2+b_85st_aug*lsteam    if year>1880 & year<=1885 
gen      lpred_trad_5ys_aug_90st = lpred_trad_5ys
replace  lpred_trad_5ys_aug_90st = b_90sa*lsail2+b_90st_aug*lsteam    if year>1885 & year<=1890
gen      lpred_trad_5ys_aug_95st = lpred_trad_5ys
replace  lpred_trad_5ys_aug_95st = b_95sa*lsail2+b_95st_aug*lsteam    if year>1890 & year<=1895 
gen      lpred_trad_5ys_aug_00st = lpred_trad_5ys
replace  lpred_trad_5ys_aug_00st = b_00sa*lsail2+b_00st_aug*lsteam    if year>1895 
***

***CONSTRUCT WEIGHTS 
egen global_trade2=sum(mean_trade_usable) if year==1870 & country_o~=country_d, by(year country_o)
egen global_trade3=mean( global_trade2), by(country_o)
gen weight=mean_trade/global_trade3
drop  global_trade2 

***CONSTRUCT PREDICTED AGGREGATE TRADE (THE EXCLUDED INSTRUMENT IN THE 2SLS REGRESSIONS)
egen lpred_TOTAL_trad_5ys	=sum(weight*lpred_trad_5ys), by(country_o year)
replace lpred_TOTAL_trad_5ys=  lpred_TOTAL_trad_5ys+ dummy_year if lpred_TOTAL_trad_5ys~=.

***CREATE MEASURES OF GEOGRAPHIC ISOLATION OF A COUNTRY (AVERAGE DISTANCE WITH RESPECT TO THE REST OF THE WORLD)
egen weight_ave_lsteam		=sum(weight*lsteam)		, by(country_o year)
egen weight_ave_lsail2		=sum(weight*lsail2)		, by(country_o year)

egen top1_ave_lsteam		=mean(lsteam)		if large_country_top1==1 , by(country_o year)
egen top1_ave_lsail2		=mean(lsail2)		if large_country_top1==1 , by(country_o year)

egen weight_top5_ave_lsteam		=sum(weight*lsteam)		if large_country_top5==1 , by(country_o year)
egen weight_top5_ave_lsail2		=sum(weight*lsail2)		if large_country_top5==1 , by(country_o year)


***NECESSARY TO COMPUTE CORRECTED STANDARD ERRORS IN THE COUNTRY-YEAR LEVEL REGRESSIONS
egen lpred_TOTAL_trad_5ys_aug_60sa=sum(weight*lpred_trad_5ys_aug_60sa), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_65sa=sum(weight*lpred_trad_5ys_aug_65sa), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_70sa=sum(weight*lpred_trad_5ys_aug_70sa), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_75sa=sum(weight*lpred_trad_5ys_aug_75sa), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_80sa=sum(weight*lpred_trad_5ys_aug_80sa), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_85sa=sum(weight*lpred_trad_5ys_aug_85sa), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_90sa=sum(weight*lpred_trad_5ys_aug_90sa), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_95sa=sum(weight*lpred_trad_5ys_aug_95sa), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_00sa=sum(weight*lpred_trad_5ys_aug_00sa), by(country_o year)

egen lpred_TOTAL_trad_5ys_aug_60st=sum(weight*lpred_trad_5ys_aug_60st), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_65st=sum(weight*lpred_trad_5ys_aug_65st), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_70st=sum(weight*lpred_trad_5ys_aug_70st), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_75st=sum(weight*lpred_trad_5ys_aug_75st), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_80st=sum(weight*lpred_trad_5ys_aug_80st), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_85st=sum(weight*lpred_trad_5ys_aug_85st), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_90st=sum(weight*lpred_trad_5ys_aug_90st), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_95st=sum(weight*lpred_trad_5ys_aug_95st), by(country_o year)
egen lpred_TOTAL_trad_5ys_aug_00st=sum(weight*lpred_trad_5ys_aug_00st), by(country_o year)
***

collapse lpred_TOTAL_trad_5ys* weight_ave_ls* top1_ave_ls* weight_top5*, by(country_o year)
sort country_o year
rename country_o country
label var lpred_TOTAL_trad_5ys "Predicted Trade assuming technology varies every 5 years"
keep if year==1845 | year==1850 |  year==1855 |  year==1860  |  year==1865 |  year==1870 |  year==1875 |  year==1880 |  year==1885 |  year==1890 |  year==1895 |  year==1900 |  year==1905   
save "TEMP/TEMP_predTOTALtrade.dta",replace


***DATA exported to ArcGis and used for Figure 10 
preserve
**new
gen weight_ave_lsteam1_1850=weight_ave_lsteam if year==1850
egen weight_ave_lsteam_1850=min(weight_ave_lsteam1_1850), by(country)
gen weight_ave_lsteam1_1900=weight_ave_lsteam if year==1900
egen weight_ave_lsteam_1900=min(weight_ave_lsteam1_1900), by(country)

gen weight_ave_lsail21_1850=weight_ave_lsail2 if year==1850
egen weight_ave_lsail2_1850=min(weight_ave_lsail21_1850), by(country)
gen weight_ave_lsail21_1900=weight_ave_lsail2 if year==1900
egen weight_ave_lsail2_1900=min(weight_ave_lsail21_1900), by(country)

keep if year==1850
gen LnChange_Isolation=weight_ave_lsteam_1900-weight_ave_lsail2_1850
**
keep country LnChange_Isolation
export excel using "Data_for_Figure10.xls", replace
restore

*****************************************************************************
******************REGRESSIONS at COUNTRY-YEAR LEVEL
*****************************************************************************


set more off
use "PUBLIC_DATA/COUNTRY_LEVEL_PUBLIC.dta", clear  
merge m:1 country year using "TEMP/TEMP_predTOTALtrade.dta"
drop if _merge==2

foreach vari in ln_GDPpc lpopulation ln_exportpc ln_exportGDP l1urban_50pc l1urban_100pc SHARE_NONAGRI_EXP lSHARE_NONAGRI_EXP {
gen `vari'=.
recast double `vari'
}
 
replace ln_GDPpc=ln(GDPpc)
replace lpopulation=ln(population*1000)

replace ln_exportpc=ln(EXPORT)-lpopulation
replace ln_exportGDP=ln_exportpc-ln_GDPpc
 
replace l1urban_50pc=ln(1+urban_50pc)
replace l1urban_100pc=ln(1+urban_100pc)

replace SHARE_NONAGRI_EXP	=SHARE_MAN_EXP+SHARE_COM_EXP
replace lSHARE_NONAGRI_EXP=ln(1+SHARE_NONAGRI_EXP)


*weights to be used in regression
gen population_1860_1=population if year==1860
egen population_1860=sum(population_1860_1), by(country)
replace population_1860=. if population_1860==0
drop population_1860_1
gen lpopulation_1860=ln(population_1860)

*Construct dataset to be used in the second part of Table 5 (In this table, the sample is larger as we consider also country-year observations for which GDP data are not available)
preserve
keep if year==1850 | year==1905
egen count2_ln_exportpc				=count(ln_exportpc) , by(country)
keep if count2_ln_exportpc==2
keep country year GDPpc ln_exportpc lpopulation_1860 population population_1860 top1_ave_ls* weight_top* weight_ave_ls* 
save "TEMP/DATA_TABLE_5.dta", replace
restore


*HERE WE EXCLUDE OBSERVATIONS THAT ARE COMPLETELY UNINFLUENTIAL
*The following lines delete the countries for which we have data only before 1870 or only after 1870
*Excluding these countries does not have any effect on the estimates but it makes it easier to understand the real number of observations that we are using to identify the parameters.

egen count2_ln_exportGDP_bef			=count(ln_exportGDP) 					 if year<=1870, by(country)
egen count2_TRADE2_TOT_bef				=count(ln_exportpc) 					 if year<=1870, by(country)
egen count2_SHARE_NONAGRI_TOT_bef		=count(lSHARE_NONAGRI_EXP) 				 if year<=1870, by(country)
egen count2_URBAN_TOT_bef				=count(l1urban_50pc) 					 if year<=1870, by(country)

egen count_ln_exportGDP_bef				=min(count2_ln_exportGDP_bef) 			, by(country)
egen count_TRADE2_TOT_bef				=min(count2_TRADE2_TOT_bef) 			, by(country)
egen count_SHARE_NONAGRI_TOT_bef		=min(count2_SHARE_NONAGRI_TOT_bef) 		, by(country)
egen count_URBAN_TOT_bef				=min(count2_URBAN_TOT_bef) 				, by(country)

egen count2_ln_exportGDP_af				=count(ln_exportGDP) 					if year>1870, by(country)
egen count2_TRADE2_TOT_af				=count(ln_exportpc) 					if year>1870, by(country)
egen count2_SHARE_NONAGRI_TOT_af		=count(lSHARE_NONAGRI_EXP)				if year>1870, by(country)
egen count2_URBAN_TOT_af				=count(l1urban_50pc) 					if year>1870, by(country)

egen count_ln_exportGDP_af				=min(count2_ln_exportGDP_af) 			, by(country)
egen count_TRADE2_TOT_af				=min(count2_TRADE2_TOT_af) 				, by(country)
egen count_SHARE_NONAGRI_TOT_af			=min(count2_SHARE_NONAGRI_TOT_af)		, by(country)
egen count_URBAN_TOT_af					=min(count2_URBAN_TOT_af) 				, by(country)

drop if count_ln_exportGDP_bef==0 &  count_TRADE2_TOT_bef==0 &   count_SHARE_NONAGRI_TOT_bef==0 &   count_URBAN_TOT_bef==0  &  count_ln_exportGDP_af==0  &  count_TRADE2_TOT_af==0  &  count_SHARE_NONAGRI_TOT_af==0  &  count_URBAN_TOT_af==0
gen complete=1 		if count_ln_exportGDP_bef>0 & count_ln_exportGDP_af>0
drop if complete~=1
drop count_* count2_* 


*dummies
tabulate country, gen(dummy_C)
tabulate year, gen(dummy_Y) 

save "TEMP/TEMP100.dta", replace
est clear


*****************************************************************************
****************** TABLE 1 (PANEL D and E). DESCRIPTIVE STATISTICS
*****************************************************************************

*** Table 1 - PANEL D 
ivreg2 ln_GDPpc		  			(ln_exportGDP= lpred_TOTAL_trad_5ys)  dummy_C* dummy_Y*   													, cluster(country year) partial(dummy_C* dummy_Y*) noc
latabstat EXPORT SHARE_NONAGRI_EXP GDPpc population urban_50pc urban_100pc   if e(sample)==1,s(mean p50 sd min max N) col(stat) tf(Table1_part2) f(%9.2fc) replace

*** Table 1 - PANEL E 
preserve
ivreg2 ln_GDPpc		  			(ln_exportGDP= lpred_TOTAL_trad_5ys)  dummy_C* dummy_Y*   													, cluster(country year) partial(dummy_C* dummy_Y*) noc
keep if e(sample)==1
by country, sort: keep if _n==1
latabstat colony1850 cons1860   if e(sample)==1,s(mean p50 sd min max N) col(stat) tf(Table1_part2) f(%9.2fc) append
restore


*****************************************************************************
****************** TABLE A.3 DATA AVAILABILITY BY COUNTRY FOR THE DATASET USED IN SECTION 5
*****************************************************************************

preserve
ivreg2 ln_GDPpc		  			 ln_exportGDP  			  dummy_C* dummy_Y*   					if lpred_TOTAL_trad_5ys~=.					, cluster(country year) partial(dummy_C* dummy_Y*) noc
keep if e(sample)==1
egen mmSHARE_NONAGRI_EXP=max(lSHARE_NONAGRI_EXP),by(country)
egen mmTRADE_TOT=max(ln_exportGDP),by(country)
by country, sort: keep if _n==1
gen Data_Export_GDP_Pop=1 if mmTRADE_TOT~=.
gen Data_ConsExecutive=1  if cons1860~=.
gen Data_ShippingTimes=1  if lpred_TOTAL_trad_5ys_aug_60sa~=.
gen Data_Share_NonAgric_Exp=1 if mmSHARE_NONAGRI_EXP~=.
latabstat Data_* , by(country) tf(TableA3) f(%9.0fc) replace
restore

*****************************************************************************
****************** FIGURE A.7. DATA AVAILABILITY BY COUNTRY/YEAR FOR THE DATASET USED IN SECTION 5
*****************************************************************************

ivreg2 ln_GDPpc		  			 ln_exportGDP  			  dummy_C* dummy_Y*   					if lpred_TOTAL_trad_5ys~=.					, cluster(country year) partial(dummy_C* dummy_Y*) noc
encode country, gen(country_num)
levelsof country, local(levels)			// create different dataset and merge them so that i can create a unique graph with time coverage of all exporters
gen countries=country
scalar ciao=0
label define countries 0 "none"
foreach lname of local  levels    {
scalar ciao=ciao+1
local ciao1=ciao
gen v`ciao1'=country_num if country=="`lname'" &  e(sample)==1
label define countries `ciao1' "`lname'", add
}
foreach l of numlist 1(1)37 {		
	label val v`l' countries
}
scatter  v1-v37 year, ylabel(, val angle(horizontal) labsize(small)) ylabel(1(1)37) xlabel(, angle(45) labsize(small)) xlabel(1845(5)1905) xtitle("Years") graphregion(fcolor(white) lwidth(medthin) lcolor(black)) legend(off) 
graph save "FigureA7_Time_Coverage_Exports.gph", replace
graph export FigureA7_Time_Coverage_Exports.tif, as(tif) width(800) replace


*****************************************************************************
****************** TABLE 6. TRADE AND DEVELOPMENT
*****************************************************************************
****PANEL A: Second stage
set more off
ivreg2 ln_GDPpc		  			 ln_exportGDP  			  dummy_C* dummy_Y* if lpred_TOTAL_trad_5ys~=., cluster(country year) partial(dummy_C* dummy_Y*)  noc
outreg2 using  Table6_corrected_Panel_A, replace  e(N) se dec(3)  ctitle(" ") cttop(" ") nonotes drop(const dummy_*)   excel tex 
est clear 
correction_se_IV	ln_GDPpc	ln_exportGDP " " Table6_corrected_Panel_A
est clear
correction_se_IV	ln_GDPpc	ln_exportGDP "[aweight=lpopulation_1860]" Table6_corrected_Panel_A
est clear
ivreg2 ln_GDPpc		  			 ln_exportpc  			  dummy_C* dummy_Y*   if ln_exportGDP~=. & lpred_TOTAL_trad_5ys~=., cluster(country year) partial(dummy_C* dummy_Y*)  noc
outreg2 using  Table6_corrected_Panel_A, append  e(N) se dec(3)  ctitle(" ") cttop(" ") nonotes drop(const dummy_*)   excel tex
est clear
correction_se_IV	ln_GDPpc	ln_exportpc " " Table6_corrected_Panel_A
est clear
correction_se_IV	ln_GDPpc	ln_exportpc "[aweight=lpopulation_1860]" Table6_corrected_Panel_A
est clear
correction_se_red	ln_GDPpc	" " Table6_corrected_Panel_A
est clear
correction_se_red	ln_GDPpc   "[aweight=lpopulation_1860]" Table6_corrected_Panel_A
est clear
****PANEL A: First stage
correction_se_red	ln_exportGDP	" " Table6_corrected_Panel_B
est clear
correction_se_red	ln_exportGDP   "[aweight=lpopulation_1860]" Table6_corrected_Panel_B
est clear
correction_se_red	ln_exportpc	" " Table6_corrected_Panel_B
est clear
correction_se_red	ln_exportpc   "[aweight=lpopulation_1860]" Table6_corrected_Panel_B
est clear

 
*****************************************************************************
****************** TABLE 7. TRADE, POPULATION DENSITY AND URBANIZATION RATES
*****************************************************************************
set more off
ivreg2 lpopulation		  			 ln_exportGDP  			  dummy_C* dummy_Y* if lpred_TOTAL_trad_5ys~=., cluster(country year)  partial(dummy_C* dummy_Y*) noc
outreg2 using  Table7_corrected, replace  e(N) se dec(3)  ctitle(" ") cttop(" ") nonotes drop(const dummy_*)   excel tex
est clear 
correction_se_IV	lpopulation	ln_exportGDP " " Table7_corrected
est clear
correction_se_IV	lpopulation	ln_exportGDP "[aweight=lpopulation_1860]" Table7_corrected
est clear
ivreg2 l1urban_50pc		  			 ln_exportGDP  			  dummy_C* dummy_Y* if lpred_TOTAL_trad_5ys~=., cluster(country year)  partial(dummy_C* dummy_Y*) noc
outreg2 using  Table7_corrected, append  e(N) se dec(3)  ctitle(" ") cttop(" ") nonotes drop(const dummy_*)   excel tex
est clear 
correction_se_IV	l1urban_50pc	ln_exportGDP " " Table7_corrected
est clear
correction_se_IV	l1urban_50pc	ln_exportGDP "[aweight=lpopulation_1860]" Table7_corrected
est clear
ivreg2 lpopulation		  			 ln_exportGDP  			  dummy_C* dummy_Y* if lpred_TOTAL_trad_5ys~=., cluster(country year)  partial(dummy_C* dummy_Y*) noc
outreg2 using  Table7_corrected, append  e(N) se dec(3)  ctitle(" ") cttop(" ") nonotes drop(const dummy_*)   excel tex
est clear 
correction_se_IV	l1urban_100pc	ln_exportGDP " " Table7_corrected
est clear
correction_se_IV	l1urban_100pc	ln_exportGDP "[aweight=lpopulation_1860]" Table7_corrected
est clear

*****************************************************************************
****************** TABLE A.7. TRADE, POPULATION DENSITY AND URBANIZATION RATES. ROBUSTNESS USING PER-CAPITA EXPORTS
*****************************************************************************

set more off
ivreg2 lpopulation		  			 ln_exportpc  			  dummy_C* dummy_Y* if lpred_TOTAL_trad_5ys~=. & ln_exportGDP~=., cluster(country year)  partial(dummy_C* dummy_Y*) noc
outreg2 using  TableA7_corrected, replace  e(N) se dec(3)  ctitle(" ") cttop(" ") nonotes drop(const dummy_*)   excel tex
est clear 
correction_se_IV	lpopulation	ln_exportpc " " TableA7_corrected
est clear
correction_se_IV	lpopulation	ln_exportpc "[aweight=lpopulation_1860]" TableA7_corrected
est clear
ivreg2 l1urban_50pc		  			 ln_exportpc  			  dummy_C* dummy_Y* if lpred_TOTAL_trad_5ys~=. & ln_exportGDP~=., cluster(country year)  partial(dummy_C* dummy_Y*) noc
outreg2 using  TableA7_corrected, append  e(N) se dec(3)  ctitle(" ") cttop(" ") nonotes drop(const dummy_*)   excel tex
est clear 
correction_se_IV	l1urban_50pc	ln_exportpc " " TableA7_corrected
est clear
correction_se_IV	l1urban_50pc	ln_exportpc "[aweight=lpopulation_1860]" TableA7_corrected
est clear
ivreg2 lpopulation		  			 ln_exportpc  			  dummy_C* dummy_Y* if lpred_TOTAL_trad_5ys~=. & ln_exportGDP~=., cluster(country year)  partial(dummy_C* dummy_Y*) noc
outreg2 using  TableA7_corrected, append  e(N) se dec(3)  ctitle(" ") cttop(" ") nonotes drop(const dummy_*)   excel tex
est clear 
correction_se_IV	l1urban_100pc	ln_exportpc " " TableA7_corrected
est clear
correction_se_IV	l1urban_100pc	ln_exportpc "[aweight=lpopulation_1860]" TableA7_corrected
est clear


*****************************************************************************
****************** TABLE 8. TRADE AND ECONOMIC DIVERGENCE
*****************************************************************************

***************** RE-CREATE TABLE 8 (Trade and the great divergence)

gen GDPpc_1850_1=GDPpc if year==1850
egen GDPpc_1850=min(GDPpc_1850_1), by(country)
drop GDPpc_1850_1
gen lGDPpc_1850=ln(GDPpc_1850)
xtile perc_lGDPpc_1850=lGDPpc_1850, nq(100)

summ lGDPpc_1850 if year==1850
scalar mean_gpd_1850	=r(mean)
gen above_mean_gdp_1850=(lGDPpc_1850>=mean_gpd_1850 & perc_lGDPpc_1850~=.)
gen above_75_gdp_1850=(perc_lGDPpc_1850>75 & perc_lGDPpc_1850~=.)
foreach vara of var above_mean_gdp_1850-above_75_gdp_1850 {
replace `vara'=. if perc_lGDPpc_1850==.
}

 gen instr_by_above_mean_gdp_1850 = lpred_TOTAL_trad_5ys*above_mean_gdp_1850		
 gen trade_by_above_mean_gdp_1850 = ln_exportGDP	    *above_mean_gdp_1850   
 gen trade2_by_above_mean_gdp_1850= ln_exportpc	    	*above_mean_gdp_1850   

 gen instr_by_above_p75_gdp_1850  = lpred_TOTAL_trad_5ys*above_75_gdp_1850		
 gen trade_by_above_p75_gdp_1850  = ln_exportGDP	  	*above_75_gdp_1850   
 gen trade2_by_above_p75_gdp_1850 = ln_exportpc	  		*above_75_gdp_1850 
 
 ivreg2 ln_GDPpc  	 					(ln_exportGDP trade_by_above_mean_gdp_1850= lpred_TOTAL_trad_5ys instr_by_above_mean_gdp_1850) dummy_C* dummy_Y*  							, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b1
 ivreg2 ln_GDPpc  	 					(ln_exportGDP trade_by_above_mean_gdp_1850= lpred_TOTAL_trad_5ys instr_by_above_mean_gdp_1850) dummy_C* dummy_Y* [aweight=lpopulation_1860] , cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b2

 ivreg2 ln_GDPpc  	 					(ln_exportGDP trade_by_above_p75_gdp_1850= lpred_TOTAL_trad_5ys instr_by_above_p75_gdp_1850)   dummy_C* dummy_Y*  							, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b3
 ivreg2 ln_GDPpc  	 					(ln_exportGDP trade_by_above_p75_gdp_1850= lpred_TOTAL_trad_5ys instr_by_above_p75_gdp_1850)   dummy_C* dummy_Y* [aweight=lpopulation_1860] , cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b4
 
 ivreg2 ln_GDPpc  	 					(ln_exportpc trade2_by_above_mean_gdp_1850= lpred_TOTAL_trad_5ys instr_by_above_mean_gdp_1850) dummy_C* dummy_Y*  						  	if ln_exportGDP~=.	, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b5
 ivreg2 ln_GDPpc  	 					(ln_exportpc trade2_by_above_mean_gdp_1850= lpred_TOTAL_trad_5ys instr_by_above_mean_gdp_1850) dummy_C* dummy_Y* [aweight=lpopulation_1860] if ln_exportGDP~=. 	, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b6

 ivreg2 ln_GDPpc  	 					(ln_exportpc trade2_by_above_p75_gdp_1850= lpred_TOTAL_trad_5ys instr_by_above_p75_gdp_1850)   dummy_C* dummy_Y*  						  	if ln_exportGDP~=.	, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b7
 ivreg2 ln_GDPpc  	 					(ln_exportpc trade2_by_above_p75_gdp_1850= lpred_TOTAL_trad_5ys instr_by_above_p75_gdp_1850)   dummy_C* dummy_Y* [aweight=lpopulation_1860] if ln_exportGDP~=.	, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b8
  
esta b* using Table8.tex, replace title(Table 8 Trade and economic convergence)  s(N widstat) se nostar






* ================================
* Country-level predicted effects
* Based on Table 8 regressions
* ================================
* ================================
* Country-level predicted effects
* ================================

cap mkdir results
cap erase results/predicted_effects.dta

foreach m in b1 b2 b3 b4 b5 b6 b7 b8 {
    
    est restore `m'

    * Step 1: find which interaction term exists in this model
    local inter ""
    foreach v in trade_by_above_mean_gdp_1850 trade_by_above_p75_gdp_1850 ///
                trade2_by_above_mean_gdp_1850 trade2_by_above_p75_gdp_1850 {
        capture noisily test _b[`v']=0
        if !_rc local inter `v'
    }

    di "Model `m' interaction term = `inter'"

    * Step 2: predict marginal effect for each country
    predictnl eff = _b[ln_exportGDP] + _b[`inter']*`inter', se(se_eff)

    * Step 3: collapse to country-level mean
    collapse (mean) eff se_eff, by(country)
    gen model = "`m'"

    tempfile tmp
    save `tmp'

    * Append results
    capture append using results/predicted_effects.dta
    save results/predicted_effects.dta, replace
}

* Export to CSV
use results/predicted_effects.dta, clear
export delimited using "results/predicted_effects.csv", replace











































*****************************************************************************
****************** TABLE 9. TRADE AND DEVELOPMENT. THE ROLE OF LOCAL INSTITUTIONS
*****************************************************************************

gen trade_by_initial= ln_exportGDP*cons1860   
gen trade2_by_initial= ln_exportpc*cons1860   
gen instr_by_initial= lpred_TOTAL_trad_5ys*cons1860

ivreg2 ln_GDPpc  	 					 ln_exportGDP trade_by_initial								   			dummy_C* dummy_Y* [aweight=lpopulation_1860]					, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b1
ivreg2 ln_GDPpc  	 					(ln_exportGDP trade_by_initial= lpred_TOTAL_trad_5ys instr_by_initial) 	dummy_C* dummy_Y* [aweight=lpopulation_1860] 					, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b2
ivreg2 ln_GDPpc  	 					(ln_exportpc trade2_by_initial= lpred_TOTAL_trad_5ys instr_by_initial) 	dummy_C* dummy_Y* [aweight=lpopulation_1860] if ln_exportGDP~=.	, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b3
ivreg2 lpopulation	 					 ln_exportGDP trade_by_initial                                			dummy_C* dummy_Y* [aweight=lpopulation_1860]					, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b4
ivreg2 lpopulation	 					(ln_exportGDP trade_by_initial= lpred_TOTAL_trad_5ys instr_by_initial) 	dummy_C* dummy_Y* [aweight=lpopulation_1860] 					, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b5
ivreg2 lpopulation	 					(ln_exportpc trade2_by_initial= lpred_TOTAL_trad_5ys instr_by_initial) 	dummy_C* dummy_Y* [aweight=lpopulation_1860] if ln_exportGDP~=.	, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b6 
esta b* using Table9.tex, replace title(Table 9 Trade and development: the tole of local institutions) se  s( N widstat)  nostar
est clear


*****************************************************************************
****************** TABLE A.8. TRADE AND DEVELOPMENT. THE ROLE OF LOCAL INSTITUTIONS. ROBUSTNESS: UNWEIGHTED RESULTS
*****************************************************************************

set more off
ivreg2 ln_GDPpc  	 					 ln_exportGDP trade_by_initial								   			dummy_C* dummy_Y*  						, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b1
ivreg2 ln_GDPpc  	 					(ln_exportGDP trade_by_initial= lpred_TOTAL_trad_5ys instr_by_initial) 	dummy_C* dummy_Y*  						, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b2
ivreg2 ln_GDPpc  	 					(ln_exportpc trade2_by_initial= lpred_TOTAL_trad_5ys instr_by_initial) 	dummy_C* dummy_Y*  if ln_exportGDP~=.	, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b3
ivreg2 lpopulation	 					 ln_exportGDP trade_by_initial                                			dummy_C* dummy_Y*  						, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b4
ivreg2 lpopulation	 					(ln_exportGDP trade_by_initial= lpred_TOTAL_trad_5ys instr_by_initial) 	dummy_C* dummy_Y*  						, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b5
ivreg2 lpopulation	 					(ln_exportpc trade2_by_initial= lpred_TOTAL_trad_5ys instr_by_initial) 	dummy_C* dummy_Y*  if ln_exportGDP~=.	, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b6 
esta b* using TableA8.tex, replace title(Table A8 Trade and development: the tole of local institutions. Unweighted regressions) se s( N widstat)  nostar
est clear

*****************************************************************************
****************** TABLE A.9. TRADE AND DEVELOPMENT. COLONIES VS INDEPENDENT STATES
*****************************************************************************

set more off
gen instr_by_initial1= lpred_TOTAL_trad_5ys*colony1850		
gen trade_by_initial1= ln_exportGDP	*colony1850

ivreg2 ln_GDPpc  	 				(ln_exportGDP trade_by_initial1= lpred_TOTAL_trad_5ys instr_by_initial1) dummy_C* dummy_Y*  							, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b1
ivreg2 ln_GDPpc  	 				(ln_exportGDP trade_by_initial1= lpred_TOTAL_trad_5ys instr_by_initial1) dummy_C* dummy_Y* [aweight=lpopulation_1860] 	, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b2
ivreg2 lpopulation 					(ln_exportGDP trade_by_initial1= lpred_TOTAL_trad_5ys instr_by_initial1) dummy_C* dummy_Y*  							, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b3
ivreg2 lpopulation 					(ln_exportGDP trade_by_initial1= lpred_TOTAL_trad_5ys instr_by_initial1) dummy_C* dummy_Y* [aweight=lpopulation_1860] 	, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b4
ivreg2 l1urban_50pc  	 			(ln_exportGDP trade_by_initial1= lpred_TOTAL_trad_5ys instr_by_initial1) dummy_C* dummy_Y*  							, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b5
ivreg2 l1urban_50pc  	 			(ln_exportGDP trade_by_initial1= lpred_TOTAL_trad_5ys instr_by_initial1) dummy_C* dummy_Y* [aweight=lpopulation_1860] 	, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b6
ivreg2 l1urban_100pc  	 			(ln_exportGDP trade_by_initial1= lpred_TOTAL_trad_5ys instr_by_initial1) dummy_C* dummy_Y*  							, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b7
ivreg2 l1urban_100pc  	 			(ln_exportGDP trade_by_initial1= lpred_TOTAL_trad_5ys instr_by_initial1) dummy_C* dummy_Y* [aweight=lpopulation_1860] 	, cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto b8 
 
drop instr_by_initial1 trade_by_initial1

esta b* using TableA9.tex, replace title(Table A.9 Trade and development: colonies versus independent states) se s(widstat N) nostar
est clear


*****************************************************************************
****************** TABLE 10. TRADE, INDUSTRIALIZATION AND URBANIZATION. THE ROLE OF LOCAL INSTITUTIONS
***************************************************************************** 

ivreg2 lSHARE_NONAGRI_EXP  				ln_exportGDP trade_by_initial								 			dummy_C* dummy_Y* [aweight=lpopulation_1860], cluster(country year) partial(dummy_C* dummy_Y*) noc 
est sto a1
ivreg2 lSHARE_NONAGRI_EXP 				(ln_exportGDP trade_by_initial= lpred_TOTAL_trad_5ys instr_by_initial)  dummy_C* dummy_Y* [aweight=lpopulation_1860], cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto a2
ivreg2 l1urban_50pc  					 ln_exportGDP trade_by_initial							 		 		dummy_C* dummy_Y* [aweight=lpopulation_1860], cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto a4
ivreg2 l1urban_50pc  					 (ln_exportGDP trade_by_initial= lpred_TOTAL_trad_5ys instr_by_initial) dummy_C* dummy_Y* [aweight=lpopulation_1860], cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto a5 
ivreg2 l1urban_100pc 					 ln_exportGDP trade_by_initial									 		dummy_C* dummy_Y* [aweight=lpopulation_1860], cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto a7
ivreg2 l1urban_100pc 					 (ln_exportGDP trade_by_initial= lpred_TOTAL_trad_5ys instr_by_initial) dummy_C* dummy_Y* [aweight=lpopulation_1860], cluster(country year) partial(dummy_C* dummy_Y*) noc
est sto a8
esta a* using Table10.tex, replace title(Table 10 Trade, industrialization and urbanization: the role of local institutions) se s( N widstat) nostar
est clear


*****************************************************************************
****************** TABLE A10. TRADE AND ECONOMIC DIVERGENCE: THE ROLE OF INITIAL SECTORAL COMPOSITION
*****************************************************************************

gen SHARE_NONAGRI_EXP_1860_1=SHARE_NONAGRI_EXP if year==1860
egen SHARE_NONAGRI_EXP_1860=min(SHARE_NONAGRI_EXP_1860_1), by(country)
gen lSHARE_NONAGRI_EXP_1860=ln(SHARE_NONAGRI_EXP_1860)
xtile perc_lSHARE_NONAGRI_EXP_1860=lSHARE_NONAGRI_EXP_1860, nq(100)

summ lSHARE_NONAGRI_EXP_1860 if year==1860
scalar mean_SHARE_NONAGRI_EXP_1860	=r(mean)
gen above_me_SHARE_NONAGRI_EXP_1860=(lSHARE_NONAGRI_EXP_1860>=mean_SHARE_NONAGRI_EXP_1860 & perc_lSHARE_NONAGRI_EXP_1860~=.)
gen above_75_SHARE_NONAGRI_EXP_1860=(perc_lSHARE_NONAGRI_EXP_1860>=75 & perc_lSHARE_NONAGRI_EXP_1860~=.)
foreach vara of var above_me_SHARE_NONAGRI_EXP_1860-above_75_SHARE_NONAGRI_EXP_1860 {
replace `vara'=. if perc_lSHARE_NONAGRI_EXP_1860==.
}

 gen instr_by_above_me_SNE_1860 = lpred_TOTAL_trad_5ys*above_me_SHARE_NONAGRI_EXP_1860		
 gen trade_by_above_me_SNE_1860 = ln_exportGDP	    *above_me_SHARE_NONAGRI_EXP_1860 
 gen trade2_by_above_me_SNE_1860= ln_exportpc	    	*above_me_SHARE_NONAGRI_EXP_1860  

 gen instr_by_above_p75_SNE_1860  = lpred_TOTAL_trad_5ys*above_75_SHARE_NONAGRI_EXP_1860		
 gen trade_by_above_p75_SNE_1860  = ln_exportGDP	  	*above_75_SHARE_NONAGRI_EXP_1860
 gen trade2_by_above_p75_SNE_1860 = ln_exportpc	  		*above_75_SHARE_NONAGRI_EXP_1860 
 
 ivreg2 ln_GDPpc  	 					(ln_exportGDP trade_by_above_me_SNE_1860= lpred_TOTAL_trad_5ys instr_by_above_me_SNE_1860) dummy_C* dummy_Y*  							, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b1
 ivreg2 ln_GDPpc  	 					(ln_exportGDP trade_by_above_me_SNE_1860= lpred_TOTAL_trad_5ys instr_by_above_me_SNE_1860) dummy_C* dummy_Y* [aweight=lpopulation_1860] , cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b2

 ivreg2 ln_GDPpc  	 					(ln_exportGDP trade_by_above_p75_SNE_1860= lpred_TOTAL_trad_5ys instr_by_above_p75_SNE_1860)   dummy_C* dummy_Y*  							, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b3
 ivreg2 ln_GDPpc  	 					(ln_exportGDP trade_by_above_p75_SNE_1860= lpred_TOTAL_trad_5ys instr_by_above_p75_SNE_1860)   dummy_C* dummy_Y* [aweight=lpopulation_1860] , cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b4
 
 ivreg2 ln_GDPpc  	 					(ln_exportpc trade2_by_above_me_SNE_1860= lpred_TOTAL_trad_5ys instr_by_above_me_SNE_1860) dummy_C* dummy_Y*  						  if ln_exportGDP~=.	, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b5
 ivreg2 ln_GDPpc  	 					(ln_exportpc trade2_by_above_me_SNE_1860= lpred_TOTAL_trad_5ys instr_by_above_me_SNE_1860) dummy_C* dummy_Y* [aweight=lpopulation_1860] if ln_exportGDP~=. 	, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b6

 ivreg2 ln_GDPpc  	 					(ln_exportpc trade2_by_above_p75_SNE_1860= lpred_TOTAL_trad_5ys instr_by_above_p75_SNE_1860)   dummy_C* dummy_Y*  						  if ln_exportGDP~=.	, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b7
 ivreg2 ln_GDPpc  	 					(ln_exportpc trade2_by_above_p75_SNE_1860= lpred_TOTAL_trad_5ys instr_by_above_p75_SNE_1860)   dummy_C* dummy_Y* [aweight=lpopulation_1860] if ln_exportGDP~=.	, cluster(country year) partial(dummy_C* dummy_Y*) noc
 est sto b8
  
esta b* using TableA10.tex, replace title(Table A10 Trade and economic convergence: the role of initial sectoral composition)  s(widstat N) se nostar
est clear
drop SHARE_NONAGRI_EXP_1860_1-trade2_by_above_p75_SNE_1860


*****************************************************************************
******************REGRESSIONS at COUNTRY LEVEL
*****************************************************************************
use "TEMP/TEMP100.dta", clear

  gen GDPpc1_1850=GDPpc if year==1850
  egen GDPpc_1850=min(GDPpc1_1850), by(country)
  gen GDPpc1_1905=GDPpc if year==1905
  egen GDPpc_1905=min(GDPpc1_1905), by(country)
  
  gen ln_exportGDP1_1905=ln_exportGDP if year==1905
  egen ln_exportGDP_1905=min(ln_exportGDP1_1905), by(country)
  gen ln_exportGDP1_1850=ln_exportGDP if year==1850
  egen ln_exportGDP_1850=min(ln_exportGDP1_1850), by(country)
  
  gen ln_exportpc1_1905=ln_exportpc if year==1905
  egen ln_exportpc_1905=min(ln_exportpc1_1905), by(country)
  gen ln_exportpc1_1850=ln_exportpc if year==1850
  egen ln_exportpc_1850=min(ln_exportpc1_1850), by(country)

  gen real_instrument1_1905=lpred_TOTAL_trad_5ys if year==1905
  egen real_instrument_1905=min(real_instrument1_1905), by(country)
  gen real_instrument1_1850=lpred_TOTAL_trad_5ys if year==1850
  egen real_instrument_1850=min(real_instrument1_1850), by(country)
  
  gen isolation1_1905=weight_ave_lsteam if year==1905
  egen isolation_1905=min(isolation1_1905), by(country)
  gen isolation1_1850=weight_ave_lsail2 if year==1850
  egen isolation_1850=min(isolation1_1850), by(country)
  
  gen isolationUK1_1905=top1_ave_lsteam if year==1905
  egen isolationUK_1905=min(isolationUK1_1905), by(country)
  gen isolationUK1_1850=top1_ave_lsail2 if year==1850
  egen isolationUK_1850=min(isolationUK1_1850), by(country)
    
  gen wisolation51_1905=weight_top5_ave_lsteam if year==1905
  egen wisolation5_1905=min(wisolation51_1905), by(country)
  gen wisolation51_1850=weight_top5_ave_lsail2 if year==1850
  egen wisolation5_1850=min(wisolation51_1850), by(country)
 
  
  
  keep if year==1850 
  drop year
 
  gen Change_exportGDP_1850_1905= ln_exportGDP_1905-ln_exportGDP_1850
  gen Change_exportpc_1850_1905	= ln_exportpc_1905-ln_exportpc_1850
  gen Change_GDP_1850_1905		= ln(GDPpc_1905)-ln(GDPpc_1850) 

  gen Change_isolation       =  isolation_1905 - isolation_1850
  gen Change_isolation_t1    =  isolationUK_1905 - isolationUK_1850 if country~="United Kingdom"
  gen Change_wisolation_t5   =  wisolation5_1905 - wisolation5_1850
    
  gen Change_real_instrument=real_instrument_1905 -real_instrument_1850 

 
*****************************************************************************
****************** TABLES 5 and A5 + FIGURES A5 and A6. TRADE AND ISOLATION
*****************************************************************************  
 set more off
******TABLE 5: Geographical Isolation and Trade (weighted results) FIRST PART
 reg Change_exportGDP_1850_1905 Change_isolation		[aweight=lpopulation_1860]
 est sto a1
 reg Change_exportGDP_1850_1905 Change_isolation_t1	[aweight=lpopulation_1860]
 est sto a2
 reg Change_exportGDP_1850_1905 Change_wisolation_t5	[aweight=lpopulation_1860]
 est sto a3 
 
******TABLE A.5 in APPENDIX: Geographical Isolation and Trade (unweighted results)  FIRST PART
 reg Change_exportGDP_1850_1905 Change_isolation		 
 est sto b1
 reg Change_exportGDP_1850_1905 Change_isolation_t1	 
 est sto b2
 reg Change_exportGDP_1850_1905 Change_wisolation_t5	 
 est sto b3
 
******FIGURE A.5 in APPENDIX: Geographical Isolation and Trade/GDP
 *Change in sailing times and change in exports
 preserve
 drop if Change_exportGDP_1850_1905==. |  Change_isolation==.
 twoway (scatter Change_exportGDP_1850_1905 Change_isolation  , xtitle("Log-change average time-to-sail (from sail to steam)") ytitle("Log-change export-to-GDP ratio")          msize(vtiny) mlabel(country) mlabsize(vsmall) legend(off) ) (lfit Change_exportGDP_1850_1905 Change_isolation  ,legend(off)) (lfitci Change_exportGDP_1850_1905 Change_isolation , level(89) ciplot(rline) legend(off)   )
 graph save in_exportGDP, replace  
 graph export FigureA5_isolation_exportGDP.tif, as(tif) width(800) replace
 restore
  
******TABLE 5: Geographical Isolation and Trade (weighted results) SECOND PART
 preserve
 use  "TEMP/DATA_TABLE_5.dta", clear

 gen GDPpc1_1850=GDPpc if year==1850
 egen GDPpc_1850=min(GDPpc1_1850), by(country)
 gen GDPpc1_1905=GDPpc if year==1905
 egen GDPpc_1905=min(GDPpc1_1905), by(country)
 
 gen ln_exportpc1_1905=ln_exportpc if year==1905
 egen ln_exportpc_1905=min(ln_exportpc1_1905), by(country)
 gen ln_exportpc1_1850=ln_exportpc if year==1850
 egen ln_exportpc_1850=min(ln_exportpc1_1850), by(country)

 gen isolation1_1905=weight_ave_lsteam if year==1905
 egen isolation_1905=min(isolation1_1905), by(country)
 gen isolation1_1850=weight_ave_lsail2 if year==1850
 egen isolation_1850=min(isolation1_1850), by(country)
  
 gen isolationUK1_1905=top1_ave_lsteam if year==1905
 egen isolationUK_1905=min(isolationUK1_1905), by(country)
 gen isolationUK1_1850=top1_ave_lsail2 if year==1850
 egen isolationUK_1850=min(isolationUK1_1850), by(country)
    
 gen wisolation51_1905=weight_top5_ave_lsteam if year==1905
 egen wisolation5_1905=min(wisolation51_1905), by(country)
 gen wisolation51_1850=weight_top5_ave_lsail2 if year==1850
 egen wisolation5_1850=min(wisolation51_1850), by(country)
  
 keep if year==1850 
 drop year
  
 gen Change_exportpc_1850_1905	= ln_exportpc_1905-ln_exportpc_1850
 
 gen Change_isolation       =  isolation_1905 - isolation_1850
 gen Change_isolation_t1    =  isolationUK_1905 - isolationUK_1850 if country~="United Kingdom"
 gen Change_wisolation_t5   =  wisolation5_1905 - wisolation5_1850

 reg Change_exportpc_1850_1905 Change_isolation		[aweight=lpopulation_1860] 
 est sto a4
 reg Change_exportpc_1850_1905 Change_isolation_t1	[aweight=lpopulation_1860] 
 est sto a5
 reg Change_exportpc_1850_1905 Change_wisolation_t5	[aweight=lpopulation_1860] 
 est sto a6
 
 esta a* using Table5.tex, replace se title(Table 5 Geographical isolation and trade  (weighted results))  s(r2 N) nostar
  
******TABLE A.5 in APPENDIX: Geographical Isolation and Trade (unweighted results)  SECOND PART
 reg Change_exportpc_1850_1905 Change_isolation		
 est sto b4
 reg Change_exportpc_1850_1905 Change_isolation_t1	 
 est sto b5
 reg Change_exportpc_1850_1905 Change_wisolation_t5	
 est sto b6 
 
 esta b* using TableA5.tex, replace se title(Table A5 Geographical isolation and trade (unweighted results))  s(r2 N) nostar
 est clear
   
******FIGURE A.6 in APPENDIX: Geographical Isolation and Trade/Population
 drop if Change_exportpc_1850_1905==. |  Change_isolation==.
 egen w_average_Change_exportpc   = wtmean(Change_exportpc_1850_1905)   , weight(population_1860) 
 tab w_average_Change_exportpc
 
 twoway (scatter Change_exportpc_1850_1905 Change_isolation   ,  xtitle("Log-change average time-to-sail (from sail to steam)") ytitle("Log-change export-to-population ratio")  msize(vtiny) mlabel(country) mlabsize(vsmall) legend(off) ) (lfit Change_exportpc_1850_1905  Change_isolation  ,legend(off)) (lfitci Change_exportpc_1850_1905  Change_isolation, level(89) ciplot(rline)  legend(off)    )
 graph save in_exportpc, replace  
 graph export FigureA6_isolation_exportpc.tif, as(tif) width(800) replace
 restore 

*****************************************************************************
****************** FIGURE A8. FIRST STAGE
*****************************************************************************  
 *Panel A
 preserve
 drop if Change_GDP_1850_1905==. |  Change_real_instrument==.
 egen Change_real_instrument_std=std(Change_real_instrument)
 reg Change_exportGDP_1850_1905 Change_real_instrument_std
 twoway (scatter Change_exportGDP_1850_1905 Change_real_instrument_std  ,  xtitle("Log-change Predicted Trade (standardized)") ytitle("Log-change export-to-GDP ratio")              msize(vtiny) mlabel(country) mlabsize(vsmall) legend(off) mlabpos(6) ) (lfit Change_exportGDP_1850_1905 Change_real_instrument_std  , legend(off)) (lfitci Change_exportGDP_1850_1905      Change_real_instrument_std, level(89) ciplot(rline)  legend(off)  )   
 graph save in_first_stage_exportGDP, replace
 restore
 *Panel B
 preserve
 drop if Change_GDP_1850_1905==. |  Change_real_instrument==.
 egen Change_real_instrument_std=std(Change_real_instrument)
 reg Change_exportpc_1850_1905 Change_real_instrument_std
 twoway (scatter Change_exportpc_1850_1905 Change_real_instrument_std   ,  xtitle("Log-change Predicted Trade (standardized)") ytitle("Log-change export-to-population ratio")       msize(vtiny) mlabel(country) mlabsize(vsmall) legend(off) mlabpos(6) ) (lfit Change_exportpc_1850_1905 Change_real_instrument_std  , legend(off)) (lfitci Change_exportpc_1850_1905      Change_real_instrument_std, level(89) ciplot(rline)  legend(off)    )   
 graph save in_first_stage_exportpc, replace
 restore
 graph combine in_first_stage_exportGDP.gph in_first_stage_exportpc.gph
 graph save FigureA8_First_stage, replace
 graph export FigureA8_First_stage.tif, as(tif) width(800) replace
 
*****************************************************************************
****************** FIGURE A9. REDUCED FORM: PREDICTED TRADE AND PER-CAPITA GDP
*****************************************************************************  
 preserve
 drop if Change_GDP_1850_1905==. |  Change_real_instrument==.
 egen Change_real_instrument_std=std(Change_real_instrument)
 reg Change_GDP_1850_1905 Change_real_instrument_std
 twoway (scatter Change_GDP_1850_1905 Change_real_instrument_std        ,  xtitle("Log-change Predicted Trade (standardized)") ytitle("Log-change per-capita GDP")                  msize(vtiny) mlabel(country) mlabsize(vsmall) legend(off) mlabpos(6) ) (lfit Change_GDP_1850_1905       Change_real_instrument_std  , legend(off)) (lfitci Change_GDP_1850_1905      Change_real_instrument_std, level(89) ciplot(rline)  legend(off)    )   
 graph save FigureA9_ReducedForm, replace
 graph export FigureA9_ReducedForm.tif, as(tif) width(800) replace
 restore
 
*****************************************************************************
****************** FIGURE A10. REDUCED FORM: PREDICTED TRADE AND PER-CAPITA GDP FOR NON-EUROPEAN COUNTRIES
*****************************************************************************  
 preserve
 gen Europe=1 if continent=="1 EUROPE WEST" |continent=="2 EUROPE EAST"
 gen No_Europe=1 if Europe==.
 drop if Change_GDP_1850_1905==. |  Change_real_instrument==.
 egen Change_real_instrument_std=std(Change_real_instrument)
 keep if Europe==.
 reg Change_GDP_1850_1905 Change_real_instrument_std
 twoway (scatter Change_GDP_1850_1905 Change_real_instrument_std        ,  xtitle("Log-change Predicted Trade (standardized)") ytitle("Log-change per-capita GDP")                   msize(vtiny) mlabel(country) mlabsize(vsmall) legend(off) mlabpos(6) ) (lfit Change_GDP_1850_1905       Change_real_instrument_std  , legend(off)) (lfitci Change_GDP_1850_1905      Change_real_instrument_std, level(89) ciplot(rline)  legend(off)   )   
 graph save FigureA10_ReducedForm_nonEurope, replace
 graph export FigureA10_ReducedForm_nonEurope.tif, as(tif) width(800) replace
 restore
 
