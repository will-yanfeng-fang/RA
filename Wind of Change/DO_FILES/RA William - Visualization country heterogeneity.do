clear all
set more off
cd "/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change"
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
  
esttab b* using Table8.tex, replace title(Table 8 Trade and economic convergence)  s(N widstat) se nostar

save "TEMP/TEMP101.dta", replace
***************************************************************************************************
*********************************************************************************

* ===================================================
* Predicted Trade Effects by Country (Table 8 models)
* ===================================================
cap mkdir results
cap erase results/predicted_effects.dta

* -------------------------
* b1
use TEMP/TEMP101.dta, clear
est restore b1
predictnl eff_b1 = _b[ln_exportGDP] + _b[trade_by_above_mean_gdp_1850]*above_mean_gdp_1850, se(se_b1)
collapse (mean) eff_b1 se_b1, by(country)
gen model = "b1"
save results/predicted_effects.dta, replace

* -------------------------
* b2
use TEMP/TEMP101.dta, clear
est restore b2
predictnl eff_b2 = _b[ln_exportGDP] + _b[trade_by_above_mean_gdp_1850]*above_mean_gdp_1850, se(se_b2)
collapse (mean) eff_b2 se_b2, by(country)
gen model = "b2"
append using results/predicted_effects.dta
save results/predicted_effects.dta, replace

* -------------------------
* b3
use TEMP/TEMP101.dta, clear
est restore b3
predictnl eff_b3 = _b[ln_exportGDP] + _b[trade_by_above_p75_gdp_1850]*above_75_gdp_1850, se(se_b3)
collapse (mean) eff_b3 se_b3, by(country)
gen model = "b3"
append using results/predicted_effects.dta
save results/predicted_effects.dta, replace

* -------------------------
* b4
use TEMP/TEMP101.dta, clear
est restore b4
predictnl eff_b4 = _b[ln_exportGDP] + _b[trade_by_above_p75_gdp_1850]*above_75_gdp_1850, se(se_b4)
collapse (mean) eff_b4 se_b4, by(country)
gen model = "b4"
append using results/predicted_effects.dta
save results/predicted_effects.dta, replace

* -------------------------
* b5
use TEMP/TEMP101.dta, clear
est restore b5
predictnl eff_b5 = _b[ln_exportpc] + _b[trade2_by_above_mean_gdp_1850]*above_mean_gdp_1850, se(se_b5)
collapse (mean) eff_b5 se_b5, by(country)
gen model = "b5"
append using results/predicted_effects.dta
save results/predicted_effects.dta, replace

* -------------------------
* b6
use TEMP/TEMP101.dta, clear
est restore b6
predictnl eff_b6 = _b[ln_exportpc] + _b[trade2_by_above_mean_gdp_1850]*above_mean_gdp_1850, se(se_b6)
collapse (mean) eff_b6 se_b6, by(country)
gen model = "b6"
append using results/predicted_effects.dta
save results/predicted_effects.dta, replace

* -------------------------
* b7
use TEMP/TEMP101.dta, clear
est restore b7
predictnl eff_b7 = _b[ln_exportpc] + _b[trade2_by_above_p75_gdp_1850]*above_75_gdp_1850, se(se_b7)
collapse (mean) eff_b7 se_b7, by(country)
gen model = "b7"
append using results/predicted_effects.dta
save results/predicted_effects.dta, replace

* -------------------------
* b8
use TEMP/TEMP101.dta, clear
est restore b8
predictnl eff_b8 = _b[ln_exportpc] + _b[trade2_by_above_p75_gdp_1850]*above_75_gdp_1850, se(se_b8)
collapse (mean) eff_b8 se_b8, by(country)
gen model = "b8"
append using results/predicted_effects.dta
save results/predicted_effects.dta, replace

* -------------------------
* Export to CSV
use results/predicted_effects.dta, clear
export delimited using "results/predicted_effects.csv", replace


* ===============================
* Country-specific effects graphs
* ===============================
use results/predicted_effects.dta, clear

* -------------------------
* b1
keep if model == "b1"
rename eff_b1 eff
rename se_b1 se
gen ci_low = eff - 1.96*se
gen ci_high = eff + 1.96*se
sort eff
gen order = _n

label define countrylbl 0 ""
levelsof country, local(clist)
local i = 1
foreach c of local clist {
    label define countrylbl `i' "`c'", add
    local ++i
}
label values order countrylbl

twoway (rcap ci_low ci_high order, lcolor(navy)) ///
       (scatter eff order, msymbol(O) mcolor(navy)) ///
       , yline(0, lcolor(red)) ///
         title("Predicted Trade Effects by Country (b1)") ///
         subtitle("ln(GDPpc) = β1 ln(export/GDP) + β2 [ln(export/GDP) × AboveMeanGDP1850] + FE", size(small)) ///
         xtitle("Country") ytitle("Effect on ln(GDPpc)") ///
         xlabel(1(1)`=_N', angle(50) valuelabel labsize(small)) ///
         legend(off) scheme(s1color)
graph export "results/predicted_effects_b1.png", replace
use results/predicted_effects.dta, clear

* -------------------------
* b2
keep if model == "b2"
rename eff_b2 eff
rename se_b2 se
gen ci_low = eff - 1.96*se
gen ci_high = eff + 1.96*se
sort eff
gen order = _n
label define countrylbl 0 ""
levelsof country, local(clist)
local i = 1
foreach c of local clist {
    label define countrylbl `i' "`c'", add
    local ++i
}
label values order countrylbl
twoway (rcap ci_low ci_high order, lcolor(navy)) ///
       (scatter eff order, msymbol(O) mcolor(navy)) ///
       , yline(0, lcolor(red)) ///
         title("Predicted Trade Effects by Country (b2)") ///
         subtitle("ln(GDPpc) = β1 ln(export/GDP) + β2 [ln(export/GDP) × AboveMeanGDP1850] + FE, weighted", size(small)) ///
         xtitle("Country") ytitle("Effect on ln(GDPpc)") ///
         xlabel(1(1)`=_N', angle(50) valuelabel labsize(small)) ///
         legend(off) scheme(s1color)
graph export "results/predicted_effects_b2.png", replace
use results/predicted_effects.dta, clear

* -------------------------
* b3
keep if model == "b3"
rename eff_b3 eff
rename se_b3 se
gen ci_low = eff - 1.96*se
gen ci_high = eff + 1.96*se
sort eff
gen order = _n
label define countrylbl 0 ""
levelsof country, local(clist)
local i = 1
foreach c of local clist {
    label define countrylbl `i' "`c'", add
    local ++i
}
label values order countrylbl
twoway (rcap ci_low ci_high order, lcolor(navy)) ///
       (scatter eff order, msymbol(O) mcolor(navy)) ///
       , yline(0, lcolor(red)) ///
         title("Predicted Trade Effects by Country (b3)") ///
         subtitle("ln(GDPpc) = β1 ln(export/GDP) + β2 [ln(export/GDP) × AboveTop25%GDP1850] + FE", size(small)) ///
         xtitle("Country") ytitle("Effect on ln(GDPpc)") ///
         xlabel(1(1)`=_N', angle(50) valuelabel labsize(small)) ///
         legend(off) scheme(s1color)
graph export "results/predicted_effects_b3.png", replace
use results/predicted_effects.dta, clear

* -------------------------
* b4
keep if model == "b4"
rename eff_b4 eff
rename se_b4 se
gen ci_low = eff - 1.96*se
gen ci_high = eff + 1.96*se
sort eff
gen order = _n
label define countrylbl 0 ""
levelsof country, local(clist)
local i = 1
foreach c of local clist {
    label define countrylbl `i' "`c'", add
    local ++i
}
label values order countrylbl
twoway (rcap ci_low ci_high order, lcolor(navy)) ///
       (scatter eff order, msymbol(O) mcolor(navy)) ///
       , yline(0, lcolor(red)) ///
         title("Predicted Trade Effects by Country (b4)") ///
         subtitle("ln(GDPpc) = β1 ln(export/GDP) + β2 [ln(export/GDP) × AboveTop25%GDP1850] + FE, weighted", size(small)) ///
         xtitle("Country") ytitle("Effect on ln(GDPpc)") ///
         xlabel(1(1)`=_N', angle(50) valuelabel labsize(small)) ///
         legend(off) scheme(s1color)
graph export "results/predicted_effects_b4.png", replace
use results/predicted_effects.dta, clear

* -------------------------
* b5
keep if model == "b5"
rename eff_b5 eff
rename se_b5 se
gen ci_low = eff - 1.96*se
gen ci_high = eff + 1.96*se
sort eff
gen order = _n
label define countrylbl 0 ""
levelsof country, local(clist)
local i = 1
foreach c of local clist {
    label define countrylbl `i' "`c'", add
    local ++i
}
label values order countrylbl
twoway (rcap ci_low ci_high order, lcolor(navy)) ///
       (scatter eff order, msymbol(O) mcolor(navy)) ///
       , yline(0, lcolor(red)) ///
         title("Predicted Trade Effects by Country (b5)") ///
         subtitle("ln(GDPpc) = β1 ln(export/pop) + β2 [ln(export/pop) × AboveMeanGDP1850] + FE", size(small)) ///
         xtitle("Country") ytitle("Effect on ln(GDPpc)") ///
         xlabel(1(1)`=_N', angle(50) valuelabel labsize(small)) ///
         legend(off) scheme(s1color)
graph export "results/predicted_effects_b5.png", replace
use results/predicted_effects.dta, clear

* -------------------------
* b6
keep if model == "b6"
rename eff_b6 eff
rename se_b6 se
gen ci_low = eff - 1.96*se
gen ci_high = eff + 1.96*se
sort eff
gen order = _n
label define countrylbl 0 ""
levelsof country, local(clist)
local i = 1
foreach c of local clist {
    label define countrylbl `i' "`c'", add
    local ++i
}
label values order countrylbl
twoway (rcap ci_low ci_high order, lcolor(navy)) ///
       (scatter eff order, msymbol(O) mcolor(navy)) ///
       , yline(0, lcolor(red)) ///
         title("Predicted Trade Effects by Country (b6)") ///
         subtitle("ln(GDPpc) = β1 ln(export/pop) + β2 [ln(export/pop) × AboveMeanGDP1850] + FE, weighted", size(small)) ///
         xtitle("Country") ytitle("Effect on ln(GDPpc)") ///
         xlabel(1(1)`=_N', angle(50) valuelabel labsize(small)) ///
         legend(off) scheme(s1color)
graph export "results/predicted_effects_b6.png", replace
use results/predicted_effects.dta, clear

* -------------------------
* b7
keep if model == "b7"
rename eff_b7 eff
rename se_b7 se
gen ci_low = eff - 1.96*se
gen ci_high = eff + 1.96*se
sort eff
gen order = _n
label define countrylbl 0 ""
levelsof country, local(clist)
local i = 1
foreach c of local clist {
    label define countrylbl `i' "`c'", add
    local ++i
}
label values order countrylbl
twoway (rcap ci_low ci_high order, lcolor(navy)) ///
       (scatter eff order, msymbol(O) mcolor(navy)) ///
       , yline(0, lcolor(red)) ///
         title("Predicted Trade Effects by Country (b7)") ///
         subtitle("ln(GDPpc) = β1 ln(export/pop) + β2 [ln(export/pop) × AboveTop25%GDP1850] + FE", size(small)) ///
         xtitle("Country") ytitle("Effect on ln(GDPpc)") ///
         xlabel(1(1)`=_N', angle(50) valuelabel labsize(small)) ///
         legend(off) scheme(s1color)
graph export "results/predicted_effects_b7.png", replace
use results/predicted_effects.dta, clear

* -------------------------
* b8
keep if model == "b8"
rename eff_b8 eff
rename se_b8 se
gen ci_low = eff - 1.96*se
gen ci_high = eff + 1.96*se
sort eff
gen order = _n
label define countrylbl 0 ""
levelsof country, local(clist)
local i = 1
foreach c of local clist {
    label define countrylbl `i' "`c'", add
    local ++i
}
label values order countrylbl
twoway (rcap ci_low ci_high order, lcolor(navy)) ///
       (scatter eff order, msymbol(O) mcolor(navy)) ///
       , yline(0, lcolor(red)) ///
         title("Predicted Trade Effects by Country (b8)") ///
         subtitle("ln(GDPpc) = β1 ln(export/pop) + β2 [ln(export/pop) × AboveTop25%GDP1850] + FE, weighted", size(small)) ///
         xtitle("Country") ytitle("Effect on ln(GDPpc)") ///
         xlabel(1(1)`=_N', angle(50) valuelabel labsize(small)) ///
         legend(off) scheme(s1color)
graph export "results/predicted_effects_b8.png", replace







****************************************************
* Country-specific effects (Table 9 with cons1860)
****************************************************
cd "/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change"
use "TEMP/TEMP101.dta", clear
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
esttab b* using Table9.tex, replace title(Table 9 Trade and development: the tole of local institutions) se  s( N widstat)  nostar


* Re-run the key IV regression and store estimates
****************************************************
* Predict country-specific effects from Table 9 (IV)
****************************************************
cd "/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change"

* Restore the key regression
est restore b2

* Grab coefficient vector and variance-covariance matrix
matrix b  = e(b)
matrix V  = e(V)

* Identify coefficient positions
local idx_trade = colnumb(b, "ln_exportGDP")
local idx_int   = colnumb(b, "trade_by_initial")

* Store coefficients
scalar b_trade = b[1, `idx_trade']
scalar b_int   = b[1, `idx_int']

* Store relevant var/cov elements
scalar v11 = V[`idx_trade', `idx_trade']
scalar v22 = V[`idx_int',   `idx_int']
scalar v12 = V[`idx_trade', `idx_int']

* Now build predicted effects by country
preserve
collapse (mean) cons1860, by(country)

gen eff = b_trade + b_int*cons1860

* Variance of linear combo: v11 + c^2*v22 + 2*c*v12
gen var_eff = v11 + (cons1860^2)*v22 + 2*cons1860*v12
gen se      = sqrt(var_eff)

gen ci_low  = eff - 1.96*se
gen ci_high = eff + 1.96*se

sort eff
gen order = _n

save "results/predicted_effects_table9.dta", replace
restore





* 1) Load dataset
cd "/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change"
use "results/predicted_effects_table9.dta", clear

* 1) Create combined label: Country (cons1860)
tostring cons1860, gen(cons1860_str) format(%9.0g) force
gen country_label = country + " (" + cons1860_str + ")"

* 2) Confidence intervals
capture confirm variable ci_low
if _rc {
    gen ci_low  = eff - 1.96*se
    gen ci_high = eff + 1.96*se
}

* 3) Ensure plotting order 1..N sorted by eff
gsort eff
drop order
gen order = _n

* 4) Build value labels for 'order' from country_label in this sorted order
capture label drop ylab
quietly count
local N = r(N)
forvalues i = 1/`N' {
    local lbl = country_label[`i']
    label define ylab `i' `"`lbl'"', add
}
label values order ylab

* 5) Plot with value labels on the y-axis, ranked low → high
twoway ///
    (rcap ci_low ci_high order, horizontal lcolor(gs8)) ///
    (scatter order eff, msymbol(O) msize(small) mcolor(black)), ///
    xline(0, lpattern(dash) lcolor(gs8)) ///
    xlabel(-1(1)1) ///
    ylabel(1/`N', valuelabel angle(0) noticks labsize(vsmall)) ///
    yscale(reverse range(0.5 `= `N' + 0.5')) ///
    ytitle("") ///
    xtitle("d ln(GDPpc) / d ln(export/GDP)") ///
    title("Effect of ln(export/GDP) on ln(GDP per capita), by country", size(medsmall)) ///
    subtitle("OLS; Table 9 col 1", size(small)) ///
    note("Dot = estimate; line = 95% CI; countries ordered by estimate" ///
         "cons_1860 shown in parentheses", size(vsmall)) ///
    legend(off)
