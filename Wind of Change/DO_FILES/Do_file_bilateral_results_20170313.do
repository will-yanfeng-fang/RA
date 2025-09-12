clear all
set more off
set maxvar 10000
set matsize 2000
set varabbrev on
global REPLICATIONroot "/Users/Luigi/Dropbox/SAILING PROJECT/REPLICATION MATERIAL 24Feb2017/REPLICATION_MATERIAL_SUBMITTED/"

cd "$REPLICATIONroot"
adopath + "$REPLICATIONroot/ADO_FILES/"
adopath + "$REPLICATIONroot/ADO_FILES/esta/"

*******************************************************************************************************************************
** The Wind of Change: Maritime Technology, Trade and Economic Development
** American Economic Review 2017
** Luigi Pascali

*This do-file generates all the descriptive statistics, regressions results, and scatter plot associated with the country-pair analysis:
*Table 1 (Panels A, B and C), Tables 2-4, Table A.2, Table A.4, Figure A.3
*******************************************************************************************************************************

************************************************************************************************************************
****************** INSTALL RELEVANT ADO-FILES
ssc install latab, replace
net sj 13-3 st0143_4
net install st0143_4, replace


************************************************************************************************************************
****************** DEFINE VARIABLES TO BE USED IN GRAVITY REGRESSIONS 
use "PUBLIC_DATA/BILATERAL_TRADE_PUBLIC.dta", clear
gen lexpr=log(expr)
gen lsteam=log(TIME_4_2) if year<1870
replace lsteam=log(TIME_4_1) if year>=1870
gen lsail2=log(TIME_5_2_5) if year<1870
replace lsail2=log(TIME_5_1_5) if year>=1870
gen ldist=log(geo_dist)
gen pair=country_d+country_o if country_d>country_o
replace pair=country_o+country_d if country_d<=country_o	


gen d6_60=1 if year<=1860 
replace d6_60=0 if d6_60==.
gen d6_65=1 if year<=1865 & year>1860
replace d6_65=0 if d6_65==.
gen d6_70=1 if year<=1870 & year>1865
replace d6_70=0 if d6_70==.
gen d6_75=1 if year<=1875 & year>1870
replace d6_75=0 if d6_75==.
gen d6_80=1 if year<=1880 & year>1875
replace d6_80=0 if d6_80==.
gen d6_85=1 if year<=1885 & year>1880
replace d6_85=0 if d6_85==.
gen d6_90=1 if year<=1890 & year>1885
replace d6_90=0 if d6_90==.
gen d6_95=1 if year<=1895 & year>1890
replace d6_95=0 if d6_95==.
gen d6_00=1 if year<=1900 & year>1895
replace d6_00=0 if d6_00==.

gen dd6_60lsteam=   c.d6_60#c.lsteam
gen dd6_b65lsteam=  c.d6_65#c.lsteam 
gen dd6_b70lsteam=  c.d6_70#c.lsteam 
gen dd6_b75lsteam=  c.d6_75#c.lsteam 
gen dd6_b80lsteam=  c.d6_80#c.lsteam 
gen dd6_b85lsteam=  c.d6_85#c.lsteam  
gen dd6_b90lsteam=  c.d6_90#c.lsteam 
gen dd6_b95lsteam=  c.d6_95#c.lsteam   
gen dd6_b00lsteam=  c.d6_00#c.lsteam

gen dd6_60lsail2 =  c.d6_60#c.lsail2
gen dd6_b65lsail2 = c.d6_65#c.lsail2 
gen dd6_b70lsail2 = c.d6_70#c.lsail2 
gen dd6_b75lsail2 = c.d6_75#c.lsail2 
gen dd6_b80lsail2 = c.d6_80#c.lsail2 
gen dd6_b85lsail2 = c.d6_85#c.lsail2 
gen dd6_b90lsail2 = c.d6_90#c.lsail2 
gen dd6_b95lsail2 = c.d6_95#c.lsail2  
gen dd6_b00lsail2 = c.d6_00#c.lsail2 

save "PUBLIC_DATA/BILATERAL_TRADE_PUBLIC2.dta",replace


************************************************************************************************************************
****************** DEFINE VARIABLES TO BE USED IN FREIGHT RATES REGRESSIONS

use "PUBLIC_DATA/FREIGHT_RATES_PUBLIC.dta", clear

**The following lines imply that we will have 1 obs per year/origin/destination.
gen vojage= origin +destination
gen vojage_country=origin_country+destination_country
egen mean_shipping_cost_route_year=mean( Tot_Shellings_per_tons), by( vojage year)
by vojage year, sort: keep if _n==1
drop Tot_Shellings_per_tons
gen Tot_Shellings_per_tons=mean_shipping_cost_route_year
drop mean_shipping_cost_route_year

keep if  year>=1840 & year<=1900
gen lfreight=log(Tot_Shellings_per_tons)

gen lsteam=log(STEAM_CLOSED) if year<1870
replace lsteam=log(STEAM_OPEN) if year>=1870
gen lsail2=log(SAIL_MAY_CLOSED) if year<1870
replace lsail2=log(SAIL_MAY_OPEN) if year>=1870
gen ldist=log(GEO_DISTANCE)

tabulate year, gen(dummy_Y)
tabulate origin_country, gen(dummy_country_ORI)
tabulate destination_country, gen(dummy_country_DES)
tabulate product_rev_, gen(dummy_PROD)
tabulate vojage, gen(int_DUM)
tabulate vojage_country, gen(int_country_DUM)

* Construct time dummies (each dummy is for a decade between 1850 and 1900)

gen d_60=1 if year<=1860 
replace d_60=0 if d_60==.
gen d_70=1 if year<=1870 & year>1860
replace d_70=0 if d_70==.
gen d_80=1 if year<=1880 & year>1870
replace d_80=0 if d_80==.
gen d_90=1 if year<=1890 & year>1880
replace d_90=0 if d_90==.
gen d_00=1 if year<=1900 & year>1890
replace d_00=0 if d_00==.

gen dd5_60_lsail2=d_60*lsail2
gen dd5_70_lsail2=d_70*lsail2
gen dd5_80_lsail2=d_80*lsail2
gen dd5_90_lsail2=d_90*lsail2
gen dd5_00_lsail2=d_00*lsail2

gen dd5_60_lsteam=d_60*lsteam
gen dd5_70_lsteam=d_70*lsteam
gen dd5_80_lsteam=d_80*lsteam
gen dd5_90_lsteam=d_90*lsteam
gen dd5_00_lsteam=d_00*lsteam

gen 	decade=40  if year<1850 
replace decade=50  if year<=1860 & year>1850
replace decade=60  if year<=1870 & year>1860
replace decade=70  if year<=1880 & year>1870
replace decade=80  if year<=1890 & year>1880
replace decade=90  if year<=1900 & year>1890
replace decade=100 if year<=1910 & year>1900

save "PUBLIC_DATA/FREIGHT_RATES_PUBLIC2.dta",replace


*****************************************************************************
****************** TABLE 1 (PANEL A, B and C). DESCRIPTIVE STATISTICS
*****************************************************************************

****TABLE 1 PANEL A
use "PUBLIC_DATA/BILATERAL_DISTANCES_PUBLIC.dta", clear
latabstat TIME_4_2 TIME_4_1 TIME_5_2_5 TIME_5_1_5 geo_dist, s(mean p50 sd min max N) col(stat) tf(Table1_part1) f(%9.2fc) replace


****TABLE 1 PANEL B (Part 1)
use "PUBLIC_DATA/FREIGHT_RATES_PUBLIC2.dta", clear
ivreg2 lfreight dd5_60_lsteam dd5_70_lsteam dd5_80_lsteam dd5_90_lsteam dd5_00_lsteam  dd5_60_lsail2 dd5_70_lsail2 dd5_80_lsail2 dd5_90_lsail2 dd5_00_lsail2 dummy_Y* dummy_country*	dummy_PROD*		, cluster(destination_country year) partial(dummy_Y* dummy_country* dummy_PROD*)
gen freight=exp(lfreight)
latabstat freight   if e(sample)==1,s(mean p50 sd min max N) col(stat) tf(Table1_part1) f(%9.2fc) append


***TABLE 1 PANEL B (Part 2)
use "PUBLIC_DATA/CLIWOC_PUBLIC.dta", clear
gen voj_duration_hours=voj_duration*24
gen clipper_min=(TIME_5_2_5)
drop if voj_duration<=1 | voj_duration>180 
drop if clipper_min <=(1*24) | clipper_min >(180*24) 
reg voj_duration_hours clipper_min 
latabstat voj_duration_hours   if e(sample)==1,s(mean p50 sd min max N) col(stat) tf(Table1_part1) f(%9.2fc) append


***TABLE 1 PANEL C
use "PUBLIC_DATA/BILATERAL_TRADE_PUBLIC2.dta", clear
reg  lexpr dd6_*m dd6_*l2 dummy_Y* dummy_ORI* dummy_DES* 
latabstat expr if e(sample)==1				, s(mean p50 sd min max N) col(stat) tf(Table1_part1) f(%9.2fc) append


*****************************************************************************
****************** TABLE A.2. LIST OF COUNTRIES WITH AVAILABLE BILATERAL TRADE DATA
*****************************************************************************

use "PUBLIC_DATA/BILATERAL_TRADE_PUBLIC2.dta", clear
reg  lexpr dd6_*m dd6_*l2 dummy_Y* dummy_ORI* dummy_DES* 
keep if e(sample)==1

preserve
by country_o, sort: keep if _n==1
rename country_o country
keep country
save "TEMP/TEMP_aa1", replace
restore

preserve
by country_d, sort: keep if _n==1
rename country_d country
keep country
save "TEMP/TEMP_aa2", replace
restore

clear 
use "TEMP/TEMP_aa1", clear
append using "TEMP/TEMP_aa2"
by country, sort: keep if _n==1
export delimited using "TableA2.txt", replace



*****************************************************************************
******************TABLE 2 and FIGURE A3. ACTUAL AND OPTIMIZED DURATION OF VOYAGES BY SAIL
*****************************************************************************

set more off
use "PUBLIC_DATA/CLIWOC_PUBLIC.dta", clear

gen voj_duration_hours=voj_duration*24


gen clipper_min=(TIME_5_2_5)

drop if voj_duration<=1 | voj_duration>180 
drop if clipper_min <=(1*24) | clipper_min >(180*24)  
tabulate year, gen(dummy_Y) 

cgmreg voj_duration_hours clipper_min ,cluster(VoyageFrom VoyageTo year)
est sto a1
cgmreg voj_duration_hours clipper_min dummy_Y* ,cluster(VoyageFrom VoyageTo year)
est sto a2
reg voj_duration_hours clipper_min dummy_Y* 
avplot clipper_min, xtitle("Duration optimized voyage") ytitle("Duration voyage in CLIWOC")
graph save FigureA3, replace
graph export FigureA3.tif, as(tif) width(1500) replace

cgmreg voj_duration_hours clipper_min  ,cluster(VoyageFrom VoyageTo year)
est sto a3
cgmreg voj_duration_hours clipper_min  dummy_Y* ,cluster(VoyageFrom VoyageTo year)
est sto a4
cgmreg voj_duration_hours clipper_min geo_distance1 ,cluster(VoyageFrom VoyageTo year)
est sto a3
cgmreg voj_duration_hours clipper_min geo_distance1 dummy_Y* ,cluster(VoyageFrom VoyageTo year)
est sto a4

esta a* using Table2.tex, replace se title(TABLE 2) drop(dummy_Y*) s(r2 N)  nostar

*****************************************************************************
******************TABLE 3. GRAVITY REGRESSIONS  
*****************************************************************************

***columns 1-2 (GRAVITY MODEL, IMPORTER, EXPORTER and YEAR FIXED EFFECTS)
use "PUBLIC_DATA/BILATERAL_TRADE_PUBLIC2.dta", clear

cgmreg  lexpr dd6_*m dd6_*l2 dummy_Y* dummy_ORI* dummy_DES* ,cluster(country_o country_d year)
est store ff1

cgmreg  lexpr dd6_*m dd6_*l2 ldist dummy_Y* dummy_ORI* dummy_DES* ,cluster(country_o country_d year)
est store ff2

esta ff* using Table3.tex, keep(dd* ldist) se  stat(r2 N) title(TABLE 3, columns 1-2 COUNTRY AND YEAR FIXED EFFECTS)  replace nostar
est clear


*** columns 3-4 (GRAVITY MODEL, IMPORTER_X_YEAR and EXPORTER_X_YEAR FIXED EFFECTS)
use "PUBLIC_DATA/BILATERAL_TRADE_PUBLIC2.dta", clear
tostring year, gen(year1)
drop  dummy_O* dummy_D* dummy_Y*
gen des_year= country_d+year1
gen ori_year= country_o+year1
encode des_year, gen(des_year1)
encode ori_year, gen(ori_year1)
encode pair, gen(pair1)
encode country_o, gen(country_o1)

felsdvreg lexpr ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_lexpr) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons

felsdvreg ldist ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_ldist) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons

felsdvreg dd6_60lsteam ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b60lsteam) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b65lsteam ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b65lsteam) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b70lsteam ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b70lsteam) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b75lsteam ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b75lsteam) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b80lsteam ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b80lsteam) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b85lsteam ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b85lsteam) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b90lsteam ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b90lsteam) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b95lsteam ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b95lsteam) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b00lsteam ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b00lsteam) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons

felsdvreg dd6_60lsail2 ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b60lsail) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b65lsail2 ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b65lsail) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b70lsail2 ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b70lsail) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b75lsail2 ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b75lsail) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b80lsail2 ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b80lsail) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b85lsail2 ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b85lsail) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b90lsail2 ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b90lsail) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b95lsail2 ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b95lsail) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons
felsdvreg dd6_b00lsail2 ,ivar(des_year) jvar(ori_year) xb(xb) peff(phat) feff(fhat) res(_dd6_b00lsail) mover(mov1) mnum(mnum1) pobs(pobs1) group(group1) cons

save "TEMP/COUNTRY_X_YEAR_FE.dta", replace
use "TEMP/COUNTRY_X_YEAR_FE.dta", clear

cgmreg _lexpr _dd6_*m _dd6_*l ,cluster(country_o country_d year)
est store bbaa1
cgmreg _lexpr _dd6_*m _dd6_*l _ldist ,cluster(country_o country_d year)
est store bbaa2

esta bbaa* using Table3.tex, keep(_dd* _ldist)  stat(r2 N)  se  title(TABLE 3, columns 3-4 COUNTRYXYEAR FIXED EFFECTS)   append  nostar
est clear


***column 5 (GRAVITY MODEL, PAIR FIXED EFFECTS)
use "PUBLIC_DATA/BILATERAL_TRADE_PUBLIC2.dta", clear
tabulate pair, gen(int_DUM)

cgmreg  lexpr dd6_b*m dd6_b*l2 dummy_Y* int_DUM* ,cluster(country_o country_d year)
est store fff1

esta fff1 using Table3.tex, keep(dd* ) se  stat(r2 N) title(TABLE 3, column 5 PAIR AND YEAR FIXED EFFECTS)   append nostar
est clear


*****************************************************************************
****************** TABLE A.4. GRAVITY REGRESSIONS: SHORT VS LONG DISTANCES 
*****************************************************************************
use "PUBLIC_DATA/BILATERAL_TRADE_PUBLIC2.dta",clear
tostring year, gen(year1)
local CLUSTERING "country_o country_d year1"
tabulate pair, gen(int_DUM)
gen  lsteam_b=log(TIME_4_2) 
summ lsteam_b
scalar mean_lsteam=r(mean)
summ ldist
scalar mean_lgeo_dist=r(mean)

cgmreg lexpr  dd6_b*m dd6_b*l2 dummy_Y* int_DUM*    			if  lsteam_b>=mean_lsteam ,cluster(`CLUSTERING')
est sto a1
cgmreg lexpr  dd6_b*m dd6_b*l2 dummy_Y* int_DUM*    			if  lsteam_b<mean_lsteam ,cluster(`CLUSTERING')
est sto a2
cgmreg lexpr  dd6_b*m dd6_b*l2 dummy_Y* int_DUM*    			if  ldist>=mean_lgeo_dist ,cluster(`CLUSTERING')
est sto a3
cgmreg lexpr  dd6_b*m dd6_b*l2 dummy_Y* int_DUM*    			if  ldist<mean_lgeo_dist ,cluster(`CLUSTERING')
est sto a4

drop lsteam_b 

esta a1 a2 a3 a4 using TableA4.tex, keep(dd6_b*m dd6_b*l2) se order(dd6_b*m dd6_b*l2) stat(r2 N) title(TABLE A.4 The shift from sail to steam: short versus long distances) replace nostar
est clear

*****************************************************************************
****************** TABLE 4. GRAVITY REGRESSIONS WITH FREIGHT RATES 
*****************************************************************************

use "PUBLIC_DATA/FREIGHT_RATES_PUBLIC2.dta", clear

ivreg2 lfreight dd5_60_lsteam dd5_70_lsteam dd5_80_lsteam dd5_90_lsteam dd5_00_lsteam  dd5_60_lsail2 dd5_70_lsail2 dd5_80_lsail2 dd5_90_lsail2 dd5_00_lsail2 dummy_Y* dummy_country*	dummy_PROD*							, cluster(destination_country year) partial(dummy_Y* dummy_country* dummy_PROD*)
est sto a1
ivreg2 lfreight dd5_60_lsteam dd5_70_lsteam dd5_80_lsteam dd5_90_lsteam dd5_00_lsteam  dd5_60_lsail2 dd5_70_lsail2 dd5_80_lsail2 dd5_90_lsail2 dd5_00_lsail2 ldist dummy_Y* dummy_country*	dummy_PROD*						, cluster(destination_country year) partial(dummy_Y* dummy_country* dummy_PROD*)
est sto a2
ivreg2 lfreight dd5_70_lsteam dd5_80_lsteam dd5_90_lsteam dd5_00_lsteam            dd5_70_lsail2 dd5_80_lsail2 dd5_90_lsail2 dd5_00_lsail2 dummy_Y* int_DUM* dummy_PROD*													, cluster(destination_country year) partial(dummy_Y* int_DUM* dummy_PROD*)
est sto a3

esta a* using Table4.tex, replace se  nostar stat(r2 N) title(TABLE 4, The shift from sail to steam: shipping times and freight rates)
est clear

