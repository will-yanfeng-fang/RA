//Data cleaning for life satisfaction

clear all

cd "/Users/fangxiaoling/Documents/Nathan Nunn RA/beyond GDP"
use "WVS_Time_Series_1981-2022_stata_v5_0.dta"

keep S020 COW_NUM A170

rename S020 year
rename COW_NUM country
rename A170 life_sat

decode country, gen(country_str)
drop country
rename country_str country

collapse (mean) life_sat, by(country year)

save "Life_Satisfaction.dta", replace

use "life_sa_81_21.dta", clear

keep S020 COW_NUM A170

rename S020 year
rename COW_NUM country
rename A170 life_sat

decode country, gen(country_str)
drop country
rename country_str country

collapse (mean) life_sat, by(country year)

save "Life_Satisfaction_2.dta", replace

use "Life_Satisfaction.dta", clear
append using "Life_Satisfaction_2.dta"

save "Life_Satisfaction_3.dta", replace

//Combine EF and biocap

import excel "EF_Biocap.xlsx", sheet("Sheet1") firstrow clear

rename country_name country
tostring country, replace

save "EF_Biocap.dta", replace

merge 1:1 country year using "Life_Satisfaction_3.dta" //I need more life satisfaction data from Gallup

save "combined.dta", replace

//Combine GPIpc



