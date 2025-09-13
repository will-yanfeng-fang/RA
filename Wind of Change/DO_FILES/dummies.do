* ===============================
* 1) Load dataset
* ===============================
cd "/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change"
use "TEMP/TEMP101.dta", clear
// use "results/predicted_effects_table9.dta", clear

* ===============================
* 2) Define mutually exclusive groups
* ===============================
gen atlantic_trader   = inlist(country,"United Kingdom","Britain","France","Spain","Portugal","Netherlands")
gen colonial_offshoot = inlist(country,"New Zealand","Australia","Canada","United States","Cape of Good Hope")

gen europe_nonAT = 0
replace europe_nonAT = 1 if inlist(country,"Germany","Italy","Austria-Hungary","Finland","Sweden")
replace europe_nonAT = 1 if inlist(country,"Denmark","Norway","Belgium","Romania","Greece")

* ===============================
* 3) Baseline trade + interaction terms
* ===============================
* Baseline trade variable (applies to everyone)
gen trade_base = ln_exportGDP

* Group interactions
gen trade_AT       = ln_exportGDP * atlantic_trader
gen trade_offshoot = ln_exportGDP * colonial_offshoot
gen trade_europe   = ln_exportGDP * europe_nonAT

* Instruments
gen instr_base     = lpred_TOTAL_trad_5ys
gen instr_AT       = lpred_TOTAL_trad_5ys * atlantic_trader
gen instr_offshoot = lpred_TOTAL_trad_5ys * colonial_offshoot
gen instr_europe   = lpred_TOTAL_trad_5ys * europe_nonAT

* ===============================
* 4) IV regression (all usable obs)
* ===============================
eststo clear
ivreg2 ln_GDPpc (trade_base trade_AT trade_offshoot trade_europe = instr_base instr_AT instr_offshoot instr_europe) ///
       dummy_C* dummy_Y* [aweight=lpopulation_1860], ///
       cluster(country year) partial(dummy_C* dummy_Y*) noc
di "Extended sample N (all usable obs): " e(N)
eststo group_trade_effects

* ===============================
* 5) Export regression table
* ===============================
esttab group_trade_effects using Table_AT_Offshoot.tex, replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    keep(trade_base trade_AT trade_offshoot trade_europe) ///
    order(trade_base trade_AT trade_europe trade_offshoot) ///
    coeflabels(trade_base "Trade (baseline)" ///
               trade_AT "Trade × Atlantic" ///
               trade_europe "Trade × Europe non-AT" ///
               trade_offshoot "Trade × Offshoot") ///
    title("Trade Effects by Country Group (with baseline)")
