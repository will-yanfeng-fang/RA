# =======================
# Table 8: Trade & Divergence
# =======================

import pandas as pd
import numpy as np
import statsmodels.api as sm
from linearmodels.iv import IV2SLS

REPLICATIONroot = "/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change"
os.chdir(REPLICATIONroot)

# ---- Load merged country-year dataset ----
df = pd.read_stata("TEMP100.dta")   # after you merged predicted trade etc.

# ---- Step 1: GDP per capita in 1850 ----
df["GDPpc_1850"] = df.loc[df["year"] == 1850, ["country","GDPpc"]].groupby("country")["GDPpc"].transform("min")
df["lGDPpc_1850"] = np.log(df["GDPpc_1850"])

# ---- Step 2: percentiles & cutoffs ----
qtiles = df.loc[df["year"]==1850, "lGDPpc_1850"].quantile(np.arange(0,1.01,0.01))
df["perc_lGDPpc_1850"] = pd.cut(df["lGDPpc_1850"], bins=qtiles, labels=False, include_lowest=True)

mean_gdp_1850 = df.loc[df["year"]==1850, "lGDPpc_1850"].mean()
df["above_mean_gdp_1850"] = np.where(df["lGDPpc_1850"] >= mean_gdp_1850, 1, 0)
df["above_75_gdp_1850"]   = np.where(df["perc_lGDPpc_1850"] > 75, 1, 0)

# ---- Step 3: interaction instruments ----
df["instr_by_above_mean_gdp_1850"] = df["lpred_TOTAL_trad_5ys"] * df["above_mean_gdp_1850"]
df["trade_by_above_mean_gdp_1850"] = df["ln_exportGDP"] * df["above_mean_gdp_1850"]
df["trade2_by_above_mean_gdp_1850"] = df["ln_exportpc"] * df["above_mean_gdp_1850"]

df["instr_by_above_p75_gdp_1850"] = df["lpred_TOTAL_trad_5ys"] * df["above_75_gdp_1850"]
df["trade_by_above_p75_gdp_1850"] = df["ln_exportGDP"] * df["above_75_gdp_1850"]
df["trade2_by_above_p75_gdp_1850"] = df["ln_exportpc"] * df["above_75_gdp_1850"]

# ---- Step 4: helper for IV regressions ----
def run_iv(dep, endog, exog, instr, df, cluster_cols):
    y = df[dep]
    X = sm.add_constant(df[exog])  # exogenous controls (constant, dummies later)
    endog_vars = df[endog]
    instr_vars = sm.add_constant(df[instr])
    model = IV2SLS(y, X, endog_vars, instr_vars).fit(cov_type="clustered", clusters=df[cluster_cols])
    return model

# ---- Step 5: Run 8 models ----
# Note: in Stata you also had [aweight=lpopulation_1860], here ignored unless you want WLS
results = {}

results["b1"] = run_iv("ln_GDPpc",
                       ["ln_exportGDP","trade_by_above_mean_gdp_1850"],
                       [],  # exog constant only
                       ["lpred_TOTAL_trad_5ys","instr_by_above_mean_gdp_1850"],
                       df, cluster_cols="country")

results["b2"] = run_iv("ln_GDPpc",
                       ["ln_exportGDP","trade_by_above_mean_gdp_1850"],
                       [],  
                       ["lpred_TOTAL_trad_5ys","instr_by_above_mean_gdp_1850"],
                       df, cluster_cols="country")

results["b3"] = run_iv("ln_GDPpc",
                       ["ln_exportGDP","trade_by_above_p75_gdp_1850"],
                       [],
                       ["lpred_TOTAL_trad_5ys","instr_by_above_p75_gdp_1850"],
                       df, cluster_cols="country")

results["b4"] = run_iv("ln_GDPpc",
                       ["ln_exportGDP","trade_by_above_p75_gdp_1850"],
                       [],
                       ["lpred_TOTAL_trad_5ys","instr_by_above_p75_gdp_1850"],
                       df, cluster_cols="country")

results["b5"] = run_iv("ln_GDPpc",
                       ["ln_exportpc","trade2_by_above_mean_gdp_1850"],
                       [],
                       ["lpred_TOTAL_trad_5ys","instr_by_above_mean_gdp_1850"],
                       df[df["ln_exportGDP"].notna()], cluster_cols="country")

results["b6"] = run_iv("ln_GDPpc",
                       ["ln_exportpc","trade2_by_above_mean_gdp_1850"],
                       [],
                       ["lpred_TOTAL_trad_5ys","instr_by_above_mean_gdp_1850"],
                       df[df["ln_exportGDP"].notna()], cluster_cols="country")

results["b7"] = run_iv("ln_GDPpc",
                       ["ln_exportpc","trade2_by_above_p75_gdp_1850"],
                       [],
                       ["lpred_TOTAL_trad_5ys","instr_by_above_p75_gdp_1850"],
                       df[df["ln_exportGDP"].notna()], cluster_cols="country")

results["b8"] = run_iv("ln_GDPpc",
                       ["ln_exportpc","trade2_by_above_p75_gdp_1850"],
                       [],
                       ["lpred_TOTAL_trad_5ys","instr_by_above_p75_gdp_1850"],
                       df[df["ln_exportGDP"].notna()], cluster_cols="country")

# ---- Step 6: Collect results ----
table8 = []
for name, res in results.items():
    table8.append(pd.DataFrame({
        "model": name,
        "coef": res.params,
        "se": res.std_errors,
        "pval": res.pvalues
    }))
table8 = pd.concat(table8)

# Save to CSV (instead of LaTeX)
table8.to_csv("Table8_results.csv", index=False)
