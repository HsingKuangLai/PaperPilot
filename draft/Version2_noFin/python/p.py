import pandas as pd
import numpy as np
from scipy.stats.mstats import winsorize
import statsmodels.api as sm
import statsmodels.formula.api as smf

# Read CSV file
data = pd.read_csv("Total_NICtd.csv", na_values="#N/A")

# Preprocessing
data['ABSDA'] = data['DA'].abs()
data['ABSDA1'] = data['DA1'].abs()
data['ABSDA2'] = data['DA2'].abs()
data['ABCFO'] = data['ABCFO'] * -1
data['ABEXP'] = data['ABEXP'] * -1
data['RM'] = data['ABCFO'] + data['ABEXP'] + data['ABPROD']
data['RM1'] = data['ABCFO'] + data['ABEXP']
data['RM2'] = data['ABEXP'] + data['ABPROD']
data['MVE'] = data['Equity'] / data['Asset_1']
data['ABSRM'] = data['RM'].abs()
data['ESG'] = np.log(data['ESG'])
data['Suspect'] = np.where((data['ROA'] < 0.005) & (data['ROA'] > 0), 1, 0)

# Winsorizing
def custom_winsorize(series):
    return winsorize(series, limits=[0.01, 0.01])

data = data[data['Adopt'] == 1]

# Factorize the first 9 columns
for col in data.columns[:9]:
    data[col] = data[col].astype('category')

# Apply winsorize
data.iloc[:, 9:] = data.iloc[:, 9:].apply(custom_winsorize)

data['ADJROA_sq'] = data['ADJROA']**2
data['ROA_sq'] = data['ROA']**2
data['Age_sq'] = data['Age']**2
data['Year'] = data['Year'].astype('float') + 2016

# Define control variables
control_vars = ["RPA_Ctd", "NOA", "Suspect", "INST", "MS", "LEV", "OCF", "MTB", "SG", "ADJROA", "ADJROA_sq", "ADV", "LGTA", "Big4"]

# AM and RM models
AM_proxy = "ABSDA1"
RM_proxy = "RM2"

# AM model with control variables and AM proxy
formula_AM_HAT = f"{AM_proxy} ~ {' + '.join(control_vars)}"
modelAM_HAT = smf.ols(formula=formula_AM_HAT, data=data).fit()
data['AM.hat'] = modelAM_HAT.fittedvalues

# RM model with AM.hat and control variables (including Year)
control_vars_with_year = control_vars + ["Year"]
formula_RM = f"{RM_proxy} ~ AM.hat + {' + '.join(control_vars_with_year)}"
modelRM = smf.ols(formula=formula_RM, data=data).fit()

print(modelRM.summary())
print(sm.stats.sandwich_covariance.cov_hc0(modelRM))

# RM model without AM.hat to get RM.hat
formula_RM_HAT = f"{RM_proxy} ~ {' + '.join(control_vars)}"
modelRM_HAT = smf.ols(formula=formula_RM_HAT, data=data).fit()
data['RM.hat'] = modelRM_HAT.fittedvalues

# AM model with RM.hat, control variables, and AM proxy
formula_AM = f"{AM_proxy} ~ RM.hat + {' + '.join(control_vars_with_year)}"
modelAM = smf.ols(formula=formula_AM, data=data).fit()

print(modelAM.summary())
print(sm.stats.sandwich_cov)
