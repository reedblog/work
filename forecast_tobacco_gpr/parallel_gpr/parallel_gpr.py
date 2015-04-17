# script to parallelize gpr
# load in, subset data, run gpr, output
# given sex, age, country
import sys
import pandas as pd
import os
sys.path.append('/homes/rsoren/prog/work_scripts/forecast_tobacco_gpr/')
import gpr.gpr as gpr

if os.name == 'nt':
    hpath = 'H:/'
else:
    hpath = '/homes/rsoren/'

output_name = "logit_rescaled_l2_o05_z99_1333_bothsexes"

loc = sys.argv[1]
sex = sys.argv[2]
age = int(sys.argv[3])

# just for debugging
# loc = "USA"
# sex = "male"
# age = 20

idx = pd.IndexSlice
if not os.path.exists('/homes/rsoren/prog/data/forecast_tobacco_gpr/{model:s}/parallel/'.format(model=output_name)):
    os.popen('mkdir /homes/rsoren/prog/data/forecast_tobacco_gpr/{model:s}/parallel/'.format(model=output_name))

num_draws = 1000

results = pd.read_csv('{hpath:s}prog/data/forecast_tobacco_gpr/{model:s}/spacetime_output_prepped.csv'.format(
    hpath=hpath, model=output_name))

results = results.set_index(['iso3', 'sex', 'age', 'year'])
results = results.sortlevel()
iso_sex_age_results = results.loc[idx[loc, sex, age,:], :].copy()
iso_sex_age_results = iso_sex_age_results.reset_index()
# Using the global estimator for the gpr amplitude... could use the regional/national MAD
amp = iso_sex_age_results.spacetime_amplitude_1.values[0] * 1.4826 # what is 1.4826?? --PR
gpr_out = gpr.fit_gpr(iso_sex_age_results,amp=amp, obs_variable='observed_data', obs_var_variable='obs_data_variance', draws=num_draws)
gpr_out.to_csv('/homes/rsoren/prog/data/forecast_tobacco_gpr/{model:s}/parallel/_parallel_{loc:s}_{sex:s}_{age:d}.csv'.format(model=output_name, loc=loc, sex=sex, age=age))
