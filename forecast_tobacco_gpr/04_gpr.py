import os

if os.name == 'nt':
    hpath = 'H:/'
else:
    hpath = '/homes/rsoren/'

output_name = "logit_rescaled_l2_o05_z99_1333_bothsexes"
out_of_sample = False

if out_of_sample == True:
    oos = "_outofsample_test"
else:
    oos = ""

# os.chdir("/home/j/WORK/01_covariates/02_inputs/education/update_2015/code/model/")
os.chdir(hpath + "prog/work_scripts/forecast_tobacco_gpr/")

import matplotlib.pyplot as plt
import sys
import gpr.gpr as gpr
import pandas as pd
reload(gpr)


#results = pd.read_csv('/home/j/WORK/01_covariates/02_inputs/education/update_2015/data/output_data/spacetime_output_prepped.csv')
results = pd.read_csv(hpath + 'prog/data/forecast_tobacco_gpr/' +
                      output_name + '/spacetime_output' + oos + '_prepped.csv')


gpr_results = pd.DataFrame()
for iso in pd.unique(results['iso3']):
	for sexy in pd.unique(results['sex']):
		for age in pd.unique(results['age']): 
			# Run one country-year-age-sex group at a time
			iso_results = results.ix[results.iso3==iso]
			iso_sex_results = iso_results.ix[iso_results.sex==sexy]
			iso_sex_age_results = iso_sex_results.ix[iso_sex_results.age==age]
			# Using the global estimator for the gpr amplitude... could use the regional/national MAD
			amp = iso_sex_age_results.spacetime_amplitude_1.values[0] * 1.4826 
			gpr_out = gpr.fit_gpr(iso_sex_age_results,amp=amp)
			# Append the results for this cyas run of gpr to all results
			gpr_results = gpr_results.append(gpr_out)

    
# Reorder columns and write to csv
column_order = ['iso3','year', 'observed_data','obs_data_variance','st_prediction','gpr_mean','gpr_lower','gpr_upper']
column_order.extend(list(set(gpr_results.columns) - set(column_order)))
# gpr_results[column_order].to_csv('/home/j/WORK/01_covariates/02_inputs/education/update_2015/data/output_data/gpr_output.csv', index=False)
gpr_results[column_order].to_csv(hpath + 'prog/data/forecast_tobacco_gpr/' + output_name + '/gpr_output' + oos + '.csv', index=False)
