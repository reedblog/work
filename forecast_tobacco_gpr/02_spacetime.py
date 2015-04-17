import os

if os.name == 'nt':
    hpath = 'H:/'
else:
    hpath = '/homes/rsoren/'


# set parameters
output_name = "logit_rescaled_l2_o05_z99_1333_bothsexes"
out_of_sample = False
sex_subset = "female"


if out_of_sample == True:
    oos = "_outofsample_test"
else:
    oos = ""

# os.chdir('/home/j/WORK/01_covariates/02_inputs/education/update_2015/code/model')
os.chdir(hpath + 'prog/work_scripts/forecast_tobacco_gpr')

import sys
sys.path.append(hpath + 'prog/work_scripts/forecast_tobacco_gpr')
import spacetime.spacetime as st
import pandas as pd
import matplotlib.pyplot as plt
reload(st)



# Read in some data...
#####################################################################################
#male
#data = pd.read_csv('/home/j/WORK/01_covariates/02_inputs/education/update_2015/data/output_data/linear_output.csv')
#data = pd.read_csv(hpath + 'prog/data/forecast_tobacco_gpr/logit_rescaled_1497442/linear_output.csv')
data = pd.read_csv(hpath + 'prog/data/forecast_tobacco_gpr/' + output_name + '/linear_output' + oos + '.csv')
data = data.ix[data.sex == sex_subset]

################################
## All country example
################################

# Initialize the smoother
s = st.Smoother(data)

# Set parameters (can additionally specify omega (age weight, positive real number) and zeta (space weight, between 0 and 1))
s.lambdaa = 2.0
s.omega = 0.5
s.zeta = .99

# Tell the smoother to calculate both time weights and age weights
s.time_weights()
s.age_weights()

# Run the smoother and write the results to a file
s.smooth()
results = s.format_output(include_mad=True)
print "done"

# results.to_csv('/home/j/WORK/01_covariates/02_inputs/education/update_2015/data/output_data/spacetime_output_male.csv', index=False)
results.to_csv(hpath + 'prog/data/forecast_tobacco_gpr/'
               + output_name + '/spacetime_output' + oos + '_' + sex_subset + '.csv', index=False)
