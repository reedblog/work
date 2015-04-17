import os
import sys

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
sys.path.append(hpath + "prog/work_scripts/forecast_tobacco_gpr/")
sys.path.append(hpath + 'prog/repos/')

import matplotlib.pyplot as plt
import gpr.gpr
import pandas as pd
import numpy as np
reload(gpr)

np.seterr(invalid='warn')
np.seterr(divide='warn')

results = pd.read_csv('{hpath:s}prog/data/forecast_tobacco_gpr/{model:s}/spacetime_output_prepped.csv'.format(
    hpath=hpath, model=output_name))


i = 0
jids = []
for sex in results.sex.unique():
    for age in results.age.unique():
        for loc in results.iso3.unique():
            os.system('qsub -N tobacco_{loc:s}_{sex:s}_{age:d} -pe multi_slot 9 /homes/rsoren/prog/repos/fbd/grid_engine/python_shell.sh /homes/rsoren/prog/work_scripts/forecast_tobacco_gpr/parallel_gpr/parallel_gpr.py {loc:s} {sex:s} {age:d}'.format(loc=loc, sex=sex, age=age))
            jids.append('tobacco_{loc:s}_{sex:s}_{age:d}'.format(loc = loc, sex = sex, age = age))
if os.path.exists('/homes/rsoren/prog/data/forecast_tobacco_gpr/{model:s}/parallel/finished.txt'.format(model=output_name)):
    # remove finished file if it already exists
    os.system('rm /homes/rsoren/prog/data/forecast_tobacco/gpr/{model:s}/parallel/finished.txt'.format(model = output_name))

os.system('qsub -N final_gpr -hold_jid {jids:s} /homes/rsoren/prog/repos/fbd/grid_engine/python_shell.sh /homes/rsoren/prog/work_scripts/forecast_tobacco_gpr/parallel_gpr/p_finish.py {path:s}'.format(jids = ','.join(jids), path = '/homes/rsoren/prog/data/forecast_tobacco_gpr/{model:s}/parallel/'.format(model = output_name)))

import time
# wait for last job to run (which is waiting for all jobs for a risk to run and just writes a txt file)
while not os.path.exists('/homes/rsoren/prog/data/forecast_tobacco_gpr/{model:s}/parallel/finished.txt'.format(model=output_name)):
    time.sleep(5) #wait 5 seconds
os.system('rm /homes/rsoren/prog/data/forecast_tobacco_gpr/{model:s}/parallel/finished.txt'.format(model=output_name))
# import gpr.gpr_append
# gpr.gpr_append.aggregate(risk, '/clustertmp/preidy/sev/{risk:s}'.format(risk = risk))

