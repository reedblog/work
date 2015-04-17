#parallel gpr post_job
#get all the individual files and append them

import pandas as pd
import pdb
import os
def aggregate(risk, user_direct):
    st_results = pd.DataFrame.from_csv('/clustertmp/preidy/sev/{risk:s}/sev_prep.csv'.format(risk = risk))

    #initial dataframe:
    first_age = st_results.age_group_id.min()
    first_sex = st_results.sex.min()
    first_loc = st_results.location_id.min()

    # data science
    os.system('cat {dir:s}/_parallel_{loc:d}_{sex:d}_{age:d}.csv > {dir:s}/gpr_results_{risk:s}.csv'.format(loc = first_loc, sex = first_sex, age = first_age, risk = risk, dir = user_direct))

    for loc in st_results.location_id.unique():
        for sex in st_results.sex.unique():
            for age in st_results.age_group_id.unique():
                if not(loc == first_loc and sex == first_sex and age == first_age):
                    os.system('cat {dir:s}/_parallel_{loc:d}_{sex:d}_{age:d}.csv | tail -n +2 >> {dir:s}/gpr_results_{risk:s}.csv'.format(loc = loc, sex =sex, age = age, risk = risk, dir = user_direct))

        print loc
    os.system('rm {dir:s}/_parallel*.csv'.format(dir = user_direct))
