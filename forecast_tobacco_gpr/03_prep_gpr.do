

	if c(os) == "Unix" {
		local prefix "/home/j"
		set more off
		set odbcmgr unixodbc
	}
	else if c(os) == "Windows" {
		local prefix "J:"
	}

	local root = "`prefix'/WORK/01_covariates/02_inputs/education/update_2015"
	local root2 = "`prefix'/temp/reed/prog/forecasting/smoking_prev_GPR"
	
	adopath + "`prefix'/WORK/01_covariates/common/lib"


import delimited using "`root2'/data/output_data/spacetime_output_male.csv", clear varnames(1)
tempfile male
save `male', replace
import delimited using "`root2'/data/output_data/spacetime_output_female.csv", clear varnames(1)
append using `male'



rename mad_regional spacetime_amplitude_1
sort iso3 year

** export delimited using "`root'/data/output_data/spacetime_output_prepped.csv", replace
export delimited using "`root2'/data/output_data/spacetime_output_prepped.csv", replace

di "done!"






