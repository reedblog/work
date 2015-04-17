import numpy as np
import pandas 
import os

thispath = os.path.dirname(__file__)

def gbd_spacemap(locs_pred, locs_obs):
    
    gbd_regions = pandas.read_csv(thispath+"/regions.csv")
    
    locs_pred = pandas.DataFrame(locs_pred,columns=['iso3'])
    locs_obs = pandas.DataFrame(locs_obs,columns=['iso3'])
    
    locs_pred = pandas.merge(locs_pred,gbd_regions)
    locs_obs = pandas.merge(locs_obs,gbd_regions)
    
    in_country = (np.asmatrix(locs_pred['location_id']).T==np.asmatrix(locs_obs['location_id'])).astype(int)
    in_region = (np.asmatrix(locs_pred['gbd_analytical_region_id']).T==np.asmatrix(locs_obs['gbd_analytical_region_id'])).astype(int)
    in_sr = (np.asmatrix(locs_pred['gbd_analytical_superregion_id']).T==np.asmatrix(locs_obs['gbd_analytical_superregion_id'])).astype(int)

    spatial_relatedness = in_country + in_region + in_sr
    
    sp_rel_lookup = pandas.DataFrame(spatial_relatedness,columns=locs_obs['iso3'],index=locs_pred['iso3'])

    return spatial_relatedness