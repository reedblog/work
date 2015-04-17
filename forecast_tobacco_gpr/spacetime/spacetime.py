import numpy as np
import scipy as sp
from scipy import stats
import scipy.spatial
import pandas as pd
import os

thispath = os.path.dirname(__file__)

# Utility functions
def invlogit(x):
    return np.exp(x)/(np.exp(x)+1)

def logit(x):
    return np.log(x / (1-x))
    
def invlogit_var(mu,var):
    return (var * (np.exp(mu)/(np.exp(mu)+1)**2)**2)
    
def logit_var(mu,var):
    return (var / (mu*(1-mu))**2)
    
def mad(x):
    return stats.nanmedian(np.abs(x - stats.nanmedian(x)))

class Smoother: 
        
    def __init__(self, dataset, timevar='year', agevar='age', spacevar='iso3', datavar='observed_data', modelvar='stage1_prediction', snvar=None):
        
        self.gbd_ages = [0, 0.01, 0.1, 1]
        self.gbd_ages.extend(range(5,85,5))
        
        self.age_map = { value:idx for idx,value in enumerate(self.gbd_ages) }

        # No default age and time weights, must be set explicitly
        self.a_weights = None
        self.t_weights = None
        
        # Assume an input data set with default variable names
        self.timevar = timevar
        self.agevar = agevar
        self.spacevar = spacevar     
        self.datavar = datavar
        self.modelvar = modelvar
        self.snvar = snvar
        
        # Bind the data and stage 1 models, reogranizing for smoothing
        self.inputset = dataset 
        self.data = dataset.ix[pd.notnull(dataset[datavar])]
        self.stage1 = dataset.ix[pd.notnull(dataset[modelvar])].drop_duplicates()
        self.data['resid'] = self.data[self.datavar] - self.data[self.modelvar]
        self.stage1 = self.stage1.sort([self.spacevar, self.agevar, self.timevar])
        
        # Default years / ages to predict
        self.p_startyear = np.min(self.stage1.year)
        self.p_endyear = np.max(self.stage1.year)
        
        self.p_ages = sorted(pd.unique(self.stage1[self.agevar]))
        self.results = pd.DataFrame(data={
            self.timevar: np.tile(range(self.p_startyear,self.p_endyear+1),len(self.p_ages)).T,
            self.agevar:  np.repeat(self.p_ages,self.p_endyear-self.p_startyear+1,axis=0)})
        
        # Set default smoothing parameters
        self.lambdaa=1.0
        self.omega=2
        self.zeta=0.9
        self.sn_weight=0.2
                
    # Generate age weights
    def time_weights(self):
        p_years = np.asanyarray([[i for i in range(self.p_startyear,self.p_endyear+1)]], dtype=float)
        o_years = np.asanyarray([self.data[self.timevar].values])
        
        # Pre-compute weights for each time-distance
        t_weights_lookup = {}
        for i in range(self.p_startyear,self.p_endyear+1):
            t_weights_lookup[i] = {}
            for j in range(self.p_startyear,self.p_endyear+1):
                t_weights_lookup[i][j] = (1 - (abs(float(i-j)) / (max(abs(i-self.p_startyear),abs(i-self.p_endyear))+1))**self.lambdaa)**3
            
        t_weights= sp.spatial.distance.cdist(p_years.T, o_years.T, lambda u, v: t_weights_lookup[u[0]][v[0]])
        self.t_weights = t_weights
        
        return t_weights
        
    # Generate age weights
    def age_weights(self):
        p_ages = np.asanyarray([ sorted([self.age_map[k] for k in self.p_ages]) ])
        o_ages = np.asanyarray([[ self.age_map[i] for i in self.data[self.agevar] ]])
        
        # Pre-compute weights for each age-distance
        a_weights_lookup = {}
        for i in range(0,np.int(np.max(self.age_map.values())-np.min(self.age_map.values())+2)):
            a_weights_lookup[i] = 1 / np.exp(self.omega*i)
            a_weights_lookup[-i] = a_weights_lookup[i]
        
        a_weights = sp.spatial.distance.cdist(p_ages.T, o_ages.T, lambda u, v: a_weights_lookup[u[0]-v[0]])
        self.a_weights = a_weights
        
        return a_weights
    
    # This will generate a spatial-relatedness matrix based on the GBD region/super-regions.  
    # Arbitrary spatial-relatedness matrices will be accepted by the space-weighting function, but they must be of this form.
    def gbd_spacemap(self,locs):
        
        gbd_regions = pd.read_csv(thispath+"/regions.csv")
        
        p_locs = pd.DataFrame([locs],columns=['iso3'])
        o_locs = self.data[[self.spacevar]]
        
        # Make sure the data doesn't get misaligned, since the matrix lookups are not order invariant
        p_locs = pd.merge(p_locs,gbd_regions,left_on=self.spacevar,right_on='iso3')
        o_locs['data_order'] = o_locs.reset_index().index
        o_locs = pd.merge(o_locs,gbd_regions,left_on=self.spacevar,right_on='iso3').sort('data_order').reset_index()
        
        in_country = (np.asmatrix(p_locs['location_id']).T==np.asmatrix(o_locs['location_id'])).astype(int)
        in_region = (np.asmatrix(p_locs['gbd_analytical_region_id']).T==np.asmatrix(o_locs['gbd_analytical_region_id'])).astype(int)
        in_sr = (np.asmatrix(p_locs['gbd_analytical_superregion_id']).T==np.asmatrix(o_locs['gbd_analytical_superregion_id'])).astype(int)
    
        spacemap = in_country + in_region + in_sr
        
        return np.array(spacemap)
        
    # Generate spaceweights
    def space_weights(self, loc):
        
        # Generate spatial weights based on the GBD regions/superregions
        spacemap = self.gbd_spacemap(loc)
        
        if (self.t_weights!=None and self.a_weights!=None):
            # Align time weights with a prediction space that is sorted by age group, then year
            t_weights = np.tile(self.t_weights.T,len(self.p_ages)).T
            
            # Align age weights with a prediction space that is sorted by age group, then year
            a_weights = np.repeat(self.a_weights,self.p_endyear-self.p_startyear+1,axis=0)
        
            weights = t_weights * a_weights
            
        elif self.t_weights!=None:
            t_weights = np.tile(self.t_weights.T,len(self.p_ages)).T
            weights = t_weights

        elif self.a_weights!=None:
            a_weights = np.repeat(self.a_weights,self.p_endyear-self.p_startyear+1,axis=0)
            weights = a_weights

        else:
            return None
        
        sp_weights = {  3: self.zeta,
                        2: self.zeta*(1-self.zeta),
                        1: (1-self.zeta)**2,
                        0: 0 }
            
        # Downweight subnational data points if requested
        if self.snvar!=None:
            nat_map = [0 if val==1 else 1 for val in self.dataset[self.snvar]]
            sp_grp_mask = (spacemap==3).astype(int)
            all_sum = np.sum(weights*sp_grp_mask)
            subnat_sum = np.sum(weights*sp_grp_mask*[self.dataset[self.subnatvar]])
            
            # If there is no national data, treat subnational as national
            if subnat_sum > 0 and subnat_sum!=all_sum:
                # Isolate the subnational weights
                subnational_weights = all_sum*(self.sn_weight) / ((1-self.sn_weight)*subnat_sum) * (weights*sp_grp_mask*[self.dataset[self.subnatvar]])
                
                # Zero out the subnationals in the original weight file, and add them back in
                weights = weights*([nat_map]) + subnational_weights
            
        # Normalize to 1  
        # NOTE A POSSIBLE BUG... IF SUBNATIONAL IS ON AND THERE IS NO NATIONAL DATA IN THE SURROUNDING REGION, THIS WILL BREAK... CONSIDER POSSIBLE FIXES
        normalized_weights = np.zeros(weights.shape)
        for spatial_group in np.unique(spacemap):
            sp_grp_mask = (spacemap==spatial_group).astype(int)
            if (np.sum(weights*sp_grp_mask)>0):
                normalized_weights = normalized_weights + (((sp_weights[spatial_group] * (weights*sp_grp_mask)).T / (np.sum(weights*sp_grp_mask,axis=1)))).T
            
        self.final_weights = normalized_weights
        
        return normalized_weights
    
    # Add the weighted-average of the residuals back into the original predictions
    def smooth(self, locs=None):
        if locs==None:
            locs = pd.unique(self.stage1[self.spacevar])
            
        for loc in locs:
            print 'iso: '+loc
               
            # Generate space weights... Could modify this to count whether there is any data for this country [[ len(data[data['iso3'] == iso]) == 0  ]] and change zeta accordingly
            self.space_weights(loc)
        
            # Smooth away... might want to take care of the residuals in the function itself at some point
            prior = self.stage1[(self.stage1[self.spacevar]==loc)].drop_duplicates(cols=[self.timevar,self.agevar])[self.modelvar]
                                  
            smooth = np.array(prior) + np.sum(np.array(self.data['resid'])*self.final_weights, axis=1)
                        
            self.results[loc] = smooth
        
        return smooth
        
    # Method to set ST parameters
    def set_params(self, params, values):
        for i,param in enumerate(params):
            self[param] = values[i]
    
    # Merge the ST results back onto the input dataset
    def format_output(self, include_mad=False):
        
        gbd_regions = pd.read_csv(thispath+"/regions.csv")
        gbd_regions = gbd_regions[['iso3','gbd_analytical_region_id','gbd_analytical_region_name','gbd_analytical_superregion_id','gbd_analytical_superregion_name']]

        melted = pd.melt(self.results,id_vars=[self.agevar,self.timevar],var_name=self.spacevar,value_name='st_prediction')
        merged = pd.merge(self.inputset,melted,on=[self.agevar,self.timevar,self.spacevar],how='left')
        merged = pd.merge(merged,gbd_regions,on='iso3',how='left')
        
        if include_mad:
            # Calculate residuals
            merged['st_resid'] = merged[self.datavar] - merged['st_prediction']
        
            # Calculate MAD estimates at various geographical levels
            mad_global = mad(merged.st_resid)
            mad_regional = merged.groupby('gbd_analytical_region_id').agg({'st_resid': mad}).reset_index().rename(columns={'st_resid':'mad_regional'})
            mad_national = merged.groupby('iso3').agg({'st_resid': mad}).reset_index().rename(columns={'st_resid':'mad_national'})
            
            merged['mad_global'] = mad_global
            merged = pd.merge(merged,mad_regional,on="gbd_analytical_region_id",how="left")
            merged = pd.merge(merged,mad_national,on="iso3",how="left")
            
            merged['mad_regional'].fillna(np.median(merged["mad_regional"]),inplace=True)
            merged['mad_national'].fillna(np.median(merged["mad_national"]),inplace=True)
        
        return merged