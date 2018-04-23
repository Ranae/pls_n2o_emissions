# Predicting spring nitrous oxide emissions using partial least squares regression. 

I am trying to build a model to predict cumulative spring nitrous oxide (N2O) emissions based off of soil characteristics and winter weather. There are five sites. Within each site, there are multiple soil types, but soil does not change between years. Soil types within a site share the same weather every year. Weather variables are different every year.   

##Predictor variables fall into a few latent variables.
The following variables are closely related and covary within groups, (e.g. theta_s values depend upon how much sand/silt/clay are in the soil).

###Soil texture

sand, silt, clay, theta_r, theta_s, silt_f_psa, silt_c_psa, Ks,  

###Soil chemical

ph_h2o, cec7, cec82, bs7, ex_ca, ex_mg, ex_k, 

###Soil substrate

oc, estimated_om, n_tot, estimated_c_to_n, ex_k_saturation

###Weather

cold_sum, sum_tmax, sum_rad, snow_sum, precip_sum, cfd
