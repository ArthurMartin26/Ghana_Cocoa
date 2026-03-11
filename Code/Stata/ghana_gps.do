/**************************
==========================
This file uses the geospatial data for DHS files for both 2022 and 2014 ghana DHS files to link clusters together
currently clusters are linked using nearest neighbour within 10km of each other but this can be made smaller if needed 

This leaves us with a df which can be used to merge the rest of the results together. 

**************************/

ssc install geodist, replace
ssc install geonear, replace

*global path_in "C:\Users\Arthur.Martin\OneDrive - Department of Health and Social Care\Documents\LSE\Diss\CODING\Ghana\Ghana_panel"
global path_out "C:\Users\Arthur.Martin\OneDrive - Department of Health and Social Care\Documents\LSE\Diss\CODING\DATA\Ghana\Panel\Outputs"
global path_in22 "C:\Users\Arthur.Martin\OneDrive - Department of Health and Social Care\Documents\LSE\Diss\CODING\DATA\Ghana\2022\GH_2022_DHS_GEOG"
global path_in14 "C:\Users\Arthur.Martin\OneDrive - Department of Health and Social Care\Documents\LSE\Diss\CODING\DATA\Ghana\2014\GH_2014_DHS_GEOG"

**### Ghana 2022 ####

**global path_out22 "C:\Users\Arthur.Martin\OneDrive - Department of Health and Social Care\Documents\LSE\Diss\CODING\Ghana\Ghana 2022\Outputs"

** read in the shape data and save as a .dta file


spshape2dta "$path_in22\GHGE8AFL\GHGE8AFL.shp" , replace
use"GHGE8AFL.dta",clear

rename DHSCLUST cluster
rename LONGNUM lon
rename LATNUM lat
gen year = 2022
keep cluster lon lat year

compress


save "$path_out/Ghana22_clean_gps.dta", replace

***********************************************
** Ghana 2014 *********************************

spshape2dta "$path_in14\GHGE71FL\GHGE71FL.shp" , replace
use"GHGE71FL.dta",clear

rename DHSCLUST cluster
rename LONGNUM lon
rename LATNUM lat
gen year = 2014
keep cluster lon lat year

compress

save "$path_out/Ghana14_clean_gps.dta", replace

**************************************************
************************************************
* NEAREST-NEIGHBOUR MATCH: 2022 clusters to 2014 clusters
************************************************
use "$path_out\Ghana22_clean_gps.dta", clear
rename cluster cluster22
rename lon lon22
rename lat lat22

compress
save "$path_out\Ghana22_mastergps.dta", replace

** 2014 

use "$path_out\Ghana14_clean_gps.dta", clear
rename cluster cluster14
rename lon lon14
rename lat lat14

compress
save "$path_out\Ghana14_mastergps.dta", replace


************************************************
* MATCH 2022 CLUSTERS TO NEAREST 2014 CLUSTERS
************************************************

* Load 2022 master file
use "$path_out\Ghana22_mastergps.dta", clear

cross using "$path_out/Ghana14_mastergps.dta"

geodist lat22 lon22 lat14 lon14, gen(dist_km)

bysort cluster22 (dist_km): keep if _n==1

keep if dist_km <= 5
** not sure if this is the 'best' way to select nearest neighbouts but it works
bysort cluster14 (dist_km): keep if _n==1

drop lon22 lat22 year lon14 lat14 dist_km

order cluster22 cluster14

save "$path_out\Ghana_linked_clusters.dta", replace


*******************************************************
****** Link clusters to the Nightlights data **********
*******************************************************

** import the data which has the nightlights in it 
import delimited "$path_in22\GHGC8AFL\GHGC8AFL.csv",clear
keep dhsclust nightlights_composite
rename nightlights_composite nl22
rename dhsclust cluster22

save "$path_out\Ghana22_nl.dta", replace

***do the same for 2014

** import the data which has the nightlights in it 
import delimited "$path_in14\GHGC72FL\GHGC72FL.csv",clear
keep dhsclust nightlights_composite
rename nightlights_composite nl14
rename dhsclust cluster14

save "$path_out\Ghana14_nl.dta", replace



use "$path_out\Ghana_linked_clusters.dta", clear

merge m:1 cluster22 using "$path_out/Ghana22_nl.dta"
keep if inlist(_merge, 1, 3)
drop _merge

merge m:1 cluster14 using "$path_out/Ghana14_nl.dta"
keep if inlist(_merge, 1, 3)
drop _merge


save "$path_out\Ghana_nl_final.dta", replace




