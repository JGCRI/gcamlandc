Updated November 2nd, 2023 by Kendalynn A. Morris

# gcamlandc
GCAM land carbon module with added carbon cycling and climate feedbacks via Hector simple climate model.

This code was originally written by Dawn Woodard with guidance from Abigail C. Synder, 
while the current version has been curated by K.A.M. and Maridee Weber with substantial input from A.C.S.

D.W. repo: https://github.com/dawnlwoodard/gcamlandc

Cloned into: https://github.com/JGCRI/gcamlandc on July 23
https://github.com/JGCRI/gcamlandc/commit/1b62b8b0255c654c0707b6903d5227adc6ab81d3

Purpose: These scripts require land allocation and carbon density data from a GCAM database.
If parameters are set to TRUE (see run_land_calc.R), this information is passed to Hector which 
incorporates terrestrial carbon-cycling feedbacks.

These include...1) Updating C density at each timestep
		2) temperature sensitivity of soil respriation (Q10)
		3) CO2 fertilization of photosynthesis
		4) Net biome production constraint at each timestep

The outputs are new aboveground (ag) and belowground (bg) carobn density values,
as well as land-use and land-change emission data that includes these biogeochemical feedbacks.


The current version [Add commit link when available]

Requires:
Land Input XML's 1-5 and protected land inputs 2 & 3
NOTE - any change in XML structure from the configuration present in GCAMv7 
may result in issues with matching historical to modern land allocation

64-bit version of Java

R packages:
hector v3 or higher
data.table
ggplot2
ggsci
tidyr
dplyr
zoo

an empty folder called "data"

carefully check:
	1) all file paths in run_land_calc.R AND gcam_utils.R
	2) all output names (ie., scenaro_name at the end of run_land_calc.R)
	3) input files for correct implementation of land protection;
a gcam core model database should be used for output with protected lands (protected)
a gcam database under SSP1 should be used for output without protected lands (reference)


