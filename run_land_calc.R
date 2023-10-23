# run with GCAM data (see [other script] for example running with other data)
source("gcam_utils.R")
source("land_utils.R")
library(tidyr)
#Sys.setenv("PATH" = "C:/Users/morr497/Documents/OneDriveSafeSpace/openjdk-20.0.1_windows-x64_bin/jdk-20.0.1/bin")

# necessary inputs: 5 gcam land xmls + 2 protected lands, gcam database (for grabbing modern land allocation data)
# will need to set paths for each of these in code below

read_data <- TRUE  # set this flag if land allocation data needs to be updated. If false, will read from saved files
read_params <- TRUE # set this flag if land leaf parameter data needs to be updated (carbon densities, soil timescales, etc). If false, will read from saved files
protected <- TRUE # set this flag to include protected lands. If true, will read in protected lands data to replace land inputs 2 & 3

year0 <- 1745
last_year <- 2100  # the year to have carbon emissions vectors go through
stop_year <- 2010  # the year to actually stop calculations

ccycling= TRUE  # if TRUE, turns on carbon density calculations at each time step. If FALSE, code uses fixed densities
rhEff= TRUE  # if TRUE, enables Q10 feedback with temperature (affects soil respiration)
betaEff= TRUE  # if TRUE, enables CO2 fertilization feedback (affects NPP)
coupled= TRUE  # this refers to coupling with Hector. If true, then NBP_constraint is set each year for Hector


# Load in leaf data:
# Either by reading from raw gcamdata xml files and GCAM output data base with
# functions in gcam_utils.R (read_data == TRUE),
# OR if that has been done already and saved (read_data == FALSE), just load those

if (read_data){
  # get input data from GCAM
    gcam_land_alloc <- get_gcam_land_alloc(db_name="database_basexdb",
                                           gcam_dir= "reference/DawnDB/",
                                           scenario="Reference",
                                           read_from_file=FALSE)
    # scenario is doing nothing when read_from_file is TRUE
    
    land_roots <- read_land_inputs_xml2(folder = "reference", protected = protected)
    leaf_data <- process_xml_inputs(land_roots, gcam_land_alloc)
if(protected){
  saveRDS(leaf_data,file="data/protected_leaf_data.RDS")  # store for future use
}
  else{
    saveRDS(leaf_data, file="data/leaf_data.RDS") 
  } # end store for future use
    
} else {
  if (protected){
    leaf_data <- readRDS(file="data/protected_leaf_data.RDS")
  } else leaf_data <- readRDS(file="data/leaf_data.RDS")
} # end loading leaf data



leaf_data$name <- paste0(leaf_data$region,"_",leaf_data$landleaf)  # add single column with region + leaf info for easier reference later

if (read_params){
  # get soil timescale data
  # TODO update to be compatible with eventual updates to make landleaf specific soil timescales
  soil_timescales <- get_soilTS_byRegion(land_roots[[1]])  
  soil_timescales$soilTimeScale <- as.numeric(soil_timescales$soilTimeScale)

  outer_params2 <- get_leaf_params(land_roots, soil_timescales, leaf_data)
  saveRDS(outer_params2,file="data/param_data.RDS")
  
} else {
  outer_params2 <- readRDS("data/param_data.RDS")
  
}


outer_land_alloc2 <- leaf_data  # or protected_data
outer_land_alloc2$name <- paste(outer_land_alloc2$region, outer_land_alloc2$landleaf, sep="_")


# uncomment two lines below to run with a smaller set of land leaves
# selected <- sample(outer_params2$name,25)
# outer_params2 <- filter(outer_params2,name %in% selected)

years <- unique(outer_land_alloc2$year)
years <- years[years>=year0]

run_years <- years[years<=stop_year]
outer_land_alloc2 <- filter(outer_land_alloc2,name %in% outer_params2$name, year %in% run_years) %>%
  mutate(value=land_alloc) %>% select(-c("land_alloc"))

# initialize Hector
rcp <- "ssp245"
scenario_file <- paste0("input/hector_",rcp,".ini")
ini_file <- system.file(scenario_file, package="hector")



outer_land_alloc2 <- data.table::setDT(outer_land_alloc2)
outer_params2 <- data.table::setDT(outer_params2)

output <- run_all_years(outer_land_alloc2, outer_params2, ini_file,
                        stop_year=stop_year, last_year=last_year,
                        rhEff=rhEff, betaEff=betaEff,
                        cCycling=ccycling, coupled=coupled)

scenario_name <- "full_world_protected_DawnDB_2100"
write.csv(output[["leaf_data"]],file=paste0("data/leaf_data_",scenario_name,".csv"))
write.csv(output[["params"]],file=paste0("data/leaf_params_",scenario_name,".csv"))
write.csv(output[["climate"]],file=paste0("data/climate_data_",scenario_name,".csv"))
write.csv(output[["ag_emiss"]],file=paste0("data/ag_emiss_",scenario_name,".csv"))
write.csv(output[["bg_emiss"]],file=paste0("data/bg_emiss_",scenario_name,".csv"))

