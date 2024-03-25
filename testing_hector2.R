library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)

data_dir <- 'plot_data_06Mar_2024/'

# Set up hector for r if needed
# https://jgcri.github.io/hector/articles/BuildHector.html
# install.packages("remotes")
# remotes::install_github("jgcri/hector")
library(hector)

###############################################################################
# package scenario
rcp <- "ssp245"
scenario_file <- paste0("input/hector_",rcp,".ini")
ini_file245 <- system.file(scenario_file, package="hector")

core245 <- hector::newcore(ini_file245, suppresslogging = FALSE, name='245')
hector::run(core245,runtodate = 2100)

out <- hector::fetchvars(core245, dates=1746:2100, vars=c(hector::NBP()))

rcp <- "ssp585"
scenario_file <- paste0("input/hector_",rcp,".ini")
ini_file585 <- system.file(scenario_file, package="hector")

core585 <- hector::newcore(ini_file585, suppresslogging = FALSE, name='585')
hector::run(core585,runtodate = 2100)

out <- bind_rows(out, hector::fetchvars(core585, dates=1746:2100, vars=c(hector::NBP())))
ggplot(out) + geom_line(aes(x = year, y = value, color = scenario))


###############################################################################

# GCAM scenario
# the orignal gcam emissions the ini file calls:
ini_dir <- 'pic_data/pic_hector_ini/climate/'
orig_emiss <- read.csv(paste0(ini_dir, 'gcam_emissions.csv'), skip = 4, stringsAsFactors = F)

# read in our LUC emissions fromt he uncoupled model and aggregate to global"
gcam_years <- c(1750, 1800, 1850, 1900, 1950, 1975, 1990, 2005, 2010, 2015)

net_luc_emiss_uncoupled_offline <- read.csv(paste0(data_dir, 'all_tot_nbp.csv'),
                                                             stringsAsFactors = F) %>%
  filter(scenario == 'uncoupled') %>% 
  group_by(year, variable) %>%
  summarize(value=sum(value)*1e-3) %>%
  ungroup 

# tidy the gcam year jumps
# applying NA out the gcam years and use approx() to interpolate between years 
gcam_years <- c(1750, 1800, 1850, 1900, 1950, 1975, 1990, 2005, 2010)
# gcam_yr_ind <- which(net_luc_emiss_uncoupled_offline$year %in% gcam_years)
# for (ind in gcam_yr_ind){
#   net_luc_emiss_uncoupled_offline[ind,]$value <-0.5*(net_luc_emiss_uncoupled_offline[ind-1,]$value +
#                                                       net_luc_emiss_uncoupled_offline[ind+1,]$value)
# }
# net_luc_emiss_uncoupled_offline[net_luc_emiss_uncoupled_offline$year == 2010,]$value <-
#   net_luc_emiss_uncoupled_offline[net_luc_emiss_uncoupled_offline$year == 2009,]$value

# consider using this pipeline to fill NAs for gcam_years and interpolating to
# re-fill NAs
net_luc_emiss_uncoupled_offline <- net_luc_emiss_uncoupled_offline %>%
  # replace values for years in gcam_years with NAs
  mutate(value = replace(value, year %in% gcam_years, NA)) %>%
  # if there is an NA in the value column approximate that value, otherwise use original value
  mutate(value = ifelse(is.na(value),
                        approx(year, value, xout= year, rule = 2)$y, # rule two copies 2009 value to 2010
                        value))

# # make a new emissions data frame that is identical to the old but with
# # luc_emissions column updated:
# new_emiss <- orig_emiss
# # another way to do this would be to use a join -- left_join()
# new_emiss$luc_emissions <- (net_luc_emiss_uncoupled_offline %>%
#                                  filter(year %in% min(orig_emiss$Date):max(orig_emiss$Date)))$value

# consider a pipeline that joins luc_emissions data from the offline result as a value column
# then replaces the luc_emissions column with offline values, then drops the value column.
new_emiss <- orig_emiss %>%
  # left_join the offline values by Date after renaming columns in the offline results
  left_join(net_luc_emiss_uncoupled_offline %>% 
              # rename year column to Date
              rename(Date = year) %>% 
              # delete variable column from offline results  
              select(-variable),
            by = "Date") %>%
  # substitute the original luc_emissions with the offline values 
  mutate(luc_emissions = value) %>% 
  # delete the value column, leaving the new luc_emissions column
  select(-value)

# write new_emission .csv
write.csv(new_emiss, paste0(ini_dir, 'new_emissions.csv'), quote = FALSE, row.names = FALSE)

# get the header info of the original emissions data frame:
## readLines() to read first four lines and store
header <- readLines(paste0(ini_dir, 'gcam_emissions.csv'), n = 4)

# add it to the new data frame and save
## readLines() of the new_emiss .csv file
dat_new_emiss <- readLines(paste0(ini_dir, 'new_emissions.csv'))

## concatenate with header
dat <- c(header, dat_new_emiss)

## and write new lines
writeLines(dat, paste0(ini_dir, "new_emissions.csv"))

# replace old emissions file path with new emissions file path
## readLines() of the old emissions file path
old_emission_ini <- readLines(paste0(ini_dir, 'hector-gcam.ini'))

## substitute old emissions path with new emissions path in the old emissions ini
new_emission_ini <- gsub("./gcam_emissions.csv", "./new_emissions.csv", old_emission_ini)

## write lines for the new emisisons ini.
writeLines(new_emission_ini, paste0(ini_dir, 'new-hector-gcam.ini'))

## save and close out
### ALSO check the beta and q10 values in that file and update with beta=0.55
### and q10=2.2 that we used to generate the coupled data, if needed 

# read in the new ini file and run hector with it 
ini_file <- paste0(ini_dir, 'new-hector-gcam.ini') # update to whatever name of new file
core <- hector::newcore(ini_file)

hector::run(core, runtodate = 2005) #gcam_emissions.csv file only goes to 2005

out <- fetchvars(core, 1750:2005, vars = NBP())
head(out)
# If hector::run returns an interpolation error, it means we gave it a runtodate
# that is later than the emissions file actually has data for.

# have to reset(core) before updating the runtodate or else you will get 
# errors referring to oh values at the start of their name.
