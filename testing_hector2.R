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
gcam_years <- c(1750, 1800, 1850, 1900, 1950, 1975, 1990, 2005, 2010)
gcam_yr_ind <- which(net_luc_emiss_uncoupled_offline$year %in% gcam_years) 
for (ind in gcam_yr_ind){
  net_luc_emiss_uncoupled_offline[ind,]$value <-0.5*(net_luc_emiss_uncoupled_offline[ind-1,]$value +
                                                      net_luc_emiss_uncoupled_offline[ind+1,]$value) 
}
net_luc_emiss_uncoupled_offline[net_luc_emiss_uncoupled_offline$year == 2010,]$value <-
  net_luc_emiss_uncoupled_offline[net_luc_emiss_uncoupled_offline$year == 2009,]$value

# make a new emissions data frame that is identical to the old but with
# luc_emissions column updated:
new_emiss <- orig_emiss
new_emiss$luc_emissions <- (net_luc_emiss_uncoupled_offline %>%
                                 filter(year %in% min(orig_emiss$Date):max(orig_emiss$Date)))$value

# get the header info of the original emissions data frame:
## Probably read in but instead of skipping the first 4 lines, just read in 
## the first 4. I don't remember the read.csv() argument to do that tho

# add it to the new data frame and save
## This is the example I would start from if I had time to do it - I wrote
## this at some point.
## https://github.com/JGCRI/osiris/blob/b6cb3e6211f93cdc7d0ad91a5fde3fbae00d40e7/R/create_AgProdChange_xml.R#L68

##  offline: copy over the ini file with a new name 
## open it in a text editor, do a find and replace on ./gcam_emissions.csv and
## replace with ./<new file name>.csv
## save and close out
### ALSO check the beta and q10 values in that file and update with beta=0.55
### and q10=2.2 that we used to generate the coupled data, if needed 

# read in the new ini file and run hector with it 
ini_dir <- 'pic_data/pic_hector_ini/climate/'
ini_file <- paste0(ini_dir, 'hector-gcam.ini') # update to whatever name of new file
core <- hector::newcore(ini_file, suppresslogging = FALSE)

hector::run(core, runtodate = 2005)#gcam_emissions.csv file only goes to 2005


# If hector::run returns an interpolation error, it means we gave it a runtodate
# that is later than the emissions file actually has data for.

# have to reset(core) before updating the runtodate or else you will get 
# errors referring to oh values at the start of their name.
