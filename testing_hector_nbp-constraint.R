#remotes::install_github("jgcri/hector", force = T)
library(hector)
library(ggplot2)
library(tidyverse)

## what we want to test ##
# 1) Use free running hector to get NBP
# 2) Use free run NBP values to constrain hector - should exactly equal #1
# 3) +1 to constrained values to #2

# read in ini file
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")

## 1) Free running hector 

# initiate core
core_hector <- newcore(ini_file, name = "hector")

## Set vars for newest version of hector
## Run or don't run, don't think it matters right now
# setvar(core_hector, NA, var = BETA(), values = 0.53, NA)
# fetchvars(core_hector, NA, BETA())
# reset(core_hector)
# 
# setvar(core_hector, NA, var = Q10_RH(), values = 1.76, NA)
# fetchvars(core_hector, NA, Q10_RH())
# reset(core_hector)

# Run hector - hector free run to 2100
run(core_hector, runtodate = 2100)

# fetch vars
out_hector <-
  fetchvars(core = core_hector,
            dates = 1750:2100,
            vars = c(CONCENTRATIONS_CO2(),
                     GLOBAL_TAS(),
                     NBP(),
                     NPP(),
                     RH()))

# shut down current core
shutdown(core_hector)

plot_hector <- 
  ggplot(data = out_hector) +
  geom_line(
    aes(
      x = year, 
      y = value),
    color = "red") +
  facet_wrap(~variable, scales = "free_y")
plot_hector

## 2) Run hector with free run NBP values as constraint 

# extract NBP values from the free run Hector output
NBP_values <- out_hector %>% 
  filter(variable == "NBP")

# initiate a new core the for the constrained NBP run
core_nbp_constrain <- newcore(ini_file, name = "hector_nbp_constrain")

# # set vars for newest version of Hector
# setvar(core_nbp_constrain, NA, var = BETA(), values = 0.53, NA)
# reset(core_nbp_constrain)
# 
# setvar(core_nbp_constrain, NA, var = Q10_RH(), values = 1.76, NA)
# reset(core_nbp_constrain)

# use setvar() to constrain nbp to values in NBP_values
setvar(core_nbp_constrain,
       dates = NBP_values$year,
       var = NBP_CONSTRAIN(),
       values = NBP_values$value,
       unit = getunits(NBP_CONSTRAIN()))
reset(core_nbp_constrain)
core_nbp_constrain

# Run hector - constrained nbp to 2100
run(core_nbp_constrain, runtodate = 2100)

# Fetch results 
out_nbp_constrain <- fetchvars(core_nbp_constrain,
                               dates = 1746:2100,
                               vars = c(CONCENTRATIONS_CO2(),
                                        GLOBAL_TAS(),
                                        NBP(),
                                        NPP(),
                                        RH()))
# shutdown current core
shutdown(core_nbp_constrain)

plot_hector +
  geom_line(data = out_nbp_constrain,
            aes(
              x = year, 
              y = value),
            color = "blue") +
  facet_wrap(~variable, scales = "free_y")

## 3) Increasing NBP to make a forced sink

## CURRENTLY GETTING FLUX ERROR ##

# Create new df with added nbp column to force an NBP sink
NBP_force_sink <- NBP_values %>% 
  mutate(nbp_sink = value + 1)

# initiate a new core
core_nbp_sink <- newcore(ini_file, name = "hector_nbp_sink")

# # set vars for newest version of Hector
# setvar(core_nbp_2, NA, var = BETA(), values = 0.53, NA)
# reset(core_nbp_2)
# 
# setvar(core_nbp_2, NA, var = Q10_RH(), values = 1.76, NA)
# reset(core_nbp_2)

# Set nbp vars to forced sink values
setvar(core_nbp_sink,
       dates = NBP_force_sink$year,
       var = NBP_CONSTRAIN(),
       values = NBP_force_sink$nbp_sink,
       unit = getunits(NBP_CONSTRAIN()))

# Run hector
run(core_nbp_sink, runtodate = 2100)

# fetch vars 
out_nbp_sink <- fetchvars(core_nbp_sink, 
                          dates = 1750:2100, 
                          vars = c(CONCENTRATIONS_CO2(),
                                   GLOBAL_TAS(),
                                   NBP(),
                                   NPP(),
                                   RH()))

## plot with key ##

compare_df <- rbind(out_hector, out_nbp_constrain) #out_nbp_sink)

compare_plot <- 
  ggplot(data = compare_df) +
  geom_line(aes(
    x = year,
    y = value,
    color = scenario)) +
  facet_wrap(~ variable, scales = "free_y")
compare_plot
