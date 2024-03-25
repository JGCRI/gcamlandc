
library(dplyr)

write_dir <- 'results/plot_data_14Mar_2024/'

###NOTE
# file paths will need to be changed if new output is generated from run_land_calc.R
###

#
## Aspirational goal: make this into a function so that manipulating which output gets read-in is easier
#

#files are large, sourcing takes a moment

year0 <- 1745
last_year <- 2100

#all things reference should become "Uncoupled"
# Uncoupled data (protected = TRUE, spatially resolved = TRUE, coupled = FALSE)
# AND cCycling=FALSE -> Original GCAM Hector approach
uc_AG_emissions <- read.csv("Feb24_set2of5/ag_emiss_Uncoupled_pro_newBeta_newQ10.csv", row.names = 1)
uc_BG_emissions <- read.csv("Feb24_set2of5/bg_emiss_Uncoupled_pro_newBeta_newQ10.csv", row.names = 1)
# uc_climate_data <- read.csv("Feb24_set2of5/climate_data_UnCoupled_pro_newBeta_newQ10.csv")
# uc_gcam_land <- read.csv("Feb24_set2of5/gcam_land_alloc.csv")
uc_leaf_data <- read.csv("Feb24_set2of5/leaf_data_Uncoupled_pro_newBeta_newQ10.csv")
# uc_leaf_params <- read.csv("Feb24_set2of5/leaf_params_Uncoupled_pro_newBeta_newQ10.csv")


# transform bg emissions to format able to be joined with other leaf data
uc_BG <- data.frame(t(uc_BG_emissions))
colnames(uc_BG) <- row.names(uc_BG_emissions)
rm(uc_BG_emissions)
uc_BG$year <- seq(year0,last_year)
uc_BG_final <- uc_BG %>% tidyr::pivot_longer(cols=-c("year"),
                                       names_to = "name",
                                       values_to="bg_emiss")
rm(uc_BG)

# same for ag emissions
uc_AG <- data.frame(t(uc_AG_emissions))
colnames(uc_AG) <- row.names(uc_AG_emissions)
rm(uc_AG_emissions)
uc_AG$year <- seq(year0,last_year)
uc_AG_final <- uc_AG %>% tidyr::pivot_longer(cols=-c("year"),
                                       names_to = "name",
                                       values_to="ag_emiss")
rm(uc_AG)

# combine
uc_leaf_data %>% select(-X) %>%
  left_join(uc_BG_final, by = c("year", "name")) %>%
  left_join(uc_AG_final, by = c("year", "name")) -> uc_plot_data
rm(uc_AG_final)
rm(uc_BG_final)
rm(uc_leaf_data)

# calculate total net biome production
uc_plot_data$tot_land_to_atm_emiss <- uc_plot_data$ag_emiss + uc_plot_data$bg_emiss
# TODO
#check if this gets used anywhere
uc_plot_data$npp_rh <- uc_plot_data$NPP/uc_plot_data$Rh

write.csv(uc_plot_data %>% mutate(scenario='uncoupled'),
          paste0(write_dir, "uc_plot_data.csv"), row.names = FALSE)

# make long for plotting
uc_plot_data_long <- uc_plot_data %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_land_to_atm_emiss", "npp_rh"),
                      names_to="variable",
                      values_to="value")
uc_plot_data_long$scenario <- "uncoupled"
rm(uc_plot_data)

write.csv(uc_plot_data_long, paste0(write_dir, "uc_plot_data_long.csv"), row.names = FALSE)
rm(uc_plot_data_long)

#all things protected should become "Coupled"
#fully coupled data (protected = TRUE,  spatially resolved = TRUE, coupled = TRUE)
coup_AG_emissions <- read.csv("Feb24_set3of5/ag_emiss_Coupled_pro_newBeta_newQ10.csv", row.names = 1)
coup_BG_emissions <- read.csv("Feb24_set3of5/bg_emiss_Coupled_pro_newBeta_newQ10.csv", row.names = 1)
# coup_climate_data <- read.csv("Feb24_set3of5/climate_data_Coupled_pro_newBeta_newQ10.csv")
# coup_gcam_land <- read.csv("Feb24_set3of5/gcam_land_alloc.csv")
coup_leaf_data <- read.csv("Feb24_set3of5/leaf_data_Coupled_pro_newBeta_newQ10.csv")
# coup_leaf_params <- read.csv("Feb24_set3of5/leaf_params_Coupled_pro_newBeta_newQ10.csv")

# transform bg emissions to format able to be joined with other leaf data
coup_BG <- data.frame(t(coup_BG_emissions))
colnames(coup_BG) <- row.names(coup_BG_emissions)
rm(coup_BG_emissions)
coup_BG$year <- seq(year0,last_year)
coup_BG_final <- coup_BG %>% tidyr::pivot_longer(cols=-c("year"),
                                               names_to = "name",
                                               values_to="bg_emiss")
rm(coup_BG)

# same for ag emissions
coup_AG <- data.frame(t(coup_AG_emissions))
colnames(coup_AG) <- row.names(coup_AG_emissions)
rm(coup_AG_emissions)
coup_AG$year <- seq(year0,last_year)
coup_AG_final <- coup_AG %>% tidyr::pivot_longer(cols=-c("year"),
                                               names_to = "name",
                                               values_to="ag_emiss")
rm(coup_AG)

# combine
coup_leaf_data %>% select(-X) %>%
  left_join(coup_BG_final, by = c("year", "name")) %>%
  left_join(coup_AG_final, by = c("year", "name")) -> coup_plot_data
rm(coup_leaf_data)
rm(coup_AG_final)
rm(coup_BG_final)

# calculate total net biome production
coup_plot_data$tot_land_to_atm_emiss <- coup_plot_data$ag_emiss + coup_plot_data$bg_emiss
# TODO
#check if this gets used anywhere
coup_plot_data$npp_rh <- coup_plot_data$NPP/coup_plot_data$Rh

write.csv(coup_plot_data %>% mutate(scenario='coupled'),
          paste0(write_dir, "coup_plot_data.csv"), row.names = FALSE)



# make long for plotting
coup_plot_data_long <- coup_plot_data %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_land_to_atm_emiss", "npp_rh"),
                      names_to="variable",
                      values_to="value") %>%
  mutate(scenario = 'coupled') 
rm(coup_plot_data)

write.csv(coup_plot_data_long, paste0(write_dir, "coup_plot_data_long.csv"), row.names = FALSE)
rm(coup_plot_data_long)



