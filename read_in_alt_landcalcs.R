
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
alt_uc_AG_emissions <- read.csv("data/uncoupled_protected_Beta0.22/ag_emiss_UnCoupled_pro_Beta0.22.csv", row.names = 1)
alt_uc_BG_emissions <- read.csv("data/uncoupled_protected_Beta0.22/bg_emiss_Uncoupled_pro_Beta0.22.csv", row.names = 1)
# alt_uc_climate_data <- read.csv("data/uncoupled_protected_Beta0.22/climate_data_UnCoupled_pro_Beta0.22.csv")
# alt_uc_gcam_land <- read.csv("data/uncoupled_protected_Beta0.22/gcam_land_alloc.csv")
alt_uc_leaf_data <- read.csv("data/uncoupled_protected_Beta0.22/leaf_data_Uncoupled_pro_Beta0.22.csv")
# alt_uc_leaf_params <- read.csv("data/uncoupled_protected_Beta0.22/leaf_params_Uncoupled_pro_Beta0.22.csv")


# transform bg emissions to format able to be joined with other leaf data
alt_uc_BG <- data.frame(t(alt_uc_BG_emissions))
colnames(alt_uc_BG) <- row.names(alt_uc_BG_emissions)
rm(alt_uc_BG_emissions)
alt_uc_BG$year <- seq(year0,last_year)
alt_uc_BG_final <-alt_uc_BG %>% tidyr::pivot_longer(cols=-c("year"),
                                       names_to = "name",
                                       values_to="bg_emiss")
rm(alt_uc_BG)

# same for ag emissions
alt_uc_AG <- data.frame(t(alt_uc_AG_emissions))
colnames(alt_uc_AG) <- row.names(alt_uc_AG_emissions)
rm(alt_uc_AG_emissions)
alt_uc_AG$year <- seq(year0,last_year)
alt_uc_AG_final <- alt_uc_AG %>% tidyr::pivot_longer(cols=-c("year"),
                                       names_to = "name",
                                       values_to="ag_emiss")
rm(alt_uc_AG)

# combine
alt_uc_leaf_data %>% select(-X) %>%
  left_join(alt_uc_BG_final, by = c("year", "name")) %>%
  left_join(alt_uc_AG_final, by = c("year", "name")) -> alt_uc_plot_data
rm(alt_uc_AG_final)
rm(alt_uc_BG_final)
rm(alt_uc_leaf_data)

# calculate total net biome production
alt_uc_plot_data$tot_land_to_atm_emiss <- alt_uc_plot_data$ag_emiss + alt_uc_plot_data$bg_emiss
# TODO
#check if this gets used anywhere
alt_uc_plot_data$npp_rh <- alt_uc_plot_data$NPP/alt_uc_plot_data$Rh

write.csv(alt_uc_plot_data %>% mutate(scenario='uncoupled'),
          paste0(write_dir, "alt_uc_plot_data.csv"), row.names = FALSE)

# make long for plotting
alt_uc_plot_data_long <- alt_uc_plot_data %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_land_to_atm_emiss", "npp_rh"),
                      names_to="variable",
                      values_to="value")
alt_uc_plot_data_long$scenario <- "uncoupled"
rm(alt_uc_plot_data)

write.csv(alt_uc_plot_data_long, paste0(write_dir, "alt_uc_plot_data_long.csv"), row.names = FALSE)
rm(alt_uc_plot_data_long)

#all things protected should become "Coupled"
#fully coupled data (protected = TRUE,  spatially resolved = TRUE, coupled = TRUE)
alt_coup_AG_emissions <- read.csv("data/coupled_protected_Beta0.22/ag_emiss_Coupled_pro_Beta0.22.csv", row.names = 1)
alt_coup_BG_emissions <- read.csv("data/coupled_protected_Beta0.22/bg_emiss_Coupled_pro_Beta0.22.csv", row.names = 1)
# alt_coup_climate_data <- read.csv("data/coupled_protected_Beta0.22/climate_data_Coupled_pro_Beta0.22.csv")
# alt_coup_gcam_land <- read.csv("data/coupled_protected_Beta0.22/gcam_land_alloc.csv")
alt_coup_leaf_data <- read.csv("data/coupled_protected_Beta0.22/leaf_data_Coupled_pro_Beta0.22.csv")
# alt_coup_leaf_params <- read.csv("data/coupled_protected_Beta0.22/leaf_params_Coupled_pro_Beta0.22.csv")

# transform bg emissions to format able to be joined with other leaf data
alt_coup_BG <- data.frame(t(alt_coup_BG_emissions))
colnames(alt_coup_BG) <- row.names(alt_coup_BG_emissions)
rm(alt_coup_BG_emissions)
alt_coup_BG$year <- seq(year0,last_year)
alt_coup_BG_final <- alt_coup_BG %>% tidyr::pivot_longer(cols=-c("year"),
                                               names_to = "name",
                                               values_to="bg_emiss")
rm(alt_coup_BG)

# same for ag emissions
alt_coup_AG <- data.frame(t(alt_coup_AG_emissions))
colnames(alt_coup_AG) <- row.names(alt_coup_AG_emissions)
rm(alt_coup_AG_emissions)
alt_coup_AG$year <- seq(year0,last_year)
alt_coup_AG_final <-alt_coup_AG %>% tidyr::pivot_longer(cols=-c("year"),
                                               names_to = "name",
                                               values_to="ag_emiss")
rm( alt_coup_AG)

# combine
alt_coup_leaf_data %>% select(-X) %>%
  left_join(alt_coup_BG_final, by = c("year", "name")) %>%
  left_join(alt_coup_AG_final, by = c("year", "name")) ->  alt_coup_plot_data
rm(alt_coup_leaf_data)
rm(alt_coup_AG_final)
rm(alt_coup_BG_final)

# calculate total net biome production
alt_coup_plot_data$tot_land_to_atm_emiss <- alt_coup_plot_data$ag_emiss + alt_coup_plot_data$bg_emiss
# TODO
#check if this gets used anywhere
alt_coup_plot_data$npp_rh <- alt_coup_plot_data$NPP/alt_coup_plot_data$Rh

write.csv(alt_coup_plot_data %>% mutate(scenario='coupled'),
          paste0(write_dir, "alt_coup_plot_data.csv"), row.names = FALSE)



# make long for plotting
alt_coup_plot_data_long <- alt_coup_plot_data %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_land_to_atm_emiss", "npp_rh"),
                      names_to="variable",
                      values_to="value") %>%
  mutate(scenario = 'coupled') 
rm(alt_coup_plot_data)

write.csv(alt_coup_plot_data_long, paste0(write_dir, "alt_coup_plot_data_long.csv"), row.names = FALSE)
rm(alt_coup_plot_data_long)



