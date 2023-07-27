
library(dplyr)

getwd()
Sys.setenv("PATH" = "C:/Users/morr497/Documents/OneDriveSafeSpace/openjdk-20.0.1_windows-x64_bin/jdk-20.0.1/bin")
system2("java", "-version")

AG_emissions <- read.csv("data/ag_emiss_full_world_real-protected_2100.csv", row.names = 1)
BG_emissions <- read.csv("data/bg_emiss_full_world_real-protected_2100.csv", row.names = 1)
climate_data <- read.csv("data/climate_data_full_world_real-protected_2100.csv")
gcam_land <- read.csv("data/gcam_land_alloc.csv")
leaf_data <- read.csv("data/leaf_data_full_world_real-protected_2100.csv")
leaf_params <- read.csv("data/leaf_params_full_world_real-protected_2100.csv")

AG_emissions <- read.csv("data_reference_TTTT/ag_emiss_full_world_real-baseline_no-protected_2100.csv", row.names = 1)
BG_emissions <- read.csv("data_reference_TTTT/bg_emiss_full_world_real-baseline_no-protected_2100.csv", row.names = 1)
climate_data <- read.csv("data_reference_TTTT/climate_data_full_world_real-baseline_no-protected_2100.csv")
gcam_land <- read.csv("data_reference_TTTT/gcam_land_alloc.csv")
leaf_data <- read.csv("data_reference_TTTT/leaf_data_full_world_real-baseline_no-protected_2100.csv")
leaf_params <- read.csv("data_reference_TTTT/leaf_params_full_world_real-baseline_no-protected_2100.csv")

# transform bg emissions to format able to be joined with other leaf data
BG <- data.frame(t(BG_emissions))
#BG <- data.frame(t(full_bg_emiss))
colnames(BG) <- row.names(BG_emissions)
#colnames(BG) <- row.names(full_bg_emiss)
BG$year <- seq(year0,last_year)
BG_final <- BG %>% tidyr::pivot_longer(cols=-c("year"),
                                 names_to = "name",
                                 values_to="bg_emiss")

# same for ag emissions
AG <- data.frame(t(AG_emissions))
#AG <- data.frame(t(full_ag_emiss))
colnames(AG) <- row.names(AG_emissions)
#colnames(AG) <- row.names(full_ag_emiss)
AG$year <- seq(year0,last_year)
AG_final <- AG %>% tidyr::pivot_longer(cols=-c("year"),
                                       names_to = "name",
                                       values_to="ag_emiss")

# combine
leaf_data %>% select(-X) %>%
  left_join(BG_final, by = c("year", "name")) %>%
  left_join(AG_final, by = c("year", "name")) -> plot_data

plot_data$tot_nbp <- plot_data$ag_emiss + plot_data$bg_emiss
plot_data$npp_rh <- plot_data$NPP/plot_data$Rh

plot_data_long <- plot_data %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp", "npp_rh"),
                      names_to="variable",
                      values_to="value")
