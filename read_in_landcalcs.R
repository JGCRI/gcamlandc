
library(dplyr)

###NOTE
# file paths will need to be changed is new output is generated from run_land_calc.R
###

#source this code to create two identical datasets,
#one with baseline or reference GCAM land allocation
#the other with protected land turned on for GCAM land allocation

#files are large, sourcing takes a moment

year0 <- 1745
last_year <- 2100 

#reference data (protected = FALSE)
ref_AG_emissions <- read.csv("Nov23_data_reference/ag_emiss_full_world_reference_PIC_DB_2100.csv", row.names = 1)
ref_BG_emissions <- read.csv("Nov23_data_reference/bg_emiss_full_world_reference_PIC_DB_2100.csv", row.names = 1)
ref_climate_data <- read.csv("Nov23_data_reference/climate_data_full_world_reference_PIC_DB_2100.csv")
#ref_gcam_land <- read.csv("Nov23_data_reference/gcam_land_alloc.csv") #skipping for now until regenerated
ref_leaf_data <- read.csv("Nov23_data_reference/leaf_data_full_world_reference_PIC_DB_2100.csv")
ref_leaf_params <- read.csv("Nov23_data_reference/leaf_params_full_world_reference_PIC_DB_2100.csv")

# transform bg emissions to format able to be joined with other leaf data
ref_BG <- data.frame(t(ref_BG_emissions))
colnames(ref_BG) <- row.names(ref_BG_emissions)
ref_BG$year <- seq(year0,last_year)
ref_BG_final <- ref_BG %>% tidyr::pivot_longer(cols=-c("year"),
                                       names_to = "name",
                                       values_to="bg_emiss")
# same for ag emissions
ref_AG <- data.frame(t(ref_AG_emissions))
colnames(ref_AG) <- row.names(ref_AG_emissions)
ref_AG$year <- seq(year0,last_year)
ref_AG_final <- ref_AG %>% tidyr::pivot_longer(cols=-c("year"),
                                       names_to = "name",
                                       values_to="ag_emiss")
# combine
ref_leaf_data %>% select(-X) %>%
  left_join(ref_BG_final, by = c("year", "name")) %>%
  left_join(ref_AG_final, by = c("year", "name")) -> ref_plot_data

# calculate total net biome production
ref_plot_data$tot_nbp <- ref_plot_data$ag_emiss + ref_plot_data$bg_emiss
# TODO
#check if this gets used anywhere
ref_plot_data$npp_rh <- ref_plot_data$NPP/ref_plot_data$Rh

# make long for plotting
ref_plot_data_long <- ref_plot_data %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp", "npp_rh"),
                      names_to="variable",
                      values_to="value")
ref_plot_data_long$scenario <- "baseline"




#protected data (protected = TRUE)
pro_AG_emissions <- read.csv("Nov23_data_protected/ag_emiss_full_world_protected_PIC_DB_2100.csv", row.names = 1)
pro_BG_emissions <- read.csv("Nov23_data_protected/bg_emiss_full_world_protected_PIC_DB_2100.csv", row.names = 1)
pro_climate_data <- read.csv("Nov23_data_protected/climate_data_full_world_protected_PIC_DB_2100.csv")
pro_gcam_land <- read.csv("Nov23_data_protected/gcam_land_alloc.csv")
pro_leaf_data <- read.csv("Nov23_data_protected/leaf_data_full_world_protected_PIC_DB_2100.csv")
pro_leaf_params <- read.csv("Nov23_data_protected/leaf_params_full_world_protected_PIC_DB_2100.csv")

# transform bg emissions to format able to be joined with other leaf data
pro_BG <- data.frame(t(pro_BG_emissions))
colnames(pro_BG) <- row.names(pro_BG_emissions)
pro_BG$year <- seq(year0,last_year)
pro_BG_final <- pro_BG %>% tidyr::pivot_longer(cols=-c("year"),
                                               names_to = "name",
                                               values_to="bg_emiss")
# same for ag emissions
pro_AG <- data.frame(t(pro_AG_emissions))
colnames(pro_AG) <- row.names(pro_AG_emissions)
pro_AG$year <- seq(year0,last_year)
pro_AG_final <- pro_AG %>% tidyr::pivot_longer(cols=-c("year"),
                                               names_to = "name",
                                               values_to="ag_emiss")
# combine
pro_leaf_data %>% select(-X) %>%
  left_join(pro_BG_final, by = c("year", "name")) %>%
  left_join(pro_AG_final, by = c("year", "name")) -> pro_plot_data

# calculate total net biome production
pro_plot_data$tot_nbp <- pro_plot_data$ag_emiss + pro_plot_data$bg_emiss
# TODO
#check if this gets used anywhere
pro_plot_data$npp_rh <- pro_plot_data$NPP/pro_plot_data$Rh

# make long for plotting
pro_plot_data_long <- pro_plot_data %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp", "npp_rh"),
                      names_to="variable",
                      values_to="value")

pro_plot_data_long$scenario <- "protected"
