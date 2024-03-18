
plot_data_all <- read.csv("plot_data_14Mar_2024/all_tot_nbp.csv",
                          stringsAsFactors = F)

################### Plotting ######################


# Reshape data a bit
plot_data_all %>%
  select(year, region, landleaf, name, scenario, variable, value) %>%
  # Clarify that the uncoupled model's "nbp" is actually just the 
  # land use change emissions of the old GCAM way:
  # delta_land * constant carbon density
  # in a full GCAM run, this land use change emission passes to hector, which
  # simulates global NPP and Rh and combines the 3 processes to get NBP.
  # In the coupled run, all three of these processes are occuring at the land 
  # leaf level, so it is a true NBP (or at least more true than for the uncoupled).
  mutate(scenario = if_else(scenario == 'uncoupled',
                            '2. uncoupled_luc_noNPP_noRh',
                            '3. coupled_luc_NPP_Rh'))   -> 
  for_emissions
rm(plot_data_all)

for_emissions %>%
  group_by(scenario, year) %>% 
  summarise(nbp=sum(value)) %>% 
  ungroup() -> world_totals

world_totals$nbp <- rollmean(world_totals$nbp, k=5, fill=NA)

#comparison with GCP
gcp_data %>%
  select(year, scenario, nbp) %>%
  full_join(world_totals) %>%
  mutate(nbp = -(nbp)) -> #[world_totals$mgd == "managed",]
  world_totals_gcp

world_totals_gcp$beta <- "Beta = 0.22"

ggplot(data=dplyr::filter(world_totals_gcp,year<=2020),
       aes(x=year,y=nbp,colour=scenario, group = scenario))+
  geom_hline(yintercept = 0) +
  geom_line(size=1.5, aes(linetype = beta))+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr)") +
  xlab("Year") + 
  ggtitle('GCAM-Hector (all leaves) vs GCP (all leaves).
          \nNBP is inaccurate label for uncoupled case which is -LUC_e') + 
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14))



# Option to just read in once above has been executed:
# alt_plot_data_all <- read.csv(paste0(data_dir, 'all_tot_nbp.csv'),
#                           stringsAsFactors = F)
# 
# ################### Plotting ######################
# 
# 
# # Reshape data a bit
# alt_plot_data_all %>%
#   select(year, region, landleaf, name, scenario, variable, value) %>%
#   # Clarify that the uncoupled model's "nbp" is actually just the 
#   # land use change emissions of the old GCAM way:
#   # delta_land * constant carbon density
#   # in a full GCAM run, this land use change emission passes to hector, which
#   # simulates global NPP and Rh and combines the 3 processes to get NBP.
#   # In the coupled run, all three of these processes are occuring at the land 
#   # leaf level, so it is a true NBP (or at least more true than for the uncoupled).
#   mutate(scenario = if_else(scenario == 'uncoupled',
#                             '2. uncoupled_luc_noNPP_noRh',
#                             '3. coupled_luc_NPP_Rh'))   -> 
#   alt_for_emissions
# rm(alt_plot_data_all)

alt_for_emissions %>%
  group_by(scenario, year) %>% 
  summarise(nbp=sum(value)) %>% 
  ungroup() -> alt_world_totals

alt_world_totals$nbp <- rollmean(world_totals$nbp, k=5, fill=NA)

#comparison with GCP
gcp_data %>%
  select(year, scenario, nbp) %>%
  full_join(alt_world_totals) %>%
  mutate(nbp = -(nbp)) -> #[world_totals$mgd == "managed",]
  alt_world_totals_gcp

alt_world_totals_gcp$beta <- "Beta = 0.55"

ggplot(data=dplyr::filter(alt_world_totals_gcp,year<=2020),
       aes(x=year,y=nbp,colour=scenario, group = scenario))+
  geom_hline(yintercept = 0) +
  geom_line(size=1.5, aes(linetype = beta))+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr)") +
  xlab("Year") + 
  ggtitle('GCAM-Hector (all leaves) vs GCP (all leaves).
          \nNBP is inaccurate label for uncoupled case which is -LUC_e') + 
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14))

#comparison with multiple beta value
alt_world_totals_gcp %>%
  bind_rows(world_totals_gcp) -> beta_comp

ggplot(data=dplyr::filter(beta_comp,year<=2020),
       aes(x=year,y=nbp,colour=scenario))+
  geom_hline(yintercept = 0) +
  geom_line(size=1.5, aes(linetype = beta))+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr)") +
  xlab("Year") + 
  ggtitle('GCAM-Hector (all leaves) vs GCP (all leaves).
          \nNBP is inaccurate label for uncoupled case which is -LUC_e') + 
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14))
