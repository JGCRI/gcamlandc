library(dplyr)
library(ggplot2)
library(ggsci)
library(zoo)


###
###
###
#Older script, the purpose of this code has migrated to plotting_and_hector_code.Rmd
#code reflects old terms (pre-code-clean-up-party)
###
###
###

#You've been warned.


################### read-in data ######################
data_dir <- 'results/plot_data_06Mar_2024/'

# # create if needed, but restart Rstudio to free up memory
# source('read_in_landcalcs.R') 

# # Not bad run-wise but does take a few min:
# bind_rows(read.csv(paste0(data_dir, 'ref_plot_data.csv'),
#                    stringsAsFactors = F) %>%
#             select(year, region, landleaf, name, scenario, tot_nbp) %>%
#             rename(value=tot_nbp)%>%
#             mutate(variable = 'tot_nbp'),
#           read.csv(paste0(data_dir, 'pro_plot_data.csv'),
#                    stringsAsFactors = F) %>%
#             select(year, region, landleaf, name, scenario, tot_nbp) %>%
#             rename(value=tot_nbp)%>%
#             mutate(variable = 'tot_nbp')
# ) ->
#   plot_data_all
# 
# write.csv(plot_data_all, paste0(data_dir, 'all_tot_nbp.csv'), row.names = F)

# Option to just read in once above has been executed:
plot_data_all <- read.csv(paste0(data_dir, 'all_tot_nbp.csv'),
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

# pull out emissions by land type (managed vs unmanaged):
#all leaf names
all_leaves <- unique(for_emissions$name)

managed_leaves <- grep("Unmanaged|RockIceDesert|Tundra", all_leaves, value=TRUE, invert=TRUE)
#removes Unmanaged, RockIceDesert, and Tundra 

unmanaged_leaves <- grep("Unmanaged|RockIceDesert|Tundra", all_leaves, value=TRUE)
#selects Unmanaged, RockIceDesert, and Tundra 

static_leaves <- grep("Urban|RockIceDesert|Tundra", all_leaves, value=TRUE)

# acs note - is no land change or C density change for rock ice desert, tundra, or 
# urban. Maybe want to pull them out separately as Gcam's static leaves?
# maybe just for sanity checking or something


##NOTE
#Dawn previously had pasture as part of unmanaged,
#here they are counted as managed land

#for managed leaves, create regional (comparing to gasser) and 
# global(comparing to GCP) nbp data sets
managed_data <- dplyr::filter(for_emissions,name %in% managed_leaves)

managed_data %>%
  group_by(region,scenario, year) %>% 
  summarise(nbp=sum(value)) %>%
  ungroup -> 
  reg_totals_mgd
reg_totals_mgd$mgd <- "managed"

managed_data %>% 
  group_by(scenario, year) %>%
  summarise(nbp=sum(value)) %>%
  ungroup -> 
  world_totals_mgd
world_totals_mgd$mgd <- "managed"

#same for unmanaged
unmgd_data <- dplyr::filter(for_emissions,!(name %in% managed_leaves))

unmgd_data %>%
  group_by(region,scenario, year) %>% 
  summarise(nbp=sum(value))%>% 
  ungroup -> 
  reg_totals_unmgd
reg_totals_unmgd$mgd <- "unmanaged"

unmgd_data %>% 
  group_by(scenario, year) %>% 
  summarise(nbp=sum(value)) %>% 
  ungroup->
  world_totals_unmgd
world_totals_unmgd$mgd <- "unmanaged"


# process static leaves similarly
# note that this includes leaves from both managed and unmanaged
for_emissions %>%
  filter(name %in% static_leaves) %>%
  group_by(scenario, year) %>% 
  summarise(nbp=sum(value)) %>% 
  ungroup %>%
  mutate(mgd = 'static') ->
  world_totals_static

# combine the managed and unmanaged leaves for global
# Right now, ignore static. Just there for reference if needed.
world_totals <- bind_rows(# world_totals_static, 
                          world_totals_mgd, world_totals_unmgd)


#protected vs reference (aka baseline), managed vs unmanaged
ggplot(data=world_totals,aes(x=year,y=nbp,color= mgd)) +
  geom_point()+
  ylab("Net Biome Production (Mt C/yr)") +
  facet_grid(mgd~scenario, scales = "free") + 
  theme_classic() -> fig

ggplot(data=world_totals,aes(x=year,y=nbp,color= scenario)) +
  geom_point()+
  facet_wrap(~mgd)+
  ylab("Net Biome Production (Mt C/yr)") +
  theme_classic() -> fig2

ggsave(filename="figures/coupled_vs_un_world_2010_mgd_comp.png", plot=fig, width=10, height=6)
ggsave(filename="figures/coupled_vs_un_world_2010_mgd_comp2.png", plot=fig2, width=10, height=6)




################### Comparison with Global Carbon Project 1 of 2 ###############
# Assuming that GCP total land flux is ONLY looking at managed land leaves for 
# both S_land and E_luc

# load the file passed from Dawn
gcp_data_dawn <- read.csv("GCP_data/nbp_gcp.csv")

# Load the file pulled directly from GCP:
read.csv('GCP_data/Global_Carbon_Budget_2022v1p0_historical_co2_tab.csv', stringsAsFactors = F) %>%
  select(year=Year, e_luc=land.use.change.emissions, s_land = land.sink, Units) %>%
  tidyr::replace_na(list(e_luc=0)) %>%
  # If we want to sum e_luc + s_land, uncomment from the `gather` to `ungroup` 
  # calls here and comment out the `mutate`:
  # gather(reporting_id, value, -year, -Units) %>%
  # group_by(year, Units) %>%
  # summarize(nbp = -sum(value, na.rm=T)) %>%
  # ungroup %>%
  # Following slide 56 of GCP_data/papers/GCP_CarbonBudget_2023_slides_v1.0-2-Alissa-Haward.pdf
  mutate(nbp = -(s_land - e_luc)) ->
  gcp_data
# note that for both in the above, we have to take the negative for the flux to be 
# going in the same direction as the offline model's


# Check if nbp = -(s_land-e_luc) from the raw GCP download agrees
# with file from Dawn:
print("Difference to Dawn's file:")
print(max(abs(gcp_data$nbp-gcp_data_dawn$nbp)))

# Label data and convert units
gcp_data$scenario <- "1. Global Carbon Project"
gcp_data$nbp_raw <- gcp_data$nbp*1000
gcp_data$nbp <- rollmean(gcp_data$nbp*1000,k=10,fill=NA)

#comparison with GCP
gcp_data %>%
  select(year, scenario, nbp) %>%
  full_join(world_totals[world_totals$mgd == "managed",]) ->
  world_totals_gcp

ggplot(data=dplyr::filter(world_totals_gcp,year<=2015),
       aes(x=year,y=nbp,colour=scenario, group = interaction(mgd, scenario)))+
  geom_line(size=1.5)+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr)") +
  xlab("Year") +
  ggtitle('world total (managed leaves) vs GCP total (all leaves). \nNBP is inaccurate label for uncoupled case. \nactually -LUC emissions in that case.') + 
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) -> 
  fig

ggsave(filename="figures/coupled_vs_un_world_2015_gcp_comparison_mgd_leaves.png", 
       plot=fig,
       width=8, height=4.5)

ggsave(filename="figures/coupled_vs_un_world_2015_gcp_comparison_mgd_leaves_w_ELUC.png", 
       plot=fig +  
         geom_line(data = dplyr::filter(gcp_data,year<=2015) %>%
                     mutate(mgd = 'E_LUC'),
                   mapping = aes(x = year, y = e_luc*1000, color =interaction(mgd, scenario))) +
         geom_line(data = dplyr::filter(world_totals_gcp,
                                        scenario == '2. uncoupled_luc_noNPP_noRh',
                                        year<=2015) %>%
                     mutate(mgd = 'E_LUC'),
                   mapping = aes(x = year, y = -nbp, color =interaction(mgd, scenario))),
       width=8, height=4.5)

#same as comparison with GCP above
#but with raw GCP nbp
gcp_data %>%
  select(year, scenario, nbp_raw) %>%
  mutate(nbp = nbp_raw) %>%
  full_join(world_totals[world_totals$mgd == "managed",]
  ) -> 
  raw_world_totals_gcp

ggplot(data=dplyr::filter(raw_world_totals_gcp,year<=2015),
       aes(x=year,y=nbp,colour=scenario))+
  geom_line(size=1.5)+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr) on managed leaves") +
  xlab("Year")+
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) -> fig

ggsave(filename="figures/coupled_vs_un_world_2015_raw_mgd.png",plot=fig,width=8,height=3.5)


################### Comparison with Global Carbon Project 2 of 2 ###############
# Assuming that GCP total land flux is land allocation, NPP, Rh effects on all 
# leaves

# acs note -  THINK this is how Dawn made their plot. Both based on plots and
# based on the comments in the 'regional' section below.
# This is more consistent with their pattern, if you consider that our coupled
# run should be more of a sink than Dawn's because our beta is MUCH higher
# compared to the  value of 0.15 (or 0.36) that we think DW ran with.
# 
# The uncoupled result _should_ be pretty similar to DW `baseline`.
# We did make some changes, including to reading in protected lands. 
# But our uncoupled result should be pretty close to Dawn's.

# clip the world totals values on the GCAM period years:
gcam_years <- c(1800, 1850, 1900, 1950, 1975, 1990, 2005, 2010, 2015)

#comparison with GCP
gcp_data %>%
  select(year, scenario, nbp) %>%
  full_join(world_totals %>% 
              group_by (scenario, year) %>% 
              summarize(nbp = sum(nbp)) %>%
              ungroup %>%
              filter(!(year %in% gcam_years))) ->
  world_totals_gcp2

ggplot(data=dplyr::filter(world_totals_gcp2,year<=2015),
       aes(x=year,y=nbp,colour=scenario, group = scenario))+
  geom_line(size=1.5)+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr)") +
  xlab("Year") +
  ggtitle('world total (all leaves) vs GCP total (all leaves). \nNBP is inaccurate label for uncoupled case. \nactually -LUC emissions in that case.') + 
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) -> 
  fig2

ggsave(filename="figures/coupled_vs_un_world_2015_gcp_comparison_all_leaves.png", 
       plot=fig2,
       width=8, height=4.5)

ggsave(filename="figures/coupled_vs_un_world_2015_gcp_comparison_all_leaves_clip_y_compare_DW.png", 
       plot=fig2+ylim(-2000, 1200),
       width=8, height=4.5)




################### climate data ######################

#TODO

#not sure if these two categories are still relevant
#seems that Dawn means "baseline" as in, no coupling with Hector???
ref_climate_data <- read.csv("data/uncoupled_protected_lands_Feb24/climate_data_UnCoupled_pro_newBeta_newQ10.csv")
pro_climate_data <- read.csv("data/coupled_protected_lands_Feb24/climate_data_Coupled_pro_newBeta_newQ10.csv")

ref_climate_data$scenario <- "uncoupled"
pro_climate_data$scenario <- "coupled"

ref_climate_data %>%
  bind_rows(pro_climate_data) %>%
  select(-X) -> climate_data

# base_climate_data$scenario <- "baseline"
# full_climate_data$scenario <- "fully-coupled"
# all_climate <- dplyr::bind_rows(base_climate_data,full_climate_data)

ggplot(data=dplyr::filter(climate_data,year<=2050),aes(x=year,y=value))+
  geom_line()+
  facet_grid(scenario ~ variable, scales="free_y")+
  theme_classic() -> fig
ggsave(filename=paste0("figures/coupled_vs_un_climate_data.png"),plot=fig,width=10,height=6)


break
################### Regional Plots ######################

#not sure if it makes sense to have individual and combined scenarios with managed and unmanaged
#keeping for now just in case
# reg_totals_unmgd <- reg_totals_unmgd %>% dplyr::filter(scenario=="fully-coupled") %>%
#   mutate(scenario="Unmanaged")
# reg_totals_all <- reg_totals %>% dplyr::filter(scenario=="fully-coupled") %>%
#   mutate(scenario="Unmanaged+Managed")
# reg_totals_mgd <- reg_totals_mgd %>% dplyr::filter(scenario=="fully-coupled") %>%
#   mutate(scenario="Managed")
# reg_totals_base <- reg_totals %>% dplyr::filter(scenario=="baseline") %>%
#   mutate(scenario="Baseline")
# reg_totals <- dplyr::bind_rows(reg_totals_base,reg_totals_mgd,reg_totals_all,reg_totals_unmgd)

reg_totals <- bind_rows(reg_totals_mgd, reg_totals_unmgd)

#step through all 32 regions in 4 chunks of 8
all_regions <- unique(reg_totals$region)
chunk1 <- all_regions[1:8] 
chunk2 <- all_regions[9:16]
chunk3 <- all_regions[17:24]
chunk4 <- all_regions[25:32]

ggplot(data=filter(reg_totals,region %in% chunk1,
                   year < 1955 & year > 1945),
       aes(x=year,y=nbp, color = mgd))+
  geom_point(aes(shape=scenario, size=2))+ facet_wrap(.~region,scales="free_y")+
  theme_classic() -> fig
ggsave(filename=paste0("figures/regional_data_chunk1.png"),plot=fig,width=10,height=6)

ggplot(data=filter(reg_totals,region %in% chunk2),
       aes(x=year,y=nbp,linetype=scenario, color = mgd))+
  geom_point()+ facet_wrap(~region,scales="free_y")+
  theme_classic() -> fig
ggsave(filename=paste0("figures/regional_data_chunk2.png"),plot=fig,width=10,height=6)

ggplot(data=filter(reg_totals,region %in% chunk3),
       aes(x=year,y=nbp,linetype=scenario, color = mgd))+
  geom_point()+ facet_wrap(~region,scales="free_y")+
  theme_classic() -> fig
ggsave(filename=paste0("figures/regional_data_chunk3.png"),plot=fig,width=10,height=6)

ggplot(data=filter(reg_totals,region %in% chunk4),
       aes(x=year,y=nbp,linetype=scenario, color = mgd))+
  geom_point()+ facet_wrap(~region,scales="free_y")+
  theme_classic() -> fig
ggsave(filename=paste0("figures/regional_data_chunk4.png"),plot=fig,width=10,height=6)


################### Ploting with a random subset of landleaves ######################
# 
# sample_leaf_names <- sample(plot_data_all$name,500)
# sample_leaf_data <- dplyr::filter(plot_data_all, name %in% sample_leaf_names)
# 
# 
# sample_nbp <- sample_leaf_data %>% dplyr::filter(variable=="tot_nbp")
# sample_density <- sample_leaf_data %>% dplyr::filter(variable %in% c("agCDensity","bgCDensity"))
# 
# sample_ag_emiss <- sample_leaf_data %>% dplyr::filter(variable=="ag_emiss")
# sample_bg_emiss <- sample_leaf_data %>% dplyr::filter(variable=="bg_emiss")
# sample_emiss_data <- bind_rows(sample_ag_emiss, sample_bg_emiss)
# 
# ggplot(data=sample_emiss_data,
#        aes(x=year, y=value, linetype=scenario))+
#   geom_line()+
#   ylab("Land Carbon Flux (Mt C/yr)")+
#   xlab("Year")+
#   facet_wrap(~variable,scales="free_y",nrow=4)+
#   theme_classic()#->fig
# #ggsave(filename="figures/sample_leaf_emissions.png",plot=fig,width=10,height=8)
# 
# ggplot(data=sample_density,
#        aes(x=year, y=value,linetype=scenario))+
#   geom_line()+
#   ylab("Carbon Density")+
#   xlab("Year")+
#   facet_wrap(~variable,scales="free_y",nrow=4)+
#   theme_classic()#->fig
# #ggsave(filename="figures/sample_leaf_C_density.png",plot=fig,width=10,height=8)
# 
# 
# ggplot(sample_leaf_data[sample_leaf_data$variable == "land_alloc",],
#        aes(x=year,y=value, linetype=scenario))+
#   geom_line()+
#   ylab("Land Allocation")+
#   xlab("Year")+
#   facet_wrap(~name,scales="free_y",nrow=4)+
#   theme_classic() #-> fig
#ggsave(filename="figures/sample_leaf_land_alloc.png",plot=fig,width=10,height=8)


################### Plotting with a single leaf ######################

# Steps through getting data and plotting for a single leaf
dplyr::filter(plot_data_all, name=="Canada_Grassland_NWTerr") -> debug_leaf

ggplot(data=debug_leaf,aes(x=year,y=value))+
  geom_line(aes(color=scenario))+
  facet_wrap(.~variable,scales="free_y")+
  theme_classic()


# TODO figure out better place to put this
read_luc_data <- function() {
  gcp_hist <- read.csv("~/Dropbox/Research/gcam_projects/LUC/data/gcp_historical.csv", header=TRUE, skip=14)
  gcp_mdrn <- read.csv("~/Dropbox/Research/gcam_projects/LUC/data/gcp_modern.csv",header=TRUE, skip=27)
  gcp_hist <- gcp_hist[101:nrow(gcp_hist),1:7]
  gcp_hist <- as_tibble(gcp_hist)
  gcp_mdrn <- as_tibble(gcp_mdrn)
  names(gcp_hist) <- c("year", "fossil", "luc", "atm", "ocean", "land", "imbalance")
  
  houghton_regions <- read.csv("~/Dropbox/Research/gcam_projects/LUC/data/houghton_regions.csv")
  
  nc <- nc_open("~/Dropbox/Research/gcam_projects/LUC/data/Gasser_et_al_2020_best_guess.nc")
  v4 <- nc$var[[4]]
  gasser_yr <- v4$dim[[5]]$vals
  gasser <- ncvar_get(nc,v4)
  
  varsize <- v4$varsize
  ndims   <- v4$ndims
  nt      <- varsize[ndims]  # Remember timelike dim is always the LAST dimension!
  
  gasser_keys <- c("Unknown"=0, "Sub-Saharan Africa"=1, "Latin America"=2,  "South and Southeast Asia"=3,
                   "North America"=4, "Europe"=5, "Former USSR"=6, "China"=7,
                   "North Africa and the Middle East"=8, "East Asia"=9,  "Oceania"=10)
  
  gcam_gasser <- list("Sub-Saharan Africa"=c("South Africa", "Africa_Eastern", "Africa_Southern", "Africa_Western"),
                      "Latin America"=c("Argentina", "Brazil", "Columbia", "Mexico", "South America_Northern", "South America_Southern", "Central America and the Caribbean"),
                      "South and Southeast Asia" = c("Pakistan", "India", "Indonesia", "South Asia", "Southeast Asia"),
                      "North America" = c("Canada", "USA"),
                      "Europe"= c("EU-12", "EU-15", "Europe Free Trade Association", "Europe Non-EU"),
                      "Former USSR" = c("Russia", "Europe Eastern", "Central Asia"),
                      "China" = c("China"),
                      "North Africa and the Middle East" = c("Middle East", "Africa_Northern"),
                      "East Asia" = c("Japan", "South Korea"),
                      "Oceania" = c("Australia_NZ"))
  
  gasser_luc <- rep(0,nt)
  for (i in 1:nt){
    gasser_luc[i] <- sum(gasser[,,,,i])
  }
  
  gasser_luc_reg <- array(0L,c(11,nt))
  
  for (i in 1:10){
    for (j in 1:nt){
      gasser_luc_reg[i,j] <- sum(gasser[,,,i,j])
    }
  }
  
  gasser_df <- tibble(luc = gasser_luc, year=gasser_yr)
  
  return(list("gasser"=gasser_df, "houghton"=houghton_regions, "gcp_hist"=gcp_hist, "gcp_mdrn"=gcp_mdrn, "gasser_reg"=gasser_luc_reg))
  
}



