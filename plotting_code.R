library(dplyr)
library(ggplot2)
library(ggsci)
library(zoo)

################### read-in data ######################
data_dir <- 'plot_data_06Mar_2024/'

# # create if needed, but restart Rstudio to free up memory
# source('read_in_landcalcs.R') 

# Not bad run-wise but does take a few min:
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


#pull out emissions by land type (managed vs unmanaged)
# acs - now does nothing with way changed file io
plot_data_all %>%
  select(year, region, landleaf, name, scenario, variable, value) %>%
  filter(variable == "tot_nbp") -> for_emissions
rm(plot_data_all)

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

#for managed leaves, create regional and global nbp data sets
managed_data <- dplyr::filter(for_emissions,name %in% managed_leaves)

managed_data %>%
  group_by(region,scenario, year) %>% 
  summarise(nbp=sum(value)) %>%
  ungroup -> 
  reg_totals_mgd
#only variable in the dataframe is nbp (see creation of for_emissions)
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
  ungroup->
  world_totals_static

#combine for global
world_totals <- bind_rows(world_totals_static, 
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
ggsave(filename="figures/coupled_vs_un_world_2010_mgd_comp2.png", plot=fig, width=10, height=6)




################### Comparison with Global Carbon Project ######################

###NOTE
#change path if needed
###
gcp_data <- read.csv("nbp_gcp.csv")
gcp_data$scenario <- "Global Carbon Project"
gcp_data$nbp_raw <- gcp_data$nbp*1000
gcp_data$nbp <- rollmean(gcp_data$nbp*1000,k=10,fill=NA)

#comparison with GCP, unmanaged only
gcp_data %>%
  select(year, scenario, nbp) %>%
  full_join(world_totals[world_totals$mgd!= "unmanaged",]) -> world_totals_gcp

ggplot(data=dplyr::filter(world_totals_gcp,year<=2015),
       aes(x=year,y=nbp,colour=scenario))+
  geom_line(size=1.5)+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr) - world total vs GCP - unmganaged leaves") +
  xlab("Year")+
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) -> fig

ggsave(filename="figures/coupled_vs_un_world_2015_gcp_comparison_unmgd_leaves.png", 
       plot=fig,
       width=8, height=3.5)


#same as comparison with GCP above
#but with raw GCP nbp
gcp_data %>%
  select(year, scenario, nbp_raw) %>%
  mutate(nbp = nbp_raw) %>%
  full_join(world_totals[world_totals$mgd!= "unmanaged",]
  ) -> raw_world_totals_gcp

ggplot(data=dplyr::filter(raw_world_totals_gcp,year<=2015),
       aes(x=year,y=nbp,colour=scenario))+
  geom_line(size=1.5)+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr)") +
  xlab("Year")+
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) -> fig

ggsave(filename="figures/coupled_vs_un_world_2015_raw.png",plot=fig,width=8,height=3.5)




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


################### climate data ######################

#TODO

#not sure if these two categories are still relevant
#seems that Dawn means "baseline" as in, no coupling with Hector???
ref_climate_data <- read.csv("Feb24_set2of5/climate_data_UnCoupled_pro_newBeta_newQ10.csv")
pro_climate_data <- read.csv("Feb24_set3of5/climate_data_Coupled_pro_newBeta_newQ10.csv")

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

