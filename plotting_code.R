library(dplyr)
library(ggplot2)
library(ggsci)
library(zoo)

################### read-in data if needed ######################

#combine outputs from read_in_landcalcs.R
plot_data_all <- dplyr::bind_rows(ref_plot_data_long,pro_plot_data_long)

#option to export and read this in to save time
write.csv(plot_data_all, "plot_data_all.csv")
plot_data_all <- read.csv("plot_data_all.csv")

#working with the full data set can be prohibtively slow
#below is just the data from North America
#(Canda, USA, and Mexico)

plot_data_all <- read.csv("NorthAmerica_data_all.csv")

################### climate data ######################

#TODO bring in climate data separately from read_in_landcalcs
#comparison of climate data
base_climate_data$scenario <- "baseline"
full_climate_data$scenario <- "fully-coupled"
all_climate <- dplyr::bind_rows(base_climate_data,full_climate_data)

ggplot(data=dplyr::filter(climate_data,year<=2050),aes(x=year,y=value))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic() -> fig
ggsave(filename=paste0("climate_data.png"),plot=fig,width=10,height=6)

################### BASIC? ######################








################### Comparison with Global Carbon Project ######################

###NOTE
#change path if needed
###
gcp_data <- read.csv("nbp_gcp.csv")
gcp_data$scenario <- "Global Carbon Project"
gcp_data$nbp_raw <- gcp_data$nbp*1000
gcp_data$nbp <- rollmean(gcp_data$nbp*1000,k=10,fill=NA)

#pull out emissions by land type (managed vs unmanaged)
plot_data_all %>%
  select(year, region, name, scenario, variable, value) %>%
  filter(variable == "tot_nbp") -> for_emissions

#all leaf names
all_leaves <- unique(for_emissions$name)

managed_leaves <- grep("Unmanaged|RockIceDesert|Tundra", all_leaves, value=TRUE, invert=TRUE)
#removes Unmanaged, RockIceDesert, and Tundra


unmanaged_leaves <- grep("Unmanaged|RockIceDesert|Tundra", all_leaves, value=TRUE)
#selects Unmanaged, RockIceDesert, and Tundra

##NOTE
#Dawn previously had pasture as part of unmanaged,
#here they are counted as managed land

#for managed leaves, create regional and global nbp data sets
managed_data <- dplyr::filter(for_emissions,name %in% managed_leaves)
managed_data %>% group_by(region,scenario, year) %>% summarise(nbp=sum(value)) -> reg_totals_mgd
#only variable in the dataframe is nbp (see creation of for_emissions)
reg_totals_mgd$mgd <- "managed"
managed_data %>% group_by(scenario, year) %>% summarise(nbp=sum(value)) -> world_totals_mgd
world_totals_mgd$mgd <- "managed"

#same for unmanaged
unmgd_data <- dplyr::filter(for_emissions,!(name %in% managed_leaves))
unmgd_data %>% group_by(region,scenario, year) %>% summarise(nbp=sum(value)) -> reg_totals_unmgd
reg_totals_unmgd$mgd <- "unmanaged"
unmgd_data %>% group_by(scenario, year) %>% summarise(nbp=sum(value)) -> world_totals_unmgd
world_totals_unmgd$mgd <- "unmanaged"

#combine for global
world_totals <- bind_rows(world_totals_mgd, world_totals_unmgd)

#protected vs reference (aka baseline), managed vs unmnaged
ggplot(data=world_totals,aes(x=year,y=nbp,color= mgd))+
  geom_point()+
  ylab("LUC Emissions (Mt C/yr") +
  facet_grid(mgd~scenario, scales = "free") + 
  theme_classic() -> fig

ggsave(filename="figures/world_2010_mgd_comp.png", plot=fig, width=10, height=6)


#comparison with GCP, unmanaged only
gcp_data %>%
  select(year, scenario, nbp) %>%
  full_join(world_totals[world_totals$mgd!= "unmanaged",]
  ) -> world_totals_gcp

ggplot(data=dplyr::filter(world_totals_gcp,year<=2015),
       aes(x=year,y=nbp,colour=scenario))+
  geom_line(size=1.5)+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr)") +
  xlab("Year")+
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) -> fig

ggsave(filename="figures/world_2015_gcp_comparison.png", plot=fig, width=8, height=3.5)


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

ggsave(filename="world_2015_raw.png",plot=fig,width=8,height=3.5)




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

ggplot(data=filter(reg_totals,region %in% chunk1),
       aes(x=year,y=nbp,linetype=scenario, color = mgd))+
  geom_point()+ facet_wrap(~region,scales="free_y")+
  theme_classic() -> fig
ggsave(filename=paste0("regional_data_chunk1.png"),plot=fig,width=10,height=6)

ggplot(data=filter(reg_totals,region %in% chunk2),
       aes(x=year,y=nbp,linetype=scenario, color = mgd))+
  geom_point()+ facet_wrap(~region,scales="free_y")+
  theme_classic() -> fig
ggsave(filename=paste0("regional_data_chunk2.png"),plot=fig,width=10,height=6)

ggplot(data=filter(reg_totals,region %in% chunk3),
       aes(x=year,y=nbp,linetype=scenario, color = mgd))+
  geom_point()+ facet_wrap(~region,scales="free_y")+
  theme_classic() -> fig
ggsave(filename=paste0("regional_data_chunk3.png"),plot=fig,width=10,height=6)

ggplot(data=filter(reg_totals,region %in% chunk4),
       aes(x=year,y=nbp,linetype=scenario, color = mgd))+
  geom_point()+ facet_wrap(~region,scales="free_y")+
  theme_classic() -> fig
ggsave(filename=paste0("regional_data_chunk4.png"),plot=fig,width=10,height=6)


#sample leaves

#not sure if this works
# test_leaves <- sample(row.names(AG_emissions),500)
# grep("Forest",test_leaves,value=TRUE)

sample_leaves <- c("Grassland_NWTerr", "OtherArableLand_LBalkash",
                   "biomassTree_BrahmaniR_IRR_lo", "FruitsTree_IndCstS_IRR_lo",
                   "OilCropTree_OrinocoR_RFD_hi", "FodderHerbC4_DanubeR_RFD_lo")

sample_leaf_data <- dplyr::filter(plot_data_long, landleaf %in% sample_leaves)

plot_data_long2 <- sample_leaf_data %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp"),
                      names_to="variable",
                      values_to="value")

sample_emiss_data <- sample_leaf_data[1:6] %>% dplyr::filter(variable=="tot_nbp") %>% select(-c("variable"))
sample_density <- sample_leaf_data %>% dplyr::filter(variable %in% c("agCDensity","bgCDensity"))

sample_ag_emiss <- sample_leaf_data %>% dplyr::filter(variable=="ag_emiss") %>% select(-c("variable"))
sample_bg_emiss <- sample_leaf_data %>% dplyr::filter(variable=="bg_emiss") %>% select(-c("variable"))


ggplot(data=sample_emiss_data,aes(x=year,y=value, linetype=scenario))+
  geom_line()+
  ylab("Land Carbon Flux (Mt C/yr)")+
  xlab("Year")+
  facet_wrap(~variable,scales="free_y",nrow=4)+
  theme_classic()->fig
ggsave(filename="sample_leaf_emissions.png",plot=fig,width=10,height=8)


ggplot(data=sample_ag_emiss,aes(x=year,y=value)) +#,linetype=scenario))+
  geom_line()+
  ylab("Aboveground Emissions")+
  xlab("Year")+
  facet_wrap(~name,scales="free_y",nrow=4)+
  theme_classic()->fig
ggsave(filename="sample_leaf_ag_emiss.png",plot=fig,width=10,height=8)

ggplot(data=sample_density[sample_density$variable == "agCDensity",],aes(x=year,y=value)) +#,linetype=scenario))+
  geom_line()+
  ylab("Aboveground Density")+
  xlab("Year")+
  facet_wrap(~name,scales="free_y",nrow=4)+
  theme_classic()->fig
ggsave(filename="sample_leaf_ag_density.png",plot=fig,width=10,height=8)

ggplot(data=sample_density[sample_density$variable == "bgCDensity",],aes(x=year,y=value)) +#,linetype=scenario))+
  geom_line()+
  ylab("Belowground Density")+
  xlab("Year")+
  facet_wrap(~name,scales="free_y",nrow=4)+
  theme_classic()->fig
ggsave(filename="sample_leaf_bg_density.png",plot=fig,width=10,height=8)

ggplot(data=sample_bg_emiss,aes(x=year,y=value)) +#,linetype=scenario))+
  geom_line()+
  ylab("Belowground Emissions")+
  xlab("Year")+
  facet_wrap(~name,scales="free_y",nrow=4)+
  theme_classic()->fig
ggsave(filename="sample_leaf_bg_emiss.png",plot=fig,width=10,height=8)

ggplot(data=plot_data_long[plot_data_long$variable == "land_alloc",],aes(x=year,y=value)) +#,linetype=scenario))+
  geom_line()+
  ylab("Land Allocation")+
  xlab("Year")+
  facet_wrap(~name,scales="free_y",nrow=4)+
  theme_classic() #-> fig
#ggsave(filename="sample_leaf_land_alloc.png",plot=fig,width=10,height=8)

ggplot(data=filter(sample_density,name %in% c("China_Grassland_IndusR", "India_Shrubland_BrahmaniR",
                                              "USA_CornC4_GreatBasin_RFD_hi", "South America_Northern_Wheat_SAmerCstNE_RFD_lo",
                                              "Russia_Tundra_BalticSea")),
       aes(x=year,y=value,linetype=scenario))+
  geom_line(size=1.5)+
  ylab("Carbon Density")+
  xlab("Year")+
  facet_grid(name~variable,scales="free_y")+
  theme_classic()->fig
ggsave(filename="sample_leaf_emissions.png",plot=fig,width=10,height=8)



ggplot(data=dplyr::filter(plot_data_long,name==sample_leaves[[2]]),aes(x=year,y=value))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic()

ggsave(filename="sample_leaf_emissions.png",plot=fig,width=8,height=8)


debug_leaf_long <- debug_leaf %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp"),
                      names_to="variable",
                      values_to="value")

ggplot(data=debug_leaf_long,aes(x=year,y=value))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic()



for (leaf in sample_leaves){
  ggplot(data=dplyr::filter(plot_data_long,name==leaf),aes(x=year,y=value,linetype=scenario))+
    geom_line()+
    facet_wrap(~variable,scales="free_y")+
    theme_classic() -> fig
  ggsave(filename=paste0("leaf_plot_",leaf,".png"),plot=fig,width=10,height=6)
  
}


-> fig

ggsave(filename="single_leaf_Africa_Eastern_Soybean_RiftValley_RFD_lo_test.png",plot=fig,width=10,height=6)





################### current plots above this line ######################

#plot_data_long <- plot_data_first %>% tidyr::pivot_longer(cols=c("land_alloc","tot_nbp"),names_to="variable",
#                                                    values_to="value")



plot_data_long <- plot_data %>% filter(name %in% leaf_set) %>% tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                                                                                          "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp"),names_to="variable",
                                                                                   values_to="value")

plot_data_long <- debug_leaf %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity",
                             "bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh",
                             "litter","bg_emiss",
                             "ag_emiss","tot_nbp"),
                      names_to="variable", values_to="value")

ggplot(data=plot_data_long,aes(x=year,y=value))+
  geom_line()+
  facet_wrap(variable~name,scales="free_y")+
  theme_classic()

ggplot(data=us_data,aes(x=year,y=value))+
  geom_line()+
  facet_wrap(variable~name,scales="free_y")+
  theme_classic()

all_leaves <- unique(output[["leaf_data"]]$name)

#leaf_set <- sample(all_leaves,500)

table(grepl('RFD_hi',us_data$name))
table(grepl('IRR_lo',us_data$name))

patterns <- c("_RFD_hi", "_IRR_lo")

filter(us_data, grepl(paste(patterns, collapse="|")), name) -> short_us_data
filter(us_data, grepl("Nelson", name)) -> Nelson_data

Nelson_data %>%
  filter(grepl("Unmanaged", name)) -> um_Nelson

ggplot(data=um_Nelson, aes(x=year,y=value))+
  geom_line()+
  facet_wrap(name~variable,scales="free_y")+
  ggtitle("Nelson River Basin, Unmanaged, Protected") +
  theme_classic() -> fig

ggsave(filename="NelsonRiver_Unmanaged_Protected.png",plot=fig,width=12,height=8)

ggplot(data=dplyr::filter(plot_data_long,name %in% leaves),aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  facet_wrap(name~variable,scales="free_y")+
  theme_classic()

# select 10 sample land leaves to plot all variables for in 10 separate plots
ggplot(data=dplyr::filter(plot_data_long,name==leaf_set[[375]],year<=1850),aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic()#-> fig

ggsave(filename="single_leaf_Africa_Eastern_Soybean_RiftValley_RFD_lo_test.png",plot=fig,width=10,height=6)


# select 6 sample land leaves to plot key variables for: land alloc, agDensity, bgDensity, agEmiss, bgEmiss

ggplot(data=dplyr::filter(plot_data,variable %in% c("land_alloc",
                                                    "tot_nbp")),
       aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  facet_grid(variable~landleaf,scales="free_y")+
  theme_classic()


# select 6 sample land leaves to plot key variables for: land alloc, agEmiss, bgEmiss, totalEmiss

ggplot(data=dplyr::filter(plot_data,variable %in% c("agCDensity","bgCDensity","land_alloc")),aes(x=year,y=value))+
  geom_line()+
  facet_grid(variable~landleaf,scales="free_y")+
  theme_classic()






############################################

# *************** TESTING US DATA

# GETTING DEBUG DATA FOR US LEAF
dplyr::filter(plot_data,name=="Canada_Grassland_NWTerr") -> debug_leaf

dplyr::filter(full_leaf_params,name=="USA_OtherArableLand_UsaPacNW") -> debug_full_leaf_params
dplyr::filter(base_leaf_params,name=="USA_OtherArableLand_UsaPacNW") -> debug_base_leaf_params
debug_base_leaf_params
debug_full_leaf_params

us_data <- dplyr::filter(plot_data_long,region=="USA")

us_data %>%
  tidyr::pivot_wider(id_cols=c("year","name", #"scenario",
                               "region","tot_nbp"),
                     names_from=scenario,
                     values_from = tot_nbp) %>%
  mutate(diff=`fully-coupled`-baseline) %>%
  dplyr::filter(year==1975) -> us_data2

dplyr::filter(us_test,`fully-coupled`>=10)

all_leaves <- unique(us_data$name)

i <- 1
first_idx <- i*12-11
curr_leaves <- all_leaves[first_idx:(i*12)]
#curr_leaves <- all_leaves[first_idx:length(all_leaves)]

curr_leaves <- sample(all_leaves,50)

sample(all_leaves,50)

curr_leaves <- grep("Vegetables", all_leaves,value=TRUE)
#curr_leaves <- grep("UnmanagedPasture", curr_leaves,value=TRUE,invert=TRUE)
length(curr_leaves)
curr_leaves <- curr_leaves[1:16]
ggplot(data=dplyr::filter(us_data,name %in% curr_leaves),aes(x=year,y=tot_nbp,linetype=scenario))+
  geom_line()+
  facet_wrap(~name,scales="free_y")+
  theme_classic()

test_leaves <- sample(unique(plot_data_long$landleaf), 3)

ggplot(data=dplyr::filter(plot_data_long,name %in% test_leaves),aes(x=year,y=value))+
  geom_line()+
  facet_wrap(variable~name,scales="free_y")+
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
