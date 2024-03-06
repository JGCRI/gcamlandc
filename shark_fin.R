
library(tidyr)
library(dplyr)
library(ggplot2)

# all_regions <- unique(plot_data_all$region)
# chunk1 <- all_regions[1:8]
# chunk1_data <- plot_data_all %>%
#   filter(region %in% chunk1)
# 
# write.csv(chunk1_data, "chunk1_data.csv")
chunk1_data <- read.csv("chunk1_data.csv")



unique(chunk1_data$name)

chunk1_USA_oilcrop <- chunk1_data %>%
  #select(-X) %>%
  filter(grepl("OilCrop", name),
         region == "USA")

ggplot(data=chunk1_USA_oilcrop,aes(x=year,y=value))+
  geom_point(aes(color=scenario))+
  facet_wrap(.~variable,scales="free_y")+
  theme_classic()

chunk1_USA_oilcrop %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  filter(year > 1925,
         npp_rh > 1.06) -> chunk1_USA_oilcrop_wide

chunk1_USA_oilcrop_wide %>%
  filter(grepl("Texas", name)) -> texas_test

ggplot(data=texas_test,aes(x=year,y=land_alloc))+
  geom_point(aes(color=name)) +
  facet_grid(name~scenario) +
  theme_classic()

texas_test %>%
  pivot_longer(cols = c("land_alloc", "agCDensity", "bgCDensity",
                        "agCarbon", "bgCarbon", "NPP", "Rh", "litter",
                        "bg_emiss", "ag_emiss", "tot_nbp", "npp_rh"),
               names_to = "variable", values_to = "value") -> long_tx_test



ggplot(data=long_tx_test,aes(x=year,y=value))+
  geom_point(aes(shape=name, color=scenario)) +
  facet_grid(variable~scenario, scales="free_y") +
  theme_classic()

chunk1_forest <- chunk1_data %>%
  filter(grepl("Forest", name))

ggplot(data=chunk1_forest,aes(x=year,y=value))+
  geom_point(aes(color=scenario))+
  facet_wrap(.~variable,scales="free_y")+
  theme_classic()


#pull out emissions by land type (managed vs unmanaged)
#chunk1_data %>%
plot_data_all %>%
  select(year, region, landleaf, name, scenario, variable, value) -> for_emissions

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
managed_data %>% group_by(region,scenario, year) %>%
  filter(variable == "tot_nbp") %>%
  summarise(nbp=sum(value)) -> reg_totals_mgd
#only variable in the dataframe is nbp (see creation of for_emissions)
reg_totals_mgd$mgd <- "managed"
managed_data %>% group_by(scenario, year) %>%
  filter(variable == "tot_nbp") %>%
  summarise(nbp=sum(value)) -> world_totals_mgd
world_totals_mgd$mgd <- "managed"

#same for unmanaged
unmgd_data <- dplyr::filter(for_emissions,!(name %in% managed_leaves))
unmgd_data %>% group_by(region,scenario, year) %>%
  filter(variable == "tot_nbp") %>%
  summarise(nbp=sum(value)) -> reg_totals_unmgd
reg_totals_unmgd$mgd <- "unmanaged"
unmgd_data %>% group_by(scenario, year) %>%
  filter(variable == "tot_nbp") %>%
  summarise(nbp=sum(value)) -> world_totals_unmgd
world_totals_unmgd$mgd <- "unmanaged"

for_emissions %>% group_by(scenario, year) %>%
  filter(variable == "tot_nbp") %>%
  summarise(nbp=sum(value)) -> world_totals_scenario


#combine for global
world_totals <- bind_rows(world_totals_mgd, world_totals_unmgd)

# world_totals %>%
#   filter(scenario == "coupled") %>%
#   mutate(scenario = mgd) -> world_totals

#combined <- bind_rows(world_totals, world_totals_scenario)


unmgd_data %>%
  filter(variable == "land_alloc") %>%
  mutate(mgd = "unmgd")-> unmgd_alloc
managed_data %>%
  filter(variable == "land_alloc") %>%
  mutate(mgd = "mgd")-> mgd_alloc

chunk1_land_alloc <- bind_rows(unmgd_alloc, mgd_alloc)

ggplot(data=chunk1_land_alloc[chunk1_land_alloc$region == "Africa_Eastern",],
       aes(x=year,y=value,color= mgd)) +
  geom_point()+ geom_line(aes(group = name)) +
  ylab("Land Allocation") +
  facet_grid(.~scenario, scales = "free") + 
  theme_classic()

world_totals_scenario$nbp <- rollmean(world_totals_scenario$nbp,k=10,fill=NA)

gcp_data %>%
  select(year, scenario, nbp_raw) %>%
  mutate(nbp = nbp_raw) %>%
  full_join(world_totals_scenario) -> world_totals_gcp


#coupled vs uncoupled (aka baseline), managed vs unmanaged
ggplot(data= world_totals_gcp,aes(x=year,y=nbp,color= scenario)) +
  geom_line(size=1.5)+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr)") +
  xlab("Year")+
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) -> fig

ggsave(filename="figures/c_vs_uc_test4.png", plot=fig, width=10, height=6)

# gcam <- read.csv("luc_queries.csv")
# 
# gcam_long <- gcam %>%
#   group_by(scenario, region, LandLeaf) %>%
#   pivot_longer(cols = c(X1980:X2100)) %>%
#   mutate(year = as.numeric(str_replace_all(name, "X", ""))) %>%
#   select(-X, -name)
# 
# gcam_long[gcam_long$scenario == "Reference",]$scenario <- "GCAMv6_baseline"
# gcam_long[gcam_long$scenario ==  "Protected_Lands",]$scenario <- "GCAMv6_protected"

# gcam_long %>% group_by(scenario, year) %>%
#   summarise(nbp=sum(value)) -> gcam_scenarios
# 
# all_scenarios <- bind_rows(world_totals_gcp, gcam_scenarios)
# 
# #protected vs reference (aka baseline), managed vs unmanaged
# ggplot(data= all_scenarios[all_scenarios$year <= 2010,],
#        aes(x=year,y=nbp,color= scenario)) +
#   geom_line(size=1.5)+
#   scale_color_uchicago()+
#   ylab("Net Biome Production (Mt C/yr)") +
#   xlab("Year")+
#   theme_classic() +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=14)) -> fig
# 
# ggsave(filename="figures/test4.png", plot=fig, width=10, height=6)
