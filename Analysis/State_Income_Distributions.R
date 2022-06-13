#creates and applies pca model using gini's

#Load libraries
library(assertthat)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggsci)
library(zoo)
library(knitr)
library(data.table)
library(assertthat)
library(rpart)

#check working directory
getwd()
setwd("C:/Users/casp111/OneDrive - PNNL/Documents/Projects/GODEEEP/Income_distributions/Data")


#this sources data from repository
source("../Repository/GCAM_Income_Distributions/Code/pca_ineq.R")

# First get US data

#generates income dist
#Scheme for plots
##makes scheme for plots to format plots same
scheme_basic <- theme_bw() +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12)) +
  theme(axis.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 11, face = "bold")) +
  theme(plot.title = element_text(size = 12, face = "bold", vjust = 1)) +
  theme(plot.subtitle = element_text(size = 9, face = "bold", vjust = 1))+
  theme(strip.text = element_text(size = 7))+
  theme(strip.text.x = element_text(size = 11, face = "bold"))+
  theme(strip.text.y = element_text(size = 11, face = "bold"))+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 11))+
  theme(legend.title = element_text(size = 11,color = "black",face="bold"))+
  theme(axis.text.x= element_text(angle=60,hjust=1))+
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  theme(legend.key.width=unit(2,"cm"))

setwd("../Data/")

#income deciles data

US_agg_data <- read.csv("GCAM_32_regions_Income_deciles_SSPs_1967_2100_Rao_et_al_downscaled.csv"
                        , stringsAsFactors = FALSE) %>% filter(GCAM_region_ID ==1, sce=="Historical data") %>%
  rename(Category = category)

Wider_data_full <- read.csv("Wider_aggregated_deciles.csv",stringsAsFactors = FALSE) %>%
  arrange(year)

#*aggregate components, we're only using the first two
pc_loadings_matrix <- get_PCA_loadings(Wider_data_full,category_col = "Category",value_col = "Income..net.")

#*mean and standard deviation for each decile
pc_center_sd <- get_sd_center(Wider_data_full , category_col = "Category", value_col = "Income..net.")

#computing coefficients a and b for PC1 and PC2 respectively

#*the function needs the dataset, center and stds, loading matrix, create output value (shares), declare grouping variables
compute_components(US_agg_data,center_and_scaler_data = pc_center_sd, pc_loadings = pc_loadings_matrix, value_col = "shares",
                   grouping_variables = c("GCAM_region_ID","year"),category_col = "Category")->Coefficients_US


#can use this to get other 5 year intervals

### interpolation function
approx_fun <- function(year, value, rule = 1) {
  assert_that(is.numeric(year))
  assert_that(is.numeric(value))
  
  if(rule == 1 | rule == 2) {
    tryCatch(stats::approx(as.vector(year), value, rule = rule, xout = year, ties = mean)$y,
             error = function(e) NA)
    
  } else {
    stop("Use fill_exp_decay_extrapolate!")
  }
}


#take out gini and replace with GDP, correct year to start with 2020 to fit data

state_gini<- read.csv("State_level_GINI.csv", stringsAsFactors = FALSE) %>%
  rename(year = year)

state_level_deciles <- read.csv("State_level_GINI.csv", stringsAsFactors = FALSE) %>%
  rename(year = year) %>%
  left_join(Coefficients_US, by = c("year")) %>%
  mutate(Component2 = ifelse(is.na(Component2),
                             approx_fun(year,Component2,rule = 2),Component2),
         Component1 = (gini*29.717083)-11.48152) %>%
  select(-GCAM_region_ID) %>%
  distinct() %>%
  get_deciles_from_components(pc_loadings = pc_loadings_matrix,
                              center_and_scaler = pc_center_sd,category_col = "Category",
                              grouping_variables = c("state","year")) %>%
  adjust_negative_predicted_features(grouping_variables = c("state","year")) %>%
  mutate(category = Category) %>%
  compute_gini_deciles(inc_col = "pred_shares",
                       grouping_variables = c("state","year")) %>%
  left_join(state_gini %>%
              select(state, year, gini),
            by = c("state","year"))


g <- ggplot(data=state_level_deciles %>% filter(Category=="d10"), aes(x=year,y=gini,group=state))+
  geom_line(color="grey")

g + scheme_basic

g <- ggplot(data=state_level_deciles, aes(x=gini, y=output_name))+
  geom_point()+
  xlab("Historical GINI")+
  ylab("GINI based on calculated deciles")

g + scheme_basic

US_agg_data %>% filter(year==2015)-> US_2015_data

#scaler ratio for gini between ssps
US_forecast_data <- read.csv("GCAM_32_regions_Income_deciles_SSPs_1967_2100_Rao_et_al_downscaled.csv"
                             , stringsAsFactors = FALSE) %>% filter(GCAM_region_ID ==1, year > 2015) %>%
  rename(Category = category) %>%
  arrange(year) %>%
  mutate(gini_2015= unique(US_2015_data$gini),
         gini_scaler = gini/gini_2015)


state_level_deciles %>% filter(year==2015) %>%
  select(-year) %>%
  mutate(reg="US") %>%
  left_join(US_forecast_data %>% select(year,sce,gini_scaler) %>% distinct() %>% mutate(reg="US")) %>%
  select(state,gini,year,sce,gini_scaler,Component2) %>%
  mutate(gini=gini*gini_scaler,
         gini= pmin(gini,0.93),
         Component1 = (gini*29.717083)-11.48152) %>%
  get_deciles_from_components(pc_loadings = pc_loadings_matrix,center_and_scaler = pc_center_sd,category_col = "Category",
                              grouping_variables = c("state","year","sce")) %>%
  adjust_negative_predicted_features(grouping_variables = c("state","year","sce")) %>%
  mutate(category= Category) %>%
  compute_gini_deciles(inc_col = "pred_shares",grouping_variables = c("state","year","sce")) %>%
  rename(gini=output_name,shares=pred_shares) %>%
  select(-Component2,-Component1,-share_of_richer_pop,-Category,-score)->state_level_forecast


##state level forecast provides income decile share of income(state gdp) based on gini coefficient
state_level_forecast %>%
  bind_rows(state_level_deciles %>% select(-gini,-Component2,-Component1,-share_of_richer_pop,-Category,-score) %>%
              rename(gini=output_name,
                     shares=pred_shares)%>% mutate(sce="Historical data"))%>%  distinct()->consolidated_state_level_data

write.csv(consolidated_state_level_data, "US_50_states_DC_net_income_deciles_1917_2050_SSP.csv", row.names = FALSE)


#add gdp and pop data to state_level_forecast
# Script to aggregate population by state and year for SSP2, SSP3 & SSP5

##* Data gathered from Jiang et al. pop projections 
#* link: https://zenodo.org/record/3956412#.YogQ2KjMI2z
#Load data from file
##aggregate population across states by year and ssp

#*separately looped SSPs because it doesn't take long and can be combined
#*creating a for loop for all ssps was becoming too time intensive

#check working directory
getwd()
setwd("C:/Users/casp111/OneDrive - PNNL/Documents/Projects/GODEEEP/Income_distributions/Data")

#add in direction to folders we need
#create for loop to fill in file names and load csv
#load in csv using loop, pastes in name object, check.names takes out x in front of years
#bind rows after loading in
#create objects to store file location and names


#SSP2
list.dirs("./SSP2")->directories2
file_list <- list.files(path = directories2, pattern = "*_proj_pop.csv", full.names = T)
dfs <- list() #creates empty data frame

#for loop
for(i in 1:length(file_list)) {
  
  # read in the ith csv
  temp_df <- read.csv(file = file_list[[i]],
                      stringsAsFactors = F,check.names=FALSE) %>%  
    gather(year, population, starts_with("2")) %>%
    select(state,year,population) %>%
    group_by(state,year) %>%
    mutate(population=sum(population)) %>%
    ungroup()  %>%
    distinct() %>%              
    mutate(sce="SSP2")
  
  dfs[[i]] <- temp_df
  
}

SSP2<- do.call(rbind, dfs)
#*if aggregating correctly AL pop=4782789.0 for 2010 in ssp2

#SSP3
list.dirs("./SSP3")->directories3
file_list <- list.files(path = directories3, pattern = "*_proj_pop.csv", full.names = T)
dfs <- list() #creates empty data frame

#for loop
for(i in 1:length(file_list)) {
  
  # read in the ith csv
  temp_df <- read.csv(file = file_list[[i]],
                      stringsAsFactors = F,check.names=FALSE) %>%  
    gather(year, population, starts_with("2")) %>%
    select(state,year,population) %>%
    group_by(state,year) %>%
    mutate(population=sum(population)) %>%
    ungroup()  %>%
    distinct() %>%  
    mutate(sce="SSP3")
  
  dfs[[i]] <- temp_df
  
}

SSP3<- do.call(rbind, dfs)
#*if aggregating correctly AL pop=3917760 for 2100 in SSP3

#SSP5
list.dirs("./SSP5")->directories5
file_list <- list.files(path = directories5, pattern = "*_proj_pop.csv", full.names = T)
dfs <- list() #creates empty data frame

#for loop
for(i in 1:length(file_list)) {
  
  # read in the ith csv
  temp_df <- read.csv(file = file_list[[i]],
                      stringsAsFactors = F,check.names=FALSE) %>%  
    gather(year, population, starts_with("2")) %>%
    select(state,year,population) %>%
    group_by(state,year) %>%
    mutate(population=sum(population)) %>%
    ungroup()  %>%
    distinct() %>%  
    mutate(sce="SSP5")
  
  dfs[[i]] <- temp_df
  
}

SSP5<- do.call(rbind, dfs)
#*if aggregating correctly AK pop=726220.1 for 2100 in SSP5

#join ssps
#*has data for every year for 3 ssps in actual population units
#*should have 13923 rows
agg_pop <- rbind(SSP2,SSP3,SSP5) %>% 
  arrange(year,sce,state) %>%
  rename(total_pop=population) %>% 
  mutate(year=as.numeric(year))
#rearrange columns
agg_pop <- agg_pop[,c("year","sce","state","total_pop")]



#Get ratio of gdp per capita between scenarios:


#load in USA GPD and POP projections as objects
##gdp units billions $
#*have to change gdp units to millions to match population
us_gdp_proj <- read.csv("C:/Users/casp111/OneDrive - PNNL/Documents/Projects/GODEEEP/Income_distributions/Data/SspDb_country_data_2013-06-12.csv"
                        , stringsAsFactors= FALSE, check.names=FALSE) %>%
  filter(VARIABLE=="GDP|PPP", MODEL=="OECD Env-Growth", REGION=="USA") %>%
  gather(YEAR,GDP, starts_with("2")) %>%
  filter(YEAR >= 2015) %>%
  filter(YEAR <= 2100) %>%
  mutate(SCEN = substr(SCENARIO,1,4), gdp= GDP*1000) %>%
  select(YEAR,SCEN,gdp) %>%
  arrange(YEAR,SCEN)
#*if loaded correctly GDP for SSP5 in 2100 is 120517863 (million)

#load in USA population
#*years 2015-2100
## units millions
us_pop_proj <- read.csv("C:/Users/casp111/OneDrive - PNNL/Documents/Projects/GODEEEP/Income_distributions/Data/SspDb_country_data_2013-06-12.csv"
                        , stringsAsFactors= FALSE, check.names=FALSE) %>%
  filter(VARIABLE=="Population", MODEL=="IIASA-WiC POP", REGION=="USA") %>%
  gather(YEAR,POP, starts_with("2")) %>%
  filter(YEAR >= 2015) %>%
  filter(YEAR <= 2100) %>%
  mutate(SCEN = substr(SCENARIO,1,4)) %>%
  select(YEAR,SCEN,POP) %>%
  arrange(YEAR,SCEN)
#*if loaded correctly population for SSP4 in 2100 is ~364.5837 (million)


#create ratio (merge objects with left join)
us_ratio <- left_join(us_gdp_proj, us_pop_proj, by= c("YEAR", "SCEN")) %>%
  mutate(gdp_per_cap = gdp/POP) %>%
  group_by(YEAR) %>%
  mutate(ratio = gdp_per_cap / gdp_per_cap[SCEN=="SSP2"]) %>%
  arrange(YEAR, SCEN) %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  rename(US_gdp=gdp, US_pop=POP)
#*if joined correctly ratio is ~0.9942036 for SSP3 in 2030



## will apply ratio for each year and scenario to state data for same scenario and year
## have baseline SSP2 GDP data,
##scale up or down for other SSPs based on ratio


#GDP Per Capita by State for SSP2
#loading gdp per capita baseline scenario
state_gdp_pc_baseline <- read.csv("C:/Users/casp111/OneDrive - PNNL/Documents/Projects/GODEEEP/Income_distributions/DATA/GDP_percapita_US_states.csv"
                                  , stringsAsFactors= FALSE, check.names=FALSE) %>%
  arrange(year, state) %>%
  rename(YEAR=year, STATE=state)

#*50 states plus DC


#scale gdp per capita up or down using ratio
#total_pop units= actual population
state_gdp_pc_all <- inner_join(us_ratio, state_gdp_pc_baseline,by= "YEAR") %>%
  mutate(gdp_pc= ratio * GDP_PC_2011_USD) %>%
  select(-Population)
#*if joined correctly FL gdp_pc is ~32446.53 for SSP3 in 2030
#* not using "Population" column because that it is from a different SSP2 projection
#* we want "total_pop" because it is based on the paper (link at top) projections


#solve for GDP
state_gdp <- inner_join(agg_pop, state_gdp_pc_all
                        , by= c("year"="YEAR","sce"="SCEN","state"="STATE")) %>%
  mutate(gdp = gdp_pc * total_pop) %>%
  arrange(year, sce, state)
#* should have 2754 rows (51 sates*3 SSPs*18 time intervals)
#* if joined correctly FL gdp is 661228953577 for SSP3 in 2030
#* gdp = 32446.53gdp_pc * 20379035population = 661228953577
#* gdp in actual dollar units
#* population in actual units

#add state names column to state_gdp so it can be joined to state_level_forecast
#*have to use ifelse because DC isn't in state.names data set
names(state.name) <- state.abb
?state
state_gdp$state_name <- ifelse(state_gdp$state== "DC"
                               , "District of Columbia"
                               , state.name[state_gdp$state])


#join to shares based on gini
#join 2015 data to state_level_forecast
consolidated_state_level_data %>%
  filter(year==2015) %>%
  mutate(sce="SSP1") %>%
  bind_rows(consolidated_state_level_data %>%
              filter(year==2015) %>%
              mutate(sce="SSP2"),
            consolidated_state_level_data %>%
              filter(year==2015) %>%
              mutate(sce="SSP3"),
            consolidated_state_level_data %>%
              filter(year==2015) %>%
              mutate(sce="SSP4"),
            consolidated_state_level_data %>%
              filter(year==2015) %>%
              mutate(sce="SSP5")) -> ssp_2015

#joining state level gdp and population with income shares by decile
## calculating gdp and pop by deciles
state_level_gdp <- left_join(state_gdp, state_level_forecast %>% bind_rows(ssp_2015),
                             by= c("state_name"="state", "year", "sce")) %>%
  select(state, state_name, year, sce, shares,category,total_pop,gdp) %>%
  mutate(decile_pop = total_pop*0.1, decile_gdp= shares*gdp) %>%
  group_by(year, sce) %>%
  mutate(decile_gdp_pc = decile_gdp/decile_pop) %>%
  arrange(decile_gdp_pc) %>%  #arranging from poorest to richest
  mutate(cumulative_pop = cumsum(decile_pop),
         cumulative_gdp= cumsum(decile_gdp)) %>%
  ungroup()
#*gdp in actual units
#* equations correct if maryland decile_gdp=4245836726 for d1 in year 2060 SSP5
#* maryland decile_gdp_pc=4921.73 for d1 in year 2060 SSP5

##income shares calculated using forecasted scaled-down national gini to get the level of inequality


#*after previous step now have income and population shares for Lorenz curves
#want national lorenz curve and where states fall on it

#create data frame for Lorenz Curves

state_level_gdp %>% group_by(sce,year) %>%
  arrange(decile_gdp_pc) %>%
  mutate(nat_gdp= sum(decile_gdp),
         nat_pop=sum(decile_pop),
         share_of_population= cumulative_pop/nat_pop,
         share_of_gdp = cumulative_gdp/nat_gdp) %>%
  ungroup() -> lorenz_curve_state_gdp

#richest and poorest states (by GDP per capita)


#Lorenz Curves using state gdp
#*title is fixed

L <- ggplot(data=lorenz_curve_state_gdp %>% filter(year %in% c(2030,2050,2100)), aes(x=share_of_population, y=share_of_gdp,color=sce))+
  geom_line(size=1.0)+
  scale_color_npg()+
  facet_wrap(~year)+
  geom_abline(slope=1,color="grey",linetype="dashed",size=1.3)+
  xlab("Share of Population")+
  ylab("Share of GDP")+
  ggtitle("Lorenz curves using state level projections of income distributions", subtitle = "1st Income Decile For Select States Displayed")+
  theme(legend.title = element_blank())+
  geom_point(data=lorenz_curve_state_gdp %>% filter(year %in% c(2030,2050,2100),
                                                    state_name %in% c("New York"),
                                                    category=="d1"),aes(x=share_of_population, y=share_of_gdp,shape="New York"),color="black",size=2.2)+
  geom_point(data=lorenz_curve_state_gdp %>% filter(year %in% c(2030,2050,2100),
                                                    state_name %in% c("West Virginia"),
                                                    category=="d1"),aes(x=share_of_population, y=share_of_gdp,shape="West Virginia"),color="black",size=2.2)+
  scale_shape(solid = FALSE)
L+scheme_basic+theme(legend.title = element_blank())

L <- ggplot(data=lorenz_curve_state_gdp %>% filter(year %in% c(2030,2050,2100)), aes(x=share_of_population, y=share_of_gdp,color=sce))+
  geom_line(size=1.0)+
  scale_color_npg()+
  facet_wrap(~year)+
  geom_abline(slope=1,color="grey",linetype="dashed",size=1.3)+
  xlab("Share of Population")+
  ylab("Share of GDP")+
  ggtitle("Lorenz curves using state level projections of income distributions", subtitle = "10th Income Decile For Select States Displayed")+
  geom_point(data=lorenz_curve_state_gdp %>% filter(year %in% c(2030,2050,2100),
                                                    state_name %in% c("New York"),
                                                    category=="d10"),aes(x=share_of_population, y=share_of_gdp,shape="New York"),color="black",size=2.2)+
  geom_point(data=lorenz_curve_state_gdp %>% filter(year %in% c(2030,2050,2100),
                                                    state_name %in% c("West Virginia"),
                                                    category=="d10"),aes(x=share_of_population, y=share_of_gdp,shape="West Virginia"),color="black",size=2.2)+
  scale_shape(solid = FALSE)
L+scheme_basic+theme(legend.title = element_blank())





#palma ratio (d10/d1+D2+D3+d4), richest 10% share of income/poorest 40% share of income
palma_ratio_data <- lorenz_curve_state_gdp %>% select(state,state_name,sce,year,shares,category) %>% distinct() %>%
  spread(category, shares) %>%
  mutate(palma_ratio= d10/(d1+d2+d3+d4)) %>%
  select(state,state_name,sce,year,palma_ratio) %>%
  distinct()

#load map libraries
library(sf)
library(sp)
library(viridis) #good for nicer colors in ggplot
library(purrr)
#install.packages("maps")
library(maps)
#install.packages("usmapdata")
#install.packages("C:/Users/casp111/Downloads/usmap_0.6.0.tar.gz", repos = NULL, type = "source")
library(usmap)
library(stringr)
library(ggplot2)
#*not including dc on map

plot_usmap(regions=c("states"), data=palma_ratio_data %>% filter(year %in% c(2100)), values = "palma_ratio")+
  facet_wrap(~sce)+
  scheme_basic+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "left")+
  scale_fill_viridis_c(option="plasma",name = "Palma Ratio",
                       guide= guide_colourbar(barwidth = 1.5)) 

plot_usmap(regions=c("states"), data=palma_ratio_data %>% filter(sce=="SSP3", year %in% c(2030,2050,2100)), values = "palma_ratio")+
  facet_wrap(~year)+
  scheme_basic+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "left")+
  scale_fill_viridis_c(option="plasma",name = "Palma Ratio",
                       guide= guide_colourbar(barwidth = 1.5)) 

#income distribution plots
income_dist_data <- state_level_gdp %>% filter(year %in% c(2030,2050,2100), state_name %in% c("New York", "West Virginia")) %>% 
  mutate(id = paste0(state_name, sce)) #for line plot


USA_agg_data <- read.csv("Aggregate_US_data.csv", stringsAsFactors = FALSE) 

USA_agg_data %>% filter(year==2100) %>% mutate(state_name="USA National") %>% 
  bind_rows(income_dist_data %>% filter(year==2100)) %>% rename(Region=state_name) -> income_dist_data_us_join

g <- ggplot(data=income_dist_data_us_join,
            aes(x=factor(category,
                         levels= c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")),
                y=shares,group=id))+
  geom_point(aes(color=Region))+
  geom_line(aes(size=Region, color=Region))+
  facet_wrap(~sce)+
  xlab("Population decile")+
  ylab("Income shares")+
  ylim(0,0.7)+
  scale_linetype_manual(values= c("solid","dotdash","solid"))+
  scale_color_manual(values=c("blue", "grey", "orange"))+
  scale_size_manual(values = c(1,0.8,1))
g+scheme_basic


#gdp per capita distribution
income_dist_data <- state_level_gdp %>% filter(year %in% c(2030,2050,2100), state_name %in% c("New York", "West Virginia")) %>% 
  mutate(id = paste0(state_name, sce))

g <- ggplot(data=income_dist_data %>% filter(year==2100), 
            aes(x=factor(category, 
                         levels= c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")),
                y=decile_gdp_pc,group=id,color=sce))+
  geom_point(size=2)+
  geom_line(size=1.3)+
  facet_wrap(~state_name)+
  xlab("Population decile")+
  ylab("GDP per capita")+
  #ylim(0,0.7)+
  scale_color_manual(name = "Region", values=c("blue", "dark grey", "orange"))


g+scheme_basic


