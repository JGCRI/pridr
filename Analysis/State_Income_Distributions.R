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

#source from repository code
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

setwd("../../Input_Data/")

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


#load in state gini csv and use to compute components and shares

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
  adjust_negative_predicted_features(grouping_variables = c("state","year"),min_lowest_feature_val = 0.00001) %>%
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
  adjust_negative_predicted_features(grouping_variables = c("state","year","sce"),min_lowest_feature_val = 0.00001) %>%
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
##load in prebuilt dataframe from csv
#* if we need original data files we can add the loop back in
agg_pop <- read.csv("./consolidated_SSP_state_population_projections.csv",
			stringsAsFactors= FALSE, check.names=FALSE)


#Get ratio of gdp per capita between scenarios:

#load in USA GPD and POP projections as objects
##gdp units billions $
#*have to change gdp units to millions to match population
us_gdp_proj <- read.csv("./SspDb_country_data_2013-06-12.csv"
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
us_pop_proj <- read.csv("./SspDb_country_data_2013-06-12.csv"
                        , stringsAsFactors= FALSE, check.names=FALSE) %>%
  filter(VARIABLE=="Population", MODEL=="IIASA-WiC POP", REGION=="USA") %>%
  gather(YEAR,POP, starts_with("2")) %>%
  filter(YEAR >= 2015) %>%
  filter(YEAR <= 2100) %>%
  mutate(SCEN = substr(SCENARIO,1,4)) %>%
  select(YEAR,SCEN,POP) %>%
  arrange(YEAR,SCEN)
#*if loaded correctly population for SSP4 in 2100 is ~364.5837 (million)


#create ratio (merge objects with left join) to scale GDP per capita for the SSPs
us_ratio <- left_join(us_gdp_proj, us_pop_proj, by= c("YEAR", "SCEN")) %>%
  mutate(gdp_per_cap = gdp/POP) %>%
  group_by(YEAR) %>%
  mutate(ratio = gdp_per_cap / gdp_per_cap[SCEN=="SSP2"]) %>%
  arrange(YEAR, SCEN) %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  rename(US_gdp=gdp, US_pop=POP)
#*if joined correctly ratio is ~0.9942036 for SSP3 in 2030



## will apply ratio for each year and scenario to state data for same scenario and year
## have baseline SSP2 GDP per capita data
##scale up or down for other SSPs based on ratio


#GDP Per Capita by State for SSP2
#loading gdp per capita baseline scenario
state_gdp_pc_baseline <- read.csv("./GDP_percapita_US_states.csv"
                                  , stringsAsFactors= FALSE, check.names=FALSE) %>%
  arrange(year, state) %>%
  rename(YEAR=year, STATE=state)
#*50 states plus DC

#calculating state gdp per capita growth rates from AEO data
#then apply them to ratio
#*AEO gdp pc only grow every 5 years, so only taking 5 year intervals
state_growth_rate <- state_gdp_pc_baseline %>% filter(grepl("(5|0)$",YEAR)) %>%
  arrange(STATE,YEAR) %>%
  group_by(STATE) %>%
  mutate(gr = (GDP_PC_2011_USD/lag(GDP_PC_2011_USD))) %>%
  mutate(gr= replace_na(gr,1.0)) %>%
  ungroup()
#*should be 918 rows after taking 5 year intervals
#* gr= 1.0580574 for AK in 2030 if equation works
#*2015 is first year so calculated growth rate is NA, change to 1 so the calculation with ratio works


#join national ratio with state gdp per capita growth rates
## national ratio to scale SSPs is the same across states in each year and SSP
## state growth rates vary across states and years, but are the same in the SSPs
growth_ratio <- left_join(state_growth_rate, us_ratio, by= c("YEAR")) %>%
  mutate(growth_ratio= gr*ratio) %>%
  mutate(gdp_pc= growth_ratio * GDP_PC_2011_USD) %>%
  select(-Population)
## have to set SSP2 growth rates/growth ratio to 1 because it is our base data and doesn't need to be scaled
growth_ratio$growth_ratio[growth_ratio$SCEN=="SSP2"] <- 1.0
#* AK growth_ratio=1.0657045 in 2045 for SSP3
#* AK gdp_pc=64454.84 in 2045 for SSP3


#solve for GDP
state_gdp <- inner_join(agg_pop, growth_ratio
                        , by= c("year"="YEAR","sce"="SCEN","state"="STATE")) %>%
  mutate(gdp = gdp_pc * total_pop) %>%
  arrange(year, sce, state)
#* should have 2754 rows (51 sates*3 SSPs*18 time intervals)
#* if joined correctly FL gdp is 703586382834 for SSP3 in 2030
#* gdp in actual dollar units
#* population in actual units

#add state names column to state_gdp so it can be joined to state_level_forecast
#*have to use ifelse because DC isn't in state.names data set
names(state.name) <- state.abb
?state
state_gdp$state_name <- ifelse(state_gdp$state== "DC"
                               , "District of Columbia"
                               , state.name[state_gdp$state])



#join gdp to shares based on gini

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
#*income shares calculated using forecasted scaled-down national gini to get the level of inequality

