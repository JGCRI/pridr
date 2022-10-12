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
  theme(axis.text.x= element_text(angle=0,hjust=0.5))+
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  theme(legend.key.width=unit(2,"cm"))

setwd("./Input_Data/")

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
#using tax adjusted data
state_gini<- read.csv("acs_net_income_state_gini.csv", stringsAsFactors = FALSE) %>%
  rename(year = year)

state_level_deciles <- read.csv("acs_net_income_state_gini.csv", stringsAsFactors = FALSE) %>%
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
  adjust_negative_predicted_features(grouping_variables = c("state","year"),min_lowest_feature_val = 0.0095) %>%
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
  adjust_negative_predicted_features(grouping_variables = c("state","year","sce"), min_lowest_feature_val = 0.0095) %>%
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

#replace part at beginning with just file for consolidated state level data
#* this is not in the repository yet?
consolidated_state_level_data <- read.csv("~/Projects/GODEEEP/Income_distributions/Data/US_50_state_DC_net_income_deciles_2011_2100_SSP.csv",
                                          stringsAsFactors= FALSE, check.names=FALSE) %>%
  rename(category = Category,
         shares = pred_shares)

#aggregate population across states by year and ssp
#* load in prebuilt dataframe from csv
#* if we need original data files we can add the loop back in
agg_pop <- read.csv("consolidated_SSP_state_population_projections.csv",
			stringsAsFactors= FALSE, check.names=FALSE)


#Get ratio of gdp per capita between scenarios for scaling
#* will apply ratio for each year and scenario to state data for same scenario and year
#* SSP2 is considered our baseline GDP per capita


#load in USA GPD and POP projections as objects
##gdp units billions 2005 USD$
#*have to change gdp units to millions to match population
#* gdp_deflator(1990, 2012) = 0.7258882
us_gdp <- read.csv("SspDb_country_data_2013-06-12.csv"
                   , stringsAsFactors= FALSE, check.names=FALSE) %>%
  filter(VARIABLE=="GDP|PPP", REGION=="USA") %>%
  gather(YEAR,GDP, starts_with("2")) %>%
  filter(YEAR >= 2010) %>%
  filter(YEAR <= 2100) %>%
  mutate(SCEN = substr(SCENARIO,1,4), gdp= GDP*0.7258882*1000,
         YEAR = as.numeric(YEAR)) %>%
  select(YEAR,SCEN,MODEL,gdp) %>%
  filter(SCEN %in% c("SSP2","SSP3","SSP5"))

#load in USA population
#*years 2010-2100
## units millions
us_pop <- read.csv("SspDb_country_data_2013-06-12.csv"
                   , stringsAsFactors= FALSE, check.names=FALSE) %>%
  filter(VARIABLE=="Population",REGION=="USA",MODEL %in% us_gdp$MODEL) %>%
  gather(YEAR,POP, starts_with("2")) %>%
  filter(YEAR >= 2010) %>%
  filter(YEAR <= 2100) %>%
  mutate(SCEN = substr(SCENARIO,1,4),
         YEAR = as.numeric(YEAR)) %>%
  select(YEAR,SCEN,MODEL,POP) %>%
  filter(SCEN %in% c("SSP2","SSP3","SSP5"))


#scale GDP per Capita for the SSPs
#* constant turns defualt options on or off
#* default SSPs: SSP2-OECD, SSP3-PIK, SSP5-OECD
#* when TRUE only default options are used for deciles (one set of projections)
#* when FALSE all SSP models are used to produce three sets of decile projections (set of projections for each model)
#* construct 5 year average annual growth rate for comparision between SSP Models

SCALE_TO_DEFAULT=TRUE

if(SCALE_TO_DEFAULT){
   us_ratio <- left_join(us_gdp, us_pop, by= c("YEAR", "SCEN","MODEL")) %>%
    mutate(us_gdp_pc = gdp/POP) %>%
    group_by(YEAR,MODEL) %>%
    mutate(ratio = us_gdp_pc / us_gdp_pc[SCEN=="SSP2"]) %>%
    ungroup() %>%
    group_by(MODEL,SCEN) %>%
    mutate(five_yr_gr =((us_gdp_pc/lag(us_gdp_pc))^(1/5)-1)*100) %>%
    ungroup() %>%
    mutate(YEAR = as.numeric(YEAR)) %>%
    rename(US_gdp=gdp, US_pop=POP) %>%
    filter(MODEL=="OECD Env-Growth" & SCEN %in% c("SSP2","SSP5")|
             MODEL=="PIK GDP-32" & SCEN=="SSP3")}else{
               us_ratio <- left_join(us_gdp, us_pop, by= c("YEAR", "SCEN","MODEL")) %>%
                 mutate(us_gdp_pc = gdp/POP) %>%
                 group_by(YEAR,MODEL) %>%
                 mutate(ratio = us_gdp_pc / us_gdp_pc[SCEN=="SSP2"]) %>%
                 ungroup() %>%
                 group_by(MODEL,SCEN) %>%
                 mutate(five_yr_gr =((us_gdp_pc/lag(us_gdp_pc))^(1/5)-1)*100) %>%
                 ungroup() %>%
                 mutate(YEAR = as.numeric(YEAR)) %>%
                 rename(US_gdp=gdp, US_pop=POP)
             }


#GDP Per Capita by State for baseline (SSP2) from GCAM
#* based on AEO growth rates
#* load in yearly gdp per capita data from gcam to get historical gdp per capita for 2011-2015
hist_gcam_gdp_pc <- read.csv("GCAMUSA_hist_pcGDP_thous90usd_state.csv", skip=1,
                             stringsAsFactors= FALSE, check.names=FALSE) %>%
  filter(year %in% 2011:2014)
#bind 2011-2014 to 5 year timestep gdp per capita projections
#* units converted to actual terms (from thousand USD)
gcam_usa_state_gdp_pc <- read.csv("GCAMUSA_GDPperCapita.csv"
                                  , stringsAsFactors= FALSE, check.names=FALSE) %>%
  select(state,year,value) %>%
  bind_rows(hist_gcam_gdp_pc) %>%
  mutate(gdp_pc_1990usd = value*1000) %>%
  filter(year>=2010) %>%
  select(-value)

#create df to use to fill in historical years ssps
df <- data.frame(year = rep(c(2011,2012,2013,2014),9),
                 SCEN = rep(c("SSP2","SSP3","SSP5"),3)) %>%
  arrange(year,SCEN) %>%
  mutate(MODEL = rep(c("IIASA GDP","OECD Env-Growth","PIK GDP-32"),12))

#join national ratio with state gdp per capita
state_ssp_gdp_pc <- left_join(gcam_usa_state_gdp_pc,us_ratio,by= c("year"="YEAR"))

growth_ratio <- state_ssp_gdp_pc %>%
  mutate(ratio = ifelse(is.na(ratio),1,ratio)) %>%
  mutate(ssp_gdp_pc = gdp_pc_1990usd*ratio) %>%
  left_join(df %>% filter(MODEL %in% state_ssp_gdp_pc$MODEL), by=c("year")) %>%
  mutate(sce = ifelse(year %in% 2011:2014,SCEN.y,SCEN.x),
         MODEL = ifelse(year %in% 2011:2014,MODEL.y,MODEL.x)) %>%
  select(-SCEN.x,-SCEN.y,-MODEL.y,-MODEL.x)

#solve for GDP
state_gdp <- inner_join(agg_pop, growth_ratio
                        , by= c("year","sce","state")) %>%
  mutate(gdp = ssp_gdp_pc * total_pop)
#* gdp in actual dollar units
#* population in actual units

#add state names column to state_gdp so it can be joined to state_level_forecast
#*have to use ifelse because DC isn't in state.names data set
names(state.name) <- state.abb
?state
state_gdp$state_name <- ifelse(state_gdp$state== "DC"
                               , "District of Columbia"
                               , state.name[state_gdp$state])

#join 2011-2015 data
consolidated_state_level_data %>%
  filter(year %in% 2011:2015) %>%
  mutate(sce="SSP1") %>%
  bind_rows(consolidated_state_level_data %>%
              filter(year %in% 2011:2015) %>%
              mutate(sce="SSP2"),
            consolidated_state_level_data %>%
              filter(year %in% 2011:2015) %>%
              mutate(sce="SSP3"),
            consolidated_state_level_data %>%
              filter(year %in% 2011:2015) %>%
              mutate(sce="SSP4"),
            consolidated_state_level_data %>%
              filter(year %in% 2011:2015) %>%
              mutate(sce="SSP5")) -> ssp_2011_2015

#joining state level gdp and population with income shares by decile
#* calculating gdp and pop by deciles
state_decile_gdp <- left_join(state_gdp %>% filter(!year==2010) %>%
                                select(-ratio,-gdp_pc_1990usd),
                              consolidated_state_level_data %>%
                                filter(!sce=="Historical data") %>%
                                bind_rows(ssp_2011_2015),
                              by= c("state_name"="state", "year", "sce")) %>%
  select(state,state_name,year,MODEL,sce,category,shares,total_pop,gdp) %>%
  mutate(decile_pop = total_pop*0.1,
         decile_gdp = shares*gdp) %>%
  mutate(decile_gdp_pc=decile_gdp/decile_pop)
#*gdp in actual units
#*income shares calculated using forecasted scaled-down national gini to get the level of inequality

