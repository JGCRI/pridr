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
consolidated_state_level_data <- read.csv("US_50_state_DC_net_income_deciles_2011_2100_SSP.csv",
                                          stringsAsFactors= FALSE, check.names=FALSE) %>% 
  rename(category = Category,
         shares = pred_shares)

##aggregate population across states by year and ssp
##load in prebuilt dataframe from csv
#* if we need original data files we can add the loop back in
agg_pop <- read.csv("./consolidated_SSP_state_population_projections.csv",
			stringsAsFactors= FALSE, check.names=FALSE)


#Get ratio of gdp per capita between scenarios:

#load in USA GPD and POP projections as objects
##gdp units billions 2005 USD$
#*have to change gdp units to millions to match population
us_gdp_proj <- read.csv("./SspDb_country_data_2013-06-12.csv"
                        , stringsAsFactors= FALSE, check.names=FALSE) %>%
  filter(VARIABLE=="GDP|PPP", MODEL=="OECD Env-Growth", REGION=="USA") %>%
  gather(YEAR,GDP, starts_with("2")) %>%
  filter(YEAR >= 2010) %>%
  filter(YEAR <= 2100) %>%
  mutate(SCEN = substr(SCENARIO,1,4), gdp= GDP*1000) %>%
  select(YEAR,SCEN,gdp) %>%
  arrange(YEAR,SCEN)
#*have to deflate GDP to 1990 USD

#load in USA population
#*years 2010-2100
## units millions
us_pop_proj <- read.csv("./SspDb_country_data_2013-06-12.csv"
                        , stringsAsFactors= FALSE, check.names=FALSE) %>%
  filter(VARIABLE=="Population", MODEL=="IIASA-WiC POP", REGION=="USA") %>%
  gather(YEAR,POP, starts_with("2")) %>%
  filter(YEAR >= 2010) %>%
  filter(YEAR <= 2100) %>%
  mutate(SCEN = substr(SCENARIO,1,4)) %>%
  select(YEAR,SCEN,POP) %>%
  arrange(YEAR,SCEN)
#*if loaded correctly population for SSP4 in 2100 is ~364.5837 (million)


#create ratio (merge objects with left join) to scale GDP per capita for the SSPs
us_ratio <- left_join(us_gdp_proj, us_pop_proj, by= c("YEAR", "SCEN")) %>%
  mutate(us_gdp_pc = gdp/POP) %>%
  group_by(YEAR) %>%
  mutate(ratio = us_gdp_pc / us_gdp_pc[SCEN=="SSP2"]) %>%
  arrange(YEAR, SCEN) %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  rename(US_gdp=gdp, US_pop=POP)
#*if joined correctly ratio is ~0.9942036 for SSP3 in 2030



## will apply ratio for each year and scenario to state data for same scenario and year
## have baseline SSP2 GDP per capita data
##scale up or down for other SSPs based on ratio


#GDP Per Capita by State for baseline (SSP2) from GCAM
#*based on AEO growth rates
#* load in yearly gdp per capita data from gcam to get historical gdp per capita for 2011-2015

hist_gcam_gdp_pc <- read.csv("L100.pcGDP_thous90usd_state.csv", skip=1,
                             stringsAsFactors= FALSE, check.names=FALSE) %>%
  filter(year %in% 2011:2014)
#bind 2011-2014 to 5 year timestep gdp per capita projections
#* units converted to actual terms (from thousand USD)
gcam_usa_state_gdp_pc <- read.csv("C:/Users/casp111/OneDrive - PNNL/Documents/Projects/GODEEEP/Income_distributions/Data/GCAMUSA_GDPperCapita.csv"
                                  , stringsAsFactors= FALSE, check.names=FALSE) %>%
  select(state,year,value) %>%
  bind_rows(hist_gcam_gdp_pc) %>% 
  mutate(gdp_pc_1990usd = value*1000) %>% 
  filter(year>=2010) %>% 
  select(-value)

#create df to use to fill in historical years ssps
df <- data.frame(year = rep(c(2011,2012,2013,2014),5),
                 SCEN = c("SSP1","SSP2","SSP3","SSP4","SSP5")) %>% 
  arrange(year,SCEN)

#join national ratio with state gdp per capita
state_ssp_gdp_pc <- left_join(gcam_usa_state_gdp_pc,us_ratio,by= c("year"="YEAR")) 

growth_ratio <- state_ssp_gdp_pc %>% 
  mutate(ratio = ifelse(is.na(ratio),1,ratio)) %>% 
  select(state,year,SCEN,gdp_pc_1990usd,ratio) %>% 
  mutate(ssp_gdp_pc = gdp_pc_1990usd*ratio) %>%
  left_join(df, by=c("year")) %>% 
  mutate(sce = ifelse(year %in% 2011:2014,SCEN.y,SCEN.x)) %>% 
  select(-SCEN.x,-SCEN.y)


#solve for GDP
state_gdp <- inner_join(agg_pop, growth_ratio
                        , by= c("year","sce","state")) %>%
  mutate(gdp = ssp_gdp_pc * total_pop) %>%
  arrange(year, sce, state)
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
## calculating gdp and pop by deciles
state_decile_gdp <- left_join(state_gdp %>% filter(!year==2010) %>% 
                                select(-ratio,-gdp_pc_1990usd),
                              consolidated_state_level_data %>% 
                                filter(!sce=="Historical data") %>% 
                                bind_rows(ssp_2011_2015),
                              by= c("state_name"="state", "year", "sce")) %>%
  select(state, state_name, year, sce,category,shares,total_pop,gdp) %>%
  mutate(decile_pop = total_pop*0.1,
         decile_gdp = shares*gdp) %>%
  mutate(decile_gdp_pc=decile_gdp/decile_pop)
#*gdp in actual units
#*income shares calculated using forecasted scaled-down national gini to get the level of inequality

