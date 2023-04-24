##Workflow to generate state level deciles of the net-income distribution
#for all 50 states and DC.
#Authors- Kanishka B. Narayan, Kelly C. Casper, Brian C. ONeill & Stephanie Waldhoff


#This script generates all state level income distribution data for the US.
#This was specifically developed for the GODEEEP project.


#Load libraries
library(pridr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(assertthat)
library(data.table)
library(parallel)
library(ggsci)
devtools::load_all()

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

# Helper function to calculate gini from quintiles
compute_gini_quintiles <- function(df){

  df %>%
    distinct() %>%
    mutate(share_of_richer_pop = if_else(Quintile== "q1",0.8,
                                         if_else(Quintile== "q2", 0.6,
                                                 if_else(Quintile== "q3", 0.4,
                                                         if_else(Quintile== "q4", 0.2,0))))) %>%
    mutate(score = (value_after_state_tax/100) *(0.2+ (2*share_of_richer_pop))) %>%
    group_by(state,year) %>%
    mutate(gini= 1- sum(score)) %>%
    ungroup() %>%
    select(-score, -share_of_richer_pop) %>%
    distinct()->df

  return(df)}

#Set this to TRUE to print plots
print_diagnostics <- TRUE


#Scheme for plots
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
  theme(legend.key.width=unit(4,"cm"))


#Step 1: Process ACS data

#First read in ACS data. This is data downloaded from ACS website
acs_quintiles <- read.csv("Input_Data/GODEEEP_data/acs_gross_quintiles.csv", stringsAsFactors = FALSE) %>%
  gather("state","value","Alabama":"Puerto.Rico")

#Read in actual gross incomes from ACS. Downloaded from ACS website.
acs_gross_income <- read.csv("Input_Data/GODEEEP_data/acs_gross_income.csv", stringsAsFactors = FALSE)%>%
  gather("state","gross_income","Alabama":"Puerto.Rico")

#Now calculate federal tax adjustment using a regression.This was estimated using CBO data.
#This equation only uses 2015 federal tax regime (R squared= 0.99, shape=linear)
acs_quintiles %>%
  left_join(acs_gross_income) %>%
  mutate(tax = (((gross_income))*-0.3284)+19240,
         gross_after_fed_tax= gross_income+((tax))) %>%
  #Add adjustment to re-calculate quintiles after tax
  group_by(state,year) %>%
  mutate(tot_income= sum(gross_after_fed_tax*100),
         net_value=(gross_after_fed_tax*100/tot_income)*100) %>%
  ungroup() %>%
  dplyr::select(-tot_income)->acs_after_federal_tax


#The diagnostics below check if federal tax adjustment worked.
if(print_diagnostics){

  g <- ggplot(data=acs_after_federal_tax %>% filter(state=="New.York"),aes(color=Quintile))+
    geom_line(aes(x=year,y=value),linetype="solid",size=1.3)+
    geom_line(aes(x=year,y=net_value),linetype="dashed",size=1.3)+
    facet_wrap(~Quintile)+
    ggtitle("Federal Tax adjustment for New.York")

  g+scheme_basic


}

if(print_diagnostics){

  g <- ggplot(data=acs_after_federal_tax %>% filter(Quintile=="q5") ,aes(x=value,y=net_value))+
    geom_point()+
    geom_abline(slope=1)+
    xlim(40,60)+
    ylim(40,60)
    ggtitle("Federal Tax adjustment effect")

  g+scheme_basic


}



#Now compile state tax adjustment from ITEP. This was a working paper by Brookings Institution.
itep_state_tax <- read.csv("Input_Data/GODEEEP_data/ITEP_state_taxes.csv", stringsAsFactors = FALSE) %>%
  gather("Quintile","state_tax", "q1":"q5")

acs_after_federal_tax %>%
  #Assume dummy population of 100 people
  mutate(qpop=100,
         q_tot_income= qpop*gross_after_fed_tax) %>%
  group_by(state, year) %>%
  mutate(q_tot_income=sum(q_tot_income)) %>%
  ungroup() %>%
  mutate(income_after_federal = gross_after_fed_tax) %>%
  left_join(itep_state_tax, by =c("state","Quintile")) %>%
  na.omit() %>%
  mutate(
    state_net_income=income_after_federal -(state_tax*income_after_federal)) %>%
  mutate(qpop=100,
         q_tot_income= qpop*state_net_income) %>%
  group_by(state,year) %>%
  mutate(tot_income= sum(state_net_income*100),
         value_after_state_tax=(state_net_income*100/tot_income)*100) %>%
  ungroup() %>%
  select(Quintile,year,state,gross_income,income_after_federal,state_net_income,value_after_state_tax,value,net_value) %>%
  compute_gini_quintiles() ->  acs_after_state_tax

##Adjust for household size here in ACS data


#Check if all tax adjustments worked.
if(print_diagnostics){

  g <- ggplot(data=acs_after_state_tax %>% filter(state=="New.York"),aes(color=Quintile))+
    geom_line(aes(x=year,y=value,linetype="ACS gross income"),size=1.3)+
    geom_line(aes(x=year,y=net_value,linetype="Adjusted for federal taxes"),size=1.3)+
    geom_line(aes(x=year,y=value_after_state_tax,linetype="Adjusted for state taxes"),size=1.3)+
    facet_wrap(~Quintile,scales="free")+
    ylab("Shares (%)")+
    scale_linetype_manual(name = "Data",
                          values = c("solid","dashed","dotted"))+
    ggtitle("Income shares for New York by quintile")

  g+scheme_basic+theme(legend.position = "right")
}
ggsave(filename = paste("outputs/US_state_level_distributions/figures/ny_tax_quintiles.png"),
       width=42, height=22, units = "cm")


#Write the output here.
write.csv(acs_after_state_tax %>% select(state,year,gini,gross_income,income_after_federal,state_net_income,Quintile,shares=value_after_state_tax) %>% distinct(), "outputs/US_state_level_distributions/acs_state_consolidated_data.csv")


#Step 2: Produce income distribution projections

##First read in US level projection data
US_agg_data <- read.csv("outputs/GCAM_32_regions_Income_deciles_SSPs_1967_2100_Rao_et_al_downscaled.csv"
                        , stringsAsFactors = FALSE) %>% filter(GCAM_region_ID ==1, sce=="Historical data") %>%
  rename(Category = category)

#Read in WIDER dataset to recompute parameters for PC model
Wider_data_full <- read.csv("Input_Data/WIDER_aggregated_deciles.csv",stringsAsFactors = FALSE) %>%
  arrange(year)

#Calculate PC 1 and PC2 params from raw WIDER data.
pc_loadings_matrix <- get_PCA_loadings(Wider_data_full ,category_col = "Category",value_col = "Income..net.")
pc_center_sd <- get_sd_center(Wider_data_full , category_col = "Category", value_col = "Income..net.")


##Compute components (PC1, PC2) here
pridr::compute_components_data(US_agg_data,center_and_scaler_data = pc_center_sd, pc_loadings = pc_loadings_matrix, value_col = "shares",
                               grouping_variables = c("GCAM_region_ID","year"),category_col = "Category")->Components_US
##Compile other IV data for USA in 2010
compile_independent_variables() %>% filter(iso=="usa",year==2010)->IV_data


###Now read in ACS state net income GINI
state_gini<- acs_after_state_tax %>% select(state,year,gini) %>% distinct()

##Now compute historical deciles
state_level_deciles <- state_gini

state_level_deciles$lagged_ninth_decile <- IV_data$lagged_ninth_decile
state_level_deciles$lagged_palma_ratio <- IV_data$lagged_palma_ratio
state_level_deciles$labsh <- IV_data$labsh
state_level_deciles$Component1 <- state_level_deciles$gini*29.717083 + (-11.48152)
state_level_deciles$Component2 <- IV_data$Component2

PC_model(state_level_deciles,grouping_variables = c("state","year"),
         value_col = "shares",id_col = c("state")) %>%
  mutate(category=Category) %>%
  compute_gini_deciles(inc_col = "pred_shares",grouping_variables = c("state","year")) %>%
  left_join(state_gini %>% select(state, year, gini), by = c("state","year"))->state_level_deciles_historical


#We are going to need the values for the lagged palma ratio, lagged ninth decile later. Save here for now
state_level_deciles_historical %>%
  compute_palma_ratio(group_cols = c("state","year"),
                      income_col = "pred_shares",
                      category_col = "Category") %>%
  filter(year==2015) %>%
  select(-year) %>%
  rename(lagged_palma_ratio=palma_ratio)->state_palma_ratio

state_level_deciles_historical %>%
  filter(Category=="d9") %>%
  filter(year==2015) %>%
  rename(lagged_ninth_decile=pred_shares) %>%
  dplyr::select(state,lagged_ninth_decile)->state_lagged_ninth_decile

if(print_diagnostics){

  g <- ggplot(data=state_level_deciles_historical, aes(x=gini, y=output_name))+
    geom_point()+
    xlab("Historical GINI (Based on ACS net income quintiles)")+
    ylab("GINI based on calculated deciles")+
    geom_abline(slope=1)

  g + scheme_basic

}


if(print_diagnostics){

  acs_gross_income <- read.csv("Input_Data/GODEEEP_data/acs_gross_quintiles.csv", stringsAsFactors = FALSE)%>%
    gather("state","gross_income","Alabama":"Puerto.Rico")


  state_level_deciles_historical %>%
    select(state,year, pred_shares,Category) %>%
    spread(Category,pred_shares) %>%
    mutate(q1=d1+d2,
           q2=d3+d4,
           q3=d5+d6,
           q4=d7+d8,
           q5=d9+d10) %>%
    select(state,year,q1,q2,q3,q4,q5) %>%
    gather("Quintile","pred_value","q1":"q5") %>%
    mutate(state=gsub(" ",".",state))->state_level_quintiles

  state_level_quintiles %>%
    left_join(acs_gross_income)->validation_data

  g <- ggplot(validation_data , aes(x=gross_income,y=pred_value*100))+
    geom_point(aes(color=Quintile))+
    geom_abline(slope=1)+
    xlab("Gross quintile value from ACS")+
    ylab("Predicted value (Deciles aggregated to quintiles)")+
    xlim(0,60)+
    ylim(0,60)+
    scale_color_aaas()

  g+scheme_basic

}
ggsave(filename = paste("outputs/US_state_level_distributions/figures/pred_actual_quintiles.png"),
       width=42, height=22, units = "cm")

#Now produce future projections
US_agg_data %>% filter(year==2015)->US_2015_data

US_forecast_data <- read.csv("outputs/ISO_level_projections.csv"
                             , stringsAsFactors = FALSE) %>% filter(country =="United States of America", year > 2015) %>%
  mutate(category=Category) %>%
  compute_gini_deciles(inc_col = "pred_shares",
                       grouping_variables = c("country","year","sce")) %>%
  arrange(year) %>%
  mutate(gini_2015= unique(US_2015_data$gini),
         gini_scaler = output_name/gini_2015)

state_level_deciles_historical %>% filter(year==2015) %>%
  select(-year) %>%
  mutate(reg="US",
         gini=output_name) %>%
  left_join(US_forecast_data %>% select(year,sce,gini_scaler) %>% distinct() %>% mutate(reg="US"))%>%
  select(state,gini,year,sce,gini_scaler,Component2,Component1)  %>%
  mutate(gini=gini*gini_scaler) %>% distinct() ->state_level_future

#Join in all variables needed for PC model
state_level_future %>%
  left_join(state_lagged_ninth_decile) %>%
  left_join(state_palma_ratio)->state_level_future

#For the labor share, just USE US labor share
state_level_future$labsh <- IV_data$labsh


#Run the model
PC_model(state_level_future,grouping_variables = c("state","sce","year"),id_col=c("state","sce"))->state_future_projections

#Compile final data
state_future_projections %>% bind_rows(state_level_deciles_historical %>% mutate(sce="Historical data") %>% select(colnames(state_future_projections)))->consolidated_data

write.csv(consolidated_data, "outputs/US_state_level_distributions/US_50_state_DC_net_income_deciles_2011_2100_SSP.csv",row.names = FALSE)

diag_state="New.York"
if(print_diagnostics){

  g<-ggplot(data=consolidated_data %>%
           filter(state==diag_state,Category %in% c("d1","d10")) %>%
           filter(year>2015),aes(x=year,
                                 y=pred_shares*100,
                                 color=sce))+
    geom_line(size=1.3)+
    ylab("Predicted shares")+
    scale_color_npg()+
    facet_wrap(~Category, scales="free")+
    labs(subtitle = paste0("Income distribution in ",diag_state))

  g+scheme_basic
}


#state level income distributions (absolute value)
?state
library(tidyverse)
shares <- consolidated_data %>%
  mutate(state_name = str_replace_all(state,"[.]"," ")) %>%
  select(-state) %>%
  mutate(state = ifelse(state_name=="District of Columbia",
                        "DC",
                        state.abb[state_name]),
         sce = ifelse(sce=="Historical data", "Historical", sce))

gcamusa_pop_ouput_ssps_hist <- read.csv("Input_Data/GODEEEP_data/gcamusa_jiang_pop_state.csv",
                                        stringsAsFactors = FALSE,
                                        check.names = FALSE)
state_ssp_gdp_pc <- read.csv("Input_Data/GODEEEP_data/allssps_2020base_gdppc_state.csv",
                             stringsAsFactors = FALSE,
                             check.names = FALSE)

state_gdp <- inner_join(gcamusa_pop_ouput_ssps_hist, state_ssp_gdp_pc
                        , by= c("year","sce","state")) %>%
  mutate(gdp = ssp_gdp_pc * total_pop)

#joining state level gdp and population with income shares by decile
## calculating gdp and pop by deciles
state_decile_gdp <- left_join(state_gdp,
                              shares,
                              by= c("state", "year", "sce")) %>%
  select(state, state_name, year,sce,MODEL,Category,pred_shares,ssp_gdp_pc,total_pop,gdp) %>%
  mutate(decile_pop = total_pop*0.1,
         decile_gdp = pred_shares*gdp) %>%
  mutate(decile_gdp_pc=decile_gdp/decile_pop) %>%
  rename(shares = pred_shares,
         category = Category)
#*gdp in thousand 1990 USD

#Step 3- Aggregate state level income distributions to national level
##TODO- Kelly generates this state_decile_gdp file in her workflow. She will update the below
#make sure you give it all the columns it needs

national_decile_gdp <- state_decile_gdp %>%
  filter(year>2019,MODEL %in% c("GCAM-USA","OECD Env-Growth"),
         !sce=="Historical") %>%
  mutate(gdp_pc_1990usd = ssp_gdp_pc,
         ratio = 1,
         gini = "na") %>%
  select(year,sce,state,total_pop,gdp_pc_1990usd,ratio,ssp_gdp_pc,gdp,
         state_name,shares,category,gini,decile_pop,decile_gdp,decile_gdp_pc) %>%
  arrange(year,state,category) %>%
  group_by(year,sce,state) %>%
  mutate(gdp_ppp_pc_usd2011= sum(decile_gdp)/sum(decile_pop),
         population = sum(decile_pop),
         Category = category,
         GCAM_region_ID = 1,
         country=state) %>%
  ungroup() %>%
  aggregate_country_deciles_to_regions(value_col = "shares",
                                       grouping_variables = c("GCAM_region_ID","sce","year"))

write.csv(national_decile_gdp, "outputs/US_state_level_distributions/US_aggregated_income_distributions.csv",row.names = FALSE)

national_decile_gdp <- state_decile_gdp %>%
  filter(year %in% 2011:2015,
         sce=="Historical") %>%
  mutate(gdp_pc_1990usd = ssp_gdp_pc,
         ratio = 1,
         gini = "na") %>%
  rename(category = Category,
         shares = pred_shares) %>%
  select(year,sce,state,total_pop,gdp_pc_1990usd,ratio,ssp_gdp_pc,gdp,
         state_name,shares,category,gini,decile_pop,decile_gdp,decile_gdp_pc) %>%
  arrange(year,state,category) %>%
  group_by(year,sce,state) %>%
  mutate(gdp_ppp_pc_usd2011= sum(decile_gdp)/sum(decile_pop),
         population = sum(decile_pop),
         Category = category,
         GCAM_region_ID = 1,
         country=state) %>%
  ungroup() %>%
  aggregate_country_deciles_to_regions(value_col = "shares",
                                       grouping_variables = c("GCAM_region_ID","sce","year"))

write.csv(national_decile_gdp, "outputs/US_state_level_distributions/US_aggregated_income_distributions_hist.csv",row.names = FALSE)


