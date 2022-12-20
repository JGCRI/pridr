##Simple workflow file to generate income distribution projections for all SSPs using pridr
#Authors- Kanishka B. Narayan (kanishka.narayan@pnnl.gov) , Brian O'Neill, Claudia Tebaldi, Stephanie Waldhoff & Kelly C. Casper

# This workflow file contains following components
### Part 1- Compute all deciles historically using lognormal functional form
### Part 2- PC model deciles for all historical data
### Part 3- PC model deciles for all future years for all SSPs
### Part 4- Aggregate income distribution projections to GCAM regions from ISOs for all SSPs
### Part 5- Plot global lorenz curves for SSPs
### Part 6- Write outputs ###


library(dplyr)
library(tidyr)
library(parallel)
library(data.table)
library(assertthat)
library(ggplot2)
library(ggsci)


##Load all data
Wider_data_test <- read.csv("Input_Data/Wider_aggregated_deciles.csv", stringsAsFactors = FALSE) %>%
  arrange(year)


### Part 1- Compute all deciles historically using lognormal ###

## Generate a dataset to pass to main function
Wider_data_test %>%
  select(country, year, gdp_ppp_pc_usd2011, gini) %>%
  distinct() %>%
  mutate(sce="Historical data") ->data_for_lognorm

## Compute deciles
compute_deciles_lognormal(data_for_lognorm)->lognormal_model



### Part 2- PC model deciles for all historical data

##Compile Data
IV_data <- compile_independent_variables() %>% mutate(Component1_act= Component1, Component2_act= Component2)


##Combine independent variables with IV data
Final_historical_data <- read.csv("Input_Data/Final_Historical_data_ISO.csv", stringsAsFactors = FALSE) %>%
   select(year,gini=output_name,iso,country) %>%
   distinct() %>%
   left_join(IV_data %>% select(-gini,-country), by= c("iso","year")) %>%
   mutate(sce= "Historical data") %>%
   select(country,year,iso,sce,gini,labsh,lagged_ninth_decile,lagged_palma_ratio) %>%
   group_by(iso,sce) %>%
   mutate(labsh = ifelse(is.na(labsh),approx_fun(year,labsh,rule = 2),labsh),
          lagged_ninth_decile = ifelse(is.na(lagged_ninth_decile),approx_fun(year,lagged_ninth_decile,rule = 2),lagged_ninth_decile),
          lagged_palma_ratio=ifelse(is.na(lagged_palma_ratio),approx_fun(year,lagged_palma_ratio,rule=2),lagged_palma_ratio)) %>%
   ungroup() %>%
  group_by(year) %>%
  arrange(gini) %>%
   mutate(labsh = ifelse(is.na(labsh),0,labsh),
          lagged_ninth_decile =ifelse(is.na(lagged_ninth_decile),0,lagged_ninth_decile),
          lagged_palma_ratio=ifelse(is.na(lagged_palma_ratio),0,lagged_palma_ratio)) %>%
  ungroup() %>%
   distinct() %>%
   na.omit()



##Check if all required columns exist
for (i in c("country","sce","year","gini","lagged_ninth_decile","lagged_palma_ratio",
            "labsh")){

  if(!i %in% c(colnames(Final_historical_data))){

    stop("Data does not contain all required column types.")

  }}


##Generate historical deciles using PC model
Historical_deciles <- PC_model(Final_historical_data,id_col = c("country","sce"))


### Part 3- PC model deciles for all future years



#1. Read in Rao GINI data
Rao_data_scalars <- read.csv("Input_Data/Rao_scalers.csv", stringsAsFactors = FALSE) %>%
                   gather(iso,scalar,"ABW":"ZWE") %>%
                    rename(sce=ï..sce) %>%
                    mutate(iso= tolower(iso)) %>%
                    spread(year,scalar)

#2. Compile base data for future years
Final_historical_data %>% filter(year==2015) %>%
  select(-sce,-year) %>%
  left_join(Rao_data_scalars, by = c("iso")) %>%
  gather(year,scalar,"2015":"2100") %>%
  mutate(gini=gini*scalar) %>%
  filter(sce=="SSP2",
         labsh>0)->compiled_future_data

#3. Compile future projections
income_dist_projections_ssp <- PC_model(compiled_future_data)

### Part 4- Aggregate income distribution projections to GCAM regions from ISOs

#Now compile population and GDP data from the SSPs
#Population
pop_data <- read.csv("Input_Data/pop_ssp_database.csv",
                     stringsAsFactors = FALSE) %>%
            gather("year","population","X2015":"X2100") %>%
            mutate(year= as.integer(gsub("X","",year)),
                   iso=tolower(iso))%>%
  spread(sce,population) %>%
  na.omit() %>%
  gather("sce","population","SSP1":"SSP5")

#GDP_PC
gdp_pc_data <- read.csv("Input_Data/All_GDP_percapita.csv",
                        stringsAsFactors = FALSE) %>% gather("year","gdp_ppp_pc_usd2011","X2010":"X2100") %>%
                 mutate(year= as.integer(gsub("X","",year)),
                        iso=tolower(iso)) %>%
                 filter(year %in% c(2015:2100))%>%
                 spread(sce,gdp_ppp_pc_usd2011) %>%
                 na.omit() %>%
                 gather("sce","gdp_ppp_pc_usd2011","SSP1":"SSP5")

#Read in ISO mapping file to map ISOs to regions
iso_mapping_file <- read.csv("Input_Data/iso_GCAM_regID.csv",skip=6)

## Match isos with ISOs in our dataset
Final_historical_data %>%
  left_join(iso_mapping_file) %>%
  select(GCAM_region_ID,iso,country) %>%
  distinct()->ISO_mapping

#This will drop countries for which we don't have hist data
income_dist_projections_ssp %>%
  left_join(ISO_mapping) %>%
  mutate(year = as.integer(year)) %>%
  left_join(pop_data) %>%
  left_join(gdp_pc_data) %>%
  na.omit() %>%
  mutate(id= paste0(GCAM_region_ID,sce,year))->data_future_aggregation


#Now generate aggregated distribution
GCAM_regions_income_dist <- aggregate_country_deciles_to_regions(data_future_aggregation, value_col = "pred_shares",
                                                                 grouping_variables = c("GCAM_region_ID","sce","year"))



### Part 5 - Plot global lorenz curves ###

GCAM_regions_income_dist %>%
  mutate(decile_gdp_pcap=(tot_gdp*shares)/(tot_pop*0.1),
         decile_pop=tot_pop*0.1) %>%
  na.omit() %>%
  group_by(sce,year) %>%
  arrange(decile_gdp_pcap) %>%
  mutate(cum_gdp = cumsum(decile_gdp_pcap*decile_pop),
         cum_pop= cumsum(decile_pop),
         share_of_cum_pop= cum_pop/sum(decile_pop),
         share_of_cum_gdp = cum_gdp/sum(decile_gdp_pcap*decile_pop)) %>%
  ungroup() ->lorenz_curve_data

lorenz_curve_data_2015 <- lorenz_curve_data %>% filter(sce=="SSP2",year==2015)


g <- ggplot(data=lorenz_curve_data %>% filter(year %in% c(2015,2050,2100),
                                              sce %in% c("SSP2","SSP4","SSP5")),
            aes(x=share_of_cum_pop,y=share_of_cum_gdp,color=sce))+
    geom_line(linetype="solid",size=1.3)+
    geom_line(data=lorenz_curve_data_2015 %>% select(-year),
              aes(x=share_of_cum_pop,y=share_of_cum_gdp), color="grey",linetype="dashed",show.legend=FALSE,size=1.5)+
    facet_wrap(~year)+
    scale_color_aaas()+
    xlab("Share of global population")+
    ylab("Share of global GDP")+
    geom_abline(slope=1,linetype="solid",color="black")+
    labs(subtitle = "Dashed grey line is the income distribution in 2015. Solid black line is the line of perfect equality.")+
    ggtitle("Global Lorenz curves by SSP")

g+theme(axis.text.x = element_text(angle=45))

### Part 6 - Write outputs ###

GCAM_regions_income_dist %>% select(GCAM_region_ID,year,sce,shares,category) %>%
  spread(sce,shares) %>%
  mutate(SSP1 = if_else(is.na(SSP1),SSP5,SSP1),
         )->final_projections_data

#Write out outputs
write.csv(GCAM_regions_income_dist %>% select(GCAM_region_ID,year,sce,shares,category) %>% distinct(), "outputs/Income_distributions_32_GCAM_regions.csv")





