#Simple workflow

Wider_data_test <- read.csv("Input_Data/Wider_aggregated_deciles.csv", stringsAsFactors = FALSE) %>%
  arrange(year)

##1. Based on lognormal distribution
Wider_data_test %>%
  select(country, year, gdp_ppp_pc_usd2011, gini) %>%
  distinct() %>%
  mutate(sce="Historical data") ->data_for_lognorm

compute_deciles_lognormal(data_for_lognorm)->lognormal_model


##2.Compile Data

IV_data <- compile_independent_variables() %>% mutate(Component1_act= Component1, Component2_act= Component2)

IV_data %>% mutate(Component2_act = -17.77579+(labsh*0.99944)+(112.35374*lagged_ninth_decile))

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




for (i in c("country","sce","year","gini","lagged_ninth_decile","lagged_palma_ratio",
            "labsh")){

  if(!i %in% c(colnames(Final_historical_data))){

    stop("Data does not contain all required column types.")

  }}

Final_historical_data %>%
  #First we need first year values
  mutate(id= paste0(country,sce)) ->data_for_split

data_for_func <- split(data_for_split, data_for_split$id)

cl <- create_cores()


t <- parLapply(cl,data_for_func,compute_PC_model_components)

# t <- foreach(
#   i = data_for_func,
#   .combine = 'bind_rows'
# ) %dopar% {
#
#   compute_PC_model_components(i)
# }

stopCluster(cl)

t_data <- rbindlist(t) %>%
          get_deciles_from_components(use_second_comp = TRUE,grouping_variables = c("country","year","sce")) %>%
          adjust_negative_predicted_features(grouping_variables = c("country","year","sce"))



PC_model(Final_historical_data)->x

final_future_data <- read.csv("outputs/ISO_level_projections.csv", stringsAsFactors = FALSE)

pridr::create_cores()

act_data_GCAM_regions <- aggregate_country_deciles_to_regions(final_hist_data)

x %>%  left_join(final_hist_data %>% select(country, GCAM_region_ID,year,gdp_ppp_pc_usd2011,
                                          population,Category),by = c("country","year","Category")) %>%
       aggregate_country_deciles_to_regions(value_col = "pred_shares",grouping_variables = c("GCAM_region_ID","year","sce"))->pred_GCAMreg

compute_components_data(x ,category_col = "Category", value_col ="pred_shares",
                        grouping_variables = c("country","year","sce")) %>%
                       rename(Component2_act=Component2,
                              Component1_act=Component1) %>%
  left_join(x)->PC_data_act


Diff_factors <- pred_GCAMreg %>% select(GCAM_region_ID,year,category,sce,pred_shares=shares) %>%
                left_join(act_data_GCAM_regions %>% select(GCAM_region_ID,year,category,shares)) %>%
                mutate(diff_factors = shares-pred_shares)

#Compile future data here

#1. Read in Rao scalers

Rao_data_scalars <- read.csv("../../Input_Data/Rao_scalers.csv", stringsAsFactors = FALSE) %>%
                   gather(iso,scalar,"ABW":"ZWE") %>%
                    rename(sce=ï..sce) %>%
                    mutate(iso= tolower(iso)) %>%
                    spread(year,scalar)

Final_historical_data %>% filter(year==2015) %>%
  select(-sce,-year) %>%
  left_join(Rao_data_scalars, by = c("iso")) %>%
  gather(year,scalar,"2015":"2100") %>%
  mutate(gini=gini*scalar)->compiled_future_data


income_dist_projections_ssp <- PC_model(compiled_future_data)

#Now compile population and GDP data
income_dist_projections_ssp <- read.csv("outputs/ISO_level_projections.csv", stringsAsFactors = FALSE)
pop_data <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/pop_ssp_database.csv",
                     stringsAsFactors = FALSE) %>%
            gather("year","population","X2015":"X2100") %>%
            mutate(year= as.integer(gsub("X","",year)),
                   iso=tolower(iso))%>%
  spread(sce,population) %>%
  na.omit() %>%
  gather("sce","population","SSP1":"SSP5")

gdp_pc_data <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/All_GDP_percapita.csv",
                        stringsAsFactors = FALSE) %>% gather("year","gdp_ppp_pc_usd2011","X2010":"X2100") %>%
                 mutate(year= as.integer(gsub("X","",year)),
                        iso=tolower(iso)) %>%
                 filter(year %in% c(2015:2100))%>%
                 spread(sce,gdp_ppp_pc_usd2011) %>%
                 na.omit() %>%
                 gather("sce","gdp_ppp_pc_usd2011","SSP1":"SSP5")

## Read in iso data
final_hist_data %>%
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

data_future_aggregation_split <- split(data_future_aggregation, data_future_aggregation$id)



data_future_aggregation_final <- lapply(data_future_aggregation_split,aggregate_country_deciles_to_regions(value_col = "pred_shares",grouping_variables = c("GCAM_region_ID","year","sce")))

num_unique_iso <- length(unique(data_future_aggregation$country))
num_unique_sce <- length(unique(data_future_aggregation$sce))
num_unique_year <- length(unique(data_future_aggregation$year))

num_unique_vals <- num_unique_iso*num_unique_sce*num_unique_year*10

#Now aggregate to GCAM regions
par_agg <- function(df){df %>%
    aggregate_country_deciles_to_regions(value_col = "pred_shares",grouping_variables = c("GCAM_region_ID","year","sce"))->df_updated

  return(df_updated)

  }


GCAM_regions_income_dist %>% select(GCAM_region_ID,year,sce,shares,category) %>%
  spread(sce,shares) %>%
  mutate(SSP1 = if_else(is.na(SSP1),SSP5,SSP1),
         )->final_projections_data

#Write out outputs
write.csv(GCAM_regions_income_dist %>% select(GCAM_region_ID,year,sce,shares,category) %>% distinct(), "outputs/Income_distributions_32_GCAM_regions.csv")
