#Functions to prep data

compile_independent_variables<- function(){

  Consolidated_data <- read.csv("Input_Data/Consolidated_data.csv",
                                stringsAsFactors = FALSE) %>%
    select(-Component2) %>%
    #filter(country %in% c(unique(anomalies$country))) %>%
    filter(Category =="d9") %>%
    select(-Category) %>%
    #left_join(data_comp_2 %>% filter(Category=="d1") %>% select(country,year,Component1,pred_shares,Component2,palma_ratio) %>% distinct()) %>%
    distinct() %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(Pub_spend_Educ = ifelse(is.na(Pub_spend_Educ), approx_fun(year,Pub_spend_Educ,rule=2),Pub_spend_Educ),
           Educ_GINI = ifelse(is.na(Educ_GINI), approx_fun(year,Educ_GINI,rule=2),Educ_GINI),
           gdp_gr = (gdp_ppp_pc_usd2011- lag(gdp_ppp_pc_usd2011))/lag(gdp_ppp_pc_usd2011),
           Trade = ifelse(is.na(Trade), approx_fun(year,Trade,rule=2),Trade),
           lagged_comp2 = lag(Component2),
           Comp2_gr = (Component2- lag(Component2))/lag(Component2),
           lagged_comp1 = lag(Component1),
           lagged_ninth_decile = lag(Income..net.),
           lagged_palma_ratio = lag(palma_ratio),
           mean_gini = mean(gini)) %>%
    ungroup()


  return(Consolidated_data)



}









