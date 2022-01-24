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

#Create inverted error function. Will need this for lognormal calculations below.
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)


#Function to calculate GINI coefficient from deciles.
compute_gini_deciles<- function(df, inc_col = "shares",
                                grouping_variables = c("region","year","sce","model","method") ){
  
  df %>% 
    distinct() %>% 
    mutate(share_of_richer_pop = if_else(category== "d1",0.9,
                                         if_else(category== "d2", 0.8,
                                                 if_else(category== "d3", 0.7,
                                                         if_else(category== "d4", 0.6,
                                                                 if_else(category == "d5",0.5,
                                                                         if_else(category =="d6", 0.4, if_else(category == "d7",0.3,
                                                                                                               if_else(category =="d8", 0.2,                                                                       
                                                                                                                       if_else(category =="d9",0.1,0)))))))))) %>% 
    mutate(score = !!as.name(inc_col) *(0.1+ (2*share_of_richer_pop))) %>%   
    group_by(across(grouping_variables)) %>% 
    mutate(output_name= 1- sum(score)) %>% 
    ungroup() %>% 
    #select(-score, -share_of_richer_pop) %>%
    distinct()->df  
  
  
  return(df)}

get_centers <- function(df, category_col = "category",
                           value_col = "value"){

df %>% 
    group_by(!!as.name(category_col)) %>% 
    mutate(center=mean(!!as.name(value_col))) %>% 
    select(c(category_col,center)) %>% 
    distinct()->output_list
  
  

return(output_list)

}

get_sd_center <- function(df, category_col = "category",
                   value_col = "value",SCALE=TRUE){
  
  #First get the centers
  centers <- get_centers(df,category_col, value_col)
  
  if(SCALE){
  df %>% 
    left_join(centers, by = c(category_col)) %>%
    mutate(centered_value=!!as.name(value_col)- center) %>% 
    group_by(!!as.name(category_col)) %>% 
    mutate(sd = sd(centered_value)) %>% 
    select(c(category_col,sd,center)) %>% 
    distinct()->output_list}else{
    
      df %>% 
        left_join(centers, by = c(category_col)) %>%
        mutate(centered_value=!!as.name(value_col)- center) %>% 
        group_by(!!as.name(category_col)) %>% 
        mutate(sd = 1) %>% 
        select(c(category_col,sd,center)) %>% 
        distinct()->output_list      
      
      
      
    }
  
  return(output_list)
  
  
}


get_PCA_loadings <- function(df,
    category_col = "category",
                    value_col = "value",SCALE=TRUE,
    number_of_features = 10,
    grouping_variables = c("country","year")){
  
  
  
  
  center_and_scaler <- get_sd_center(df,category_col = category_col, value_col= value_col)
  
  df %>%
    group_by(across(grouping_variables)) %>% 
    arrange(value_col) %>% 
    ungroup() %>% 
    select(category_col) %>% 
    distinct()->names_of_features
  
  as.list(names_of_features[category_col])->names_of_features
  #print(names_of_features)
  
  df %>% 
    left_join(center_and_scaler, by = c(category_col)) %>% 
    group_by(across(grouping_variables)) %>% 
    arrange(value_col) %>% 
    ungroup() %>% 
    mutate(value= (!!as.name(value_col)- center)/sd) %>%   
    select(category_col, value,grouping_variables) %>%   
    spread(category_col, value)->data_for_pca
  
  data_for_pca <- data_for_pca[, c(unlist(names_of_features))]
  
  
  pc_results <- prcomp(data_for_pca[1:number_of_features],center=FALSE,scale. = FALSE)
  
  as_tibble(pc_results$rotation)->pc_results
  
  pc_results[category_col]<-names_of_features
  
  return(as_tibble(pc_results))
}

compute_components <- function(df,center_and_scaler_data=NULL, 
                               pc_loadings =NULL,category_col = "category",
                               value_col = "value",SCALE=TRUE,
                               number_of_features =10,
                               grouping_variables= c("country", "year")){
    
  if(is.null(center_and_scaler_data)){
    
    center_and_scaler_data <- get_sd_center(df, category_col = category_col, value_col = value_col)
    }
  
  center_and_scaler_data<- as_tibble(center_and_scaler_data)
  
  df<- as_tibble(df)
  
   df %>% 
     left_join(center_and_scaler_data, by=c(category_col))->df_with_center_scaled
    
    if(is.null(pc_loadings)){ 
    pca_stats <- get_PCA_loadings(df,category_col = category_col, value_col = value_col,number_of_features= number_of_features,grouping_variables = grouping_variables)
    
    }
   df_with_center_scaled<- as_tibble(df_with_center_scaled)
   pca_stats <- as_tibble(pc_loadings)
   
   
   df_with_center_scaled %>% 
     left_join(pca_stats, by = c(category_col)) %>% 
     mutate(value=(!!as.name(value_col)-center)/sd)->df_centered_scaled
   
   
  
    data_for_pc_full_centered_scaled_wPC <- df_centered_scaled %>%
      group_by(across(grouping_variables)) %>% 
      mutate(Component1 = sum(value*PC1),
             Component2 = sum(value*PC2)) %>% 
      ungroup() %>% 
      select(c(grouping_variables,Component1,Component2)) %>% 
      distinct()         
    
    return(data_for_pc_full_centered_scaled_wPC)
    
  }

     
get_deciles_from_components <- function(df,use_second_comp = TRUE,
                                        pc_loadings =NULL,
                                        center_and_scaler = NULL,
                                        features = c("d1","d2","d3","d4",
                                                     "d5","d6","d7","d8","d9","d10"),
                                        category_col ="category",
                                        input_df= Wider_data_full,
                                        value_col = "Income..net.",
                                        grouping_variables = c("country","year")){
  
  if(use_second_comp){
    comp1_weight <- 1
    comp2_weight<- 1
  }else{
    comp1_weight <- 1
    comp2_weight<- 0
    
  }
  df_val <- df
  for (i in features){
    df_val[i] <- 0
    
  }
  
  df_val %>% 
    gather("Category","pred_shares",toString(features[1]):toString(features[length(features)]))->df_with_categories
  
  if(is.null(center_and_scaler)){
    
    center_and_scaler <- get_sd_center(input_df, category_col = category_col, value_col = value_col)
    
    
  }
  if(is.null(pc_loadings)){
    
    pc_loadings <- get_PCA_loadings(input_df,category_col = category_col, value_col = value_col,,
                                    number_of_features = length(features),
                                    grouping_variables = grouping_variables)
    
  }
  
  df_with_categories %>% 
    left_join(center_and_scaler, by = c(category_col)) %>% 
    left_join(pc_loadings, by = c(category_col)) %>% 
    mutate(pred_shares = (((Component1*PC1*comp1_weight)+(Component2*PC2*comp2_weight))*sd)+center)->df_with_pred_shares
    
  df_with_pred_shares  %>% 
    filter(pred_shares <0)->negative_features
  
  if(nrow(negative_features>0)){
    
    print(paste0("There are negative values for ",nrow(negative_features), " observations. Please use the correction function to correct negative values!."))
  }
  
  df_with_pred_shares %>% 
    select(grouping_variables, category_col, pred_shares,Component1,Component2) %>% 
    distinct()->df_with_pred_shares
  
  return(df_with_pred_shares)
}    


adjust_negative_predicted_features <- function(df,value_col="pred_shares",
                                     min_lowest_feature_val = 0.006,
                                     grouping_variables = c("country","year")){
  
  
  df %>% group_by(across(grouping_variables)) %>% 
    arrange(pred_shares) %>% 
    mutate(min_val = min(pred_shares),
           max_val = max(pred_shares)) %>% 
    mutate(diff_adj = min_lowest_feature_val - min_val,
           pred_shares = if_else(
                                 #First check if adjustment is necessary
                                 min_val <0,
                                 #Now see if this is the feature to be adjusted
                                 if_else(min_val==pred_shares,min_lowest_feature_val,
                                         if_else(max_val==pred_shares,pred_shares-diff_adj,pred_shares)),pred_shares)) %>%
    arrange(pred_shares) %>% 
    ungroup() %>% 
    select(-min_val, -max_val,-diff_adj)->adjusted_features
  
  negative_features <- adjusted_features  %>% filter(pred_shares <0)

  while(nrow(negative_features)> 0){
    print(paste0("There are still ",nrow(negative_features)," negative observations. Restarting loop."))
    adjusted_features %>%group_by(across(grouping_variables)) %>% 
      arrange(pred_shares) %>% 
      mutate(min_val = min(pred_shares),
             max_val = max(pred_shares)) %>% 
      mutate(diff_adj = min_lowest_feature_val - min_val,
             pred_shares = if_else(min_val <0, 
                                   if_else(min_val==pred_shares,min_lowest_feature_val,
                                           if_else(max_val==pred_shares,pred_shares-min_lowest_feature_val,pred_shares)),pred_shares)) %>%
      ungroup() %>% 
      select(-min_val, -max_val,-diff_adj)->adjusted_features 
    
    negative_features <- adjusted_features %>% filter(pred_shares <0)
    
  }

  adjusted_features %>% 
  group_by(across(grouping_variables)) %>% 
  arrange(pred_shares) %>% 
  mutate(Category =paste0("d",rank(pred_shares,ties.method = "first"))) %>% 
  ungroup()->adjusted_features
  
  return(adjusted_features)
}

GINI_downscaling_model <- function(df,gini_col = "gini",
                           intercept=-0.347554,
                           coeff=0.872650,
                           raw_data=df,
                           base_year=2015,
                           grouping_variables = c("country"),
                           historical_fit = TRUE,
                           start_index =4,
                           pc_loadings = NULL){
  
   df %>% 
    mutate(Component1= (!!as.name(gini_col)*coeff)+intercept)->df_w_Comp1
  
  if(historical_fit){
   df_comp_2 <- compute_components(raw_data, d1_start_col = start_index) %>% 
                select(c(grouping_variables,"Component2","year")) %>% 
                distinct()
   
   df_w_Comp1 %>% 
   left_join(df_comp_2, by = c(grouping_variables,"year"))->df_consolidated
  }else{
    
    df_comp_2 <- compute_components(raw_data, d1_start_col = start_index) %>%  
      group_by(across(grouping_variables)) %>% 
      mutate(max_year = max(year))%>% 
      ungroup() %>% 
      filter(year == max_year) %>% 
      select(c(grouping_variables,"Component2")) %>% 
      distinct()
    
    df_w_Comp1 %>% 
      left_join(df_comp_2, by = c(grouping_variables))->df_consolidated
    
    
  }
   pca_stats <- run_PCA(raw_data,start_index =start_index)
   
   
   
   df_deciles <- get_deciles_from_components(df_consolidated,
                                             center = get_center_PCA(raw_data),
                                             scaler = get_sd(raw_data),
                                             pc_loadings = pca_stats$rotation)
   
  
  return(df_deciles)
}    
    

  
aggregate_country_deciles_to_regions <- function(df,value_col="Income..net.",grouping_variables = c("GCAM_region_ID","year")){
  
  
  df %>%
    mutate(
      gdp_quintile = gdp_ppp_pc_usd2011*population* !!as.name(value_col),
      population = round(population * 0.1,2),
      gdp_ppp_pc_usd2011 = gdp_quintile/population) %>%
    group_by(across(grouping_variables)) %>%
    arrange(gdp_ppp_pc_usd2011) %>%   
    mutate(tot_pop = round(sum(population),2),
           tot_gdp = sum(gdp_quintile),
           cum_pop = round(cumsum(population),2)) %>%
    ungroup() %>% 
    dplyr::select(country, Category,gdp_ppp_pc_usd2011,population,gdp_quintile,
                  tot_pop,tot_gdp,cum_pop,grouping_variables)->processed_data


  ## B: Now calculate cutoffs (for population) and map the data to the respective cut-offs/deciles using the calculated cumulative population 
  processed_data %>% 
    group_by(across(grouping_variables)) %>% 
    mutate(d1_cutoff= round(tot_pop *0.1,2),
           d2_cutoff = round(tot_pop * 0.2,2),
           d3_cutoff = round(tot_pop * 0.3,2),
           d4_cutoff = round(tot_pop * 0.4,2),
           d5_cutoff= round(tot_pop *0.5,2),
           d6_cutoff = round(tot_pop * 0.6,2),
           d7_cutoff = round(tot_pop * 0.7,2),
           d8_cutoff = round(tot_pop * 0.8,2),
           d9_cutoff = round(tot_pop * 0.9,2)) %>% 
    ungroup() %>% 
    mutate(category = if_else(cum_pop <= d1_cutoff, "d1",
                              if_else(cum_pop <= d2_cutoff, "d2",
                                      if_else(cum_pop <= d3_cutoff, "d3",
                                              if_else(cum_pop <= d4_cutoff, "d4", 
                                                      if_else(cum_pop <= d5_cutoff, "d5",
                                                              if_else(cum_pop <= d6_cutoff, "d6",
                                                                      if_else(cum_pop <= d7_cutoff, "d7",
                                                                              if_else(cum_pop <= d8_cutoff, "d8",
                                                                                      if_else(cum_pop <= d9_cutoff, "d9","d10")))))))))) ->processed_data_with_categories
  
  ## C: Now calculate shortfalls within new deciles and calculate observations that need to be adjusted    
  
  ##Create a vector for below group_by
  grouping_variables_w_category <- c(grouping_variables,"category")
  
  processed_data_with_categories %>% 
    dplyr::select(country, population, grouping_variables_w_category, cum_pop, 
                  d1_cutoff, d2_cutoff, d3_cutoff, d4_cutoff,
                  d5_cutoff, d6_cutoff, d7_cutoff, d8_cutoff,d9_cutoff,
                  tot_pop,gdp_ppp_pc_usd2011) %>%
    group_by(across(grouping_variables_w_category)) %>% 
    mutate(shortfall = if_else(category=="d1", d1_cutoff - max(cum_pop),
                               if_else(category=="d2", d2_cutoff - max(cum_pop),
                                       if_else(category=="d3", d3_cutoff - max(cum_pop),
                                               if_else(category=="d4", d4_cutoff - max(cum_pop),
                                                       if_else(category=="d5", d5_cutoff - max(cum_pop),
                                                               if_else(category=="d6", d6_cutoff - max(cum_pop),
                                                                       if_else(category=="d7", d7_cutoff - max(cum_pop),
                                                                               if_else(category=="d8", d8_cutoff - max(cum_pop),
                                                                                       if_else(category=="d9", d9_cutoff - max(cum_pop),tot_pop - max(cum_pop)))))))))),
           min_cum_pop = min(cum_pop)) %>% 
    ungroup() %>% 
    group_by(across(grouping_variables)) %>% 
    mutate(lag_shortfall = if_else(is.na(lag(shortfall)),0,lag(shortfall))) %>% 
    ungroup() %>% 
    filter(cum_pop == min_cum_pop, category != "d1") %>%
    mutate(shortfall = lag_shortfall) %>% 
    filter(shortfall != 0) %>% 
    dplyr::select(country,category,shortfall,grouping_variables) %>%
    rename(shortfall_adj = shortfall)->shortfall_for_adjustment
  
  ## D: Now, we will calculate three categories,
  ## i.) adjusted entries based on the shortfall calculated above
  ## ii.) new entries with just the shortfall
  ## iii.) entries which did not have to be adjusted
  
  #Create a vector here for the group_by
  
  grouping_variables_w_category_country <- c(grouping_variables_w_category,"country")
  ### adjusted entries based on the shortfall calculated above
  processed_data_with_categories %>% 
    left_join(shortfall_for_adjustment, by= c("country","category",grouping_variables)) %>% 
    mutate(shortfall_adj = if_else(is.na(shortfall_adj),0, shortfall_adj)) %>% 
    filter(shortfall_adj != 0) %>% 
    group_by(across(grouping_variables_w_category_country)) %>% 
    mutate(min_cum_pop = min(cum_pop)) %>% 
    ungroup() %>% 
    mutate(population = if_else(cum_pop == min_cum_pop, population -shortfall_adj, population))-> population_adjusted_for_shortfall
  
  ### new entries with just the shortfall
  processed_data_with_categories %>% 
    left_join(shortfall_for_adjustment, by= c("country",grouping_variables,"category")) %>% 
    mutate(shortfall_adj = if_else(is.na(shortfall_adj),0, shortfall_adj)) %>% 
    filter(shortfall_adj != 0) %>% 
    group_by(across(grouping_variables_w_category_country)) %>% 
    mutate(min_cum_pop = min(cum_pop)) %>% 
    ungroup() %>% 
    filter(cum_pop == min_cum_pop) %>% 
    mutate(population = shortfall_adj) %>% 
    mutate(category= if_else(category=="d10", "d9",
                             if_else(category=="d9", "d8",
                                     if_else(category =="d8", "d7",
                                             if_else(category =="d7", "d6",
                                                     if_else(category =="d6", "d5",
                                                             if_else(category =="d5", "d4",
                                                                     if_else(category =="d4", "d3",
                                                                             if_else(category =="d3", "d2","d1")))))))))-> population_new_entries
  
  ### entries which did not have to be adjusted
  processed_data_with_categories %>% 
    left_join(shortfall_for_adjustment, by= c("country","category",grouping_variables)) %>% 
    mutate(shortfall_adj = if_else(is.na(shortfall_adj),0, shortfall_adj)) %>% 
    filter(shortfall_adj == 0) %>% 
    mutate(min_cum_pop = 0)-> population_no_adjustment
  
  
  ## E. Re-calculate deciles, cutoffs and ensure that problem does not exist
  test_data <- bind_rows(population_no_adjustment, population_new_entries, population_adjusted_for_shortfall) %>% 
    dplyr::select(-min_cum_pop, -shortfall_adj) %>%  
    group_by(across(grouping_variables)) %>% 
    arrange(gdp_ppp_pc_usd2011)%>%
    mutate(tot_pop = round(sum(population),2),
           tot_gdp = sum(gdp_quintile),
           cum_pop = round(cumsum(population),2),
           category = if_else(cum_pop <= d1_cutoff, "d1",
                              if_else(cum_pop <= d2_cutoff, "d2",
                                      if_else(cum_pop <= d3_cutoff, "d3",
                                              if_else(cum_pop <= d4_cutoff, "d4", 
                                                      if_else(cum_pop <= d5_cutoff, "d5",
                                                              if_else(cum_pop <= d6_cutoff, "d6",
                                                                      if_else(cum_pop <= d7_cutoff, "d7",
                                                                              if_else(cum_pop <= d8_cutoff, "d8",
                                                                                      if_else(cum_pop <= d9_cutoff, "d9","d10")))))))))) %>% 
    ungroup() %>% 
    group_by(across(grouping_variables_w_category)) %>% 
    mutate(max_cum_pop = max(cum_pop)) %>% 
    ungroup() %>% 
    filter(cum_pop == max_cum_pop) %>% 
    mutate(shortfall = if_else(category=="d1", d1_cutoff - cum_pop,
                               if_else(category=="d2", d2_cutoff - cum_pop,
                                       if_else(category=="d3", d3_cutoff - cum_pop,
                                               if_else(category=="d4", d4_cutoff - cum_pop,
                                                       if_else(category=="d5", d5_cutoff - cum_pop,
                                                               if_else(category=="d6", d6_cutoff - cum_pop,
                                                                       if_else(category=="d7", d7_cutoff - cum_pop,
                                                                               if_else(category=="d8", d8_cutoff - cum_pop,
                                                                                       if_else(category=="d9", d9_cutoff - cum_pop,tot_pop - cum_pop)))))))))) %>%   
    filter(shortfall != 0)
  
  if (nrow(test_data)>0){
    write.csv(test_data, "debug.csv")
    stop("Deciles are not lined up correctly in ",nrow(test_data),". Please check calculations above!")
  }
  
  
  ## F. Generate final data
  final_data <- bind_rows(population_no_adjustment, population_new_entries, population_adjusted_for_shortfall) %>% 
    dplyr::select(-min_cum_pop, -shortfall_adj) %>%  
    arrange(year, gdp_ppp_pc_usd2011, GCAM_region_ID) %>% 
    group_by(across(grouping_variables)) %>%
    mutate(cum_pop = round(cumsum(population),2),
           category = if_else(cum_pop <= d1_cutoff, "d1",
                              if_else(cum_pop <= d2_cutoff, "d2",
                                      if_else(cum_pop <= d3_cutoff, "d3",
                                              if_else(cum_pop <= d4_cutoff, "d4",
                                                      if_else(cum_pop <= d5_cutoff, "d5",
                                                              if_else(cum_pop <= d6_cutoff, "d6",
                                                                      if_else(cum_pop <= d7_cutoff, "d7",
                                                                              if_else(cum_pop <= d8_cutoff, "d8",
                                                                                      if_else(cum_pop <= d9_cutoff, "d9","d10")))))))))) %>% 
    ungroup() %>%      
    mutate(gdp_quintile = gdp_ppp_pc_usd2011*population) %>%   
    group_by(across(grouping_variables_w_category)) %>% 
    mutate(gdp_pcap_quintile = sum(gdp_quintile)/sum(population),
           population = sum(population)) %>%
    ungroup() %>%
    dplyr::select(grouping_variables_w_category, gdp_pcap_quintile, population,tot_pop, tot_gdp)%>%
    distinct() %>%
    group_by(across(grouping_variables)) %>%
    mutate(gdp_pcap = sum(gdp_pcap_quintile)) %>%
    ungroup() %>%
    mutate(shares = gdp_pcap_quintile/gdp_pcap,
           gdp_pcap = tot_gdp/tot_pop) %>%
    dplyr::select(grouping_variables_w_category, category, shares, gdp_pcap,tot_gdp,tot_pop) %>%
    distinct() %>% compute_gini_deciles(inc_col = "shares",grouping_variables = grouping_variables)
  
  return(final_data)
  
}  
  
