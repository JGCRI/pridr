## Load libraries
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

compute_gini_deciles<- function(df, inc_col = "shares",
                                grouping_variables = c(region,year,sce,model,method) ){
  
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
    group_by(!!!syms(grouping_variables)) %>% 
    mutate(output_name= 1- sum(score)) %>% 
    ungroup() %>% 
    #select(-score, -share_of_richer_pop) %>%
    distinct()->df  
  
  
  return(df)}


## Read in format for images
scheme_basic <- theme_bw() +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title = element_text(size = 10)) +
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
        legend.box.background = element_rect(colour = "black"))


## Read in data (Update paths below to wherever user has data )
Wider_data_test <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/WIDER_aggregated_deciles.csv",stringsAsFactors = FALSE) %>% 
              arrange(year) 

Wider_data_train <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/WIDER_aggregated_deciles.csv",stringsAsFactors = FALSE) %>% 
  arrange(year) %>% 
  filter(year <= 2004)

Wider_data_full <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/WIDER_aggregated_deciles.csv",stringsAsFactors = FALSE) %>% 
  arrange(year) 


GCAM_regions <- read.csv("C:/Users/nara836/Downloads/GCAM_region_names.csv",stringsAsFactors = FALSE, skip = 6)


#Create inverted error function. Will need this for lognormal calculations below.
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)


data_for_pc_full <- Wider_data_full  %>% 
  dplyr::select(country,year,Category,Income..net.,gini) %>% 
  mutate(Income..net. = Income..net.) %>% 
  spread(Category,Income..net.)  

center <- c(mean(data_for_pc_full$d1),mean(data_for_pc_full$d2),
            mean(data_for_pc_full$d3),mean(data_for_pc_full$d4),
            mean(data_for_pc_full$d5),mean(data_for_pc_full$d6),
            mean(data_for_pc_full$d7),mean(data_for_pc_full$d8),
            mean(data_for_pc_full$d9),mean(data_for_pc_full$d10))

data_for_pc_full_centered <- data_for_pc_full %>%
                            mutate(d1= d1-center[1],
                                   d2= d2-center[2],
                                   d3= d3-center[3],
                                   d4= d4-center[4],
                                   d5= d5-center[5],
                                   d6= d6-center[6],
                                   d7= d7-center[7],
                                   d8= d8-center[8],
                                   d9= d9-center[9],
                                   d10= d10-center[10])
scale_var <- TRUE

if(scale_var){
                              
scaler <- c(sd(data_for_pc$d1),
           sd(data_for_pc$d2),
           sd(data_for_pc$d3),
           sd(data_for_pc$d4),
           sd(data_for_pc$d5),
           sd(data_for_pc$d6),
           sd(data_for_pc$d7),
           sd(data_for_pc$d8),
           sd(data_for_pc$d9),
           sd(data_for_pc$d10))}else{
             
scaler <- c(1,1,1,1,1,1,1,1,1,1)             
             
           }

data_for_pc_full_centered_scaled <- data_for_pc_full_centered %>% 
                                   mutate(d1=d1/scaler[1],
                                          d2=d2/scaler[2],
                                          d3=d3/scaler[3],
                                          d4=d4/scaler[4],
                                          d5=d5/scaler[5],
                                          d6=d6/scaler[6],
                                          d7=d7/scaler[7],
                                          d8=d8/scaler[8],
                                          d9=d9/scaler[9],
                                          d10=d10/scaler[10])

pc_results <- prcomp(data_for_pc_full_centered_scaled[4:13],center=FALSE,scale. = FALSE)

pc_data <- as.data.frame(pc_results$x)


pc_loadings <- (pc_results$rotation) 



data_for_pc_full_centered_scaled_wPC <- data_for_pc_full_centered_scaled %>% 
                                        mutate(Component1 = (d1*pc_loadings[1])+
                                                     (d2*pc_loadings[3])+
                                                     (d3*pc_loadings[4])+
                                                     (d4*pc_loadings[5])+
                                                     (d5*pc_loadings[6])+
                                                     (d6*pc_loadings[7])+
                                                     (d7*pc_loadings[8])+
                                                     (d8*pc_loadings[9])+
                                                     (d9*pc_loadings[10])+
                                                     (d10*pc_loadings[2]),
                                               Component2 = (d1*pc_loadings[2,1])+
                                                 (d2*pc_loadings[2,3])+
                                                 (d3*pc_loadings[2,4])+
                                                 (d4*pc_loadings[2,5])+
                                                 (d5*pc_loadings[2,6])+
                                                 (d6*pc_loadings[2,7])+
                                                 (d7*pc_loadings[2,8])+
                                                 (d8*pc_loadings[2,9])+
                                                 (d9*pc_loadings[2,10])+
                                                 (d10*pc_loadings[2,2]))

data_for_pc_full %>% 
  mutate(top_bottom_ratio = d10/d1)->data_for_pc_full

data_for_pc_full_actual <- data_for_pc_full %>% 
  gather("category","value","d1":"d9")  

data_for_pc_full_centered_scaled_wPC %>% 
  gather("category","value","d1":"d9") %>% 
  mutate(pred_shares = if_else(category == "d1",
                               ((Component1 * pc_loadings[1,1])*scaler[1])+center[1],
                               if_else(category == "d2",
                                       ((Component1 * pc_loadings[3,1])*scaler[2])+center[2],
                                       if_else(category == "d3",
                                               ((Component1 * pc_loadings[4,1])*scaler[3])+center[3],
                                               if_else(category == "d4",
                                                       ((Component1 * pc_loadings[5,1])*scaler[4])+center[4],
                                                       if_else(category == "d5",
                                                               ((Component1 * pc_loadings[6,1])*scaler[5])+center[5],
                                                               if_else(category == "d6",
                                                                       ((Component1 * pc_loadings[7,1])*scaler[6])+center[6],
                                                                       if_else(category == "d7",
                                                                               ((Component1 * pc_loadings[8,1])*scaler[7])+center[7],
                                                                               if_else(category == "d8",
                                                                                       ((Component1 * pc_loadings[9,1])*scaler[8])+center[8],
                                                                                       if_else(category == "d9",
                                                                                               ((Component1 * pc_loadings[10,1])*scaler[9])+center[9],
                                                                                               ((Component1 * pc_loadings[2,1])*scaler[10])+center[10])))))))))) %>% 
  select(-value) %>% 
  spread("category","pred_shares") %>% 
  mutate(diff_adj_d1 = 0.006 - d1,
         d10 = if_else(d1<0, d10-diff_adj_d1/4,d10),
         d9 = if_else(d1<0, d9-diff_adj_d1/4,d9),
         d8 = if_else(d1<0, d8-diff_adj_d1/4,d8),
         d7 = if_else(d1<0, d7-diff_adj_d1/4,d7),
         d1= if_else(d1<0, 0.006,d1),
         diff_adj_d2 = 0.011 - d2,
         d10 = if_else(d2<0, d10-diff_adj_d2/4,d10),
         d9 = if_else(d2<0, d9-diff_adj_d2/4,d9),
         d8 = if_else(d2<0, d8-diff_adj_d2/4,d8),
         d7 = if_else(d2<0, d7-diff_adj_d2/4,d7),
         d2= if_else(d2<0, 0.011,d2)
         ) %>% 
  select(-diff_adj_d1, -diff_adj_d2) %>% 
  gather("category","pred_shares","d1":"d9")->data_for_pc_pred

full_data_PCA <- data_for_pc_full_actual %>% 
          left_join(data_for_pc_pred %>% select(country,year,category,pred_shares)) %>% 
  compute_gini_deciles(inc_col = "value",
                       grouping_variables = c("country","year")) %>% 
  rename(gini_act = output_name) %>% 
  compute_gini_deciles(inc_col = "pred_shares",
                       grouping_variables = c("country","year")) %>% 
  rename(gini_pred = output_name)





g <- ggplot()+
     geom_point(data=full_data_PCA %>% filter(category %in% c("d1")), aes(x=gini_act,y=gini_pred),color="grey")+
     #facet_wrap(~category, scales="free")+
     geom_abline(slope = 1, linetype="dashed")

g+scheme_basic
full_data_PCA %>% select(country, year, category,gini_act,gini_pred) %>% 
  mutate(category="gini") %>% 
  distinct() %>% 
  rename(pred_shares= gini_pred,value=gini_act) %>% 
  bind_rows(full_data_PCA %>% select(country,year,category,pred_shares,value))->full_data_PCA_inc_GINI

full_data_PCA %>% 
  select(-value) %>% 
  spread("category","pred_shares")->top_bottom_data

top_bottom_data %>% 
  mutate(top_bottom_predicted = d10/d1)->top_bottom_data


#Calculate variables we need for PCA based on regional data
data_for_pc <- Wider_data_train  %>% 
  dplyr::select(country,year,Category,Income..net.,gini) %>% 
  mutate(Income..net. = Income..net.) %>% 
  spread(Category,Income..net.)  


pc_results <- prcomp(data_for_pc[4:13], center = TRUE)

pc_data <- as.data.frame(pc_results$x)


pc_loadings <- (pc_results$rotation) 

center <- pc_results$center

data_for_pc$Component1 <- pc_data$PC1


gini_model <- lm(data=data_for_pc, formula ="Component1 ~ gini")


gini_model_coeff <- 0.872650
gini_model_intercept <- -0.347554


## Read in mapping file to map ISO's to GCAM regions (Update paths below to wherever user has data 
wider_mapping_file <- read.csv("C:/Users/nara836/Downloads/WIDER_mapping_file.csv",stringsAsFactors = FALSE)


## Read in GCAM ISOs
GCAM_region_iso_file <- read.csv("C:/Users/nara836/Downloads/iso_GCAM_regID.csv", stringsAsFactors = FALSE, skip =6) 

#Build deciles based on lognormal

#Function to compute lognormal distribution
compute_lognormal_dist <- function(mean_income, gini, max_income, len_sample){
  
  sd <- 2 * erfinv(gini)
  m <- log(mean_income) -((sd^2)/2)  
  
  draws3 <- dlnorm(seq(0, max_income, length.out=len_sample), m, sd)
  
  draw_d <- as.data.frame(draws3)  %>% 
    mutate(gdp_pcap = seq(0, max_income, length.out=len_sample)) %>% 
    rename(density = draws3 )
  
  return(draw_d)
}
##1. Based on lognormal distribution
Wider_data_test %>% 
  select(country, year, gdp_ppp_pc_usd2011, gini) %>% 
  distinct() %>% 
  mutate(id = paste0(country,year)) ->data_for_lognorm

data_for_lognorm_split <- split(data_for_lognorm, data_for_lognorm$id)


compute_lognormal_country <- function(df){
  
  mean <- (df$gdp_ppp_pc_usd2011)
  gini <- unique(df$gini)
  max_income <- (df$gdp_ppp_pc_usd2011)*50
  len_sample <- 10000
  year <- unique(df$year)
  
  results <- compute_lognormal_dist(mean_income = mean,
                                    gini = gini,
                                    max_income = max_income,
                                    len_sample = len_sample)
  results$country <- unique(df$country)
  results$year <- year
  return(results)
}

computed_lognorm_model_list <- lapply(data_for_lognorm_split,compute_lognormal_country)

computed_lognorm_model <- rbindlist(computed_lognorm_model_list)



  g <- ggplot()+
    geom_line(data=computed_lognorm_model %>% filter(country %in% c("India","China","USA","South Africa"), year == 2010) , aes(x=gdp_pcap, y= density),color="blue", linetype ="dashed")+
    scale_x_continuous(labels = unit_format(scale = 0.001, unit="Thous USD"))+
    scale_y_continuous(labels = unit_format(scale = 10000))+
    xlab("GDP per capita")+
    facet_wrap(~country, scales="free")
  
  g + scheme_basic


computed_lognorm_model %>%
  group_by(country, year) %>% 
  arrange(gdp_pcap) %>% 
  mutate(tot_density = sum(density),
         tot_income = sum(density * gdp_pcap),
         cut_off_quintile = tot_density * 0.1,
         cut_off_d1 = cut_off_quintile,
         cut_off_d2 = cut_off_d1 + cut_off_quintile,
         cut_off_d3 =  cut_off_d2 + cut_off_quintile,
         cut_off_d4 =  cut_off_d3 + cut_off_quintile,
         cut_off_d5 = cut_off_d4 + cut_off_quintile,
         cut_off_d6 =  cut_off_d5 + cut_off_quintile,
         cut_off_d7 =  cut_off_d6 + cut_off_quintile,
         cut_off_d8 =  cut_off_d7 + cut_off_quintile,
         cut_off_d9 =  cut_off_d8 + cut_off_quintile,
         cum_density = cumsum(density)) %>% 
  ungroup() %>% 
  mutate(category = if_else(cum_density < cut_off_d1, "d1",
                            if_else(cum_density < cut_off_d2, "d2",
                                    if_else(cum_density < cut_off_d3, "d3",
                                            if_else(cum_density < cut_off_d4, "d4",
                                                    if_else(cum_density < cut_off_d5, "d5",
                                                            if_else(cum_density < cut_off_d6, "d6",
                                                                    if_else(cum_density < cut_off_d7, "d7",
                                                                            if_else(cum_density < cut_off_d8, "d8",
                                                                                    if_else(cum_density < cut_off_d9, "d9","d10")))))))))) %>% 
  group_by(country, year, category) %>% 
  mutate(gdp_quintile = sum(density* gdp_pcap),
         shares = gdp_quintile/tot_income,
         gdp_pcap_quintile = sum(density* gdp_pcap)/sum(density)
  ) %>% 
  ungroup() %>% 
  dplyr::select(country, year, category, shares) %>% 
  distinct() %>% 
  mutate(model = "Log normal based downscaling (using country GINI)")->log_normal_downscaled_results


##2. Based on PCA
Wider_data_test %>%
  select(country,year,Category,gini) %>% 
  distinct() %>% 
  rename(category = Category) %>% 
  mutate(gini_coeff = gini_model_coeff,
         gini_model_intercept = gini_model_intercept) %>% 
  mutate(PC1_pred = (gini *gini_coeff) + gini_model_intercept) %>% 
  mutate(pred_shares = if_else(category == "d1",
                               (PC1_pred * pc_loadings[1])+center[1],
                               if_else(category == "d2",
                                       (PC1_pred * pc_loadings[3])+center[3],
                                       if_else(category == "d3",
                                               (PC1_pred * pc_loadings[4])+center[4],
                                               if_else(category == "d4",
                                                       (PC1_pred * pc_loadings[5])+center[5],
                                                       if_else(category == "d5",
                                                               (PC1_pred * pc_loadings[6])+center[6],
                                                               if_else(category == "d6",
                                                                       (PC1_pred * pc_loadings[7])+center[7],
                                                                       if_else(category == "d7",
                                                                               (PC1_pred * pc_loadings[8])+center[8],
                                                                               if_else(category == "d8",
                                                                                       (PC1_pred * pc_loadings[9])+center[9],
                                                                                       if_else(category == "d9",
                                                                                               (PC1_pred * pc_loadings[10])+center[10],
                                                                                               (PC1_pred * pc_loadings[2])+center[2])))))))))) %>% 
  spread(category, pred_shares) %>% 
  mutate(d10 = if_else(d1 < 0, d10 - 0.002, d10),
         d9 = if_else(d1 < 0, d9 - 0.002, d9),
         d8 = if_else(d1 < 0, d8 - 0.002, d8),
         d7 = if_else(d1 < 0, d7 - 0.002, d7),
         d1 = if_else(d1 < 0, 0.008, d1)) %>% 
  gather("category", "shares", "d1":"d9") %>% 
  dplyr::select(country, category, shares, year) %>% 
  mutate(model = paste0("PCA based downscaling"))->PCA_based_downscaling


#3. PCA combined with regression
Wider_data_test_regression <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/WIDER_aggregated_deciles.csv",stringsAsFactors = FALSE) %>% 
  arrange(year) %>% 
  #filter(year > 2003) %>% 
  select(country, year,gdp_ppp_pc_usd2011,category=Category,shares=Income..net.) %>% 
  distinct()

Wider_data_test_regression %>%
  spread(category,shares) %>% 
  mutate(Component1 =((d1-center[1])*pc_loadings[1,1])+
           ((d2-center[3])*pc_loadings[3,1])+
           ((d3-center[4])*pc_loadings[4,1])+
           ((d4-center[5])*pc_loadings[5,1])+
           ((d5-center[6])*pc_loadings[6,1])+
           (d6-center[7])*pc_loadings[7,1]+
  ((d7-center[8])*pc_loadings[8,1])+
  ((d8-center[9])*pc_loadings[9,1])+
  ((d9-center[10])*pc_loadings[10,1])+
  ((d10-center[2])*pc_loadings[2,1])) %>% 
  select(country, year,gdp_ppp_pc_usd2011,Component1) %>%
  arrange(country,year) %>% 
  group_by(country) %>% 
  mutate(lagged= lag(Component1)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(PC1_pred = (gdp_ppp_pc_usd2011*-0.0004146)+
           (lagged*0.8599759)+0.0070582) %>% 
  mutate(d1 = (PC1_pred * pc_loadings[1])+center[1],
         d2 = (PC1_pred * pc_loadings[3])+center[3],
         d3 = (PC1_pred * pc_loadings[4])+center[4],
         d4 = (PC1_pred * pc_loadings[5])+center[5],
         d5 = (PC1_pred * pc_loadings[6])+center[6],
         d6 = (PC1_pred * pc_loadings[7])+center[7],
         d7 = (PC1_pred * pc_loadings[8])+center[8],
         d8 = (PC1_pred * pc_loadings[9])+center[9],
         d9 = (PC1_pred * pc_loadings[10])+center[10],
         d10 = (PC1_pred * pc_loadings[2])+center[2]) %>% 
  mutate(diff_adj_d1 = 0.006 - d1,
         d10 = if_else(d1<0, d10-diff_adj_d1/4,d10),
         d9 = if_else(d1<0, d9-diff_adj_d1/4,d9),
         d8 = if_else(d1<0, d8-diff_adj_d1/4,d8),
         d7 = if_else(d1<0, d7-diff_adj_d1/4,d7),
         d1= if_else(d1<0, 0.006,d1),
         diff_adj_d2 = 0.011 - d2,
         d10 = if_else(d2<0, d10-diff_adj_d2/4,d10),
         d9 = if_else(d2<0, d9-diff_adj_d2/4,d9),
         d8 = if_else(d2<0, d8-diff_adj_d2/4,d8),
         d7 = if_else(d2<0, d7-diff_adj_d2/4,d7),
         d2= if_else(d2<0, 0.011,d2)
  ) %>% 
  gather("category", "shares", "d1":"d10") %>% 
  mutate(id= paste0(country,year)) %>% 
  dplyr::select(country, category, shares, year, id) %>% 
  mutate(model = "PCA combined with regression")->Regression_Results

Wider_data_test %>% 
  mutate(id = paste0(country,year)) %>% 
  filter(id %in% c(unique(Regression_Results$id)))->historical_data

bind_rows(PCA_based_downscaling , log_normal_downscaled_results,Regression_Results %>% select(-id)) %>% 
  compute_gini_deciles(inc_col = "shares",grouping_variables = c("country",
                                                                 "year",
                                                                 "model")) %>% 
  rename(gini_pred=output_name) %>% 
  left_join(historical_data %>% rename(category=Category,
                                       shares_hist =Income..net.) %>% 
            select(country,year,category,shares_hist,gini)) %>% 
          na.omit() %>% 
  mutate(sample= ifelse(model=="Log normal based downscaling (using country GINI)","all data",
                        ifelse(year <= 2004, "in sample","out of sample"))) %>% 
  mutate(model = ifelse(model == "Log normal based downscaling (using country GINI)",
                        "Lognormal based downscaling",model))->data_for_plot

data_for_plot %>% 
  select(country, year,category,shares,shares_hist,model) %>% 
  filter(model %in% c("Lognormal based downscaling"))->data_for_plot_deciles

data_for_plot %>% 
  select(country, year,gini,gini_pred,model) %>% 
  mutate(category="gini") %>% 
  rename(shares=gini_pred,
         shares_hist= gini) %>% 
  distinct() %>% 
  select(country, year,category,shares,shares_hist,model) %>% 
  mutate(sample= ifelse(model=="Log normal based downscaling","all data",
                        ifelse(year <= 2004, "in sample","out of sample"))) %>% 
  filter(model %in% c("Lognormal based downscaling"))->data_for_plot_gini

Wider_data_pop <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/WIDER_aggregated_deciles.csv",stringsAsFactors = FALSE) %>% 
  select(country, year, population) %>% 
  mutate(population = population/1000000) %>% 
  distinct()

Wider_data_pop <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/WIDER_aggregated_deciles.csv",stringsAsFactors = FALSE) %>% 
  select(country, year, population) %>% 
  mutate(population = population) %>% 
  distinct()

data_for_plot_deciles %>%
  bind_rows(data_for_plot_gini) %>% 
  left_join(Wider_data_pop , by =c("country","year")) %>% 
  group_by(model,category) %>% 
  mutate(sum_of_squared_error_pop = sum(((shares-shares_hist)^2)*population)/sum(population),
         sum_of_sq_error = sum((shares-shares_hist)^2)) %>%  
  ungroup() %>% 
  select(model, sum_of_sq_error,sum_of_squared_error_pop,category) %>% 
  distinct()->error_stat



g <- ggplot(data= data_for_plot_deciles %>%
              bind_rows(data_for_plot_gini) %>% 
              filter(category %in% c("d10","d1")) %>% 
              mutate(category = if_else(category=="d1","a.) First Decile",
                                        if_else(category=="d10", "b.) Tenth Decile",
                                                "c.) GINI coefficient"))), aes(x=shares_hist, y=shares,color="Deciles based on lognormal"),alpha=0.5)+
     geom_point()+
    geom_point(data=full_data_PCA_inc_GINI %>% filter(category %in% c("d10","d1")) %>% 
                 mutate(category = if_else(category=="d1","a.) First Decile",
                                           if_else(category=="d10", "b.) Tenth Decile",
                                                   "c.) GINI coefficient"))), aes(x=value,y=pred_shares),fill="grey",color="grey",alpha=0.4,show.legend = FALSE)+
     facet_wrap(~category, scales="free")+
     # geom_text(data= error_stat %>% filter(category %in% c("d1","d10","gini")) %>% 
     #             mutate(category = if_else(category=="d1","1. First Decile",
     #                                       if_else(category=="d10", "2. 10th Decile",
     #                                               "3.GINI (based on deciles)"))),aes(x=max(), y= 0.8, label= paste0("Sum of squared error = ",round(sum_of_sq_error,3))),color="black",show.legend = FALSE)+
     geom_abline(slope = 1,linetype="dashed")+
    xlab("Historical data shares of income")+
    ylab("Calculated shares")+
    ggtitle("Comparing deciles calculated using different methods")
g+scheme_basic

full_data_PCA %>% 
  group_by(category) %>% 
  mutate(mean_sq_error = sum((pred_shares - value)^2)) %>% 
  ungroup() %>% 
  select(category, mean_sq_error) %>% 
  distinct()->error_state_PCA


g <- ggplot(data= data_for_plot_deciles %>%
              bind_rows(data_for_plot_gini) %>% 
              filter(category %in% c("d10","d1")) %>% 
              mutate(category = if_else(category=="d1","a.) First Decile",
                                        if_else(category=="d10", "b.) Tenth Decile",
                                                "c.) Fifth Decile"))), aes(x=shares_hist, y=shares))+

  geom_point(alpha=0.4,aes(color=model))+
  geom_smooth(aes(color=model),method="lm",linetype="dashed",size=1.4,alpha=5,se=F, level=0.90)+
  #geom_point(alpha=0.1,color="white")+
  #
  facet_wrap(~category, scales="free")+
  # geom_text(data= error_stat %>% filter(category %in% c("d1","d10","gini")) %>% 
  #             mutate(category = if_else(category=="d1","1. First Decile",
  #                                       if_else(category=="d10", "2. 10th Decile",
  #                                               "3.GINI (based on deciles)"))),aes(x=max(), y= 0.8, label= paste0("Sum of squared error = ",round(sum_of_sq_error,3))),color="black",show.legend = FALSE)+
  geom_abline(slope = 1,linetype="dashed")+
  xlab("Historical shares of income")+
  ylab("Forecasted shares")+
  ggtitle("Evaluating different models to forecast deciles")
g+scheme_basic


Rao_gini <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/Rao_et_al_allGINI.csv") %>% 
            gather("ISO","gini","ABW":"ZWE") %>% 
            mutate(gini=gini/100,
                   sce=scenario)

Dom_van_gini <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/DOMINIC_VAN_DER_MENS.CSV", stringsAsFactors = FALSE) %>% 
gather("year","gini","X2017":"X2100") %>% 
mutate(year = as.numeric(gsub("X","",year)))  

Dom_van_gini <- as_tibble(Dom_van_gini)

GDP_per_capita <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/All_GDP_percapita.csv")%>% 
  gather("year","GDP_per_capita","X2010":"X2100") %>%
  mutate(GDP_per_capita = GDP_per_capita/1000 ,
         year = as.numeric(gsub("X","",year)))


categories <- as_tibble(c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10"))

GINI_data_downscaled <- bind_rows(Rao_gini %>% select(model,sce,year,ISO,gini), as_tibble(Dom_van_gini)) %>% 
  mutate(gini_coeff = gini_model_coeff,
         gini_model_intercept = gini_model_intercept) %>% 
  repeat_add_columns(categories) %>% 
  rename(category= value) %>% 
  mutate(PC1_pred = (gini *gini_coeff) + gini_model_intercept) %>% 
  mutate(pred_shares = if_else(category == "d1",
                               (PC1_pred * pc_loadings[1])+center[1],
                               if_else(category == "d2",
                                       (PC1_pred * pc_loadings[3])+center[3],
                                       if_else(category == "d3",
                                               (PC1_pred * pc_loadings[4])+center[4],
                                               if_else(category == "d4",
                                                       (PC1_pred * pc_loadings[5])+center[5],
                                                       if_else(category == "d5",
                                                               (PC1_pred * pc_loadings[6])+center[6],
                                                               if_else(category == "d6",
                                                                       (PC1_pred * pc_loadings[7])+center[7],
                                                                       if_else(category == "d7",
                                                                               (PC1_pred * pc_loadings[8])+center[8],
                                                                               if_else(category == "d8",
                                                                                       (PC1_pred * pc_loadings[9])+center[9],
                                                                                       if_else(category == "d9",
                                                                                               (PC1_pred * pc_loadings[10])+center[10],
                                                                                               (PC1_pred * pc_loadings[2])+center[2])))))))))) %>% 
  spread(category, pred_shares) %>% 
  mutate(d10 = if_else(d1 < 0, d10 - 0.002, d10),
         d9 = if_else(d1 < 0, d9 - 0.002, d9),
         d8 = if_else(d1 < 0, d8 - 0.002, d8),
         d7 = if_else(d1 < 0, d7 - 0.002, d7),
         d1 = if_else(d1 < 0, 0.008, d1)) %>% 
  gather("category", "shares", "d1":"d9") %>% 
  select(model, sce, year, category,shares,ISO) %>% 
  mutate(method= "2. Calculated by downscaling GINI using PCA (using Rao et al. GINI )")

log_normal_data <- bind_rows(Rao_gini %>% select(model,sce,year,ISO,gini), Dom_van_gini) %>% 
                   left_join(GDP_per_capita, by = c("ISO","sce","year")) %>% 
                   na.omit() %>% 
                   mutate(id = paste0(model,sce,year,ISO)) %>% 
                   filter(year %in% c(2100))

log_normal_iso_list <- split(log_normal_data, log_normal_data$id)



compute_lognormal_country <- function(df){
  
  mean <- (df$GDP_per_capita)
  gini <- unique(df$gini)
  max_income <- (df$GDP_per_capita)*10
  len_sample <- 5000
  year <- unique(df$year)
  model <- unique(df$model)
  ISO <- unique(df$ISO)
  sce <- unique(df$sce)
  results <- compute_lognormal_dist(mean_income = mean,
                                    gini = gini,
                                    max_income = max_income,
                                    len_sample = len_sample)
  results$ISO <- unique(df$ISO)
  results$year <- year
  results$model <- model
  results$sce <- sce
  return(results)
}

log_normal_iso_processed <- lapply(log_normal_iso_list, compute_lognormal_country)

log_normal_iso_data <- rbindlist(log_normal_iso_processed)


log_normal_iso_data %>% 
  group_by(ISO, year,sce,model) %>% 
  arrange(gdp_pcap) %>% 
  mutate(tot_density = sum(density),
         tot_income = sum(density * gdp_pcap),
         cut_off_quintile = tot_density * 0.1,
         cut_off_d1 = cut_off_quintile,
         cut_off_d2 = cut_off_d1 + cut_off_quintile,
         cut_off_d3 =  cut_off_d2 + cut_off_quintile,
         cut_off_d4 =  cut_off_d3 + cut_off_quintile,
         cut_off_d5 = cut_off_d4 + cut_off_quintile,
         cut_off_d6 =  cut_off_d5 + cut_off_quintile,
         cut_off_d7 =  cut_off_d6 + cut_off_quintile,
         cut_off_d8 =  cut_off_d7 + cut_off_quintile,
         cut_off_d9 =  cut_off_d8 + cut_off_quintile,
         cum_density = cumsum(density)) %>% 
  ungroup() %>% 
  mutate(category = if_else(cum_density < cut_off_d1, "d1",
                            if_else(cum_density < cut_off_d2, "d2",
                                    if_else(cum_density < cut_off_d3, "d3",
                                            if_else(cum_density < cut_off_d4, "d4",
                                                    if_else(cum_density < cut_off_d5, "d5",
                                                            if_else(cum_density < cut_off_d6, "d6",
                                                                    if_else(cum_density < cut_off_d7, "d7",
                                                                            if_else(cum_density < cut_off_d8, "d8",
                                                                                    if_else(cum_density < cut_off_d9, "d9","d10")))))))))) %>% 
  group_by(ISO, year, category,sce,model) %>% 
  mutate(gdp_quintile = sum(density* gdp_pcap),
         shares = gdp_quintile/tot_income,
         gdp_pcap_quintile = sum(density* gdp_pcap)/sum(density)
  ) %>% 
  ungroup() %>% 
  dplyr::select(ISO, year, category, shares, sce,model) %>% 
  distinct() %>% 
  mutate(method = "1. Deciles calculated assumping lognormal distribution (using Rao et al. GINI) ")->lognormal_models


consolidated_inequalities <- bind_rows(lognormal_models, GINI_data_downscaled) %>% 
                            filter(ISO %in% c("CHN","IND","ZAF","USA")) %>%
                               mutate(region = if_else(ISO== "CHN","China",
                                                       if_else(ISO=="ZAF","South Africa",
                                                               if_else(ISO=="IND","India","USA")))) %>%
                             select(-ISO) %>%
                             filter(year == 2100) %>% 
                             mutate(model = if_else(model =="Nrao","Rao 2019",
                                                    if_else(model =="DOM_V_MENS","Van der Mensbrugghe D 2015",model)))


Forecasts_using_GDP <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/Net_income_deciles_GCAM_regions_1967_2100.csv", stringsAsFactors = FALSE) %>% 
                       left_join(GCAM_regions) %>% 
                     filter(region %in% c("China","South Africa","India","USA")) %>%
                       filter(year ==2100) %>% 
                       mutate(method = "3. Regression (using Autoregressive + GDP per capita) and PCA ",
                              model = "Autoregressive + GDP per capita") %>% 
                       rename(category=Category,
                              shares = Income..net.) %>% 
                       select( -gini)

Forecasts_using_Education <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/Net_income_deciles_Rao_et_al_2016_2100.csv", stringsAsFactors = FALSE) %>% 
  left_join(GCAM_regions) %>% 
  filter(region %in% c("China","South Africa","India","USA")) %>% 
  mutate(method = "Regression",
         model = "Autoregressive + Education") %>% 
  rename(category=Category,
         shares = Income..net.) %>% 
  select(-GCAM_region_ID, -gini)

Forecasts_consolidated <- bind_rows(Forecasts_using_GDP)


compute_gini_GCAM_region_ID_SSP_deciles_method <- function(df){
  
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
    mutate(score = (shares) *(0.1+ (2*share_of_richer_pop))) %>%   
    group_by(region,year,sce,model,method) %>% 
    mutate(gini= 1- sum(score)) %>% 
    ungroup() %>% 
    select(-score, -share_of_richer_pop) %>%
    distinct()->df  
  
  return(df)}

GDP_per_capita_GCAM <- read.csv("Input_Data/GDP_pcap_forecast_GCAM_regions.csv",stringsAsFactors = FALSE) %>% 
  filter(year %in% c(2100),
         region %in% c("India","China","South Africa","USA")) %>%
  mutate(GDP_pcap = gdppcap/1000) %>% 
  select(region, GDP_pcap,sce=scenario) %>% 
  distinct()
  

consolidated_inequalities %>% bind_rows(Forecasts_using_GDP) %>%
  mutate(sce = if_else(sce=="Historical data","SSP1",sce)) %>%
  compute_gini_GCAM_region_ID_SSP_deciles_method() %>% 
  filter(sce%in%c("SSP4")) %>% 
  filter(model %in% c("Rao 2019",
                      "Autoregressive + GDP per capita")) %>% 
  inner_join(pop_data_GCAM %>% select(-GCAM_region_ID)%>%  filter(year==2100),by=c("region","sce","year")) %>% 
  inner_join(GDP_per_capita_GCAM, by = c("region","sce")) %>% 
  mutate(GDP = GDP_pcap*pop_mil*0.1*shares*1000,
    pop_mil = pop_mil *0.1) %>% 
  group_by(region,model,method,sce) %>%
  arrange(shares) %>% 
  mutate(tot_pop = sum(pop_mil),
         cum_pop = cumsum(pop_mil),
         cum_shares = cumsum(GDP*0.001)) %>% 
  ungroup()->data_for_plot

data_for_plot %>% 
  select(-cum_shares,-cum_pop,-GDP,-X) %>% 
  spread("category","shares") %>% 
  mutate(decile_ratio = d10/(d1+d2)) %>% 
  select(region,sce,model,method,decile_ratio)->t



g <- ggplot(data= data_for_plot %>% mutate(id = paste0(method,sce)) %>% filter(region != "South Africa"), aes(x=(cum_pop),y=cum_shares,color=method,group=id))+
   geom_line()+
  geom_point()+
   facet_wrap(~region,scales="free")+
  xlab("Cumulative population in millions")+
  ylab("Cumulative GDP in billion USD")+
  ggtitle("Distributions of income in 2100 for SSP4")
g + scheme_basic

g <- ggplot()+
     #geom_line(data= Forecasts_using_GDP %>% bind_rows(consolidated_inequalities) %>%  filter(year == 2100) %>% mutate(id =paste0(region)), aes(x=reorder(category,shares),y=shares*100, color=region,group=id,linetype=method),alpha=0.6,size=1.2)+
  geom_point(data= Forecasts_using_GDP %>% filter(year == 2100) %>% mutate(id =paste0(region)),aes(x=reorder(category,shares),y=shares*100, color=region),size=1.5)+
  geom_line(data= Forecasts_using_GDP %>% filter(year == 2015) %>% mutate(id =paste0(region)), aes(x=reorder(category,shares),y=shares*100,group=id),color="black",alpha=0.5,size=1.4, linetype = "dashed")+
  facet_grid(~model+sce, scales="free")+
  ylab("Shares of total income")+
  xlab("Decile")+
  scale_color_npg()
g + scheme_basic

pop_data_post_2015 <- read.csv("C:/Users/nara836/Downloads/GDP_pcap_forecast_GCAM_regions.csv", stringsAsFactors = FALSE) %>% 
            select(GCAM_region_ID, sce=scenario,year,pop_mil) %>% 
            left_join(GCAM_regions) 

pop_data_2015 <- read.csv("C:/Users/nara836/Downloads/GDP_pcap_forecast_GCAM_regions.csv", stringsAsFactors = FALSE) %>% 
  select(GCAM_region_ID, sce=scenario,year,pop_mil) %>% 
  left_join(GCAM_regions) %>% filter(year ==2016, sce=="SSP1") %>% mutate(year=2015,sce="Historical data") 

pop_data_2015 %>% bind_rows(pop_data_post_2015) ->pop_data_GCAM            

pop_data_ISO <- read.csv("Input_Data/Pop_SSP.csv", stringsAsFactors = FALSE) %>% 
                mutate(pop_mil = population)

GDP_per_capita %>% filter(year==2100) ->GDP_per_capita_ISO

GDP_per_capita_GCAM <- read.csv("Input_Data/GDP_pcap_forecast_GCAM_regions.csv",stringsAsFactors = FALSE) %>% 
                      filter(year %in% c(2100)) %>% 
                      rename(GDP_per_capita_GCAM = gdppcap) %>% 
                      select(GCAM_region_ID, sce=scenario, year,GDP_per_capita_GCAM) %>% 
                     distinct()

Forecasts_using_GDP %>% 
  bind_rows(consolidated_inequalities) %>% 
  left_join(GDP_per_capita_ISO, by = c("ISO","sce","year")) %>% 
  left_join(GDP_per_capita_GCAM, by = c("GCAM_region_ID","sce","year")) %>% 
  left_join(pop_data, by = c("region","year","sce",
                             "GCAM_region_ID")) %>% 
  left_join(pop_data_ISO %>% select(-pop_mil) %>% distinct() , by = c("ISO","sce")) %>%
  mutate(pop_mil = if_else(is.na(pop_mil), population, pop_mil),
         GDP_per_capita = if_else(is.na(GDP_per_capita),GDP_per_capita_GCAM,GDP_per_capita)) %>% 
  select(-ISO,-population,-GCAM_region_ID,-X,-GDP_per_capita_GCAM,-region) %>% 
  distinct() %>% 
  na.omit() %>% 
  mutate(GDP_share = pop_mil*GDP_per_capita*shares,
    pop_mil =pop_mil *0.1) %>% 
  group_by(year,sce,model, method) %>% 
  arrange(GDP_share) %>% 
  mutate(tot_pop = sum(pop_mil),
         tot_gdp = sum(GDP_share),
          cum_pop = cumsum(pop_mil)/tot_pop,
         cum_shares = cumsum(GDP_share)/tot_gdp) %>% 
  ungroup() %>% 
  distinct() %>% 
  select(year,sce,model, method,cum_pop,cum_shares,category) %>% 
  distinct() %>% 
  filter(model %in% c("Autoregressive + GDP per capita",
                      "Rao 2019"),
         sce %in% c("SSP1","SSP3","SSP4")) %>% 
  mutate(region="Global")->plot_data_2

g <- ggplot()+
     geom_line(data=plot_data_2 %>% filter(sce != "Historical data",year==2100) %>% mutate(id = paste0(region,model,method)),
               aes(x=cum_pop, y= cum_shares, group=id, color=method,linetype =model),size=1.2,alpha=0.6)+
  geom_line(data=plot_data_2 %>% filter(sce == "SSP1",year==2016,method=="Regression") %>% mutate(id = paste0(region,sce)),
            aes(x=cum_pop, y= cum_shares, group=id),size=1.3,alpha=0.6,color="grey",linetype="solid",show.legend = FALSE)+
     facet_grid(cols=vars(region),rows = vars(sce) )+
     ylab("Cumulative share of income")+
     xlab("Cumulative share of population")+
     scale_color_npg()+
  geom_abline(slope = 1,linetype="dashed")+
  labs(subtitle = "Grey line is distribution in 2015, colored ines are distribbutions in 2100")+
  ggtitle("Lorenz curves in 2100 across models and methods")

g + scheme_basic

