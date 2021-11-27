# Script to aggregate income distributions from raw WIDER data
## Authors- Kanishka Narayan, Stephanie Waldhoff, Brian O'Neil and Claudia Tebaldi

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




## Read in format for images
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
        legend.box.background = element_rect(colour = "black"))


## Read in data (Update paths below to wherever user has data )
Wider_data <- read.csv("C:/Users/nara836/Downloads/WIDER_aggregated.csv",stringsAsFactors = FALSE)
print_diagnostics <- TRUE


GCAM_regions <- read.csv("C:/Users/nara836/Downloads/GCAM_region_names.csv",stringsAsFactors = FALSE, skip = 6)
#Create inverted error function. Will need this for lognormal calculations below.
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)

#Read in WIDER data
Wider_data %>% filter(Category=="q5") %>% mutate(mean_q5 = mean(Income..net.))->t

#Create a distribution of q5 values
if(print_diagnostics){
g <- ggplot(data=t, aes(x=Income..net.))+
     geom_histogram(bins=50)+
     xlab("Income Shares")+
     ggtitle("Distribution of Q5 values across countries ")

g
}

#Calculate variables we need for PCA based on regional data
data_for_pc <- Wider_data  %>% 
               dplyr::select(country,year,Category,Income..net.,gini) %>% 
               mutate(Income..net. = Income..net./100) %>% 
               spread(Category,Income..net.)  
               

pc_results <- prcomp(data_for_pc[4:8], center = TRUE)

pc_data <- as.data.frame(pc_results$x)


pc_loadings <- (pc_results$rotation) 

center <- pc_results$center

data_for_pc$Component1 <- pc_data$PC1

if(print_diagnostics){
  
  g <- ggplot(data= data_for_pc, aes(x=gini, y =Component1 ))+
       geom_point(alpha =0.6)+
       xlab("GINI coefficient")+
       ylab("Component values")+
       ggtitle("Comparison of the principal component and GINI coefficient across all countries for all years")+
    geom_abline(slope=1.0582, color= "blue", linetype = "dashed",intercept=-0.38, size=1.2)
  
  
  g + scheme_basic
  
  
  
}

gini_model <- lm(data=data_for_pc, formula ="Component1 ~ gini")

gini_model_coeff <- -0.385619
gini_model_intercept <- 1.058156
## Read in mapping file to map ISO's to GCAM regions (Update paths below to wherever user has data 
wider_mapping_file <- read.csv("C:/Users/nara836/Downloads/WIDER_mapping_file.csv",stringsAsFactors = FALSE)


## Read in GCAM ISOs
GCAM_region_iso_file <- read.csv("C:/Users/nara836/Downloads/iso_GCAM_regID.csv", stringsAsFactors = FALSE, skip =6) 

##WIDER GINI data
GINI_data <- read.csv("C:/Users/nara836/Downloads/WIDER_GINI_data.csv", stringsAsFactors = FALSE) %>% 
             dplyr::select(iso, year, gini_reported, gdp_ppp_pc_usd2011, population)   


# Part 1 : Pre-processing
## A. Load interpolation and other helper functions

### function to add columns
repeat_add_columns <- function(x, y) {
  UNIQUE_JOIN_FIELD <- NULL           # silence package checks.
  #assert_that(tibble::is_tibble(x))
  #assert_that(tibble::is_tibble(y))
  
  x %>%
    mutate(UNIQUE_JOIN_FIELD = 1) %>%
    full_join(mutate(y, UNIQUE_JOIN_FIELD = 1), by = "UNIQUE_JOIN_FIELD") %>%
    select(-UNIQUE_JOIN_FIELD)
}


compute_gini <- function(df){
  
  df %>% 
    distinct() %>% 
    mutate(share_of_richer_pop = if_else(Category== "q1",0.8,
                                         if_else(Category== "q2", 0.6,
                                                 if_else(Category== "q3", 0.4,
                                                         if_else(Category== "q4", 0.2,0))))) %>% 
    mutate(score = (`Income (net)`) *(0.2+ (2*share_of_richer_pop))) %>%   
    group_by(country,year) %>% 
    mutate(gini= 1- sum(score)) %>% 
    ungroup() %>% 
    select(-score, -share_of_richer_pop) %>%
    distinct()->df  
  
  return(df)}

compute_gini_GCAM_region_ID_component <- function(df){
  
  df %>% 
    distinct() %>% 
    mutate(share_of_richer_pop = if_else(category== "q1",0.8,
                                         if_else(category== "q2", 0.6,
                                                 if_else(category== "q3", 0.4,
                                                         if_else(category== "q4", 0.2,0))))) %>% 
    mutate(score = (shares) *(0.2+ (2*share_of_richer_pop))) %>%   
    group_by(GCAM_region_ID,year,Component) %>% 
    mutate(gini= 1- sum(score)) %>% 
    ungroup() %>% 
    select(-score, -share_of_richer_pop) %>%
    distinct()->df  
  
  return(df)}


compute_gini_GCAM_region_ID <- function(df){
  
  df %>% 
    distinct() %>% 
    mutate(share_of_richer_pop = if_else(category== "q1",0.8,
                                         if_else(category== "q2", 0.6,
                                                 if_else(category== "q3", 0.4,
                                                         if_else(category== "q4", 0.2,0))))) %>% 
    mutate(score = (shares) *(0.2+ (2*share_of_richer_pop))) %>%   
    group_by(GCAM_region_ID,year) %>% 
    mutate(gini= 1- sum(score)) %>% 
    ungroup() %>% 
    select(-score, -share_of_richer_pop) %>%
    distinct()->df  
  
  return(df)}

compute_gini_GCAM_region_ID_SSP <- function(df){
  
  df %>% 
    distinct() %>% 
    mutate(share_of_richer_pop = if_else(Category== "q1",0.8,
                                         if_else(Category== "q2", 0.6,
                                                 if_else(Category== "q3", 0.4,
                                                         if_else(Category== "q4", 0.2,0))))) %>% 
    mutate(score = (`Income (net)`) *(0.2+ (2*share_of_richer_pop))) %>%   
    group_by(GCAM_region_ID,year,sce) %>% 
    mutate(gini= 1- sum(score)) %>% 
    ungroup() %>% 
    select(-score, -share_of_richer_pop) %>%
    distinct()->df  
  
  return(df)}

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

### Helper function for the opposite of %in%
`%notin%` <- Negate(`%in%`)


years_WIDER <- as_tibble(1967:2015)
categories <- as_tibble(c("q1","q2","q3","q4","q5"))

## B. Fill holes in data for inequality (distribution values), gdp per capita and population

### Start with GCAM ISOs. 

GCAM_region_iso_file %>% 
  repeat_add_columns(years_WIDER) %>% 
  rename(year = value)->GCAM_region_iso_year

### Match in GINI values

GCAM_region_iso_year %>%  left_join(GINI_data, by= c("iso","year")) %>% 
  #group_by(iso) %>% 
  #mutate(gini_reported = ifelse(is.na(gini_reported), approx_fun(year,gini_reported,rule=1),gini_reported)) %>% 
  #ungroup()%>% 
  repeat_add_columns(categories) %>% 
  rename(Category = value) %>% 
  left_join(Wider_data %>%  select(iso, year, Category, Income..net.,gini), by = c("iso","year","Category")) %>% 
  #First fill holes within countries where we have data
  mutate(gini_reported = if_else(is.na(gini),gini_reported/100,gini)) %>% 
  select(-gini) %>% 
  arrange(country_name, year, Category) %>% 
  mutate(gdp_ppp_pc_usd2011 = as.numeric(gdp_ppp_pc_usd2011)) %>% 
  group_by(country_name,Category) %>%
  mutate(Income..net. = ifelse(is.na(Income..net.), approx_fun(year,Income..net.,rule=2),Income..net.),
         gdp_ppp_pc_usd2011 = ifelse(is.na(gdp_ppp_pc_usd2011),approx_fun(year, gdp_ppp_pc_usd2011,rule=2),gdp_ppp_pc_usd2011),
         population = ifelse(is.na(population), approx_fun(year, population, rule =2),population),
         gini_reported = ifelse(is.na(gini_reported), approx_fun(year,gini_reported,rule=2),gini_reported)) %>% 
  ungroup() %>% 
  #Sort by GINI
  arrange(Category, gini_reported) %>%
  group_by(Category) %>% 
  mutate(Income..net. = na.approx(Income..net.,na.rm=FALSE)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  rename(country= country_name)->WIDER_nearest_neighbor_interpolation
  
WIDER_nearest_neighbor_interpolation %>%  rename(`Income (net)` = Income..net.) %>% 
  mutate(`Income (net)` = `Income (net)`/100) %>% 
  compute_gini() %>% 
  mutate(diff = abs(((gini_reported - (gini))/gini_reported)*100))->test_data
  
test_data %>%  filter(diff > 1)->anomalies



### Read in countries
countries <- as.data.frame(unique(Wider_data$country)) %>% 
  rename(country = `unique(Wider_data$country)`) 

### Convert to tibble
countries <- as_tibble(countries)

### Same for years and categories
years <- as_tibble(unique(Wider_data$year)) 
categories <- as_tibble(unique(Wider_data$Category))

### Now find missing entries which we will interpolate
country_data_missing <- countries %>%
  repeat_add_columns(years)%>% rename(year = value) %>% 
  mutate(unique_id= paste0(country,year)) %>% 
  filter(unique_id %notin% unique(Wider_data$id)) %>% 
  repeat_add_columns(categories) %>% rename(Category = value) %>% 
  mutate(Income..net. = NA,
         population = NA,
         gdp_ppp_pc_usd2011 = NA) %>% 
  rename(id = unique_id)

### Complete interpolation
Wider_data %>% dplyr::select(country, year, Category,gdp_ppp_pc_usd2011, population, Income..net.,id) %>% 
               bind_rows(country_data_missing) %>% 
               arrange(country, Category, year) %>% 
               group_by(country,Category) %>% 
               mutate(Income..net. = ifelse(is.na(Income..net.), approx_fun(year,Income..net.,rule=1),Income..net.),
                      gdp_ppp_pc_usd2011 = ifelse(is.na(gdp_ppp_pc_usd2011),approx_fun(year, gdp_ppp_pc_usd2011),gdp_ppp_pc_usd2011),
                      population = ifelse(is.na(population), approx_fun(year, population, rule =1),population)) %>% na.omit() %>% 
               left_join(wider_mapping_file, by=c("country")) %>% na.omit()->Wider_data_interpolated
              
# Part 2 : Main processing code 
## A : Calculate the GDP per capita in each quintile, arrange the quintiles in each GCAM region by gdp per capita
## and line up quintiles by GCAM_region_ID, year and income level, then compute cumulatives  

USE_NEAREST_NEIGHBOR_INTERPOLATION <- TRUE

if(USE_NEAREST_NEIGHBOR_INTERPOLATION){
  Wider_data_interpolated <- WIDER_nearest_neighbor_interpolation  
  
}



Wider_data_interpolated %>%
           mutate(
                  gdp_quintile = gdp_ppp_pc_usd2011*population* (Income..net./100),
                  population = round(population * 0.2,2),
                  gdp_ppp_pc_usd2011 = gdp_quintile/population) %>%
           arrange(year, gdp_ppp_pc_usd2011, GCAM_region_ID) %>%
           group_by(GCAM_region_ID,year) %>% 
         mutate(tot_pop = round(sum(population),2),
         tot_gdp = sum(gdp_quintile),
         cum_pop = round(cumsum(population),2)) %>%
          ungroup() %>% 
          dplyr::select(country, year, Category,gdp_ppp_pc_usd2011,population,gdp_quintile,
                 tot_pop,tot_gdp,cum_pop, GCAM_region_ID)->processed_data

## B: Now calculate cutoffs (for population) and map the data to the respective cut-offs/quintiles using the calculated cumulative population 
processed_data %>% 
  group_by(GCAM_region_ID, year) %>% 
  mutate(q1_cutoff= round(tot_pop *0.2,2),
         q2_cutoff = round(tot_pop * 0.4,2),
         q3_cutoff = round(tot_pop * 0.6,2),
         q4_cutoff = round(tot_pop * 0.8,2)) %>% 
  ungroup() %>% 
  mutate(category = if_else(cum_pop <= q1_cutoff, "q1",
                            if_else(cum_pop <= q2_cutoff, "q2",
                                    if_else(cum_pop <= q3_cutoff, "q3",
                                            if_else(cum_pop <= q4_cutoff, "q4", "q5"))))) ->processed_data_with_categories

## C: Now calculate shortfalls within new quintiles and calculate observations that need to be adjusted    
processed_data_with_categories %>% 
  dplyr::select(country, year, population, GCAM_region_ID, cum_pop, category, q1_cutoff, q2_cutoff, q3_cutoff, q4_cutoff, tot_pop,gdp_ppp_pc_usd2011) %>%
  group_by(GCAM_region_ID, year, category) %>% 
  mutate(shortfall = if_else(category=="q1", q1_cutoff - max(cum_pop),
                             if_else(category=="q2", q2_cutoff - max(cum_pop),
                                     if_else(category=="q3", q3_cutoff - max(cum_pop),
                                             if_else(category=="q4", q4_cutoff - max(cum_pop),
                                                     tot_pop - max(cum_pop))))),
         min_cum_pop = min(cum_pop)) %>% 
  ungroup() %>% 
  group_by(GCAM_region_ID, year) %>% 
  mutate(lag_shortfall = if_else(is.na(lag(shortfall)),0,lag(shortfall))) %>% 
  ungroup() %>% 
  filter(cum_pop == min_cum_pop, category != "q1") %>%
  mutate(shortfall = lag_shortfall) %>% 
  filter(shortfall != 0) %>% 
  dplyr::select(country, year,GCAM_region_ID,category,shortfall) %>%
  rename(shortfall_adj = shortfall)->shortfall_for_adjustment

## D: Now, we will calculate three categories,
## i.) adjusted entries based on the shortfall calculated above
## ii.) new entries with just the shortfall
## iii.) entries which did not have to be adjusted

### adjusted entries based on the shortfall calculated above
processed_data_with_categories %>% 
  left_join(shortfall_for_adjustment, by= c("country","year","GCAM_region_ID","category")) %>% 
  mutate(shortfall_adj = if_else(is.na(shortfall_adj),0, shortfall_adj)) %>% 
  filter(shortfall_adj != 0) %>% 
  group_by(country, year, GCAM_region_ID, category) %>% 
  mutate(min_cum_pop = min(cum_pop)) %>% 
  ungroup() %>% 
  mutate(population = if_else(cum_pop == min_cum_pop, population -shortfall_adj, population))-> population_adjusted_for_shortfall

### new entries with just the shortfall
processed_data_with_categories %>% 
  left_join(shortfall_for_adjustment, by= c("country","year","GCAM_region_ID","category")) %>% 
  mutate(shortfall_adj = if_else(is.na(shortfall_adj),0, shortfall_adj)) %>% 
  filter(shortfall_adj != 0) %>% 
  group_by(country, year, GCAM_region_ID, category) %>% 
  mutate(min_cum_pop = min(cum_pop)) %>% 
  ungroup() %>% 
  filter(cum_pop == min_cum_pop) %>% 
  mutate(population = shortfall_adj) %>% 
  mutate(category= if_else(category=="q5", "q4",
                           if_else(category=="q4", "q3",
                                   if_else(category =="q3", "q2", "q1"))))-> population_new_entries

### entries which did not have to be adjusted
processed_data_with_categories %>% 
  left_join(shortfall_for_adjustment, by= c("country","year","GCAM_region_ID","category")) %>% 
  mutate(shortfall_adj = if_else(is.na(shortfall_adj),0, shortfall_adj)) %>% 
  filter(shortfall_adj == 0) %>% 
  mutate(min_cum_pop = 0)-> population_no_adjustment


## E. Re-calculate quintiles, cutoffs and ensure that problem does not exist
test_data <- bind_rows(population_no_adjustment, population_new_entries, population_adjusted_for_shortfall) %>% 
  dplyr::select(-min_cum_pop, -shortfall_adj) %>%  
  arrange(GCAM_region_ID,year, gdp_ppp_pc_usd2011) %>% 
  group_by(GCAM_region_ID,year) %>%
  mutate(tot_pop = round(sum(population),2),
         tot_gdp = sum(gdp_quintile),
         cum_pop = round(cumsum(population),2),
         category = if_else(cum_pop <= q1_cutoff, "q1",
                            if_else(cum_pop <= q2_cutoff, "q2",
                                    if_else(cum_pop <= q3_cutoff, "q3",
                                            if_else(cum_pop <= q4_cutoff, "q4", "q5"))))) %>% 
  ungroup() %>% 
  group_by(GCAM_region_ID,year, category) %>% 
  mutate(max_cum_pop = max(cum_pop)) %>% 
  ungroup() %>% 
  filter(cum_pop == max_cum_pop) %>% 
  mutate(shortfall = if_else(category=="q1", q1_cutoff - (cum_pop),
                             if_else(category=="q2", q2_cutoff - (cum_pop),
                                     if_else(category=="q3", q3_cutoff - (cum_pop),
                                             if_else(category=="q4", q4_cutoff - (cum_pop),
                                                     tot_pop - (cum_pop)))))) %>%   
  filter(shortfall != 0)

if (nrow(test_data)>0){
  stop("Quintiles are not lined up correctly. Please check calculations above!")
}


## F. Generate final data
final_data <- bind_rows(population_no_adjustment, population_new_entries, population_adjusted_for_shortfall) %>% 
                dplyr::select(-min_cum_pop, -shortfall_adj) %>%  
  arrange(year, gdp_ppp_pc_usd2011, GCAM_region_ID) %>% 
  group_by(GCAM_region_ID,year) %>%
  mutate(cum_pop = round(cumsum(population),2),
         category = if_else(cum_pop <= q1_cutoff, "q1",
                            if_else(cum_pop <= q2_cutoff, "q2",
                                    if_else(cum_pop <= q3_cutoff, "q3",
                                            if_else(cum_pop <= q4_cutoff, "q4", "q5"))))) %>% 
  ungroup() %>%      
  mutate(gdp_quintile = gdp_ppp_pc_usd2011*population) %>%   
  group_by(GCAM_region_ID, year, category) %>% 
  mutate(gdp_pcap_quintile = sum(gdp_quintile)/sum(population),
         population = sum(population)) %>%
   ungroup() %>%
    dplyr::select(GCAM_region_ID, year, category, gdp_pcap_quintile, population,tot_pop, tot_gdp)%>%
    distinct() %>%
    group_by(GCAM_region_ID, year) %>%
    mutate(gdp_pcap = sum(gdp_pcap_quintile)) %>%
    ungroup() %>%
    mutate(shares = gdp_pcap_quintile/gdp_pcap,
           gdp_pcap = tot_gdp/tot_pop) %>%
    dplyr::select(GCAM_region_ID, year, category, shares, gdp_pcap,tot_gdp,tot_pop) %>%
    distinct() %>% compute_gini_GCAM_region_ID() %>% group_by(GCAM_region_ID) %>% 
    mutate(mean_gini= last(gini)) %>% ungroup()

final_data %>% 
  mutate(pop = tot_pop *0.2) %>% 
  mutate(gdp_pcap_quintile = (tot_gdp*shares)/pop) %>% 
  filter(year> 1990) %>% 
  mutate(logged_income = log(gdp_pcap_quintile),
         id = paste0(GCAM_region_ID, year))->test_results

test_results_GCAM_region_ID <- split(test_results,test_results$id)

compute_lognormal_dist_gcam_id <- function(df){
  
  mean <- mean(df$gdp_pcap_quintile)
  gini <- unique(df$gini)
  max_income <- max(df$gdp_pcap_quintile)*5
  len_sample <- 10000
  year <- unique(df$year)
  
  results <- compute_lognormal_dist(mean_income = mean,
                                    gini = gini,
                                    max_income = max_income,
                                    len_sample = len_sample)
  results$GCAM_region_ID <- unique(df$GCAM_region_ID)
  results$year <- year
  return(results)
}

list_results <- lapply(test_results_GCAM_region_ID, compute_lognormal_dist_gcam_id)

log_results <- rbindlist(list_results)

if(print_diagnostics){
g <- ggplot()+
  geom_line(data=log_results %>% left_join(GCAM_regions) %>% filter(region %in% c("India","China","USA","South Africa"), year == 2015) , aes(x=gdp_pcap, y= density),color="blue", linetype ="dashed")+
  scale_x_continuous(labels = unit_format(scale = 0.001, unit="Thous USD"))+
  scale_y_continuous(labels = unit_format(scale = 10000))+
  xlab("GDP per capita")+
  facet_wrap(~region, scales="free")

g + scheme_basic}

log_results %>%
  group_by(GCAM_region_ID, year) %>% 
  mutate(tot_density = sum(density),
         tot_income = sum(density * gdp_pcap),
         cut_off_quintile = tot_density * 0.2,
         cut_off_q1 = cut_off_quintile,
         cut_off_q2 = cut_off_q1 + cut_off_quintile,
         cut_off_q3 =  cut_off_q2 + cut_off_quintile,
         cut_off_q4 =  cut_off_q3 + cut_off_quintile,
         cum_density = cumsum(density)) %>% 
  ungroup() %>% 
  mutate(category = if_else(cum_density < cut_off_q1, "q1",
                            if_else(cum_density < cut_off_q2, "q2",
                                    if_else(cum_density < cut_off_q3, "q3",
                                            if_else(cum_density < cut_off_q4, "q4","q5"))))) %>% 
  group_by(GCAM_region_ID, year, category) %>% 
  mutate(gdp_quintile = sum(density* gdp_pcap),
         shares = gdp_quintile/tot_income,
         gdp_pcap_quintile = sum(density* gdp_pcap)/sum(density)
         ) %>% 
  ungroup() %>% 
  dplyr::select(GCAM_region_ID, year, category, shares, gdp_pcap_quintile) %>% 
  distinct() %>% 
  mutate(model = "Log normal based downscaling")->log_normal_downscaled_results
         
test_results %>% 
  mutate(gini_coeff = 1.0582,
         gini_model_intercept = -0.3856) %>% 
  mutate(PC1_pred = (gini *gini_coeff) + gini_model_intercept) %>% 
  mutate(pred_shares = if_else(category == "q1",
                                 (PC1_pred * pc_loadings[1])+center[1],
                                 if_else(category == "q2",
                                         (PC1_pred * pc_loadings[2])+center[2],
                                         if_else(category == "q3",
                                                 (PC1_pred * pc_loadings[3])+center[3],
                                                 if_else(category == "q4",
                                                         (PC1_pred * pc_loadings[4])+center[4],
                                                         (PC1_pred * pc_loadings[5])+center[5]))))) %>% 
  select(-shares, -gdp_pcap_quintile, -logged_income) %>% 
  spread(category, pred_shares) %>% 
  mutate(q5 = if_else(q1 < 0, q5 - 0.002, q5),
         q4 = if_else(q1 < 0, q4 - 0.002, q4),
         q3 = if_else(q1 < 0, q3 - 0.002, q3),
         q2 = if_else(q1 < 0, q2 - 0.002, q2),
         q1 = if_else(q1 < 0, 1 -(q1+q2+q3+q4+q5), q1)) %>% 
  gather("category", "shares", "q1":"q5") %>% 
  dplyr::select(GCAM_region_ID, category, shares, year) %>% 
  mutate(model = "PCA based downscaling")->PCA_based_downscaling
#bringing the downscaling results together

final_data %>% 
  filter(year > 1990) %>% 
  mutate(model = "Historical data") %>% 
  select(GCAM_region_ID, year, shares, model, category) %>% 
  bind_rows(PCA_based_downscaling %>% select(GCAM_region_ID, year, shares, model,category)) %>% 
  bind_rows(log_normal_downscaled_results %>% select(GCAM_region_ID, year, shares, model,category)) ->consolidated_results 


if(print_diagnostics){
  
  g <- ggplot(data = consolidated_results %>% left_join(GCAM_regions) %>% 
                filter(region %in% c("India", "China", "South Africa", "USA")), aes(x= year, y=shares))+
       geom_line(aes(linetype = model, color= category), size= 1.2, alpha=0.5)+
       scale_color_npg()+
    ylab("Shares of total income")+
    scale_linetype_manual(values = c("solid", "dotted", "dashed"))+
       facet_grid(~region)+
       ggtitle(" Comparison of actual data with downscaling models for selected countries")
  
  
  g + scheme_basic
}

consolidated_results %>% 
  left_join(test_results %>% select(GCAM_region_ID, year, tot_pop,
                                    tot_gdp) %>% distinct(), by =c("GCAM_region_ID", "year")) %>% 
  mutate(pop = tot_pop *0.2,
         gdp_pcap_quintile = ((tot_gdp * shares)/pop)/1000)->consolidated_results_w_income

if(print_diagnostics){
  
  g <- ggplot(data = consolidated_results_w_income %>% left_join(GCAM_regions) %>% 
                filter(region %in% c("India", "China", "South Africa", "USA")), aes(x= year, y=gdp_pcap_quintile))+
    geom_line(aes(linetype = model, color= category), size= 1.2, alpha=0.5)+
    scale_color_npg()+
    ylab("GDP per capita in thousand USD")+
    scale_linetype_manual(values = c("solid", "dotted", "dashed"))+
    facet_grid(~region)+
    ggtitle(" Comparison of income with downscaling models for selected countries")
  
  
  g + scheme_basic
}

final_data %>% 
  mutate(model1_PC_coef = 0.8909799,
         model1_intercept = 0.0030592,
         model1_GDP_coef = -0.0002668) %>% 
  spread(category, shares) %>% 
  mutate(Component1 =((q1-center[1])*pc_loadings[1,1])+
           ((q2-center[2])*pc_loadings[2,1])+
           ((q3-center[3])*pc_loadings[3,1])+
           ((q4-center[4])*pc_loadings[4,1])+
           ((q5-center[5])*pc_loadings[5,1])) %>%
  select(GCAM_region_ID, year, Component1, gdp_pcap,
         model1_PC_coef,model1_intercept,model1_GDP_coef) %>% 
  distinct() %>% 
  arrange(GCAM_region_ID, year) %>%
  group_by(GCAM_region_ID) %>% 
  mutate(lagged = lag(Component1)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  filter(year > 1990) %>% 
  mutate(Pred_Comp = ((gdp_pcap/1000) * model1_GDP_coef)+model1_intercept+ (lagged *model1_PC_coef)) %>% 
  mutate(q1= ((Component1* pc_loadings[1,1])+center[1]),
         q2= ((Component1* pc_loadings[2,1])+center[2]),
         q3= ((Component1* pc_loadings[3,1])+center[3]),
         q4= ((Component1* pc_loadings[4,1])+center[4]),
         q5= ((Component1* pc_loadings[5,1])+center[5]),
         q5 = if_else(q1 < 0, q5 - 0.002, q5),
         q4 = if_else(q1 < 0, q4 - 0.002, q4),
         q3 = if_else(q1 < 0, q3 - 0.002, q3),
         q2 = if_else(q1 < 0, q2 - 0.002, q2),
         q1 = if_else(q1 < 0, 1 -(q1+q2+q3+q4+q5), q1)) %>% 
  gather("category", "shares", "q1":"q5") %>% 
  mutate(model = "Auto regressive + GDP model") %>% 
  select(GCAM_region_ID, year, shares,category, model)->GDP_model_historical_results


if(print_diagnostics){
  
  g <- ggplot(data = consolidated_results %>% bind_rows(GDP_model_historical_results) %>% left_join(GCAM_regions) %>% 
                filter(region %in% c("India", "China", "South Africa", "USA")), aes(x= year, y=shares))+
    geom_line(aes(color= category, linetype = model), size= 1.2, alpha=0.4)+
    scale_color_npg()+
    scale_linetype_manual(values = c("dotdash","solid", "dotted", "longdash", "twodash"))+
    facet_grid(~region)+
    ylim(0, 0.9)
    ggtitle(" Comparison of actual data with downscaling models and forecast model")
  
  
  g + scheme_basic
}


shapiro.test(test_results$logged_income)->shapiro_test_results

bind_rows(population_adjusted_for_shortfall, population_new_entries, population_no_adjustment) %>% filter(GCAM_region_ID == 3, year == 2015)->North_Africa_data

log_normal_data_global <- compute_lognormal_dist(mean_income = mean(North_Africa_data$gdp_ppp_pc_usd2011),
                                                 gini = 0.32,
                                                 max_income = max(North_Africa_data$gdp_ppp_pc_usd2011),
                                                 len_sample = 300000)
g <- ggplot()+
  geom_histogram(data= North_Africa_data, aes(x=gdp_ppp_pc_usd2011,y= ..density..),bins=35, color="black",fill= "white")+
  geom_density(data= North_Africa_data, aes(x= gdp_ppp_pc_usd2011, y= ..density..), color="red")+
  geom_line(data=log_normal_data_global , aes(x=gdp_pcap, y= density),color="blue", linetype ="dashed")+
  scale_x_continuous(labels = unit_format(scale = 0.001, unit="Thous USD"))+
  scale_y_continuous(labels = unit_format(scale = 10000))+
  xlab("GDP per capita per quintile")

g+scheme_basic
#Create plots showing distribution

bind_rows(population_no_adjustment, population_new_entries, population_adjusted_for_shortfall) %>% 
  dplyr::select(-min_cum_pop, -shortfall_adj,-category,-gdp_quintile) %>% 
  group_by(country,year,Category) %>% 
  mutate(population = sum(population)) %>% 
  ungroup() %>% 
  distinct() %>% 
  arrange(year, gdp_ppp_pc_usd2011, GCAM_region_ID) %>% filter(GCAM_region_ID ==15, year ==2015) %>% 
  mutate(country = if_else(grepl("Bosnia",country),"Bosnia",
                           if_else(grepl("Macedonia",country),"Macedonia",
                                         country))) %>% 
  distinct() %>% 
  mutate(inc_grp = paste0(country," ",Category)) %>% 
  gather("cutoff","cutoff_val","q1_cutoff":"q4_cutoff")->plot_data

plot_data %>% select(cutoff, cutoff_val) %>% 
   distinct() %>% mutate(cutoff_val= cutoff_val*0.000001)->cutoff_data

plot_data %>% select(-cutoff, -cutoff_val) %>% distinct()->plot_data_revised
tot_pop <- unique(plot_data_revised$tot_pop)

g <- ggplot()+
     geom_bar(data=plot_data_revised, aes(x=reorder(inc_grp,gdp_ppp_pc_usd2011), y= population),stat="identity")+
     geom_point(data=plot_data_revised, aes(x=reorder(inc_grp,gdp_ppp_pc_usd2011), y= cum_pop,color=Category),size=3)+
#scale_y_continuous()+
  scale_color_npg()+
  xlab("Quntiles sorted from poorest to richest")+
  ylab("population in millions")+
  scale_y_continuous(labels = unit_format(scale = 0.000001, unit="Million"),breaks = c(tot_pop *0.2,tot_pop *0.4,tot_pop *0.6,tot_pop *0.8))+
  ggtitle("Population by quintile (bars) & cumulative population (dots) in Eastern Europe") 

g+scheme_basic

#Tests for lognormality



final_data %>% filter(year==2015, GCAM_region_ID== 17) %>% 
  mutate(share_of_richer_pop = if_else(category== "q1",0.8,
                                if_else(category== "q2", 0.6,
                                        if_else(category== "q3", 0.4,
                                                if_else(category== "q4", 0.2,0.05))))) %>% 
    mutate(pop = tot_pop *0.2,
           gdp_pcap = (tot_gdp*shares)/pop) %>% 
    select(gdp_pcap, pop,category, share_of_richer_pop) %>% 
   mutate(pop = pop/1000000) %>% 
    distinct()->x

g <- ggplot() + 
     geom_histogram(bins=10)

final_data %>% filter(year==2015, GCAM_region_ID== 1)->c_data
mean_income <- mean(x$gdp_pcap)
gini <- unique(c_data$gini)

sd <- 2 * erfinv(gini)
m <- log(mean_income) -((sd^2)/2)

draws3 <- dlnorm(seq(0, 500000, length.out=300000), m, sd)

draw_d <- as.data.frame(draws3)  %>% 
  mutate(gdp_pcap = seq(0, 500000, length.out=300000)) %>% 
  rename(density = draws3 ) 

g <- ggplot(data=x)+
     geom_histogram(data= x, aes(x=gdp_pcap, weight= share_of_richer_pop, y= ..density..),bins=35, color="black",fill= "white")+
     geom_density(data= x, aes(x= gdp_pcap, y= ..density.., weight=share_of_richer_pop), color="red")+
  geom_line(data=draw_d %>% filter(gdp_pcap < 600000), aes(x=gdp_pcap, y= density),color="blue", linetype ="dashed")+
  scale_x_continuous(labels = unit_format(scale = 0.001, unit="Thous USD"))+
  scale_y_continuous(labels = unit_format(scale = 10000))
     
g+scheme_basic
#Part 3: Start PCA analysis to fit a model

final_data %>% select(GCAM_region_ID, year, category, shares, gdp_pcap, mean_gini) %>% 
   spread(category,shares) %>% mutate(id= paste0(GCAM_region_ID,year)) %>% arrange(id)->pc_raw

pc_raw %>%  
  select(-q1,-q2,-q3,-q4,-q5) %>% distinct() %>%mutate(id =paste0(GCAM_region_ID,year)) ->data_comp_PC

data_comp_PC %>% left_join(pc_raw %>% select(GCAM_region_ID,year, q1,q2,q3,q4,q5)
                           , by= c("GCAM_region_ID","year")) %>% 
               mutate(center_q1 = mean(q1),
                      center_q2 = mean(q2),
                      center_q3 = mean(q3),
                      center_q4 = mean(q4),
                      center_q5 = mean(q5)) %>% 
               mutate(Component1 =((q1-center_q1)*pc_loadings[1,1])+
                                    ((q2-center_q2)*pc_loadings[2,1])+
                                    ((q3-center_q3)*pc_loadings[3,1])+
                                     ((q4-center_q4)*pc_loadings[4,1])+
                                      ((q5-center_q5)*pc_loadings[5,1]))->data_comp_PC 

data_comp_PC %>% arrange(id)%>% group_by(GCAM_region_ID) %>%  mutate(lagged1= lag(Component1)) %>%
  ungroup() %>%na.omit()->data_comp_PC_processed

modelPCA1 <- lm(Component1 ~ lagged1+log(gdp_pcap), data=data_comp_PC_processed)

data_comp_PC_processed$model1_intercept <-  0.0030592
data_comp_PC_processed$model1_PC_coef <-    0.8909799
data_comp_PC_processed$model1_GDP_coef <-   -0.0002668

data_comp_PC_processed %>% mutate(Component1_pred = ((gdp_pcap/1000)*model1_GDP_coef)+
                                                    lagged1*model1_PC_coef+
                                                     model1_intercept,
                                  diff_factor = Component1- Component1_pred)->data_comp_PC_processed

data_comp_PC_processed %>%  filter(year == 2015) %>% select(-id) %>% mutate(sce = "SSP1",lagged1=Component1)->data_base_year

data_base_year %>% bind_rows(data_base_year %>% mutate(sce = "SSP2"),
                             data_base_year %>% mutate(sce = "SSP3"),
                             data_base_year %>% mutate(sce = "SSP4"),
                             data_base_year %>% mutate(sce = "SSP5"))->data_base_year_SSPs

GDP_data_forecast <- read.csv("C:/Users/nara836/Downloads/GDP_pcap_forecast_GCAM_regions.csv",stringsAsFactors = FALSE)

GDP_data_forecast_csv <- read.csv("C:/Users/nara836/Downloads/GDP_pcap_forecast_GCAM_regions_smoothed.csv",stringsAsFactors = FALSE) %>% 
                         left_join(GCAM_regions)

g <- ggplot(data= GDP_data_forecast_csv %>% filter(region %in% c("India","China","USA")), aes(x=year, y=Smooted, color=scenario))+ 
     geom_line(size=1.2)+
     facet_wrap(~region)+
     ylab("GDP per capita (2016=1)")+
  scale_color_manual(values= c("dark green","dark blue", "red", "yellow", "light green"))
    

g+ scheme_basic

Rao_et_al_data_forecast <- read.csv("C:/Users/nara836/Downloads/Rao_et_al_sce_data.CSV", stringsAsFactors = FALSE) %>%
                           select(-ref) %>% 
                           group_by(sce, iso) %>% 
                           mutate(Primary = ifelse(is.na(Primary), approx_fun(year,Primary,rule=2),Primary),
                                  Secondary = ifelse(is.na(Secondary), approx_fun(year,Secondary,rule=2),Secondary),
                                  Tertiary = ifelse(is.na(Tertiary), approx_fun(year,Tertiary,rule=2),Tertiary),
                                  GDP = ifelse(is.na(GDP), approx_fun(year,GDP,rule=2),GDP)) %>% 
                          ungroup() %>% 
                          na.omit() %>% 
                          group_by(GCAM_region_ID, sce, year) %>% 
                          mutate(Primary = sum(Primary*Population)/sum(Population),
                                 Secondary = sum(Secondary*Population)/sum(Population),
                                 Tertiary = sum(Tertiary*Population)/sum(Population),
                                 TFP_yoy = sum(as.numeric(TFP_yoy)*GDP)/sum(GDP)) %>% 
                         ungroup() %>% 
                         select(sce, GCAM_region_ID, year, Primary, Secondary, Tertiary, TFP_yoy) %>% 
                         distinct()

Rao_et_al_data_forecast %>% 
  filter(year != 2015) %>% 
  mutate(Component1 = NA,
         Component2 = NA,
         lagged1 = 0,
         lagged2= 0,
         model1_lagged = 0.863079 ,
         model1_intercept = 0.012395,
         model1_TFP_coef =  -0.001787,
         model1_Primary = 0.015218,
         model1_Secondary = -0.021848,
         model1_Tertiary = -0.022568,
         model1_year = -6.806e-05)  %>% 
  arrange(GCAM_region_ID, sce, year) %>% select(-lagged1, -lagged2)->Rao_model_data                          
                           


GDP_data_forecast %>%  rename(gdp_pcap = gdppcap, sce= scenario) %>% select(-POP, -pop,-pop_mil, -value) %>% 
                       mutate(Component1 = NA,
                              Component2 = NA,
                              lagged1 = 0,
                              lagged2= 0,
                              model1_PC_coef = 0.8909799,
                              model1_intercept = 0.0030592,
                              model1_GDP_coef = -0.0002668)  %>% 
                              arrange(GCAM_region_ID, sce, year) %>% select(-lagged1, -lagged2)->GDP_model_data

GDP_model_data %>%  left_join(data_base_year_SSPs %>% 
                                select(GCAM_region_ID, sce, lagged1, diff_factor) %>% 
                                distinct(), by = c("GCAM_region_ID","sce")) %>%
                    #kbn 18th August 2021: Trying to add TFP multiplier
                    left_join(Rao_model_data %>%  select(sce, GCAM_region_ID, year, TFP_yoy) %>% distinct()) %>% 
                    group_by(GCAM_region_ID, sce) %>% 
                    mutate(TFP_Gr = ifelse(is.na(TFP_yoy- lag(TFP_yoy)/lag(TFP_yoy)),0,(TFP_yoy- lag(TFP_yoy)/lag(TFP_yoy))),
                           lagged1 = ifelse(year == 2016, lagged1, NA)) %>% 
                    ungroup() %>% 
                   mutate(id = paste0(GCAM_region_ID, sce), Component1 = NA) %>% arrange(id,year)->GDP_model_data_temp


GDP_model <- function(df){
  for (row in 1:nrow(df)){
    if(df$year[row]==2016){
    df$Component1[row] <- df$model1_intercept[row] + ((df$gdp_pcap[row]/1000) * df$model1_GDP_coef[row]) + (df$model1_PC_coef[row]*df$lagged1[row])+df$diff_factor
    if(df$GCAM_region_ID[row] == 10){
      
      }
    }else{
      
      
    #print(df$year[row])    
    df$Component1[row] <- (df$model1_intercept[row]+ ((df$gdp_pcap[row]/1000) * df$model1_GDP_coef[row])+ (df$model1_PC_coef[row]*(df$Component1[row-1]))+df$diff_factor)
    if(df$GCAM_region_ID[row] == 10){
     print(df$Component1[row-1])
    }
    if(df$year[row]==2030 && df$GCAM_region_ID[row]==24){
      
     write.csv(df, "Data_in_2030.csv") 
      
      
    }
    }
    }
  
  return(df)
  
}



tmp_data <- split(GDP_model_data_temp, GDP_model_data_temp$id)

lapply(tmp_data, GDP_model)->t

df_temp <- rbindlist(t)


df_temp %>% 
                 mutate(q1= ((Component1* pc_loadings[1,1])+center[1]),
                      q2= ((Component1* pc_loadings[2,1])+center[2]),
                      q3= ((Component1* pc_loadings[3,1])+center[3]),
                      q4= ((Component1* pc_loadings[4,1])+center[4]),
                      q5= ((Component1* pc_loadings[5,1])+center[5]),
                      tmp_tot = q1+q2+q3+q4+q5) %>% 
                 select(GCAM_region_ID,q1,q2,q3,q4,q5, sce,year) %>% 
                gather("Category","Income (net)","q1":"q5") %>% compute_gini_GCAM_region_ID_SSP()->GDP_data_forecast_SSPs

#GDP_data_forecast_SSPs %>% 
  
Rao_model <- function(df){
  for (row in 1:nrow(df)){
    if(df$year[row]==2016){
      
      df$Component1[row] <- df$model1_intercept[row]+ (df$TFP_yoy[row] * df$model1_TFP_coef[row]) + (df$Primary[row] * df$model1_Primary[row])+(df$Secondary[row] * df$model1_Secondary[row])+ (df$Tertiary[row]*df$model1_Tertiary[row])+ (df$lagged1[row]*df$model1_lagged[row])+df$diff_factor
                            
      if(df$GCAM_region_ID[row] == 24){
        print(df$model1_intercept[row])
        print(df$lagged1[row])
        #print(df$model1_GDP_coef[row])
        #print(df$gdp_pcap[row])
        print(df$model1_intercept[row]+ (df$TFP_yoy[row] * df$model1_TFP_coef[row]) + (df$Primary[row] * df$model1_Primary[row])
              + (df$Secondary[row] * df$model1_Secondary[row])+ (df$Tertiary[row]*df$model1_Tertiary[row])+ (df$lagged1[row]*df$model1_lagged[row]))
        print(df$Component1[row])
      }
      
      
      
    }else{
      #print(df$year[row])    
      df$Component1[row] <- df$model1_intercept[row]+ (df$TFP_yoy[row] * df$model1_TFP_coef[row]) + (df$Primary[row] * df$model1_Primary[row])+ (df$Secondary[row] * df$model1_Secondary[row])+ (df$Tertiary[row]*df$model1_Tertiary[row])+ (df$model1_lagged[row]*df$Component1[row-1])+df$diff_factor
      if(df$Component1[row] < -0.38){
        
        df$Component1[row] <- -0.38
        
      }
      if(df$GCAM_region_ID[row] == 10){
        #print(df$Component1[row-1])
      }
    }
  }
  
  return(df)
  
  }
  
  


  
Rao_model_data %>%  left_join(data_base_year_SSPs %>% 
                                select(GCAM_region_ID, sce, lagged1, -Component1,diff_factor) %>% 
                                distinct(), by = c("GCAM_region_ID","sce")) %>% 
                                mutate(lagged1 = ifelse(year == 2016, lagged1, NA),
                                       id = paste0(GCAM_region_ID, sce))%>% arrange(id,year)-> Rao_model_data_temp

tmp_data_RAO <- split(Rao_model_data_temp, Rao_model_data_temp$id)

lapply(tmp_data_RAO, Rao_model)->t_rao

df_temp_rao <- rbindlist(t_rao)

df_temp_rao %>% 
  mutate(q1= ((Component1* pc_loadings[1,1])+center[1]),
         q2= ((Component1* pc_loadings[2,1])+center[2]),
         q3= ((Component1* pc_loadings[3,1])+center[3]),
         q4= ((Component1* pc_loadings[4,1])+center[4]),
         q5= ((Component1* pc_loadings[5,1])+center[5]),
         tmp_tot = q1+q2+q3+q4+q5) %>% 
  select(GCAM_region_ID,q1,q2,q3,q4,q5, sce,year) %>% 
  gather("Category","Income (net)","q1":"q5") %>% compute_gini_GCAM_region_ID_SSP()->Rao_data_forecast_SSPs


final_data %>% mutate(`Income (net)` = shares , Category = category, sce = "Historical data") %>% compute_gini_GCAM_region_ID_SSP() %>% 
      select(GCAM_region_ID, sce, year, Category, `Income (net)`, gini) %>% distinct()->historical_data

## Rao et al GINI data processing

Rao_data <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/Rao_et_al_data.csv") %>% 
  mutate(q1= ((Component1* pc_loadings[1,1])+center[1]),
         q2= ((Component1* pc_loadings[2,1])+center[2]),
         q3= ((Component1* pc_loadings[3,1])+center[3]),
         q4= ((Component1* pc_loadings[4,1])+center[4]),
         q5= ((Component1* pc_loadings[5,1])+center[5]),
        tmp_tot = q1+q2+q3+q4+q5) %>% 
  gather("Category","shares","q1":"q5") %>% 
  select(sce, year, region, gini, Category, shares, GCAM_region_ID = Col3) %>% 
  mutate( model= "Rao et al Forecast")

final_data %>% mutate(model1_PC_coef = 0.8963142,
                      model1_intercept = 0.0067474,
                      model1_GDP_coef = -0.0034178) %>% 
                      rename(`1. gini_act` = gini) %>% 
                      spread(category, shares) %>% 
                      mutate(Component1 =((q1-center[1])*pc_loadings[1,1])+
                        ((q2-center[2])*pc_loadings[2,1])+
                        ((q3-center[3])*pc_loadings[3,1])+
                        ((q4-center[4])*pc_loadings[4,1])+
                        ((q5-center[5])*pc_loadings[5,1])) %>% 
               arrange(GCAM_region_ID, year) %>% 
               group_by(GCAM_region_ID) %>% 
               mutate(lagged1 = lag(Component1,1)) %>% 
               ungroup() %>% 
               na.omit() %>% 
               mutate(Component1_pred = (log(gdp_pcap/1000)*model1_GDP_coef)+
                                        (model1_PC_coef*lagged1)+
                                         model1_intercept) %>% 
               #select(-lagged1) %>% 
               gather("Component", "comp_value", "Component1":"Component1_pred") %>% 
               mutate(q1= ((comp_value* pc_loadings[1,1])+center[1]),
                             q2= ((comp_value* pc_loadings[2,1])+center[2]),
                             q3= ((comp_value* pc_loadings[3,1])+center[3]),
                             q4= ((comp_value* pc_loadings[4,1])+center[4]),
                             q5= ((comp_value* pc_loadings[5,1])+center[5])) %>% 
  filter(Component %in% c("Component1_pred")) %>% 
  gather("category","shares","q1":"q5") %>% compute_gini_GCAM_region_ID() %>%
  rename(`2. gini_pred` = gini) %>% 
  select(GCAM_region_ID, year, `1. gini_act`, `2. gini_pred`,Component,comp_value) %>% 
  gather("gini_type",
         "gini_val","1. gini_act":"2. gini_pred") %>% 
  distinct()%>% left_join(GCAM_regions, by = c("GCAM_region_ID"))->t
  


g <- ggplot(data = t%>% select(region, year, gini_type, gini_val) %>% distinct()
            , aes(x=year, y=gini_val, linetype=gini_type))+
     geom_line()+
     facet_wrap(~region, scales="free")+
     ylab("GINI coefficient")+
     ggtitle("Actual vs Predicted GINI coefficient accross GCAM regions (free scales)")

g+ scheme_basic


final_data %>% mutate(model1_PC_coef = 0.8963142,
                      model1_intercept = 0.0067474,
                      model1_GDP_coef = -0.0034178) %>% 
  rename(`1. gini_act` = gini) %>% 
  spread(category, shares) %>% 
  mutate(Component1 =((q1-center[1])*pc_loadings[1,1])+
           ((q2-center[2])*pc_loadings[2,1])+
           ((q3-center[3])*pc_loadings[3,1])+
           ((q4-center[4])*pc_loadings[4,1])+
           ((q5-center[5])*pc_loadings[5,1])) %>% 
  arrange(GCAM_region_ID, year) %>% 
  group_by(GCAM_region_ID) %>% 
  mutate(lagged1 = lag(Component1,1)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(Component1_pred = (log(gdp_pcap/1000)*model1_GDP_coef)+
           (model1_PC_coef*lagged1)+
           model1_intercept) %>% 
  select(-lagged1) %>% 
  gather("Component", "comp_value", "Component1":"Component1_pred") %>% 
  mutate(q1= ((comp_value* pc_loadings[1,1])+center[1]),
         q2= ((comp_value* pc_loadings[2,1])+center[2]),
         q3= ((comp_value* pc_loadings[3,1])+center[3]),
         q4= ((comp_value* pc_loadings[4,1])+center[4]),
         q5= ((comp_value* pc_loadings[5,1])+center[5])) %>% 
  select(GCAM_region_ID, year, Component, comp_value) %>%  
  distinct()%>% left_join(GCAM_regions, by = c("GCAM_region_ID"))->t
# Part 3: validation figures and final data

g <- ggplot(data = t
            , aes(x=year, y=comp_value, linetype=Component))+
  geom_line()+
  facet_wrap(~region, scales="free")+
  ggtitle("Actual and predicted components under the GDP model")+
  ylab("Component value (unitless)")

g+ scheme_basic



### First plot Figures for India from raw data and aggregated data


g <- ggplot(data= GDP_data_forecast_SSPs   %>% bind_rows(historical_data) %>% mutate(shares= `Income (net)`) %>% left_join(GCAM_regions, by = c("GCAM_region_ID"))%>% 
              filter(Category=="q5", region %in% c("Africa_Northern")), aes(x=year, y=shares, color= category))+
  geom_line(aes(color= sce),size=1.2,alpha=0.5)+
  geom_point(aes(color= sce),alpha=0.5)+
  facet_wrap(~region)+
  ylab("Income shares as a percentage of total income")+
  ggtitle("Income distribution shares for Q5 for GCAM regions (Autoregressive + GDP model)")+
  scale_color_manual(values= c("grey","dark green","dark blue", "red", "yellow", "light green"))


g+scheme_basic

GDP_data_forecast_SSPs %>% mutate(model = "GDP") %>% 
  bind_rows(Rao_data_forecast_SSPs %>% mutate(model="Rao et al"))   %>% 
  rename(shares= `Income (net)`) %>% left_join(GCAM_regions, by = c("GCAM_region_ID"))%>%
  select(-GCAM_region_ID) %>% 
  bind_rows(Rao_data %>%  select(-GCAM_region_ID) %>% mutate(region = ifelse(region == "United States of America", "USA",region))) %>% 
  filter(Category=="q5", region %in% c("USA", "India","China"))->plot_data

g <- ggplot(data= plot_data %>% filter(!model %in% c("GDP","Rao et al")), aes(x=year, y=shares, color= sce, linetype=model))+
  #geom_line(size=1.2)+
  geom_point(aes(color= sce))+
  facet_wrap(~region)+
  scale_linetype_manual(values = c("dotted","dashed", "solid"))+
  ylab("Income shares as a percentage of total income")+
  ggtitle("Rao et al. downscaled using GINI function and PCA")+
  scale_color_npg()

g+scheme_basic



g <- ggplot(data= Rao_data_forecast_SSPs  %>% mutate(shares= `Income (net)`*100) %>% left_join(GCAM_regions, by = c("GCAM_region_ID"))%>% 
              filter(Category=="q5", region %in% c("South Africa")), aes(x=year, y=shares, color= category))+
  geom_line(aes(color= sce),size=1.2)+
  geom_point(aes(color= sce))+
  facet_wrap(~region, scales= "free_y")+
  ylab("Income shares as a percentage of total income")+
  ggtitle("Income distribution shares for Q5 for GCAM regions (Autoregressive + Rao et al.)")+
  scale_color_npg()

g+scheme_basic


g <- ggplot(data= GDP_data_forecast_SSPs  %>% bind_rows(historical_data)  %>%   mutate(shares= `Income (net)`*100) %>% left_join(GCAM_regions, by = c("GCAM_region_ID"))%>% 
                filter(Category=="q5") %>% filter(region %in% c("South Africa"))%>% mutate(id = paste0(GCAM_region_ID,sce)) %>% distinct(), aes(x=year, y=gini, group=id, color=sce))+
  #geom_path(size=1.2,linejoin = "round")+
  geom_point(aes(color= sce))+
  facet_wrap(~region)+
  ylab("GINI coefficient")+
  ggtitle("GINI coefficient for GCAM regions (Autoregressive + GDP model)")+
  scale_color_manual(values= c("grey","dark green","dark blue", "red", "yellow", "light green"))

g+scheme_basic


g <- ggplot(data= Rao_data_forecast_SSPs %>% bind_rows(historical_data)  %>% mutate(shares= `Income (net)`*100)  %>% left_join(GCAM_regions, by = c("GCAM_region_ID"))%>% 
              filter(Category=="q5"), aes(x=year, y=gini, color= category, group = sce))+
  geom_line(aes(color= sce),size=1.2)+
  geom_point(aes(color= sce))+
  facet_wrap(~region, scales="free")+
  ylab("GINI coefficient")+
  ggtitle("GINI coefficient for GCAM regions (Autoregressive + Rao et al. model)")+
  scale_color_npg()

g+scheme_basic

g <- ggplot(data= Rao_data_forecast_SSPs %>% mutate(model="Rao et al. + Autoreg") %>%  bind_rows(GDP_data_forecast_SSPs %>% mutate(model="GDP + AutoReg"))  %>% mutate(shares= `Income (net)`*100)  %>% left_join(GCAM_regions, by = c("GCAM_region_ID"))%>% 
              filter(Category=="q5") %>% mutate(id= paste0(GCAM_region_ID, sce,model)), aes(x=year, y=gini, color= sce, group= id))+
  geom_line(size=1.2,aes(linetype= model))+
  geom_line(data= historical_data %>% mutate(model="Data",id= paste0(GCAM_region_ID, sce, model)) %>% left_join(GCAM_regions), size=1.2, color= "grey",linetype= "solid")+
  #geom_point()+
  facet_wrap(~region)+
  scale_linetype_manual(values = c("solid", "dashed"))+
  ylab("GINI coefficient")+
  ggtitle("GINI coefficient for GCAM regions (Autoregressive + Rao et al. model)")+
  scale_color_npg()

g+scheme_basic

ggsave( paste0('C:/Projects/Inequality_data_processing/GINI coefficient for GCAM regions (Autoregressive + Rao et al. model)','.png'),width = 14, height = 14 )


g <- ggplot(data= final_data %>% filter(GCAM_region_ID==17) %>% mutate(shares= shares*100), aes(x=year, y=shares))+
     geom_line(aes(color= category),size=1.2)+
     facet_grid(~GCAM_region_ID)+
     ylab("Income shares as a percentage of total income")+
     ggtitle("Income distribution shares for India (GCAM region no 17)")+
     scale_color_npg()

g+scheme_basic
  
g <- ggplot(data= Wider_data_interpolated %>% filter(country=="India") , aes(x=year, y=Income..net.))+
  geom_line(aes(color= Category),size=1.2)+
  facet_grid(~country)+
  ylab("Income shares as a percentage of total income")+
  ggtitle("Income distribution shares for India from raw WIDER data")+
  scale_color_npg()

g+scheme_basic  

  
### Now plot figures for Eastern Europe and Eastern Europe member countries

g <- ggplot(data= final_data %>% filter(GCAM_region_ID==15) %>% mutate(shares= shares*100), aes(x=year, y=shares))+
  geom_line(aes(color= category),size=1.2)+
  facet_grid(~GCAM_region_ID)+
  ylab("Income shares as a percentage of total income")+
  ggtitle("Income distribution shares for Eastern Europe (GCAM region no 15)")+
  scale_color_npg()

g+scheme_basic

g <- ggplot(data= Wider_data_interpolated %>%   filter(GCAM_region_ID==15) , aes(x=year, y=Income..net.))+
  geom_line(aes(color= Category),size=1.2)+
  facet_grid(~country)+
  ylab("Income shares as a percentage of total income")+
  ggtitle("Income distribution shares for India from raw WIDER data")+
  scale_color_npg()

g+scheme_basic  



g <- ggplot(data= final_data  %>% mutate(shares= shares*100) %>% left_join(GCAM_regions, by = c("GCAM_region_ID")), aes(x=year, y=shares, color= category))+
  geom_line(aes(color= category),size=1.2)+
  geom_point(aes(color= category))+
  facet_wrap(~region)+
  ylab("Income shares as a percentage of total income")+
  ggtitle("Income distribution shares for GCAM regions (with nearest neighbor interpolation)")+
  scale_color_npg()

g+scheme_basic


### Save the data
write.csv(final_data, "Aggregated_quintile_data_normal.csv", row.names = FALSE)

GDP_data <- bind_rows(GDP_data_forecast_SSPs, historical_data) %>% arrange(GCAM_region_ID, sce,year)

write.csv(GDP_data, "Net_income_quintiles_GCAM_regions_1967_2100.csv")            

food_data <- read.csv("C:/Users/nara836/Downloads/US_food_demand.csv", stringsAsFactors = FALSE) %>% 
  bind_rows(read.csv("C:/Users/nara836/Downloads/AfE_food_demand.csv", stringsAsFactors = FALSE))  

food_data %>% group_by(region,year) %>% 
          mutate(Tot_cal = sum(value)) %>% 
          ungroup() %>% 
          group_by(region,year, input) %>% 
          mutate(Tot_input = sum(value)) %>% 
          ungroup() %>% 
          mutate(shares = value/Tot_input)->food_data_processed 
compute_gini_food <- function(df){
  
  df %>% 
    distinct() %>% 
    mutate(share_of_richer_pop = if_else(gcam.consumer== "FoodDemand_Group1",0.8,
                                         if_else(gcam.consumer== "FoodDemand_Group2", 0.6,
                                                 if_else(gcam.consumer== "FoodDemand_Group3", 0.4,
                                                         if_else(gcam.consumer== "FoodDemand_Group4", 0.2,0))))) %>% 
    mutate(score = (shares) *(0.2+ (2*share_of_richer_pop))) %>%   
    group_by(region,year, input) %>% 
    mutate(gini= 1- sum(score)) %>% 
    ungroup() %>% 
    select(-score, -share_of_richer_pop) %>%
    distinct()->df         

  return(df)}

food_data_processed %>% compute_gini_food() %>% 
   select(year, region, input, gini) %>% 
   distinct()->plot_data


g <- ggplot(data= plot_data, aes(x=year, y=gini, color=input))+
     geom_point()+
     geom_line()+
     facet_wrap(~region)+
     ggtitle("Caloric GINI for USA and Africa Eastern for Staples and Non-Staples")

g+ scheme_basic
