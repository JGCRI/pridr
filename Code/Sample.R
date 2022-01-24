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


#1. Get data
Wider_data_full <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/WIDER_aggregated_deciles.csv",stringsAsFactors = FALSE) %>% 
  arrange(year)

data_for_pc_full <- Wider_data_full  %>% 
  dplyr::select(country,year,Category,Income..net.,gini,iso) %>% 
  mutate(Income..net. = Income..net.,
         ISO = toupper(iso)) %>% 
  select(-iso) %>% 
  spread(Category,Income..net.) 


#Run PCA and save stats
pca_stats <- run_PCA(data_for_pc_full,start_index =5)
summary(pca_stats)
#Compute using GINI model

data_for_pc_full %>% compute_components(pc_loadings = pca_stats$rotation)%>% 
  get_deciles_from_components(pc_loadings = pca_stats$rotation,
  center = get_center_PCA(data_for_pc_full),
  scaler = get_sd(data_for_pc_full),
  use_second_comp = TRUE) %>% 
  left_join(Wider_data_full %>% select(country,year,category=Category,Income..net.)) %>% 
  compute_gini_deciles(inc_col = "pred_shares",grouping_variables = c("country","year"))->data_comp_2

data_for_pc_full %>% compute_components(pc_loadings = pca_stats$rotation)%>% 
  get_deciles_from_components(pc_loadings = pca_stats$rotation,
                              center = get_center_PCA(data_for_pc_full),
                              scaler = get_sd(data_for_pc_full),
                              use_second_comp = FALSE) %>% 
  left_join(Wider_data_full %>% select(country,year,category=Category,Income..net.)) %>% 
  compute_gini_deciles(inc_col = "pred_shares",grouping_variables = c("country","year"))->data_comp_1

data_comp_2 %>% left_join(data_comp_1 %>% select(shares_w_comp1 = pred_shares,country,year,category)) %>% 
  #filter(category %in% c("d9")) %>% 
  group_by(country,year) %>% 
  mutate(abs_perc_error = sum(abs(((pred_shares-shares_w_comp1))))) %>% 
  ungroup() %>% 
  #select(country,abs_perc_error,year) %>% 
  distinct()->t

t %>% filter(category=="d9")->comp_data

g <- ggplot(data=comp_data, aes(x=abs_perc_error*100, y= abs(Component2)))+
     geom_point()+
     xlab("Absolute difference in perc points (summed across all deciles)")+
     ylab("Absolute value of Component2")

g+scheme_basic

hist(t$abs_perc_error)
t %>% filter(abs_perc_error>=0.1)->t_data

data_comp_2 %>% left_join(data_comp_1 %>% select(shares_w_comp1 = pred_shares,country,year,category))->country_data

g <- ggplot(data=country_data %>% filter(country %in% c("Malawi"),category %in% c("d9","d1","d10")))+
     geom_line(aes(x=year,y=Income..net.),linetype="solid",color="black")+
     geom_line(aes(x=year,y=pred_shares),linetype="dotted",color="red")+
     geom_line(aes(x=year,y=shares_w_comp1),linetype="dotted",color="blue")+
  geom_point(aes(x=year,y=Income..net.),color="black")+
  geom_point(aes(x=year,y=pred_shares),color="red")+
  geom_point(aes(x=year,y=shares_w_comp1),color="blue")+
     facet_wrap(~category+country)+
     ylab("Decile value")

g+scheme_basic

g <- ggplot(data=data_comp_1 %>% filter(category=="d10"), aes(x=gini,y=output_name)) + 
     geom_point(color="grey")+
     xlab("Actual GINI")+
     ylab("GINI from PCA")+
     geom_abline(slope = 1, linetype="dashed")

g+scheme_basic

Wider_data_full %>% left_join(data_comp_1 %>% select(country,year,Component1,Component2) %>% distinct()) %>% 
  filter(Category=="d10") %>% 
  arrange(country,year) %>% 
  group_by(country) %>% 
  mutate(lag1 =lag(Component1),
         lag2 = lag(Component2)) %>% 
  distinct() %>% ungroup() %>% na.omit()->model_data

model <- lm(formula = 'Component1 ~ ',data=model_data)


data_pred <- GINI_downscaling_model(data_for_pc_full,grouping_variables = "country",historical_fit = TRUE,
                                    coeff = 29.71708,intercept = -11.4815,raw_data = data_for_pc_full,start_index =5)

Wider_data_full %>% 
  left_join(data_pred %>% select(country,year,pred_shares,Category=category,Component1) %>% distinct(),
            by=c("country","year","Category"))->data_for_plot


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

g <- ggplot(data=data_for_plot, aes(x=Income..net.,y=pred_shares))+
     geom_point(color="grey")+
  geom_abline(slope = 1, linetype="dashed")+
     facet_wrap(~Category,scales = "free")+
     xlab("Actual values")+
     ylab("Predicted using GINI downscaling model")

g+scheme_basic



Rao_gini_data <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/Rao_et_al_allGINI.csv", 
                          stringsAsFactors = FALSE) %>% 
                 gather("iso","gini","ABW":"ZWE") %>% 
                 mutate(gini=gini/100,
                        ISO=iso) 

data_for_pc_full_2 <- Wider_data_full  %>% 
  dplyr::select(country,year,Category,Income..net.,gini,iso) %>% 
  mutate(ISO = toupper(iso)) %>% 
  spread(Category,Income..net.) %>% 
  select(-iso)


Rao_deciles_processed <- GINI_downscaling_model(Rao_gini_data,
                                                coeff = 29.72,
                                                intercept = -11.48,
                                                raw_data = data_for_pc_full,
                                                grouping_variables = c("ISO"),
                                                historical_fit = FALSE,
                                                start_index =5)

Rao_deciles_processed_GINI <- compute_gini_deciles(Rao_deciles_processed,
                                                   inc_col = "pred_shares",
                                                   grouping_variables = c("ISO","scenario","year")
                                                   )
Rao_deciles_processed_GINI %>% mutate(diff = abs(output_name-gini)/gini) %>% na.omit()->diff_data

g <- ggplot(data=diff_data %>% filter(category=="d10"), aes(x=gini,y=output_name))+
    geom_point(color="grey")+
    xlab("Rao et al. GINI")+
    ylab("Predicted GINI using Downscaling model")+
    facet_wrap(~scenario)

g+scheme_basic
