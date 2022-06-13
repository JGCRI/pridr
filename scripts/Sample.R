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
pca_stats <- get_PCA_loadings(Wider_data_full,category_col = "Category",
                              value_col = "Income..net.")

pca_stats_plot <- pca_stats %>% mutate(id="A")

pca_stats_plot$Category_f <- factor(pca_stats_plot$Category, levels=c('d1','d2','d3','d4',
                                                                     'd5','d6','d7','d8',
                                                                     'd9','d10'))

g <- ggplot(data=pca_stats_plot, aes(group=id))+
     geom_point(data=pca_stats_plot, aes(x=factor(Category,levels =c('d1','d2','d3','d4',
                                                                     'd5','d6','d7','d8',
                                                                     'd9','d10')), y=PC1,color="PC1"),show.legend = FALSE)+
     geom_line(data=pca_stats_plot, aes(x=factor(Category,levels =c('d1','d2','d3','d4',
                                                                    'd5','d6','d7','d8',
                                                                    'd9','d10')), y=PC1,color="PC1"))+
  geom_point(data=pca_stats_plot, aes(x=factor(Category,levels =c('d1','d2','d3','d4',
                                                                  'd5','d6','d7','d8',
                                                                  'd9','d10')), y=PC2,color="PC2"),show.legend = FALSE)+
  geom_line(data=pca_stats_plot, aes(x=factor(Category,levels =c('d1','d2','d3','d4',
                                                                 'd5','d6','d7','d8',
                                                                 'd9','d10')), y=PC2,color="PC2"))+
  
  xlab("Decile")+
  ylab("Component loading")

g+scheme_basic
pc_center_sd <- get_sd_center(Wider_data_full,category_col = "Category",value_col = "Income..net.")

Wider_data_full %>% compute_components(center_and_scaler_data = pc_center_sd,
                                        pc_loadings = pca_stats,category_col = "Category",
                                        value_col = "Income..net.") %>% 
                    get_deciles_from_components(use_second_comp = TRUE,
                                                pc_loadings = pca_stats,
                                                center_and_scaler = pc_center_sd,
                                                category_col = "Category") %>% 
                    adjust_negative_predicted_features()->data_comp_2

g <- ggplot()+
     geom_line(data=data_comp_2 %>% filter(country=="United States"), aes(x=year,y=Component1,color="PC1"))+
  geom_line(data=data_comp_2 %>% filter(country=="United States"), aes(x=year,y=Component2,color="PC2"))+
  geom_point(data=data_comp_2 %>% filter(country=="United States", year==2010), aes(x=year,y=Component1,color="PC1"),size=4,show.legend = FALSE)+
  geom_point(data=data_comp_2 %>% filter(country=="United States", year==2010), aes(x=year,y=Component2,color="PC2"),size=4,show.legend = FALSE)+
   ylab("Component values in USA")

g+scheme_basic
     
g <- ggplot(data=Wider_data_full,aes(color=country))+
     geom_point(data=Wider_data_full %>% filter(country%in% c("Norway","South Africa"), year==2015), aes(x=factor(Category,levels =c('d1','d2','d3','d4',
                                                                                               'd5','d6','d7','d8',
                                                                                               'd9','d10')), y=Income..net.*100,group=country))+
  geom_line(data=Wider_data_full %>% filter(country%in% c("Norway","South Africa"), year==2015), aes(x=factor(Category,levels =c('d1','d2','d3','d4',
                                                                                                      'd5','d6','d7','d8','d9','d10')), y=Income..net.*100,group=country))+
  xlab("Decile")+
  ylab("Income Share")
g+scheme_basic

Wider_data_full %>% 
  group_by(country,year) %>% 
  arrange(Income..net.) %>% 
  mutate(cum_inc = cumsum(Income..net.),
         cum_inc = if_else(cum_inc > 1,1, cum_inc)) %>% 
  ungroup()->cumulative_incomes


g <- ggplot(data=cumulative_incomes,aes(color=country))+
  geom_point(data=cumulative_incomes %>% filter(country%in% c("Norway","South Africa"), year==2015), aes(x=factor(Category,levels =c('d1','d2','d3','d4',
                                                                                                                                  'd5','d6','d7','d8',
                                                                                                                                  'd9','d10')), y=cum_inc,group=country))+
  geom_line(data=cumulative_incomes %>% filter(country%in% c("Norway","South Africa"), year==2015), aes(x=factor(Category,levels =c('d1','d2','d3','d4',
                                                                                                                                 'd5','d6','d7','d8','d9','d10')), y=cum_inc,group=country))+
  xlab("Decile")+
  ylab("Cumulative Income Share")
g+scheme_basic



Wider_data_full %>% compute_components(center_and_scaler_data = pc_center_sd,
                                       pc_loadings = pca_stats,category_col = "Category",
                                       value_col = "Income..net.") %>% 
  get_deciles_from_components(use_second_comp = FALSE,
                              pc_loadings = pca_stats,
                              center_and_scaler = pc_center_sd,
                              category_col = "Category") %>% 
  adjust_negative_predicted_features()->data_comp_1

Wider_data_full %>% 
  left_join(data_comp_1 %>% select(country,year,Category,pred_shares,Component1)) %>% 
  filter(Category=="d10")->p_comparison

g <- ggplot(data=p_comparison, aes(x=Income..net.,y=Component1))+
     geom_point(color="grey")+
     facet_wrap(~Category,scales="free")+
     xlab("Actual decile shares (Percentage of total income)")+
     ylab("Decile shares from PCA algorithm (Single Component)")+
     geom_abline(slope=1,linetype="dashed",size=1.2,color="black")

g+scheme_basic


model_d10 <- lm(data=p_comparison,formula="Component1~Income..net.")
data_comp_2 %>% 
  spread(Category,pred_shares) %>% 
  mutate(palma_ratio = d10/(d1+d2+d3+d4)) %>% 
  gather("Category","pred_shares","d1":"d9")->data_comp_2

data_comp_2 %>% left_join(data_comp_1 %>% select(shares_w_comp1 = pred_shares,country,year,Category,-Component2)) %>% 
  #filter(category %in% c("d9")) %>% 
  #group_by(country,category) %>% 
  mutate(abs_perc_error = ((((pred_shares-shares_w_comp1))))) %>% 
  ungroup() %>% 
  #select(country,abs_perc_error,year) %>% 
  distinct() %>% 
  mutate(category=Category) %>% 
  compute_gini_deciles(grouping_variables = c("country","year"), inc_col = "pred_shares")->t

g <- ggplot(data= t %>% filter(Category=="d10"), aes(x=output_name, y=palma_ratio))+
     geom_point()+
     xlab("GINI")+
     ylab("palma ratio")

g+scheme_basic



g <- ggplot(data=t, aes(x=(Component2), y= abs_perc_error*100))+
     geom_point()+
     xlab("Absolute value of Component2")+
     ylab("Difference in perc points")+
     facet_wrap(~Category, scales="free")

g+scheme_basic

t %>% #filter(year %in% c(2010:2015)) %>% 
  filter(Category %in% c("d1","d8","d9","d7"))%>% 
  select(country,year,Component2,abs_perc_error,Category) %>%  
  group_by(country,year) %>% 
  mutate(abs_perc_error=sum(abs_perc_error)) %>% 
  ungroup() %>% 
  select(-Category)%>% distinct() %>% 
  mutate(abs_Comp2 = abs(Component2))->base_year_data

base_year_data %>% 
  filter(abs_Comp2 < 4) %>% 
  mutate(obs=1) %>% 
  group_by(country) %>% 
  mutate(obs = sum(obs)) %>% 
  ungroup() %>% 
  filter(obs >= 2)->anomalies

anomalies %>% filter(Category != "d10")->p

Wider_data_full %>% 
  left_join(data_comp_2 %>% select(country,year,Component2,Component1) %>% distinct()) %>% 
  filter(country %in% c(unique(anomalies$country))) %>% distinct() %>% filter(Category =="d9") %>% 
  mutate(obs=1) %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  mutate(gdp_gr =(gdp_ppp_pc_usd2011- lag(gdp_ppp_pc_usd2011))/lag(gdp_ppp_pc_usd2011),
         obs=sum(obs)) %>% 
  ungroup() %>% 
  filter(obs >= 3,
         gdp_gr < 1)->anomaly_data

g <- ggplot(data=anomaly_data, aes(x=life_exp, y=Component2))+
     geom_point()

g+scheme_basic

Consolidated_data <- read.csv("C:/Projects/Inequality_data_processing/software_repo/GCAM_Income_Distributions/Code/Consolidated_data.csv",
                              stringsAsFactors = FALSE) %>% 
                    select(-Component2) %>% 
                     #filter(country %in% c(unique(anomalies$country))) %>%
                    filter(Category =="d9") %>% 
                     select(-Category) %>% 
                    left_join(data_comp_2 %>% filter(Category=="d1") %>% select(country,year,Component1,pred_shares,Component2,palma_ratio) %>% distinct()) %>% 
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
                     

tree_data <- Consolidated_data %>% dplyr::select(-population,-incomegroup,-avg_gdp,
                                                 -lagged_comp2,-pred_shares,-country,-iso,-ref,-id)

tree <- rpart("Component2~.",data=tree_data %>% select(-gini) ,method = "anova")
plot(tree)
text(tree)

g <- ggplot(data=Consolidated_data, aes(x=Component2, y=lagged_palma_ratio))+
     geom_point()

g+scheme_basic

tree_data %>% arrange(year) ->tree_data

head(tree_data,n=nrow(tree_data)/2)->train_data
test_model <- lm(" gdp_gr~ Income..net.",data= train_data %>% mutate(capsh= 1-labsh) )
#train_data$gdp_ppp_pc_usd2011
summary(test_model)
BIC(test_model)
AIC(test_model)
Consolidated_data %>% filter(year > 2004) %>% arrange(country,year)->test_data 

test_data %>% mutate(pred_Comp2 = -17.12887 +(112.73476*lagged_ninth_decile)+(labsh*1.09037)+(lagged_palma_ratio * -0.36292))->test_data_pred
m <- lm(data=test_data_pred,formula = "Component2 ~ pred_Comp2")

g <- ggplot(data=test_data_pred, aes(x=Component2,y=pred_Comp2))+geom_point()+geom_abline(slope = 1, color= "black", linetype = "dashed")+
     ylab("Predicted Component2")+
     xlab("Actual Component 2")
g+scheme_basic


Consolidated_data$pred_Comp1 <- 7.469648+(Consolidated_data$gdp_ppp_pc_usd2011*-0.057345)+(Consolidated_data$labsh*-5.303954)+(Consolidated_data$Secondary_prct*-7.108874)

Consolidated_data %>% filter(year >=2004) %>% 
  mutate(Component1 = (gini*29.71708)-11.4815 ,
         Component2 = -17.77579 + (0.00998*life_exp)+(labsh*0.99944)+(112.35374*lagged_ninth_decile)+(lagged_palma_ratio*-0.34552)) %>% 
  get_deciles_from_components(pc_loadings = pca_stats,
                              center_and_scaler = pc_center_sd,
                              category_col = "Category",
                              grouping_variables = c("country","year"),
                              use_second_comp = TRUE) %>% 
  inner_join(Wider_data_full %>% select(country,year,Income..net.,Category))->final_test_data 

g <- ggplot(data=final_test_data, aes(x=Income..net.*100,y=pred_shares*100)) + 
     geom_point(color="grey")+
    geom_abline(slope = 1,linetype="dashed")+
     facet_wrap(~Category,scales = "free")+
     xlab("Actual shares")+
     ylab("Predicted shares")

g+scheme_basic

t %>% filter(Category=="d9")->t2
hist(t2$abs_perc_error)
t %>% filter(abs_perc_error>=0.1)->t_data

data_comp_2 %>% left_join(data_comp_1 %>% select(shares_w_comp1 = pred_shares,country,year,Category)) %>% 
  left_join(Wider_data_full %>% select(country,year,Income..net.,Category))->country_data

g <- ggplot(data=country_data %>% filter(country %in% c("United States"),Category %in% c("d1","d10")))+
     geom_line(aes(x=year,y=Income..net.),linetype="solid",color="black")+
     geom_line(aes(x=year,y=pred_shares),linetype="dotted",color="red")+
     geom_line(aes(x=year,y=shares_w_comp1),linetype="dotted",color="blue")+
  geom_point(aes(x=year,y=Income..net.),color="black")+
  geom_point(aes(x=year,y=pred_shares),color="red")+
  geom_point(aes(x=year,y=shares_w_comp1),color="blue")+
     facet_wrap(~Category+country)+
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
state_level_forecast %>% left_join(GDP_data %>% select(-state) %>% mutate(state=state_name)) %>% 
  mutate(pop_decile= Population*0.1,
         GDP=GDP_PC_2011_USD*Population,
         gdp_pc_decile = (GDP*shares)/pop_decile) %>% 
  group_by(sce,year) %>% 
  arrange(gdp_pc_decile) %>% 
  mutate(nat_gdp= sum(GDP*shares),
         nat_pop=sum(pop_decile),
         share_of_population= cumsum(pop_decile)/nat_pop,
         share_of_gdp = cumsum(GDP*shares)/nat_gdp) %>% 
  ungroup()->lorenz_curve_data


