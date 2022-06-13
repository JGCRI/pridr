library(dplyr)

# First get US data


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


US_agg_data <- read.csv("C:/Projects/Inequality_data_processing/software_repo/GCAM_Income_Distributions/Code/GCAM_32_regions_Income_deciles_SSPs_1967_2100_Rao_et_al_downscaled.csv"
                        , stringsAsFactors = FALSE) %>% filter(GCAM_region_ID ==1, sce=="Historical data") %>%
              rename(Category = category)

Wider_data_full <- read.csv("C:/Projects/Inequality_data_processing/Input_Data/WIDER_aggregated_deciles.csv",stringsAsFactors = FALSE) %>%
    arrange(year)

pc_loadings_matrix <- get_PCA_loadings(Wider_data_full,category_col = "Category",value_col = "Income..net.")

pc_center_sd <- get_sd_center(Wider_data_full , category_col = "Category", value_col = "Income..net.")

compute_components(US_agg_data,center_and_scaler_data = pc_center_sd, pc_loadings = pc_loadings_matrix, value_col = "shares",
                   grouping_variables = c("GCAM_region_ID","year"),category_col = "Category")->Components_US



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


state_gini<- read.csv("C:/Projects/Inequality_data_processing/GODEEP/State_level_GINI.csv", stringsAsFactors = FALSE) %>%
             rename(year = ï..year)

state_level_deciles <- read.csv("C:/Projects/Inequality_data_processing/GODEEP/State_level_GINI.csv", stringsAsFactors = FALSE) %>%
                      rename(year = ï..year) %>%
                       left_join(Components_US, by = c("year")) %>%
                       #group_by(year) %>%
                       mutate(Component2 = ifelse(is.na(Component2),approx_fun(year,Component2,rule = 2),Component2),
                              Component1 = (d10_value*35.76)-11.13) %>%
                      select(-GCAM_region_ID) %>%
                      distinct() %>%
                      get_deciles_from_components(pc_loadings = pc_loadings_matrix, center_and_scaler = pc_center_sd,category_col = "Category",
                                                  grouping_variables = c("state","year"),use_second_comp = TRUE) %>%
                     adjust_negative_predicted_features(grouping_variables = c("state","year")) %>%
                     mutate(category = Category) %>%
                     compute_gini_deciles(inc_col = "pred_shares",grouping_variables = c("state","year")) %>%
                     left_join(state_gini %>% select(state, year, gini), by = c("state","year"))

g <- ggplot(data=state_level_deciles %>% filter(Category=="d10"), aes(x=year,y=output_name,group=state))+
     geom_line(color="grey")

g+scheme_basic

g <- ggplot(data=state_level_deciles, aes(x=gini, y=output_name))+
     geom_point()+
     xlab("Historical GINI")+
     ylab("GINI based on calculated deciles")

g + scheme_basic

STATE_DATA <- read.csv("C:/Projects/Inequality_data_processing/State_level_deciles.csv", stringsAsFactors = FALSE)

g <- ggplot(data=STATE_DATA, aes(x=shares_actual,y=gini))+geom_point()

g+scheme_basic

US_agg_data %>% filter(year==2015)->US_2015_data

US_forecast_data <- read.csv("C:/Projects/Inequality_data_processing/software_repo/GCAM_Income_Distributions/Code/GCAM_32_regions_Income_deciles_SSPs_1967_2100_Rao_et_al_downscaled.csv"
                        , stringsAsFactors = FALSE) %>% filter(GCAM_region_ID ==1, year > 2015) %>%
    rename(Category = category) %>%
    arrange(year) %>%
    mutate(gini_2015= unique(US_2015_data$gini),
           gini_scaler = gini/gini_2015)

state_level_deciles %>% filter(year==2015) %>%
                     select(-year) %>%
                     mutate(reg="US",
                            gini=output_name) %>%
                     left_join(US_forecast_data %>% select(year,sce,gini_scaler) %>% distinct() %>% mutate(reg="US")) %>%
                     select(state,gini,year,sce,gini_scaler,Component2,Component1) %>%
                    rename(Component1_act= Component1) %>% 
                     mutate(gini=gini*gini_scaler,
                            gini= pmin(gini,0.93),
                            Component1 = (gini*29.717083)-11.48152) %>%
                     #filter(year <= 2050) %>%
                     get_deciles_from_components(pc_loadings = pc_loadings_matrix,center_and_scaler = pc_center_sd,category_col = "Category",
                                                 grouping_variables = c("state","year","sce")) %>%
                     adjust_negative_predicted_features(grouping_variables = c("state","year","sce")) %>%
                     mutate(category= Category) %>%
                    compute_gini_deciles(inc_col = "pred_shares",grouping_variables = c("state","year","sce")) %>%
                    rename(gini=output_name,shares=pred_shares) %>%
                   select(-Component2,-Component1,-share_of_richer_pop,-Category,-score)->state_level_forecast

state_level_forecast %>% select(state,year,sce,gini) %>% distinct() %>% filter(state %in% c("New York","West Virginia"),
                                                                               sce %in% c("SSP2"))->dat_plot

g <- ggplot(data=dat_plot, aes(x=year,y=gini,color=state))+
     geom_line()+
     scale_color_manual(values=c("blue","orange"))

g+scheme_basic

state_level_forecast %>%
    bind_rows(state_level_deciles %>% select(-gini,-Component2,-Component1,-share_of_richer_pop,-Category,-score) %>%
                  rename(gini=output_name,
                         shares=pred_shares)%>% mutate(sce="Historical data"))%>%  distinct()->consolidated_state_level_data

write.csv(consolidated_state_level_data, "US_50_states_DC_net_income_deciles_1917_2050_SSP_updated.csv", row.names = FALSE)

GDP_data <- read.csv("C:/Projects/Inequality_data_processing/GODEEP/GDP_percapita_US_states.csv", stringsAsFactors = FALSE)

state_names <- read.csv("C:/Projects/Inequality_data_processing/GODEEP/state_names.csv", stringsAsFactors = FALSE)

GDP_data %>% left_join(state_names)->GDP_data


GDP_model_state <- function(df){
    df %>% arrange(year)->df
    for (row in 1:nrow(df)){
        if(df$year[row]==2020){
            df$Component1[row] <- 0.18943 + ((df$GDP_PC_2011_USD[row]/1000) * -0.01031) + (df$lag_Comp1[row]*0.90520)+df$diff_factor[row]
            
          
        }else{  
            df$Component1[row] <- 0.18943 + ((df$GDP_PC_2011_USD[row]/1000) * -0.01031) + (df$Component1[row-1]*0.90520)+df$diff_factor[row]
            
            
        }
    }
    
    return(df)  
}


state_level_deciles %>% group_by(state,Category)%>%
    arrange(year) %>% 
    mutate(lag_Comp1 = lag(Component1)) %>% 
    ungroup() %>% 
    filter(year==2015,Category=="d10") %>%
    left_join(GDP_data %>% select(-state) %>% rename(state=state_name) %>% filter(year==2015)) %>% 
    mutate(diff_factor = (0.18943+ ((GDP_PC_2011_USD/1000)*-0.01031) + (lag_Comp1*0.90520))-Component1) %>% 
    select(-year,-GDP_PC_2011_USD) %>%
    mutate(reg="US") %>%
    left_join(US_forecast_data %>% select(year,sce,gini_scaler) %>% distinct() %>% mutate(reg="US")) %>%
    select(state,gini,year,sce,gini_scaler,Component2,lag_Comp1,diff_factor,Component1) %>%
    left_join(GDP_data %>% select(-state) %>% rename(state=state_name)) %>% 
    group_by(state,sce) %>% 
    arrange(year) %>% 
    ungroup() %>% 
    mutate(id= paste0(state,sce)) %>% 
    filter(year %in% c(seq(2015,2100, by =5)))->data_for_gdp_model

list <- split(data_for_gdp_model, data_for_gdp_model$id)


lapply(list, GDP_model_state)->t

df_temp <- rbindlist(t)

df_temp <- as.data.frame(df_temp)

df_temp %>% get_deciles_from_components(pc_loadings = pc_loading_matrix,center_and_scaler = pc_center_sd,
                                        category_col = "Category",
                                        grouping_variables = c("state","year","sce")) %>% 
           adjust_negative_predicted_features(grouping_variables = c("state","year","sce")) %>% 
           mutate(category = Category) %>% 
           compute_gini_deciles(inc_col = "pred_shares",grouping_variables = c("state","sce","year"))->d

consolidated_state_level_data %>% 
    filter(sce != "Historical data") %>% 
    bind_rows(consolidated_state_level_data %>% filter(sce=="Historical data") %>% mutate(sce="SSP1"),
              consolidated_state_level_data %>% filter(sce=="Historical data") %>% mutate(sce="SSP2"),
              consolidated_state_level_data %>% filter(sce=="Historical data") %>% mutate(sce="SSP3"),
              consolidated_state_level_data %>% filter(sce=="Historical data") %>% mutate(sce="SSP4"),
              consolidated_state_level_data %>% filter(sce=="Historical data") %>% mutate(sce="SSP5"))->updated_state_level_data




g <- ggplot(data=updated_state_level_data %>% filter(category=="d10",year>2000), aes(x=year,y=gini,color=sce,group=paste0(state,sce)))+
     geom_line(size=1.3)+
     geom_line(data=consolidated_state_level_data %>% filter(category=="d10",sce=="Historical data",year > 2000,year <2020), aes(x=year,y=gini,group=paste0(state,sce)),color="grey",
               show.legend = FALSE,size=1.3)+
     facet_wrap(~state)+
     scale_color_npg()
g+scheme_basic

g <- ggplot(data=df_temp, aes(x=year,y=GDP_PC_2011_USD/1000))+
     geom_line()+
     facet_wrap(~state)+
     xlab("year")+
     ylab("GDP per capita in thousand USD")

g+scheme_basic

d %>% filter(year <=2050)->d
g <- ggplot()+
    geom_line(data=d %>% filter(sce=="SSP2",Category=="d10"), aes(x=year,y=output_name,group=state),color="grey")+
    geom_boxplot(data=d %>% filter(year %in% c(2020,2050,2100),sce=="SSP2"),aes(x=year,y=output_name,group=paste0(year)),width = 0.9)+
    stat_boxplot(geom = "errorbar")+
    ylab("Predicted GINI using GDP model")

g+scheme_basic



g <- ggplot(data=d %>% filter(year %in% c(2050),Category=="d10",sce=="SSP2"), aes(output_name,fill=sce))+
    geom_histogram(show.legend = FALSE)+
    #geom_density(show.legend = FALSE)+
    xlab("GINI coefficients in 2050")+
    xlim(0.1,0.75)


g+scheme_basic


g <- ggplot(data=updated_state_level_data %>% filter(year %in% c(2050),category=="d10"), aes(gini,fill=sce))+
     geom_histogram()+
    facet_wrap(~sce)+
    scale_fill_npg()+
    geom_density()+
    xlab("GINI coefficients in 2050")

    
g+scheme_basic


g <- ggplot(data=lorenz_curve_data %>% filter(year %in% c(2030,2050,2100)), aes(x=share_of_population, y=share_of_gdp,color=sce))+
    geom_line(size=1.3)+
    scale_color_npg()+
    facet_wrap(~year)+
    geom_abline(slope=1,color="grey",linetype="dashed",size=1.3)+
    xlab("Share of Population")+
    ylab("Share of GDP")+
    geom_point(data=lorenz_curve_data %>% filter(year %in% c(2030,2050,2100),
                                                 state %in% c("New York"),
                                                 category=="d5"),aes(x=share_of_population, y=share_of_gdp,shape="New York"),color="black",size=1.9)+
    geom_point(data=lorenz_curve_data %>% filter(year %in% c(2030,2050,2100),
                                                 state %in% c("West Virginia"),
                                                 category=="d5"),aes(x=share_of_population, y=share_of_gdp,shape="West Virginia"),color="black",size=1.9)
g+scheme_basic


d %>% left_join(GDP_data %>% select(-state) %>% mutate(state=state_name)) %>% 
    mutate(pop_decile= Population*0.1,
           GDP=GDP_PC_2011_USD*Population,
           gdp_pc_decile = (GDP*pred_shares)/pop_decile) %>% 
    group_by(sce,year) %>% 
    arrange(gdp_pc_decile) %>% 
    mutate(nat_gdp= sum(GDP*pred_shares),
           nat_pop=sum(pop_decile),
           share_of_population= cumsum(pop_decile)/nat_pop,
           share_of_gdp = cumsum(GDP*pred_shares)/nat_gdp) %>% 
    ungroup() %>% 
    filter(sce=="SSP2") %>% 
    mutate(sce= "GDP model")->lorenz_curve_data_gdp




g <- ggplot(data=lorenz_curve_data %>% bind_rows(lorenz_curve_data_gdp) %>% filter(year %in% c(2030,2050,2100)), aes(x=share_of_population, y=share_of_gdp,color=sce))+
    geom_line(size=1.3)+
    scale_color_npg()+
    facet_wrap(~year)+
    geom_abline(slope=1,color="grey",linetype="dashed",size=1.3)+
    xlab("Share of Population")+
    ylab("Share of GDP")+
    geom_point(data=lorenz_curve_data %>% bind_rows(lorenz_curve_data_gdp) %>%  filter(year %in% c(2030,2050,2100),
                                                 state %in% c("New York"),
                                                 category=="d5"),aes(x=share_of_population, y=share_of_gdp,shape="New York"),color="black",size=1.9)+
    geom_point(data=lorenz_curve_data  %>% bind_rows(lorenz_curve_data_gdp) %>%  filter(year %in% c(2030,2050,2100),
                                                 state %in% c("West Virginia"),
                                                 category=="d5"),aes(x=share_of_population, y=share_of_gdp,shape="West Virginia"),color="black",size=1.9)
g+scheme_basic
