.libPaths("D:/R/library")


library(tidyverse)
library(survival)
library(survminer)
library(simPH)
library(openxlsx)



setwd("N:\\Vahe\\Commuting gap")

# result<- "D:/LM Discovery/Presentations/"
 filename <- '//NSDATA1/asheoutputdata/Commuting time/Data/ashe2002_2018_reg_cleaned.Rdata'
#filename <- 'D:\\LM Discovery\\ashe2002_2018_reg_cleaned.Rdata'

#filename <- 'N:\\Vahe\\Commuting gapashe2002_2018_reg_cleaned.Rdata'

load(filename)


df <- df %>% mutate(idbrsta=as.factor(idbrsta))

####------------------------------------------------------------####
#### I )Summary statistics of sample used for survival analysis ####
####------------------------------------------------------------####

## Number of observations 
print(paste0("Number of indivudals:", length(unique(df$piden))))
print(paste0("avg times observed:", nrow(df) /length(unique(df$piden))))


## job leaving rate by gender
df %>% 
  group_by(Gender) %>% 
  summarise(mean_sep_emp=mean(sep_emp, na.rm=TRUE),
            mean_sep_noemp=mean(sep_nonemp, na.rm=TRUE))

## SUrvival by travel time 
# create banded travel time
df$travel_time_gr <- cut(df$travel_time_imp, breaks = c(0, 30, 60, Inf))

# plot survival by travel time
fit1 <- survfit(Surv(tenure,tenure1, event == "Changed job") ~travel_time_gr , data = filter(df, tenure <= 30 * 12))
ggsurvplot(fit1, ggtheme = theme_minimal())



## Create table of descriptive stats (Table 1 in paper)
d <- select(df, female, age, tenure, urban, public, idbrnemp, occ1, left_emp, travel_time_imp,  rhe, rpay , thrs, alday )

# function to convert a factor variable into a set of dummies, named after the labels of the factor.
to_dummies <- function(df, x){
  d =  as.data.frame( model.matrix( as.formula(paste0("~ 0 + ", x)), data=df))
  names(d) <- gsub(x, '', names(d))
  df <- cbind(df, d)
  df
  
} 
d <- to_dummies(d, "occ1")

names(d) <- gsub("_", ".", names(d))

# for all
desc_all <- d %>% 
  na.omit() %>% 
  select(-occ1) %>% 
  summarise_all(list(~mean(.), ~sd(.))) %>% 
  gather(stat, val, factor_key = TRUE) %>%
  separate(stat, into = c("var", "stat"), sep = "_") 

# For men
desc_male <- d %>% 
  na.omit() %>% 
  select(-occ1) %>% 
  filter(female == 0) %>% 
  summarise_all(list(~mean(.), ~sd(.))) %>% 
  gather(stat, val, factor_key = TRUE) %>%
  separate(stat, into = c("var", "stat"), sep = "_") 

# for women
desc_female <- d %>% 
    na.omit() %>% 
    select(-occ1) %>% 
  filter(female == 1) %>%  
    summarise_all(list(~mean(.), ~sd(.))) %>% 
    gather(stat, val, factor_key = TRUE) %>%
    separate(stat, into = c("var", "stat"), sep = "_") 

# combine   
desc <- data.frame(var = desc_all$var,
                   mean.all = filter(desc_all, stat=="mean")$val,
                   sd.all = filter(desc_all, stat=="sd")$val,
                   n.all=nrow(na.omit(d)),
                   mean.male = filter(desc_male, stat=="mean")$val,
                   sd.male = filter(desc_male, stat=="sd")$val,
                   n.male=nrow(na.omit(filter(d, female==0))),
                   mean.female = filter(desc_female, stat=="mean")$val,
                   sd.female = filter(desc_female, stat=="sd")$val,
                   n.female=nrow(na.omit(filter(d, female==1))))

write.csv(desc, file = "Results/desc.csv")

## check job separation by gender by year
left_emp_by_year <- df %>% 
  group_by(year, Gender) %>% 
  summarise(mean_left_emp = mean(left_emp, na.rm=TRUE),
            mean_sep_emp = mean(sep_emp, na.rm=TRUE),
            mean_sep_nonemp = mean(sep_nonemp, na.rm=TRUE))


#### II ) Modelling - Survival analysis ####

# Keep only relevant variables
df <- df %>% 
  mutate(log_t=log(tenure1)) %>% 
         select(tenure,tenure1, event, Gender ,age, age2,travel_time_imp,lnhpay,log_t,year, urban, thrs,thrs2  ,  occ2,
             ttwaXGender, piden, ttwa, rhe, number_employees_q, alday, alday2, public ) %>% 
  na.omit()


## Main specification ##
# variables to keep for table/ to look at
xvars <- c("travel_time_imp", "GenderFemale:travel_time_imp", "lnhpay", "GenderFemale:lnhpay", "thrs", "thrs2",
           "GenderFemale:thrs", "GenderFemale:thrs2")


# left job
system.time(m1_lj <- coxph(Surv(tenure,tenure1, event != "Censored") ~
                             Gender * (age+ age2+travel_time_imp+lnhpay+
                                         + year+ urban + thrs+ thrs2+ occ2+number_employees_q+alday+alday2+public)+
                             strata(ttwaXGender)+
                             cluster(piden), data = df))

broom::tidy(m1_lj) %>%
  filter(term %in% xvars)

## test PH assumption

ph.test <- cox.zph(m1_lj)
labels=as.numeric(unlist(attr(ph.test$y, "dimnames")[1]))
ph_test_df = data.frame(x= ph.test$x, travel_time= ph.test$y[,4], 
                        travel_timeXfemale= ph.test$y[,55],
                        lnhpay = ph.test$y[,5],
                        lnhpayXfemale = ph.test$y[,56],
                        x_label= labels )

attr(ph.test$y, "dimnames")[1]

# pvalue of the test
p = ph.test$table[5,3]
ph_test_lnhpay<-ggplot(data = filter(ph_test_df, x_label <=400), aes(x=x_label, y=lnhpay)) + 
  geom_smooth(colour="#0075a3", fill="#0075a3")+
  geom_hline(yintercept = coef(m1_lj)[["lnhpay"]] )+
  labs(title= "B-Log of hourly pay", subtitle = paste0("p-value = ", formatC(p, format = "e", digits = 2)) ,
       x="Tenure in months", y="Beta(t)") +
  theme_bw()+theme(text= element_text(size=9))
# ph_test_lnhpay

# pvalue of the test
p = ph.test$table[4,3]
ph_test_ct <- ggplot(data = filter(ph_test_df, x_label <=400), aes(x=x_label, y=travel_time)) + 
  geom_smooth(colour="#0075a3", fill="#0075a3")+
  geom_hline(yintercept = coef(m1_lj)[["travel_time_imp"]] )+
  labs(title= "A-Commuting time",subtitle = paste0("p-value = ", formatC(p, format = "e", digits = 2)) ,
       x="Tenure in months", y="Beta(t)") +
  theme_bw()+theme(text= element_text(size=9))
# ph_test_ct 
p = ph.test$table[55,3]
ph_test_ctXf <- ggplot(data = filter(ph_test_df, x_label <=400), aes(x=x_label, y=travel_timeXfemale)) + 
  geom_smooth(colour="#0075a3", fill="#0075a3")+
  geom_hline(yintercept = coef(m1_lj)[["GenderFemale:travel_time_imp"]] )+
  labs(title= "C-Commuting time X Female",subtitle = paste0("p-value = ", formatC(p, format = "e", digits = 2)) ,
       x="Tenure in months", y="Beta(t)") +
  theme_bw()+theme(text= element_text(size=9))
# ph_test_ctXf
p = ph.test$table[56,3]
ph_test_lnhpayXf <-ggplot(data = filter(ph_test_df, x_label <=400), aes(x=x_label, y=lnhpayXfemale)) + 
  geom_smooth(colour="#0075a3", fill="#0075a3")+
  geom_hline(yintercept = coef(m1_lj)[["GenderFemale:lnhpay"]] )+
  labs(title= "D-Log of hourly pay X Female", subtitle = paste0("p-value = ", formatC(p, format = "e", digits = 2)) ,
       x="Tenure in months", y="Beta(t)") +
  theme_bw()+theme(text= element_text(size=9))
# ph_test_lnhpayXf

ggpubr::ggarrange(ph_test_ct ,
                  ph_test_lnhpay,
                  ph_test_ctXf ,
                  ph_test_lnhpayXf,
                  ncol=2, nrow=2) +
  ggsave("Results/ph_test.png",  height = 4, width = 4/3*4, dpi=400)


# m <-  mgcv::gam(formula = travel_time ~ s(x, bs = "cs"), data =ph_test_df)
# pred <- predict(m, se=T)
# data <- data.frame(b=)
# 
# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "Commuting time")
# writeData(wb, sheet = 1, x = ph_test_ct$data , startCol = 2, startRow = 2)
# 
# addWorksheet(wb, sheetName = "Hourly pay")
# writeData(wb, sheet = 2, x = ph_test_lnhpay$data , startCol = 2, startRow = 2)
# 
# addWorksheet(wb, sheetName = "Commuting time X Female")
# writeData(wb, sheet = 3, x = ph_test_ctXf$data , startCol = 2, startRow = 2)
# 
# addWorksheet(wb, sheetName = "Hourly pay X female")
# writeData(wb, sheet = 4, x = ph_test_lnhpayXf$data , startCol = 2, startRow = 2)
# 
# 
# saveWorkbook(wb, file="Results/data_figure8.xlsx", overwrite=TRUE)

## add interaction with time - this is fine since we use a person - year dataset
df <- df %>% 
  mutate(travel_time_impXlog_t=travel_time_imp * log_t,
         lnhpayXlog_t = lnhpay * log_t,
         travel_time_impXt=travel_time_imp * tenure1,
         lnhpayXt = lnhpay * tenure1)
system.time(m1_lj_t <- coxph(Surv(tenure,tenure1, event != "Censored") ~
                             Gender * (age+ age2+travel_time_imp+lnhpay+
                                         + year+ urban + thrs+ thrs2+ occ2+number_employees_q+alday+alday2+public +
                                         travel_time_impXlog_t+lnhpayXlog_t)+
                             strata(ttwaXGender)+
                             cluster(piden), data = df))
broom::tidy(m1_lj_t) %>%
  filter(term %in% xvars)


exp( (coef(m1_lj_t)[["GenderFemale:travel_time_imp"]] + coef(m1_lj_t)[["GenderFemale:travel_time_impXlog_t"]]*log(72) )*72)

m1_lj_t

# changed job 
system.time(m1_cj <- coxph(Surv(tenure,tenure1, event == "Changed job") ~
                             Gender * (age+ age2+travel_time_imp+lnhpay+
                                         + year+ urban + thrs+ thrs2+ occ2+number_employees_q+alday+alday2+public)+
                             strata(ttwaXGender)+
                             cluster(piden), data = df))
broom::tidy(m1_lj_t) %>%
  filter(term %in% xvars)



#' Function to produce results by gender with commuting time and pay as splines
#' The function it estimates the cox model and then does some post-estimation calculation (To create Figure 6 and 7)
#' Fitted models can be supplied to the function - in this case, it only performs post-estimations
#' The function performs the estimation and post-estimation for men and women separately
#' @param formula Formula for the Cox PH model 
#' @param df Dataframe used to estime the model
#' @param models Optional argument; the user car supply already estimated Cox PH models to the function (to save time)
#' @param ref_ct Reference commuting time used for the Hazard ratio (HR)
#' @return A list containing:
#' 1- Model for men 
#' 2- model for women 
#' 3- datasets of HR for different commuting times 
#' 3- datasets of HR for different commuting times 
#' 4-datasets of HR for different commuting times
m_spline_gender <- function(formula, df, models=NULL, ref_ct = 1){
  ## men
  # if models is null, then estimate the cox ph model
  if (is.null(models)){
    cat("Estimating model for men \n")
    m_men <- coxph( as.formula( formula) , data = filter(df, Gender=="Male"))
    print(m_men)
    cat("Estimating model for women \n")
    m_women <- coxph(as.formula( formula), data = filter(df, Gender=="Female"))
    print(m_women)
  # Otherwise, use the models supplied as an argument
  }else{
    print("models")
    m_men <- models$m_men
    m_women <-models$m_women
  }

  cat("Predictions for men \n")
  # This is inspired by https://cran.r-project.org/web/packages/survival/vignettes/splines.pdf
  # get prediction (with standard errors)
  pred_men <- termplot(m_men, term=3:4, se=TRUE, plot=F)
  # data frame for commuting time
  pred_men_ct <- pred_men$travel_time_imp # this will be a data frame
  # data frame for hourly pay
  pred_men_rhe <- pred_men$rhe
  # choose reference point
  center_ct <- with(pred_men_ct, y[x==ref_ct])
  # find value closest to minimum wage
  pred_men_rhe$diff=abs(pred_men_rhe$x - 8.21)
  closes_min = pred_men_rhe$x[pred_men_rhe$diff==min(pred_men_rhe$diff)]
  
  center_rhe <- with(pred_men_rhe, y[x==closes_min])
  
  ## compute Relative Hazards with CIs
  ytemp_ct <- exp( (pred_men_ct$y + outer(pred_men_ct$se, c(0, -1.96, 1.96), '*')) - center_ct)
  ytemp_rhe <- exp( (pred_men_rhe$y + outer(pred_men_rhe$se, c(0, -1.96, 1.96), '*')) -center_rhe)
  
  
  res_ct_df_men = data.frame(ct_time= pred_men_ct$x, RH = ytemp_ct[,1] , rh_low=ytemp_ct[,2],
                             rh_up=ytemp_ct[,3], Gender= "Male")
  
  res_rhe_df_men = data.frame(rhe= pred_men_rhe$x,  RH = ytemp_rhe[,1] , rh_low=ytemp_rhe[,2],
                              rh_up=ytemp_rhe[,3], Gender= "Male")
  
  # Same as above, but for women
  cat("Predictions for women \n")

  
  #get prediction
  pred_women <- termplot(m_women, term=3:4, se=TRUE, plot=F)
  
  pred_women_ct <- pred_women$travel_time_imp # this will be a data frame
  pred_women_rhe <- pred_women$rhe
  # choose referebnce point
  center_ct <- with(pred_women_ct, y[x==ref_ct])
  pred_women_rhe$diff=abs(pred_women_rhe$x - 8.21)
  closes_min = pred_women_rhe$x[pred_women_rhe$diff==min(pred_women_rhe$diff)]
  center_rhe <- with(pred_women_rhe, y[x==closes_min])
  ## compute RH with CIs
  ytemp_ct <- exp( (pred_women_ct$y + outer(pred_women_ct$se, c(0, -1.96, 1.96), '*')) - center_ct)
  ytemp_rhe <- exp( (pred_women_rhe$y + outer(pred_women_rhe$se, c(0, -1.96, 1.96), '*')) -center_rhe)
  
  
  res_ct_df_women = data.frame(ct_time= pred_women_ct$x, RH = ytemp_ct[,1] , rh_low=ytemp_ct[,2],
                               rh_up=ytemp_ct[,3], Gender= "Female")
  
  res_rhe_df_women = data.frame(rhe= pred_women_rhe$x,  RH = ytemp_rhe[,1] , rh_low=ytemp_rhe[,2],
                                rh_up=ytemp_rhe[,3], Gender= "Female")
  
  
  res_ct_df <- rbind(res_ct_df_women, res_ct_df_men)
  res_rhe_df <- rbind(res_rhe_df_women, res_rhe_df_men)
  
  # output: 1) model for men 2) model for women 3) Hazard ratios for commuting time 4) Hazard ratios for pay
  res <- list(m_men, m_women, res_ct_df,res_rhe_df  )
  names(res) <- c("m_men", "m_women", "df_ct", "df_rhe")
  return(res)
}

## Run models
formula0 = paste0("Surv(tenure,tenure1, event != 'Censored') ~ age+ age2+pspline(travel_time_imp,df=4)+pspline(rhe, df= 4)",
                "+ year+ urban + thrs+thrs2+alday+alday2+number_employees_q + occ2+public+strata(ttwa)+cluster(piden)")

# Main results
system.time(res_lj <- m_spline_gender(formula0, df))

# Results by age (Less than 30, 30 or above)
system.time(res_lj_less30 <- m_spline_gender(formula0, filter(df, age <30)))

system.time(res_lj_30p <- m_spline_gender(formula0, filter(df, age >=30)))

# same specification, but higher degrees of freedom for the the penalised spline
formula2=paste0("Surv(tenure,tenure1, event != 'Censored') ~ age+ age2+pspline(travel_time_imp,df=3)+pspline(rhe, df= 3)",
                "+ year+ urban + thrs+thrs2+alday+alday2+number_employees_q + occ2+public+strata(ttwa)+cluster(piden)")

system.time(res_lj_df8 <- m_spline_gender(formula2, df))

formula1=paste0("Surv(tenure,tenure1, event == 'Changed job') ~ age+ age2+pspline(travel_time_imp,df=4)+pspline(rhe, df= 4)",
                "+ year+ urban + thrs+thrs2+alday+alday2+number_employees_q + occ2+public+strata(ttwa)+cluster(piden)")


system.time(res_cj <- m_spline_gender(formula1, df ))


## produce graphs
# function to draw the graph
gen_rh_graph <- function(df){
  ct_graph <- ggplot(data = filter(df[["df_ct"]], ct_time <=120), aes(x=ct_time, y=RH, colour=Gender, fill=Gender))+geom_line()+
    geom_ribbon(aes(ymin=rh_low, ymax = rh_up), alpha=0.2, colour = NA)+
    geom_vline(xintercept = 10)+
    scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
    scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
    labs(x = "Commuting time (in mins)", y = "Risk of leaving current job compared to \n a 10-minute commute", title="A - Commuting time")+
    theme_bw()+theme(legend.position = "bottom", text= element_text(size=10))
  
  hpay_graph <- ggplot(data = filter(df[["df_rhe"]], rhe >= 8 & rhe <=50), aes(x=rhe, y=RH, colour=Gender, fill=Gender))+geom_line()+
    geom_ribbon(aes(ymin=rh_low, ymax = rh_up), alpha=0.2, colour = NA)+
    scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
    scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
    labs(x = "Hourly pay (£)", y = "Risk of leaving current job compared to\n the minimum wage", title="B - Hourly pay")+
    theme_bw()+theme(legend.position = "bottom", text= element_text(size=10))
  
return(list(ct_graph, hpay_graph))
}

graph_all <- gen_rh_graph(res_lj)
ggpubr::ggarrange(graph_all[[1]],
                  graph_all[[2]],
          ncol=2, common.legend = TRUE, legend = "bottom") +
  ggsave("Results/rh_all_spline2.png",  height = 4, width = 4/3*4, dpi=400)

graph_all_df8 <- gen_rh_graph(res_lj)
ggpubr::ggarrange(graph_all_df8[[1]],
                  graph_all_df8[[2]],
                  ncol=2, common.legend = TRUE, legend = "bottom") +
  ggsave("Results/rh_all_spline_8.png",  height = 4, width = 4/3*4, dpi=400)

## run models by age groups
graph_less30 <- gen_rh_graph(res_lj_less30)
graph_30p <- gen_rh_graph(res_lj_30p)

graph_less30 <- ggplot(data = filter(res_lj_less30[["df_ct"]], ct_time <=120), aes(x=ct_time, y=RH, colour=Gender, fill=Gender))+geom_line()+
  geom_ribbon(aes(ymin=rh_low, ymax = rh_up), alpha=0.2, colour = NA)+
  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  labs(x = "Commuting time (in mins)", y = "Relative Hazard", title="A - Less than 30")+
  theme_bw()+theme(legend.position = "bottom", text= element_text(size=10))

graph_30p  <- ggplot(data = filter(res_lj_30p[["df_rhe"]], rhe >= 8 & rhe <=50), aes(x=rhe, y=RH, colour=Gender, fill=Gender))+geom_line()+
  geom_ribbon(aes(ymin=rh_low, ymax = rh_up), alpha=0.2, colour = NA)+
  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  labs(x = "Hourly pay (£)", y = "Relative Hazard", title="B - 30 or over")+
  theme_bw()+theme(legend.position = "bottom", text= element_text(size=10))


ggpubr::ggarrange(graph_less30,
                  graph_30p,
                  ncol=2, common.legend = TRUE, legend = "bottom") +
  ggsave("Results/rh_sline_byage.png",  height = 4, width = 4/3*4, dpi=400)


save(res_lj, res_cj, res_lj_df3, file="Results/res_splines.Rdata")  


## Alternative presentation of results
load("Results/res_splines.Rdata")

# get hazard ratio with 10 minutes as reference category
res_10min <- m_spline_gender(formula0, df, models=res_lj, ref_ct = 10)

## full graphs for figure 7 (in appendix)
graph_all <- gen_rh_graph(res_10min)
ggpubr::ggarrange(graph_all[[1]],
                  graph_all[[2]],
                  ncol=2, common.legend = TRUE, legend = "bottom") +
  ggsave("Results/rh_all_spline2.png",  height = 4, width = 4/3*4, dpi=400)
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Commuting time")
writeData(wb, sheet = 1, x = res_10min$df_ct, startCol = 2, startRow = 2)

addWorksheet(wb, sheetName = "Hourly pay")
writeData(wb, sheet = 2, x = filter(res_10min$df_rhe, rhe >= 8 & rhe <= 50), startCol = 2, startRow = 2)

saveWorkbook(wb, file="Results/data_figure7.xlsx", overwrite=TRUE)
# code to keep only one in 10 obs
#res <- res_10min$df_rhe[as.numeric(rownames(res_10min$df_rhe)) %% 10 == 0,]

## graph for summary article
# function to get theobs with the closest value to a reference 
get_closest <- function(d, val){
  d$diff=abs(d[,1] - val)
  closest_min = d[,1][d$diff==min(d$diff)]
  return(closest_min)
}
get_closest(res_10min$df_ct, 15)

# commuting time
data_chart_ct = filter(res_10min$df_ct, ct_time %in% c(20 ,30, 40, 60) ) %>% 
  mutate(ct_time=as.factor(ct_time))

g_ct <- ggplot(data = data_chart_ct, aes(x=ct_time, y=RH - 1, colour=Gender, fill=Gender))+
  geom_col(position="dodge", alpha=0.4) + 
  geom_errorbar(aes(ymin=rh_low - 1, ymax=rh_up-1), position="dodge")+
  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  labs(y="Risk of leaving current job \n  compared to a 10-minute commute ", x="Commuting time", title= "A- Commuting time")+
  theme_classic()+theme(legend.position = "bottom", text= element_text(size=10))+
  scale_y_continuous(labels = function(y) y + 1)+
  coord_flip()+ggsave("Results/res_ct_simple.png",  height = 4, width = 4/3*4, dpi=400)


# pay 
# find closest value to 110%, 120% etc of minimum wage
minw <- 8.21
vals <- c(minw * 1.1, minw * 1.2, minw * 1.5, minw * 2 )
rhe_label =c("110%", "120%", "150%", "200%")

h_rates_men  <- sapply(vals, function(y) get_closest(filter(res_10min$df_rhe, Gender == "Female"), y))
h_rates_women  <- sapply(vals, function(y) get_closest(filter(res_10min$df_rhe, Gender == "Male"), y))

data_chart_rhe = filter(res_10min$df_rhe, rhe %in% c(h_rates_women, h_rates_men) ) %>% 
  mutate(rhe_exact_value= rhe, rhe=as.factor(round(rhe, 0)), rhe_label=c(rhe_label, rhe_label))


g_pay <- ggplot(data = data_chart_rhe, aes(x=rhe_label, y=RH - 1, colour=Gender, fill=Gender))+
  geom_col(position="dodge", alpha=0.4) + 
  geom_errorbar(aes(ymin=rh_low - 1, ymax=rh_up-1), position="dodge")+
  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  #labs(y="Relative rate compared to the minimum wage (£8.21)", x="Hourly pay (£)")+
  labs(y="Risk of leaving current job  \n compared to the minimum wage ", x="Hourly pay (% minimum wage)", title="B- Hourly pay")+
  theme_classic()+theme(legend.position = "bottom", text= element_text(size=10))+
  scale_y_continuous(labels = function(y) y + 1)+
  coord_flip()+ggsave("Results/res_rhe_simple.png",  height = 4, width = 4/3*4, dpi=400)


## Combine the two graphs for the paper (Figure 6) 
ggpubr::ggarrange(g_ct,
                  g_pay,
                  ncol=2, common.legend = TRUE, legend = "bottom") +
  ggsave("Results/rh_all_spline_simp.png",  height = 4, width = 4/3*4, dpi=400)


## Save results in Excel
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Commuting time")
writeData(wb, sheet = 1, x = data_chart_ct, startCol = 2, startRow = 2)

addWorksheet(wb, sheetName = "Hourly pay")
writeData(wb, sheet = 2, x = data_chart_rhe, startCol = 2, startRow = 2)

saveWorkbook(wb, file="Results/data_graphs_summary_article.xlsx", overwrite=TRUE)



#### compensating pay for increase in commuting time ####

# function to get the obs with the closest value to a reference 
get_pay_RH <- function(d, val, sex){
  d <- filter(d, Gender == sex)
  d$diff=abs(d[["RH"]] - val)
  closest_min = d[["rhe"]][d$diff==min(d$diff)]
  return(closest_min)
}
get_pay_RH <- function(d, val, sex, approx=0){
  d <- filter(d, Gender == sex)
  d$diff=abs(d[["RH"]] - val)
  # because the relationship between pay and probability to leave is non-monotonic, the closest
  # value in terms of HR may correspond to a very large pay, whilst the second-closest value may corespond to a much
  # lower one - so we allow some flexibility in terms of the minimum difference
  closest_min = min(d[["rhe"]][d$diff>= - (min(d$diff) + approx) & d$diff <= min(d$diff) + approx])

  return(closest_min)
}

# get opposite Risk ratio (If the RR of CT is 1.05, then we want 0.95 )
get_comp_pay <- function(res, sex, ct){
  x = 1 + 1 - filter(res_10min$df_ct, ct_time == ct & Gender == sex)$RH 
  comp_pay = get_pay_RH(res_10min$df_rhe, x, sex=sex, approx = 0.000001)
  return(comp_pay)
  
}

comp_pay_f_pc <- get_comp_pay(res= res_10min, "Female", ct=20)/minw
comp_pay_m_pc <- get_comp_pay(res=res_10min, "Male", ct=20)/minw

comp_pay_f_pc <- map_dfr(11:50, function(x) {
  val = get_comp_pay(sex="Female", ct=x)/minw
  #print(val)
  res = data.frame(ct=x, comp_pay = val, Gender="Female")
  
})

comp_pay_m_pc <- map_dfr(11:50, function(x) {
  val = get_comp_pay(sex="Male", ct=x)/minw
  #print(val)
  res = data.frame(ct=x, comp_pay = val, Gender="Male")
  
})

comp_pay_pc <- rbind(comp_pay_m_pc, comp_pay_f_pc)

ggplot(data = comp_pay_pc, aes(x=ct, y=comp_pay, colour=Gender))+
  geom_line()+
  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  labs(y="Increase in pay to compensate \n a longer commute ", x="Commuting time", title= "A- Commuting time")+
  theme_classic()+theme(legend.position = "bottom", text= element_text(size=10))+
  ggsave("Results/comp_pay_mw.png",  height = 4, width = 4/3*4, dpi=400)


res <- m_spline_gender(formula0, df, models=res_lj, ref_ct = ct)

d <- filter(res$df_rhe, Gender == sex)
d$diff=abs(d[["RH"]] - val)
closest_min = d[["rhe"]][d$diff==min(d$diff)]

## testing PH assumption
# for men
ph.test <- cox.zph(res_lj[["m_men"]])

# get the values of time variable (tenure in our case)
labels=as.numeric(unlist(attr(ph.test$y, "dimnames")[1]))
# create dataset containing the results from the test 
ph_test_df = data.frame(x= ph.test$x, travel_time= ph.test$y[,4], lnhpay = ph.test$y[,5], x_label= labels )

attr(ph.test$y, "dimnames")[1]

ph_test_lnhpay<-ggplot(data = filter(ph_test_df, x_label <=400), aes(x=x_label, y=lnhpay)) + 
  geom_hline(yintercept = )
  geom_smooth()
ph_test_lnhpay

ph_test_ct <- ggplot(data = filter(ph_test_df, x_label <=400), aes(x=x_label, y=travel_time)) + 
  geom_smooth() + theme_bw()

ph_test_ct

#### Robustness checks ####

# function to run several cox PH model with different specifications
# The only argument is the name of the dependent variable
run_rc <- function(var){
  df$y <- df[[var]]
  rc <- list()
  c = 1
  # no control (just age and year)
  rc[[c]] <- coxph(Surv(tenure,tenure1, y) ~
                     Gender * (age+ age2+travel_time_imp+lnhpay+
                                 + year)+
                     cluster(piden), data = df)
  broom::tidy(rc[[c]]) %>%
    filter(term %in% xvars)
  c = c + 1
  # + hours
  rc[[c]] <- coxph(Surv(tenure,tenure1, y) ~
                     Gender * (age+ age2+travel_time_imp+lnhpay+
                                 + year + thrs+thrs2+alday+alday2)+
                     cluster(piden), data = df)
  broom::tidy(rc[[c]]) %>%
    filter(term %in% xvars)
  c = c + 1
  
  
  # + ttwa & urban
  rc[[c]] <- coxph(Surv(tenure,tenure1,  y) ~
                     Gender * (age+ age2+travel_time_imp+lnhpay+
                                 + year+ urban + thrs+thrs2+alday+alday2)+
                     strata(ttwaXGender)+
                     cluster(piden), data = df)
  broom::tidy(rc[[c]]) %>%
    filter(term %in% xvars)
  c = c + 1
  
  
  
  # main specification 
  rc[[c]] <- coxph(Surv(tenure,tenure1,  y) ~
                     Gender * (age+ age2+travel_time_imp+lnhpay+
                                 + year+ urban + thrs+ thrs2+ occ2+number_employees_q+alday+alday2+public)+
                     strata(ttwaXGender)+
                     cluster(piden), data = df)
  c = c + 1
  # exclude if ct imputed
  rc[[5]] <- coxph(Surv(tenure,tenure1,  y) ~
                     Gender * (age+ age2+travel_time_imp+lnhpay+
                                 + year+ urban + thrs+thrs2 +  occ2+alday+alday2 +public+number_employees_q)+
                     strata(ttwaXGender)+
                     cluster(piden), data = filter(df, travel_time_imp_flag ==0))
  broom::tidy(rc[[c]]) %>%
    filter(term %in% xvars)
  c = c + 1
  
  # exclude if commuting time >= 2 hours
  rc[[c]] <- coxph(Surv(tenure,tenure1,y) ~
                     Gender * (age+ age2+travel_time_imp+lnhpay+
                                 year+ urban + thrs+thrs2 +  occ2+alday+alday2 +public+number_employees_q)+
                     strata(ttwaXGender)+
                     cluster(piden), data = filter(df, travel_time_imp <120))
  c = c + 1
  # full-time only
  rc[[c]] <- coxph(Surv(tenure,tenure1,  y) ~
                     Gender * (age+ age2+travel_time_imp+lnhpay+
                                 + year+ urban + thrs+thrs2 +   occ2+alday+alday2 +public+number_employees_q)+
                     strata(ttwaXGender)+
                     cluster(piden),  data = filter(df, part_time == 0))
  broom::tidy(rc[[c]]) %>%
    filter(term %in% xvars)
  # no control for pay
  c = c + 1
  rc[[c]] <- coxph(Surv(tenure,tenure1,y) ~
                     Gender * (age+ age2+travel_time_imp+
                                 + year+ urban)+
                     strata(ttwaXGender)+
                     cluster(piden),  data = filter(df, !is.na(lnhpay)))
  broom::tidy(rc[[c]]) %>%
    filter(term %in% xvars)
  c= c + 1
  rc[[c]] <- lfe::felm(y ~ Gender * (age+ age2+travel_time_imp+lnhpay+thrs+thrs2
                                     + year+ urban +    occ2+alday+alday2 +public+number_employees_q+log(tenure1))|ttwaXGender|0|piden, data=df)
  
  c= c + 1
  
  rc[[c]] <- lfe::felm(y ~ Gender * (age+ age2+travel_time_imp+lnhpay+thrs+thrs2
                                     + year+ urban +    occ2+alday+alday2 +public+number_employees_q+region+log(tenure1))| piden|0|piden, data=df)
  
  return(rc)  
}


rc_left_emp <-run_rc("left_emp")
rc_sep_emp <-run_rc("sep_emp2")


#save(rc, file="Results/rc.Rdata")
save(rc, file="D:\\LM Discovery/rc.rdata")

concordance <- sapply(rc_left_emp[1:8], function(x) x$concordance[["concordance"]])

stargazer::stargazer(rc_left_emp,  
                     keep=c("travel_time_imp", "GenderFemale:travel_time_imp", "lnhpay", "GenderFemale:lnhpay" ),
                     type = 'html',
                     dep.var.caption="",
                     dep.var.labels=c('Leaving job'),
                     omit.stat =c('rsq','ser'),
                     notes='All models control for age. Standard errors clustered at individual level parentheses',
                     notes.align='l', notes.append = FALSE , digits = 5,
                     out = "Results/rc.html")




#### Heterogenous effects ####

## Results by age ##
age_cut <-  c( "Less than 25", "25-34", "35-44", "45-54", "55+" )
df$age_gr <- cut(df$age, c(0, 24, 34, 44, 54,  Inf),
                 labels =age_cut )
age_cut <-  c( "Less than 30", "30-39", "40-49", "50-59", "60+" )
df$age_gr <- cut(df$age, c(0, 29, 39, 49, 59,  Inf),
                 labels =age_cut )

m_all_int_by_age <- lapply(age_cut,  function(a){
  coxph(Surv(tenure,tenure1, event != "Censored") ~
          Gender * (age+ age2+travel_time_imp+lnhpay+
                      + year+ urban + thrs+ thrs2+ occ2+number_employees_q+alday+alday2+public)+
          strata(ttwaXGender)+
          cluster(piden), data = filter(df, age_gr == a))})

res <- list(m1_cj)

res <- c(res, m_all_int_by_age )

stargazer::stargazer(m_all_int_by_age,  
                     keep=c("travel_time_imp", "GenderFemale:travel_time_imp", "lnhpay", "GenderFemale:lnhpay" ),
                     type = 'html',
                     column.labels =  age_cut,
                     dep.var.caption="",
                     dep.var.labels=c('Leaving job'),
                     omit.stat =c('rsq','ser'),
                     notes='All models control for age. Standard errors clustered at individual level parentheses',
                     notes.align='l', notes.append = FALSE , digits = 5,
                     out = "Results/m_by_age.html")




plt_by_age <- lapply(age_cut , function(a){
  ggplot(data=sim_df_by_age[[a]]$ct, aes(x = Xj, y = Median, colour = Gender, fill=Gender))+
    geom_line()+
    geom_ribbon(aes(ymin=Lower50, ymax = Upper50), alpha=0.2, colour = NA)+
    scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
    scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
    labs(x = "Commuting time (in mins)", y = "Relative Hazard", title=a)+
    theme_bw()+theme(legend.position = "bottom", text= element_text(size=10))+
    ggsave(paste0("Results/rh_lin_",a, ".png"),  height = 4, width = 4/3*4, dpi=400)
})

## results by occupation
m_by_occ <- lapply(levels(df$occ1),  function(a){
  coxph(Surv(tenure,tenure1, event != "Censored") ~
          Gender * (age+ age2+travel_time_imp+lnhpay+
                      + year+ urban + thrs+ thrs2+ occ2+number_employees_q+alday+alday2+public)+
          strata(ttwaXGender)+
          cluster(piden), data = filter(df, occ1 == a))})

# get p-values
p2 <- lapply(m_by_occ, function(x) summary(x)$coefficients[, 4])
# get OR
OR <- lapply(m_by_occ, function(x) exp(coef(x)))
stargazer::stargazer(m_by_occ,  
                     keep=c("travel_time_imp", "GenderFemale:travel_time_imp", "lnhpay", "GenderFemale:lnhpay" ),
                     coef = OR,
                     p = p2,
                     report=('vc*p'),
                     type = 'html',
                     column.labels =levels(df$occ1),
                     dep.var.caption="",
                     dep.var.labels=c('Leaving job'),
                     omit.stat =c('rsq','ser'),
                     notes='All models control for age. Standard errors clustered at individual level parentheses',
                     notes.align='l', notes.append = FALSE , digits = 5,
                     out = "Results/m_by_occ.html")

## by london
m_cj_london <- coxph(Surv(tenure,tenure1, event == "Changed job") ~
                 Gender * (age+ age2+travel_time_imp+lnhpay+
                             + year+ urban + lnhours +  occ2)+
                 strata(ttwa)+
                 cluster(piden), data = filter(df, region == "London"))
broom::tidy(m_cj_london ) %>%
  filter(term %in% xvars)

m_cj_other <- coxph(Surv(tenure,tenure1, event == "Changed job") ~
                       Gender * (age+ age2+travel_time_imp+lnhpay+
                                   + year+ urban + lnhours  +  occ2)+
                       strata(ttwa)+
                       cluster(piden), data = filter(df, region != "London"))
broom::tidy(m_cj_other ) %>%
  filter(term %in% xvars)

m_cj_ft <- coxph(Surv(tenure,tenure1, event == "Changed job") ~
                   Gender * (age+ age2+travel_time_imp+lnhpay+
                               + year+ urban + part_time  +  occ2)+
                   strata(ttwa)+
                   cluster(piden), data = filter(df, part_time == 0))

broom::tidy(m_cj_ft) %>%
  filter(term %in% xvars)
## by region
m_region <- lapply(levels(df$region),  function(a){
  coxph(Surv(tenure,tenure1, event == "Changed job") ~
          Gender * (age + age2 + travel_time_imp + lnhpay+
                      year + urban + thrs+thrs2+ occ2)+
          strata(ttwaXGender) +
          cluster(piden), data = filter(df, region == a))})

res <- m_region 
# get p-values
p2 <- lapply(res, function(x) summary(x)$coefficients[, 4])
# get OR
OR <- lapply(res, function(x) exp(coef(x)))
stargazer::stargazer(res,  
                     keep=c("travel_time_imp", "GenderFemale:travel_time_imp", "lnhpay", "GenderFemale:lnhpay" ),
                     coef = OR,
                     p = p2,
                     report=('vc*p'),
                     type = 'html',
                     column.labels = levels(df$region),
                     dep.var.caption="",
                     dep.var.labels=c('Leaving job'),
                     omit.stat =c('rsq','ser'),
                     notes='All models control for age. Standard errors clustered at individual level parentheses',
                     notes.align='l', notes.append = FALSE , digits = 5,
                     out = "Results/m_by_regin.html")

rc <- list()
c = 1
# no control (just age and year)
rc[[c]] <- coxph(Surv(tenure,tenure1, event != "Censored") ~
                   Gender * (age+ age2+travel_time_imp+lnhpay+
                               + year)+
                   cluster(piden), data = df)
broom::tidy(rc[[c]]) %>%
  filter(term %in% xvars)
c = c + 1
# + hours
rc[[c]] <- coxph(Surv(tenure,tenure1, event != "Censored") ~
                   Gender * (age+ age2+travel_time_imp+lnhpay+
                               + year + thrs+thrs2+alday+alday2)+
                   cluster(piden), data = df)
broom::tidy(rc[[c]]) %>%
  filter(term %in% xvars)
c = c + 1


# + ttwa & urban
rc[[c]] <- coxph(Surv(tenure,tenure1, event != "Censored") ~
                   Gender * (age+ age2+travel_time_imp+lnhpay+
                               + year+ urban + thrs+thrs2+alday+alday2)+
                   strata(ttwaXGender)+
                   cluster(piden), data = df)
broom::tidy(rc[[c]]) %>%
  filter(term %in% xvars)
c = c + 1



# main specification 
rc[[c]] <- coxph(Surv(tenure,tenure1, event != "Censored") ~
                   Gender * (age+ age2+travel_time_imp+lnhpay+
                               + year+ urban + thrs+ thrs2+ occ2+number_employees_q+alday+alday2+public)+
                   strata(ttwaXGender)+
                   cluster(piden), data = df)
c = c + 1
# exclude if ct imputed
rc[[5]] <- coxph(Surv(tenure,tenure1, event != "Censored") ~
                   Gender * (age+ age2+travel_time_imp+lnhpay+
                               + year+ urban + thrs+thrs2 +  occ2+alday+alday2 +public+number_employees_q)+
                   strata(ttwaXGender)+
                   cluster(piden), data = filter(df, travel_time_imp_flag ==0))
broom::tidy(rc[[c]]) %>%
  filter(term %in% xvars)
c = c + 1

# exclude if commuting time >= 2 hours
rc[[c]] <- coxph(Surv(tenure,tenure1, event != "Censored") ~
                   Gender * (age+ age2+travel_time_imp+lnhpay+
                               year+ urban + thrs+thrs2 +  occ2+alday+alday2 +public+number_employees_q)+
                   strata(ttwaXGender)+
                   cluster(piden), data = filter(df, travel_time_imp <120))
c = c + 1
# full-time only
rc[[c]] <- coxph(Surv(tenure,tenure1, event != "Censored") ~
                   Gender * (age+ age2+travel_time_imp+lnhpay+
                               + year+ urban + thrs+thrs2 +   occ2+alday+alday2 +public+number_employees_q)+
                   strata(ttwaXGender)+
                   cluster(piden),  data = filter(df, part_time == 0))
broom::tidy(rc[[c]]) %>%
  filter(term %in% xvars)
# no control for pay
c = c + 1
rc[[c]] <- coxph(Surv(tenure,tenure1, event != "Censored") ~
                   Gender * (age+ age2+travel_time_imp+
                               + year+ urban)+
                   strata(ttwaXGender)+
                   cluster(piden),  data = filter(df, !is.na(lnhpay)))
broom::tidy(rc[[c]]) %>%
  filter(term %in% xvars)
c= c + 1
rc[[c]] <- lfe::felm(left_emp ~ Gender * (age+ age2+travel_time_imp+lnhpay+thrs+thrs2
                                          + year+ urban +    occ2+alday+alday2 +public+number_employees_q+log(tenure1))|ttwaXGender|0|piden, data=df)

c= c + 1

rc[[c]] <- lfe::felm(left_emp ~ Gender * (age+ age2+travel_time_imp+lnhpay+thrs+thrs2
                                          + year+ urban +    occ2+alday+alday2 +public+number_employees_q+region+log(tenure1))| piden|0|piden, data=df)
