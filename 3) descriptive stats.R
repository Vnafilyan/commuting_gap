
## library
.libPaths("D:/R/Library")
## Load packages
library(tidyverse)
library(spatstat) 
library(openxlsx)
setwd("N:\\Vahe\\Commuting gap")


#### define functions ###
## get a function to compute weighted mean

wmean<-function(y, w){
  w[is.na(y)]<-NA
  m<-mean(y*w, na.rm=TRUE)/mean(w, na.rm=TRUE)
  m
}


## function to compute bootstrapped confidence intervals for weighted medians
bootstrap_ci <- function(d, group_vars, iterations=500){
  ## compute weighted median
  medians <- d %>%
    group_by_(.dots=group_vars) %>%
    summarise(wmedian_travel_time=weighted.median(travel_time_imp, calwght, na.rm=TRUE),
              wmedian_gpay=weighted.median(rpay, calwght, na.rm=TRUE),
              wmedian_he=weighted.median(rhe, calwght, na.rm=TRUE))
  ## bootstrapped standard errors
  cat("Start bootstrapping")
  boot_d <- purrr::map_dfr(1:iterations, function(i){
    if (i %% 10 == 0){print(i)}
    # We have to bootstrap on individuals rather than observations
    # create list of individuals
    ind <- unique(d$piden)
    # resample individuals
    index = sample(length(ind),  length(ind), replace=TRUE )
    b_ind <- data.frame(piden=ind[index])
    # compute medians
    d2 <- right_join(d, b_ind, by="piden") %>%
      group_by_(.dots=group_vars) %>%
      summarise(iteration = i,
                wmedian_travel_time=weighted.median(travel_time_imp, calwght, na.rm=TRUE),
                wmedian_gpay=weighted.median(rpay, calwght, na.rm=TRUE),
                wmedian_he=weighted.median(rhe, calwght, na.rm=TRUE))
    return(d2)
    
  })
  # compute the CIs
  ci <- boot_d %>%
    group_by_(.dots=group_vars) %>%
    summarise(travel_time_low_ci=quantile(wmedian_travel_time,0.0275),
              travel_time_up_ci=quantile(wmedian_travel_time,0.975),
              gpay_low_ci=quantile(wmedian_gpay,0.0275),
              gpay_up_ci=quantile(wmedian_gpay,0.975),
              he_low_ci=quantile(wmedian_he,0.0275),
              he_time_up_ci=quantile(wmedian_he,0.975))
  
  res <- left_join(medians, ci, by=group_vars)
  return(res)
  
  
  
  
}

##########################################

## Load data

load(file='//NSDATA1/asheoutputdata/Commuting time/Data/ashe1997_2018_sum.Rdata')

set.seed(11235)

# cap travel time to 240 minutes
df_m <- df_m %>%
  ungroup() %>% 
  mutate(travel_time_imp=ifelse(travel_time_imp >= 240, 240, travel_time_imp),
         travel_time_gr = cut(travel_time_imp, breaks = c(-1, 15, 30, 45, 61, Inf)))

##-----------------------------------------------##
##        Compare commuting time with LFS        ##
##          (Figure 6 in Appendix)               ##
##-----------------------------------------------##
## read LFS data
lfs_ct <- read_csv("Data/LFS_ct.csv") %>%
  mutate(travel_time_gr = as.factor(travel_time_gr),
         Gender=factor(Gender,levels=0:2, labels=c("All","Male", "Female")),
         data="LFS")
  
## Compute CT in ASHE 
# By gender
ashe_ct_gender <- df_m %>% 
  filter(year == 2018) %>%
  group_by(Gender) %>% 
  mutate(N=n()) %>% 
  group_by(Gender, travel_time_gr) %>% 
  summarise(N_gr=n(), N=mean(N)) %>%
  ungroup() %>% 
  mutate(mean_ct = N_gr / N,
         travel_time_gr=factor(travel_time_gr, labels=levels(lfs_ct$travel_time_gr)),
         data="ASHE",
         Gender=as.numeric(Gender)) %>% 
  select(-N, -N_gr)

# for all
ashe_ct_all <- df_m %>% 
  filter(year == 2017) %>%
  mutate(N=n()) %>% 
  group_by(travel_time_gr) %>% 
  summarise(N_gr=n(), N=mean(N)) %>%
  mutate(mean_ct = N_gr / N,
         travel_time_gr=factor(travel_time_gr, labels=levels(lfs_ct$travel_time_gr)),
         data="ASHE", 
         Gender=0) %>% 
  select(-N, -N_gr)

# combine ASHE
ashe_ct <- rbind(ashe_ct_all, ashe_ct_gender ) %>%
  mutate(Gender=factor(Gender,levels=0:2, labels=c("All","Male", "Female")))
# Combine ASHE and LFS
ct_all <- rbind.data.frame(as.data.frame(ashe_ct), lfs_ct) 

## Graph for all (Figure 6)
ggplot(data = filter(ct_all, Gender== "All"), aes(x = travel_time_gr, y = mean_ct, fill=data) )+
  geom_col(position = "dodge")+
  labs(y="Proportion", x="Travel time (mins)")+
  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  theme_bw() +theme(legend.position = "bottom", text= element_text(size=10))+
  ggsave("Results/ct_by_survey.png", height = 4, width = 4/3*4, dpi=400)

## Save to excel
# create workbook
wb <- createWorkbook()
addWorksheet(wb, sheetName = "By Survey")
writeData(wb, sheet = 1, x = ct_all, startCol = 2, startRow = 2)

####----------------------------####
#### Median commuting time and  ####
#### Pay by year and age        ####
#### Pay by year and age        ####
####----------------------------####

# Select relevant varaibles and observations
df <- df_m %>% 
  filter(age <65) %>%
  ungroup() %>% 
  select(travel_time_imp, rhe, rpay, age, ft, year, Gender, calwght, region, piden) 

# CT and pay by year
ct_by_year <- bootstrap_ci(df,  c("Gender", "year"), iterations=500)

# CT and pay by age
ct_by_age <- bootstrap_ci(filter(df, year >= 2010),  c("Gender", "age"), iterations=500)

# CT and pay by year
ct_by_age_ft <- bootstrap_ci(filter(df, year >= 2010, ft == 1),  c("Gender", "age"), iterations=500)


#ct_by_age_region <- sum_by(df_m, c("Gender", "age", "region"))
df$Area = ifelse(df$region == "London", "London", "Rest of the country")
ct_by_age_london <- bootstrap_ci(filter(df, year >= 2010),  c("Gender", "age", "Area"), iterations=500)

save(ct_by_year, ct_by_age,ct_by_age_ft, ct_by_age_london, file = "Results/ct.RData")

load("Results/ct.RData")
## Save to excel

addWorksheet(wb, sheetName = "By year")
writeData(wb, sheet = 2, x = ct_by_year, startCol = 2, startRow = 2)

addWorksheet(wb, sheetName = "By Age")
writeData(wb, sheet = 3, x = ct_by_age, startCol = 2, startRow = 2)

addWorksheet(wb, sheetName = "By Age FT")
writeData(wb, sheet = 4, x = ct_by_age_ft, startCol = 2, startRow = 2)

addWorksheet(wb, sheetName = "By Age & London")
writeData(wb, sheet = 5, x = ct_by_age_london, startCol = 2, startRow = 2)



saveWorkbook(wb, file="Results/desciptives.xlsx", overwrite=TRUE)


load("Results/ct.RData")


## Median commuting time by year (Figure 1)
median_ct_year <-ggplot(data=ct_by_year,  aes(x=year, y=wmedian_travel_time,  group=Gender, color=Gender, fill=Gender)) + 
  geom_point(size=0.5)+geom_line()+
  geom_ribbon(aes(ymin=travel_time_low_ci,ymax= travel_time_up_ci ), alpha=0.2, colour = NA)+
  ggtitle("Median commuting time, working age adults")+
  xlab("Year") +
  ylab("Median time in mins")+  
  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  theme_bw()+  theme(legend.position = "bottom", text= element_text(size=10))+
  ggsave("Results/ct_byyear.png", height = 4, width = 4/3*4, dpi=400)

## Commuting time by age  (Figure 2)
median_ct_age <-ggplot(data=ct_by_age,  aes(x=age, y=wmedian_travel_time,  group=Gender, color=Gender, fill=Gender)) + 
  geom_point(size=0.5)+geom_line()+
  geom_ribbon(aes(ymin=travel_time_low_ci,ymax= travel_time_up_ci ), alpha=0.2, colour = NA)+
  ggtitle("Median commuting time, 2010-2018")+
  xlab("Age") +
  ylab("Median time in mins")+  
  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  theme_bw()+  theme(legend.position = "bottom", text= element_text(size=10))+
  ggsave("Results/ct_byage.png", height = 4, width = 4/3*4, dpi=400)

## Commuting time by age - full-time only
median_ct_age_ft <-ggplot(data=ct_by_age_ft,  aes(x=age, y=wmedian_travel_time,  group=Gender, color=Gender, fill=Gender)) + 
  geom_point(size=0.5)+geom_line()+
  geom_ribbon(aes(ymin=travel_time_low_ci,ymax= travel_time_up_ci ), alpha=0.2, colour = NA)+
  ggtitle("Median commuting time, 2010-2018")+
  xlab("Age") +
  ylab("Median time in mins")+  
  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  theme_bw()+  theme(legend.position = "bottom", text= element_text(size=10))+
  ggsave("Results/ct_byage_ft.png", height = 4, width = 4/3*4, dpi=400)

## Gross weekly pay by age and gender (Figure 4)
gpay_age <- ggplot(data=ct_by_age,  aes(x=age, y=wmedian_gpay,  group=Gender, color=Gender, fill=Gender)) + 
  geom_point(size=0.5)+geom_line()+
  geom_ribbon(aes(ymin=gpay_low_ci,ymax= gpay_up_ci ), alpha=0.2, colour = NA)+
  ggtitle('Median weekly gross pay, 2010-2018')+
  xlab("Age") +
  ylab("Gross weekly pay, £")+  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  theme_bw()+  theme(legend.position = "bottom", text= element_text(size=10))+
  ggsave("Results/gpay_byage.png",height = 4, width = 4/3*4, dpi=400)

gpay_age

## Gross hourly pay by age and gender 
hpay_age <- ggplot(data=ct_by_age,  aes(x=age, y=wmedian_he,  group=Gender, color=Gender, fill=Gender)) + 
  geom_point(size=0.5)+geom_line()+
  geom_ribbon(aes(ymin=he_low_ci,ymax= he_time_up_ci ), alpha=0.2, colour = NA)+
  ggtitle('Median hourly gross pay, 2010-2018')+
  xlab("Age") +
  ylab("Gross hourly pay, £")+  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  theme_bw()+  theme(legend.position = "bottom", text= element_text(size=10))+
  ggsave("Results/hpay_byage.png",height = 4, width = 4/3*4, dpi=400)

## London vs the rest of the country  (Figure 3)
# London
median_ct_age_london <- ggplot(data=filter(ct_by_age_london, Area=="London"),
                                           aes(x=age, y=wmedian_travel_time,  color=Gender, fill=Gender)) + 
  geom_line()+
  geom_point(size=0.5)+
  geom_ribbon(aes(ymin=travel_time_low_ci,ymax= travel_time_up_ci ), alpha=0.2, color=NA)+
  ggtitle("London")+
  xlab("Age") +
  ylab("Median time in mins")+  
  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  theme_bw()+ylim(10,50)
# rest of UK
median_ct_age_rest <- ggplot(data=filter(ct_by_age_london, Area!="London"),
                                 aes(x=age, y=wmedian_travel_time,  color=Gender, fill=Gender)) + 
    geom_line()+
    geom_point(size=0.5)+
    geom_ribbon(aes(ymin=travel_time_low_ci,ymax= travel_time_up_ci ), alpha=0.2, color=NA)+
  ggtitle("Other regions")+
    xlab("Age") +
    ylab("Median time in mins")+  
    scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
    scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
    theme_bw() +ylim(10,50)

ggpubr::ggarrange(median_ct_age_london, 
          median_ct_age_rest,
          ncol=2,
          common.legend = TRUE, legend = "bottom") +
  ggsave("Results/ct_byage_london.png",  height = 4, width = 4/3*4, dpi=400)



# for academic paper

median_ct_age <-ggplot(data=ct_by_age,  aes(x=age, y=wmedian_travel_time,  group=Gender, color=Gender, fill=Gender)) + 
  geom_point(size=0.5)+geom_line()+
  geom_ribbon(aes(ymin=travel_time_low_ci,ymax= travel_time_up_ci ), alpha=0.2, colour = NA)+
  ggtitle("A - Median commuting time, 2010-2018")+
  xlab("Age") +
  ylab("Median time in mins")+  
  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  theme_bw()+  theme(legend.position = "bottom", text= element_text(size=10))

hpay_age <- ggplot(data=ct_by_age,  aes(x=age, y=wmedian_he,  group=Gender, color=Gender, fill=Gender)) + 
  geom_point(size=0.5)+geom_line()+
  geom_ribbon(aes(ymin=he_low_ci,ymax= he_time_up_ci ), alpha=0.2, colour = NA)+
  ggtitle('B - Median hourly gross pay, 2010-2018')+
  xlab("Age") +
  ylab("Gross weekly pay, £")+  scale_color_manual(values=c("#0075a3", '#e2bc22' ))+
  scale_fill_manual(values=c("#0075a3", '#e2bc22' )) +
  theme_bw()+  theme(legend.position = "bottom", text= element_text(size=10))

ggpubr::ggarrange(median_ct_age, 
                  hpay_age,
                  ncol=2,
                  common.legend = TRUE, legend = "bottom") +
  ggsave("Results/ct_pay.png",  height = 4, width = 4/3*4, dpi=400)



### Data by ttva

## Loda data
#load(file='//nsdata1/ASHEdata/ASHE and LM Discovery/Data/ashe1997_2018_sum.Rdata')
load(file='D:/LM Discovery/ashe1997_2018_sum.Rdata')


res_ttwa_by_age <-  df_m %>%
    filter(year >= 2010 & age< 65) %>%
  mutate(age30 = ifelse(age >= 30, 1,0)) %>% 
    group_by(ttwa, Gender, age30) %>%
    summarise(wmedian_travel_time=weighted.median(travel_time_imp, calwght, na.rm=TRUE), 
              wmedian_gpay=weighted.median(rpay, calwght, na.rm=TRUE),
              wmedian_he=weighted.median(rhe, calwght, na.rm=TRUE),
              N=n()) 
    
# Data for men
res_ttwa_wide_male <- res_ttwa_by_age %>% 
  filter(Gender == "Male") %>% 
  rename(wmedian_travel_time_men = wmedian_travel_time,
         wmedian_gpay_men = wmedian_gpay,
         wmedian_he_men = wmedian_he, 
         N_men=N)%>% 
  ungroup() %>% 
  select(-Gender)

# Data for women
res_ttwa_wide_female <- res_ttwa_by_age %>% 
  filter(Gender == "Female") %>% 
  rename(wmedian_travel_time_women = wmedian_travel_time,
         wmedian_gpay_women = wmedian_gpay,
         wmedian_he_women = wmedian_he,
         N_women=N) %>% 
  ungroup() %>% 
  select(-Gender)

res_ttwa_wide <- left_join(res_ttwa_wide_female, res_ttwa_wide_male, by=c("ttwa", "age30")) %>% 
  mutate(gap_travel_time = wmedian_travel_time_men - wmedian_travel_time_women, 
         gap_gpay = wmedian_gpay_men - wmedian_gpay_women,
         gap_he =  wmedian_he_men -  wmedian_he_women,
         gap_travel_time_percent = gap_travel_time/wmedian_travel_time_women * 100,
         gap_gpay_percent = gap_gpay/wmedian_gpay_women * 100,
         gap_he_percent = gap_he/wmedian_he_women * 100,
         N=N_women + N_men) 
  
rm(res_ttwa_wide_female, res_ttwa_wide_male)

write_csv(res_ttwa_wide, "Results/desc_by_ttwa.csv")
## more work on the TTWA level data
ttwa_code <- read_csv('//NSDATA1/asheoutputdata/Commuting gap/Data/Travel_to_Work_Areas_December_2011_Names_and_Codes_in_the_United_Kingdom.csv') %>% 
  select(ttwa=TTWA11CD, ttwa_name=TTWA11NM)

res_ttwa_wide <- left_join(res_ttwa_wide, ttwa_code) %>% 
  select(ttwa, ttwa_name, everything())

cornwall <- c("Bude", "Launceston", "Liskeard", "St Austell and Newquay", "Redruth and Truro",
              "Falmouth", "Penzance", "Plymouth")

cornwall  %in% unique(res_ttwa_wide$ttwa_name)

View(filter(res_ttwa_wide, ttwa_name %in% cornwall))

#### Data for tool ####

load(file='D:/LM Discovery/ashe1997_2018_sum.Rdata')

df_m <- df_m %>% 
  mutate(travel_time_imp =ifelse(travel_time_imp >240, 240, travel_time_imp )) %>% 
  filter(age <=65, year >=2010)

perc=seq(1,100)/100

## percentile of commuting time by age and gender
percentile_ct_by_age_gender <- df_m %>%
  group_by(Gender, age) %>% 
  summarise(n=n(),
            ct_percentile = list(enframe(Hmisc::wtd.quantile(travel_time_imp, probs=perc, weight=calwght)))) %>% 
  unnest %>% 
  write_csv("Results/percentile_ct_by_age_gender.csv")

## percentile of real hourly pay by age, commuting time and gender  

# 5 mins band up to an hour, 10 mintes up to two hours
ct_groups <- c(seq(0, 60, 5)  , seq(70, 120, 10), Inf  )    
# 5 year age bands
age_groups <- seq(15,70,5)

df_m<- df_m %>% 
  mutate(ct_gr=cut(travel_time_imp, ct_groups, right = FALSE),
         age_groups=cut(age, age_groups, right = FALSE))  

inc_by_ct_age <- df_m %>%
  filter(!is.na(rhe)) %>% 
  group_by(Gender, age_groups, ct_gr) %>%  
  summarise(n=n(),
            pay_decile = list(enframe(Hmisc::wtd.quantile(rhe, probs=perc, weight=calwght)))) %>% 
  unnest %>% 
  write_csv("Results/percentiles_hourly_pay_by_age_ct_gender2.csv")

res1 <- read_csv("Results/percentiles_hourly_pay_by_age_ct_gender.csv")
res2 <- read_csv("Results/percentile_ct_by_age_gender.csv")

### User requested data ###


## library
.libPaths("D:/R/Library")
## Load packages
library(tidyverse)
library(spatstat) 
library(openxlsx)
setwd("N:\\Vahe\\Commuting gap")


load(file='//NSDATA1/asheoutputdata/Commuting gap/Data/ashe1997_2018_sum.Rdata')

set.seed(11235)

# cap travel time to 240 minutes
df <- df_m %>%
  ungroup() %>% 
  mutate(travel_time_imp=ifelse(travel_time_imp >= 240, 240, travel_time_imp),
         travel_time_gr = cut(travel_time_imp, breaks = c(-1, 15, 30, 45, 61, Inf)),
         age30 = ifelse(age >= 30, 1,0)) %>% 
  filter(age <65) %>%
  ungroup() %>% 
  select(travel_time_imp, rhe, rpay, age, age30, ft, year, Gender, calwght, ttwa, piden) 


d <- dplyr::filter(df, year >= 2010)



N_ttwa <- d %>% 
  group_by(.dots=c("Gender", "ttwa")) %>% 
  summarise(N=n())


low_n_ttwa =unique(filter(N_ttwa, N < 200)$ttwa)


N_ttwa_byage <- d %>% 
  filter(!(ttwa %in% low_n_ttwa)) %>% 
  group_by(.dots=c("Gender", "age30", "ttwa")) %>% 
  summarise(N=n())

d2 <-  filter(d, !(ttwa %in% low_n_ttwa)) 
# CT and pay by age
ct_by_ttwa <- bootstrap_ci(d2,  c("Gender",  "ttwa"), iterations=500)

ct_by_ttwa_age <- bootstrap_ci(d2,  c("Gender", "age30", "ttwa"), iterations=500)


ttwa_code <- read_csv('//NSDATA1/asheoutputdata/Commuting gap/Data/Travel_to_Work_Areas_December_2011_Names_and_Codes_in_the_United_Kingdom.csv') %>% 
  select(ttwa=TTWA11CD, ttwa_name=TTWA11NM)

ct_by_ttwa <- left_join(ct_by_ttwa, ttwa_code) %>% 
  select(ttwa, ttwa_name, everything())

ct_by_ttwa_age  <- left_join(ct_by_ttwa_age, ttwa_code) %>% 
  select(ttwa, ttwa_name, everything())


save(ct_by_ttwa, ct_by_ttwa_age, file = "Results/ct_by_ttwa.rds")

## function to spread multiple variables
multi_spread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}



load("Results/ct_by_ttwa.rds")

ct_by_ttwa$age = "All"
ct_by_ttwa_age <- ct_by_ttwa_age %>% 
  mutate(age = ifelse(age30 == 1, "30 or over", "Less than 30")) %>% 
  ungroup() %>% 
  select(-age30)

ct_by_ttwa_final <- bind_rows(ct_by_ttwa, ct_by_ttwa_age) %>% 
  select(ttwa_code=ttwa, ttwa_name, Gender, age, wmedian_travel_time, travel_time_low_ci, travel_time_up_ci ) %>% 
  multi_spread(Gender,  c(wmedian_travel_time,  travel_time_low_ci, travel_time_up_ci )) %>% 
  mutate(gap = Male_wmedian_travel_time - Female_wmedian_travel_time)

names(ct_by_ttwa_final) <- gsub("_", " ", names(ct_by_ttwa_final))
write_csv(ct_by_ttwa_final, "Results/ct_by_ttwa.csv" )
