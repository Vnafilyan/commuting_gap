
##---------------------------------------------------------------##
## Analyse longitudinal dimension: moving home and changing jobs ##
##---------------------------------------------------------------##
## input: 'W:/ASHE and LM Discovery/Data/ashe1997_2016.Rdata', created using ASHE Travel to work 081217.R code
## Author: Vahé Nafilyan


#--Library path:
.libPaths("D:/R/library")

## Load packages ##
#library(ff)
#options(fftempdir = "D:/LM Discovery")
library(data.table)
library(spatstat) ## for weighted median
library(tidyverse)
library(ggplot2)
#library(lfe)
library(stringr)


##---------------------------------------------------------------##
##        impute missing spells                                   ##
##---------------------------------------------------------------##

load(file='//nsdata1/ASHEdata/ASHE and LM Discovery/Data/ashe1997_2018.Rdata')

## keep data for 2002 and later
df_m <- df_m %>% 
  filter( year >= 2002)

# keep main job only
df_m <- df_m %>%  
  mutate(mjob = ifelse(djob == 1, mjob, 1)) %>%
  filter(mjob == 1) 



## replace starting date by NA if it is 0
df_m$empsta[df_m$empst == 0] <- NA

## There should only be one obs per person - year

df_m <- df_m %>% 
  group_by(piden, year) %>%
  mutate(N = n())

# this is true for:

nrow(filter(df_m, N==1))/nrow(df_m)

## let's remove those who have more than one obs per person/employer ref/year
df_m <- filter(df_m, N ==1) 
  

## get earliest year when observed in ASHE
df_m <- df_m %>% 
  group_by(piden) %>%
  mutate(min_year = min(year, na.rm=TRUE), 
         max_year = max(year, na.rm=TRUE)) %>% 
  # create rank of obs evration within piden
  mutate(rank=rank(year))


## create a data frame containing one observation for each individual for every year

# get list of piden
pid <- as.data.frame(unique(df_m$piden))
# create n= max(year) - 2002 obs per piden
n = max(df_m$year) -min(df_m$year)+1

pid <- as.data.frame(pid[rep(seq_len(nrow(pid)), n), 1])

names(pid) <- 'piden'

# create variable year
pid <- pid %>%  
  group_by(piden) %>%
  mutate(year=2002+row_number()-1) %>%
  arrange(piden, year)

df_m <- left_join(pid, df_m, by=c('piden', 'year'))

rm(pid)

gc()


## fill missing with either lag or average of lead and lag
## depending on whether a specified condition is met


df <- df_m
for (i in 2:1){
df <- df %>% group_by(piden) %>%
    mutate(empsta = ifelse(is.na(empsta) & lag(empsta, n= 1) == lead(empsta, n=i), lag(empsta, n= 1), empsta  ),
           entref= ifelse(is.na(entref) & lag(entref, n=1) == lead(entref, n=i) & lag(empsta)==lead(empsta), lag(entref), entref),
           dist_km= as.numeric(ifelse( is.na(dist_km) & lag(dist_km, n=1) == lead(dist_km, n=i) & lag(empsta)==lead(empsta), lag(dist_km), dist_km)),
           age= as.numeric(ifelse( is.na(age) , lag(age)+ (lead(age, n=i)- lag(age))/(1+i),age)),
           idbrnemp= as.numeric(ifelse( is.na(idbrnemp) , lag(idbrnemp)+ (lead(idbrnemp, n=i)- lag(idbrnemp))/(1+i),idbrnemp)),
           gpay= as.numeric(ifelse( is.na(gpay) & lag(entref, n=1) == lead(entref, n=i)& lag(empsta)==lead(empsta), lag(gpay)+ (lead(gpay, n=i)- lag(gpay))/(1+i),gpay)),
           he= as.numeric(ifelse( is.na(he) & lag(entref, n=1) == lead(entref, n=i)& lag(empsta)==lead(empsta), lag(he)+ (lead(he, n=i)- lag(he))/(1+i), he)),
           region=factor(as.numeric(ifelse(is.na(region) & lag(region, n=1) == lead(region, n=i), 
                                           lag(region),region)), levels = 1:length(levels(region)), labels = levels(region)),
           h_la=factor(as.numeric(ifelse(is.na(h_la) & lag(h_la, n=1) == lead(h_la, n=i), 
                                         lag(h_la),h_la)), levels = 1:length(levels(h_la)), labels = levels(h_la)),
           occ1=factor(as.numeric(ifelse(is.na(occ1) & lag(occ1, n=1) == lead(occ1, n=i)& lag(empsta)==lead(empsta), 
                                         lag(occ1),occ1)), levels = 1:length(levels(occ1)), labels = levels(occ1)),
           occ = ifelse(is.na(occ)  & lag(occ, n= 1) == lead(occ, n=i), lag(occ, n= 1), occ ),
           sic07 = ifelse(is.na(sic07)  & lag(sic07, n= 1) == lead(sic07, n=i), lag(sic07, n= 1), sic07 ),
           hpost = ifelse(is.na(hpost)  & lag(hpost, n= 1) == lead(hpost, n=i), lag(hpost, n= 1), hpost ),
           wpost = ifelse(is.na(wpost)  & lag(wpost, n= 1) == lead(wpost, n=i), lag(wpost, n= 1),wpost ),
           Gender=factor(as.numeric(ifelse(is.na(Gender) & lag(Gender, n=1) == lead(Gender, n=i), 
                                           lag(Gender),Gender)), levels = 1:length(levels(Gender)), labels = levels(Gender))
    )
}

## start date 

df <- df %>% 
  ungroup() %>%
  mutate(start_year = stringr::str_sub(empsta, -4, -1),
         start_month = stringr::str_sub(df$empsta, 1, str_length(df$empsta) - 4),
         start_month = ifelse(start_month < 10, as.character(start_month), paste0(0, start_month)),
         start_date =  zoo::as.yearmon(paste(start_year, start_month, sep="-")),
         start_date = ifelse(as.numeric(start_date >2018)| as.numeric(start_date<1950), NA, start_date),
         date = zoo::as.yearmon(paste(year, "04", sep="-")),
         tenure = (date  - start_date) * 12,
         tenure = ifelse(tenure < 0, NA, tenure))


## remove if observation is before first year observed in ASHE
df <- df %>% 
  group_by(piden) %>%
  mutate(min_year = min(min_year, na.rm=TRUE),
         max_year = min(max_year, na.rm=TRUE)) %>% 
  filter(year >= min_year & year <= max_year)   

save(df, file='W:/ASHE and LM Discovery/Data/ashe1997_2018_imp.Rdata')

####

load(file='W:/ASHE and LM Discovery/Data/ashe1997_2018_imp.Rdata')

# Keep mainland UK
df <- df %>%
  filter(!(region %in% c("Northern Ireland", "Channel Islands", "Isle of Man", "N.A." )))

# replace entreprise reference number by 0 if missing
df$entref <- ifelse(is.na(df$entref), 0, df$entref)

## non response stats
df$missing <- ifelse(is.na(df$sex), 1, 0 ) 
df$missing_after_imp <- ifelse(is.na(df$gpay), 1, 0 )


mean(df$missing)
mean(df$missing_after_imp)


### leaving employer (equal to one if left employer in previous period)
df<-df %>% 
  group_by(piden) %>%
  mutate(left_emp = as.numeric(entref!=lead(entref) & entref!=0),
         left_emp = ifelse(year == max_year & year < 2018, 1, left_emp), # for the last period obs in the data: they have left unless it's 2018
         left_emp = ifelse(entref==0 & left_emp==0, 2, left_emp)) # this is for the years when the person is out of work (so can't leave their employment)
df$left_emp [df$left_emp==2] <-NA   

## separation to employement : 1 if individuals is employed in the year after they left job
df$sep_emp <- df$left_emp
df$sep_emp <- ifelse(lead(df$entref==0) |  df$year == df$max_year | df$year == 2018 , 0, df$left_emp)
df$sep_emp[is.na(df$left_emp)] <-NA

## separation to employement - alternative definition : 1 if individuals is employed in the year t or t+1 after they left job
df$sep_emp2 <- df$sep_emp
df$sep_emp2 <- ifelse(lead(df$entref != 0, n = 2) , df$left_emp, 0)
df$sep_emp2[is.na(df$left_emp)] <-NA

df$sep_emp3 <- ifelse(df$sjd==2, 1, 0)
df$sep_emp3 <- ifelse(is.na(df$sjd==2), NA, df$sep_emp2)
## separation to non_employement

df$sep_nonemp <- ifelse(lead(df$entref==0) | df$year == df$max_year & df$year < 2018 ,    df$left_emp,0 )
df$sep_nonemp[is.na(df$left_emp)] <-NA

df$sep_nonemp2 <- ifelse(df$left_emp == 1 & df$sep_emp2 == 0, 1, 0)
df$sep_nonemp2[is.na(df$left_emp)] <-NA

## event

df$event <- ifelse(!is.na(df$left_emp), 0,NA)
df$event[df$sep_emp2 == 1] <- 1
df$event[df$sep_nonemp2 == 1] <- 2

df$event <- factor(df$event, levels=0:2, labels=c("Censored", "Changed job", "Left Labour Force"))


df$occ1 <- addNA(df$occ1)

## variables indicating if person moved home and changed job
# 
# df<-df %>% group_by(piden) %>%
#   mutate(moved_home=as.numeric(hpost!=lag(hpost)),
#          lag_dist_km=lag(dist_km),
#          lag_he=lag(he),
#          lag_gpay=lag(gpay),
#          d_dist_km=(dist_km-lag_dist_km)/lag_dist_km,
#          d_he=(he-lag_he)/lag_he,
#          d_gpay=(gpay-lag_gpay)/lag_gpay,
#          lag_occ = lag(occ1),
#          lag_tenure=lag(tenure)
#   )

# View(head(select(df, piden, year, hpost, entref, sep_emp, sep_nonemp, moved_home), n=1000))         

## get earliest age when observed in ASHE
#df <- df %>% group_by(piden) %>%
#  mutate(min_age = min(age, na.rm=TRUE))


## Create some covariatees##
df <- df %>% 
  mutate(female = ifelse(Gender == "Female", 1, 0),
    year = as.factor(year),
    lngpay = log(gpay + 1),
    lndist_km = log(dist_km + 1)
  )

df$age2 <- df$age ^ 2
df$hpay <- df$rhe
df$hpay <- ifelse(df$hpay <= 1, NA,  df$hpay)
df$lnhpay <- log(df$hpay)
df$lnhpayXfemale <- df$lnhpay * df$female
df$lnhours <- ifelse(df$thrs <=0, NA, log(df$thrs) )
df$year<-as.factor(df$year)

df %>% group_by(year) %>% summarise(mean(left_emp, na.rm = TRUE))

#save(df, file='W:/ASHE and LM Discovery/Data/ashe2002_2018_reg.Rdata') # needs to be saved when in the office
save(df, file='D:/LM Discovery/ashe2002_2018_reg.Rdata')

## Add information about postcode

# No need to run this again
#pcde <- data.table::fread("N:\\Vahe\\Commuting gap\\Postcode lookup\\NSPL_NOV_2018_UK.csv")
#save(pcde, file='N:\\Vahe\\Commuting gap\\Postcode lookup\\pcde.Rdata')

load('N:\\Vahe\\Commuting gap\\Postcode lookup\\pcde.Rdata')
pcde <- select(pcde, pcd, imd, ru11ind, ttwa, laua) %>%
  mutate(hpost = gsub(" ", "", pcd))
 
#### Regression analysis ####
#load('W:/ASHE and LM Discovery/Data/ashe2002_2018_reg.Rdata')
load('D:/LM Discovery/ashe2002_2018_reg.Rdata')

for (x in c("left_emp", "female", "tenure", "dist")){
  print(paste("Missing in", x, ":", sum(is.na(df[[x]]))))
}

# perhaps we should remove those aged 65
df <- df %>%
  filter(!is.na(left_emp) , !is.na(female),   age <= 65 , !is.na(tenure) & year != 2018 , !is.na(dist)) %>%
  mutate(hpost = gsub(" ", "", hpost))

df <- left_join(df, pcde, by=c('hpost'))

## number of observations
n_1 <- nrow(df)
n_ind_1 <- nrow(distinct(select(df, piden)))


# Urban/rural - not avilable for Northern Ireland
df$urban[df$region != "Northern Ireland"] <- 0

df$urban[(df$ru11ind == "A1" | df$ru11ind == "B1"|df$ru11ind == "C1"|df$ru11ind == "C2"|
            df$ru11ind == "1"|df$ru11ind == "2"|df$ru11ind == "3"|df$ru11ind == "4"|df$ru11ind == "5")] <- 1



df <- df%>%
  mutate(temporary = ifelse(pt==2, 1, 0),
         part_time= ifelse(ft == 2, 1, 0)) %>%
  select(piden, entref,  tenure, left_emp, sep_emp, sep_nonemp, event, female, Gender, lnhours, thrs, temporary,
         part_time, age, age2, dist, rpay, rhe, dist_km,
         lndist_km,lngpay, lnhpay, region,hpost, wpost, h_la, w_la,laua, ttwa, occ1, occ, sic07, year, urban, alday, idbrnemp, idbrsta)

## merge to travel time
#load("N:\\Vahe\\Commuting gap\\Data\\travel_time.RDS")
load("N:\\Vahe\\Commuting gap\\Data\\travel_time2.RDS")



df <- left_join(df,
                select(travel_time2, hpost, wpost, dr_duration_mins, pt_duration_mins, travel_time2),
                by=c("hpost", "wpost") )

# remove Ni because travel time is missing
df <- df %>%
  #filter(as.numeric(as.character(year)) >=2004 ) %>%
  mutate(region=droplevels(region), year=droplevels(year), occ1=droplevels(occ1))


## inspect relationship between distance and travel time
ggplot(df, aes(x=dist_km, y= travel_time2, colour=Gender))+
  geom_smooth(aes(fill=Gender))+
  theme_classic()


dist_sum <- df %>%
            mutate(ct_NA = is.na(travel_time2)) %>%
            group_by(ct_NA) %>%
            summarise(mean = mean(dist_km, na.rm = TRUE),
                    median = median(dist_km, na.rm = TRUE),
                    max=max(dist_km))

## drop if distance if greater than 400 km
# df <- df %>%
#   filter(dist_km <= 400)
# impute travel time when missing 9based on distance, by region

m2 <- lm(data=travel_time2, travel_time2 ~  dist_km *urban * region)


df$pred_travel_time <- predict(m2, df)


df <- df %>%
  mutate(
    travel_time_imp = ifelse(is.na(travel_time2), pred_travel_time, travel_time2),
    travel_time_imp_flag = is.na(travel_time2)
  )

# convert as datafarem (to avoid annoying warnings)
df <- as.data.frame(df)

# add some variables
df <- df %>%
  mutate(tenure1= tenure + 12,
         age2 = age^2 ,
         occ2=as.numeric(substr(as.character(occ), start = 1, stop = 2)))


soc_lookup <- read.csv('N:\\Vahe\\Commuting gap\\Data\\soc2010.csv')
soc_lookup2 <- soc_lookup %>%
  filter(!is.na(Sub.Major.Group)) %>%
  mutate(occ2= Sub.Major.Group,
         occ2_name= Hmisc::capitalize(tolower(Group.Title))) %>%
  select(occ2, occ2_name)

df$occ2<-factor(df$occ2, levels=soc_lookup2$occ2,
                  labels=soc_lookup2$occ2_name)
rm(soc_lookup, soc_lookup2)
# Remove unused levels in factor variables
df <- df %>%
  mutate_if(is.factor, droplevels)

# Quintile of employee size
df <- df %>% mutate(number_employees_q = as.factor(ntile(idbrnemp, 5)),
                    public= ifelse(idbrsta >=4, 1, 0))


# filter data by year
df <- df %>%
#  filter(as.numeric(as.character(year)) >=2003) %>% # because separation rates look wrong fro 2002 and 2017
  mutate(sic=as.factor(as.numeric(substr(as.character(sic07), start = 1, stop = 2))))


# remove if work less than 2 hours (likely miss recording)
sum(df$thrs < 2, na.rm=TRUE)

df <- df %>%
  filter(thrs >= 2)
# remove if pay or ct >= top 0.5%
top0.5_rhe= quantile(df$rhe, probs = c(0.005, 0.995), na.rm=TRUE)
top0.5_ct= quantile(df$travel_time_imp, probs = c(0.005, 0.995), na.rm=TRUE)
top0.5_alday=quantile(df$alday, probs = c(0.995), na.rm=TRUE)

bot0.5_rhe= quantile(df$rhe, probs = c(0.005), na.rm=TRUE)


df <- df %>%
  filter(thrs >= 2, travel_time_imp <= 240, rhe >= top0.5_rhe[[1]], rhe <= top0.5_rhe[[2]] ) %>%
  mutate( thrs= ifelse(thrs > 80, 80, thrs),
          alday = ifelse(alday > top0.5_alday, top0.5_alday, alday),
          thrs2=thrs^2,
          lnhpay =  ifelse(thrs < 2, NA, log(rhe)),
          lnhours =  ifelse(thrs < 2, NA, lnhours)) %>%
  filter(!is.na(lnhpay))

df <- df %>% mutate(alday2=alday ^2)
# generate interaction between TTWA and gender to include have a stratified cox model.
df$ttwaXGender <- interaction(df$ttwa, df$Gender)

#save(df, file='N:\\Vahe\\Commuting gap\\Data\\ashe2002_2018_reg_cleaned.Rdata')
df_m <- df
save(df_m, file='//nsdata1/ASHEdata/ASHE and LM Discovery/Data/ashe1997_2018_sum.Rdata')

df <- df %>%
  filter(!is.na(alday))

save(df, file='D:/LM Discovery\\ashe2002_2018_reg_cleaned.Rdata')
save(df, file='W:/ASHE and LM Discovery/Data/ashe2002_2018_reg_cleaned.Rdata')

rm(df_m, pcde)

# left_year <- df %>% 
#   group_by(year) %>% 
#   summarise(m=mean(left_emp))
# 
# left_year <- df %>% 
#   filter(age < 60) %>% 
#   group_by(year) %>% 
#   summarise(m=mean(left_emp))
#  
# df <- df %>%
#   group_by(piden) %>% 
#   mutate(left_job = ifelse(lead(year) != year +1 ))
# 
# 
# result<- "D:/LM Discovery/Presentations/"
# haven::write_dta(df, paste0(result,"ashe.dta"), version = 11)
# 
# 
# library(lfe)
# 
# 
# pres <- "D:/LM Discovery/Presentations/"
# 
# df_or<- df
# 
# df <- filter(df, !is.na(left_emp), !is.na(female)) 
#   
# 
# ## descriptives
# df$dist_cat<-factor(as.numeric(cut(df$dist_km, c(0,2.5, 5, 10, 15, 20, 50, 100, Inf))),
#                       levels = 1:8,
#                       labels = c('0 - 2.5', '2.5 - 5', '5 - 10', '10 - 15', '15 - 20', '20 - 50', '50-100', '100+') )
# 
# 
# sep_by_dist <- ggplot(data=df, aes(x=dist_km, y=sep_emp, group=Gender, colour=Gender))+
#   geom_smooth()+
#   xlim(c(0.1, 100)) +
#   theme_bw()
# 
# thrs_by_dist <- ggplot(data=df, aes(x=dist_km, y=thrs, group=Gender, colour=Gender))+
#   geom_smooth()+
#   xlim(c(0.1, 100)) +
#   theme_bw()
# 
# 
# sep_by_hpay <- ggplot(data=df, aes(x=hpay, y=sep_emp, group=Gender, colour=Gender))+
#   geom_smooth()+
#   xlim(c(5, 30)) +
#   theme_bw()
# 
# thrs_by_dist <- ggplot(data=df, aes(x=dist_km, y=thrs, group=Gender, colour=Gender))+
#   geom_smooth()+
#   xlim(c(0.1, 100)) +
#   theme_bw()
# 
# left_emp <- list()
# 
# left_emp[[1]] <- felm(data=df, left_emp ~ female+ age+ age2+lndist_km+lndist_kmXfemale+lngpay+ lngpayXfemale+year|0|0|piden) 
# 
# left_emp[[2]] <- felm(data=df, left_emp ~age+ age2+lndist_km+lndist_kmXfemale+lngpay+ lngpayXfemale+year|piden|0|piden) 
# 
# left_emp[[3]] <- felm(data=df, left_emp ~ female+ age+ age2+lndist_km+lndist_kmXfemale+lngpay+ lngpayXfemale+year+occ1+region|0|0|piden) 
# 
# left_emp[[4]] <- felm(data=df, left_emp ~ age+age+ age2+lndist_km+lndist_kmXfemale+lngpay+ lngpayXfemale+year+occ1+region|piden|0|piden) 
# 
# stargazer::stargazer(left_emp, 
#                      keep=c("lag_lndist_km", "lag_lndist_kmXfemale", 'lag_lngpay', "lag_lngpayXfemale" ),
#                      covariate.labels = c("Log distance", "Log distance X female", "Log weekly pay", "Log weekly pay X female"),
#                      add.lines=list(c('Individual FE', 'No',  'Yes', 'No', 'Yes'), 
#                                     c('Occupation and region' , 'No',  'No', 'Yes', 'Yes')),
#                      type = 'latex',
#                      dep.var.caption="", dep.var.labels=c('Left job'),
#                      omit.stat =c('rsq','ser'),
#                      notes='All models control for age. Standard errors clustered at individual level parentheses',
#                      notes.align='l', notes.append = FALSE , digits = 4,
#                      out = "D:/LM Discovery/Presentations/left_emp.tex")
# 
# sep_emp <- list()
# sep_emp[[1]] <- felm(data=df, sep_emp ~ female+ age+ age2+lndist_km+lndist_kmXfemale+log_he+ log_heXfemale+year|0|0|piden) 
# 
# sep_emp[[2]] <- felm(data=df, sep_emp ~age+ age2+lndist_km+lndist_kmXfemale+lngpay+ lngpayXfemale+year|piden|0|piden) 
# 
# sep_emp[[3]] <- felm(data=df, sep_emp ~ female+ age+ age2+lndist_km+lndist_kmXfemale+lngpay+ lngpayXfemale+year+occ1+region|0|0|piden) 
# 
# sep_emp[[4]] <- felm(data=df, sep_emp ~ age+age+ age2+lndist_km+lndist_kmXfemale+lngpay+ lngpayXfemale+year+occ1+region|piden|0|piden) 
# 
# 
# sep_emp[[1]] <- felm(data=df, sep_emp ~ female+ age+ age2+lndist_km+lndist_kmXfemale+lnhpay+ lnhpayXfemale+year|0|0|piden) 
# 
# hrs <- felm(data=df, lnhours  ~ female+ age+ age2+lndist_km+lndist_kmXfemale+lnhpay+ lnhpayXfemale+year|0|0|piden) 
# 
# #require(rlist)
# #list.save(sep_emp, file = "D:/LM Discovery/Presentations/reg_res.RData")
# 
# stargazer::stargazer(sep_emp, 
#                      keep=c("female", "lndist_km", "lndist_kmXfemale", "lngpay" ),
#                      covariate.labels = c("Female","Log distance", "Log distance X female", "Log weekly pay", "Log weekly pay X female"),
#                      add.lines=list(c('Individual FE', 'No',  'No', 'Yes', 'Yes'), 
#                                     c('Occupation and region' , 'No',  'Yes', 'No', 'Yes')),
#                      type = 'latex',
#                      dep.var.caption="", dep.var.labels=c('Gender pay gap'),
#                      omit.stat =c('rsq','ser'),
#                      notes='All models control for age. Standard errors clustered at individual level parentheses',
#                      notes.align='l', notes.append = FALSE , digits = 4,
#                      out = "D:/LM Discovery/Presentations/sep_emp.tex")
# 
# 
# lnhours <- list()
# lnhours[[1]] <- felm(data=df, lnhours ~ female+ age+ age2+lndist_km+lndist_kmXfemale+lnhpay+ lnhpayXfemale+year|0|0|piden) 
# 
# lnhours[[2]] <- felm(data=df, lnhours ~age+ age2+lndist_km+lndist_kmXfemale+lnhpay+ lnhpayXfemale+year|piden|0|piden) 
# 
# lnhours[[3]] <- felm(data=df, lnhours ~ female+ age+ age2+lndist_km+lndist_kmXfemale+lnhpay+ lnhpayXfemale+year+occ1+region|0|0|piden) 
# 
# lnhours[[4]] <- felm(data=df, lnhours ~ age+age+ age2+lndist_km+lndist_kmXfemale+lnhpay+ lnhpayXfemale+year+occ1+region|piden|0|piden) 
# 
# 
# sep_nonemp <- list()
# sep_nonemp[[1]] <- felm(data=df, sep_nonemp ~ female+ age+age2+lag_lndist_km+lag_lndist_kmXfemale+
#                           lag_lngpay+ lag_lngpayXfemale+year |0|0|piden) 
# 
# sep_nonemp[[2]] <- felm(data=df, sep_nonemp ~ age+age2+lag_lndist_km+lag_lndist_kmXfemale+
#                           lag_lngpay+ lag_lngpayXfemale+year|piden|0|piden) 
# 
# sep_nonemp[[3]] <- felm(data=df, sep_nonemp ~ female+ age+age2+lag_lndist_km+lag_lndist_kmXfemale+
#                           lag_lngpay+ lag_lngpayXfemale+year+occ1+region|0|0|piden) 
# 
# sep_nonemp[[4]] <- felm(data=df, sep_nonemp ~ age+age2+lag_lndist_km+lag_lndist_kmXfemale+
#                           lag_lngpay+ lag_lngpayXfemale+year+occ1+region|piden|0|piden) 
# 
# stargazer::stargazer(sep_nonemp, 
#                      keep=c("lag_lndist_km", "lag_lndist_kmXfemale", 'lag_lngpay', "lag_lngpayXfemale" ),
#                      covariate.labels = c("Log distance", "Log distance X female", "Log weekly pay", "Log weekly pay X female"),
#                      add.lines=list(c('Individual FE', 'No',  'Yes', 'No', 'Yes'), 
#                                     c('Occupation and region' , 'No',  'No', 'Yes', 'Yes')),
#                      type = 'latex',
#                      dep.var.caption="", dep.var.labels=c('Left job'),
#                      omit.stat =c('rsq','ser'),
#                      notes='All models control for age. Standard errors clustered at individual level parentheses',
#                      notes.align='l', notes.append = FALSE , digits = 4,
#                      out = "D:/LM Discovery/Presentations/sep_nonemp.tex")
# 
# 
# 
# left_emp <- list()
# left_emp[[1]] <- felm(data=df, left_emp ~ female+ age+lag_lndist_km+lag_lndist_kmXfemale+
#                         lag_lngpay+ lag_lngpayXfemale+year|0|0|piden) 
# 
# left_emp[[2]] <- felm(data=df, left_emp ~ age+lag_lndist_km+lag_lndist_kmXfemale+
#                         lag_lngpay+ lag_lngpayXfemale+year|piden|0|piden) 
# 
# left_emp[[3]] <- felm(data=df, left_emp ~ female+ age+lag_lndist_km+lag_lndist_kmXfemale+
#                         lag_lngpay+ lag_lngpayXfemale+year+occ1+region|0|0|piden) 
# 
# left_emp[[4]] <- felm(data=df, left_emp ~ age+lag_lndist_km+lag_lndist_kmXfemale+
#                         lag_lngpay+ lag_lngpayXfemale+year+occ1+region|piden|0|piden) 
# 
# stargazer::stargazer(left_emp, 
#                      keep=c("lag_lndist_km", "lag_lndist_kmXfemale", 'lag_lngpay', "lag_lngpayXfemale" ),
#                      covariate.labels = c("Log distance", "Log distance X female", "Log weekly pay", "Log weekly pay X female"),
#                      add.lines=list(c('Individual FE', 'No',  'Yes', 'No', 'Yes'), 
#                                     c('Occupation and region' , 'No',  'No', 'Yes', 'Yes')),
#                      type = 'latex',
#                      dep.var.caption="", dep.var.labels=c('Left job'),
#                      omit.stat =c('rsq','ser'),
#                      notes='All models control for age. Standard errors clustered at individual level parentheses',
#                      notes.align='l', notes.append = FALSE , digits = 4,
#                      out = "D:/LM Discovery/Presentations/left_emp.tex")
# 
# 
# ## by region
# 
# res_london <- felm(data=filter(df, region == "London"), sep_emp ~ female+ age+lag_lndist_km+lag_lndist_kmXfemale+
#                      lag_lngpay+ lag_lngpayXfemale+year+occ1|0|0|piden)
# res_other <- felm(data=filter(df, region != "London"), sep_emp ~ female+ age+lag_lndist_km+lag_lndist_kmXfemale+
#                     lag_lngpay+ lag_lngpayXfemale+year+occ1|0|0|piden)
# 
# 
# res_region <- lapply(unique(df$region)[-3], function (x){
#   felm(data=filter(df, region == x), sep_emp ~ female+ age+lag_lndist_km+lag_lndist_kmXfemale+
#          lag_lngpay+ lag_lngpayXfemale+year+occ1|0|0|piden)
# })
# 
# regs <- as.character(unique(df$region)[-3])
# stargazer::stargazer(res_region, 
#                      keep=c("lag_lndist_km", "lag_lndist_kmXfemale", 'lag_lngpay', "lag_lngpayXfemale" ),
#                      column.labels =regs,
#                      covariate.labels = c("Log distance", "Log distance X female", "Log weekly pay", "Log weekly pay X female"),
#                      type = 'html',
#                      dep.var.caption="", dep.var.labels=c('Left job'),
#                      omit.stat =c('rsq','ser'),
#                      notes='All models control for age. Standard errors clustered at individual level parentheses',
#                      notes.align='l', notes.append = FALSE ,
#                      out = "D:/LM Discovery/Presentations/res_reg.html")
# 
# 
# 
# ##by age
# df$age_gr <- cut(df$age, c(0, 24, 34, 44, 54,  Inf),
#                  labels = c( "Less than 25", "25-34", "35-44", "45-54", "55+" ))
# res <- purrr::map_dfr(c( "Less than 25", "25-34", "35-44", "45-54", "55+" ), function(x){
#   d <- broom::tidy(felm(data=filter(df, age_gr == x), sep_emp ~ female+ age+lag_lndist_km+lag_lndist_kmXfemale+
#                           lag_lngpay+ lag_lngpayXfemale+year+occ1+region|0|0|piden)) %>%
#     mutate(age = x)
#   d})
# res_dist <- res %>% filter( term =="lag_lndist_kmXfemale" | term == "lag_lngpayXfemale") %>%
#   mutate(up_ci = estimate + 1.97 * std.error, low_ci = estimate - 1.97 * std.error)
# 
# res_dist$age <- factor(res_dist$age, res_dist$age) 
# save(res_dist,  file = "D:/LM Discovery/Presentations/reg_res_age.RData")
# 
# 
# ggplot(data = res_dist, aes(x=age, y=estimate, group = term, colour=term))+geom_point()+
#   geom_linerange(aes(ymin=low_ci, ymax=up_ci))+
#   geom_hline(yintercept = 0) +
#   coord_flip()  + scale_x_discrete(limits = rev(levels(res_dist$age)))+
#   labs(x="Age" , y="p.p.", title="Gender difference in effect of distance on probability of changing job ")+
#   theme_bw()
# 
# +theme(text = element_text(size=20))
# 
# ## by occupation
# occ_list <- as.character(unique(df$occ1))
# 
# res_occ <- purrr::map_dfr(occ_list[-3], function(x){
#   d <- broom::tidy(felm(data=filter(df, occ1 == x), sep_emp ~ female+ age+lag_lndist_km+lag_lndist_kmXfemale+
#                           lag_lngpay+ lag_lngpayXfemale+year+region|0|0|piden)) %>%
#     mutate(occ1 = x)
#   d})
# 
# ggplot(data = res_occ, aes(x=occ1, y=estimate))+geom_point()+
#   geom_linerange(aes(ymin=low_ci, ymax=up_ci))+
#   geom_hline(yintercept = 0) +
#   coord_flip()  + scale_x_discrete(limits = rev(levels(res_occ$occ1)))+
#   labs(y="p.p.", title="Gender difference in effect of distance on probability of changing job ")+
#   theme_bw()
# 
# res_occ <- res_occ %>% filter( term =="lag_lndist_kmXfemale") %>%
#   mutate(up_ci = estimate + 1.97 * std.error, low_ci = estimate - 1.97 * std.error)
# 
# res_occ$occ1 <- factor(res_occ$occ1, res_occ$occ1) 
# 
# rc<-list()
# rc[[1]] <- glm(data=df, sep_emp ~ female+ age+lag_lndist_km+lag_lndist_kmXfemale+
#                  lag_lngpay+ lag_lngpayXfemale+year+occ1+region, family = binomial(link = "logit"))
# rc[[2]] <- felm(data=filter(df, region != "London"), sep_emp ~ female+ age+lag_lndist_km+lag_lndist_kmXfemale+
#                   lag_lngpay+ lag_lngpayXfemale+year+occ1+region|0|0|piden)
# rc[[3]] <- felm(data=df, sep_emp ~ female+ age+lag_lndist_km+lag_lndist_kmXfemale+
#                   lag_lngpay+ lag_lngpayXfemale|year+occ1+h_la|0|piden)
# 
# 
# rc[[4]] <- felm(data=df, sep_emp ~ female+ age+lag_lndist_km+lag_lndist_kmXfemale+
#                   lag_lngpay+ lag_lngpayXfemale+log(lag_tenure2)+year+occ1|0|0|piden)
# 
# 
# 


