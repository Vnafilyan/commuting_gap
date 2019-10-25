##----------------------------------------------------##
## Exploratory analysis of Travel to Work patterns
## Created by VN on 26/10/2017
##----------------------------------------------------##



#--Library path:
.libPaths("D:/R/library")

## Load packages ##
#library(ff)
#options(fftempdir = "D:/LM Discovery")
library(data.table)
library(spatstat) ## for weighted median
library(tidyverse)
library(ggplot2)

x <- c("haven", "ggmap", "rgdal", "rgeos", "maptools", "tidyverse", 'spatstat', 'data.table', 'ggplot2', "tmap")
#install.packages(x) # warning: uncommenting this may take a number of minutes
lapply(x, library, character.only = TRUE) 


## set directory
setwd('//nsdata1/ASHEdata/ASHE and LM Discovery')

##------------------------------------------##
##                  Load data               ##
##        and select relevant varaiables    ##
##------------------------------------------##


#### 1) get Postcode lookup file (NO NEED TO RERUN SO COMMENTED OUT)


#pcde<-fread('D:/Perso/PLW/National_Statistics_Postcode_Lookup_UK.csv')
#save(pcde, file='//nsdata1/ASHEdata/ASHE and LM Discovery/Data/Postcode lookup/pcde.Rdata')

load('//nsdata1/ASHEdata/ASHE and LM Discovery/Data/Postcode lookup/pcde.Rdata')

pcde$Postcode<-as.character(pcde$Postcode.3)

pcde<-select(pcde, Latitude, Longitude, Postcode, Easting, Northing, Local.Authority.Name, Country.Name, Region.Name)


#### 2) ASHE data          ####
### 2A- AHESE dataset 97-2011

## read in SAS dataset 97-2011 and save in .Rdata (NO NEED TO RERUN SO COMMENTED OUT)

#df <- haven::read_sas('//nsdata1/ASHEdata/ASHE and LM Discovery/Data/Analysis/asheuk_1997_2011prov_NINO.sas7bdat')
#save(df, file='//nsdata1/ASHEdata/ASHE and LM Discovery/Data/Postcode lookup/ashe1997_2001.Rdata')

load('//nsdata1/ASHEdata/ASHE and LM Discovery/Data/Postcode lookup/ashe1997_2001.Rdata')



df <- select(df, nino1, NINO, piden, numstrata, year, HPOST, WPOST, SEX, age, BPAY, GPAY, THRS, EMPSTA,alday, calwght,  FT, pt, occ00, IDBRSTA, IDBRNEMP, ENTREF, SJD, LOP , MJOB, DJOB ,
           sic07, sic03) %>%
  filter(year<2011) %>%
  mutate(piden=as.numeric(piden))

gc()
names(df) <- tolower(names(df))

## read in dta dataset 2011-2018 and save in .Rdata (NO NEED TO RERUN SO COMMENTED OUT)

#df2 <- haven::read_sas("//nsdata1//asheoutputdata//Datasets from LMHD - DO NOT ALTER//ashe_2011_2018_uk.sas7bdat")
#save(df2, file='//nsdata1/ASHEdata/ASHE and LM Discovery/Data/Postcode lookup/ashe2011_2018.Rdata')

load('//nsdata1/ASHEdata/ASHE and LM Discovery/Data/Postcode lookup/ashe2011_2018.Rdata')

names(df2)<-tolower(names(df2))

df2 <- select(df2,  nino1, piden, numstrata, year, hpost, wpost, sex, bpay, gpay, thrs, empsta, alday, calwght, age , ft,pt, 
            occ10, dob, idbrsta, idbrnemp, entref, sjd, lop, djob, mjob, sic07) %>%
    mutate(piden=as.numeric(piden))
gc()


names(df2) <- tolower(names(df2))

nrow(inner_join(select(df2, entref), select(df, entref)))
## combine two datasets
df <- bind_rows(df, df2)

rm(df2)

df <- df %>% mutate(hpost = str_replace(hpost, " ", ""),
                    wpost = str_replace(wpost, " ", ""))

pcde$Postcode <- str_replace(pcde$Postcode, " ", "")


##------------------------------------------##
##              Data cleaning               ##
##------------------------------------------##

#### 1) Compute distance travelled to work ####

## get coordinates of home postcode
# get coordinates of home postcode
df_m <- left_join(as.data.frame(df), pcde, by = c('hpost' = 'Postcode'))

df_m <- df_m %>%
  mutate(
    h_lat = Latitude,
    h_long = Longitude,
    h_east = Easting,
    h_north = Northing,
    h_la = Local.Authority.Name,
    h_ctry = Country.Name,
    h_reg = Region.Name
  )  %>%
  select(
    -Latitude,
    -Longitude,
    -Easting,
    -Northing,
    -Local.Authority.Name,
    -Country.Name,
    -Region.Name
  )

## get coordinates of work postcode
df_m <- left_join(as.data.frame(df_m), pcde, by = c('wpost' = 'Postcode')) %>%
  mutate(
    w_lat = Latitude,
    w_long = Longitude,
    w_east = Easting,
    w_north = Northing,
    w_la = Local.Authority.Name,
    w_ctry = Country.Name,
    w_reg = Region.Name
  )  %>%
  select(
    -Latitude,
    -Longitude,
    -Easting,
    -Northing,
    -Local.Authority.Name,
    -Country.Name,
    -Region.Name
  )

## compute distance
df_m <-  mutate(df_m, dist = sqrt((h_east - w_east) ^ 2 + (h_north - w_north) ^ 2), dist_km =
           dist / 1000)

summary(df_m$dist)

#### 2) derive other variables ####

## Gender
df_m$Gender<-factor(df_m$sex, levels=1:2, labels=c('Male', 'Female') )

## get 1 digit occupation
df_m <- mutate(df_m, occ=occ00, 
               occ=ifelse(is.na(occ), occ10, occ),
               occ1=as.numeric(substr(as.character(occ), start = 1, stop = 1)))

df_m$occ1 <- factor(df_m$occ1, levels=1:9, 
                  labels=c('Managers/directors',
                           'Professional',
                           'Associate prof./technical ',
                           'Administrative',
                           'Skilled trades',
                           'Caring/leisure/service',
                           'Sales/customer service',
                           'Plant/machine operatives',
                           'Elementary' ))

## remove people from channel islands and Isle of Man
#df_m<- filter(df_m, h_ctry!='Channel Islands' & h_ctry!= 'Isle of Man' &h_ctry !='')

## Region
df_m < -mutate(df_m, 
               h_ctry = ifelse(h_ctry == "", 'N.A.', as.character(h_ctry)),
               region=as.factor(ifelse(h_ctry=='England', as.character(h_reg), as.character(h_ctry))))

df_m %>%
  group_by(year) %>%
  summarise( sum(is.na(ft)), sum(is.na(occ10)) )


## hourly earnings and logs


df_m <-df_m %>% 
  mutate(he = gpay / thrs,
    log_gpay = log(gpay),
    log_he = log(he))

df_m$he[df_m$he == Inf] <- NA

## remove old data for 2004 and 2006
df_m <-
  filter(df_m,
         year != 2004 &
           year != 2006 |
           year == 2004 & numstrata == 3 | year == 2006 & numstrata == 4)


## get main job indicator

df_m <- df_m %>%
  group_by(piden, year) %>%
  arrange(piden, year, desc(gpay)) %>%
  mutate(N = n(),
         n = row_number(),
         main_job = ifelse(n == 1, 1, 0))

## count how many waves people are being tracked for
counts <- df_m %>% filter(main_job == 1) %>%
  group_by(nino) %>%
  summarise(N = n())



#### 3) identify duplicates

## remove records with no NINO
N_raw <- nrow(df_m)
df_m <- filter(df_m,!is.na(piden))
N_raw - nrow(df_m)

## check duplicates in term of nino, year and employer
df_m <-
  df_m %>% group_by(piden, year, entref) %>% mutate(N = n(), n = row_number())

## display by year
df_m %>%
  group_by(year) %>%
  summarise(mean(N), mean(n))

table(df_m$N)
#View(filter(df_m, N>1))
## NOTE: most duplicates by NINO and YEAR seem genuine (people having several jobs)


## quick fix : remove duplicated observations
#df_m<-filter(df_m, n==1) %>% select(-N, -n)


## get pay in 2018 prices
cpi <- read.csv('//nsdata1/ASHEdata/ASHE and LM Discovery/Data/CPI/cpi.csv')
cpi <- cpi[, c(1, 4)]

df_m <- left_join(df_m, cpi, by = "year")
df_m <- mutate(df_m, rpay = gpay * multiplier,
               rhe = he * multiplier)


## save cleaned data and remove obsejts no longer needed

#save
save(df_m, file='//nsdata1/ASHEdata/ASHE and LM Discovery/Data/ashe1997_2018.Rdata')



#remove objects
rm(df, pcde)

#clean RAM
gc()

###### travel time estimates ######




### driving time ###
path_dr <- "N:\\Vahe\\Commuting gap\\Data\\allTrips\\allTrips\\driving"
files_list <- list.files(path_dr)

driving <- purrr::map_dfr(1:length(files_list), function(d) {
  d1 <- readr::read_csv(paste0(path_dr, "\\", files_list[[d]])) %>%
    select(
      hpost = origin,
      wpost = destination,
      dr_distance_km = distance_km,
      dr_duration_mins = duration_mins
    ) %>%
    mutate(hpost = str_replace(hpost, " ", ""),
           wpost = str_replace(wpost, " ", ""))
  d1
})

### time by publuc transport ###
path_pt <- "N:\\Vahe\\Commuting gap\\Data\\allTrips\\allTrips\\public-transport"
files_list <- list.files(path_pt)

public_transport <-
  purrr::map_dfr(1:length(files_list), function(d) {
    d1 <- readr::read_csv(paste0(path_pt, "\\", files_list[[d]])) %>%
      select(
        hpost = origin,
        wpost = destination,
        pt_distance_km = distance_km,
        pt_duration_mins = duration_mins
      ) %>%
      mutate(hpost = str_replace(hpost, " ", ""),
             wpost = str_replace(wpost, " ", ""))
    d1
  })

## combine the two files together ##
travel_time <- inner_join(driving, public_transport) %>%
  mutate(travel_time = ifelse(
    is.na(pt_duration_mins),
    dr_duration_mins,
    ifelse(
      pt_duration_mins >= pt_duration_mins,
      dr_duration_mins,
      pt_duration_mins
    )
  ))

rm(driving, public_transport)




save(travel_time, file="N:\\Vahe\\Commuting gap\\Data\\travel_time.RDS")



### merge travel time
load(file='//nsdata1/ASHEdata/ASHE and LM Discovery/Data/ashe1997_2018.Rdata')
load("N:\\Vahe\\Commuting gap\\Data\\travel_time.RDS")

df_m <- left_join(df_m, travel_time, by=c("hpost", "wpost"))
summary(df_m$travel_time)
summary(df_m$dist_km)
rm(travel_time)
save(df_m, file='//nsdata1/ASHEdata/ASHE and LM Discovery/Data/ashe1997_2018.Rdata')





#### derive travel time based on driving and public transport time


load('N:\\Vahe\\Commuting gap\\Postcode lookup\\pcde.Rdata')
pcde <- select(pcde, pcd, imd, ru11ind, ttwa, laua) %>%
  mutate(hpost = gsub(" ", "", pcd))

load(file='//nsdata1/ASHEdata/ASHE and LM Discovery/Data/ashe1997_2018.Rdata')
# remove Northern ireland, Channel Islands etc

df_m <- filter(df_m, !is.na(dist_km) )

df_m <- filter(df_m,  !(region %in% c("Channel Islands", "Isle of Man", "N.A." , "Northern Ireland" ))) %>% 
  mutate(region=droplevels(region))

df_m <- left_join(df_m, pcde, by=c('hpost')) 


# Urban/rural - 
df_m$urban <- 0

df_m$urban[(df_m$ru11ind == "A1" | df_m$ru11ind == "B1"|df_m$ru11ind == "C1"|df_m$ru11ind == "C2"|
            df_m$ru11ind == "1"|df_m$ru11ind == "2"|df_m$ru11ind == "3"|df_m$ru11ind == "4"|df_m$ru11ind == "5")] <- 1


#df_m <- mutate(df_m, missing_time = is.na(travel_time), same_reg = h_reg==w_reg)

# As the moment,  travel time is equal  driving time if it is shorter than public transport
# Let's force it to use public transport if workplace is in central london or iflive in inner london

central_london <- c("City of London", "Camden", "Islington", "Kensington and Chelsea", "Lambeth",  "Southwark","Westminster" )
inner_london <- c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith and Fulham", "Islington", "Kensington and Chelsea", "Lambeth", "Lewisham", "Southwark", "Tower Hamlets", "Wandsworth", "Westminster" )
inner_london %in%  unique(df_m$w_la)

## derive travel time (use driving time, unless public transport is quicker , or driving duration is missing or implausible
## (except if pt duration is greater than 4 hours)


df_m <- df_m %>% 
  mutate(travel_time = ifelse(dist_km < dr_distance_km, dr_duration_mins, NA), # remove implausible cases (driving distance should be > flying distance)
        # if pt duration is shorter than driving and less than 4 hours, use it
          travel_time = ifelse(dr_duration_mins >= pt_duration_mins |
                                is.na(dr_duration_mins) & (!is.na(pt_duration_mins) & pt_duration_mins< 240), 
                              pt_duration_mins, travel_time),
        # if no public transport duration, use driving time
         travel_time=ifelse(is.na(pt_duration_mins), dr_duration_mins, travel_time))      

## if live in inner london, or work in central london, then use PT time

df_m$travel_time2 <- ifelse((df_m$h_la %in% inner_london|df_m$w_la %in% central_london) & !is.na(df_m$pt_duration_mins), 
                            df_m$pt_duration_mins,
                            df_m$travel_time)



## Some travel time values are obviously wrong - due to impossible public transport time and no driving time
# Let's set travel_time2 to NA if it's higher than   top 0.1% of travel  time 
# 
travel_time2 <- df_m %>% 
  ungroup() %>% 
  select(hpost, wpost, dr_distance_km, dr_duration_mins, pt_duration_mins, pt_distance_km, travel_time, travel_time2,
         urban, region, w_la, h_la, dist_km) %>%
  distinct()

# number of records with straigtline distance  > travel distance
travel_time2 <- travel_time2%>% 
  mutate(diff_dist = dr_distance_km - dist_km )


N_dist_issue <- sum(travel_time2$diff_dist < -1, na.rm=TRUE)
print(paste("N:", N_dist_issue, "; %:", N_dist_issue/nrow(travel_time2)))

## initial NAs ##
# function to print number of  NAs
print_na <- function(x){
  N_na = sum(is.na(travel_time2[[x]]))
  print(paste("N:", N_na, "; %:", N_na/nrow(travel_time2) *100))
}


print_na("dr_duration_mins")
print_na("pt_duration_mins")
print_na("travel_time2")

# set travel time to missing if > 4 hours

travel_time2 <- travel_time2%>% 
  mutate(travel_time2 = ifelse(travel_time2 > 240, NA, travel_time2))
print_na("travel_time2")


save(travel_time2, file="N:\\Vahe\\Commuting gap\\Data\\travel_time2.RDS")




# impute travel time when missing 9based on distance, by region

# # impute travel time when missing 9based on distance, by region
# m0 <- lm(data=travel_time2, travel_time2 ~  dist_km*urban + region)
# m1 <- lm(data=travel_time2, travel_time2 ~  dist_km *urban + region)
# m2 <- lm(data=travel_time2, travel_time2 ~  dist_km *urban * region)
# m3 <- lm(data=travel_time2, travel_time2 ~  poly(dist_km,2) *urban * region)
# m4 <- lm(data=travel_time2, travel_time2 ~  log(dist_km + 1) *urban * region)
# m3 <- lm(data=travel_time2, travel_time2 ~  poly(dist_km,) *urban * region)
# #m3 <- lm(data=travel_time2, travel_time2 ~  poly(dist_km, 2) * urban + ttwa)
# 
# modelr::mse(m0, travel_time2)
# modelr::mse(m1, travel_time2)
# modelr::mse(m2, travel_time2)
# modelr::mse(m3, travel_time2)
m <- lm(data=travel_time2, travel_time2 ~  dist_km * urban * region)


df_m$pred_travel_time <- predict(m, df_m)

df_m <- df_m %>% 
  mutate(travel_time_imp= ifelse(is.na(travel_time2), pred_travel_time, travel_time2 ),
         travel_time_imp_flag = is.na(travel_time2))

summary(df_m$travel_time_imp)    

save(df_m, file='//nsdata1/ASHEdata/ASHE and LM Discovery/Data/ashe1997_2018_sum.Rdata')


  
