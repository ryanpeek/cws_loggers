# 02 ADD NEW DATA

library(tidyverse)
library(lubridate)
library(purrr)
library(fs)

# Read Master Data --------------------------------------------------------

load("data_output/2011_2016_solinst_hourly_all.rda")

master <- hrly_out

# View Data ---------------------------------------------------------------

# plot all sites/times
# ggplot() +
#   geom_line(data=hrly_out,
#             aes(x=datetime, y=level_comp, color=site, group=WY)) +
#   facet_grid(site~., scales="free")

# look at range of data
hrly_out %>% 
  group_by(site, WY)  %>% 
  summarize(
    "date_mx"=max(as.Date(datetime))) %>% 
  print(n=Inf)
# rubicon stops in 2015

# Read in New Data  --------------------------------------------------------

# list all files that meet specific year and file type
(sol_files <- fs::dir_ls(path = "data/solinst/", regexp = "201(7|8)\\_..+\\_solinst\\_*"))

# read in all with purrr
sol <- sol_files %>%
  map_dfr(read_csv, skip=14, col_types="cccdd",
          col_names = c("date","time","ms","level", "temp_C"), .id = "source")

# make datetime
sol$datetime <- mdy_hms(paste0(sol$date, " ", sol$time))

# make datetime but need to split out diff datetypes first:
# sol_ymd <- sol %>% filter(source %in% c("data/solinst/2017_NFA_solinst_06_30.csv"))
# sol_ymd$datetime <- ymd_hms(paste0(sol_ymd$date, " ", sol_ymd$time))
# sol_mdy <- sol %>% filter(source !="data/solinst/2017_NFA_solinst_06_30.csv")
# sol_mdy$datetime <- mdy_hms(paste0(sol_mdy$date, " ", sol_mdy$time))

# recombine:
#sol_new <- bind_rows(sol_ymd, sol_mdy) %>% select(-date, -ms, -time) 

sol_new <- sol %>% select(-date, -ms, -time) %>% 
  mutate(compensated="N",
         site = substr(source, start = 19, stop = 21)) %>% 
  add_WYD(., "datetime")

# cleanup
#rm(sol_mdy, sol_ymd, sol)

# check data
ggplot() +
  geom_line(data=sol_new,
            aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales="free")

# adjust stage here:
sol_new <- sol_new %>% 
  mutate(level=if_else(site=="CLA" & WY>=2017, 
                            level - 9.45, level),
         level=if_else(site=="TUO" & WY>=2017,
                            level - 9, level),
         level=if_else(site=="NFA" & datetime>=ymd_hms("2017-06-30 14:00:00"),
                            level - 9, level),
  )

# check data again
ggplot() +
  geom_line(data=sol_new,
            aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales="free")


# Pull Data from USGS -----------------------------------------------------

library(dataRetrieval)
# data type: 00065 Stage, 00060 Flow
dataCodes <- c("00065","00060") # parameter codes
sites <- c("11413000") # NFA=11427000, NFY=11413000
#siteINFO <- readNWISsite(sites)

# whats avail?
whatNWISdata(siteNumber=sites,service=c("iv","dv"), statCd="00003")

# using package
# nfy<-readNWISuv(siteNumbers = sites, parameterCd = dataCodes, startDate = "2016-10-01", 
#            endDate = "2018-09-30", tz = "America/Los_Angeles")

# NFY: using custom function
riv <- "NFY"

get.USGS(gage = "11413000", river = riv, sdate="2016-09-01", edate="2018-09-30")

# pull relevant data for joining:
nfy_hr <- NFY_hr %>% select(datetime, stage_m) %>% 
  rename(level_comp=stage_m) %>% # match col names
  mutate( # make some matching cols
    site = riv,
    level_comp=level_comp-.27, # adjust stage here so it matches
    compensated = "Y") %>% 
  add_WYD(., "datetime")

# cleanup:
rm(NFY_15, NFY_dy, NFY_hr)

# Merge USGS with Master --------------------------------------------------

# bind the datasets
master_updated <- master %>% bind_rows(., nfy_hr)

# bind the solinst data
master_updated <- master_updated %>% bind_rows(., sol_new)

# update level:
master_updated <- master_updated %>% 
  mutate(level_comp = if_else(is.na(level_comp), level, level_comp))

# plot and check:
# ggplot() +
#   geom_line(data=master_updated[master_updated$site==riv,],
#             aes(x=datetime, y=level_comp, color=site, group=WY)) +
#   facet_grid(site~., scales="free")
# may need to go back here to adjust stage so it matches raw data

# plot all sites/times
ggplot() +
  geom_line(data=master_updated,
            aes(x=datetime, y=level_comp, color=site, group=WY)) +
  facet_grid(site~., scales="free")

# plot by site
ggplot() +
  geom_line(data=master_updated[master_updated$site=="NFA",],
            aes(x=datetime, y=level_comp, color=site, group=WY))
    
# Make Daily --------------------------------------------------------------

master_daily<- master_updated %>%
  mutate(date = floor_date(datetime, unit = "day")) %>% 
  group_by(site, date) %>%
  summarize("temp.avg"=mean(temp_C,na.rm=TRUE),
            "temp.sd"= sd(temp_C,na.rm=TRUE),
            "temp.cv"= (temp.sd/temp.avg),
            "temp.min"=min(temp_C,na.rm=TRUE),
            "temp.max"=max(temp_C,na.rm=TRUE),
            "temp.rng" = (max(temp_C)-min(temp_C)),
            "lev.avg"=mean(level_comp,na.rm=TRUE),
            "lev.sd"= sd(level_comp,na.rm=TRUE),
            "lev.cv"= (lev.sd/lev.avg),
            "lev.min"=min(level_comp,na.rm=TRUE),
            "lev.max"=max(level_comp,na.rm=TRUE))%>%
  transform("lev.delt" = (lag(lev.avg)-lev.avg)/lev.avg,
            "temp.7.avg"= runmean(temp.avg, k=7, endrule="mean",align="left"),
            "temp.7.avg_L"= runmean(temp.avg, k=7, endrule="mean",align="left"),
            "lev.7.avg"= runmean(lev.avg, k=7, endrule="mean",align="left")) %>%
  add_WYD(., "date")

ggplot() +
  geom_line(data=master_daily,
            aes(x=date, y=lev.avg, color=site, group=WY)) +
  facet_grid(site~., scales="free")


# WRITE OUT ---------------------------------------------------------------

# updated data:
save(master_updated, file = "data_output/2011_2018_solinst_hourly_master.rda")

save(master_daily, file = "data_output/2011_2018_solinst_daily_master.rda")
