# Read Loggers

library(tidyverse)
library(lubridate)

# Read Existing Data in ---------------------------------------------------

load("data/2011-2015_solinst_mainstem_hourly_compensated.rda") # hrly2, includes TUO and Clavey

hrly2 <- hrly2 %>% rename(DOWY=wyd, DOY=yday, datetime=Datetime, level_comp=Level, temp_C=Temperature) %>% 
  select(-mon, -hours, -year, -grp)
names(hrly2)

load("data/2011-2016_solinst_mainstem_hourly_compensated.rda") # hr.df2, doesn't include TUO/Clavey
names(hr.df2)

hrly<-hr.df2

# TEST PLOTS --------------------------------------------------------------
 
# compensated/adj STAGE: AMER/YUB
ggplot() +
  geom_line(data=hrly,
            aes(x=datetime, y=level_comp, color=site, group=WY)) +
  facet_grid(site~., scales="free")

# STAGE: CLA/TUO
ggplot() +
  geom_line(data=hrly2,
            aes(x=datetime, y=level_comp, color=site, group=WY)) +
  facet_grid(site~., scales="free") +
  geom_line(data=hrly,
            aes(x=datetime, y=level_comp, group=WY), color="black", alpha=0.6) +
  theme_bw()


# Add TUO/CLA TO RECENT SET -----------------------------------------------

# merge the TUO/CLA data with the final hourly dataset
cla_tuo <- hrly2 %>% filter(site=="TUO" | site=="CLA")
hrly_out <- bind_rows(hrly, cla_tuo) # add cla/tuo data

# view
ggplot() +
  geom_line(data=hrly_out,
            aes(x=datetime, y=level_comp, color=site, group=WY)) +
  facet_grid(site~., scales="free")


# Save Combined data ------------------------------------------------------

save(hrly_out, file = "data_output/2011_2016_solinst_hourly_all.rda")

