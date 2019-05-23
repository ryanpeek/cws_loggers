# Make thermohydros

library(tidyverse)
library(viridis)
library(scales)

# set breaks:
breaks<-seq(0,30,3) 


# Load Data ---------------------------------------------------------------

load("data_output/2011_2018_solinst_hourly_master.rda") # master_updated

# Filter to Region of Interest --------------------------------------------

sites <- c("NFA")
years <- c(2014:2017)

data_to_plot <- filter(master_updated, site %in% sites, WY %in% years)

table(data_to_plot$site)

# Plot --------------------------------------------------------------------

ggplot() + 
  geom_linerange(data=data_to_plot, 
                 aes(x=datetime, ymin=0, ymax=level_comp, colour=temp_C),
                 size=0.65, alpha=.6) +
  geom_line(data=data_to_plot, 
            aes(x=datetime, y=level_comp),
            size=0.3, alpha=1) +
  ylab("Stage (m)") + xlab("") +
  scale_x_datetime(breaks=date_breaks("1 months"),
                   labels = date_format("%b")) +
  scale_colour_gradientn("Water \nTemp (C)",
                         colours=viridis(30, option="A"),
                         breaks=breaks, limits=c(0,30)) + 
  theme_light() + labs(title="Hourly Thermohydrograph")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(.~WY, scales = "free_x")

ggsave(filename = "figs/NFA_thermohydrograph_2014_2017.png", width = 10, height = 7, units = "in", dpi = 300)
