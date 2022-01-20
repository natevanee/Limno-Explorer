library(tidyverse)
library(readxl)
# Load the data I'll need
# META DATA (need for gps coordinates)
nearshore_meta <- read_excel("NearshoreSiteData.xlsx", 
                             skip = 4,
                             sheet = "MetaData")

nearshore_meta$Date <- as.character(nearshore_meta$Date)

# read in all the ctd data
ctd_2020 <- read_csv(file = "2020_Compiled_CTD_Data.csv")

ctd_2020$Date <- as.character(ctd_2020$Date)  

# Make better labels for the parameters (include units where needed)
my_labels <- c(
  "beamTransmission" = "Beam Transmission (%)",
  "conductivity2" = "Sp Conductivity (uS/cm)",
  "fluorescence" = "Fluorescence (mg/m3)",
  "oxygen2" = "Oxygen (% sat)",
  "par" = "LN(PAR)",
  #"pH" = "pH",
  "temperature" = str_c("Temperature C")
)

# A function for making the ctd plots

ctd_plot <- function(data, DateTime, SiteNum) {
  data %>%
    select(-conductivity, -oxygen, -beamAttenuation, -scan, -flag, -pH, -pressure, -salinity) %>%
    pivot_longer(names_to = "parameter", values_to = "measurement", 2:last_col()) %>%
    mutate(measurement = if_else(parameter=="par", log(measurement+.001), measurement)) %>%
    ggplot(aes(x=measurement, y=depth, col=parameter)) +
    geom_path(size=1.2, show.legend = F) +
    # geom_hline(yintercept = c(1.5, max_depth-2), linetype = "dashed") +
    scale_y_reverse(breaks = seq(2,12,2)) +
    scale_x_continuous(breaks = scales::pretty_breaks(4)) +
    facet_wrap(~parameter, scales = "free_x", nrow = 2, labeller = as_labeller(my_labels)) +
    labs(title = str_c(DateTime, "| CTD Cast", SiteNum, sep = " "),
         y = "Depth (M)") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#d5e4eb"),
          panel.background = element_rect(fill = "#d5e4eb"),
          panel.grid.minor = element_blank(),
          panel.spacing.x = unit(1, "lines"),
          axis.title.x = element_blank())
}


# Filter out some of the messy surface values
ctd_2020 <- ctd_2020 %>%
  filter(depth >= 1.75)

# create and store all the plots in a nested dataframe
ctd_plotting <- nearshore_meta %>%
  select(Date, SiteNum, Lat, Long, `Start Time`) %>%
  mutate(time = str_sub(as.character(`Start Time`), -8L, -4L)) %>%
  select(-`Start Time`) %>%
  right_join(ctd_2020) %>%  
  mutate(DateTime = str_c(Date, time, sep = " ")) %>%
  select(-time) %>%
  nest(-Date, -DateTime, -SiteNum, -Lat, -Long) %>%
  mutate(plot = pmap(list(data, DateTime, SiteNum), ctd_plot))
