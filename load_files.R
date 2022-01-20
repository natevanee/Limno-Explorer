# Load all the datasets needed to run the app


# High Frequency Sonde Data
shb_mke <- read_csv("high_resolution_sonde_mke_shb.csv")
hr_sonde <- shb_mke %>% 
 # pivot_longer(names_to = "parameter", values_to = "measurement", -c(date_time, location)) %>%
  mutate(date = date(date_time))

# long format daily time series summary
my_daily_ts <- read_rds("daily_timeseries_summary")

daily_ts_wide <- my_daily_ts %>%
  select(-sd) %>% 
  pivot_wider(names_from = "name", values_from="mean") %>%
    mutate("bioavailable-pp" = na_oh_p,
           "other-pp" = pp-na_oh_p) %>%
  select(-ph_upper, -tdp)

names(hr_sonde) <- c("date_time", "location", "parameter", "measurement", "date")

# Daily Isco samples and summary Sonde/discharge data
daily <- readxl::read_excel("mke_shb_daily_model_data.xlsx", skip = 4)

none_targets <- c("na_oh_p", "pp", "tdp", "do_mg_l", "lunar_tide_period", "lunar_tide_pvalue", "seiche_period", "seiche_pvalue")

daily <- daily %>%
  mutate(date = date(date),
         "other-pp" = pp-na_oh_p,
         "bioavailable-pp"=na_oh_p,
         "total_p" = tdp+pp) %>%
  select(date, location, srp, dop, `other-pp`, `bioavailable-pp`, total_p, tss) #discharge_cfs:last_col(), -all_of(none_targets)

# Long format for streamgraph
long_isco <- daily %>%
  select(date, location, srp, dop, `other-pp`, `bioavailable-pp`) %>%
  pivot_longer(names_to = "fraction", values_to = "value", -c(date, location)) %>%
  mutate(all = 1,
         bioavailable = if_else(fraction %in% c("bioavailable-pp", "srp"), 1,0),
         unavailable = if_else(bioavailable == 1, 0 , 1),
         dissolved = if_else(fraction %in% c("srp", "dop"), 1, 0),
         particulate = if_else(dissolved == 1, 0, 1),
         color = rep(c("#5E3C99", "#B2ABD2", "#FDB863", "#E66101"), length.out = 984)) %>%
  pivot_longer(names_to = "pool", values_to = "logical", all:particulate) %>%
  filter(logical == 1) %>%
  group_by(date, location, pool) %>%
  mutate(total = sum(value),
         proportion = value/total) %>%
  mutate(fraction = factor(fraction, levels = c("srp", "dop", "other-pp", "bioavailable-pp")))

# Wind data from MKE
mke_wind <- read_csv("two_min_wind_mke.csv")

# Nearshore water chemistry data
nearshore <- readxl::read_excel("NearshoreSiteData.xlsx", 
                        skip = 4,
                        sheet = "Water Chem")

nearshore$Date <- as.character(nearshore$Date)

# Leaflet map set with a view near Milwaukee's near shore zone for Survey data
m <- leaflet() %>% addTiles() %>% setView(lat = 43.0, lng = -87.91, zoom = 12)

# constant for converting between decimal degrees and meters
dd_to_m <- 111139

# Set up the information for the "Rain Explorer Tab"
default_token <- "3131927af6ca468d9018fb76c624abda"
get_precip <- "https://api.synopticdata.com/v2/stations/precip"
token <- paste0("?token=",default_token)

# LeafletvView for the whole area (both watersheds)
r <- leaflet() %>% addTiles()  %>% setView(lat = 43.1, lng = -87.8, zoom = 8) 

# watershed shapefiles
shb_mke_watersheds <- read_rds("shb_mke_watersheds.rds")

mke_lu <- readRDS("mke_simplified_lu_raster.RDS")
shb_lu <- readRDS("shb_simplified_lu_raster.RDS")

mke_sf <- readRDS("mke_simplified_sf.RDS")
shb_sf <- readRDS("shb_simplified_sf.RDS")

# set up land use color scheme and labels

my_lu_colors <- c(
  "#3C3F87", #1 open water
  "#AB0000", #2 developed
  "#B3AC9F", #3 barren
  "#1C5F2C", #4 forest
  "#DAF7A6", #5 shrub
  "#DFDFC2", #7 herbaceous
  "#AB6C28", #8 cultivated
  "#B8D9EB"  #9 wetlands
)

my_lu_pal <- colorNumeric(palette = my_lu_colors,
                          domain = c(1:5, 7:9), na.color = "#00000000")

my_lu_labels <- c(
  "Open Water",
  "Developed",
  "Barren",
  "Forest",
  "Shrub",
  "Herbaceous",
  "Cultivated",
  "Wetlands"
)

