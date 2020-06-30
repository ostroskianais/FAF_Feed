library(tidyverse)
library(ggplot2)
library(dplyr)
library(sf)
library(maps)
library(rnaturalearthdata)
library(rnaturalearth)
library(viridis)

library(extrafont)
font_import()
loadfonts(device = "win")

# Load data

domestic <- read.csv("Data/FAFfeed_2018.csv")

# Glimpse
glimpse(domestic)

# Values
values <- domestic %>% select(DMS_ORIG, Total.Current.M..in.2018) %>% 
  rename(Total2018 = Total.Current.M..in.2018)

values %>%
  ggplot() +
  geom_histogram(aes(x=Total2018)) +
  #xlim(quantile(values$Total2018, 0.5),quantile(values$Total2018, 0.8)) +
  theme_bw()

domestic %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x=DMS_MODE, y=Total.Current.M..in.2018)) +
  theme_classic()

quantile(values$Total2018, c(0.1,0.25,0.5,0.75,0.8,0.9,0.99))

# Load FAF geometry
# And also merge to add a column wiht FAF codes
# because the shapefile has CFS names
FAFcodes <- read.csv("Data/CFS_FAF_codes.csv")

FAFzones <- st_read(dsn = "Shapefiles/FAF4_zones.shp") %>% 
  merge(FAFcodes, by="CFS12_NAME", all=T)

# Extract the centroids
# We need point coordinates for the curve
FAFcentroids1 <- st_centroid(FAFzones)

FAFcentroids <- st_coordinates(FAFcentroids1) %>% tbl_df() %>% 
  cbind(FAFcentroids1$FAFcode) %>% 
  rename(long = X, lat = Y, FAFcode = "FAFcentroids1$FAFcode") %>% 
  select(FAFcode, long, lat)

link_dom <- domestic %>% 
  select(DMS_ORIG, DMS_MODE, DMS_DEST, Total.Current.M..in.2018) %>% 
  group_by(DMS_ORIG, DMS_DEST) %>% 
  summarise(Total.Current.M..in.2018 = sum(Total.Current.M..in.2018)) %>% 
  merge(FAFcentroids, by.x = "DMS_ORIG", by.y="FAFcode") %>% 
  rename(x = long, y = lat) %>% 
  merge(FAFcentroids, by.x="DMS_DEST", by.y = "FAFcode", all=T) %>% 
  rename(xend = long, yend = lat) %>% 
  filter(!x == xend,
         !y == yend) %>% 
  filter(!DMS_DEST == "Alaska",
         !DMS_DEST == "Honolulu HI",
         !DMS_DEST == "Rest of HI") %>% 
  filter(Total.Current.M..in.2018 > quantile(values$Total2018, 0.5))

######### PLOT
# Prepare regions
sum_orig <- domestic %>% 
  select(DMS_DEST, Total.Current.M..in.2018) %>% 
  group_by(DMS_DEST) %>% 
  summarise(total_M = sum(Total.Current.M..in.2018)) %>% 
  drop_na() %>% 
  merge(FAFzones, by.x="DMS_DEST", by.y="FAFcode") %>% 
  st_as_sf()

# State geometry
state <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

#Colors
pal <- PNWColors::pnw_palette("Cascades", 8, type="continuous")
textcolor <- pal[4]

# Plot
ggplot() +
  geom_sf(data=sum_orig, color=NA, aes(fill=total_M)) +
  geom_sf(data=state, color="black", fill=NA) +
  scale_fill_gradientn(colours = rev(pal),
                       trans = "log10") +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,
                 size = Total.Current.M..in.2018, 
                 alpha = Total.Current.M..in.2018),
             data = link_dom, curvature = 0.33, 
             color = "white", show.legend = FALSE) +
  scale_size(range = c(.1, 3)) +
  scale_alpha(range = c(.1,1)) +
  annotate(geom = 'text', x = -119, y = 52, 
           label = "MOVEMENT OF ANIMAL FEED IN THE UNITED STATES", 
           color = textcolor, family = "Franklin Gothic Heavy", 
           size = 12, hjust = 0, vjust = 0) +
  theme_void() +
  coord_sf(xlim = c(-130, -60), ylim = c(23, 55), expand = FALSE) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="gray15"),
        #legend.position = c(1,1),
        legend.text = element_text(color = "white", 
                                   family = "sans",
                                   size = ),
        legend.title = element_text(color = "white", 
                                    family = "sans",
                                    size = 8),
        legend.key.size = unit(x = 0.3, units = "cm"),
        legend.background = element_blank()) 

#ggsave("Results/FAFtextiles_domestic.png")