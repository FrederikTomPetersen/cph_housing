library("readr")
library("ggmap")
library("stringr")

setwd("C:/Users/qsd161/Documents/GitHub/cph_housing")

df <- read.csv("affaldskurv.csv", encoding = 'latin1')


map_cph <- get_map(location = "copenhagen", zoom = 12, maptype = 'satellite')


str_split_fixed(df$wkb_geometry, )


df[10,24]