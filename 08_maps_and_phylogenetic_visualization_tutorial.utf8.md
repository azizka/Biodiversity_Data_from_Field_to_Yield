---
title: "Maps and Phylogenetic visualization"
output: 
  html_document:
    theme: readable
---



## A bar chart of biome classification
A bar chart to plot the biome classification from exercise 7).


```r
# dat <- read_csv()

```


## A bar chart of species threat

## Species on Phylogeny

## Maps with points

## Maps with polygons

## Maps with raster and projection

```r
# # load data dat <- raster('inst/equal_area_range_richness') # Prepare for
# plotting plo <- data.frame(rasterToPoints(dat)) # Visualize ggplot()+
# geom_map(data = world.inp, map = world.inp, aes(x = long, y = lat, map_id
# = region), fill = 'grey80')+ xlim(min(dat$decimalLongitude, na.rm = T),
# max(dat$decimalLongitude, na.rm = T))+ ylim(min(dat$decimalLatitude, na.rm
# = T), max(dat$decimalLatitude, na.rm = T))+ geom_raster(data = plo, aes(x
# = x, y = y, fill = layer))+ #scale_fill_viridis(name = 'Species',
# direction = -1)+ coord_fixed()+ theme_bw()+ ggtitle('Number of
# occurrences')+ theme(axis.title = element_blank())

```







