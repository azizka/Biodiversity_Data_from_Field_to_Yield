
# load libraries
library(rgbif)
library(tidyverse)
library(sp)
library(countrycode)
library(CoordinateCleaner)


#Search occurrence records
dat <- occ_search(scientificName = "Wittmackia patentissima", 
                  return =  "data")


nrow(dat) # Check the number of records
head(dat) # Check the data
plot(dat$decimalLatitude ~ dat$decimalLongitude) # Look at the georeferenced records

#Use the name_suggest function to get the gbif taxon key
tax_key <- name_suggest(q = "Magnoliopsida", rank = "Class")

#Sometimes groups have multiple taxon keys, in this case three, so we will check how many records are available for them
lapply(tax_key$key, "occ_count")

occ_count(tax_key, country = "BR")

# Define study area and download data
study_a <- "POLYGON((-35 -5.5, -37 -5.5, -37 -6.5, -35 -6.5, -35 -5.5))"


#dat_ne  <- occ_search(taxonKey = tax_key, return = "data", hasCoordinate = T,
#                      geometry = study_a, limit = 50000) 


##2. Data cleaning

# load gbif data
dat <- read_csv("inst/gbif_occurrences.csv", guess_max = 25000)%>%
  mutate(dataset = "GBIF")

# add the field data
field <- read_csv("inst/field.csv")

field <- field%>%
  dplyr::select(species, 
                decimalLongitude = long, 
                decimalLatitude = lat, 
                countryCode = country)%>%
  mutate(dataset = "field")%>%
  mutate(year = 2018)%>%
  mutate(countryCode = countrycode(countryCode, origin = "country.name", destination = "iso3c"))

#combine datasets
dat <- bind_rows(dat, field)


#Visualize on a map

world.inp  <- map_data("world")

ggplot()+
  geom_map(data=world.inp, map=world.inp, aes(x=long, y=lat, map_id=region), fill = "grey80")+
  xlim(min(dat$decimalLongitude, na.rm = T), max(dat$decimalLongitude, na.rm = T))+
  ylim(min(dat$decimalLatitude, na.rm = T), max(dat$decimalLatitude, na.rm = T))+
  geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude, color = dataset),
             size = 1)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank())

# Visualize on another map



# Cleaning metadata
# remove records without coordinates
dat_cl <- dat%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude))

#remove records with low coordinate precision
hist(dat_cl$coordinateUncertaintyInMeters/1000, breaks = 30)

dat_cl <- dat_cl %>%
  filter(coordinateUncertaintyInMeters/1000 <= 100 | is.na(coordinateUncertaintyInMeters))

#remove unsuitable data sources, especially fossils
table(dat$basisOfRecord)

dat_cl <- filter(dat_cl, basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "OBSERVATION" |
                   basisOfRecord == "PRESERVED_SPECIMEN" | is.na(basisOfRecord))

dat_cl <- filter(dat_cl, basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "OBSERVATION" |
                   basisOfRecord == "PRESERVED_SPECIMEN")




#Individual count
table(dat_cl$individualCount)

dat_cl <- dat_cl%>%
  filter(individualCount > 0 | is.na(individualCount))%>%
  filter(individualCount < 99 | is.na(individualCount)) # high counts are not a problem

#Age of records
table(dat_cl$year)

dat_cl <- dat_cl%>%
  filter(year > 1945) # remove records from before second world war


table(dat_cl$family) #that looks good
dat_cl <- dat_cl%>%
  filter(family == 'Malvaceae')

table(dat_cl$taxonRank) # We will only include records identified to species level
dat_cl <- dat_cl%>%
  filter(taxonRank == "SPECIES")


#Automated cleaning
#convert country code from ISO2c to ISO3c
dat_cl$countryCode <-  countrycode(dat_cl$countryCode, origin =  'iso2c', destination = 'iso3c')

#flag problems
dat_cl <- data.frame(dat_cl)
flags <- clean_coordinates(x = dat_cl, lon = "decimalLongitude", lat = "decimalLatitude",
                           countries = "countryCode", 
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros", "countries", "seas"),
                           seas_ref = buffland) # most test are on by default



summary(flags)
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

dat_cl <- dat_cl[flags$.summary, ]



#For the total dataset
dat_cl$datasettotal <- "TOTAL"

##Run dataset level test
clean_dataset(dat_cl, 
              ds = "datasettotal",
              lon = "decimalLongitude",
              lat = "decimalLatitude")



wm <- borders("world", colour="gray50", fill="gray50")
ggplot() +
  coord_fixed()+
  wm +
  geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 1)+
  geom_point(data = dat_cl, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkgreen", size = 1)+
  coord_fixed()+
  xlim(min(dat$decimalLongitude), max(dat$decimalLongitude))+
  ylim(min(dat$decimalLatitude), max(dat$decimalLatitude))


write_csv(dat_cl, "inst/occurrence_records_clean.csv")

