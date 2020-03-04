alts = c(615,775,850,975, 1200,1500)
pers = c(25, 90, 150,1000,1600,1800)
lw1 <- loess(pers ~ alts)

all_conjs_new = readRDS("RDSfiles/all_conjs")

# get operational satellites
opSats = derelictDat %>% filter(avgAlt < 2000 & operational)

combinedMass_v = vector()
persistence_v = vector()
start_time = Sys.time()
for (i in 1:nrow(all_conjs_new)) { # each loop takes 0.9162 secs... .063317 with new op sat calc
  conj = all_conjs_new[i, ]
  noradId1 = gsub("--.*", "", conj$PrimarySatellite)
  noradId2 = gsub("--.*", "", conj$SecondarySatellite)
  obj1 = filter(mcma_objs, noradId == noradId1)
  obj2 = filter(mcma_objs, noradId == noradId2)
  
  combinedMass = as.numeric(obj1$mass) + as.numeric(obj2$mass)
  persistence = if_else(conj$Altitude <= 615, 25,
                        if_else(conj$Altitude > 615 & conj$Altitude <= 1500, 
                                predict(lw1, conj$Altitude), 1000)) 
  
  combinedMass_v = append(combinedMass_v, toString(combinedMass))
  persistence_v = append(persistence_v, persistence)
}
all_conjs_new$combinedMass = combinedMass_v
all_conjs_new$persistence = persistence_v
end_time = Sys.time()

all_conjs_new = all_conjs_new %>%
  mutate(combinedMass = if_else(grepl(",", combinedMass, fixed = T), # if it contains a comma
                                as.numeric(gsub(",.*", "", combinedMass)), # make substring up to the comma
                                as.numeric(combinedMass)),
         combinedMass = if_else(is.na(combinedMass), 0, combinedMass)) # otherwise don't change 

all_conjs_new = all_conjs_new %>%
  mutate(risk = (combinedMass + persistence + numOpSats) / Range,
         # initialize scaled variables
         combinedMass_s = 1, persistence_s = 1,
         numOpSats_s = 1, Range_s = 1, risk_s = 1)

## get SD op sats per conj
alt_bins = readRDS("RDSfiles/alt_bins")
roundDown <- function(x) 10*floor(x/10)
library(zoo)
alt_bins = derelictDat %>% 
  filter(avgAlt < 2000 & operational) %>%
  mutate(altitude = roundDown((as.numeric(apogee) + as.numeric(perigee))/2)) %>% 
  group_by(altitude) %>% 
  summarise(numOpSats = n()) %>% 
  right_join(alt_bins, by="altitude") %>%
  mutate(numOpSats = replace_na(numOpSats, 0)) %>% 
  mutate(spatDensOpSats_1 = numOpSats / volume * (10^10)) %>%
  mutate(spatDensOpSats = rollmean(spatDensOpSats_1, k=5, na.pad=T)) %>% na.omit()

all_conjs_new = all_conjs_new %>% 
  mutate(altitude = roundDown(Altitude)) %>% 
  left_join(select(alt_bins, c(altitude, spatDensOpSats)), by="altitude") 

all_conjs = all_conjs_new
saveRDS(all_conjs, "RDSfiles/all_conjs") # save to RDS file

