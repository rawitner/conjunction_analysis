library(tidyverse)
#all_conjs = readRDS("all_conjs")
derelicts = readRDS("derelicts")
#file_list = readRDS("file_list")
mcma_objs = readRDS("mcma_objs")
today = "22NOV2019"

# SKIP
mcma_objs = data.frame()
for (i in 1:7) {
  temp_data = readxl::read_xlsx("/Users/rachelwitner/Documents/Centauri/fall part time/MCMA_V2point0.xlsx", sheet = i)
  mcma_objs = rbind(mcma_objs, temp_data)
}
chigh = readxl::read_xlsx("/Users/rachelwitner/Documents/Centauri/fall part time/MCMA_V2point0.xlsx", sheet = 8) %>%
  mutate(cluster = "CHIGH", cluster_new = "CHIGH", launch = NA) %>%
  dplyr::select(c(noradId = `SAT#`,
                  name = Name,
                  apogee = Apogee,
                  perigee = Perigee,
                  cluster,
                  cluster_new,
                  launch,
                  inclination = Incline,
                  mass = `Mass (kg)`)) %>%
  filter(!is.na(noradId)) %>% 
  dplyr::select(-launch) %>% 
  left_join(dplyr::select(derelicts, c(noradId, launch)), by="noradId")

mcma_objs = rbind(mcma_objs, chigh)

# add country and type to mcma_objs
mcma_objs = left_join(mcma_objs, dplyr::select(derelicts, c(noradId, country, type)), by="noradId")

# fix missing values for two CHIGH objects
mcma_objs$type[mcma_objs$noradId == 31116] <- "ROCKET BODY"
mcma_objs$type[mcma_objs$noradId == 38253] <- "ROCKET BODY"
mcma_objs$country[mcma_objs$noradId == 31116] <- "PRC"
mcma_objs$country[mcma_objs$noradId == 38253] <- "PRC"
mcma_objs$launch[mcma_objs$noradId == 31116] <- as.POSIXct("2007-04-13", tz="UTC")
mcma_objs$launch[mcma_objs$noradId == 38253] <- as.POSIXct("2012-04-29", tz="UTC")


# view new objects
mcma_objs %>% filter(cluster == "elsewhere" & cluster_new != "cc615")
########################################

path = "/Users/rachelwitner/Documents/Centauri/fall part time/conj_data/new/"
file_list = list.files(path)
#file_list_new = list.files(path)
#file_list_new = file_list_new[!(file_list_new %in% file_list)] # only the new conjunctions

all_conjs = data.frame()
for (i in 1:length(file_list)) {
  temp_data = read_csv(paste0(path, file_list[i]))
  all_conjs = rbind(all_conjs, temp_data) #for each iteration, bind the new data to the building dataset
}

# column for cluster combination
for (r in 1:nrow(all_conjs)) {
  clusts = sort(c(all_conjs$PrimaryCluster[r], all_conjs$SecondaryCluster[r]), decreasing = T)
  all_conjs$clusters[r] = paste0(clusts[1], "-", clusts[2])
}

# column for unique cluster, if a number and LEO or HIGH, only label as the number
all_conjs =  all_conjs %>%
  mutate(firstClust = gsub("-.*$", "", clusters),
         secondClust = sub(".*?-", "", clusters),
         clusterLab = if_else(firstClust=="LEO" & secondClust=="LEO", "LEO",
                              if_else(firstClust=="LEO" & secondClust!="LEO", "LEO-other",
                                      if_else(firstClust=="HIGH" & secondClust=="HIGH", "HIGH",
                                              if_else(firstClust=="HIGH" & secondClust!="HIGH", 
                                                      "HIGH-other", firstClust)))),
         clusterLab = factor(clusterLab,
           levels = c("615", "775", "850", "975", "1200", "1500", "LEO","LEO-other","HIGH-other"),
           ordered = T))

# read in country codes
country_codes = read_csv("./country_codes.csv", 
                         col_names = c("country", "Country"), col_types = "cc", skip = 1) %>%
  mutate(Country = str_to_title(Country),
         Country = if_else(str_length(Country) > 20, country, Country))

# plot percent of encounters by country
p = all_conjs %>%
  mutate(noradId = as.numeric(gsub("--.*$", "", PrimarySatellite ))) %>%
  left_join(dplyr::select(mcma_objs, c(noradId, country)), by="noradId") %>%
  mutate(country = if_else(country == "CHBZ", "PRC", country)) %>%
  group_by(clusterLab, country) %>% 
  summarise(numEncounters = n()) %>%
  left_join(country_codes, by="country") %>% 
  group_by(clusterLab) %>%
  mutate(encountersPerClust = sum(numEncounters), 
         p = numEncounters / encountersPerClust * 100) #%>%
  group_by(clusterLab) %>%
  mutate(country_new = if_else(p < 2, "Other", Country)) %>% 
  mutate(p = if_else(country_new=="Other", mean(p[country_new=="Other"]), p))

colourCount = length(unique(p$Country))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot() + 
  geom_bar(data = p, aes(x=clusterLab, y=p/100, group=Country, fill=Country), stat="identity")+
  geom_text(data = unique(dplyr::select(p, c(clusterLab, encountersPerClust))), position = position_stack(vjust=1.05), 
            aes(x=clusterLab, y=1, label=encountersPerClust))+
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = getPalette(colourCount))+
  labs(x="Cluster",y="", title = "Percent of Encounters by Country", fill="Country",
       subtitle="Number of encounters shown above each bar", caption=paste0("Encounters from 20OCT2019-", today))

ggsave(paste0("/Users/rachelwitner/Documents/Centauri/fall part time/plots/percentEncountersByCountry_", today, ".jpeg"),device="jpeg")

#####################
# PC FOR CONJUNCTIONS
#obj1 = data.frame(noradId = 123, mass=1434, length=11, diam =3.9)
#obj2 = data.frame(noradId = 456, mass=700, length=4, diam = 4)
#averad = (obj1$length + obj1$diam + obj1$diam + obj2$length)/4
#conj = all_conjs %>% filter(PrimarySatellite == "12091--COSMOS 1226" & Range < 0.4) %>%
#  mutate(Range = Range*1000, RangeX = RangeX*1000, RangeY = RangeY*1000, RangeZ = RangeZ*1000)
unc_r = 60
unc_i = 150
unc_c = 60
unc_total = ((unc_r)^2+(unc_i)^2+(unc_c)^2)^0.5

pc_wang = (0.000000092)*(unc_r/unc_i)*((obj1$length+obj2$diam)^2)/((conj$Range)^2)
pc_alfano = if_else(averad/conj$Range < .8, 
                    0.48394*averad/conj$Range,
                    0.21329*(exp(1.0511*averad/conj$Range)-0.09025))
pc_knowles = if_else(conj$Range>unc_total, 0, abs((conj$RangeX/unc_r)*(conj$RangeY/unc_i)*(conj$RangeZ/unc_c)))

# add arbitrary length and diams to each object
mcma_objs = mcma_objs %>% mutate(length = if_else(type=="ROCKET BODY", 11, 4),
                                 diam = if_else(type=="ROCKET BODY", 3.9, 3))

# create empty column
pc_alfano_v = vector()
pc_wang_v = vector()
pc_knowles_v = vector()

# for every conjunction in a cluster, calculate PC, then add within cluster
for (i in 1:nrow(all_conjs)){
  conj = all_conjs[i,] %>% # get row with ranges in m
    mutate(Range = Range*1000, RangeX = RangeX*1000, RangeY = RangeY*1000, RangeZ = RangeZ*1000)
  
  noradId1 = gsub("--.*", "", conj$PrimarySatellite)
  noradId2 = gsub("--.*", "", conj$SecondarySatellite)
  obj1 = filter(mcma_objs, noradId == noradId1)
  obj2 = filter(mcma_objs, noradId == noradId2)
  
  averad = (obj1$length + obj1$diam + obj1$diam + obj2$length)/4
  
  pc_alfano = if_else(averad/conj$Range < .8, 0.48394*averad/conj$Range, 0.21329*(exp(1.0511*averad/conj$Range)-0.09025))
  pc_wang = (0.000000092)*(unc_r/unc_i)*((obj1$length+obj2$diam)^2)/((conj$Range)^2)
  pc_knowles = if_else(conj$Range>unc_total, 0, abs((conj$RangeX/unc_r)*(conj$RangeY/unc_i)*(conj$RangeZ/unc_c)))
  
  pc_alfano_v = append(pc_alfano_v, pc_alfano)
  pc_wang_v = append(pc_wang_v, pc_wang)
  pc_knowles_v = append(pc_knowles_v, pc_knowles)
}

all_conjs$pc_alfano = pc_alfano_v
all_conjs$pc_wang = pc_wang_v
all_conjs$pc_knowles = pc_knowles_v



all_conjs %>% group_by(cluster) %>%
  summarise(CR_alfano = sum(pc_alfano),
            CR_wang = sum(pc_wang),
            CR_knowles = sum(pc_knowles))

######################
# WORST OFFENDERS
# fit persistence predictor model
alts = c(775,850,975,1500)
pers = c(300,500,1800,20000)
persistence = cbind(alts, pers) %>% as_tibble()
model = lm(log(pers) ~ alts, persistence)
exp(1.535505 + 0.005653*350)

derelictDat = readRDS("derelictDatNew")
opSats = derelictDat %>% filter(avgAlt < 2000 & operational) # df of operational sats

# calculate combined mass, persistence, and op sats for each conjunction
combinedMass_v = vector()
persistence_v = vector()
numOpSats_v = vector()
for (i in 1:nrow(all_conjs)){
  conj = all_conjs[i,]
  noradId1 = gsub("--.*", "", conj$PrimarySatellite)
  noradId2 = gsub("--.*", "", conj$SecondarySatellite)
  obj1 = filter(mcma_objs, noradId == noradId1)
  obj2 = filter(mcma_objs, noradId == noradId2)
  
  combinedMass = as.numeric(obj1$mass) + as.numeric(obj2$mass)
  persistence = exp(1.535505 + 0.005653*conj$Altitude)
  numOpSats = 0
  for (j in 1:nrow(opSats)){ # count how many op sats overlap in altitude
    opSat = opSats[j, ]
    if (conj$Altitude > opSat$perigee & conj$Altitude < opSat$apogee){
      numOpSats = numOpSats + 1
    }
  }
  
  combinedMass_v = append(combinedMass_v, combinedMass)
  persistence_v = append(persistence_v, persistence)
  numOpSats_v = append(numOpSats_v, numOpSats)
}
all_conjs$combinedMass = combinedMass_v
all_conjs$persistence = persistence_v
all_conjs$numOpSats = numOpSats_v

# now total for each object
for (i in 1:nrow(all_conjs)){
  conj = all_conjs[i,]
  noradId1 = gsub("--.*", "", conj$PrimarySatellite)
  noradId2 = gsub("--.*", "", conj$SecondarySatellite)
  obj1 = filter(mcma_objs, noradId == noradId1)
  obj2 = filter(mcma_objs, noradId == noradId2)
}

# for probability, need cumulative PC and frequency of interaction for each obj
all_conjs %>% mutate(sat1 = gsub("--.*", "", PrimarySatellite),
                     sat2 = gsub("--.*", "", SecondarySatellite))


#######################
# number of encounters per cluster pairing
all_conjs %>% group_by(clusterLab) %>%
  summarise(`number of encounters` = n())

# axis ticks for log scale
ticks <- 1:10
ooms <- 10^(0:3) # define the orders of magnitudes
breaks <- as.vector(ticks %o% ooms)

show.labels <- c(T, F, F, F, T, F, T, F, F)
labels <- as.character(breaks * show.labels)
labels <- gsub("^0$", "", labels)

# miss distance vs cumulative count plot
all_conjs %>%
  group_by(clusterLab) %>%
  arrange(Range) %>%
  mutate(rowid = 1, cumnum = cumsum(rowid)) %>% 
  ggplot(aes(x=Range, y = cumnum, color=clusterLab)) + 
  geom_line() +
  theme_light() +
  scale_x_log10(labels = labels, breaks = breaks) +
  scale_y_log10(labels = labels, breaks = breaks) +
  labs(x="Miss Distance (km)", y="Cumulative Number of Encounters", color="Cluster",
       title="Cumulative Number of Encounters by Cluster", subtitle = paste0("Encounters from 20OCT2019-", today))+
  scale_color_brewer(palette="Set1")

ggsave(paste0("/Users/rachelwitner/Documents/Centauri/fall part time/plots/cumulativeEncounters_", today, ".jpeg"),device="jpeg")

######################
all_conjs %>% group_by(clusterLab) %>% 
  ggplot(aes(x=Range, group=clusterLab, fill=clusterLab)) + 
  geom_density(adjust=.3, position = "stack")+
  facet_wrap(~clusterLab, scales="free_y")+
  theme_minimal()+
  labs(x="Miss Distance (km)", y="Density", fill="Cluster", 
       title="Distribution of Miss Distance per Cluster")

ggsave(paste0("/Users/rachelwitner/Documents/Centauri/fall part time/plots/missDistDistrib_", today, ".jpeg"),device="jpeg")

########################
firstSet = all_conjs %>% mutate(noradId = as.numeric(gsub("--.*", "", PrimarySatellite))) %>%
  dplyr::select(-c(PrimarySatellite, SecondarySatellite))
secondSet = all_conjs %>% mutate(noradId = as.numeric(gsub("--.*", "", SecondarySatellite))) %>%
  dplyr::select(-c(PrimarySatellite, SecondarySatellite))

all_conjs_expanded = rbind(firstSet, secondSet)

all_conjs_expanded %>%
  left_join(dplyr::select(mcma_objs, c(noradId, apogee,perigee)), by="noradId") %>% 
  mutate(avgAlt = (as.numeric(apogee)+as.numeric(perigee))/2) %>% 
  ggplot(aes(x=avgAlt, y=Altitude, color=clusterLab))+
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  theme_minimal()+
  labs(x="Altitude of Derelict", y="Altitude of Encounter", color="Cluster",
       title="Altitude of Derelict vs Encounter", 
       subtitle="Most encounters are happening right around the average altitude of the derelict")

ggsave(paste0("/Users/rachelwitner/Documents/Centauri/fall part time/plots/altitudeDiff_", today, ".jpeg"),device="jpeg")

########################
mcma_objs %>% arrange(cluster_new) %>% filter(cluster_new != "CHIGH") %>%
  rowid_to_column() %>%
  ggplot() +
  geom_segment(aes(x=as.numeric(perigee), xend=as.numeric(apogee), y=rowid, yend=rowid, color=cluster_new))+
  theme(axis.text.x = element_text(angle=90))+
  scale_x_continuous(breaks = trans_breaks(identity, identity, n = 40))

########################
# type of object in each cluster
mcma_objs %>% group_by(cluster_new) %>%
  summarise(numPL = length(noradId[type=="PAYLOAD"]),
            numRB = length(noradId[type=="ROCKET BODY"]),
            n = n(),
            n2 = numPL + numRB)

  
#########################
# top 50 closest approaches ever
library(tidyquant)
all_conjs %>% #mutate(conj_month = strftime(conj_day, format="%Y-%m")) %>%
  #group_by(conj_month) %>%
  #summarise(avgVel = mean(Velocity), medVel = median(Velocity))%>%
  ggplot(aes(x=conj_day)) + 
  geom_ma(aes(y=Velocity), n=900, linetype="solid")
  #geom_point(aes(y=(avgVel)), color="red")
  geom_point(aes(y=(medVel)), color="blue")
  
  