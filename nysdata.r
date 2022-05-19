
source(here::here("r", "libraries.r"))
library(vroom)

# portal_metadata_2021-10-05.xlsx
# Stream Monitoring Sites.csv
# Streams_Chemistry.csv
# Streams_Habitat.csv
# Streams_Macroinvertebrate raw data.csv
# Streams_Macroinvertebrate sample info and metrics.csv
# Streams_Sample event info.csv
# Streams_User perception.csv

csvnames <- c("Stream Monitoring Sites.csv", 
              "Streams_Chemistry.csv", 
              "Streams_Habitat.csv", 
              "Streams_Macroinvertebrate raw data.csv",
              "Streams_Macroinvertebrate sample info and metrics.csv",
              "Streams_Sample event info.csv",
              "Streams_User perception.csv")

sites <- vroom(here::here("data", "nysdec", csvnames[1]))
chem <- vroom(here::here("data", "nysdec", csvnames[2]))
habitat <- vroom(here::here("data", "nysdec", csvnames[3]))
macraw <- vroom(here::here("data", "nysdec", csvnames[4]))
macmetric <- vroom(here::here("data", "nysdec", csvnames[5]))
event <- vroom(here::here("data", "nysdec", csvnames[6]))
perceive <- vroom(here::here("data", "nysdec", csvnames[7]))


tmp <- sites %>%
  filter(str_sub(site_id, 1, 2)=="11",
         fips_code==115 | str_detect(municipality, "VT"))
# 39 sites
# don't use is.na(fips_code) as it picks up MA sites -- Wallomsac, etc.

