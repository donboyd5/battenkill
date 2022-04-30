
source(here::here("r", "libraries.r"))

# clean data --------------------------------------------------------------
bkill1 <- readRDS(here::here("data", "bkill_raw.rds"))

#.. fix latitude, longitude ----
bkill2 <- bkill1 %>%
  mutate(latitude_raw=latitude,
         longitude_raw=longitude,
         # Site id 502377, White Creek Rt 153 bridge, with stated lat, long of (42.2363891,	-73.2436065) should have latitude start with 43, not 42
         # Site id 521549	White Creek off Kent Hollow Rd, with stated lat, long of (42.2342491,	-73.2149276) should have latitude start with 43, not 42
         latitude=case_when(src=="VFWD" & siteid==502377 ~ latitude_raw + 1,
                            src=="VFWD" & siteid==502378 ~ latitude_raw + 1,
                            src=="VFWD" & siteid==521549 ~ latitude_raw + 1,
                            TRUE ~ latitude_raw),
         longitude=case_when(src=="VFWD" & siteid==502397 ~ longitude_raw + 1,
                             TRUE ~ longitude_raw))

# count(bkill2 %>% filter(latitude != latitude_raw), latitude_raw, latitude)
# count(bkill2 %>% filter(longitude != longitude_raw), longitude_raw, longitude)

bkill2 %>%
  filter(latitude != latitude_raw | longitude != longitude_raw) %>%
  select(src, siteid, locdesc, latitude_raw, longitude_raw, latitude, longitude) %>%
  distinct() %>%
  arrange(siteid)

#.. get stream identification info ----

stream_map <- read_csv("
streamid, stream
BATT, Battenkill River
BEVS, Beaver Brook       
BLKC, Black Creek        
CAMD, Camden Creek
CHNK, Chunks Brook       
FLYG, Fly Creek          
HART, Hartshorn Brook    
WBLA, West Black Creek   
WHIT, White Creek        
WHTK, Whitaker Brook
BOURN,  Bourn Brook
BRANCH,  Branch Pond Brook
BROM, Bromley Brook
FAYV, Fayville Branch 
GREEN, Green River
LYE, Lye Brook
MADTOM, Mad Tom Brook
MUNSON, Munson Brook
ROAR, Roaring Branch
TANNER, Tanner Brook
WARMB, Warm Brook
WESTBR, West Branch Battenkill
WHIT, White Creek  
")

bkill3 <- bkill2 %>%
  mutate(streamid=case_when(
    src=="NYSDEC" ~ location,
    str_detect(locdesc, "Battenkil") & # note I only use one "l" in str_detect
      !str_detect(locdesc, "West Br") ~ "BATT", # Battenkill
    str_detect(locdesc, "Bourn") ~ "BOURN",  # Bourn Brook
    str_detect(locdesc, "Branch") ~ "BRANCH",  # Branch Pond Brook
    str_detect(locdesc, "Bromley") ~ "BROM", # Bromley Brook
    str_detect(locdesc, "Fayville") ~ "FAYV", # Fayville Branch 
    str_detect(locdesc, "Green") ~ "GREEN", # Green River
    str_detect(locdesc, "Lye") ~ "LYE", # Lye Brook
    str_detect(locdesc, "Mad Tom") ~ "MADTOM", # Mad Tom Brook
    str_detect(locdesc, "Munson") ~ "MUNSON", # Munson Brook
    str_detect(locdesc, "Roaring Branch") ~ "ROAR", # Roaring Branch
    str_detect(locdesc, "Tanner Brook") ~ "TANNER", # Tanner Brook
    str_detect(locdesc, "Warm Brook") ~ "WARMB", # Warm Brook
    str_detect(locdesc, "West Br Battenkill") ~ "WESTBR", # West Branch Battenkill
    str_detect(locdesc, "White Creek") ~ "WHIT", # White Creek
    TRUE ~ NA_character_),
    stream=factor(streamid, levels=stream_map$streamid, labels=stream_map$stream)
  )

#.. get unique location id ----
# create unique location id: for NYDEC BAS_LOC_RM, for VFWD siteid
bkill4 <- bkill3 %>%
  mutate(locid=ifelse(src=="NYSDEC", 
                      paste0("NYSDEC_", bas_loc_rm),
                      paste0("VFWD_", str_pad(siteid, width = 11, side="left", pad="0")))  # up to 6 digits
  )

check <- count(bkill4, locid)

#.. add state abbreviations ----
ny_vtrecs <- c("11-BATT-34.2", "11-BATT-45.6", "11-BATT-46.1", "11-BATT-46.8")  # BAS_LOC_RM

bkill5 <- bkill4 %>%
  mutate(stabbr=case_when(src=="VFWD" ~ "VT",
                          bas_loc_rm %in% ny_vtrecs ~ "VT",
                          src=="NYSDEC" ~ "NY",
                          TRUE ~ "Error"))

#.. clean the Hilsenhoff Biotic Index (HBI) ----
bkill5 %>%
  ggplot(aes(date, hbi, colour=stabbr)) +
  geom_point()

bkill5 %>%
  filter(hbi > 10) %>%
  select(stabbr, siteid, locdesc, site, date, hbi, hbi_score)

bkill5 %>%
  # filter(siteid==502390) %>%
  filter(str_detect(locdesc, "Lye Brook"), year(date) < 1995) %>%
  select(stabbr, siteid, locdesc, site, date, hbi, hbi_score)

bkill6 <- bkill5 %>%
  mutate(year=year(date),
         hbiraw=hbi,
         hbi=ifelse(hbi > 20, 2.65, hbi)) # based on review of Excel file
glimpse(bkill6)


#.. adjusted data file ----
bkill <- bkill6
saveRDS(bkill, here::here("data", "bkill.rds"))

