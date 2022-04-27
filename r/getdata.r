


# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))

# read files --------------------------------------------------------------
ribsfn <- "2020.04.24_NYS DEC RIBS Sampling Data.xlsx"
vfwdfn <- "2020.05.04_VFWD IWIS Sampling Data.xlsx"

#.. NY data ----
ny1 <- read_excel(here::here("data", ribsfn), sheet="Streams_ Metrics") # note space in sheet name
dim(ny1) # 58, 30
summary(ny1) # date 1984-10-04 -- 2016-08-04
# drop SHANNON, SHANNON_SCORE, NCO_RICH, NCO_RICH_SCORE, PCT_DOM3, PCT_DOM3_SCORE
# all are missing

# other NY sheets
ny1b <- read_excel(here::here("data", ribsfn), sheet="Sheet1")
dim(ny1b)
summary(ny1b)

intersect(names(ny1), names(ny1b))
# ny1b appears to be a subset of variables in ny1, same records, different var names
# Sheet2 may have information on sampling plans, not included in ny1

ny <- ny1 %>%
  select(-c(SHANNON, SHANNON_SCORE, NCO_RICH, NCO_RICH_SCORE, PCT_DOM3, PCT_DOM3_SCORE)) %>%
  lcnames() %>%
  rename(locdesc=`location desc`) %>%
  mutate(date=as.Date(date),
         src="NYSDEC")
ny


#.. VT data ----
vt1 <- read_excel(here::here("data", vfwdfn), sheet="Sheet1") # only 1 sheet
dim(vt1)  # 95, 17
glimpse(vt1)
summary(vt1) # 1986-10-02..2019-09-25

# convert final_score to numeric
# 6: Very good - good
# 7: Very good
# 9: Excellent

vt <- vt1 %>%
  lcnames() %>%
  rename(biosite=`bio site`, 
         locdesc=`location desc`,
         eptchiro=`ept/ept + chiro`,
         siteid=`site id`,
         ppcs_f=`ppcs-f`) %>%
  mutate(date=as.Date(date),
         final_score=str_sub(final_score, 1, 1) %>% as.integer(),
         src="VFWD")
vt


# Combine ny and vt -------------------------------------------------------
ns(ny); ns(vt)
intersect(names(ny), names(vt))
# [1] "Location Desc" "Latitude"      "Longitude"     "Site"          "RIVMILE_NUM"   "DATE"          "FINAL_SCORE"   "RICHNESS"     
# [9] "HBI"           "PMA"           "EPT_RICH"     
intersect(unique(ny$site), unique(vt$site)) # NONE!
# I am guessing df1 (NYS DEC RIBS) is in NY, and
#               df2 is in VT (VFWD IWIS)

bkill1 <- bind_rows(ny, vt)
glimpse(bkill1)
ns(bkill1)

bkill2 <- bkill1 %>%
  rename(rivermile=rivmile_num)
saveRDS(bkill2, here::here("data", "bkill_raw.rds"))

