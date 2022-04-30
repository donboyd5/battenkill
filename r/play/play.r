

# investigate and clean up -------------------------------------------------------------
# common <- site, locdesc, stabbr, vtnyrivermile, date, hbi, pma, ept_rich, richness, final_score
bkill1 <- readRDS(here::here("data", "bkill_raw.rds"))


bkill1 %>%
  ggplot(aes(date, hbi, colour=stabbr)) +
  geom_point()

bkill1 %>%
  filter(hbi > 10) %>%
  select(stabbr, siteid, locdesc, site, date, hbi, hbi_score)

bkill1 %>%
  # filter(siteid==502390) %>%
  filter(str_detect(locdesc, "Lye Brook"), year(date) < 1995) %>%
  select(stabbr, siteid, locdesc, site, date, hbi, hbi_score)

bkill2 <- bkill1 %>%
  mutate(year=year(date),
         hbiraw=hbi,
         hbi=ifelse(hbi > 20, 2.65, hbi)) # based on review of Excel file
glimpse(bkill2)

bkill2 %>%
  ggplot(aes(date, hbi, colour=stabbr)) +
  geom_point()


bkill2 %>%
  ggplot(aes(vtnyrivermile, hbi, colour=as.factor(year))) +
  geom_point() +
  geom_vline(xintercept = 0) +
  ggtitle("Hilsenhoff’s Biotic Index (HBI), over river mile")



# saveRDS(bkill2, here::here("data", "bkill.rds"))

