

bkill

glimpse(bkill)

repeats <- bkill %>%
  filter(!is.na(hbi)) %>%
  count(locid, locdesc, sort = TRUE) %>%
  filter(n > 1)

p <- bkill %>%
  filter(locid %in% (repeats %>% filter(n >=4) %>% pull(locid))) %>%
  ggplot(aes(date, hbi)) +
  geom_point(size=2.25, colour="blue") +
  # geom_smooth(se=FALSE) +
  scale_y_continuous(breaks=0:10, limits=c(0, NA)) +
  facet_wrap(~locid+locdesc, ncol=3) +
  labs(x=NULL) +
  ggtitle("HBI over time at locations that had at least 4 repeated observations") +
  theme_bw()
p
ggsave(here::here("results", "hbi_over_time.png"), height=10, width=7.5, units="in", scale = 1)

eptrepeats <- bkill %>%
  filter(!is.na(ept_rich)) %>%
  count(locid, locdesc, sort = TRUE) %>%
  filter(n > 1)

p <- bkill %>%
  filter(locid %in% (eptrepeats %>% filter(n >=4) %>% pull(locid))) %>%
  ggplot(aes(date, ept_rich)) +
  geom_point(size=2.25, colour="blue") +
  # geom_smooth(se=FALSE) +
  scale_y_continuous(limits=c(0, NA)) +
  facet_wrap(~locid+locdesc, ncol=3) +
  labs(x=NULL) +
  ggtitle("EPT richness measure over time at locations that had at least 4 repeated observations") +
  theme_bw()
p
ggsave(here::here("results", "eptrich_over_time.png"), height=10, width=7.5, units="in", scale = 1)


