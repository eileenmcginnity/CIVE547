library(tidyverse)
library(hms)

storms2003 <- read_csv("data/StormEvents_2003.csv")
storms2004 <- read_csv("data/StormEvents_2004.csv")
storms2005 <- read_csv("data/StormEvents_2005.csv")
storms2006 <- read_csv("data/StormEvents_2006.csv")
storms2007 <- read_csv("data/StormEvents_2007.csv")
storms2008 <- read_csv("data/StormEvents_2008.csv")
storms2009 <- read_csv("data/StormEvents_2009.csv")
storms2010 <- read_csv("data/StormEvents_2010.csv")
storms2011 <- read_csv("data/StormEvents_2011.csv")
storms2012 <- read_csv("data/StormEvents_2012.csv")
storms2013 <- read_csv("data/StormEvents_2013.csv")
storms2014 <- read_csv("data/StormEvents_2014.csv")
storms2015 <- read_csv("data/StormEvents_2015.csv")
storms2016 <- read_csv("data/StormEvents_2016.csv")
storms2017 <- read_csv("data/StormEvents_2017.csv")
storms2018 <- read_csv("data/StormEvents_2018.csv")
storms2019 <- read_csv("data/StormEvents_2019.csv")
storms2020 <- read_csv("data/StormEvents_2020.csv")
storms2021 <- read_csv("data/StormEvents_2021.csv")
storms2022 <- read_csv("data/StormEvents_2022.csv")

storms <- full_join(storms2022, storms2021) 
storms <- full_join(storms, storms2020)
storms <- full_join(storms, storms2019)
storms <- full_join(storms, storms2018) 
storms <- full_join(storms, storms2017)
storms <- full_join(storms, storms2016)
storms <- full_join(storms, storms2015)
storms <- full_join(storms, storms2014)
storms <- full_join(storms, storms2013)
storms <- full_join(storms, storms2012) 
storms <- full_join(storms, storms2011)
storms <- full_join(storms, storms2010)
storms <- full_join(storms, storms2009) 
storms <- full_join(storms, storms2008)
storms <- full_join(storms, storms2007)
storms <- full_join(storms, storms2006)
storms <- full_join(storms, storms2005)
storms <- full_join(storms, storms2004)
storms <- full_join(storms, storms2003)

## DATA CLEANING ##

hail_storms <- storms %>% 
  rename_all(.fun = str_to_lower) %>%
  mutate(state = str_to_title(state),
         cz_name = str_to_title(cz_name),
         source = str_to_title(source),
         source = ifelse(source == "Official Nws Observations", "Official NWS Observations", source),
         source = ifelse(source == "Official Nws Obs.", "Official NWS Observations", source),
         source = ifelse(source == "General Public", "Public", source),
         source = ifelse(source == "Awos", "Weather Stations", source),
         source = ifelse(source == "Asos", "Weather Stations", source),
         source = ifelse(source == "Coop Observer", "Weather Stations", source),
         source = ifelse(source == "Coop Station", "Weather Stations", source),
         source = ifelse(source == "Mesonet", "Weather Stations", source),
         source = ifelse(source == "Cocorahs", "CoCoRaHS/SHAVE", source),
         source = ifelse(source == "Shave Project", "CoCoRaHS/SHAVE", source),
         source = ifelse(source == "911 Call Center", "Emergency Management", source),
         source = ifelse(source == "Emergency Manager", "Emergency Management", source),
         source = ifelse(source == "Nws Employee", "NWS Employee", source),
         source = ifelse(source == "Nws Employee(Off Duty)", "NWS Employee", source),
         source = ifelse(source == "Govt Official", "Government Officials", source),
         source = ifelse(source == "State Official", "Government Officials", source),
         source = ifelse(source == "Department Of Highways", "Government Officials", source),
         source = ifelse(source == "County Official", "Government Officials", source),
         source = ifelse(source == "Park/Forest Service", "Federal Agencies", source),
         source = ifelse(source == "Other Federal Agency", "Federal Agencies", source),
         source = ifelse(source == "Post Office", "Federal Agencies", source),
         begin_date_time = dmy_hms(begin_date_time),
         end_date_time = dmy_hms(end_date_time)) %>%
  filter(event_type == "Hail") %>% 
  filter(magnitude >= 1) %>% 
  select(event_id, episode_id, begin_date_time, end_date_time, cz_fips, cz_name, source, begin_lat, begin_lon, end_lat, end_lon, magnitude, state)

hail_storms <- hail_storms %>% 
  mutate(month = month(begin_date_time, label = TRUE, abbr = FALSE),
         day = day(begin_date_time),
         year = year(begin_date_time),
         begin_time = as_hms(begin_date_time),
         end_time = as_hms(end_date_time))

hail_storms <- hail_storms %>% 
  filter(!is.na(begin_lat)) %>% 
  filter(!event_id %in% c(30824, 837851))

co_hail_storms <- hail_storms %>% 
  filter(state == "Colorado") %>% 
  select(-state)


co_hail_storms %>% 
  group_by(source) %>% 
  count() %>%
  arrange(desc(n)) %>% 
  print(n = 18)
## count per report source

storms_2003 <- co_hail_storms %>% 
  filter(year == 2003)
storms_2004 <- co_hail_storms %>% 
  filter(year == 2004)
storms_2005 <- co_hail_storms %>% 
  filter(year == 2005)
storms_2006 <- co_hail_storms %>% 
  filter(year == 2006)
storms_2007 <- co_hail_storms %>% 
  filter(year == 2007)
storms_2008 <- co_hail_storms %>% 
  filter(year == 2008)
storms_2009 <- co_hail_storms %>% 
  filter(year == 2009)
storms_2010 <- co_hail_storms %>% 
  filter(year == 2010)
storms_2011 <- co_hail_storms %>% 
  filter(year == 2011)
storms_2012 <- co_hail_storms %>% 
  filter(year == 2012)
storms_2013 <- co_hail_storms %>% 
  filter(year == 2013)
storms_2014 <- co_hail_storms %>% 
  filter(year == 2014)
storms_2015 <- co_hail_storms %>% 
  filter(year == 2015)
storms_2016 <- co_hail_storms %>% 
  filter(year == 2016)
storms_2017 <- co_hail_storms %>% 
  filter(year == 2017)
storms_2018 <- co_hail_storms %>% 
  filter(year == 2018)
storms_2019 <- co_hail_storms %>% 
  filter(year == 2019)
storms_2020 <- co_hail_storms %>% 
  filter(year == 2020)
storms_2021 <- co_hail_storms %>% 
  filter(year == 2021)
storms_2022 <- co_hail_storms %>% 
  filter(year == 2022)
storms_2022 <- co_hail_storms %>% 
  filter(year == 2022)

annual_season_length <- storms_2018 %>% 
  summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
  mutate(year = unique(storms_2018$year))
annual_season_length <- full_join(storms_2019 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2019$year)), annual_season_length)
annual_season_length <- full_join(storms_2020 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2020$year)), annual_season_length)
annual_season_length <- full_join(storms_2021 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2021$year)), annual_season_length)
annual_season_length <- full_join(storms_2022 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2022$year)), annual_season_length)
annual_season_length <- full_join(storms_2017 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2017$year)), annual_season_length)
annual_season_length <- full_join(storms_2016 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2016$year)), annual_season_length)
annual_season_length <- full_join(storms_2015 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2015$year)), annual_season_length)
annual_season_length <- full_join(storms_2014 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2014$year)), annual_season_length)
annual_season_length <- full_join(storms_2013 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2013$year)), annual_season_length)
annual_season_length <- full_join(storms_2012 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2012$year)), annual_season_length)
annual_season_length <- full_join(storms_2011 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2011$year)), annual_season_length)
annual_season_length <- full_join(storms_2010 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2010$year)), annual_season_length)
annual_season_length <- full_join(storms_2009 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2009$year)), annual_season_length)
annual_season_length <- full_join(storms_2008 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2008$year)), annual_season_length)
annual_season_length <- full_join(storms_2007 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2007$year)), annual_season_length)
annual_season_length <- full_join(storms_2006 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2006$year)), annual_season_length)
annual_season_length <- full_join(storms_2005 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2005$year)), annual_season_length)
annual_season_length <- full_join(storms_2004 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2004$year)), annual_season_length)
annual_season_length <- full_join(storms_2003 %>% 
                                    summarise(season_length = ceiling(as.numeric(max(begin_date_time) - min(begin_date_time)))) %>% 
                                    mutate(year = unique(storms_2003$year)), annual_season_length)
annual_season_length <- annual_season_length %>% 
  arrange(desc(year))

## EXPORT DATA FRAME ##

write.csv(co_hail_storms, "C:/Users/mcgin/Documents/Grad School/Fall 2023/CIVE 547 - Statistics for Environmental Monitoring/Final Project/data/co_hail_data.csv", row.names = FALSE)

write.csv(annual_season_length, "C:/Users/mcgin/Documents/Grad School/Fall 2023/CIVE 547 - Statistics for Environmental Monitoring/Final Project/data/annual_season_length.csv", row.names = FALSE)
