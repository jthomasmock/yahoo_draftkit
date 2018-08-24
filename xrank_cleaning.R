library(tidyverse)
library(here)

# read in xrank data
df_xrank <- read_csv(here::here("./draft/data/", "yahoo_xrank.csv"))

head(df_xrank)

# clean up data
clean_xrank <- df_xrank %>% 
  mutate(player = str_remove(player, "\ue023 false \ue023 \ue061 "),
         player = str_remove(player, " Injured: Questionable| Injured: Out| Suspended| Injured: Physically Unable to Perform| NA| Injured: Injured Reserve"),
         position = str_extract(player, " - RB| - WR| - QB| - TE| - K| - DEF"),
         player = str_remove(player, position),
         position = str_extract(position, "[:alpha:]+"),
         team = str_extract(player, tomtom::substr_right(player, 3)),
         player = case_when(position == "DEF" ~ player,
                            TRUE ~ str_remove(player, team)),
         team = toupper(str_extract(team, "[:alpha:]+")),
         player = str_trim(player),
         player = str_remove(player, " Jr.| II"),
         player = case_when(player == "Odell Beckham" ~ "Odell Beckham Jr.",
                            TRUE ~ player)) %>% 
  mutate(player =  case_when(position == "DEF" ~ str_trim(substr(player,1,nchar(player)-3)),
                           TRUE ~   player),
         position = case_when(str_detect(position, "DEF") ~ "DST",
                              TRUE ~ position)) %>% 
  select(player, team, position, yahoo_xrank, everything()) %>%
  filter(yahoo_xrank <= 2570) %>% 
  rename("name" = player) %>% 
  mutate(name = case_when(str_detect(tolower((name)), "ronald jones") ~ "Ronald Jones II",
                          str_detect(tolower((name)), "will fuller") ~ "Will Fuller",
                          str_detect(tolower((name)), "devante parker") ~ "Devante Parker",
                          str_detect(tolower((name)), "trubisky") ~ "Mitch Trubisky",
                          str_detect(tolower((name)), "john brown") ~ "John Brown",
                          str_detect(tolower((name)), "martavis") ~ "Martavis Bryant",
                          str_detect(tolower(name), "elijah mcguire") ~ "Elijah McGuire",
                          TRUE ~ name),
         name = case_when(team == "LAC" & name == "Los Angeles" ~ "Los Angeles Chargers",
                          team == "LAR" & name == "Los Angeles" ~ "Los Angeles Rams",
                          TRUE ~ name),
         team = case_when(name == "Dez Bryant" ~ "FA",
                          name == "Adrian Peterson" ~ "WAS",
                          name == "Jordan Matthews" ~ "FA",
                          name == "Elijah McGuire" ~ "NYJ",
                          TRUE ~ team))

clean_xrank %>% 
  write_rds(here::here("./draft/data/", paste0(Sys.Date(),"_yahoo_xrank.rds")))
