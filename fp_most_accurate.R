library(tidyverse)
library(here)

# FP ECR most accurate
df <- read_csv(here::here("./draft/data/", "fp_ecr.csv"))

# tidy and clean
clean_df <- df %>% 
  janitor::clean_names() %>% 
  rename("name" = overall) %>% 
  mutate(position = str_extract(pos, "[:alpha:]+"),
         team = case_when(str_detect(position, "DST") ~ str_extract(str_sub(name, -4, -1), "[:alpha:]+"),
                          TRUE ~ team),
         name = case_when(position == "DST" ~ str_trim(str_sub(name, 1, -6)),
                          TRUE ~ name)) %>%
  rename(pos_rank = pos) %>% 
  select(name, team, position, pos_rank, rank, bye, avg, adp, vs_adp) %>% 
  filter(!is.na(name)) %>% 
  mutate(team = case_when(str_detect(name, "Dez Bryant") ~ "FA",
                          str_detect(name, "Jordan Matthews") ~ "FA",
                          str_detect(name, "Adrian Peterson") ~ "WAS",
                          str_detect(team, "JAC") ~ "JAX",
                          TRUE ~ team),
         name = case_when(str_detect(name, "Los Angeles") & team == "LAR" ~ "Los Angeles Rams",
                          str_detect(name, "Los Angeles") & team == "LAC" ~ "Los Angeles Chargers",
                          str_detect(name, "Jordan Matthews") & team == "LAR" ~ "Los Angeles Rams",
                          str_detect(name, "Robert Kelley") & team == "WAS" ~ "Rob Kelley",
                          str_detect(name, "Willie Snead") ~ "Willie Snead IV",
                          str_detect(name, "Terrelle Pryor") ~ "Terrelle Pryor Sr.",
                          TRUE ~ name)) %>% 
  select(-bye)

clean_df %>% 
  write_rds(here::here("./draft/data/", paste0(Sys.Date(),"_fp_ecr.rds")))
