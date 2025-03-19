library(SwimmeR)
library(tidyverse)

this_year <- lubridate::year(Sys.Date())

df_winners <- readRDS("data/preds/winners.rds")
df_teams <- read_csv("data/kaggle/MTeams.csv")

df_winners <- df_winners %>% mutate(winner_id = ifelse(winner==TeamName_lower, lower_id, higher_id))
df_winners_rep <- df_winners %>% rename(lower_id = higher_id, higher_id = lower_id)
df_winners_all <- bind_rows(df_winners,df_winners_rep) %>% select(higher_id, lower_id, winner_id)

#df_seed_round_slots <- read_csv("data/kaggle/MNCAATourneySeedRoundSlots.csv")

df_seeds <- read_csv("data/kaggle/MNCAATourneySeeds.csv") %>% 
  filter(Season==this_year) %>% 
  select(-Season)

df_slots <- read_csv("data/kaggle/MNCAATourneySlots.csv") %>% 
  filter(Season==this_year) %>% 
  select(-Season)

# df_seeds$seed_nchar <- df_seeds$Seed %>% map_dbl(nchar)
# df_seeds$seed_main <- stringr::str_sub(df_seeds$Seed,1,3)
# df_seeds$seed_sub <- stringr::str_sub(df_seeds$Seed,4)
# 
# df_playins <- df_seeds %>% 
#   filter(seed_nchar>3) %>% 
#   pivot_wider(id_cols = seed_main, names_from = seed_sub, values_from = TeamID) %>% 
#   left_join(df_winners_all, by = c("a"="lower_id", "b"="higher_id")) %>% 
#   select(Seed = seed_main, TeamID = winner_id)

# 
# df_seeds_complete <- bind_rows(df_seeds, df_playins) %>% 
#   select(Seed, TeamID)



df_slots_ready <- df_slots %>% 
  pivot_longer(cols=c(StrongSeed,WeakSeed), names_to = "SeedName", values_to = "Seed") %>% 
  left_join(df_seeds, by = "Seed") %>% 
  mutate(round = ifelse(str_sub(Slot,1,1)=="R", str_sub(Slot,2,2),0),
         round = as.integer(round))

df_slots_ready$winner_id <- NA_real_
  


max_round <- max(df_slots_ready$round)
min_round <- min(df_slots_ready$round)

for (r in min_round:max_round) {
  print(r)
  
  df_matchups <- df_slots_ready %>% 
    filter(round == r) %>% 
    pivot_wider(id_cols = Slot, names_from = SeedName, values_from = TeamID) %>% 
    left_join(df_winners_all, by = c("StrongSeed"="higher_id", "WeakSeed"="lower_id")) %>% 
    select(Slot, winner_id)
  
  
  df_slots_ready <- df_slots_ready %>% 
    left_join(df_matchups, by = "Slot", suffix = c("","_update")) %>% 
    left_join(df_matchups %>% rename(TeamID=winner_id), by = c("Seed"="Slot"), suffix = c("","_update")) %>% 
    mutate(TeamID = coalesce(TeamID, TeamID_update),
           winner_id = coalesce(winner_id, winner_id_update)) %>% 
    select(-contains("_update"))
}







draw_bracket(
  teams = as.character(1:64),
  title = "Championship Bracket",
  text_size = 0.5,
  round_two = as.character(1:32),
  round_three = as.character(1:16),
  round_four = as.character(1:8),
  round_five = as.character(1:4),
  round_six = as.character(1:2),
  champion = as.character(1)
)
