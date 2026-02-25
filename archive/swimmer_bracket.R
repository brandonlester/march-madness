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


df_region_order <- data.frame(region = c("Y","Z","W","X"), region_num = 1:4)


df_bracket <- df_slots_ready %>% 
  left_join(select(df_teams, TeamID, TeamName), by = "TeamID") %>% 
  left_join(select(df_teams, TeamID, winner_name = TeamName), by = c("winner_id" = "TeamID")) %>% 
  mutate(region = ifelse(round>0 & round<5, str_sub(Slot, 3, 3), NA_character_)) %>% 
  left_join(df_region_order, by = "region")


# for (region in regions)





#region_slots <- df_slots_ready %>% filter(region=="W")

teams <- df_bracket %>% 
  filter(round==1) %>% 
  mutate(region_seed = as.integer(str_sub(Seed, 2, 3)),
         num_regions = max(df_region_order$region_num),
         seed_subtractor = ifelse(region_seed%%2>0, 4 - region_num, region_num - 1),
         overall_seed = num_regions * region_seed - seed_subtractor) %>% 
  #arrange(region_seed, region_num) %>% 
  arrange(overall_seed) %>% 
  pull(TeamName)

r2_teams <- df_bracket %>% filter(round == 2) %>% pull(TeamName)
r3_teams <- df_bracket %>% filter(round == 3) %>% pull(TeamName)
r4_teams <- df_bracket %>% filter(round == 4) %>% pull(TeamName)
r5_teams <- df_bracket %>% filter(round == 5) %>% pull(TeamName)
r6_teams <- df_bracket %>% filter(round == 6) %>% pull(TeamName)
champ_team <- df_bracket %>% filter(round == 6) %>% pull(winner_name) %>% unique()



draw_bracket(
  teams = teams,
  title = "Championship Bracket",
  text_size = 0.5,
  round_two = r2_teams,
  round_three = r3_teams,
  round_four = r4_teams,
  round_five = r5_teams,
  round_six = r6_teams,
  champion = champ_team
)



df_bracket %>% 
  filter(region=='W')

"ABC"+"XYZ"



library(DiagrammeR)

# Create a tournament bracket
# grViz("
# digraph TournamentBracket {
#   rankdir=LR; // Horizontal layout
# 
#   // Nodes for the bracket
#   node [shape=box style=filled color=lightblue];
# 
#   A1 [label='Team 1'];
#   A2 [label='Team 2'];
#   A3 [label='Team 3'];
#   A4 [label='Team 4'];
#   A5 [label='Team 5'];
#   A6 [label='Team 6'];
#   A7 [label='Team 7'];
#   A8 [label='Team 8'];
# 
#   B1 [label='Winner 1'];
#   B2 [label='Winner 2'];
#   B3 [label='Winner 3'];
#   B4 [label='Winner 4'];
# 
#   C1 [label='Semi Finalist 1'];
#   C2 [label='Semi Finalist 2'];
# 
#   D1 [label='Champion'];
# 
#   // Edges to connect rounds
#   A1 -> B1;
#   A2 -> B1;
# 
#   A3 -> B2;
#   A4 -> B2;
# 
#   A5 -> B3;
#   A6 -> B3;
# 
#   A7 -> B4;
#   A8 -> B4;
# 
#   B1 -> C1;
#   B2 -> C1;
# 
#   B3 -> C2;
#   B4 -> C2;
# 
#   C1 -> D1;
#   C2 -> D1;
# }
# ")





s1_name <- "1"
s2_name <- "2"
s3_name <- "3"
s4_name <- "4"
s5_name <- "5"
s6_name <- "6"
s7_name <- "7"
s8_name <- "8"
s9_name <- "9"
s10_name <- "10"
s11_name <- "11"
s12_name <- "12"
s13_name <- "13"
s14_name <- "14"
s15_name <- "15"
s16_name <- "16"


get_slot <- function(arg_slot, arg_region, arg_seed=NA_character_){
  
  slot_to_pull <- str_replace(arg_slot,"_",arg_region)
  seed_to_pull <- paste(arg_region, arg_seed, sep="")
  
  if(is.na(arg_seed)){
    return_team <- df_bracket %>% 
      filter(Slot==slot_to_pull) %>% 
      pull(winner_name) %>% 
      unique()
  }
  else {
    return_team <- df_bracket %>% 
      filter(Seed == seed_to_pull) %>% 
      pull(TeamName)
  }
  
  return_team <- str_replace(return_team, "'","")
  return_team <- str_replace_all(return_team, " ","")
  return_team <- str_replace_all(return_team, "&","")
  
  return(return_team)

}

region<-"X"
fontsize<-10
fixedsize<-TRUE
shape<-"box"
rankdir<-"RL"

grViz(paste("digraph TournamentBracket {
    rankdir=", rankdir, "
    
    {
        rank=same
        node[shape=", shape, " fixedsize=", fixedsize, " fontsize=", fontsize, "]
        
        A6  [label = ", get_slot("R1_6",region,"06"), "]
        A11 [label = ", get_slot("R1_6",region,"11"), "]
        A3  [label = ", get_slot("R1_3",region,"03"), "]
        A14 [label = ", get_slot("R1_3",region,"14"), "]
        A7  [label = ", get_slot("R1_7",region,"07"), "]
        A10 [label = ", get_slot("R1_7",region,"10"), "]
        A2  [label = ", get_slot("R1_2",region,"02"), "]
        A15 [label = ", get_slot("R1_2",region,"15"), "]
        A1  [label = ", get_slot("R1_1",region,"01"), "]
        A16 [label = ", get_slot("R1_1",region,"16"), "]
        A8  [label = ", get_slot("R1_8",region,"08"), "]
        A9  [label = ", get_slot("R1_8",region,"09"), "]
        A5  [label = ", get_slot("R1_5",region,"05"), "]
        A12 [label = ", get_slot("R1_5",region,"12"), "]
        A4  [label = ", get_slot("R1_4",region,"04"), "]
        A13 [label = ", get_slot("R1_4",region,"13"), "]
    }

    {
        rank=same
        node[shape=", shape, " fixedsize=", fixedsize, " fontsize=", fontsize, "]
        
        B1 [label = ", get_slot("R1_1", region), "]
        B2 [label = ", get_slot("R1_2", region), "]
        B3 [label = ", get_slot("R1_3", region), "]
        B5 [label = ", get_slot("R1_5", region), "]
        B4 [label = ", get_slot("R1_4", region), "]
        B6 [label = ", get_slot("R1_6", region), "]
        B7 [label = ", get_slot("R1_7", region), "]
        B8 [label = ", get_slot("R1_8", region), "]
    }
    
    
    {
        rank=same
        node[shape=", shape, " fixedsize=", fixedsize, " fontsize=", fontsize, "]
        
        C1 [label = ", get_slot("R2_1", region), "]
        C2 [label = ", get_slot("R2_2", region), "]
        C3 [label = ", get_slot("R2_3", region), "]
        C4 [label = ", get_slot("R2_4", region), "]
    }
    
    {
        rank=same
        node[shape=", shape, " fixedsize=", fixedsize, " fontsize=", fontsize, "]
        
        D1 [label = ", get_slot("R3_1", region), "]
        D2 [label = ", get_slot("R3_2", region), "]
    }
    
    {
        rank=same
        node[shape=", shape, " fixedsize=", fixedsize, " fontsize=", fontsize, "]
        
        E1 [label = ", get_slot("R4_1", region), "]
    }

    
  A1 -> B1;   
  A16 -> B1; 
  A8 -> B8;   
  A9 -> B8; 
  A5 -> B5;   
  A12 -> B5;
  A4 -> B4;   
  A13 -> B4;
  A6 -> B6;   
  A11 -> B6;
  A3 -> B3;   
  A14 -> B3;    
  A7 -> B7;   
  A10 -> B7;
  A2 -> B2;   
  A15 -> B2;
      
  B1 -> C1;   
  B8 -> C1; 
  B4 -> C4;   
  B5 -> C4;
  B3 -> C3;   
  B6 -> C3;
  B2 -> C2;   
  B7 -> C2;      
  C1 -> D1;   
  C4 -> D1;      
  C2 -> D2;   
  C3 -> D2;      
  D1 -> E1;   
  D2 -> E1; 
  
}", sep=""))






grViz(paste("digraph TournamentBracket {
    rankdir=LR
    
    
    {
        rank=same
        node[shape=", shape, " fixedsize=", fixedsize, " fontsize=", fontsize, "]
        
        D1 [label = ", get_slot("R4_1", arg_region="Y"), "]
        D2 [label = ", get_slot("R4_1", arg_region="Z"), "]
    }
    
    {
        rank=same
        node[shape=", shape, " fixedsize=", fixedsize, " fontsize=", fontsize, "]
        
        E1 [label = ", df_bracket %>% filter(Slot=="R5YZ") %>% pull(winner_name) %>% unique(), "]
    }

  D1 -> E1;   
  D2 -> E1; 
  
}", sep=""))




grViz(paste("digraph TournamentBracket {
    rankdir=RL
    
    
    {
        rank=same
        node[shape=", shape, " fixedsize=", fixedsize, " fontsize=", fontsize, "]
        
        D2 [label = ", get_slot("R4_1", arg_region="W"), "]
        D1 [label = ", get_slot("R4_1", arg_region="X"), "]
    }
    
    {
        rank=same
        node[shape=", shape, " fixedsize=", fixedsize, " fontsize=", fontsize, "]
        
        E1 [label = ", df_bracket %>% filter(Slot=="R5WX") %>% pull(winner_name) %>% unique(), "]
    }

  D1 -> E1;   
  D2 -> E1; 
  
}", sep=""))




grViz(paste("digraph TournamentBracket {
    rankdir=LR
    
    {
        rank=same
        node[shape=", shape, " fixedsize=", fixedsize, " fontsize=", fontsize, "]
        
        E1 [label = ", df_bracket %>% filter(Slot=="R6CH") %>% pull(winner_name) %>% unique(), "]
    }

  
}", sep=""))
