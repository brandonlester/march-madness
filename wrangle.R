library(tidyverse)
library(lubridate)


# import ------------------------------------------------------------------

kaggle_data_folder <- "data/ncaam-march-mania-2021/MDataFiles_Stage2/"
kaggle_data_files <- list.files(kaggle_data_folder)

for (i in 1:length(kaggle_data_files)) {
  df_name <- stringr::str_remove(kaggle_data_files[i], ".csv")
  df <- readr::read_csv(paste0(kaggle_data_folder,kaggle_data_files[i]))
  assign(df_name, df)
  rm(df_name, df)
}

dfs_basics <- c(
  "MTeams", "MSeasons", "MNCAATourneySeeds", 
  "MRegularSeasonCompactResults", "MNCAATourneyCompactResults", "MSampleSubmissionStage2"
)
dfs_boxscores <- c("MRegularSeasonDetailedResults", "MNCAATourneyDetailedResults")
dfs_geography <- c("Cities", "MGameCities")
dfs_rankings <- c("MMasseyOrdinals")
dfs_supplements <- c(
  "MTeamCoaches", "Conferences", "MTeamConferences", 
  "MConferenceTourneyGames", "MSecondaryTourneyTeams", "MSecondaryTourneyCompactResults", 
  "MTeamSpellings", "MNCAATourneySlots", "MNCAATourneySeedRoundSlots"
)

teams <- MTeams
seasons <- MSeasons
seeds <- MNCAATourneySeeds
sample_stage2 <- MSampleSubmissionStage2
reg_results <- MRegularSeasonDetailedResults
tn_results <- MNCAATourneyDetailedResults
rm(MTeams, MSeasons, MNCAATourneySeeds, MSampleSubmissionStage2, MRegularSeasonDetailedResults, MNCAATourneyDetailedResults)

sample_stage1 <- readr::read_csv("data/ncaam-march-mania-2021/MDataFiles_Stage1/MSampleSubmissionStage1.csv")

# basic eda ---------------------------------------------------------------

#teams
head(teams)
skimr::skim(teams)

teams %>% count(TeamID, TeamName) %>% arrange(desc(n))
teams %>% count(TeamID) %>% arrange(desc(n))
teams %>% count(TeamName) %>% arrange(desc(n))

teams %>% filter(LastD1Season>=2020) %>% nrow()

#seasons
head(seasons)
skimr::skim(seasons)

max(seasons$Season) - min(seasons$Season) + 1 == nrow(seasons)
seasons$DayZero <- mdy(seasons$DayZero)
day0days <- day(seasons$DayZero)
day0days[nchar(day0days)==1] <- paste0("0", day0days[nchar(day0days)==1])
range(as.numeric(paste0(month(seasons$DayZero), day0days)))

seasons %>% count(RegionW) %>% arrange(desc(n))
seasons %>% count(RegionX) %>% arrange(desc(n))
seasons %>% count(RegionY) %>% arrange(desc(n))
seasons %>% count(RegionZ) %>% arrange(desc(n))

#seeds
head(seeds)
skimr::skim(seeds)

seeds %>% count(Seed) %>% arrange(desc(n))
seeds %>% .$Season %>% unique() %>% length()
seeds %>% .$TeamID %>% unique() %>% length()

#MRegularSeasonCompactResults
#MNCAATourneyCompactResults

#samples
head(sample_stage1)
skimr::skim(sample_stage1)

head(sample_stage2)
skimr::skim(sample_stage2)

#regular seasons results
head(reg_results)
skimr::skim(reg_results)

reg_results %>% count(WLoc) %>% arrange(desc(n))
WLocs <- reg_results %>% filter(WLoc != "N") %>% count(WLoc)
WLocs$n[WLocs$WLoc=="H"] / sum(WLocs$n)

reg_results %>% 
  count(WTeamID) %>% 
  arrange(desc(n)) %>% 
  mutate(TeamID = as.character(WTeamID)) %>% 
  left_join(mutate(teams, TeamID = as.character(TeamID)), by = "TeamID") %>% 
  select(TeamName, n)

reg_results %>% ggplot(aes(x = WScore)) + geom_histogram()
reg_results %>% ggplot(aes(x = LScore)) + geom_histogram()
reg_results %>% ggplot(aes(x = WScore - LScore)) + geom_histogram()

reg_results %>% count(NumOT)

# wrangle -----------------------------------------------------------------

reg_records <- full_join(
  count(reg_results, Season, WTeamID), #wins
  count(reg_results, Season, LTeamID), #loss
  by = c("Season", "WTeamID" = "LTeamID"), 
  suffix = c("_wins", "_losses")
  ) %>% 
  rename(TeamID = WTeamID) %>% 
  mutate(
    n_wins = ifelse(is.na(n_wins), 0, n_wins),
    n_losses = ifelse(is.na(n_losses), 0, n_losses)
  ) %>% 
  mutate(win_pct = n_wins / (n_wins + n_losses))

tn_15to19 <- tn_results %>% 
  filter(Season >= 2015) %>% 
  mutate(
    lower_id = ifelse(WTeamID < LTeamID, WTeamID, LTeamID),
    higher_id = ifelse(WTeamID < LTeamID, LTeamID, WTeamID),
    id = paste(Season, lower_id, higher_id, sep = "_")
  ) %>% 
  mutate(actual = ifelse(lower_id == WTeamID, 1, 0))

sample1_ids <- str_split(sample_stage1$ID, "_")
sample_stage1$Season <- sample1_ids %>% map(1) %>% unlist()
sample_stage1$LowerID <- sample1_ids %>% map(2) %>% unlist()
sample_stage1$HigherID <- sample1_ids %>% map(3) %>% unlist()

win_pct_picks <- sample_stage1 %>%
  left_join(
    transmute(reg_records, Season = as.character(Season), TeamID = as.character(TeamID), win_pct),
    by = c("Season" = "Season", "LowerID" = "TeamID")
  ) %>%
  left_join(
    transmute(reg_records, Season = as.character(Season), TeamID = as.character(TeamID), win_pct),
    by = c("Season" = "Season", "HigherID" = "TeamID"),
    suffix = c(".lower", ".higher")
  ) %>% 
  mutate(Pred = ifelse(win_pct.lower > win_pct.higher, 1, 0)) %>% 
  left_join(select(tn_15to19, id, actual), by = c("ID"="id")) %>% 
  filter(!is.na(actual)) %>% 
  mutate(correct = Pred==actual)

mean(win_pct_picks$correct)
table(Pred = win_pct_picks$Pred, Actual = win_pct_picks$actual)


# wrangle2 ----------------------------------------------------------------

reg_results <- reg_results %>% mutate(id = paste(Season, DayNum, WTeamID, sep = "_")) 

wl_to_teamopp <- function(results, side) {
  stopifnot(side %in% c("W", "L"))
  stopifnot("data.frame" %in% class(results))
  
  team_chr <- "team."
  opp_chr <- "opp."
  
  w_chr <- if(side=="W") team_chr else opp_chr
  l_chr <- if(side=="W") opp_chr else team_chr
  
  names_to_change_w <- names(results)[str_sub(names(results),1,1)=="W" & names(results)!="WLoc"]
  names(results)[str_sub(names(results),1,1)=="W" & names(results)!="WLoc"] <- str_replace(names_to_change_w, "W", w_chr)
  
  names_to_change_l <- names(results)[str_sub(names(results),1,1)=="L"]
  names(results)[str_sub(names(results),1,1)=="L"] <- str_replace(names_to_change_l, "L", l_chr)
  
  results$team.won <- if(side == "W") 1 else 0
  
  return(results)
}

reg_team_results <- bind_rows(wl_to_teamopp(reg_results, "W"), wl_to_teamopp(reg_results, "L"))
reg_team_results %>% count(id) %>% filter(n!=2) %>% nrow() == 0

reg_season_stats <- reg_team_results %>% 
  group_by(Season, team.TeamID) %>% 
  summarise( #not using WLoc or NumOT or ID at all
    games = n(),
    last_game = max(DayNum),
    across(contains("."), sum, .names = "ttl.{.col}")
  ) %>% 
  select(-ttl.opp.TeamID) %>% 
  ungroup() %>% 
  rename(wins = ttl.team.won) %>% 
  mutate(win_pct = wins / games) %>% 
  mutate(
    team.fgm3_percent = ttl.team.FGM3 / ttl.team.FGA3,
    team.fgm_percent = ttl.team.FGM / ttl.team.FGA,
    team.ft_percent = ttl.team.FTM / ttl.team.FTA,
    opp.fgm3_percent = ttl.opp.FGM3 / ttl.opp.FGA3,
    opp.fgm_percent = ttl.opp.FGM / ttl.opp.FGA,
    opp.ft_percent = ttl.opp.FTM / ttl.opp.FTA,
  )

num_games <- reg_season_stats$games

per_game <- reg_season_stats %>% 
  select(starts_with("ttl")) %>% 
  map_df(~.x/num_games)

names(per_game) <- str_replace(names(per_game), "ttl", "pg")

reg_seasons_stats_wpg <- bind_cols(reg_season_stats, per_game)

reg_df <- reg_seasons_stats_wpg %>% 
  select(
    -starts_with("ttl."),
    -c(starts_with("pg") & (contains("FG") | contains("FT"))),
    -c(games, last_game, wins)
  )

# join with seeds and tourney results then model
seeds %>% count(Seed)
seeds <- seeds %>% 
  mutate(seed_num = as.numeric(gsub("[^0-9.-]", "", Seed))) #%>% 
  #count(seed_num)

tn_results %>% count(WLoc)

tn_games <- tn_results %>% 
  mutate(spread = WScore - LScore) %>% 
  select(Season, DayNum, NumOT, spread, WTeamID, LTeamID) %>% 
  mutate(
    lower_id = ifelse(WTeamID < LTeamID, WTeamID, LTeamID),
    higher_id = ifelse(WTeamID < LTeamID, LTeamID, WTeamID),
    lower_id_won = ifelse(lower_id == WTeamID, 1, 0)
  ) %>% 
  select(-WTeamID, -LTeamID) %>% 
  left_join(
    reg_df, 
    by = c("Season" = "Season", "lower_id" = "team.TeamID")
  ) %>% 
  left_join(
    reg_df, 
    by = c("Season" = "Season", "higher_id" = "team.TeamID"),
    suffix = c(".lower", ".higher")
  ) %>% 
  left_join(
    select(seeds, -Seed),
    by = c("Season" = "Season", "lower_id" = "TeamID")
  ) %>% 
  left_join(
    select(seeds, -Seed),
    by = c("Season" = "Season", "higher_id" = "TeamID"),
    suffix = c(".lower", ".higher")
  )

pred_games <- sample_stage2 %>% 
  mutate(
    id_components = str_split(ID, "_"),
    Season = as.numeric(map_chr(id_components, 1)),
    lower_id = as.numeric(map_chr(id_components, 2)),
    higher_id = as.numeric(map_chr(id_components, 3))
  ) %>% 
  select(-id_components) %>% 
  left_join(
    reg_df, 
    by = c("Season" = "Season", "lower_id" = "team.TeamID")
  ) %>% 
  left_join(
    reg_df, 
    by = c("Season" = "Season", "higher_id" = "team.TeamID"),
    suffix = c(".lower", ".higher")
  ) %>% 
  left_join(
    select(seeds, -Seed),
    by = c("Season" = "Season", "lower_id" = "TeamID")
  ) %>% 
  left_join(
    select(seeds, -Seed),
    by = c("Season" = "Season", "higher_id" = "TeamID"),
    suffix = c(".lower", ".higher")
  )

skimr::skim(tn_games)

saveRDS(tn_games, "data/tn_games.rds")
saveRDS(pred_games, "data/pred_games.rds")
