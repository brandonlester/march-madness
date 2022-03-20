library(tidyverse)
library(ranger)
#library(rsample)

submission_sample <- read_csv("data/mens-march-mania-2022/MDataFiles_Stage2/MSampleSubmissionStage2.csv")
teams <- read_csv("data/mens-march-mania-2022/MDataFiles_Stage2/MTeams.csv")
tn_games <- read_rds("tn_games.rds")
pred_games <- read_rds("pred_games.rds")

nm_remove <- names(tn_games)[!names(tn_games) %in% names(pred_games)]
nm_remove <- nm_remove[nm_remove != "lower_id_won"]
names(pred_games)[!names(pred_games) %in% names(tn_games)]


length(unique(tn_games$Season))

# use last 2 tournaments (seasons 2019 and 2021) as test
max_training_season <- 2018
dtrain <- tn_games %>% select(-all_of(nm_remove)) %>% filter(Season <= max_training_season)
dtest <- tn_games %>% select(-all_of(nm_remove)) %>%  filter(Season > max_training_season)


m_ranger1 <- ranger::ranger(
  formula = lower_id_won ~ .-Season-lower_id-higher_id, 
  data = dtrain, 
  probability = TRUE
)

train_wp <- m_ranger1$predictions[,2]
test_results <- predict(m_ranger1, data = dtest)
test_wp <- test_results$predictions[,2]

guess_logloss <- 0.693
#log_loss <- function(n, yhat, y) -sum(y*log(yhat)+(1-y)*log(1-yhat))/n
#log_loss(n = nrow(dtrain), yhat = train_wp, y = dtrain$lower_id_won)
MLmetrics::LogLoss(y_pred = train_wp, y_true = dtrain$lower_id_won)
MLmetrics::LogLoss(y_pred = test_wp, y_true = dtest$lower_id_won)


# tune --------------------------------------------------------------------

num_vars <- m_ranger1$num.independent.variables


# create hyperparameter grid
hyperparams <- expand.grid(
  mtry = floor(num_vars * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8)                       
)

test_perfs <- vector("list", nrow(hyperparams))

system.time(
for(i in seq_len(nrow(hyperparams))) {

  m <- ranger::ranger(
    formula         = lower_id_won ~ .-Season-lower_id-higher_id, 
    data            = dtrain, 
    probability     = TRUE,
    num.trees       = 1000,
    mtry            = hyperparams$mtry[i],
    min.node.size   = hyperparams$min.node.size[i],
    replace         = hyperparams$replace[i],
    sample.fraction = hyperparams$sample.fraction[i],
    seed            = 2022
  )
  
  tst <- predict(m, data = dtest)
  test_perfs[[i]] <- MLmetrics::LogLoss(y_pred = tst$predictions[,2], y_true = dtest$lower_id_won)
  
}
)

# assess top 10 models
hyperparams$test_log_loss <- unlist(test_perfs)

top10_hyperparams <- hyperparams %>%
  arrange(test_log_loss) %>%
  mutate(perc_gain = (guess_logloss - test_log_loss) / guess_logloss * 100) %>%
  head(10)

# final -------------------------------------------------------------------


m_final <- ranger::ranger(
  formula = lower_id_won ~ .-Season-lower_id-higher_id, 
  data = bind_rows(dtrain, dtest), 
  probability = TRUE,
  num.trees = 1000,
  mtry = top10_hyperparams$mtry[1],
  min.node.size = top10_hyperparams$min.node.size[1],
  replace = top10_hyperparams$replace[1],
  sample.fraction = top10_hyperparams$sample.fraction[1],
  seed = 2022
)

pred_results <- predict(m_final, pred_games)
pred_wp <- pred_results$predictions[,2]

pred_games$model_wp <- pred_wp


my_submission <- pred_games %>% select(ID, Pred = model_wp)

if(
  all(
    all(names(submission_sample) == names(my_submission)),
    nrow(submission_sample) == nrow(my_submission),
    sum(!submission_sample[["ID"]] %in% my_submission[["ID"]])==0,
    sum(!my_submission[["ID"]] %in% submission_sample[["ID"]])==0
  )
) write_csv(my_submission, "kaggle_mm_submission.csv")


winners <- pred_games %>% 
  select(ID, lower_id, higher_id, model_wp) %>% 
  left_join(select(teams, TeamID, TeamName), by = c("lower_id" = "TeamID")) %>% 
  left_join(
    select(teams, TeamID, TeamName), 
    by = c("higher_id" = "TeamID"),
    suffix = c("_lower", "_higher")
  ) %>% 
  mutate(
    winner = case_when(
      model_wp > 0.5 ~ TeamName_lower,
      model_wp < 0.5 ~ TeamName_higher,
      TRUE ~ NA_character_
    )
  ) #%>% pull(winner) %>% is.na() %>% sum()

who_wins <- function(team_a, team_b) {
  winners %>% 
    filter(
      TeamName_lower %in% c(team_a, team_b) & 
        TeamName_higher %in% c(team_a, team_b)
    ) %>% 
    pull(winner)
}

