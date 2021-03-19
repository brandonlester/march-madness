library(tidyverse)

tn_games <- read_rds("data/tn_games.rds")
pred_games <- read_rds("data/pred_games.rds")

names(tn_games)[!names(tn_games) %in% names(pred_games)]
names(pred_games)[!names(pred_games) %in% names(tn_games)]

gen_vars <- tn_games %>% select(Season, DayNum, NumOT, spread, lower_id, higher_id) %>% names()
tn_games 

train_index <- sample(nrow(tn_games), nrow(tn_games)*0.8)
tn_games_train <- tn_games[train_index, ]
tn_games_test <- tn_games[-train_index, ]

library(ranger)

m_ranger1 <- ranger(lower_id_won ~ ., data = tn_games_train, classification = TRUE)
MLmetrics::LogLoss(m_ranger1$predictions, tn_games_train$lower_id_won)
