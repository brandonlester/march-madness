March Madness

This repository hosts the data and initial R code to build a predictive model for the NCAA Tournament.

The wrangle.R script takes the raw data provided by Kaggle and prepares it for modeling. The output of this step includes data/pred_games.rds and data/tn_games.rds.

The model.R script takes the output from data wrangling and begins the modeling process.

I did not discover the Kaggle competition until about a week before the 2021 tournament started, so only box score data was used and only a basic random forest classification model with all data thrown in was trained. I look forward to expanding upon this project for the upcoming college basketball season!

Link to Kaggle for data and competition information: https://www.kaggle.com/c/ncaam-march-mania-2021/overview