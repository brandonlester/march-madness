kaggle_data_folder <- "data/ncaam-march-mania-2021/"
kaggle_data_files <- list.files(kaggle_data_folder)

for (i in 1:length(kaggle_data_files)) {
  df_name <- stringr::str_remove(kaggle_data_files[i], ".csv")
  df <- readr::read_csv(paste0(kaggle_data_folder,kaggle_data_files[i]))
  assign(df_name, df)
  rm(df_name, df)
}
