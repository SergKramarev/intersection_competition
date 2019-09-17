train_data <- read.csv("bigquery-geotab-intersection-congestion/train.csv")
train <- sample(train_data$RowId, 50000)
train_data <- train_data[train_data$RowId %in% train, ]
