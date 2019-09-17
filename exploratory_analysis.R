library(dplyr)

train_data <- read.csv("bigquery-geotab-intersection-congestion/train.csv")
train <- sample(train_data$RowId, 50000)
train_data <- train_data[train_data$RowId %in% train, ]
atlanta <- filter(train_data, City == "Atlanta")

library(ggplot2)
g <- ggplot(atlanta, aes(Latitude, Longitude))
g + geom_point(aes(col = log(TotalTimeStopped_p50))) + scale_color_gradient(low = "blue", high = "red")
g + geom_point(aes(size = TotalTimeStopped_p50))
g + geom_point(aes(size = TotalTimeStopped_p50)) + coord_cartesian(xlim = c(33.72, 33.8), ylim = c(-84.43, -84.33))

# сделать анимацию по часам!!!!
# внести данные о поворотах, если таковые имеются: левый, правый или прямо! Можно вычислить из направлений улиц!
# логика такова: если угол при смене направления увеличивается и увеличение менее 180 градусов
# то єто левій поворот
# если угол уменьшается или увеличение угла более 180 градусов то єто правый поворот

tmp <- which(as.character(atlanta$EntryStreetName) == as.character(atlanta$ExitStreetName) & atlanta$EntryHeading != atlanta$ExitHeading)
