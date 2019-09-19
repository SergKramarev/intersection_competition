library(dplyr)

train_data <- read.csv("bigquery-geotab-intersection-congestion/train.csv", stringsAsFactors = FALSE)
train <- sample(train_data$RowId, 50000)
train_data <- train_data[train_data$RowId %in% train, ]
atlanta <- filter(train_data, City == "Atlanta")

library(ggplot2)
g <- ggplot(atlanta, aes(Latitude, Longitude))
g + geom_point(aes(col = log(TotalTimeStopped_p50))) + scale_color_gradient(low = "blue", high = "red")
g + geom_point(aes(size = TotalTimeStopped_p50))
g + geom_point(aes(size = TotalTimeStopped_p50)) + coord_cartesian(xlim = c(33.72, 33.8), ylim = c(-84.43, -84.33))




tmp <- which(as.character(atlanta$EntryStreetName) == as.character(atlanta$ExitStreetName) & atlanta$EntryHeading != atlanta$ExitHeading)

heading <- data.frame(heading = c("E", "N", "W", "S", "NE", "NW", "SE", "SW"), degree = c(0, 90, 180, 270, 45, 135, 315, 225))

#mappig data with directon in degrees
atlanta$EntryHeading <- ifelse(atlanta$EntryHeading == "N", 90,
                               ifelse(atlanta$EntryHeading == "W", 180,
                                      ifelse(atlanta$EntryHeading == "E", 0,
                                             ifelse(atlanta$EntryHeading == "S", 270,
                                                    ifelse(atlanta$EntryHeading == "NE", 45,
                                                           ifelse(atlanta$EntryHeading == "NW", 135,
                                                                  ifelse(atlanta$EntryHeading == "SW", 225,
                                                                         ifelse(atlanta$EntryHeading == "SE", 315, "something wrong"))))))))

atlanta$ExitHeading <- ifelse(atlanta$ExitHeading == "N", 90,
                               ifelse(atlanta$ExitHeading == "W", 180,
                                      ifelse(atlanta$ExitHeading == "E", 0,
                                             ifelse(atlanta$ExitHeading == "S", 270,
                                                    ifelse(atlanta$ExitHeading == "NE", 45,
                                                           ifelse(atlanta$ExitHeading == "NW", 135,
                                                                  ifelse(atlanta$ExitHeading == "SW", 225,
                                                                         ifelse(atlanta$ExitHeading == "SE", 315, "something wrong"))))))))

atlanta$EntryHeading <- as.integer(atlanta$EntryHeading)
atlanta$ExitHeading <- as.integer(atlanta$ExitHeading)

# Determining turn type between left, right, strainght, and u-turn
atlanta$TurnType <- ifelse(atlanta$ExitHeading - atlanta$EntryHeading > 0 & atlanta$ExitHeading - atlanta$EntryHeading < 180, "left",
                           ifelse(atlanta$ExitHeading == atlanta$EntryHeading, "straight",
                                  ifelse(atlanta$ExitHeading - atlanta$EntryHeading < 0 | atlanta$ExitHeading - atlanta$EntryHeading > 180, "right",
                                         ifelse(atlanta$ExitHeading - atlanta$EntryHeading == 180, "u-turn", "something went wrong"))))


# determining distribution type

for_distr <- select(atlanta, c(1, 13:17))
percentiles <- c(0.2, 0.4, 0.5, 0.6, 0.8)

# For motion chart
atlanta_weekday <- filter(atlanta, Weekend == 0, Month == 6)
tmp <- atlanta_weekday %>% group_by(Path, Hour) %>% summarise(n = n()) %>% arrange(desc(n))
# When you go strainght some pathes is the same but intersection are different. Than is why we need to
# add intersection identifier for path 
atlanta_weekday$Path.Int <- paste(atlanta_weekday$Path, as.character(atlanta_weekday$IntersectionId), sep = "_")
atlanta_weekday$Hour <- paste(as.character(atlanta_weekday$Hour), "00", sep = ".")
library(googleVis)
m <- gvisMotionChart(atlanta_weekday, idvar = "Path.Int", timevar = "Hour", xvar = "Latitude", yvar = "Longitude", sizevar = "TotalTimeStopped_p50", colorvar = "TurnType", date.format = "%H", options = list(height = 1000, width = 1400))
# привести данные о времени и о месяце в нормальное состояние, так чтоб это была одна строка в формате времени, но остальные строки остались на всякий случай
# подумать над распределением. Сделать отдельную табличку и понять распределение. Скорее всего распределение везде будет одинаковім
# но не обязательно!

distr <- data.frame(percentiles = c(0.2, 0.4, 0.5, 0.6, 0.8))
distr <- data.frame()
for (i in 1:nrow(atlanta_weekday)) {
    tmp <- data.frame(percentiles = c(0.2, 0.4, 0.5, 0.6, 0.8))
    tmp[1:5, 2] <- unlist(atlanta_weekday[i, 13:17])
    distr <- rbind(distr, tmp)
}

install.packages("rriskDistributions")
library(rriskDistributions)
get.norm.par(p = c(0.2, 0.4, 0.5, 0.6, 0.8), q = c(49, 53, 54, 55, 59))

# Критерий Шапиро-Уилка для проверки нормальности. Посмотреть можно ли его посчитать и опрееделить нормально ли распределение


for (i in 1:nrow(atlanta_weekday)){
    par <- get.norm.par(p = c(0.2, 0.4, 0.5, 0.6, 0.8), q = unlist(atlanta_weekday[i, 13:17]), show.output = FALSE)
    atlanta_weekday[i , "mean_TimeStopped"] <- par[1]
    atlanta_weekday[i, "sd_TimeStopped"] <- par[2]
    
    
}



 
 