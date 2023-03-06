# Internal functions using during the cleaning and data analysis

cleaningFun <- function(data) {

  data$home <- data$home %>% as.numeric()
  data$away <- data$away %>% as.numeric()
  A <- data %>% group_by(home_teams, away_teams) %>% mutate(n = n()) %>%  filter(n==2) %>% summarise(across(c(home, away), sum))
  B <- data %>% group_by(home_teams, away_teams) %>% mutate(n = n()) %>% filter(n==1)
  cleanedData <- rbind(A, B) %>% select(-n) %>% mutate(result = ifelse(home > away, "H", ifelse(away > home, "A", "D")))
  cleanedData

}

ConvFun <- function(data) {

  data[data == "H"] <- 1
  data[data == "D"] <- 2
  data[data == "A"] <- 3

  for (col in colnames(data)) {

    data[, col] <- as.numeric(data[, col])

  }
  data <- as.matrix(data)
  data
}


levelplotFun <- function(data){

  plt <- levelplot(t(data[c(nrow(data):1), ]), scales=list(x=list(rot=45)),
                   col.regions = colorRampPalette(c("green", "yellow", "red"))(100),
                   colorkey=FALSE,
                   xlab = "Away Team", ylab = "Home Team")

  plt

}
