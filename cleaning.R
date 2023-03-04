library(rlist)
library(dplyr)
library(tidyr)

cleaningFun <- function(data) {

  data$home <- data$home %>% as.numeric()
  data$away <- data$away %>% as.numeric()
  A <- data %>% group_by(home_teams, away_teams) %>% mutate(n = n()) %>%  filter(n==2) %>% summarise(across(c(home, away), sum))#%>% filter(n==2)
  B <- data %>% group_by(home_teams, away_teams) %>% mutate(n = n()) %>% filter(n==1)
  cleanedData <- rbind(A, B) %>% select(-n) %>% mutate(result = ifelse(home > away, "H", ifelse(away > home, "A", "D")))
  cleanedData

}


ssn01_02 <- cleaningFun(ssn01_02)
ssn13_14 <- cleaningFun(ssn13_14)
ssn14_15 <- cleaningFun(ssn14_15)
ssn15_16 <- cleaningFun(ssn15_16)
ssn16_17 <- cleaningFun(ssn16_17)
ssn17_18 <- cleaningFun(ssn17_18)
ssn18_19 <- cleaningFun(ssn18_19)
ssn19_20 <- cleaningFun(ssn19_20)



A <- list(ssn98_99, ssn99_00,ssn00_01, ssn01_02, ssn02_03, ssn03_04, ssn04_05, ssn05_06, ssn06_07, ssn07_08,
          ssn08_09, ssn09_10, ssn10_11, ssn11_12, ssn12_13, ssn13_14, ssn14_15, ssn15_16,
          ssn16_17, ssn17_18, ssn18_19, ssn19_20, ssn20_21, ssn21_22)

B <- list()


for (df in A) {

  final_table <- df %>% select(c(home_teams, away_teams, result))

  final_table <- final_table %>% pivot_wider(names_from = away_teams, values_from = result) %>% as.data.frame()
  col_order <- final_table$home_teams
  rownames(final_table) <- final_table$home_teams
  final_table$home_teams <- NULL
  final_table <- final_table[, col_order]

  B <- list.append(B, final_table)

}
