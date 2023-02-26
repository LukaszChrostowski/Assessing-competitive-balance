library(rlist)
library(dplyr)
library(tidyr)

final_table00_01 <- ssn00_01 %>% select(c(home_teams, away_teams, result))

final_table00_01 <- final_table00_01 %>% pivot_wider(names_from = away_teams, values_from = result) %>% as.data.frame()
col_order <- final_table$home_teams
rownames(final_table00_01) <- df_wider$home_teams
final_table00_01$home_teams <- NULL
final_table00_01 <- final_table00_01[, col_order]

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
