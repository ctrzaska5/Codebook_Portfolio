##Load Packages
library(tidyverse)
library(readr)

##Load raw mlb_teams.rda data set
load(file = "mlb_teams.rda")

##Clean and standardize missing values
mlb_teams[mlb_teams == ""] <- "N/A"
mlb_teams[is.na(mlb_teams)] <- "N/A"
mlb_teams[mlb_teams == 'N/A'] <- NA



##Remove all defense-related variables
mlb_teams <- select(mlb_teams, -fielding_percentage,
                    -opponents_runs_scored,
                    -earned_runs_allowed,
                    -earned_run_average,
                    -complete_games,
                    -shutouts,
                    -saves,
                    -outs_pitches,
                    -hits_allowed,
                    -walks_allowed,
                    -homeruns_allowed,
                    -strikeouts_by_pitchers,
                    -double_plays,
                    -errors)

##Remove unneccessary variables related to home games and home attendance 
mlb_teams <- select(mlb_teams, -home_games)
mlb_teams <- select(mlb_teams, -home_attendance)

## Create new variable representing winning percentage
mlb_teams<- mlb_teams|>
  mutate(
    mlb_teams, winning_per = wins/games_played
  )

##Create Singles Variable
mlb_teams <- mlb_teams %>%
  mutate(singles = (hits - (doubles + triples + homeruns)))

##Clean Data by Converting Character Variables to Numeric
mlb_teams <- mlb_teams %>%
  mutate_at(vars(hits, walks, batters_hit_by_pitch, at_bats), as.numeric)

##Create On base Percentage Variable
mlb_teams <- mlb_teams |>
  rowwise() |>
  mutate(on_base_per = sum(c(hits, walks, batters_hit_by_pitch), na.rm = TRUE) / at_bats)

#Create New Data Frame that Creates the Average On Base Percentage for each year
avg_obp <- 
  mlb_teams |>
  group_by(year) |>
  summarize(avg_obp = mean(on_base_per))

## Merge with main mlb_teams data frame
mlb_teams <- merge(mlb_teams, avg_home_obp, by = "year", suffixes = c("", "_avg"))



##Create new factor variable identifying whether a team's average on base percentage was higher than the league's average for the year
mlb_teams <- mlb_teams |>
  mutate(obp_status = ifelse(on_base_per > avg_obp, "Over", "Under"))

mlb_teams$obp_status <- factor(mlb_teams$obp_status, levels = c("Under", "Over"))


##Create slugging percentage variable
mlb_teams <- mlb_teams |>
  mutate(slugging_perc = ((singles * 1) + (doubles * 2) + (triples * 3) + (homeruns * 4)) / at_bats)

##Create data frame that calculates average league slugging percentage by year
avg_slugging <- 
  mlb_teams |>
  group_by(year) |>
  summarize(avg_slugging = mean(slugging_perc))

##Merge the slugging percentage data frame with main mlb_teams data frame
mlb_teams <- merge(mlb_teams, avg_slugging, by = "year", suffixes = c("", "_avg"))


##Create variable identifying whether a team's slugging percentage was above or below league average for that year
mlb_teams <- mlb_teams |>
  mutate(slug_status = ifelse(slugging_perc > avg_slugging, "Over", "Under"))

##Convert slugging status variable to a factor variable
mlb_teams$slug_status <- factor(mlb_teams$slug_status, levels = c("Under", "Over"))



##Create ops variable
mlb_teams <- mlb_teams|>
  mutate(ops = on_base_per + slugging_perc)

##Create batting average variable
mlb_teams <- mlb_teams|>
  mutate(batting_avg = hits/at_bats)

##Create new ops data frame that calculates the average league ops for each year
avg_ops <- 
  mlb_teams |>
  group_by(year) |>
  summarize(avg_ops = mean(ops))

##Merge the ops data frame with the main mlb_teams data frame
mlb_teams <- merge(mlb_teams, avg_ops, by = "year", suffixes = c("", "_avg"))

##Create new factor variable identifying whether a team's average ops was higher than the league's average for the year
mlb_teams <- mlb_teams %>%
  mutate(ops_status = ifelse(ops > avg_ops, "Over", "Under"))

mlb_teams$ops_status <- factor(mlb_teams$ops_status, levels = c("Under", "Over"))


##Convert several variables to be factor variables
mlb_teams$league_id <- factor(mlb_teams$league_id, levels = c("NL", "AL"))
mlb_teams$division_winner <- factor(mlb_teams$division_winner, levels = c("Y", "N"))
mlb_teams$wild_card_winner <- factor(mlb_teams$wild_card_winner, levels = c("Y", "N"))
mlb_teams$leauge_winner <- factor(mlb_teams$league_winner, levels = c("Y", "N"))
mlb_teams$world_series_winner <- factor(mlb_teams$world_series_winner, levels = c("Y", "N"))
mlb_teams$division_id <- factor(mlb_teams$division_id, levels = c("C","E","W"))

##Convert several variables to be numeric variables as opposed to character variables
mlb_teams <- 
  mlb_teams |>
  mutate_at(vars(games_played, 
                 wins, 
                 losses,
                 runs_scored,
                 at_bats,
                 strikeouts_by_batters,
                 stolen_bases,
                 caught_stealing,
                 sacrifice_flies,
                 hits,
                 singles,
                 doubles,
                 triples,
                 homeruns,
                 year,
                 walks,
                 batters_hit_by_pitch,
                 winning_per
                 ), as.numeric)



##Create new batting average data frame that calculates the average batting average for each year
avg_ba <- 
  mlb_teams |>
  group_by(year) |>
  summarize(avg_ba = mean(batting_avg))

##Merge the data frame with the main mlb_teams dataframe
mlb_teams <- merge(mlb_teams, avg_ba, by = "year", suffixes = c("", "_avg"))

##Create new variable that identifies whether a team's total batting average was above or below the league average for each year 
mlb_teams <- mlb_teams |>
  mutate(ba_status = ifelse(batting_avg > avg_ba, "Over", "Under"))

##Convert the new variable to a factor variable
mlb_teams$ba_status <- factor(mlb_teams$ba_status, levels = c("Under", "Over"))



##Follow each of these steps in the order they appear to ensure correct order of variables. 
mlb_teams <-
  mlb_teams|>
  relocate(
    team_name,
    .before = league_id)

mlb_teams <-
  mlb_teams|>
  relocate(
    ball_park,
    home_attendance,
    .before = league_id)

mlb_teams <-
  mlb_teams|>
  relocate(
    winning_per,
    .before = division_winner)

mlb_teams <-
  mlb_teams|>
  relocate(
    wild_card_winner,
    .before = division_winner)

mlb_teams <-
  mlb_teams|>
  relocate(
    singles,
    .before = doubles)

mlb_teams <-
  mlb_teams|>
  relocate(
    batting_avg,
    .before = avg_ba)

mlb_teams <-
  mlb_teams|>
  relocate(
    ops,
    .before = avg_ops)

mlb_teams <-
  mlb_teams|>
  relocate(
    slugging_perc,
    .before = avg_slugging)

mlb_teams <-
  mlb_teams|>
  relocate(
    on_base_per,
    .before = avg_obp)

mlb_teams <-
  mlb_teams|>
  relocate(
    on_base_per,
    avg_obp,
    obp_status,
    slugging_perc,
    avg_slugging,
    slug_status,
    .before = ops)


##Write the final product as a new csv files a
write.csv(mlb_teams, "mlb_teams_final.csv", row.names = FALSE)

##Save final product to current directory
save(mlb_teams, file = "mlb_teams_final.RData")





