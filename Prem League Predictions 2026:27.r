install.packages("rlang")
packageVersion("rlang")


library(readxl)
library(dplyr)

install.packages("readxl")
library(readxl)


install.packages("dplyr")
library(dplyr)


file_path <- file.choose()
excel_sheets(file_path)   #now time to rename all the different sheets and sort


data_2526_overall <- read_excel(
  file_path,
  sheet = "25 26 overall"
)

head(data_2526_overall)
names(data_2526_overall)

data_2526_overall$season <- "2025/26"

data_2425_overall <- read_excel(
  file_path,
  sheet = "24 25 overall"
)
head(data_2425_overall) 
names(data_2425_overall)

data_2425_overall$season <- "2024/25"

data_2324_overall <- read_excel(
  file_path,
  sheet = "23 24 overall"
)
head(data_2324_overall)
names(data_2324_overall)

data_2324_overall$season <- "2023/24"

data_2526_home <- read_excel(
  file_path,
  sheet = "25 26 home"
)

head(data_2526_home)
names(data_2526_home)

data_2526_home$season <- "2025/26"

data_2425_home <- read_excel(
  file_path,
  sheet = "24 25 home"
)
head(data_2425_home)
names(data_2425_home)

data_2425_home$season <- "2024/25"

data_2324_home <- read_excel(
  file_path,
  sheet = "23 24 home"
)
head(data_2324_home)
names(data_2324_home)

data_2324_home$season <- "2023/24"

data_2526_away <- read_excel(
  file_path,
  sheet = "25 26 away"
)

head(data_2526_away)
names(data_2526_away)

data_2526_away$season <- "2025/26"

data_2425_away <- read_excel(
  file_path,
  sheet = "24 25 away"
)
head(data_2425_away)
names(data_2425_away)

data_2425_away$season <- "2024/25"

data_2324_away <- read_excel(
  file_path,
  sheet = "23 24 away"
)
head(data_2324_away)
names(data_2324_away)

data_2324_away$season <- "2023/24"

#lets bind all the data together 

all_data <- bind_rows(
  data_2324_overall %>% mutate(location = "overall"),
  data_2324_home %>% mutate(location = "home"),
  data_2324_away %>% mutate(location = "away"),
  
  data_2425_overall %>% mutate(location = "overall"),
  data_2425_home %>% mutate(location = "home"),
  data_2425_away %>% mutate(location = "away"),
  
  data_2526_overall %>% mutate(location = "overall"),
  data_2526_home %>% mutate(location = "home"),
  data_2526_away %>% mutate(location = "away")
)

head(all_data)
nrow(all_data)
table(all_data$season, all_data$location)

names(all_data)
length(all_data$season)
length(all_data$location)
nrow(all_data)

all_data$location <- rep(
  c("overall", "home", "away"),
  each = 20,
  times = 3
)

head(all_data, 25)

all_data$location <- rep(
  c("overall", "home", "away"),
  each = 20,
  times = 3
)

table(all_data$season, all_data$location)

#starting to model with xG are totals over the season e.g Ars has 77.49 xG overall so per game /38
all_data$xG_per_game <- all_data$xG/all_data$matches
all_data$xGA_per_game <- all_data$xGA/all_data$matches

head(all_data)

#now want to create table like: Team | Season | Home_xG | Home_xGA | Away_xG | Away_xGA

install.packages("tidyr")
library("tidyr")

#creating team strength

team_strength <- all_data %>%
  select(team, season, location, xG_per_game, xGA_per_game) %>%
  filter(location != "overall") %>%
  pivot_wider(
    names_from = location,
    values_from = c(xG_per_game, xGA_per_game),
    names_glue = "{location}_{.value}"
  )
# %>% means take whats on the left to the rigt so in this its saying take all data
#then filter and keep only rows where location isnt only "overall"

#pivot wider essentially puts home and way data together

View(team_strength)
nrow(team_strength)

#so now we got the data we want and need for lets say Arsenal Liverpool
#arenals home xg and home xGA plus lfcs away xG and xGA

head(team_strength)
nrow(team_strength)
names(team_strength)

#now we have to work out the league average as if lfc has 2.5 home xG what we comparing to

league_averages <- team_strength %>%
  group_by(season) %>%
  summarise(
    league_home_xG = mean(home_xG_per_game),
    league_away_xG = mean(away_xG_per_game),
    league_home_xGA = mean(home_xGA_per_game),
    league_away_xGA = mean(away_xGA_per_game)
  )

league_averages

#the good bit, calculting each teams attacking stengths/weaknesses
# add: home_attack_strength
#home_defence_strength
#away_attack_strength
#away_defence_strength



team_strength <- team_strength %>%
  left_join(league_averages, by = "season")  # left_join atches correct league avg to each teams season

names(team_strength)

# now team strenth in relation to league avg

team_strength <- team_strength %>%
  mutate(
    home_attack_strength = home_xG_per_game / league_home_xG,
    away_attack_strength = away_xG_per_game / league_away_xG,
    
    home_defence_strength = home_xGA_per_game / league_home_xGA,
    away_defence_strength = away_xGA_per_game / league_away_xGA
  )


head(team_strength)

#now to build actual match predictor 

# 1st create function to look at teams strengths

get_team_strength <- function(team_name, season_name) {
  
  team_strength %>%
    filter(
      team == team_name,
      season == season_name
    )
}
#example for arsenal 2023 2024
get_team_strength("Arsenal", "2023/24")

# to predict 2027 we're going to rank each season
# so 23/24 is 20 % 24/25 is 30% and 25/26 is 50%

weights <- data.frame(
  season = c("2023/24", "2024/25", "2025/26"),
  weight = c(0.20, 0.30, 0.50)
)

weights

#now lets create team_stength_weighted

team_strength_weighted <- team_strength %>%
  left_join(weights, by = "season")

team_strength_weighted <- team_strength_weighted %>%
  group_by(team) %>%
  summarise(
    home_xG = sum(home_xG_per_game * weight),
    away_xG = sum(away_xG_per_game * weight),
    home_xGA = sum(home_xGA_per_game * weight),
    away_xGA = sum(away_xGA_per_game * weight)
  )

head(team_strength_weighted)

#now calculating the strenths

league_weighted <- team_strength %>%
  left_join(weights, by = "season") %>%
  group_by(season) %>%
  summarise(
    league_home_xG = first(league_home_xG),
    league_away_xG = first(league_away_xG),
    league_home_xGA = first(league_home_xGA),
    league_away_xGA = first(league_away_xGA),
    weight = first(weight)
  ) %>%
  summarise(
    league_home_xG = sum(league_home_xG * weight),
    league_away_xG = sum(league_away_xG * weight),
    league_home_xGA = sum(league_home_xGA * weight),
    league_away_xGA = sum(league_away_xGA * weight)
  )

#comparing WEIGHTED data to overall league data

team_strength_weighted <- team_strength_weighted %>%
  mutate(
    home_attack_strength = home_xG / league_weighted$league_home_xG,
    away_attack_strength = away_xG / league_weighted$league_away_xG,
    home_defence_strength = home_xGA / league_weighted$league_home_xGA,
    away_defence_strength = away_xGA / league_weighted$league_away_xGA
  )

team_strength_weighted %>%
  select(
    team,
    home_attack_strength,
    away_attack_strength,
    home_defence_strength,
    away_defence_strength
  ) %>%
  arrange(desc(home_attack_strength))


#arranging the teams based on strength
team_strength_weighted %>%
  select(
    team,
    home_attack_strength,
    away_attack_strength,
    home_defence_strength,
    away_defence_strength
  ) %>%
  arrange(desc(home_attack_strength)) %>%
  print(n = 25)

ls(pattern = "weighted")

head(team_strength_weighted)

team_strength_weighted %>%
  select(
    team,
    home_attack_strength,
    away_attack_strength,
    home_defence_strength,
    away_defence_strength
  ) %>%
  arrange(desc(home_attack_strength)) %>%
  print(n = 25)

#now it is time but have to work out what 5 teams we're not using
unique(team_strength_weighted$team)

teams_2627 <- c(
  "Arsenal",
  "Aston Villa",
  "Bournemouth",
  "Brentford",
  "Brighton",
  "Chelsea",
  "Crystal Palace",
  "Everton",
  "Fulham",
  "Leeds",
  "Liverpool",
  "Manchester City",
  "Manchester United",
  "Newcastle United",
  "Nottingham Forest",
  "Sunderland",
  "Tottenham",
  "Ipswich",
  "Coventry City",
  "Hull City"
)

length(teams_2627)
sum(team_strength_weighted$team %in% teams_2627)

#hull/coventry data needed, lets do without then add

#predicting one match
team_strength_weighted %>%
  filter(team %in% c("Brentford", "Tottenham"))

#telling R that home team is brentfords nuber and away is spurs
home_team <- team_strength_weighted %>%
  filter(team == "Brentford")

away_team <- team_strength_weighted %>%
  filter(team == "Tottenham")

#calculating expected goals

expected_home_xG <- league_weighted$league_home_xG *
  home_team$home_attack_strength *
  away_team$away_defence_strength

expected_away_xG <- league_weighted$league_away_xG *
  away_team$away_attack_strength *
  home_team$home_defence_strength

expected_home_xG #brentford expected goals
expected_away_xG #spurs expected goals

#to get predicted score use poisson!
home_goals <- rpois(1, lambda = expected_home_xG)
away_goals <- rpois(1, lambda = expected_away_xG)

# probability of each possible score
home_goals
away_goals

#make this more useful rpois just simulated, dpois tells us prob of EACH goal

home_probs <- dpois(0:6, lambda = expected_home_xG)
away_probs <- dpois(0:6, lambda = expected_away_xG)

score_probs <- outer(home_probs, away_probs)

rownames(score_probs) <- 0:6
colnames(score_probs) <- 0:6

score_probs

#make this into win draw loss probs
# probability of a Brentford win
home_win_prob <- sum(score_probs[row(score_probs) > col(score_probs)])

# probability of a draw
draw_prob <- sum(score_probs[row(score_probs) == col(score_probs)])

# probability of a Tottenham win
away_win_prob <- sum(score_probs[row(score_probs) < col(score_probs)])

home_win_prob
draw_prob
away_win_prob


# improve score probability matrix

home_probs <- dpois(0:10, lambda = expected_home_xG)
away_probs <- dpois(0:10, lambda = expected_away_xG)

score_probs <- outer(home_probs, away_probs)

rownames(score_probs) <- 0:10
colnames(score_probs) <- 0:10


# match probabilities

home_win_prob <- sum(score_probs[row(score_probs) > col(score_probs)])

draw_prob <- sum(score_probs[row(score_probs) == col(score_probs)])

away_win_prob <- sum(score_probs[row(score_probs) < col(score_probs)])


# expected points

home_expected_points <- 3 * home_win_prob + draw_prob

away_expected_points <- 3 * away_win_prob + draw_prob


# display

data.frame(
  Team = c("Brentford", "Tottenham"),
  xG = c(expected_home_xG, expected_away_xG),
  Win_Probability = c(home_win_prob, away_win_prob),
  Draw_Probability = c(draw_prob, draw_prob),
  Expected_Points = c(home_expected_points, away_expected_points)
)

#put the table together and lets see...

current_teams <- c(
  "Arsenal",
  "Aston Villa",
  "Bournemouth",
  "Brentford",
  "Brighton",
  "Burnley",
  "Chelsea",
  "Crystal Palace",
  "Everton",
  "Fulham",
  "Leeds",
  "Liverpool",
  "Manchester City",
  "Manchester United",
  "Newcastle United",
  "Nottingham Forest",
  "Sunderland",
  "Tottenham",
  "West Ham",
  "Wolverhampton Wanderers"
)
#need to take relagated teams out and new ones back in 
team_strength_2026 <- team_strength_weighted %>%
  filter(team %in% current_teams)

nrow(team_strength_2026)

#make all 380 fixtures

fixtures <- expand.grid(
  home_team = current_teams,
  away_team = current_teams
) %>%
  filter(home_team != away_team)

#noww nrow should be 380
nrow(fixtures)

#home strengths and away strengths

fixtures <- fixtures %>%
  left_join(
    team_strength_2026 %>%
      select(
        team,
        home_attack_strength,
        home_defence_strength,
        away_attack_strength,
        away_defence_strength
      ),
    by = c("home_team" = "team")
  ) %>%
  rename(
    home_attack = home_attack_strength,
    home_defence = home_defence_strength,
    home_away_attack = away_attack_strength,
    home_away_defence = away_defence_strength
  )

fixtures <- fixtures %>%
  left_join(
    team_strength_2026 %>%
      select(
        team,
        home_attack_strength,
        home_defence_strength,
        away_attack_strength,
        away_defence_strength
      ),
    by = c("away_team" = "team")
  ) %>%
  rename(
    away_home_attack = home_attack_strength,
    away_home_defence = home_defence_strength,
    away_attack = away_attack_strength,
    away_defence = away_defence_strength
  )

#that all created table for home attack, home defence, away attack, away defence 

#expected goals per match
fixtures <- fixtures %>%
  mutate(
    expected_home_xG =
      league_weighted$league_home_xG *
      home_attack *
      away_defence,
    
    expected_away_xG =
      league_weighted$league_away_xG *
      away_attack *
      home_defence
  )

head(fixtures)

summary(fixtures$expected_home_xG)
summary(fixtures$expected_away_xG)

head(
  fixtures %>%
    select(
      home_team,
      away_team,
      expected_home_xG,
      expected_away_xG
    )
)


#predict match function!

predict_match <- function(home_xG, away_xG) {
  
  home_probs <- dpois(0:6, lambda = home_xG)
  away_probs <- dpois(0:6, lambda = away_xG)
  
  score_probs <- outer(home_probs, away_probs)
  
  home_win <- sum(score_probs[row(score_probs) > col(score_probs)])
  
  draw <- sum(diag(score_probs))
  
  away_win <- sum(score_probs[row(score_probs) < col(score_probs)])
  
  home_points <- (home_win * 3) + draw
  away_points <- (away_win * 3) + draw
  
  return(
    c(
      home_win = home_win,
      draw = draw,
      away_win = away_win,
      home_points = home_points,
      away_points = away_points
    )
  )
}

predict_match(1.864581, 1.426539)
home_win_prob + draw_prob + away_win_prob # close to 1, exactly what we want

#actual season prediction

fixtures <- fixtures %>%
  mutate(
    prediction = purrr::map2(
      expected_home_xG,
      expected_away_xG,
      predict_match
    )
  )

fixtures <- fixtures %>%
  mutate(
    home_win_prob = sapply(prediction, function(x) x["home_win"]),
    draw_prob = sapply(prediction, function(x) x["draw"]),
    away_win_prob = sapply(prediction, function(x) x["away_win"]),
    home_expected_points = sapply(prediction, function(x) x["home_points"]),
    away_expected_points = sapply(prediction, function(x) x["away_points"])
  )

head(fixtures)

#simulate all 380 games and make the table

home_table <- fixtures %>%
  group_by(home_team) %>%
  summarise(
    team = first(home_team),
    expected_points = sum(home_expected_points)
  )

away_table <- fixtures %>%
  group_by(away_team) %>%
  summarise(
    team = first(away_team),
    expected_points = sum(away_expected_points)
  )

predicted_table <- bind_rows(home_table, away_table) %>%
  group_by(team) %>%
  summarise(
    expected_points = sum(expected_points)
  ) %>%
  arrange(desc(expected_points))

predicted_table

#now lets add position column
predicted_table <- predicted_table %>%
  mutate(
    position = row_number()
  ) %>%
  select(
    position,
    team,
    expected_points
  )

predicted_table

#fixing a few part after re-uploading the code
names(all_data)
head(all_data)

unique(all_data$season)

table(all_data$season)

head(all_data$team, 25)