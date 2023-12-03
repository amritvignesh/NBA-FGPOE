library(nbastatR)
library(dplyr)
library(xgboost)
library(caret)
library(hoopR)
library(gt)
library(ggplot2)
library(ggrepel)
library(png)
library(ggimage)
library(ggpath)
library(vip)
library(ggpmisc)
library(gtExtras)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 10)

shots <- data.frame()


shots_21 <- teams_shots(all_active_teams = TRUE, season_types = "Regular Season", seasons = 2021)
shots_22 <- teams_shots(all_active_teams = TRUE, season_types = "Regular Season", seasons = 2022)
shots_23 <- teams_shots(all_active_teams = TRUE, season_types = "Regular Season", seasons = 2023)
shots_24 <- teams_shots(all_active_teams = TRUE, season_types = "Regular Season", seasons = 2024)

shots <- rbind(shots_21, shots_22, shots_23, shots_24)

unique(shots$typeAction)

shot_types <- c("Layup", "Dunk", "Jump", "Hook", "Pull", "Putback", "Tip", "Fadeaway", "Step Back", "Reverse", "Driving", "Floating", "Cutting", "Turnaround")
                
for (type in shot_types) {
  shots[[type]] <- grepl(type, shots$typeAction, ignore.case = TRUE)
}

unique(shots$zoneBasic)

shots <- shots %>%
  mutate(zoneBasic = case_when(zoneBasic == 'Above the Break 3' ~ 'Other Three', zoneBasic == 'Restricted Area' ~ 'Restricted Paint', TRUE ~ zoneBasic))


zone_types <- c("Paint", "Corner", "Mid-Range", "Other Three", "Backcourt") 

for (type in zone_types) {
  shots[[type]] <- grepl(type, shots$zoneBasic, ignore.case = TRUE)
}

shots <- shots %>%
  mutate(made_shot = ifelse(typeEvent == "Made Shot", 1, 0), points = as.numeric(substring(typeShot, 1, 1)), time = minutesRemaining * 60 + secondsRemaining) %>%
  select(season = yearSeason, idPlayer, player = namePlayer, idTeam, team = nameTeam, made_shot, qtr = numberPeriod, x = locationX, y = locationY, time, dist = distanceShot, Layup, Dunk, Jump, Hook, Pullup = Pull, Putback, Tip, Fadeaway, Step_Back = "Step Back", Reverse, Drive = Driving, Float = Floating, Cut = Cutting, Paint, Corner, Mid_Range = "Mid-Range", Other_Three = "Other Three", Backcourt, points)

factor_data <- shots %>%
  select(-season, -idPlayer, -player, -idTeam, -team)

factor_data[, c(2, 7:24)] <- lapply(factor_data[, c(2, 7:24)], as.factor)

dummy <- dummyVars(" ~ .", data = factor_data)
shots_data <- data.frame(predict(dummy, newdata = factor_data))

shots_data <- cbind(shots, shots_data) 

shots_data <- shots_data[,-c(7,12:29,31,39:42,79)]

shots_data <- shots_data[,c(1:10,12:54,11)]

xgboost_train <- shots_data %>%
  filter(season != 2024)

xgboost_test <- shots_data %>%
  filter(season == 2024)

labels_train <- as.matrix(xgboost_train[,6])
xgboost_trainfinal <- as.matrix(xgboost_train[, c(7:53)])
xgboost_testfinal <- as.matrix(xgboost_test[, c(7:53)])

fgpoe_model <- xgboost(data = xgboost_trainfinal, label = labels_train, nrounds = 100, objective = "binary:logistic", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

vip(fgpoe_model)
vi(fgpoe_model)
summary(fgpoe_model)

fgp_predict <- predict(fgpoe_model, xgboost_testfinal)
fgp_actual <- as.matrix(xgboost_test[,6])
postResample(fgp_predict, fgp_actual)

fgp_predictions <- as.data.frame(
  matrix(predict(fgpoe_model, as.matrix(shots_data[,c(7:53)])))
)

all_stats <- cbind(shots_data, fgp_predictions) %>%
  select(idPlayer, season, player, idTeam, team, points, made_shot, pred_made_shot = V1)

all_stats <- all_stats %>%
  group_by(idPlayer, season, player) %>%
  summarize(total_pts = sum(points * made_shot), total_pred_points = sum(points * pred_made_shot))

bref_stats <- bref_players_stats(2021:2024)

bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "Reggie Bullock")] <- 203493
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "TJ Leaf")] <- 1628388
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "Cameron Reynolds")] <- 1629244
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "Charles Brown Jr.")] <- 1629718
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "Xavier Tillman Sr.")] <- 1630214
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "Tre Scott")] <- 1630286
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "Jeff Dowtin")] <- 1630288
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "M.J. Walker")] <- 1630640
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "John Butler")] <- 1631219
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "Jermaine Samuels")] <- 1631257
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "A.J. Green")] <- 1631260
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "Jeenathan Williams")] <- 1631466
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "Gregory Jackson II")] <- 1641713
bref_stats$idPlayerNBA[which(bref_stats$namePlayer == "Craig Porter Jr.")] <- 1641854

bref_stats <- bref_stats %>%
  select(season = yearSeason, idPlayer = idPlayerNBA, games = countGames)

all_stats <- left_join(all_stats, bref_stats, by = c("season", "idPlayer"))

all_stats <- all_stats %>%
  mutate(total_pts = total_pts/games, total_pred_points = total_pred_points/games) %>%
  select(season, player, fg_ppg = total_pts, pred_fg_ppg = total_pred_points, games)

stats_2024 <- all_stats %>%
  filter(season == 2024)

threshold <- 0.5 * max(stats_2024$games)

stats_2024 <- stats_2024 %>%
  filter(games >= threshold) %>%
  filter(fg_ppg >= 15.0) %>%
  mutate(fgpoe = fg_ppg - pred_fg_ppg)

ids <- unique(stats_2024$idPlayer)
for(id in ids) {
  stats_2024$headshot_link[which(stats_2024$idPlayer == id)] <- nba_playerheadshot(id)
}

stats_2024 %>%
  ggplot(aes(x = fg_ppg, y = pred_fg_ppg)) +
  geom_hline(yintercept = mean(stats_2024$pred_fg_ppg), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(stats_2024$fg_ppg), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_from_path(aes(x = fg_ppg, y = pred_fg_ppg, path = headshot_link), width = 0.1, height = 0.1) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  labs(x = "Average Field Goal PPG",
       y = "Average Predicted Field Goal PPG",
       title = "Predicting Field Goal PPG and Quantifying FGPOE (Players With 15+ FG PPG)",
       caption = "Amrit Vignesh") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))

gtdata <- stats_2024 %>%
  ungroup() %>%
  mutate(fg_ppg = round(fg_ppg, 2)) %>%
  mutate(pred_fg_ppg = round(pred_fg_ppg, 2)) %>%
  mutate(fgpoe = round(fgpoe, 2)) %>%
  select(player, fg_ppg, pred_fg_ppg, fgpoe) 

table1 <- gtdata %>%
  arrange(-fgpoe) %>%
  filter(row_number() <= 10) %>%
  ungroup()

table2 <- gtdata %>%
  arrange(fgpoe) %>%
  filter(row_number() <= 10) %>%
  ungroup()

t1 <- table1 %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, fg_ppg, pred_fg_ppg, fgpoe)
  ) %>%
  data_color(
    columns = fgpoe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    fg_ppg = md("**FG PPG**"),
    pred_fg_ppg = md("**Pred. FG PPG**"),
    fgpoe = md("**FGPOE**"),
  ) %>%
  tab_header(
    title = md("**2023-24 NBA FGPOE (Field Goal Points Per Game Over Expected) Up Till 11/27 Games**"),
    subtitle = "Trained Data From 2020-21 to 2022-23 Season"
  ) 

t2 <- table2 %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, fg_ppg, pred_fg_ppg, fgpoe)
  ) %>%
  data_color(
    columns = fgpoe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL,
      reverse = TRUE
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    fg_ppg = md("**FG PPG**"),
    pred_fg_ppg = md("**Pred. FG PPG**"),
    fgpoe = md("**FGPOE**"),
  ) %>%
  tab_header(
    title = md("**2023-24 NBA FGPOE (Field Goal Points Per Game Over Expected) Up Till 11/27 Games**"),
    subtitle = "Trained Data From 2020-21 to 2022-23 Season"
  ) 

