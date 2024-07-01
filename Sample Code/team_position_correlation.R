library(tidyverse)
library(rstan)

points <- read_csv("best_ball/data/points.csv")
points <- points %>%
  rename(player_name = player_display_name) %>%
  arrange(season)
points



# looking at correlation between teammate
points <- points %>%
    rename(ppr = fantasy_points_ppr)

# get positional tibbles
qbs <- points %>%
    filter(position == "QB") %>%
    group_by(player_name, recent_team) %>%
    summarise(total_ppr = sum(ppr)) %>%
    group_by(recent_team) %>%
    arrange(desc(total_ppr)) %>%
    slice_head(n = 1)
qbs

qb_factor <- as.factor(qbs$player_name)

qb <- points %>% 
  filter(position == "QB") %>% 
  group_by(recent_team, week) %>%
  select(player_name, ppr, week, recent_team, opponent_team)  %>%
  ungroup() %>%
  filter(player_name %in% qb_factor) %>%
  arrange(recent_team)
qb[is.na(qb)] <- 0
qb

get_wrs <- points %>%
    filter(position == "WR") %>%
    group_by(player_name, recent_team) %>%
    summarise(total_ppr = sum(ppr)) %>%
    ungroup() %>%
    arrange(recent_team, desc(total_ppr))
get_wrs

wr1_list <- get_wrs %>%
    group_by(recent_team) %>%
    slice_head(n = 1)

wr2_list <- get_wrs %>% 
    group_by(recent_team) %>%
    slice_head(n = 2) %>%
    slice_tail(n = 1)

wr1_factor <- factor(wr1_list$player_name)
wr2_factor <- factor(wr2_list$player_name)


wr1 <- points %>% 
  filter(position == "WR") %>% 
  filter(player_name %in% wr1_factor) %>%
  select(ppr, week, recent_team, player_name, opponent_team)
wr1[is.na(wr1)] <- 0
print(n = 200, wr1)

wr2 <- points %>% 
  filter(position == "WR") %>% 
  filter(player_name %in% wr2_factor) %>%
  select(ppr, week, recent_team, player_name, opponent_team)
wr2[is.na(wr2)] <- 0
print(n = 200, wr2)

rb1 <- points %>%
    filter(position == "RB") %>%
    group_by(player_name, recent_team) %>%
    summarise(total_ppr = sum(ppr)) %>%
    group_by(recent_team) %>%
    arrange(desc(total_ppr)) %>%
    slice_head(n = 1)
rb1

rb2 <- points %>%
  filter(position == "RB") %>%
  group_by(player_name, recent_team) %>%
  summarise(total_ppr = sum(ppr)) %>%
  group_by(recent_team) %>%
  arrange(desc(total_ppr)) %>%
  slice_head(n = 2) %>%
  slice_tail(n = 1)
rb2


rb1_factor <- as.factor(rb1$player_name)
rb2_factor <- as.factor(rb2$player_name)

rb1 <- points %>% 
    filter(position == "RB") %>% 
    select(player_name, ppr, week, recent_team, opponent_team) %>%
    filter(player_name %in% rb1_factor)
rb1[is.na(rb1)] <- 0
print(n = 100, rb1)

rb2 <- points %>% 
  filter(position == "RB") %>% 
  select(player_name, ppr, week, recent_team, opponent_team) %>%
  filter(player_name %in% rb2_factor)
rb2[is.na(rb2)] <- 0
print(n = 100, rb2)

tes <- points %>%
    filter(position == "TE") %>%
    group_by(recent_team, player_name) %>%
    summarise(total_ppr = sum(ppr)) %>%
    group_by(recent_team) %>%
    arrange(desc(total_ppr)) %>%
    slice_head(n = 1)
tes

te1_factor <- as.factor(tes$player_name)

te <- points %>% 
  filter(position == "TE") %>% 
  select(player_name, ppr, week, recent_team, opponent_team) %>%
  filter(player_name %in% te1_factor)
print(n = 100, te)



#==============================================================

matchups <- points %>%
    select(week, recent_team, opponent_team) %>%
    group_by(recent_team, week) %>%
    arrange(recent_team, week) %>%
    slice_head(n = 1)
print(n = 100, matchups)

q <- matchups %>%



team_factor = as.factor(unique(points$recent_team))
position_df <- tibble (
  team = character(), 
  week = numeric(), 
  qb_ppr = numeric(), 
  rb1_ppr = numeric(), 
  rb2_ppr = numeric(), 
  wr1_ppr = numeric(), 
  wr2_ppr = numeric(), 
  te_ppr = numeric(), 
  opp_qb_ppr = numeric(), 
  opp_rb1_ppr = numeric(), 
  opp_rb2_ppr = numeric(), 
  opp_wr1_ppr = numeric(), 
  opp_wr2_ppr = numeric(), 
  opp_te_ppr = numeric()
)
position_df

for (t in 1:length(team_factor)) {
    for (w in 1:17) {
        qb_week_ppr <- qb %>%
            filter(week == w & recent_team == team_factor[t])
        if (dim(qb_week_ppr)[1] == 0) {
            qb_week_ppr <- 0
        } else {
            qb_week_ppr <- pull(qb_week_ppr[2])
        }
        
        rb1_week_ppr <- rb1 %>%
          filter(week == w & recent_team == team_factor[t])
        if (dim(rb1_week_ppr)[1] == 0) {
          rb1_week_ppr <- 0
        } else {
          rb1_week_ppr <- pull(rb1_week_ppr[2])
        }

        rb2_week_ppr <- rb2 %>%
          filter(week == w & recent_team == team_factor[t])
        if (dim(rb2_week_ppr)[1] == 0) {
          rb2_week_ppr <- 0
        } else {
          rb2_week_ppr <- pull(rb2_week_ppr[2])
        }
        
        wr1_week_ppr <- wr1 %>%
          filter(week == w & recent_team == team_factor[t])
        if (dim(wr1_week_ppr)[1] == 0) {
          wr1_week_ppr <- 0
        } else {
          wr1_week_ppr <- pull(wr1_week_ppr[1])
        }
        
        wr2_week_ppr <- wr2 %>%
          filter(week == w & recent_team == team_factor[t])
        if (dim(wr2_week_ppr)[1] == 0) {
          wr2_week_ppr <- 0
        } else {
          wr2_week_ppr <- pull(wr2_week_ppr[1])
        }
        
        te_week_ppr <- te %>%
          filter(week == w & recent_team == team_factor[t])
        if (dim(te_week_ppr)[1] == 0) {
          te_week_ppr <- 0
        } else {
          te_week_ppr <- pull(te_week_ppr[2])
        }
        
        opponent <- matchups %>%
            filter(week == w & recent_team == team_factor[t])
        print(w)
        print(team_factor[t])

                if (dim(opponent)[1] == 0) {
            opp_qb_week_ppr <- 0
            opp_rb1_week_ppr <- 0
            opp_rb2_week_ppr <- 0
            opp_wr1_week_ppr <- 0
            opp_wr2_week_ppr <- 0
            opp_te_week_ppr <- 0
        } else {
            opponent <- opponent %>%
                pull(opponent_team)
            print(opponent)
            
            opp_qb_week_ppr <- qb %>%
              filter(week == w & recent_team == opponent)
            opp_qb_week_ppr <- pull(opp_qb_week_ppr[2])
            
            opp_rb1_week_ppr <- rb1 %>%
              filter(week == w & recent_team == opponent)
            opp_rb1_week_ppr <- pull(opp_rb1_week_ppr[2])
            
            opp_rb2_week_ppr <- rb2 %>%
              filter(week == w & recent_team == opponent)
            opp_rb2_week_ppr <- pull(opp_rb2_week_ppr[2])
            
            opp_wr1_week_ppr <- wr1 %>%
              filter(week == w & recent_team == opponent)
            opp_wr1_week_ppr <- pull(opp_wr1_week_ppr[1])
            
            opp_wr2_week_ppr <- wr2 %>%
              filter(week == w & recent_team == opponent)
            opp_wr2_week_ppr <- pull(opp_wr2_week_ppr[1])
            
            opp_te_week_ppr <- te %>%
              filter(week == w & recent_team == opponent)
            opp_te_week_ppr <- pull(opp_te_week_ppr[2])
        }

        position_df <- add_row(position_df, team = team_factor[t], week = w, 
                               qb_ppr = qb_week_ppr, rb1_ppr = rb1_week_ppr, rb2_ppr = rb2_week_ppr, wr1_ppr = wr1_week_ppr, 
                               wr2_ppr = wr2_week_ppr, te_ppr = te_week_ppr, opp_qb_ppr = opp_qb_week_ppr, opp_rb1_ppr = opp_rb1_week_ppr, 
                               opp_rb2_ppr = opp_rb2_week_ppr, opp_wr1_ppr = opp_wr1_week_ppr, opp_wr2_ppr = opp_wr2_week_ppr, 
                               opp_te_ppr = opp_te_week_ppr)
        
    }
}

print(n = 100, position_df)

position_df

position_cov_df <- position_df %>%
      ungroup() %>%
      select(qb_ppr, rb1_ppr, rb2_ppr, wr1_ppr, wr2_ppr, te_ppr)

cov_matrix <- cov(position_cov_df)
cor_matrix <- cov2cor(cov_matrix)
cov_matrix
cor_matrix

print(position_df, n = 100)

print(position_df$qb_ppr)
length(position_df$qb_ppr)

mean(position_df$qb_ppr)
sd(position_df$qb_ppr)

mean(position_df$rb1_ppr)
sd(position_df$rb1_ppr)

mean(position_df$rb2_ppr)
sd(position_df$rb2_ppr)

mean(position_df$wr1_ppr)
sd(position_df$wr1_ppr)

mean(position_df$wr2_ppr)
sd(position_df$wr2_ppr)

mean(position_df$te_ppr)
sd(position_df$te_ppr)

stan_input <- position_df %>%
    select(qb_ppr, rb1_ppr, rb2_ppr, wr1_ppr, wr2_ppr, te_ppr)
stan_input <- as.matrix(stan_input)

stan_input


# =================================================================================================
# Normal correlation
model <- stan_model(file = "best_ball/scripts/basic_correlation.stan", model_name = "basic_correlation_model")

data_train <- list(
  N = nrow(stan_input),
  K = ncol(stan_input),
  y = stan_input
)
fit <- sampling (
  model, data = data_train, iter = 2500, chains = 4, seed = 6969
)
fit_vars <- extract(fit)
fit_vars

summary_fit <- summary(fit)
summary_stats <- summary_fit$summary
summary_stats
means <- summary_stats[,"mean"]
sds <- summary_stats[,"sd"]
means

corr_matrix <- matrix(nrow = 6, ncol = 6, data = 0)
corr_sd_matrix <- matrix(nrow = 6, ncol = 6, data = 0)


for (i in 1:6) {
    for (j in 1:6) {
        corr_matrix[i,j] <- means[12 + (i-1)*6+j] 
        corr_sd_matrix[i,j] <- sds[12 + (i-1)*6+j] 
    }
}
rownames(corr_matrix) = c("QB", "RB1", "RB2", "WR1", "WR2", "TE")
rownames(corr_sd_matrix) = c("QB", "RB1", "RB2", "WR1", "WR2", "TE")
colnames(corr_matrix) = c("QB", "RB1", "RB2", "WR1", "WR2", "TE")
colnames(corr_sd_matrix) = c("QB", "RB1", "RB2", "WR1", "WR2", "TE")


corr_matrix
corr_sd_matrix
position_df

ggplot(data = position_df, aes(x = qb_ppr, y = rb1_ppr)) +
    geom_point(aes(color = week))

ggplot(data = position_df, aes(x = qb_ppr, y = wr1_ppr)) +
  geom_point(aes(color = week))


colors <- colorRampPalette(c("white", "green", "darkgreen"))(1000)

heatmap(corr_matrix, 
        col = colors,  # Use custom colors
        scale = "none", # Scale none
        margins = c(5, 5))  # Margins for row and column names


heatmap(corr_sd_matrix, 
        col = colors,  # Use custom colors
        scale = "none", # Scale none
        margins = c(5, 5))  # Margins for row and column names


# =================================================================================================
# With opponent teams correlation
opp_position_df <- position_df
opp_position_df

stan_input2 <- opp_position_df %>%
    select(-team, -week)
stan_input2 <- as.matrix(stan_input2)
stan_input2

model2 <- stan_model(file = "best_ball/scripts/opponent_correlation.stan", model_name = "opponent_correlation_model")

data_train2 <- list(
  N = nrow(stan_input2),
  K = ncol(stan_input2),
  y = stan_input2
)
fit2 <- sampling (
  model2, data = data_train2, iter = 2500, chains = 4, seed = 6969
)
fit_vars2 <- extract(fit2)
fit_vars2

summary_fit2 <- summary(fit2)
summary_stats2 <- summary_fit2$summary
summary_stats2
means2 <- summary_stats2[,"mean"]
sds2 <- summary_stats2[,"sd"]
means2

corr_matrix2 <- matrix(nrow = 12, ncol = 12, data = 0)
corr_sd_matrix2 <- matrix(nrow = 12, ncol = 12, data = 0)


for (i in 1:12) {
  for (j in 1:12) {
    corr_matrix2[i,j] <- means2[24 + (i-1)*12+j] 
    corr_sd_matrix2[i,j] <- sds2[24 + (i-1)*12+j] 
  }
}
rownames(corr_matrix2) = c("QB", "RB1", "RB2", "WR1", "WR2", "TE", "opp_QB", "opp_RB1", "opp_RB2", "opp_WR1", "opp_WR2", "opp_TE")
rownames(corr_sd_matrix2) = c("QB", "RB1", "RB2", "WR1", "WR2", "TE", "opp_QB", "opp_RB1", "opp_RB2", "opp_WR1", "opp_WR2", "opp_TE")
colnames(corr_matrix2) = c("QB", "RB1", "RB2", "WR1", "WR2", "TE", "opp_QB", "opp_RB1", "opp_RB2", "opp_WR1", "opp_WR2", "opp_TE")
colnames(corr_sd_matrix2) = c("QB", "RB1", "RB2", "WR1", "WR2", "TE", "opp_QB", "opp_RB1", "opp_RB2", "opp_WR1", "opp_WR2", "opp_TE")


corr_matrix2
corr_sd_matrix2

colors2 <- colorRampPalette(c("red", "white", "green", "darkgreen"))(1000)

heatmap(corr_matrix2, 
        col = colors2,  # Use custom colors
        scale = "none", # Scale none
        margins = c(5, 5))  # Margins for row and column names

