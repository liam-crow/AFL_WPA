
library(dplyr)
library(tidyr)      # data cleaning
library(ggplot2)    # create plots
library(plotly)     # interactive plots
library(patchwork)  # combine plots
library(rsample)    # data splitting 
library(earth)      # fit MARS models
library(ggdark)     # ggplot themes
library(gt)         # make cool tables
library(ggrepel)    # make labels move away

clean_scoring_prog <- readRDS('scoring_prog.rds')

# check data
clean_scoring_prog %>% 
    filter(date > '2022-01-02') %>% 
    ggplot(aes(x = event_time_seconds, y = home_margin, group = game_id)) +
    geom_line(alpha = 0.25) +
    facet_grid(cols = vars(quarter), rows = vars(home_win))

#### fill in missing data ####

all_games <- clean_scoring_prog %>% 
    distinct(game_id, home_team, away_team, date)

poss_comb <- expand_grid(game_id   = 1:4261,
                         pcnt_comp = seq(0,400,100))

fill_data <- all_games %>% 
    inner_join(poss_comb, by = "game_id") %>% 
    mutate(
        playing_for = NA_character_,
        full_name = NA_character_,
        home_win = NA_real_, 
        home_margin = NA_real_
    )

comb_data <- clean_scoring_prog %>% 
    select(game_id, home_team, away_team, date, pcnt_comp, playing_for, full_name, home_win, home_margin) %>% 
    rbind(fill_data)

scoring_prog <- comb_data %>% 
    mutate(home_margin = if_else(pcnt_comp == 0, 0, home_margin)) %>% 
    group_by(game_id) %>% 
    arrange(game_id, pcnt_comp) %>% 
    fill(home_win, .direction = 'downup') %>% 
    fill(home_margin, .direction = 'downup') %>% 
    ungroup() %>% 
    filter(home_win != 0.5) %>%
    mutate(
        home_win = as.factor(home_win)
    )

#### modelling ####

model_data <- scoring_prog %>% 
    select(pcnt_comp, home_win, home_margin)

# can do some testing (removed for this script)
set.seed(123)
prog_split <- initial_split(model_data, prop = 0.99)
prog_train <- training(prog_split)
prog_test  <- testing(prog_split)

mars1 <- earth(
    home_win ~ .,
    data = prog_train,
    glm = list(family = binomial),
    trace = 1,
    degree = 3
)

# RSq 0.446

summary(mars1)
plotmo(mars1)
plot(mars1)

#### model on fake data ####

fake_data <- tidyr::crossing(home_margin = seq(-16,16,1),
                             pcnt_comp = 0:400)

predictions <- cbind(fake_data, prob = predict(mars1, newdata = fake_data,type = 'response'))

names(predictions) <- c('home_margin','pcnt_comp','home_win')

p1 <- predictions %>% 
    ggplot(aes(x = pcnt_comp, y = home_win, colour = home_margin)) +
    geom_line() +
    xlab('Percent Game Complete') +
    ylab('Home Team Win Probability') +
    scale_colour_continuous(name = 'Home\nTeam\nMargin') +
    theme()

plotly::ggplotly(p1)

#### in-game win probability ####

ts_data <- tibble(pcnt_comp = (0:400))

# pick a game_id to view
clean_scoring_prog %>% 
    distinct(game_id, date, home_team, away_team) %>% 
    filter(date == '2013-06-23')

# A tibble: 2 x 4
# game_id home_team      away_team date      
# <int> <chr>          <chr>     <date>    
# 1    2359 Fremantle      Kangaroos 2013-06-23
# 2    2696 Brisbane Lions Geelong   2013-06-23

game_data <- clean_scoring_prog %>% 
    filter(game_id == 2359) %>% 
    select(game_id, home_team, away_team, date, pcnt_comp, playing_for, player_id, full_name, home_win, home_margin) %>% 
    mutate(pcnt_comp = pcnt_comp) %>% 
    full_join(ts_data, by = "pcnt_comp") %>% 
    arrange(pcnt_comp) %>% 
    mutate(home_margin = if_else(pcnt_comp == 0,0,home_margin)) %>% 
    fill(home_margin, .direction = 'downup') %>% 
    fill(home_win, .direction = 'downup')

plot_title <- game_data %>% select(home_team, away_team, date) %>% 
    drop_na() %>% 
    distinct() %>% 
    mutate(
        title = paste0(home_team,' vs ', away_team,' (',date,')')
    ) %>% pull(title)

game_pred <- cbind(game_data, 
                   prob = predict(mars1, newdata = game_data, type = "response")[,1]) %>% 
    mutate(
        pcnt_comp = if_else(pcnt_comp > 399,400,pcnt_comp),
        prob = if_else(pcnt_comp == (400),home_win,prob)
    )

custom_theme <- theme(
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 4)
)

p1 <- ggplot(game_pred) +
    geom_hline(yintercept = 0.5, colour = 'red') +
    geom_line(aes(x = pcnt_comp, y = prob))+
    labs(
        title = plot_title,
        subtitle = 'Win Probability'
    ) +
    ylab('Home Team Win Probability') +
    xlab(element_blank()) +
    scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) + dark_mode() +
    custom_theme

p2 <- ggplot(game_pred) +
    geom_hline(yintercept = 0, colour = 'red') +
    geom_line(aes(x = (pcnt_comp), y = home_margin)) +
    labs(
        subtitle = 'Margin'
    )+
    ylab('Home Team Margin') +
    xlab('Game Elapsed (Normalised)') + dark_mode() +
    custom_theme

comb <- p1/p2

comb

# save as Cairo for High-Res Image
# ggsave(comb, filename = 'winviz.png', dpi = 300, type = 'cairo',
#        width = 14, height = 10, units = 'cm')

#### 2022 season WPA analysis ####

ts_data_full <- expand.grid(game_id = 1:4261,
                            pcnt_comp = 0:400)

season_data_2022 <- clean_scoring_prog %>% 
    filter(lubridate::year(date) == 2022) %>% 
    select(game_id, home_team, away_team, date, pcnt_comp, playing_for, player_id, full_name, home_win, home_margin) %>% 
    group_by(game_id) %>% 
    full_join(ts_data_full, by = c('game_id',"pcnt_comp")) %>% 
    arrange(pcnt_comp) %>%
    mutate(home_margin = if_else(pcnt_comp == 0,0,home_margin)) %>% 
    fill(home_margin, .direction = 'downup') %>% 
    fill(home_win, .direction = 'downup') %>% 
    ungroup() %>% 
    filter(!is.na(game_id), !is.na(home_win))

season_pred <- cbind(season_data_2022, 
                     prob = predict(mars1, newdata = season_data_2022, type = "response")[,1]) %>% 
    mutate(
        pcnt_comp = if_else(pcnt_comp > 399,400,pcnt_comp),
        prob = if_else(pcnt_comp == (400),home_win,prob)
    )

season_tally <- season_pred %>%
    as_tibble() %>% 
    # group_by(game_id) %>% 
    mutate(
        prev_prob = lag(prob),
        WPA = abs(prev_prob-prob) %>% round(2)
    ) %>% 
    filter(!is.na(player_id)) %>% 
    group_by(player_id, full_name) %>% 
    summarise(
        tWPA = sum(WPA, na.rm = T),
        WPAs = paste(WPA,collapse = ', '),
        n = n(),
        avWPA = tWPA/n,
        .groups = 'drop'
    )

#### season tally ####

twpa_res <- season_tally %>% 
    filter(n > 50) %>% 
    ggplot(aes(x = n, y = tWPA, label = full_name)) +
    geom_smooth(se = F, method = 'lm') +
    geom_point(size = 0.7) +
    geom_text_repel(size = 2.5, box.padding = 0.6,point.padding = 0.1,
                    segment.colour = 'grey',colour = 'white',segment.alpha = 0.5, segment.size = .5) +
    ggdark::dark_mode() +
    labs(
        title = 'Total Win Probability Added (tWPA) (2022)',
        subtitle = 'How much win probability is added per player (min 50 scoring shots)'
    )+
    xlab('Total Scoring Shots') +
    ylab('tWPA') +
    scale_x_continuous(expand = c(0.5,0.5)) +
    scale_y_continuous(expand = c(0.5,0.5))

twpa_res

# save using Cairo
# ggsave(twpa_res, filename = 'tWPA.png', dpi = 300, type = 'cairo',
#        width = 14, height = 10, units = 'cm')

#### clutch game performances ####

game_twpa <- season_pred %>% 
    group_by(game_id) %>% 
    mutate(
        prev_prob = lag(prob, default = 57),
        WPA = abs(prev_prob-prob) %>% round(2)
    ) %>% 
    filter(!is.na(player_id)) %>% 
    group_by(game_id, home_team, away_team, date, player_id, full_name) %>% 
    summarise(
        tWPA = sum(WPA),
        WPAs = paste(WPA,collapse = ', '),
        scoring_shots = n(),
        avWPA = round(tWPA/scoring_shots,3),
        .groups = 'drop'
    )

game_twpa %>% 
    select(home_team, away_team, date,full_name,tWPA,WPAs,scoring_shots,avWPA) %>% 
    arrange(-tWPA) %>% 
    slice(1:20) %>% 
    gt() %>% 
    tab_header(title = 'Most Total Win Prob Added in a Game (2022)',
               subtitle = 'Top 20 players that increased their team\'s win probability through scoring shots')
