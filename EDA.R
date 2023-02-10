# install required packages; only needs to be done once
#install.packages("tidyverse")
#install.packages("nflfastR")
#install.packages("nflreadr")
#install.packages("ggimage")
#install.packages("gt")
#install.packages("dplyr")
#install.packages("nflplotR")

# import libraries; must be done for each session
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(dplyr)
library(nflplotR)
library(ggplot2)

# get 2016-2021 data
pbp <- load_pbp(2016:2021)
pbp %>% head()

# browse available columns
names(pbp)

# get total plays that resulted in TD
all_tds <- pbp %>% filter(touchdown==1) %>% select(posteam,season,td_team,touchdown)
# filter out defensive TDs
off_td <- all_tds %>% filter(td_team==posteam)
# count by team
tot_td <- count(off_td,posteam)
tot_td

######fourth down stuff######
go_for_it<- pbp %>% filter(fourth_down_converted==1 | fourth_down_failed==1) %>% select(posteam, fourth_down_converted)
tot_fourth <- count(go_for_it,posteam)
fourth_success <- go_for_it %>% filter(fourth_down_converted==1)
tot_fourth['success'] <- count(fourth_success,posteam)[,2]
tot_fourth['rate'] <- tot_fourth[,3]/tot_fourth[,2]
tot_fourth

combined <- tot_fourth %>% inner_join(tot_td, by="posteam")
combined

ggplot(combined, aes(x = n.x, y = n.y)) + 
  nflplotR::geom_mean_lines(aes(v_var = n.x, h_var = n.y)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.065, alpha = 0.7) +
  labs(x = "Fourth Down Attempts",
       y = "Offensive TDs",
       title = "Fourth Down Go-For-It vs. TDs")

#####Success Rates vs # of touchdowns#####
library(readr)
all_NFL_Success_Rates <- read_csv("C:/Users/bshos/OneDrive/Desktop/MSDS 456/Final Project/all_NFL_Success_Rates.csv") #will need to change this obviously
success_rates <- all_NFL_Success_Rates %>%
                    mutate(total_tds = tot_td$n)

#Total Success Rate
ggplot(success_rates, aes(x = total_tds, y = total_successrate_all)) + 
  nflplotR::geom_mean_lines(aes(v_var = total_tds, h_var = total_successrate_all)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.065, alpha = 0.7) +
  labs(x = "Offensive TDs",
       y = "Offensive Success Rate",
       title = "Success Rate vs. TDs")

#Run Success Rate
ggplot(success_rates, aes(x = total_tds, y = run_successrate_all)) + 
  nflplotR::geom_mean_lines(aes(v_var = total_tds, h_var = run_successrate_all)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.065, alpha = 0.7) +
  labs(x = "Offensive TDs",
       y = "Run Success Rate",
       title = "Run Success Rate vs. TDs")

#Pass Success Rate
ggplot(success_rates, aes(x = total_tds, y = pass_successrate_all)) + 
  nflplotR::geom_mean_lines(aes(v_var = total_tds, h_var = pass_successrate_all)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.065, alpha = 0.7) +
  labs(x = "Offensive TDs",
       y = "Pass Success Rate",
       title = "Pass Success Rate vs. TDs")

#1st Success Rate
ggplot(success_rates, aes(x = total_tds, y = total_successrate_1st)) + 
  nflplotR::geom_mean_lines(aes(v_var = total_tds, h_var = total_successrate_1st)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.065, alpha = 0.7) +
  labs(x = "Offensive TDs",
       y = "First Down Success Rate",
       title = "First Down Success Rate vs. TDs")

#3rd Success Rate
ggplot(success_rates, aes(x = total_tds, y = total_3rd)) + 
  nflplotR::geom_mean_lines(aes(v_var = total_tds, h_var = total_3rd)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.065, alpha = 0.7) +
  labs(x = "Offensive TDs",
       y = "Third Down Conversion Rate",
       title = "Third Down Conversion Rate vs. TDs")
