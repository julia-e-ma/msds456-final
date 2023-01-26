# install required packages; only needs to be done once
#install.packages("tidyverse")
#install.packages("nflfastR")
#install.packages("ggimage")
#install.packages("gt")
#install.packages("dplyr")

# import libraries; must be done for each session
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(dplyr)

# get 2021-2022 data
pbp <- load_pbp(2021:2022)
pbp %>% head()

# browse available columns
names(pbp)

# get only Giants offense plays; no kickoffs or punts
nyg_pbp <- pbp %>% filter(posteam == "NYG") %>% select(defteam, down, ydstogo, play_type, play_type_nfl, result, yards_gained)
nyg_pbp <- nyg_pbp %>% filter(play_type!="kickoff" & play_type!="punt")

table(nyg_pbp['play_type'])
# extra point; field goal; no play; pass; qb kneel; qb spike; run
# perhaps we only care about pass and run? 
table(nyg_pbp['play_type_nfl'])
# nfl types are field goal; pass; pat2; penalty; rush; sack; xp kick
# do we treat sack/PAT2 differently?

