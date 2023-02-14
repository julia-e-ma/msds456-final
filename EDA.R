# install required packages; only needs to be done once
#install.packages("tidyverse")
#install.packages("nflfastR")
#install.packages("nflreadr")
#install.packages("ggimage")
#install.packages("gt")
#install.packages("dplyr")
#install.packages("nflplotR")
#install.packages("randomForest")
#install.packages("xgboost")
#install.packages("neuralnet")

# import libraries; must be done for each session
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(dplyr)
library(nflplotR)
library(ggplot2)
library(randomForest)
library(xgboost)
library(neuralnet)

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


#### linear regression ####
relation <- lm(n.y ~ n.x + rate, combined)
print(summary(relation))

#### random forest ####
set.seed(1234)
rf.fit <- randomForest(n.y ~ ., data=combined, ntree=1000, importance=TRUE)
rf.fit

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

#### xgboost ####
x = data.matrix(combined[,2:4])
y = data.matrix(combined[,5])
model_xgb <- xgboost(data = x, label = y, nrounds=10)
imp <- xgb.importance(model = model_xgb)
print(imp)
xgb.plot.importance(importance_matrix = imp)

#### neural network ####

