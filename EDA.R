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


# read in CSV data
redZoneTrain <- read.csv(file = 'RedZoneTrain.csv')
redZoneTest <- read.csv(file = 'RedZoneTest.csv')
successTrain <- read.csv('Train_NFL_Success_Rates.csv')
successTest <- read.csv('Train_NFL_Success_Rates.csv')


# get 2016-2021 data
pbp <- load_pbp(2016:2021)
pbp %>% head()

# browse available columns
names(pbp)

# get total plays that resulted in TD
all_tds <- pbp %>% filter(touchdown==1) %>% select(posteam,season,td_team,touchdown)
# filter out defensive TDs
off_td <- all_tds %>% filter(td_team==posteam)
td_train <- off_td %>% filter(season != 2021)
td_test <- off_td %>% filter(season == 2021)
# count by team
train_y <- count(td_train,posteam)
test_y <- count(td_test,posteam)

######fourth down stuff######
fourth_downs <- pbp %>% filter(down == 4) %>% select(posteam, fourth_down_converted, fourth_down_failed, season)

fourth_train <- fourth_downs %>% filter(season != 2021)
fourth_test <- fourth_downs %>% filter(season == 2021)
tot_fourth_train <- count(fourth_train,posteam)
tot_fourth_test <- count(fourth_test,posteam)

go_for_it_train <- fourth_train %>% filter(fourth_down_converted==1 | fourth_down_failed==1)
go_for_it_test <- fourth_test %>% filter(fourth_down_converted==1 | fourth_down_failed==1)
tot_fourth_train['attempts'] <- count(go_for_it_train, posteam)$n
tot_fourth_test['attempts'] <- count(go_for_it_test, posteam)$n
tot_fourth_train['success'] <- count(go_for_it_train %>% filter(fourth_down_converted==1),posteam)$n
tot_fourth_test['success'] <- count(go_for_it_test %>% filter(fourth_down_converted==1),posteam)$n
tot_fourth_train['success_rate'] <- tot_fourth_train$success/tot_fourth_train$attempts
tot_fourth_test['success_rate'] <- tot_fourth_test$success/tot_fourth_test$attempts
tot_fourth_train['go_for_it_rate'] <- tot_fourth_train$attempts/tot_fourth_train$n
tot_fourth_test['go_for_it_rate'] <- tot_fourth_test$attempts/tot_fourth_test$n
tot_fourth_train
tot_fourth_test

##### explosive plays ######
run_or_pass <- pbp %>% filter(play_type == 'run' | play_type == 'pass') %>% select(posteam, play_type, yards_gained, season)
explosive_plays <- run_or_pass %>% filter((play_type == 'pass' & yards_gained >= 20) | (play_type == 'run' & yards_gained >= 10))
explosive_plays_train <- count(explosive_plays %>% filter(season != 2021), posteam)
explosive_plays_test <- count(explosive_plays %>% filter(season == 2021), posteam)

###### negative yard plays #####
neg_yard_plays <- pbp %>% filter(yards_gained < 0)
neg_yard_train <- count(neg_yard_plays %>% filter(season != 2021), posteam)
neg_yard_test <- count(neg_yard_plays %>% filter(season == 2021), posteam)

##### turnovers #####
turnovers_count <- pbp %>% filter(interception==1 | fumble_lost==1)
turn_train <- count(turnovers_count %>% filter(season != 2021), posteam)
turn_test <- count(turnovers_count %>% filter(season == 2021), posteam)

######3rd down avoidance#####
#number of conversions on 1st/2nd down
conversions <- pbp %>% filter(first_down == 1) %>% select(posteam, down, season)
conv_train <- count(conversions %>% filter(season != 2021),posteam)
conv_test <- count(conversions %>% filter(season == 2021),posteam)
conv_1st_2nd <- conversions %>% filter(down == 1 | down == 2)
conv_train['conv_1st_2nd'] <- count(conv_1st_2nd %>% filter(season != 2021), posteam)$n
conv_test['conv_1st_2nd'] <- count(conv_1st_2nd %>% filter(season == 2021), posteam)$n

#number of down sequences - filtered for repeat 1st downs due to offensive penalties
sequences <- pbp %>% filter(down == 1) %>% select(posteam, penalty_team, penalty_yards, season)
seq_filtered <- sequences %>% filter(is.na(penalty_team) | posteam != penalty_team | penalty_yards != 0)
down_sequences_train <- count(seq_filtered %>% filter(season != 2021), posteam)
down_sequences_test <- count(seq_filtered %>% filter(season == 2021), posteam)

#bring together
combined_conv_sequences_train <- conv_train %>% inner_join(down_sequences_train, by="posteam")
combined_conv_sequences_test <- conv_test %>% inner_join(down_sequences_test, by="posteam")
combined_conv_sequences_train['avoidance_3rd'] <- combined_conv_sequences_train$conv_1st_2nd / combined_conv_sequences_train$n.y
combined_conv_sequences_test['avoidance_3rd'] <- combined_conv_sequences_test$conv_1st_2nd / combined_conv_sequences_test$n.y
combined_conv_sequences_train <- combined_conv_sequences_train %>% rename(
  conversions = n.x,
  sequences = n.y,
)
combined_conv_sequences_test <- combined_conv_sequences_test %>% rename(
  conversions = n.x,
  sequences = n.y,
)

##### offensive penalties #####
penalty_plays <- pbp %>% filter(penalty==1 & penalty_team==posteam) %>% select(posteam, penalty, penalty_team,season)
penalty_train <- count(penalty_plays %>% filter(season != 2021), posteam)
penalty_test <- count(penalty_plays %>% filter(season == 2021), posteam)


##### train features #####
train_x <- combined_conv_sequences_train %>% select(posteam,avoidance_3rd)
train_x <- train_x %>% inner_join(explosive_plays_train, by="posteam") %>% rename ("exp_plays" = "n")
train_x['rushing'] <- count(pbp %>% filter(play_type == 'run' & season != 2021), posteam)$n
train_x['passing'] <- count(pbp %>% filter(play_type == 'pass' & season != 2021), posteam)$n
train_x['off_penalty'] <- penalty_train$n
train_x['neg_yard_plays'] <- neg_yard_train$n
train_x['turnovers'] <- turn_train$n
train_x['go_for_it_rate'] <- tot_fourth_train$go_for_it_rate
train_x['red_zone_td_rate'] <- redZoneTrain$RZPct
train_x['third_down_conv'] <- successTrain$total_3rd
train_x['first_down_success'] <- successTrain$total_successrate_1st
train_x['run_success'] <- successTrain$run_successrate_all
train_x['pass_success'] <- successTrain$pass_successrate_all

train_total <- train_x %>% inner_join(train_y, by="posteam")

##### test features #####
test_x <- combined_conv_sequences_test %>% select(posteam,avoidance_3rd)
test_x <- test_x %>% inner_join(explosive_plays_test, by="posteam") %>% rename ("exp_plays" = "n")
test_x['rushing'] <- count(pbp %>% filter(play_type == 'run' & season == 2021), posteam)$n
test_x['passing'] <- count(pbp %>% filter(play_type == 'pass' & season == 2021), posteam)$n
test_x['off_penalty'] <- penalty_test$n
test_x['neg_yard_plays'] <- neg_yard_test$n
test_x['turnovers'] <- turn_test$n
test_x['go_for_it_rate'] <- tot_fourth_test$go_for_it_rate
test_x['red_zone_td_rate'] <- redZoneTest$RZPct
test_x['third_down_conv'] <- successTest$total_3rd
test_x['first_down_success'] <- successTest$total_successrate_1st
test_x['run_success'] <- successTest$run_successrate_all
test_x['pass_success'] <- successTest$pass_successrate_all

test_total <- test_x %>% inner_join(test_y, by="posteam")

#### linear regression ####
relation <- lm(n ~ avoidance_3rd+exp_plays+rushing+passing+off_penalty+neg_yard_plays+
                 turnovers+go_for_it_rate+red_zone_td_rate+third_down_conv+first_down_success+
                 run_success+pass_success, train_total)
print(summary(relation))

lin_prediction <- data.frame(n = predict(relation, newdata = test_x))
lr_err <- sqrt(mean((test_y$n - lin_prediction$n)^2))

#### random forest ####
set.seed(1234)
rf_model <- randomForest(n ~ ., data=train_total, ntree=1000, importance=TRUE)

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf_model))
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

rf_prediction <- data.frame(n = predict(rf_model, newdata = test_x))
rf_err <- sqrt(mean((test_y$n - rf_prediction$n)^2))

#### xgboost ####
x = data.matrix(train_x[,2:14])
y = data.matrix(train_y$n)
xgb_model <- xgboost(data = x, label = y, nrounds=10)
imp <- xgb.importance(model = xgb_model)
print(imp)
xgb.plot.importance(importance_matrix = imp)

xgb_prediction <- data.frame(n= predict(xgb_model, data.matrix(test_x[,-1])))
xgb_err <- sqrt(mean((test_y$n - xgb_prediction$n)^2))

#### neural network ####
