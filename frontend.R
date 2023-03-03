# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)

# Load data --------------------------------------------------------------------

nyg_2022 <- read.csv("2022 NYG OFF DATA PFF.csv")
nyg_2022['state'] <- case_when((nyg_2022$DN == 1 | nyg_2022$DN == 2) & nyg_2022$ydl_100 > 20 ~ "open field",
                               (nyg_2022$DN == 3 | nyg_2022$DN == 4) & nyg_2022$ydl_100 > 20 & nyg_2022$DST >= 8 ~ "3rd_4th long",
                               (nyg_2022$DN == 3 | nyg_2022$DN == 4) & nyg_2022$ydl_100 > 20 & nyg_2022$DST >= 3 & nyg_2022$DST < 8 ~ "3rd_4th med",
                               (nyg_2022$DN == 3 | nyg_2022$DN == 4) & nyg_2022$ydl_100 > 20 & nyg_2022$DST < 2 ~ "short yardage",
                               nyg_2022$ydl_100 <= 20 ~ "red zone")

pass_dat <- nyg_2022 %>% 
  filter(run_pass == "P") %>% 
  filter(pff_DROPBACKTYPE != "SR") %>% 
  filter(pff_DROPBACKTYPE != "SL") %>% 
  filter(pff_DROPBACKTYPE != "TR") %>% 
  filter(pff_DROPBACKTYPE != "TL") 

pass_dat['play_name'] = case_when(pass_dat$pff_RUNPASSOPTION == 1 ~ "rpo",
                                  pass_dat$pff_SCREEN == 1 ~ "screen",
                                  pass_dat$pff_PLAYACTION == 1 & (pass_dat$pff_DROPBACKTYPE == "RR" | pass_dat$pff_DROPBACKTYPE == "RL" | pass_dat$pff_DROPBACKTYPE == "RSR" | pass_dat$pff_DROPBACKTYPE == "RSL" | 
                                                                    pass_dat$pff_DROPBACKTYPE == "RLR" | pass_dat$pff_DROPBACKTYPE == "RRL") ~ "naked_boot",
                                  pass_dat$pff_PLAYACTION == 0 & pass_dat$pff_DROPBACKTYPE == "SD" & pass_dat$pff_QUICKGAME == 1 & pass_dat$pff_RUNPASSOPTION == 0 ~ "quick",
                                  pass_dat$pff_PLAYACTION == 0 & pass_dat$pff_DROPBACKTYPE == "SD" & pass_dat$pff_QUICKGAME == 0 & pass_dat$pff_RUNPASSOPTION == 0 ~ "dropback",
                                  pass_dat$pff_PLAYACTION == 1 & pass_dat$pff_DROPBACKTYPE == "SD" ~ "play action",
                                  pass_dat$pff_PLAYACTION == 0 & (pass_dat$pff_DROPBACKTYPE == "RR" | pass_dat$pff_DROPBACKTYPE == "RL" | pass_dat$pff_DROPBACKTYPE == "RSR" | pass_dat$pff_DROPBACKTYPE == "RSL") ~ "sprint")

run_dat <- nyg_2022 %>% 
  filter(run_pass == "R")

run_dat['play_name'] = case_when(run_dat$pff_RUNCONCEPTPRIMARY == "INSIDE ZONE" ~ "inside zone",
                                 run_dat$pff_RUNCONCEPTPRIMARY == "OUTSIDE ZONE" ~ "outside zone",
                                 run_dat$pff_RUNCONCEPTPRIMARY == "COUNTER" ~ "counter",
                                 run_dat$pff_RUNCONCEPTPRIMARY == "DRAW" ~ "draw",
                                 run_dat$pff_RUNCONCEPTPRIMARY == "MAN" ~ "man",
                                 run_dat$pff_RUNCONCEPTPRIMARY == "POWER" ~ "power",
                                 run_dat$pff_RUNCONCEPTPRIMARY == "PULL LEAD" ~ "pull lead",
                                 run_dat$pff_RUNCONCEPTPRIMARY == "SNEAK" ~ "sneak",
                                 run_dat$pff_RUNCONCEPTPRIMARY == "TRICK" ~ "trick",
                                 run_dat$pff_RUNCONCEPTPRIMARY == "FB RUN" ~ "fullback",
                                 run_dat$pff_RUNCONCEPTPRIMARY == "TRAP" ~ "trap",
                                 run_dat$pff_RUNCONCEPTPRIMARY == "UNDEFINED" ~ "random")

nyg_2022 <- rbind(pass_dat, run_dat)

DN <- 1
DST <- 10
ydl_100 <- 75
TDS <- 0

STATE <- case_when((DN == 1 | DN == 2) & ydl_100 > 20 ~ "open field",
                   (DN == 3 | DN == 4) & ydl_100 > 20 & DST >= 8 ~ "3rd_4th long",
                   (DN == 3 | DN == 4) & ydl_100 > 20 & DST >= 3 & DST < 8 ~ "3rd_4th med",
                   (DN == 3 | DN == 4) & ydl_100 > 20 & DST < 2 ~ "short yardage",
                   ydl_100 <= 20 ~ "red zone")
df <- nyg_2022 %>% filter(state == STATE)
play_options <- unique(df$play_name)
play_options<- c("",play_options[!is.na(play_options)])


# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("NYG Offense Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      
      uiOutput("play_options")
    ),
    
    # Output: Show play result
    mainPanel(
      h1("You're the offensive coordinator for the New York Giants! Choose the type of play to call and see what happens."),
      br(),
      textOutput("down"),
    )
  )
)

get_result <- function(current_down, current_dst, current_ydl, current_tds, play){
  STATE <- case_when((current_down == 1 | current_down == 2) & current_ydl > 20 ~ "open field",
                     (current_down == 3 | current_down == 4) & current_ydl > 20 & current_dst >= 8 ~ "3rd_4th long",
                     (current_down == 3 | current_down == 4) & current_ydl > 20 & current_dst >= 3 & current_dst < 8 ~ "3rd_4th med",
                     (current_down == 3 | current_down == 4) & current_ydl > 20 & current_dst < 2 ~ "short yardage",
                     current_ydl <= 20 ~ "red zone")
  print("____________")
  print(STATE)
  df <- nyg_2022 %>% filter(state == STATE & play_name == play)
  result_yds <- df[sample(nrow(df), 1), ]$pff_GAINLOSSNET
  if (identical(result_yds, integer(0))){
    return (c(current_down, current_dst, current_ydl, 0, current_tds, STATE, 1))
  }
  new_tds <- current_tds
  print(result_yds)
  print(current_ydl)
  
  new_ydl <- current_ydl - result_yds # find new spot for ball
  if (new_ydl <= 0) { # if you score TD, reset and add 1 TD
    new_down <- 1
    new_dst <- 10
    new_ydl <- 75
    new_tds <- current_tds + 1
  }
  if (result_yds >= current_dst){  # if you surpass the down, reset downs and distance # TODO: add goal line exception
    new_down <- 1
    new_dst <- 10
  }
  else { # otherwise, increment downs by 1 and subtract  to find yds to go
    new_down = current_down + 1
    new_dst <- current_dst - result_yds
  }
  if (new_down > 4){ # if you fail to convert on 4th down, reset
    new_down <- 1
    new_dst <- 10
    new_ydl <- 75
  }
  return (c(new_down, new_dst, new_ydl, result_yds, new_tds, STATE, 0))
}

saveState <- function(data) {
  if (exists("play_history")) {
    play_history <<- rbind(play_history, data)
  }
  else {
    play_history <<- data
  }
}

getState <- function() {
  if (exists("play_history")) {
    return (tail(play_history, 1))
  }
  else {
    return (data.frame(Down = c(1), Distance = c(10), Yardline = c(75), State = c("open field"), Tds = c(0)))
  }
}

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$play_options <- renderUI({
    now <- getState()
    df <- nyg_2022 %>% filter(state == now$State)
    play_options <- c("",unique(df$play_name))
    selectInput(
      inputId = "playcall",
      label = "Play Call:",
      choices = play_options,
      selected = NULL)
  })
  output$down <- renderText({
    now <- getState()
    if (input$playcall == ""){ # beginning state
      saveState(data.frame(Down = c(1), Distance = c(10), Yardline = c(75), State = c("open field"), Tds = c(0)))
      sprintf("Down: %s, Yards to Go: %s, Yardline: %s, TDs: %s", 
              1, 10, 75, 0)
    }
    else{ # after user has selected 1st play
      new <- get_result(as.numeric(now$Down), as.numeric(now$Distance), as.numeric(now$Yardline), as.numeric(now$Tds), input$playcall)
      if (new[7] == 1){
        sprintf("Please select a different play.            Down: %s, Yards to Go: %s, Yardline: %s, TDs: %s", new[1], new[2], new[3], new[5])
      }
      else {
        
        saveState(data.frame(Down = c(new[1]), Distance = c(new[2]), Yardline = c(new[3]), Tds = c(new[5]), State = c(new[6])))
        sprintf("Previous play resulted in %s gain.             Down: %s, Yards to Go: %s, Yardline: %s, TDs: %s", new[4], new[1], new[2], new[3], new[5])
      }
    }})
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server, onStart = function() {
  onStop(function() {
    if (exists("play_history")) {
      rm(list = c("play_history"), envir = .GlobalEnv)
    }})
})
