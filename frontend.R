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

nyg_2022['play_name'] = case_when(nyg_2022$pff_RUNPASSOPTION == 1 ~ "rpo",
                                  nyg_2022$pff_SCREEN == 1 ~ "screen",
                                  nyg_2022$pff_PLAYACTION == 1 & (nyg_2022$pff_DROPBACKTYPE == "RR" | nyg_2022$pff_DROPBACKTYPE == "RL" | nyg_2022$pff_DROPBACKTYPE == "RSR" | nyg_2022$pff_DROPBACKTYPE == "RSL" | 
                                  nyg_2022$pff_DROPBACKTYPE == "RLR" | nyg_2022$pff_DROPBACKTYPE == "RRL") ~ "naked_boot",
                                  nyg_2022$pff_PLAYACTION == 0 & nyg_2022$pff_DROPBACKTYPE == "SD" & nyg_2022$pff_QUICKGAME == 1 & nyg_2022$pff_RUNPASSOPTION == 0 ~ "quick",
                                  nyg_2022$pff_PLAYACTION == 0 & nyg_2022$pff_DROPBACKTYPE == "SD" & nyg_2022$pff_QUICKGAME == 0 & nyg_2022$pff_RUNPASSOPTION == 0 ~ "dropback",
                                  nyg_2022$pff_PLAYACTION == 1 & nyg_2022$pff_DROPBACKTYPE == "SD" ~ "play action",
                                  nyg_2022$pff_PLAYACTION == 0 & (nyg_2022$pff_DROPBACKTYPE == "RR" | nyg_2022$pff_DROPBACKTYPE == "RL" | nyg_2022$pff_DROPBACKTYPE == "RSR" | nyg_2022$pff_DROPBACKTYPE == "RSL") ~ "sprint")

nyg_2022['play_name'] = case_when(nyg_2022$pff_RUNCONCEPTPRIMARY == "INSIDE ZONE" ~ "inside zone",
                                nyg_2022$pff_RUNCONCEPTPRIMARY == "OUTSIDE ZONE" ~ "outside zone",
                                nyg_2022$pff_RUNCONCEPTPRIMARY == "COUNTER" ~ "counter",
                                nyg_2022$pff_RUNCONCEPTPRIMARY == "DRAW" ~ "draw",
                                nyg_2022$pff_RUNCONCEPTPRIMARY == "MAN" ~ "man",
                                nyg_2022$pff_RUNCONCEPTPRIMARY == "POWER" ~ "power",
                                nyg_2022$pff_RUNCONCEPTPRIMARY == "PULL LEAD" ~ "pull lead",
                                nyg_2022$pff_RUNCONCEPTPRIMARY == "SNEAK" ~ "sneak",
                                nyg_2022$pff_RUNCONCEPTPRIMARY == "TRICK" ~ "trick",
                                nyg_2022$pff_RUNCONCEPTPRIMARY == "FB RUN" ~ "fullback",
                                nyg_2022$pff_RUNCONCEPTPRIMARY == "TRAP" ~ "trap",
                                nyg_2022$pff_RUNCONCEPTPRIMARY == "UNDEFINED" ~ "random")

DN <- 1
DST <- 10
ydl_100 <- 25
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
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select play call
      selectInput(
        inputId = "playcall",
        label = "Play Call:",
        choices = play_options,
        selected = NULL
      ),
    ),
    
    # Output: Show play result
    mainPanel(
      h1("You're the offensive coordinator for the New York Giants! Choose the type of play to call and see what happens."),
      br(),
      textOutput("down"),
    )
  )
)

get_result <- function(current_down, current_dst, current_ydl, play){
  STATE <- case_when((current_down == 1 | current_down == 2) & current_ydl > 20 ~ "open field",
                     (current_down == 3 | current_down == 4) & current_ydl > 20 & current_dst >= 8 ~ "3rd_4th long",
                     (current_down == 3 | current_down == 4) & current_ydl > 20 & current_dst >= 3 & current_dst < 8 ~ "3rd_4th med",
                     (current_down == 3 | current_down == 4) & current_ydl > 20 & current_dst < 2 ~ "short yardage",
                     current_ydl <= 20 ~ "red zone")
  print("____________")
  print(STATE)
  df <- nyg_2022 %>% filter(state == STATE & play_name == play)
  result_yds <- df[sample(nrow(df), 1), ]$pff_GAINLOSSNET
  print(paste("Gain:" ,result_yds))
  
  new_ydl <- current_ydl + result_yds # find new spot for ball
  tds <- 0
  if (new_ydl >= 100) { # if you score TD, reset and add 1 TD ### TODO; add multiple TD tracking and ydl_100 logic
    new_down <- 1
    new_dst <- 10
    new_ydl <- 25
    tds <- 1
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
    new_ydl <- 25
  }
  return (c(new_down, new_dst, new_ydl, result_yds, tds))
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
    return (data.frame(Down = c(1), Distance = c(10), Yardline = c(25)))
  }
}

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$down <- renderText({
    now <- getState()
    if (input$playcall == ""){ # beginning state
      saveState(data.frame(Down = c(1), Distance = c(10), Yardline = c(25)))
      sprintf("Down: %s, Yards to Go: %s, Yardline: %s", 
              1, 10, 25)
    }
    else{ # after user has selected 1st play
      new <- get_result(now$Down, now$Distance, now$Yardline, input$playcall)
      saveState(data.frame(Down = c(new[1]), Distance = c(new[2]), Yardline = c(new[3])))
      sprintf("Previous play resulted in %s gain.    \n Down: %s, Yards to Go: %s, Yardline: %s", new[4], new[1], new[2], new[3])
    }})
  }

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server, onStart = function() {
  onStop(function() {
    if (exists("play_history")) {
      rm(list = c("play_history"), envir = .GlobalEnv)
    }})
})
