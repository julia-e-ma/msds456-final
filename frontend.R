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

STATE = case_when((DN == 1 | DN == 2) & ydl_100 > 20 ~ "open field",
                              (DN == 3 | DN == 4) & ydl_100 > 20 & DST >= 8 ~ "3rd_4th long",
                              (DN == 3 | DN == 4) & ydl_100 > 20 & DST >= 3 & DST < 8 ~ "3rd_4th med",
                              (DN == 3 | DN == 4) & ydl_100 > 20 & DST < 2 ~ "short yardage",
                              ydl_100 <= 20 ~ "red zone")
df = nyg_2022 %>% filter(state == STATE)
play_options <- unique(df$play_name)
play_options<- play_options[!is.na(play_options)]


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
        selected = "man"
      ),
    ),
    
    # Output: Show play result
    mainPanel(
      h1("You're the offensive coordinator for the New York Giants! Choose the type of play to call and see what happens."),
      br(),
      textOutput("down"),
      h3("Yds to Go: "),h3("10"), br(),
      h3("Yard Line: "),h3("25")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$down <- renderText({paste("Down", input$x)})
  output$ytg <- renderText({"10"})
  output$ydline <- renderText({"25"})
  tds <- renderText({"0"})
  # filter plays by game state
  df = nyg_2022 %>% filter(state == STATE)
  play_options <- unique(df$play_name)
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
