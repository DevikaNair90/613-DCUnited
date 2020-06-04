#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/



library(shiny)
library(ggplot2)
library(readr)
library(shinythemes)
library(dplyr)
library(DT)
library(ggrepel)
library(ggthemes)



MLSPlayerPlay <- read_csv("test.csv")



# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
   # Application title
   titlePanel("Modeling Successful Soccer Play"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        tags$h4("Welcome"),
         tags$h5("Use this tool to walk through our approach and final results of our project. 
                 This analysis seeks to model successful play of individual players, 
                 optimizing a position-specific metric of play. "),
        tags$h5(" - "),
        tags$h5("Data Sources: OPTA"),
        tags$h5("Authors: Devika Nair, Dominic Thomas, Julia Smadja"),
        tags$h5("Date: 4/28/2018")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("MLS Overall", tags$h4("Analytical Approach to Soccer Play"), 
                   tags$h5("Soccer matches have simple results (wins, losses, ties) but complex behavior. Each match results from the intra-team and inter-team interactions, which occur between the various modes of play: goalie play, defender play, midfielder play, and goalie play. This analysis looks to break down each mode of play's specific targets and how those target variables result from other independent variables and how those metrics tie to game outcomes."),
                   tags$h4("MLS Players by Mode of Play"), 
                   plotOutput("recordcount"), 
                   tags$h5("The plot above shows how many individual MLS players' data was used for the following visuals."),
                   tags$h4("MLS Teams by Mode of Play"),
                   plotOutput("teamcomp"), 
                   tags$h5("The plot above shows how MLS teams staff players by mode of play. ")),
  
          tabPanel("Targets", 
                   tags$h4("Our Approach to Target Selection"),
                   tags$h5("As we investigated the various modes of play, it became clear that each mode of play affected 
                           other modes, making some modes were easier to analyze than others. The easiest mode to analyze was 
                           goalie play, because it is very isolated to the box and therefore we were able to select Saves as 
                           the target variable fairly confidently. On the other hand, defensive play, midfielder play, and 
                           offensive play are all interconnected."),
                   tags$h4("Goalie Play"),
                   fluidRow(column(8,  plotOutput("PlotGoalie")), 
                            column(3, tags$h5("Total Saves:"), 
                                   tags$li( "only goalies make saves"), 
                                   tags$li("50 MLS goalies"),
                                   tags$li("smallest volume of backing data"))),
                   tags$h4("Defender Play"),
                   fluidRow(column(4, tags$h5("Shots Conceded Differential:"),
                                   tags$li("Team's Shots Conceded - Opposition's Shots Conceded"),
                                   tags$li("or: Defender Fails - Oppositions' Defender Fails"),
                                   tags$li("measures individual defenders against team metric"),
                                   tags$li("goals - minimize opposition's shots & maximize shot opportunities"),
                                   tags$li("want this number to be more negative")),
                            column(7,  plotOutput("PlotDefense")) ),
                   tags$h4("Midfielder Play"),
                   fluidRow(column(7,  plotOutput("PlotMid")), 
                            column(4, tags$h5("Goal Differential:"),
                                   tags$li("Team's Goals - Opposition's Goals"),
                                   tags$li("measures individual midfielders against team metric"),
                                   tags$li("goals - minimize opposition goals & maximize own goals"),
                                   tags$li("want this number to be more positive"))),
                   tags$h4("Forward Play"),
                   fluidRow(column(3, tags$h5("Goals"),
                                   tags$li("forwards more likely to score goals"),
                                   tags$li("goals don't necessarily result in more wins")), 
                            column(8,  plotOutput("PlotOffense")) )), 
          
          
          tabPanel("Goalie", tags$h4("Goalie Target Variable: Saves Made"),
                   tags$h5("Our model for goalies highlights saves made as the target variable. Our model showed big chances, touches, 
                           goals conceded, shots conceded, and corners conceded as significant variables."),
                   tags$h4("How Teams Perform Across the Field"), plotOutput("SavesMadePlot"),
                   tags$h4("Goalie Touches"),
                   fluidRow(
                     column(8, plotOutput("Touches")),
                     column(3, tags$h5("More Touches Lead to More Saves"),
                            tags$li("more saves doesn't lead to more wins"),
                            tags$li("wins rely on other factors")) ),
                   tags$h4("Corners Conceded"),
                   fluidRow(   
                     column(3, tags$h5("More Corners Conceded Lead to More Saves"),
                            tags$li("more saves doesn't lead to more wins"),
                            tags$li("greater spread of game outcomes")),
                     column(8, plotOutput("CornersConceded"))  )
                   ),
          
          tabPanel("Defense", tags$h4("Defense Target Variable: Shots Conceded"),
                   tags$h5("Our model for defensive players highlights shots conceded as the target variable. Our model 
                           showed passes, interceptions, clearances, crosses corners, touches, tackles, duels, and long 
                           balls as significant variables."),
                   tags$h4("How Teams Perform Across the Field"), plotOutput("ShotDiffPlot"),
                   tags$h5("The plot above shows how different teams score goals and whether those goals won the game or not.
                           It is important to note a defensive player's objective is to to minimize this differential."),
                   tags$h4("Duels Won"),
                   fluidRow(
                     column(7, plotOutput("DuelsWon")),
                     column(3, tags$h5("More Duels Won Correlates with Smaller Shots Differential "),
                            tags$li("winning duels signifies greater ball control"),
                            tags$li("winning over 8 duels reduces overall shots per game") )),
                   tags$h4("Interceptions"),
                   fluidRow(   
                     column(3, tags$h5("More Interceptions Correlates with Smaller Shots Differential"),
                            tags$li("interceptions signifies greater ball control"),
                            tags$li("intercepting over 5 balls reduces overall shots per game") ),
                     column(7, plotOutput("Interceptions"))  )  ),
      
          tabPanel("Midfield", tags$h4("Midfield Target Variable: Goal Differential"),
                   tags$h5("Our model for midfielder players highlights the goal differential of a match as the target variable. 
                           Our model showed assists, touches, passes, shots conceded, recoveries, and tackles as significant 
                           variables."),
                   tags$h4("How Teams Perform Across the Field"), plotOutput("GoalDiffPlot"),
                   tags$h5("The plot above shows how different teams score goals and whether those goals won the game or not."),
                   tags$h4("Assists"), 
                   fluidRow(
                     column(7, plotOutput("Assists")),
                     column(4, tags$h5("Assists Shift Goal Differential Positively"),
                            tags$li("more assists lead to more won games"),
                            tags$li("assists mostly done by wide midfielders"),
                            tags$li("few assists by 'box-to-box' "))),
                   tags$h4("Recoveries"),
                   fluidRow(
                     column(4, tags$h5("More Recoveries Lead to Smaller Goal Differential"),
                            tags$li("recoveries signify shaky ball control"),
                            tags$li("over 7 recoveries shows smaller goal differential"),
                            tags$li("recoveries by wide midfielders, box-to-box midfielders, and holding midfielders")),
                     column(7, plotOutput("Recoveries"))   )),
                   
          
          tabPanel("Offense", tags$h4("Offense Target Variable: Goals"),
                  tags$h5("Our model for offensive players highlights goals as the target variable. Our model showed 
                          backward and short passes, big chances, shots on target as significant variables."),
                  tags$h4("Goals Scored by Team"), plotOutput("GoalPlot"),
                  tags$h5("The plot above shows how different teams score goals and whether those goals won the game or not. 
                          It is important to note that more goals doesn't necessarily result in more wins."),
                  tags$h4("Big Chances"), 
                  fluidRow(
                     column(6, plotOutput("ChancesPlot")),
                     column(4, tags$h5("More Chances Lead to More Goals"),
                            tags$li("playmakers create more big chances"),
                            tags$li("forwards' big chances result in more goals") )),
                  tags$h4("Shots On Target"),
                  fluidRow(   
                     column(4, tags$h5("More Shots on Target Lead to More Goals"),
                            tags$li("playmakers shoot on target more often"),
                            tags$li("forwards' on-target shots result in more goals") ),
                     column(6, plotOutput("OffenseShotsPlot"))  ))
       )
    ) ) )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  distinct.players <- MLSPlayerPlay %>% group_by(PositionName) %>% summarise(n = n_distinct(PlayerID)) 
  teamcomp <- MLSPlayerPlay %>% group_by(Team, PositionName, Year) %>% summarise(n = n_distinct(PlayerID), win = sum(WinningGoal))
  
  ### OVERALL TAB
  output$recordcount <- renderPlot({ ggplot(distinct.players, 
                                            aes(x = PositionName, y = n, fill = PositionName)) + 
                                      geom_bar(stat = "identity") +
                                      labs(x = "Mode of Play", 
                                           y = "Player Count", 
                                           fill = "Mode of Play") +
                                      theme_minimal()  + scale_fill_brewer(palette = "Blues") }) 
  
  output$teamcomp <- renderPlot({ ggplot(teamcomp, 
                                            aes(reorder(Team, -n), y = n, fill = PositionName)) +
                                      geom_bar(stat= "identity") +
                                      labs(x = "MLS Teams", 
                                           y = "Player Count ", 
                                           fill = "Mode of Play") +
                                      theme_minimal() + scale_fill_brewer(palette = "Blues")  +
                                      theme(axis.text.x = element_text(angle = 90, hjust = 1))   })
  

  
  ### GOALIE TAB
  
  output$PlotGoalie <- renderPlot({ ggplot(MLSPlayerPlay, 
                                           aes(Position, SavesMade, fill = PositionName)) + 
                                          geom_bar(stat = "identity")  +
                                          labs(x = "Position", 
                                               y = "Total Saves", 
                                               title = "Only Goalies Contribute to Saves",
                                               fill = "Mode of Play") +
                                          theme_minimal() + scale_fill_brewer(palette = "Blues") +
                                          theme(legend.position ="bottom")      })
  
  output$SavesMadePlot <- renderPlot({ ggplot(MLSPlayerPlay %>% filter(PositionName == "Goalie"), 
                                           aes(reorder(Team, -SavesMade), SavesMade, fill = factor(Points)  )) + 
                                            geom_bar(stat = "identity")  +
                                            labs(x = "Team", 
                                                 y = "Total Saves", 
                                                 title = "Saves Made per Team",
                                                 fill = "Points") +
                                            theme_minimal() + scale_fill_brewer(palette = "Greens")   +
                                            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                            theme(legend.position ="bottom")  })
  
  output$Touches <- renderPlot({ ggplot(MLSPlayerPlay %>% filter(PositionName == "Goalie"),
                                        aes(Touches, SavesMade, colour = factor(Points))) +
                                        geom_point() +
                                        labs(x = "Goalie Touches", 
                                             y = "Total Saves", 
                                             colour = "Points") +
                                        theme_minimal() + scale_colour_brewer(palette = "Greens") +
                                        theme(legend.position = "bottom")   })
  

  
  output$CornersConceded <- renderPlot({  ggplot(MLSPlayerPlay %>% filter(PositionName == "Goalie"), 
                                                 aes(CornersConceded, SavesMade, colour = factor(Points))) +
                                          geom_point() +
                                          labs( x = "Goalie Corners Conceded", 
                                                y = "Total Saves", 
                                                colour = "Points") + 
                                          theme_minimal() + scale_colour_brewer(palette = "Greens") +
                                          theme(legend.position = "bottom")  })
  
  
  
  # add data label - numerics
  
  ### DEFENSE TAB
  output$PlotDefense <- renderPlot({ ggplot(MLSPlayerPlay, 
                                            aes(Position, ShotsConceidedDifferential, fill = PositionName)) + 
                                            geom_bar(stat = "identity") +
                                            labs(x = "Position",  
                                                 y = "Shots Conceded Differential", 
                                                 title = "All Positions Contribute to Shots Conceded Differential", 
                                                 fill = "Mode of Play") +
                                            theme_minimal() + scale_fill_brewer(palette = "Blues") +
                                            theme(legend.position ="bottom")  })
  
  output$ShotDiffPlot <- renderPlot({ ggplot(MLSPlayerPlay %>% filter(PositionName == "Defense"), 
                                            aes(reorder(Team, -ShotsConceidedDifferential), 
                                                ShotsConceidedDifferential, fill = factor(Points))) + 
                                            geom_bar(stat = "identity") +
                                            labs(x = "Team",  
                                                 y = "Shots Differential", 
                                                 title = "Shots Differential per Team",
                                                 fill = "Points") +
                                            theme_minimal() + scale_fill_brewer(palette = "Greens") +
                                            theme(axis.text.x = element_text(angle = 90, hjust = 1))  })
  
  output$DuelsWon <- renderPlot({ ggplot(MLSPlayerPlay %>% filter(PositionName == "Defense"), 
                                        aes(Duelswon, ShotsConceidedDifferential, colour = factor(Points))) + 
                                        geom_point() +
                                        labs(x = "Duels", 
                                             y = "Shots Conceded",
                                             colour = "Points") +
                                        theme_minimal() + scale_colour_brewer(palette = "Greens") +
                                        theme(legend.position ="bottom")     })
  
  output$Interceptions <- renderPlot({ ggplot(MLSPlayerPlay %>% filter(PositionName == "Defense"), 
                                           aes(Interceptions, ShotsConceidedDifferential, colour = factor(Points))) + 
                                            geom_point() +
                                            labs(x = "Interceptions", 
                                                 y = "Shots Conceded",
                                                 colour = "Points") +
                                            theme_minimal() + scale_colour_brewer(palette = "Greens") +
                                            theme(legend.position ="bottom")     })
  
  
  ### MIDFIELDER TAB
   output$PlotMid <- renderPlot({ ggplot(MLSPlayerPlay, 
                                         aes(Position, GoalDifferential, fill = PositionName)) + 
                                        geom_bar(stat = "identity") +
                                        labs(x = "Position", 
                                             y = "Goals Differential ",
                                             title = "All Positions Contribute to Goals Differential",
                                             fill = "Mode of Play") +
                                        theme_minimal() + scale_fill_brewer(palette = "Blues") +
                                        theme(legend.position ="bottom")  })
   
   output$GoalDiffPlot <- renderPlot({ ggplot(MLSPlayerPlay %>% filter(PositionName == "Midfielder"), 
                                         aes(reorder(Team, GoalDifferential), GoalDifferential, fill = factor(Points))) + 
                                         geom_bar( stat = "identity") +
                                         labs(x = "Position", 
                                              y = "Goals Differential ",
                                              title = "Goals Differential per Team",
                                              fill = "Points") +
                                         theme_minimal() +  scale_fill_brewer(palette = "Greens") +
                                        theme(axis.text.x = element_text(angle = 90, hjust = 1))     })
   
   output$Assists <- renderPlot({ ggplot(MLSPlayerPlay %>% filter(PositionName == "Midfielder"), 
                                         aes(Assists, GoalDifferential, colour = factor(Position))) + 
                                         geom_point() +
                                         labs(x = "Assists", 
                                              y = "Goal Differential",
                                              colour = "Position") +
                                         theme_minimal() +  scale_colour_brewer(palette = "Blues") +
                                          theme(legend.position ="bottom")     })
   
   output$Recoveries <- renderPlot({ ggplot(MLSPlayerPlay %>% filter(PositionName == "Midfielder"), 
                                         aes(Recoveries, GoalDifferential, colour = factor(Position))) + 
                                           geom_point() +
                                           labs(x = "Recoveries", 
                                                y = "Goal Differential",
                                                colour = "Position") +
                                           theme_minimal() +  scale_colour_brewer(palette = "Blues") +
                                           theme(legend.position ="bottom")     })
   
  ### OFFENSE TAB
  output$PlotOffense <- renderPlot({ ggplot(MLSPlayerPlay, 
                                            aes(Position, Goals, fill = PositionName)) + 
                                            geom_bar(stat = "identity") +
                                            labs(x = "Position", 
                                                 y = "Goals", 
                                                 title = "Forwards Typically Contribute to Goals",
                                                 fill = "Mode of Play") +
                                            theme_minimal() +  scale_fill_brewer(palette = "Blues")  +
                                            theme(legend.position ="bottom") })
  
  output$GoalPlot <- renderPlot({ ggplot(MLSPlayerPlay[order(MLSPlayerPlay$WinningGoal), ] %>% 
                                           filter(PositionName == "Offense"),
                                         aes(reorder(Team, -Goals), Goals, fill = factor(Points), order = Points)) +
                                           geom_bar(stat = "identity") +
                                           labs( x = "Team", y = "Goals", title = "Goals Scored per Team", fill = "Points") +
                                           theme_minimal() +  scale_fill_brewer(palette = "Greens") +
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1))     })
  
  output$ChancesPlot <- renderPlot({  ggplot(MLSPlayerPlay %>% filter(PositionName == "Offense"), 
                                           aes(BigChances, GoalDifferential)) +
                                             geom_point(aes(colour = factor(Position))) +
                                            labs(x = "Big Chances", y = "Goal Differential", 
                                                 colour = "Position") +
                                            theme_minimal() +  scale_colour_brewer(palette = "Paired") +
                                            theme(legend.position ="bottom")   })
  
  output$OffenseShotsPlot <- renderPlot({  ggplot(MLSPlayerPlay %>% filter(PositionName == "Offense"), 
                                             aes(ShotsOnfromInsideBox, GoalDifferential)) +
                                            geom_point(aes(colour = factor(Position))) +
                                            labs(x = "Shots On Target", y = "Goal Differential", 
                                                 colour = "Position") +
                                            theme_minimal() +  scale_colour_brewer(palette = "Paired") +
                                            theme(legend.position ="bottom")   })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

