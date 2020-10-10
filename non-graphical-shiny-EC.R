library(shiny)
library(tidyverse)

#Global Information
df.votes <- read.csv("~/Stats/Shiny/EC/EC.csv")


# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Electoral College vs Popular Vote: Visualizing the Possibilities"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # State Vote Shares Sidebar ----
    sidebarPanel("Democratic Vote Shares",
                 
      # Input: Alabama Vote Share ----
      sliderInput("AL", "Alabama:",
                  min = 0, max = 100,
                  value = 50, step = .1,
                  post="%"),
      
      # Input: Alaska Vote Share ----
      sliderInput("AK", "Alaska:",
                  min = 0, max = 100,
                  value = 50, step = .1,
                  post="%"),
      
      # Input: Arizona Vote Share ----
      sliderInput("AZ", "Arizona:",
                  min = 0, max = 100,
                  value = 50, step = .1,
                  post="%"),
      
      # Input: Arizona Vote Share ----
      sliderInput("AR", "Arkansas:",
                  min = 0, max = 100,
                  value = 50, step = .1,
                  post="%"),
      
      # Input: California Vote Share ----
      sliderInput("CA", "California:",
                  min = 0, max = 100,
                  value = 50, step=.1,
                  post="%"),
      
      # Input: Colorado Vote Share ----
      sliderInput("CO", "Colorado:",
                  min = 0, max = 100,
                  value = 50, step=.1,
                  post="%"),
      
      # Input: Connecticut Vote Share ----
      sliderInput("CT", "Connecticut:",
                  min = 0, max = 100,
                  value = 50, step=.1,
                  post="%"),
      
      # Input: Delaware Vote Share ----
      sliderInput("DE", "Delaware:",
                  min = 0, max = 100,
                  value = 50, step=.1,
                  post="%"),
      
      # Input: New York Vote Share ----
      sliderInput("NY", "New York:",
                  min = 0, max = 100,
                  value = 50, step = 0.1,
                  post="%")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel("Results",
      
      # Output: Table summarizing the values entered ----
      tableOutput("values"),
      textOutput("dem_ec"),
      textOutput("rep_ec")
      
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----

  #Sum of Democrat EC Votes
  output$dem_ec <- renderText({
    paste0("Democratic Electoral Votes: ", (if_else(input$AL>=50,
                                                    df.votes$ec.votes[df.votes$state=="AL"],
                                                    as.integer(0)) +
                                              if_else(input$AK>=50,
                                                      df.votes$ec.votes[df.votes$state=="AK"],
                                                      as.integer(0)) +
                                              if_else(input$AZ>=50,
                                                      df.votes$ec.votes[df.votes$state=="AZ"],
                                                      as.integer(0)) +
                                              if_else(input$AR>=50,
                                                      df.votes$ec.votes[df.votes$state=="AR"],
                                                      as.integer(0)) +
                                              if_else(input$CA>=50,
                                                      df.votes$ec.votes[df.votes$state=="CA"],
                                                      as.integer(0)) + 
                                              if_else(input$CO>=50,
                                                      df.votes$ec.votes[df.votes$state=="CO"],
                                                      as.integer(0)) +
                                              if_else(input$CT>=50,
                                                      df.votes$ec.votes[df.votes$state=="CT"],
                                                      as.integer(0)) +
                                              if_else(input$NY>=50,
                                                      df.votes$ec.votes[df.votes$state=="NY"],
                                                      as.integer(0))))
  })
  #Sum of Republican EC Votes
  output$rep_ec <- renderText({
    paste0("Republican Electoral Votes: ",(if_else(input$AL<50, 
                                                   df.votes$ec.votes[df.votes$state=="AL"], 
                                                   as.integer(0)) +
                                             if_else(input$AK<50, 
                                                     df.votes$ec.votes[df.votes$state=="AK"], 
                                                     as.integer(0)) + 
                                             if_else(input$AZ<50, 
                                                     df.votes$ec.votes[df.votes$state=="AZ"], 
                                                     as.integer(0)) +
                                             if_else(input$AR<50, 
                                                     df.votes$ec.votes[df.votes$state=="AR"], 
                                                     as.integer(0)) +
                                             if_else(input$CA<50, 
                                                     df.votes$ec.votes[df.votes$state=="CA"], 
                                                     as.integer(0)) +
                                             if_else(input$CO<50, 
                                                     df.votes$ec.votes[df.votes$state=="CO"], 
                                                     as.integer(0)) +
                                             if_else(input$CT<50, 
                                                     df.votes$ec.votes[df.votes$state=="CT"], 
                                                     as.integer(0)) +
                                             if_else(input$NY<50, 
                                                     df.votes$ec.votes[df.votes$state=="NY"], 
                                                     as.integer(0))))
  })
  
  #Table with State Popular Vote and EC Votes
  sliderValues <- reactive({
    
    data.frame(
      State = c("Alabama",
                "Alaska",
                "Arizona",
                "Arkansas",
                "California",
                "Colorado",
                "Connecticut",
               "New York"),
      Dem.Pop = prettyNum(c(input$AL/100*df.votes$electorate[df.votes$state=="AL"],
                            input$AK/100*df.votes$electorate[df.votes$state=="AK"],
                            input$AZ/100*df.votes$electorate[df.votes$state=="AZ"],
                            input$AR/100*df.votes$electorate[df.votes$state=="AR"],
                            input$CA/100*df.votes$electorate[df.votes$state=="CA"], 
                            input$CO/100*df.votes$electorate[df.votes$state=="CO"],
                            input$CT/100*df.votes$electorate[df.votes$state=="CT"],
                            input$NY/100*df.votes$electorate[df.votes$state=="NY"]),
                          big.mark = ",",
                          scientific=FALSE),
      Rep.Pop = prettyNum(c((1-input$AL/100)*df.votes$electorate[df.votes$state=="AL"],
                            (1-input$AK/100)*df.votes$electorate[df.votes$state=="AK"],
                            (1-input$AZ/100)*df.votes$electorate[df.votes$state=="AZ"],
                            (1-input$AR/100)*df.votes$electorate[df.votes$state=="AR"],
                            (1-input$CA/100)*df.votes$electorate[df.votes$state=="CA"], 
                            (1-input$CO/100)*df.votes$electorate[df.votes$state=="CO"], 
                            (1-input$CO/100)*df.votes$electorate[df.votes$state=="CT"], 
                            (1-input$NY/100)*df.votes$electorate[df.votes$state=="NY"]),
                          big.mark = ",",
                          scientific=FALSE),
      Dem.EC = c(if_else(input$AL>=50,
                         df.votes$ec.votes[df.votes$state=="AL"],
                         as.integer(0)),
                 if_else(input$AK>=50,
                         df.votes$ec.votes[df.votes$state=="AK"],
                         as.integer(0)),
                 if_else(input$AZ>=50,
                         df.votes$ec.votes[df.votes$state=="AZ"],
                         as.integer(0)),
                 if_else(input$AR>=50,
                         df.votes$ec.votes[df.votes$state=="AR"],
                         as.integer(0)),
                 if_else(input$CA>=50,
                         df.votes$ec.votes[df.votes$state=="CA"],
                         as.integer(0)),
                 if_else(input$CO>=50,
                         df.votes$ec.votes[df.votes$state=="CO"],
                         as.integer(0)),
                 if_else(input$CT>=50,
                         df.votes$ec.votes[df.votes$state=="CT"],
                         as.integer(0)),
                 if_else(input$NY>=50,
                         df.votes$ec.votes[df.votes$state=="NY"],
                         as.integer(0))),
      Rep.EC = c(if_else(input$AL<50, 
                         df.votes$ec.votes[df.votes$state=="AL"], 
                         as.integer(0)),
                 if_else(input$AK<50, 
                         df.votes$ec.votes[df.votes$state=="AK"], 
                         as.integer(0)),
                 if_else(input$AZ<50, 
                         df.votes$ec.votes[df.votes$state=="AZ"], 
                         as.integer(0)),
                 if_else(input$AR<50, 
                         df.votes$ec.votes[df.votes$state=="AR"], 
                         as.integer(0)),
                 if_else(input$CA<50, 
                         df.votes$ec.votes[df.votes$state=="CA"], 
                         as.integer(0)),
                 if_else(input$CO<50, 
                         df.votes$ec.votes[df.votes$state=="CO"], 
                         as.integer(0)),
                 if_else(input$CT<50, 
                         df.votes$ec.votes[df.votes$state=="CT"], 
                         as.integer(0)),
                 if_else(input$NY<50,
                         df.votes$ec.votes[df.votes$state=="NY"],
                         as.integer(0))),
      stringsAsFactors = FALSE)
    
  })
  
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)