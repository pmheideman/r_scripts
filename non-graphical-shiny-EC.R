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
    sidebarPanel("Democratic Share of Two-Party Vote",
                 
      # Input: Alabama Vote Share ----
      sliderInput("AL", "Alabama:",
                  min = 0, max = 100,
                  value = 35.6, step = .1,
                  post="%"),
      
      # Input: Alaska Vote Share ----
      sliderInput("AK", "Alaska:",
                  min = 0, max = 100,
                  value = 41.6, step = .1,
                  post="%"),
      
      # Input: Arizona Vote Share ----
      sliderInput("AZ", "Arizona:",
                  min = 0, max = 100,
                  value = 48.1, step = .1,
                  post="%"),
      
      # Input: Arizona Vote Share ----
      sliderInput("AR", "Arkansas:",
                  min = 0, max = 100,
                  value = 35.7, step = .1,
                  post="%"),
      
      # Input: California Vote Share ----
      sliderInput("CA", "California:",
                  min = 0, max = 100,
                  value = 66.1, step=.1,
                  post="%"),
      
      # Input: Colorado Vote Share ----
      sliderInput("CO", "Colorado:",
                  min = 0, max = 100,
                  value = 52.7, step=.1,
                  post="%"),
      
      # Input: Connecticut Vote Share ----
      sliderInput("CT", "Connecticut:",
                  min = 0, max = 100,
                  value = 57.1, step=.1,
                  post="%"),
      
      # Input: Delaware Vote Share ----
      sliderInput("DE", "Delaware:",
                  min = 0, max = 100,
                  value = 56, step=.1,
                  post="%"),
      
      # Input: Florida Vote Share ----
      sliderInput("FL", "Florida:",
                  min = 0, max = 100,
                  value = 49.4, step=.1,
                  post="%"),
      
      # Input: Georgia Vote Share ----
      sliderInput("GA", "Georgia:",
                  min = 0, max = 100,
                  value = 47.3, step=.1,
                  post="%"),
      
      # Input: Hawaii Vote Share ----
      sliderInput("HI", "Hawaii:",
                  min = 0, max = 100,
                  value = 67.4, step=.1,
                  post="%"),
      
      # Input: Idaho Vote Share ----
      sliderInput("ID", "Idaho:",
                  min = 0, max = 100,
                  value = 31.7, step=.1,
                  post="%"),
      
      # Input: Illinois Vote Share ----
      sliderInput("IL", "Illinois:",
                  min = 0, max = 100,
                  value = 59.0, step=.1,
                  post="%"),
      
      # Input: Indiana Vote Share ----
      sliderInput("IN", "Indiana:",
                  min = 0, max = 100,
                  value = 39.9, step=.1,
                  post="%"),
      
      # Input: New York Vote Share ----
      sliderInput("NY", "New York:",
                  min = 0, max = 100,
                  value = 63.4, step = 0.1,
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
                                              if_else(input$DE>=50,
                                                      df.votes$ec.votes[df.votes$state=="DE"],
                                                      as.integer(0)) +
                                              if_else(input$FL>=50,
                                                      df.votes$ec.votes[df.votes$state=="FL"],
                                                      as.integer(0)) +
                                              if_else(input$GA>=50,
                                                      df.votes$ec.votes[df.votes$state=="GA"],
                                                      as.integer(0)) +
                                              if_else(input$HI>=50,
                                                      df.votes$ec.votes[df.votes$state=="HI"],
                                                      as.integer(0)) +
                                              if_else(input$ID>=50,
                                                      df.votes$ec.votes[df.votes$state=="ID"],
                                                      as.integer(0)) +
                                              if_else(input$IL>=50,
                                                      df.votes$ec.votes[df.votes$state=="IL"],
                                                      as.integer(0)) +
                                              if_else(input$IN>=50,
                                                      df.votes$ec.votes[df.votes$state=="IN"],
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
                                             if_else(input$DE<50, 
                                                     df.votes$ec.votes[df.votes$state=="DE"], 
                                                     as.integer(0)) +
                                             if_else(input$FL<50, 
                                                     df.votes$ec.votes[df.votes$state=="FL"], 
                                                     as.integer(0)) +
                                             if_else(input$GA<50, 
                                                     df.votes$ec.votes[df.votes$state=="GA"], 
                                                     as.integer(0)) +
                                             if_else(input$HI<50, 
                                                     df.votes$ec.votes[df.votes$state=="HI"], 
                                                     as.integer(0)) +
                                             if_else(input$ID<50, 
                                                     df.votes$ec.votes[df.votes$state=="ID"], 
                                                     as.integer(0)) +
                                             if_else(input$IL<50, 
                                                     df.votes$ec.votes[df.votes$state=="IL"], 
                                                     as.integer(0)) +
                                             if_else(input$IN<50, 
                                                     df.votes$ec.votes[df.votes$state=="IN"], 
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
                "Delaware",
                "Florida",
                "Georgia",
                "Hawaii",
                "Idaho",
                "Illinois",
                "Indiana",
               "New York"),
      Dem.Pop = prettyNum(round(c(input$AL/100*df.votes$electorate[df.votes$state=="AL"],
                            input$AK/100*df.votes$electorate[df.votes$state=="AK"],
                            input$AZ/100*df.votes$electorate[df.votes$state=="AZ"],
                            input$AR/100*df.votes$electorate[df.votes$state=="AR"],
                            input$CA/100*df.votes$electorate[df.votes$state=="CA"], 
                            input$CO/100*df.votes$electorate[df.votes$state=="CO"],
                            input$CT/100*df.votes$electorate[df.votes$state=="CT"],
                            input$DE/100*df.votes$electorate[df.votes$state=="DE"],
                            input$FL/100*df.votes$electorate[df.votes$state=="FL"],
                            input$GA/100*df.votes$electorate[df.votes$state=="GA"],
                            input$HI/100*df.votes$electorate[df.votes$state=="HI"],
                            input$ID/100*df.votes$electorate[df.votes$state=="ID"],
                            input$IL/100*df.votes$electorate[df.votes$state=="IL"],
                            input$IN/100*df.votes$electorate[df.votes$state=="IN"],
                            input$NY/100*df.votes$electorate[df.votes$state=="NY"])),
                          big.mark = ",",
                          scientific=FALSE),
      Rep.Pop = prettyNum(round(c((1-input$AL/100)*df.votes$electorate[df.votes$state=="AL"],
                            (1-input$AK/100)*df.votes$electorate[df.votes$state=="AK"],
                            (1-input$AZ/100)*df.votes$electorate[df.votes$state=="AZ"],
                            (1-input$AR/100)*df.votes$electorate[df.votes$state=="AR"],
                            (1-input$CA/100)*df.votes$electorate[df.votes$state=="CA"], 
                            (1-input$CO/100)*df.votes$electorate[df.votes$state=="CO"], 
                            (1-input$CT/100)*df.votes$electorate[df.votes$state=="CT"], 
                            (1-input$DE/100)*df.votes$electorate[df.votes$state=="DE"], 
                            (1-input$FL/100)*df.votes$electorate[df.votes$state=="FL"], 
                            (1-input$GA/100)*df.votes$electorate[df.votes$state=="GA"], 
                            (1-input$HI/100)*df.votes$electorate[df.votes$state=="HI"],
                            (1-input$HI/100)*df.votes$electorate[df.votes$state=="ID"],
                            (1-input$IL/100)*df.votes$electorate[df.votes$state=="IL"],
                            (1-input$IN/100)*df.votes$electorate[df.votes$state=="IN"],
                            (1-input$NY/100)*df.votes$electorate[df.votes$state=="NY"])),
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
                 if_else(input$DE>=50,
                         df.votes$ec.votes[df.votes$state=="DE"],
                         as.integer(0)),
                 if_else(input$FL>=50,
                         df.votes$ec.votes[df.votes$state=="FL"],
                         as.integer(0)),
                 if_else(input$GA>=50,
                         df.votes$ec.votes[df.votes$state=="GA"],
                         as.integer(0)),
                 if_else(input$HI>=50,
                         df.votes$ec.votes[df.votes$state=="HI"],
                         as.integer(0)),
                 if_else(input$ID>=50,
                         df.votes$ec.votes[df.votes$state=="ID"],
                         as.integer(0)),
                 if_else(input$IL>=50,
                         df.votes$ec.votes[df.votes$state=="IL"],
                         as.integer(0)),
                 if_else(input$IN>=50,
                         df.votes$ec.votes[df.votes$state=="IN"],
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
                 if_else(input$DE<50, 
                         df.votes$ec.votes[df.votes$state=="DE"], 
                         as.integer(0)),
                 if_else(input$FL<50, 
                         df.votes$ec.votes[df.votes$state=="FL"], 
                         as.integer(0)),
                 if_else(input$GA<50, 
                         df.votes$ec.votes[df.votes$state=="GA"], 
                         as.integer(0)),
                 if_else(input$HI<50, 
                         df.votes$ec.votes[df.votes$state=="HI"], 
                         as.integer(0)),
                 if_else(input$ID<50, 
                         df.votes$ec.votes[df.votes$state=="ID"], 
                         as.integer(0)),
                 if_else(input$IL<50, 
                         df.votes$ec.votes[df.votes$state=="IL"], 
                         as.integer(0)),
                 if_else(input$IN<50, 
                         df.votes$ec.votes[df.votes$state=="IN"], 
                         as.integer(0)),
                 if_else(input$NY<50,
                         df.votes$ec.votes[df.votes$state=="NY"],
                         as.integer(0))),
      stringsAsFactors = FALSE)
    
  })
  
  
  # Render the table in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)