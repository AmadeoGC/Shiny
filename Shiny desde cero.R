library(shiny)
library(babynames)
library(tidyverse)


# P1
ui <- fluidPage(
  "Hello, World"
)

server <- function(input, output, session){
  
}

shinyApp(ui=ui, server=server)

#------------------------------------------------------------

# P2
ui <- fluidPage(textInput("name", "Enter a name:"),
                textOutput("q")
  
)

server <- function(input, output){
  output$q <- renderText({
    paste("Do you prefer dogs or cats,", input$name, "?")
  })
}


shinyApp(ui=ui, server=server)


#------------------------------------------------------------

# P3

ui <- fluidPage(
  titlePanel("Baby Name Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enater Name", "Marimar")
      ),

    mainPanel(
      plotOutput("trend")
    )
  )
)


server <- function(input, output){
  output$trend <- renderPlot({
    
    data_name <- subset(babynames, name == input$name)
    
    ggplot(data_name) +
      geom_line(aes(x=year, y=n, color=sex))
  })
}


shinyApp(ui=ui, server=server)



# P3b

ui <- fluidPage(
  titlePanel("Baby Name Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enater Name", "Marimar")
    ),
    
    mainPanel(
      plotOutput("trend")
    )
  )
)


server <- function(input, output){
  output$trend <- renderPlot({
    
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x=year, y=n, color=sex))
  })
}


shinyApp(ui=ui, server=server)
