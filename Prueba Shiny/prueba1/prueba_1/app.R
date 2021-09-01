library(shiny)
library(tidyverse)
library(here)
library(DT)
library(plotly)
library(readxl)
library(shiny)
library(crosstalk)
library(stringi)

articulos_unicos_2016_2020 <- 
    read_csv(here("output",
                  "articulos.csv")) |> 
    filter(ano >= 2016,
           ano <=2020)

data7 <- articulos_unicos_2016_2020 |> 
    select(categoria, grupo) |> 
    count(grupo, sort = FALSE, name = "producciones")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("margaret"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        checkboxGroupInput(data7, "grupo", ("Grupo")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        barplot(data7)
    })
}

ui <- basicPage(
    h1("barras"),
    plotOutput("distPlot")
)

# Run the application 
shinyApp(ui = ui, server = server)
