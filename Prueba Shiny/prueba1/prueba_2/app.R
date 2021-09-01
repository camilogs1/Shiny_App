library(shiny)
library(shinythemes)
library(datasets)

ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage(
                    "Prueba",
                    tabPanel("pestana 1",
                             sidebarPanel(
                                 tags$h3("Ejemplo:"),
                                 textInput("txt1", "Nombre:", ""),
                                 textInput("txt2", "Apellido:", ""),
                             ), 
                             mainPanel(
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Plot", plotOutput("plot")),
                                             tabPanel("Summary", verbatimTextOutput("summary")),
                                             tabPanel("Table", tableOutput("table")))
                             ) 
                    ), 
                    tabPanel("pestana 2",
                             sidebarPanel(
                                 selectInput("region", "Region:", 
                                             choices=colnames(WorldPhones)),
                                 hr(),
                                 helpText("Data from AT&T (1961) The World's Telephones.")
                             ),
                             mainPanel(
                                 plotOutput("phonePlot"))),
                             
                    tabPanel("pestana 3", 
                             fluidRow(
                                 column(3, wellPanel(
                                     sliderInput("n", "N:", min = 10, max = 1000, value = 200,
                                                 step = 10)
                                 )),
                                 column(6,
                                        plotOutput("plot1", width = 400, height = 300),
                                        verbatimTextOutput("text")
                                 )
                             )),
                    
                    tabPanel("pestana 4",
                    navbarPage(
                        title = 'DataTable Options',
                        tabPanel('Length menu',        DT::dataTableOutput('ex2')),
                        tabPanel('No pagination',      DT::dataTableOutput('ex3')),
                        tabPanel('No filtering',       DT::dataTableOutput('ex4')),
                        tabPanel('Function callback',  DT::dataTableOutput('ex5'))
                    ))
                ) 
) 
 
server <- function(input, output) {
    
    output$txtout <- renderText({
        paste( input$txt1, input$txt2, sep = " " )
    })
    
    output$phonePlot <- renderPlot({
        barplot(WorldPhones[,input$region]*1000, 
                main=input$region,
                ylab="Number of Telephones",
                xlab="Year")
    })
    
    output$ex1 <- DT::renderDataTable(
        DT::datatable(iris, options = list(pageLength = 25))
    )
    
    output$ex2 <- DT::renderDataTable(
        DT::datatable(
            iris, options = list(
                lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                pageLength = 15
            )
        )
    )
    
    output$ex3 <- DT::renderDataTable(
        DT::datatable(iris, options = list(paging = FALSE))
    )
    
    output$ex4 <- DT::renderDataTable(
        DT::datatable(iris, options = list(searching = FALSE))
    )
    
    output$ex5 <- DT::renderDataTable(DT::datatable(
        iris,
    ))
    
    output$plot1 <- renderPlot({
        hist(rnorm(input$n))
    })

} 

shinyApp(ui = ui, server = server)