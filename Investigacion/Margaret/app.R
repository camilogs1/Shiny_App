ui <- dashboardPage(
    dashboardHeader(title = "Investigación"),
    dashboardSidebar(
        sidebarMenu(
            menuItemOutput("certificado")
        )
    ),
    dashboardBody( 
        fluidRow(
        column(8, align="center", offset = 2,
               textInput("txt", "Cedula"),
               actionButton("button", "Buscar")
        )))
)

server <- function(input, output) {
    output$certificado <- renderMenu({
        menuItem("Certificado", icon = icon("book-open"))
    })
}

shinyApp(ui, server)