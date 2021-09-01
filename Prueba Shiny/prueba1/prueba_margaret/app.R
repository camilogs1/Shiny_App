library(shiny)
library(shinythemes)
library(datasets)
library(tidyverse)
library(here)
library(DT)
library(plotly)
library(readxl)
library(crosstalk)
library(stringi)

articulos_unicos_2016_2020 <- 
  read_csv(here("output",
                "articulos.csv")) |> 
  filter(ano >= 2016,
         ano <=2020)

investigadores_general <- 
  read_csv(here("output",
                "investigadores.csv")) |> 
  mutate(url = str_c("<a href=","\"",
                     url,
                     "\"",
                     ">Link</a>"),
         scholar = str_c("<a href=","\"",
                         "https://scholar.google.com/citations?user=",
                         id_scholar,
                         "\"",
                         ">Scholar</a>")) |>
  select(-vinculacion,
         -fin_vinculacion) |> 
  rename(Investigador = integrantes,
         Produccion = total_papers,
         Horas = horas_dedicacion,
         CvLAC = url,
         Grupo = grupo,
         Inicio = inicio_vinculacion,
         Formacion = posgrade,
  ) |> 
  select(Investigador,
         Grupo,
         Produccion,
         h_index,
         clasification,
         Formacion,
         Inicio,
         CvLAC,
         scholar) |> 
  datatable(filter = 'top', options = list(pageLength = 15),
            escape = FALSE,
            colnames = c("Investigador", "Grupo", "Artículos", "H index",
                         "Categoría","Formación","Inicio", "CvLAC", "Scholar"))

grupos_general <- 
  read_csv(here("output",
                "grupos_general.csv")) |> 
  select(grupo, clasificacion, sum_papers, departamento , url,
         fecha_creacion,lider, email, area_conocimiento_1) |> 
  mutate(url= str_c("<a href=",
                    url,
                    ">Link</a>")) |> 
  datatable(filter = 'top', options = list(columnDefs = list(list(className = 'dt-center', targets = 5)),
                           pageLength = 15),
            escape = FALSE,
            colnames = c("Grupo", "Clasificación", "Cantidad artículos",
                         "Departamento","GrupLAC", 
                         "Fecha Creación", "Líder", "Email", 
                         "Área de Conocimiento"))

paises_general <- articulos_unicos_2016_2020 |> 
  count(pais_revista, sort = TRUE)
paises_general$porcentaje <- round(prop.table(paises_general$n),3)*100 
paises_general <- paises_general |>  
  mutate(porcentaje = str_c(porcentaje," %"),
         pais_revista = if_else(is.na(pais_revista), "No registra", pais_revista)) |> 
  datatable(options = list(pageLength = 15),
            escape = FALSE,
            colnames = c("País", "Cantidad", "Porcentaje"))

#revistas

#fin revistas

articulos_2016_2020 <- 
  read_csv(here("output",
                "articulos.csv")) |> 
  filter(ano >= 2016,
         ano <=2020) |> 
  select(-id) |> 
  mutate(DOI = str_extract(DOI, "\\d.*")) |> 
  mutate(DOI =  str_c("<a href=","\"",
                      "https://doi.org/",
                      DOI,
                      "\"",
                      ">Enlace</a>")) |>  
  datatable(filter = 'top',options = list(pageLength = 15),
            escape = FALSE,
            colnames = c("Grupo", "Categoría", "Tipo producto",
                         "Título", "País revista", "Revista", 
                         "ISSN","Casiflicación Revista", "Año", "Volumen",
                         "Fasc","Paginas", "Enlace artículo", "Autores"))

capitulos_2016_2020 <- 
  read_csv(here("output",
                "capitulos.csv")) |> 
  filter(ano >= 2016,
         ano <=2020) |> 
  select(-vol, -tipo_producto) |> 
  datatable(filter = 'top', options = list(pageLength = 15),
            escape = FALSE,
            colnames = c("Grupo", "Categoría",
                         "Título capitulo", "País", "Año", 
                         "Titulo libro","ISBN", "Paginas", "Editorial",
                         "Autores"))

libros_2016_2020 <- 
  read_csv(here("output",
                "libros.csv")) |> 
  filter(Ano >= 2016,
         Ano <=2020) |> 
  select(-Tipo_producto) |> 
  datatable(filter = 'top', options = list(pageLength = 15),
            escape = FALSE,
            colnames = c("Grupo", "Categoría",
                         "Título libro", "País", "Año", 
                         "ISBN","Editorial", "Autores"))

software_2016_2020 <- 
  read_csv(here("output",
                "softwares.csv")) |> 
  filter(ano >= 2016,
         ano <=2020) |> 
  select(-nombre_proyecto, -tipo_producto) |> 
  mutate(sitio_web= str_c("<a href=",
                          sitio_web,
                          ">Link</a>")) |>
  datatable(filter = 'top', options = list(pageLength = 15),
            escape = FALSE,
            colnames = c("Grupo", "Categoría",
                         "Título", "País", "Año", 
                         "Disponibilidad","Sitio web", "Nombre comercial", "Institución financiadora", 
                         "Autores"))

trabajo_2016_2020 <- 
  read_csv(here("output",
                "trabajos_dirigidos.csv")) |> 
  mutate(hasta = str_remove(hasta, ".* "),
         hasta = str_trim(hasta),
         desde = str_remove(desde, "\\d.* "),
         desde = str_trim(desde)) |> 
  filter(desde >= 2016,
         hasta <=2020)

innovacion_2016_2020 <- 
  read_csv(here("output",
                "innovaciones_gestion.csv")) |> 
  filter(ano >= 2016,
         ano <=2020) |> 
  datatable(filter = 'top', options = list(pageLength = 15),
            escape = FALSE,
            colnames = c("Grupo", "Categoría", "Tipo Producto",
                         "Título", "país", "Año", 
                         "Disponibilidad","Institución financiadora", "Autores"))
#fin

# dataframe filtros
grupos <- articulos_unicos_2016_2020 |> 
  select(grupo) |> 
  unique()

ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  "Margaret",
                  tabPanel("General",
                           tabsetPanel(type = "tabs", id = "a",
                                       tabPanel("Grupos", fluidPage((DT::dataTableOutput('ex1'))
                                                ),),
                                       
                                       tabPanel("Investigadores", fluidPage((selectInput("grupos", "Grupos:", 
                                                                                         c(grupos$grupo)))
                                                ),(DT::dataTableOutput('ex2')),
                                                ),
                                       
                                       tabPanel("Paises", fluidPage((DT::dataTableOutput('ex3'))
                                       ),),
                                       
                                       tabPanel("Revistas", fluidPage((DT::dataTableOutput('ex4'))
                                       ))),
                           ),
                  tabPanel("Producción científica",
                           tabsetPanel(type = "tabs",
                                       tabPanel("Articulos", fluidPage((DT::dataTableOutput('articulo'))
                                                )),
                                       tabPanel("Capitulos", fluidPage((DT::dataTableOutput('capitulo'))
                                                )),
                                       tabPanel("Libros", fluidPage((DT::dataTableOutput('libro'))
                                                )),
                                       tabPanel("Software", fluidPage((DT::dataTableOutput('software'))
                                                )),
                                       tabPanel("Innovaciones", fluidPage((DT::dataTableOutput('innovaciones'))
                                                )),
                                       tabPanel("Trabajos dirigidos/Tutorías",
                                                fluidPage((DT::dataTableOutput('trabajosd'))
                                                )))
                           ),
                        
                  tabPanel("Grupos e investigadores en cifras", 
                           navbarPage(
                             tabPanel("grupos")
                           )),
                  
                  tabPanel("Grupos individual en cifras",
                           navbarPage(
                             tabPanel("grupos")
                           ))
                ) 
) 

server <- function(input, output) {
  
  output$ex1 <- DT::renderDataTable({
    grupos_general
  })
  
  output$ex2 <- DT::renderDataTable({
    investigadores_general
  })
  
  output$ex3 <- DT::renderDataTable({
    paises_general
  })
  
  output$articulo <- DT::renderDataTable({
    articulos_2016_2020
      
  })
  
  output$capitulo <- DT::renderDataTable({
    capitulos_2016_2020
  })
  
  output$libro <- DT::renderDataTable({
    libros_2016_2020
  })
  
  output$software <- DT::renderDataTable({
    software_2016_2020
  })
  
  output$innovaciones <- DT::renderDataTable({
    innovacion_2016_2020
  })
  
  output$trabajosd <- DT::renderDataTable({
    trabajo_2016_2020
  })

} 

shinyApp(ui = ui, server = server)