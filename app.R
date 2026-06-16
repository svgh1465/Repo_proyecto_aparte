
#Base de datos utilizada 
library(readr)
library(tidyverse)
Base <- read_csv("Data_Science_Fields_Salary_Categorization.csv")
View(Base)
str(Base)
Base_nueva<- Base%>%
  mutate(Salary_In_USD= (Salary_In_Rupees/78.63))
View(Base_nueva)


# PARTE 1: estructura base + sidebar + pestaña 1
# a cargo de kiany

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)

# datos que tienen ruta relativa dentro del proyecto
datos <- read_csv("Data_Science_Fields_Salary_Categorization.csv")

# quitar separador de miles de variable "Salary_In_RUpees"
datos$Salary_In_Rupees <- as.numeric(gsub(",","",
                                          datos$Salary_In_Rupees))

# VARIABLE DERIVADA: 
# rupias a dólares con tasa fija de 1USD=78.63INR (promedio RBI 2022)
datos$Salary_In_USD <- round(datos$Salary_In_Rupees / 78.63, 2)

# variables en factor para los gráficos
datos$Experience <- factor(datos$Experience,
                           levels = c("EN","MI","SE","EX"))
datos$Company_Size <- factor(datos$Company_Size,
                             levels = c("S","M","L"))
datos$Working_Year <- as.integer(datos$Working_Year)



# INTERFAZ 

ui <- fluidPage(
  titlePanel("Explorador de salarios en Ciencia de Datos"),
  
  # filtros globales 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtros globales"),
      hr(),
      
      #filtro por año
      checkboxGroupInput(
        inputId = "filtro_año",
        label = "Año de trabajo:",
        choices = c("2020","2021","2022"),
        selected = c("2020","2021","2022")
      ),
      
      hr(),
      
      # filtro por nivel experiencia
      checkboxGroupInput(
        inputId = "filtro_experiencia",
        label = "Nivel de experiencia:",
        choices = c("EN (júnior)" = "EN",
                    "MI (intermedio)" = "MI",
                    "SE (sénior)" = "SE",
                    "EX (ejecutivo)" = "EX"),
        selected = c("EN","MI","SE","EX")
      ),
      
      hr(),
      
      # filtro por tamaño empresa
      checkboxGroupInput(
        inputId = "filtro_tamaño",
        label = "Tamaño de empresa:",
        choices = c("S (pequeña)" = "S",
                    "M (mediana)" = "M",
                    "L (grande)" = "L"),
        selected = c("S","M","L")
      ),
      
      hr(),
      
      # indicador registros activos en c/filtro
      h5("Registros activos:"),
      textOutput("n_registros")
    ),
    
    
    # PRINCIPAL: LAS PESTAÑAS
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id="pestañas",
        
        # PESTAÑA 1 por hacer 
        
        # demás pestañas 
       
            # --- Pestaña 3 ---
            tabPanel("Modalidad de trabajo y salario",
                     plotOutput("boxplot_modalidad"),
                     tableOutput("summary_modalidad")),
            
            # --- Pestaña 4 ---
            tabPanel("Evolución temporal del salario",
                     plotOutput("lineplot_evolucion"),
                     textOutput("nota_metodologica"))
      )
    )
  )
)



# SERVIDOR / LÓGICA

server <- function(input, output, session){
  
  # los datos reactivos aplica los fltros globales
  # esto lo pueden usar TODAS las pestañas
  datos_filtrados <- reactive({
    datos |>
      filter(
        Working_Year %in% as.integer(input$filtro_año),
        Experience %in% input$filtro_experiencia,
        Company_Size %in% input$filtro_tamaño
      )
  })
  
  # contador de registros activos
  output$n_registros <- renderText({
    paste(nrow(datos_filtrados()), "de", nrow(datos), "registros.")
  })
  
  # log pestaña 1 
  
  # Filtros globales
  
  # --- Pestaña 3 ---
  output$boxplot_modalidad <- renderPlot({
    datos_filtrados() %>%
      ggplot(aes(x = factor(Remote_Working_Ratio,
                            levels = c(0,50,100),
                            labels = c("Presencial","Híbrido","Remoto")),
                 y = Salary_In_USD)) +
      geom_boxplot(fill = "lightblue") +
      labs(x = "Modalidad de trabajo", y = "Salario en USD")
  })
  
  output$summary_modalidad <- renderTable({
    datos_filtrados() %>%
      group_by(Remote_Working_Ratio) %>%
      summarise(
        Mediana = median(Salary_In_USD),
        Promedio = mean(Salary_In_USD),
        N = n(),
        .groups = "drop"
      )
  })
  
  # --- Pestaña 4 ---
  output$lineplot_evolucion <- renderPlot({
    datos_filtrados() %>%
      group_by(Working_Year, Experience) %>%
      summarise(Promedio = mean(Salary_In_USD), .groups = "drop") %>%
      ggplot(aes(x = Working_Year, y = Promedio, color = Experience)) +
      geom_line(size = 1.2) +
      geom_point() +
      labs(x = "Año", y = "Salario promedio (USD)")
  })
  
  output$nota_metodologica <- renderText({
    "Nota: los años 2020 y 2021 tienen menos observaciones que 2022, lo que puede afectar la estabilidad de las estimaciones promedio."
  })
  
}



# app para terminar
shinyApp(ui=ui, server = server)



