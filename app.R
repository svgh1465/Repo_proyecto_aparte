
#Base de datos utilizada 
library(readr)
library(tidyverse)
Base <- read_csv("Data_Science_Fields_Salary_Categorization.csv")
View(Base)
str(Base)
Base_nueva<- Base%>%
  mutate(Salary_In_USD= (Salary_In_Rupees/78.63))
View(Base_nueva)


library(shiny)
library(dplyr)
library(ggplot2)
library(readr)



# --- 2. UI ---
ui <- fluidPage(
  titlePanel("Explorador de Salarios en Ciencia de Datos"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("years", "Año de trabajo:",
                         choices = c(2020, 2021, 2022),
                         selected = c(2020, 2021, 2022)),
      
      checkboxGroupInput("exp", "Nivel de experiencia:",
                         choices = c("EN", "MI", "SE", "EX"),
                         selected = c("EN", "MI", "SE", "EX")),
      
      checkboxGroupInput("size", "Tamaño de empresa:",
                         choices = c("S", "M", "L"),
                         selected = c("S", "M", "L"))
    ),
    
    mainPanel(
      tabsetPanel(
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

# --- 3. SERVER ---
server <- function(input, output) {
  
  # Filtros globales
  filtered <- reactive({
    Base_nueva %>%
      filter(Working_Year %in% input$years,
             Experience %in% input$exp,
             Company_Size %in% input$size)
  })
  
  # --- Pestaña 3 ---
  output$boxplot_modalidad <- renderPlot({
    ggplot(filtered(), aes(x = factor(Remote_Working_Ratio,
                                      labels = c("Presencial","Híbrido","Remoto")),
                           y = Salary_In_USD)) +
      geom_boxplot(fill = "#007bc2") +
      labs(x = "Modalidad de trabajo", y = "Salario en USD")
  })
  
  output$summary_modalidad <- renderTable({
    filtered() %>%
      group_by(Remote_Working_Ratio) %>%
      summarise(Mediana = median(Salary_In_USD),
                Promedio = mean(Salary_In_USD),
                N = n())
  })
  
  # --- Pestaña 4 ---
  output$lineplot_evolucion <- renderPlot({
    filtered() %>%
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

# --- 4. Ejecutar app ---
shinyApp(ui, server)





quiero_pere<- "obvio"
