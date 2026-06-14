
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
library(bslib) 

library(shiny)

ui <- fluidPage(
  titlePanel("Explorador de Salarios en Ciencia de Datos"),
  
  sidebarLayout(
    sidebarPanel(
      # Filtros globales
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
