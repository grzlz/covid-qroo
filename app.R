# Dashboard covid quintana roo, primera version 
library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(readxl)
library(lubridate)
library(plotly)
# Get data
d <- read_excel("covid_qroo.xlsx")

d$fecha <- as_date(d$fecha)

p1 <- d %>% 
  ggplot(aes(x = fecha, y = positivos)) + geom_line() + labs(x = "Fecha", y = "Casos acumulados") +
  theme_classic()

p1 <- ggplotly(p1) %>% 
  config(displayModeBar = F) %>% 
  layout(xaxis = list(fixedrange = T)) %>% 
  layout(yaxis = list(fixedrange = T))

p2 <- d %>% 
  ggplot(aes(x = fecha, y = nuevos_confirmados)) + geom_col(fill = "darkblue") + 
  labs(x = "Fecha", y = "Casos nuevos por día") + theme_classic()

p2 <- ggplotly(p2) %>% 
  config(displayModeBar = F)%>% 
  layout(xaxis = list(fixedrange = T)) %>% 
  layout(yaxis = list(fixedrange = T)) 

p3 <- d %>% 
  ggplot(aes(x = fecha, y = nuevas_defunciones)) + geom_col(fill = "black") + 
  labs(x = "Fecha", y = "Defunciones por día") + theme_classic()

p3 <- ggplotly(p3) %>% 
  config(displayModeBar = F)%>% 
  layout(xaxis = list(fixedrange = T)) %>% 
  layout(yaxis = list(fixedrange = T))



nuevo_hoy <- d %>% 
  filter(fecha == max(d$fecha)) %>% 
  select(nuevos_confirmados) %>% 
  as.numeric()

total_acum <- d %>% 
  filter(fecha == max(d$fecha)) %>% 
  select(positivos) %>% 
  as.numeric()

fallecido_acum <- d %>% 
  filter(fecha == max(d$fecha)) %>% 
  select(defunciones) %>% 
  as.numeric()

fallecido_hoy <- d %>% 
  filter(fecha == max(d$fecha)) %>% 
  select(nuevas_defunciones) %>% 
  as.numeric()

# ShinyDashboard ----

header <- dashboardHeader(title = "Covid Q.Roo")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Overview", tabName = "over", icon = icon("dashboard")
    ),
    menuItem(
      "Detalle casos", tabName = "casos", icon = icon("money-bill")
    ),
    menuItem(
      "Detalle fallecidos", tabName = "fallecidos", icon = icon("credit-card")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "over",
            fluidRow(
              valueBoxOutput("vb1", width = 3),
              valueBoxOutput("vb2", width = 3),
              valueBoxOutput("vb3", width = 3),
              valueBoxOutput("vb4", width = 3)
            ),
            fluidRow(
              box(width = 12,
                  title = h4(strong("Casos confirmados acumulados")),
                  plotlyOutput("plot1"))
            ),
            fluidRow(
              column(width = 6,
                box(width = NULL,
                    title = h4(strong("Casos confirmados por día")),
                    plotlyOutput("plot2"))
              ),
              column(width = 6,
                box(width = NULL,
                    title = h4(strong("Fallecimientos por día")),
                    plotlyOutput("plot3")
                    )
              )
            )
    ),
    tabItem(tabName = "casos")
    
  )
)

ui <- dashboardPage(skin = "green", header, sidebar, body)

server <- function(input, output){
  
  output$plot1 <- renderPlotly({
   p1 
  })
  
  output$plot2 <- renderPlotly({
    p2
  })
  
  output$plot3 <- renderPlotly({
    p3
  })
  
  output$vb1 <- renderValueBox({
    valueBox(total_acum, h4("Casos totales"), color = "red", icon = icon("credit-card"))
  })
  
  output$vb2 <- renderValueBox({
    valueBox(fallecido_acum, h4("Fallecidos totales"), color = "black", icon = icon("percent"))
  })
  
  output$vb3 <- renderValueBox({
    valueBox(nuevo_hoy, h4("Casos nuevos hoy"), color = "green", icon = icon("mouse-pointer"))
  })
  
  output$vb4 <- renderValueBox({
    valueBox(fallecido_hoy, h4("Fallecidos hoy"), color = "black", icon = icon("percent"))
  })
}

shinyApp(ui = ui, server = server)
