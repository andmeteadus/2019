library(shiny)
library(ggplot2)
library(readr)
library(tidyr)
library(knitr)
library(dplyr)
library(grid)
library(RCurl)
library(tibble)
library(DT)
library(shinydashboard)

alkohol <- read.csv("data/alkohol.csv") %>% filter(productClass != "null")
alkohol$regEntryDate <- as.Date(factor(alkohol$regEntryDate), format = "%Y-%m-%d")
levels(alkohol$producerCountry)[61]<- "Muu"
levels(alkohol$capacity)[91]<- "muu"

joonistaAeg <- function(andmed){
  joonis <- ggplot(andmed %>% count(regEntryDate), aes(x=regEntryDate, n)) + geom_point(fill="black", alpha=0.5, size=2) + geom_line(color="blue", alpha=0.5) +
  scale_x_date(date_labels = "%b - %Y") + labs(x="Registreerimise aeg", y="Arv") + theme_classic()
    
  
  return(joonis)
}

joonistaRiik <- function(andmed){
  alkoholGrupp <- andmed %>% group_by(producerCountry) %>% mutate(count= n())
  joonis <- ggplot(alkoholGrupp, aes(reorder(producerCountry, count))) + geom_histogram(stat = "count", aes( fill = ..count..))  +
    theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +
    labs(fill = "Loendus")
  return(joonis)
}

joonistaSisseostjaTop10 <- function(andmed){
  alkoholGrupp <- andmed %>% group_by(applicantName) %>% mutate(count= n())
  joonis <- ggplot(alkoholGrupp, aes(reorder(applicantName, count))) + geom_histogram(stat = "count", aes( fill = ..count..))  + 
    theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +
    labs(fill = "Loendus")
  return(joonis)
}

joonistaEthanol <- function(andmed){
  mean <- sum(andmed$ethanolRate) / nrow(andmed)
  joonis <- ggplot(andmed, aes(ethanolRate)) + geom_density(alpha = 0.3, fill = "blue", color = "blue") + 
    geom_vline(aes(xintercept=mean), color = "black", linetype="dashed") + theme_classic() + xlab("Etanooli sisaldus (%)") + 
    theme(axis.title.y=element_blank())
  return(joonis)
}

# Define UI for application
ui <- dashboardPage(
   dashboardHeader(title = "Riiklik alkoholiregister"),
   
   #Sidebar
   dashboardSidebar(
     sidebarMenu(
       menuItem("Info", tabName = "Info", icon = icon("info")),
       menuItem("Toote otsing", tabName = "Otsing", icon = icon("search")),
       menuItem("Statistika", tabName = "Statistika", icon = icon("bar-chart-o")),
       menuItem(selectInput("liik", "Alokoholi klass", choices = sort(unique(alkohol$productClass))))
       
     )
   ),
   
   dashboardBody(
     tabItems(
       
       tabItem(tabName = "Info",
               fluidRow(
                 box(title = "Riikliku alkoholiregistri rakendus", status = "primary", "Tegu on rakendusega, mis võimaldab kasutajal otsida tooteid riiklikku alkoholiregistrisse kantud jookide seast ning näha andmetel põhinevat statistikat. Registri pidamise eesmärk on korrastatud andmekogu loomine Eestis toodetava ja Eestisse toimetatava alkoholi kohta ja «Alkoholiseaduse» § 8 lõikes 1 määratud ülesannete täitmine.", br(), br(),
                     "Rakendus loodi Tartu Ülikooli 2018. aasta kursuse Statistiline visualiseerimine ja andmeteadus raames.",br(),br(),"Põnevat katsetamist!"
                     ), 
                 infoBox(title ="Autor", "Ingel-Brit Parmas", icon = icon("user")),
                 infoBox(title = "Andmed", "31.03.2018", icon = icon("table"))
               )    
       ),
       
    
       tabItem(tabName = "Statistika",
               fluidRow(
                 tabBox(
                   tabPanel(title = "Tootja riik", plotOutput("graafik", height = 500)), 
                   tabPanel(title = "Etanooli sisaldus", plotOutput("graafik3", height = 500)),
                   tabPanel(title = "Registreerimisaeg", plotOutput("graafik4", height = 500)),
                   tabPanel(title = "Taotleja nimi", plotOutput("graafik2", height = 500)), width=12
                   ))
          
       ),
       
      
       tabItem(tabName = "Otsing",
               h2("Toote otsing alkoholiregistrist"), 
               fluidRow(
                 box(selectInput("riik", "Vali tootja riik", choices = sort(unique(alkohol$producerCountry))),
                     sliderInput("etanol", "Etanoli sisaldus", 1, 100, c(30,50)),
                     selectInput("kogus", "Vali kogus", choices = c("Kõik",sort(unique(levels(alkohol$capacity))))))
               ),
               fluidRow(box(dataTableOutput("vastused")))
              
       )
     )
   )
)

server <- function(input, output, session) {
  
  observeEvent(
    input$liik,
    updateSelectInput(session, "riik", "riik", 
                      choices = sort(unique(alkohol$producerCountry[alkohol$productClass==input$liik]))))
  
  
  observeEvent(
    input$riik,
    updateSelectInput(session, "kogus", "kogus", 
                      choices = c("Kõik", 
                                  levels(droplevels(sort(unique(alkohol$capacity[alkohol$productClass==input$liik & alkohol$producerCountry==input$riik])))))))
  
  observe({
    
    andmedAgr <- alkohol %>% filter(productClass == input$liik)
    
    otsing <- andmedAgr %>% filter(producerCountry == input$riik & (ethanolRate > input$etanol[1] & ethanolRate < input$etanol[2]))
    
    if(input$kogus != "Kõik"){
      otsing <- otsing %>% filter(capacity==input$kogus)
    }
    
    otsing <- otsing %>% dplyr::select(-time, -ver, -productClass, -producerCountry)
    
    output$vastused <- renderDataTable(otsing, extensions="Responsive",  rownames= FALSE)
    
    if (input$liik != ""){
      output$graafik <- renderPlot({
        joonistaRiik(andmedAgr)
      })
      
      output$graafik2 <- renderPlot({
       joonistaSisseostjaTop10(andmedAgr)
      })
    }
    
    output$graafik3 <- renderPlot({
      joonistaEthanol(andmedAgr)
    })
    
    output$graafik4 <- renderPlot({
      joonistaAeg(andmedAgr)
    })
 })
  
}

# Run the application 
shinyApp(ui, server)

