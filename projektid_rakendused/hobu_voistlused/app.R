#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(readr)
library(rvest)
library(tidyr)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(scales)
#setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))
graafik <- function(ala, vanuseklass, mitu, algus, lopp){
  proov <- paste0(vanuseklass,".csv")
  proov <- read.csv(proov)
  proov <- proov[-1]
  proov <- proov[,c(1,(as.numeric(algus)-2013):(as.numeric(lopp)-2013))] #%>% mutate(sum = rowSums(proov[,2:length(proov)]))
  if(length(proov) > 2){
    proov$summa <- rowSums(proov[, -1])
    proov <- arrange(proov, desc(summa))
  }else{
    proov$summa <- proov[,2]
    proov <- arrange(proov, desc(proov[,2]))
  }
  gr <- melt(proov[1:mitu,c(1,2:(length(proov)-1))], id = c("Nimi"))
  #gr <- melt(proov[1:mitu,c(1,2:5)], id = c("Nimi"))
  gr$value<- as.double(gr$value)
  uus <- dcast(gr,Nimi~ variable )
  uus<- data.frame(uus)
  gr$variable <- ifelse(gr$variable=="Punktid15","2015",ifelse(gr$variable=="Punktid16","2016",ifelse(gr$variable=="Punktid17","2017","2018")))
  gr$variable <- as.integer(gr$variable)
  if(mitu > 10){
  ggplot(gr, aes(variable,value,colour = Nimi)) + geom_point() + geom_line()+#geom_text(label=gr$Nimi) + 
    theme_bw() + labs(x="aasta", y="punkte")+scale_x_continuous(breaks=c(2015:2018))#+scale_x_continuous(breaks= pretty_breaks())
  }else{
    ggplot(gr, aes(variable,value,colour = Nimi))  + geom_line()+#geom_text(label=gr$Nimi) + 
      theme_bw() + geom_label_repel(aes(label = gr$Nimi, colour=Nimi), show.legend = FALSE) + labs(x="aasta", y="punkte") + scale_x_continuous(breaks=c(2015:2018))#+scale_x_continuous(breaks= pretty_breaks())
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Takistussõidu parimad"),
   
   # Sidebar with inputs 
   sidebarLayout(
     sidebarPanel(
       selectInput("ala", h3("Vali ala"), 
                   choices = c("Takistussõit"), selected="Takistussõit"),
       selectInput("vanuseklass", h3("Vali klass"), 
                   choices = c("seeniorid", "u25", "juuniorid", "lapsed", "poniratsutajad", "hobused", "ponid"), selected="seeniorid"),      
       sliderInput("mitu",
                   h3("Vali objektide arv"),
                   min = 1,
                   max = 30,
                   value = 10),
       selectInput("alg", h3("Vali alguse aasta"), 
                   choices = 2015:2018, selected=2015),
       selectInput("lop", h3("Vali lõpu aasta"), 
                   choices = 2015:2018, selected=2018),
       br(),
       hr(),
       p("Projekti autorid: Kristjan Sinikas ja Hans Erik Atonen")
     ),
     
     mainPanel(h1("Parimad"),
               "Siin on interaktiivne graafik",
               plotOutput("ploti")
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$ploti <- renderPlot({
    #table_wiki(wikiped,input$start, input$end, input$bins)
    graafik(input$ala,input$vanuseklass, input$mitu, input$alg, input$lop)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

