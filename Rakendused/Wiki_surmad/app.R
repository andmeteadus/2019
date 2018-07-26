library(rvest)
library(dplyr)
#install.packages("pageviews")
library(pageviews)
library(ggplot2)
# andmed<-function(aasta){
# url<-paste("https://et.wikipedia.org/wiki/Surnud_",aasta, sep="")
# page<-read_html(url)
# 
# page%>%
#   html_node(".mw-parser-output") %>%
#   html_nodes("a[title]") %>%
#   html_text() -> wiki
# page%>%
#   html_node(".mw-parser-output") %>%
#   html_nodes("a[class]") %>%
#   html_text() -> wiki2
# wiki<-wiki[-c(which(wiki %in% wiki2))]
# wiki<-wiki[-(1:5)]
# wiki<-wiki[-c(which(wiki %in% c("muuda","muuda l?hteteksti")))]
# number<-c("1","2","3","4","5","6","7","8","9")
# for (i in 1:9) {
#   wiki<-wiki[-c(which(startsWith(wiki, number[i])))]
# }
# ajutine<-data.frame()
# for(i in 1:length(wiki)){
#   ajutine[i,1]<-wiki[i]
#   ajutine[i,2]<-aasta
#   tryCatch(ajutine[i,3]<-sum(article_pageviews(project="et.wikipedia.org", article = wiki[i], start = "2015100100", end = Sys.Date())$views),
#            error=function(e) NA)
# }
# return(ajutine)
# }
# data<-data.frame()
# for(i in 2001:2018){
# data<-rbind(data,andmed(i))
# }
# write.csv(x = data, file = "wikip.csv")

wikiped<-read.csv("wikip.csv",row.names = 1,encoding = "cp1257")

url2<-paste("https://et.wikipedia.org/wiki/Maailma_riikide_loend")
page2<-read_html(url2)

page2%>%
  html_node(".mw-body") %>%
  html_nodes("a[title]") %>%
  html_text() -> riik
wikiped<-wikiped[-c(which(wikiped[,1] %in% riik)),]
wikiped<-wikiped[-c(which(wikiped[,1] %in% c("Aprill","Elvis Presley","Elizabeth II","A. H. Tammsaare","Eesti NSV", "Kataloonia","Jugoslaavia",
                                             "ABBA","Apple Inc.","Põhja-Iirimaa","August","Chicago","LSD","NASA","AIDS","KGB","Ohio","BBC"))),]
#install.packages("magick")
# library(magick)
# 
# pilt <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/b/b9/Heli_L%C3%A4%C3%A4ts_1974.jpg/220px-Heli_L%C3%A4%C3%A4ts_1974.jpg")
# print(pilt)

ui <- fluidPage(
  
  # Application title
  titlePanel("Vikipeedia rakendus"),
  
  # Sidebar with inputs 
  sidebarLayout(
    sidebarPanel(
      selectInput("start", h3("Vali algusaasta"), 
                  choices = 2000:2018, selected=2000),
      selectInput("end", h3("Vali lõpuaasta"), 
                  choices = 2000:2018, selected=2018),      
      sliderInput("bins",
                  h3("Vali inimeste arv"),
                  min = 25,
                  max = 100,
                  value = 25),
      br(),
      hr(),
      p("Projekti autor: Indrek Polding")
    ),
    
    mainPanel(h1("Tutvustus"),
              "Siin on võimalik näha populaarseid artikli pealkirju",
              dataTableOutput("tabel")
    )
  )
)

# Define server logic required to make a table
server <- function(input, output) {
  
  
  
  table_wiki = function(data, start, end, n){
    data<-filter(data, data$V2<=end)
    data<-filter(data, data$V2>=start)
    new<-arrange(data, desc(V3))[1:n,]
    new
  }
  output$tabel <- renderDataTable({
    table_wiki(wikiped,input$start, input$end, input$bins)
  })
}
library(shiny)
# Run the application 
shinyApp(ui = ui, server = server)


