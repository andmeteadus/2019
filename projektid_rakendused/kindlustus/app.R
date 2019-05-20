library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinythemes)


#Elukindlustus, 2010-2017, algandmed

andmed_elu <-  read.csv("Elukindlustus.csv", sep = ",", header=T)


#Kahjukindlustus, 2010-2017, algandmed

andmed_kahju <-  read.csv("Kahjukindlustus.csv", sep = ",", header=T)



#Ebavajalike ja tühjade andmete välja filtreerimine

andmed_uus <- andmed_elu %>%
  select(-DIM2, -DIM3, -DIM4, -DIM5, -Flag.Codes, -Flags, -TIME, -Kuu)



andmed_uus_kahju <- andmed_kahju %>%
  select(-DIM2, -DIM3, -DIM4, -DIM5, -Flag.Codes, -Flags, -TIME, -Kuu) 




#Jooniste tegemine  

#Elukindlustus

ggplot(andmed_uus, aes(x = factor(Vaatlusperiood), y = Value, fill = Elukindlustusliik)) + geom_col() + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + ylab("Kindlustusmaksed, tuhat €") + xlab("Aasta") + 
  scale_y_continuous(breaks = seq(0, 900000, 100000), labels = scales::comma)


#Kahjukindlustus

ggplot(andmed_uus_kahju, aes(x = factor(Vaatlusperiood), y = Value, fill = Kahjukindlustusliik)) + geom_col() + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + ylab("Kindlustusmaksed, tuhat €") + xlab("Aasta") + 
  scale_y_continuous(breaks = seq(0, 2000000, 200000), labels = scales::comma)


#ui

ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel("Kindlustusettevõtete majandusnäitajad 2010-2017"),
                sidebarLayout(
                  sidebarPanel(
                    
                    selectInput(inputId="Elukindlustusandja",label=strong("Vali elukindlustusandja"),
                                choices=unique(andmed_uus$Elukindlustusandja), selected = "Elukindlustusandjad kokku"),
                    
                    radioButtons(inputId = "Näitaja", label = strong("Vali elu- ja kahjukindlustuse näitaja"),
                                 choices = c("Saadud kindlustuspreemiad", "Kindlustusnõuete väljamaksed")),
                    
                    selectInput(inputId="Kahjukindlustusandja",label=strong("Vali kahjukindlustusandja"),
                                choices=unique(andmed_uus_kahju$Kahjukindlustusandja), selected = "Kahjukindlustusandjad kokku"),
                   
                    
                    h5("Andmed: www.stat.ee", style = "color:grey"),
                    
                    h5("Kindlustusettevõtete majandusnäitajate rakendus õppeaines statistiline andmeteadus ja visualiseerimine 
                       MTMS.01.100", style = "color:grey"),
                    
                    h5("Autor: Egle Olesk", style = "color:grey")),
                    
          
                  mainPanel(h1("Elukindlustus- ja kahjukindlustusettevõtete majandusnäitajad"),
                            p("Projekti eesmärk on võrrelda aegreana 7 aasta jooksul (2010-2017) elu- ja kahjukindlustusettevõtete
                              majandusnäitajaid. Andmete võrdlemiseks on välja toodud joonised ning kokkuvõtlikud tabelid,
                              mis näitavad täieliku ülevaate saamiseks kõik aastaid korraga. Andmed on esitatud tuhandetes eurodes
                              ning andmeid saab sorteerida kindlustusandja ja kindlustusnäitaja järgi."),

                            tabsetPanel(type = "tabs",
                                        tabPanel("Joonised", plotOutput(outputId = "joonis"),
                                                 plotOutput(outputId = "joonis_kahju")),
                                        tabPanel("Kokkuvõte, elukindlustus", tableOutput("Kokkuvõte")),
                                        tabPanel("Kokkuvõte, kahjukindlustus", tableOutput("Kokkuvõte_kahju")),
                                        tabPanel("Mõisted", htmlOutput("Mõisted")))
                        
            
    )             
  )
)


#Serveri funktsioon andmete kättesaamiseks vastavalt filtritele

server <- function(input, output){
  
  df_elukindlustusandja <- reactive({
    req(input$Elukindlustusandja)
    filter(andmed_uus, Elukindlustusandja %in% input$Elukindlustusandja)
    
  })
  
  df_naitaja <- reactive({
    req(input$Näitaja)
    filter(df_elukindlustusandja(), Näitaja %in% input$Näitaja)
   
  })
  
  df_kahjukindlustusandja <- reactive({
    req(input$Kahjukindlustusandja)
    filter(andmed_uus_kahju, Kahjukindlustusandja %in% input$Kahjukindlustusandja)
    
  })
  
  df_naitaja_kahju <- reactive({
    req(input$Näitaja)
    filter(df_kahjukindlustusandja(), Näitaja %in% input$Näitaja)
    
  })


    
  #Genereeri joonis
  output$joonis <- renderPlot({

    
    ggplot(df_naitaja(), aes(x = factor(Vaatlusperiood), y = Value, fill = Elukindlustusliik)) +
      geom_col() + theme(axis.text.x = element_text(angle=45, hjust=1)) + ylab("Kindlustusmaksed, tuhat €") + 
      xlab("Aasta") + scale_y_continuous(labels = scales::comma) + ggtitle("Elukindlustuse joonis") +
      theme(plot.title = element_text(face="bold", color = "#666666"))
 
    
  })
  
  output$joonis_kahju <- renderPlot({
    
    ggplot(df_naitaja_kahju(), aes(x = factor(Vaatlusperiood), y = Value, fill = Kahjukindlustusliik)) +
      geom_col() + theme(axis.text.x = element_text(angle=45, hjust=1)) + ylab("Kindlustusmaksed, tuhat €") + 
      xlab("Aasta") + scale_y_continuous(labels = scales::comma) + ggtitle("Kahjukindlustuse joonis") +
      theme(plot.title = element_text(face="bold", color = "#666666"))
    
    
  })
  
  output$Kokkuvõte <- renderTable({
    df_naitaja()
    
    
  })
  
  output$Kokkuvõte_kahju <- renderTable ({
    df_naitaja_kahju()
    
    
  })
  
  output$Mõisted <- renderText({
    paste("<b>Kindlustuspreemia ehk kindlustusmakse</b> - summa, mida kindlustusvõtja maksab kindlustuse eest.",
          "<br>" ,
          "<br>" ,
          "<b>Kindlustusnõuete väljamaksed</b> - makstud kindlustus- ja tagasiostusummad ning käsitluskulud." ,
          "<br>" ,
          "<br>" ,
          "<b>Investeerimisriskiga elukindlustus</b> - elukindlustuse liik, mille puhul kindlustusandja poolt
          makstav kas ühekordne või perioodiliste väljamaksete suurus sõltub lepinguga seotud
          alusvara väärtusest ja lepingus garanteeritud kindlustussumma suurusest." ,
          "<br>" ,
          "<br>" ,
          "<b>Kapitalikogumiskindlustus</b> - elukindlustuse liik, mille tegevuse all mõistetakse aktuaarsetel 
          arvutustel põhinevat kapitali kogumist, kui kindlustusandja võtab kindlustusvõtja või soodustatud 
          isiku ees elukindlustuslepingus kokkulepitud ühekordse kindlustusmakse või regulaarsete kindlustusmaksete 
          eest määratud kestuse ja rahalise suurusega kohustused.",
          "<br>" ,
          "<br>" ,
          "<b>Lisakindlustus</b> - elukindlustuse liik, mis muudab kindlustuslepingu vajaduspõhiseks 
          ehk inimesel on võimalik valida just need riskikaitsed, mis ta arvab realiseeruvat (nt
          kriitiliste haiguste kindlustus - vähk, südameinfarkt, peaajuveresoonte haigused)." ,
          "<br>" ,
          "<br>" ,
          "<b>Tsiviilvastutuskindlustus</b> - kahjukindlustuse liik, mis hüvitab kolmandale isikule vara- ja isikukahjud, 
          mida kindlustusvõtja on tekitanud oma ameti- või kutsetegevuses ning üldises või igapäevases tegevuses." ,
          "<br>" ,
          "<br>" ,
          "___________________________________" ,
          "<br>" ,
          "www.fi.ee" ,
          "<br>" ,
          "www.riigiteataja.ee" ,
          "<br>" ,
          "www.stat.ee")
    
    

    
  })
  
  
}
  

shinyApp(ui = ui, server = server)


