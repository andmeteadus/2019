#RAKENDUSE UI

library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title="Riigikogu valimised 2019",titleWidth = 300),
                    dashboardSidebar(width = 300,
                                     sidebarMenu(
                                       menuItem("RK19 valimistulemused", tabName = "1tab", icon = icon("fas fa-male")),
                                       menuItem("Reklaamikulud", tabName = "2tab", icon = icon("fal fa-poll-h")),
                                       menuItem("Reklaamikulude seos valimistulemusega", tabName = "3tab", icon = icon("fal fa-id-card-alt"))
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "1tab",
                                fluidRow(
                                  box(plotOutput("plot5", height = 520),
                                      title = "Valimistulemused"), 
                                  box(title="Riigikogu valimised 2019", 
                                      p("Riigikogu valimised toimusid 3. märtsil 2019. aastal. Eelhääletamine toimus vahemikus 21.-24. veebruar ja e-hääletamine toimus vahemikus 21.-27. veebruar."),
                                      p("Enamik erakondi esitasid maksimaalse arvu kandidaate riigikogu valimistele ehk 125 inimest erakonna kohta. Erandiks oli Elurikkuse Erakond (Elurikkus), mis esitas 73 kandidaati."),
                                      p("Riigikogusse pääses kokku 6 erakonda: Eesti Reformierakond (REF), Eesti Keskerakond (KESK), Eesti Konservatiivne Rahvaerakond (EKRE), Isamaa Erakond (Isamaa) ja Sotsiaaldemokraatlik Erakond (SDE)."),
                                      p("Reformierakond on Eestis liberaalse maailmavaate eestvedaja. 2019. aasta riigikogu valimistel sai erakond 162363 häält ja 34 mandaati. Saadud mandaatide arv suurenes 4 võrra võrreldes eelmiste valimistega aastal 2015."),
                                      p("Keskerakond on ideoloogialt vasaktsentristlik partei. 2019. aasta riigikogu valimistel sai erakond 129618 häält ja 26 mandaati. Saadud mandaatide arv vähenes 1 võrra võrreldes eelmiste valimistega aastal 2015."),
                                      p("Lisaks sai Eesti Konservatiivne Rahvaerakond juurde 12 mandaati ehk pääses riigikogusse 19 mandaadiga. Isamaa Erakond sai 12 mandaati, mis on 2 mandaati vähem kui eelmistel valimistel. Ka Sotsiaaldemokraatlik Erakond sai vähem mandaate kui eelmisel korral: 10 mandaati, mis on 5 mandaati vähem."),
                                      p("Riigikogusse ei pääsenud Eesti Vabaerakond (Vabaerakond), Elurikkuse Erakond, Erakond Eesti 200 (E200) ja Erakond Eestimaa Rohelised (Rohelised). Vabaerakond kaotas 8 mandaati võrreldes 2015. aasta valimistega."),
                                      p("Selle projekti eesmärk on anda ülevaade valimistulemuste ning erakondade valimiseelsete reklaamikulude kohta.")
                                  )
                                ),
                                fluidRow(
                                  box(title="Erakondade häältemagnetid",
                                      plotOutput("joonis"),width=8,height = 700),
                                  box(width=4,
                                      title = "Valikud",
                                      selectInput(inputId="erakond", label=strong("Vali erakond"),
                                                  choices = haaled_eraldi$Erakond %>% unique(), 
                                                  selected="LTMS"),
                                      sliderInput("n",
                                                  "Vali kandidaatide arv",
                                                  min = 5,
                                                  max = 30,
                                                  value = 10)        
                                  )
                                )
                        ),
                        tabItem(tabName = "2tab",
                                fluidRow(
                                  box(title="Reklaamikulud kvartalite kaupa",width=12,plotOutput("joonis_vahelduv"),
                                      radioButtons(inputId="tyyp", label=strong("Vali, kas soovid näha üldist või detailset graafikut"),
                                                   choices=c("Üldine" = "yldine","Detailne" = "detailne"))       
                                  ),
                                  
                                  box(
                                    title="Reklaamikulud reklaamitüübi kaupa",
                                    plotOutput("plot3", height = 350), width=12,
                                    selectInput(inputId = "kulutus", label = strong("Vali reklaamikulutus"),
                                                choices = unique(erakonnad$Reklaam), selected = "Telereklaam"),
                                    radioButtons(inputId = "kvartal2", label=strong("Vali kvartal"), 
                                                 choices = unique(erakonnad$Kvartal), selected = "2018. a IV kvartal")
                                  ),
                                  box(title="Reklaamikulud erakondade kaupa",
                                      box(plotOutput("plot1", height = 250),
                                          
                                          
                                          title = "",
                                          selectInput(inputId = "erakond2", label = strong("Vali erakond"),
                                                      choices = unique(erakonnad$Erakond), selected = "Eesti Keskerakond"),
                                          radioButtons(inputId = "kvartal", label=strong("Vali kvartal"), 
                                                       choices = unique(erakonnad$Kvartal), selected = "2018. a IV kvartal")
                                      ), 
                                      box(plotOutput("plot2", height = 250), 
                                          title = "Suurendatud vaade"
                                      ), width=12
                                  )
                                  
                                  
                                )),
                        tabItem(tabName = "3tab",
                                fluidRow(
                                  box(
                                    h3("Reklaamikulu ja valimistulemuse võrdlus"),
                                    box(
                                      title="Kogu reklaamikulu",
                                      plotOutput("plot6", height = 450)
                                    ), 
                                    box(
                                      title="Valimistulemus",
                                      plotOutput("plot7", height = 450)
                                    ),width=12),
                                  box(title="Valimistulemuse sõltuvus reklaamikuludest",
                                      plotOutput("reklaam_protsent"),width=12)
                                )
                        )
                      )
                    )
)
