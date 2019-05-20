# Vajalikud paketid (lae ja kui pole installitud, siis installi)

#is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

#load_or_install <- function(package_names, repos="https://cloud.r-project.org/"){
#  for(package_name in package_names)  {
#    if(!is_installed(package_name))    {
#      install.packages(package_name, repos=repos, dependencies = T)
#    }
#    library(package_name, character.only=TRUE, quietly=TRUE, verbose=FALSE)
#  }
#}

#load_or_install(c('shiny', 'shinydashboard', 'cluster', 'ggdendro', 
#                  'factoextra', 'MASS', 'ggplot2', 'plotly', 'dplyr', 
#                  'knitr', 'kableExtra', 'DT', 'shinyjs', 'shinyWidgets'))

library(shiny)
library(shinydashboard)
library(cluster)
library(ggdendro)
library(factoextra)
library(MASS)
library(ggplot2)
library(plotly)
library(dplyr)
library(knitr)
library(kableExtra)
library(DT)
library(shinyjs)
library(shinyWidgets)

###############################################-

## ui ----

ui <-  tagList(
  
  dashboardPage(
    
    
    ## Pealkiri ----
    dashboardHeader(
      title = span("Rakendus polüseemia uurimiseks", 
                   style = "color: black; 
                   font-weight: bold; 
                   font-size: 20px; 
                   font-family: 'Georgia'"
                  ), 
      titleWidth = 405
      
    ), # dashboardHeader lõpp
    
    
    ## dashboardSidebar ----
    dashboardSidebar(
      
      useShinyjs(), # vajalik serveris fn-i "hide" kasutamiseks
      
      sidebarMenu(
        menuItem("Tutvustus", tabName = "tutvustus"),
        menuItem("Korpusandmed", tabName = "korpus"),
        menuItem("Katseandmed", tabName = "katse")
      )
      
    ), # dashboardSidebar lõpp
    
    ## dashboardBody ----
    dashboardBody(
      
      tags$script(HTML("$('body').addClass('fixed');")), # dashboardHeader'i asendi fikseerimiseks
      
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }" # veateadete peitmiseks
      ),
      
      
      tags$head(
        tags$style(HTML(".main-sidebar { font-size: 30px; }")) # dashboardSidebar'i fondi suurus
      ),
      
      
      tabItems(
        ## ESIMENE tab üksus ----
        tabItem(tabName = "tutvustus",
                
                p(HTML(paste0("Tegemist on algvariandiga rakendusest, mis on mõeldud ", a("keelekorpustest", href="http://www.emakeeleselts.ee/omakeel/2015_1/OK_2015-1_05.pdf"), " saadud või ", a("katsetega", href="http://jeful.ut.ee/public/files/Klavan,%20Veismann,%20Jurine%2017-34.pdf"), " kogutud andmete statistiliseks analüüsimiseks polüseemia uurimisel. Rakenduse loomiseks on kasutatud programmeerimistarkvara ", a("R", href="https://www.r-project.org/"), " paketti ", a("Shiny", href="https://shiny.rstudio.com/"), ".")), style="font-size: 20px;"),
                
                br(),
                
                h1("Mis on polüseemia ja kuidas seda uurida?", align="center"),
                
                br(),
                
                p(HTML(paste0("Polüseemiaks nimetatakse nähtust, kus ühel sõnal on mitu omavahel seotud tähendust, nt sõnal ", em("vana"), " on tähendused 'eakas', 'kauaaegne', 'endine' jne. Polüseemia on keeles väga levinud nähtus. Seletavale sõnaraamatule toetudes võib öelda, et eesti keeles on ligikaudu ", a("iga kümnes sõna polüseemne", href="http://www.emakeeleselts.ee/omakeel/2009_2/OK_2009-2_02.pdf"), ", kusjuures sagedamini kasutatavate sõnade hulgas on polüseemsete sõnade osakaal veelgi suurem. Näiteks eesti keele ", a("100 kõige sagedasema omadussõna hulka", href="http://arhiiv.rakenduslingvistika.ee/ajakirjad/index.php/aastaraamat/article/view/ERYa10.19/263"), " kuulub 80 sellist sõna, millel on sõnaraamatus eristatud kaht või enamat põhitähendust.")), style="font-size: 20px;"),
                
                br(),
                
                p("Ühesõnaga, kui me ütleme, et sõna on polüseemne, siis me mõtleme sellega seda, et antud sõnal on rohkem kui üks tähendus. Aga kui palju neid tähendusi täpselt on, see ei ole enam nii kindel. Vahel võivad eri sõnaraamatute koostajate arvamused ühe ja sama sõna tähenduste loetlemisel ja alltähendusteks liigitamisel vägagi erinevad olla. Seetõttu oleks hea, kui meil oleks ka mingi empiirilistele andmetele tuginev alus, millele toetudes võtta vastu otsust, kui mitut tähendust mingil sõnal eristada.", style="font-size: 20px;"),
                
                p("Kaks suuremat empiiriliste andmete saamise allikat, mida keeleteaduses kasutatakse, on katsed ja keelekorpused. Siinne rakendus on plaanitud eeskätt korpuspäringute tulemuste analüüsimiseks, kuid selle abil on võimalik analüüsida ka katsete tulemusi.", style="font-size: 20px;"),
                
                br(),
                
                h1("Milleks selline rakendus?", align="center"),
                
                br(),
                
                p(HTML(paste("Rakenduse loomise eesmärk on töötada kõigepealt välja vahend, mille abil leida polüseemia uurimiseks (ja arvatavasti ka iga sõnaliigi jaoks eraldi) selline sätete kombintsioon (vt sätete valikuvõimalusi täpsemalt jaotisest \"Korpusandmed\"), mis hakkaks andma statistilise analüüsi tulemuseks niisuguseid uuritava sõna kasutusjuhtude (ehk lausete) rühmi, mida on võimalik mõttekalt tõlgendada &ndash; st viia need vastavusse uuritava sõna mingite konkreetsete tähendustega, nt sõna", em("vana"), "puhul üks rühm kasutusjuhtusid tähendusega 'eakas', teine rühm kasutusjuhtusid tähendusega 'kauaaegne' jne.")), style="font-size: 20px;"),
                
                p("Rakenduse eelis tavaliste R-i skriptide kasutamise ees seisneb siin selles, et erinevaid sätete väärtusi igal koodi käivitamisel kogu aeg käsitsi sisse trükkides muuta oleks palju tülikam, kui seda graafilise kasutajaliidese kaudu teha. Pealegi, kui sobivad sätete kombinatsioonid on leitud, siis saab sellest rakendusest edasi arendada töövahendi, mida on võimalik hakata kasutama polüseemsete sõnade tähenduste kirjelduste loomisel.", style="font-size: 20px;"),
                
                br(),
                
                h1("Lühiülevaade rakenduse hetkeseisust", align="center"),
                
                br(),
                
                p(em(strong("Jaotises \"Korpusandmed\"")), "on praegu olemas algne sätete valimise paneel, kuid kuna kasutajaliides ei ole veel andmetega ühendatud, siis otsingut reaalselt teostada ning otsingutulemusi analüüsida veel ei saa.", style="font-size: 20px;"),
                
                br(),
                
                p(HTML(paste0(em(strong("Jaotises \"Katseandmed\"")), " on võimalik kasutada näidisanalüüsitulemustega tutvumiseks näiteandmestikku, milleks on omadussõna ", em("vana"), " polüseemia uurimiseks läbiviidud katse tulemused. Tabelis 1 on esitatud katseisikute keskmised hinnangud erinevates katselausetes esinenud sõna ", em("vana"), " tähenduste omavahelise erinevuse kohta. Suuremad väärtused tabeli distantsimaatriksis osutavad suuremale erinevusele ja väiksemad väärtused väiksemale erinevusele ehk suuremale sarnasusele. Katselauseid oli kokku 16 ja need on ära toodud tabeli viimases veerus. Tabeli esimeses veerus on iga katselause ehk ", em("vana"), " kasutusjuhu identifiteerimiseks loodud lühike silt, mis koosneb katselause järjekorranumbrist ja nimisõnast, mille täiendiks sõna ", em("vana"), " selles lauses oli. Neid identifitseerivaid silte on edaspidi kasutatud katselausetele viitamiseks ka kõigil näidisanalüüsiosa joonistel. Täpsemad nõuded katsetulemusi sisaldava andmefaili vormistamisele on esitatud käesoleva jaotise lõpus Lisas 1. Distantsimaatriksi jaoks vajalike andmete kogumiseks läbiviidava katse kohta vt pikemalt ", a("siit", href="https://dspace.ut.ee/bitstream/handle/10062/54060/Aimi_Pikksaar_mag.pdf?sequence=1&isAllowed=y"), ".")), style="font-size: 20px;"),
              
                br(),
                br(),
                
                h3(strong("Tabel 1. Näiteandmestik: omadussõna"), em(strong("vana")), strong("katsetulemused")),
                
                br(),
                
                img(src = "tabel.png", 
                    width = "99%", 
                    height = "100%"
                ),
                
                br(),
                br(),
                
                p(HTML(paste0("Statistiliste analüüsimeetoditena on rakenduses praegu kasutusel ", a("hierarhiline klasteranalüüs", href="https://www.statisticshowto.datasciencecentral.com/hierarchical-clustering/"), " ja ", a("multidimensionaalne skaleerimine",  href="https://www.statisticshowto.datasciencecentral.com/multidimensional-scaling/"), ".")), style="font-size: 20px;"),
                
                br(),
                
                h3(HTML("<b><u>Lisa 1. Nõuded katsetulemusi sisaldava andmefaili vormistamisele</u></b>")),
                
                br(),
                
                p("Lisaks näiteandmestiku kasutamisele on jaotises \"Katseandmed\" võimalik analüüsimiseks üles laadida ka oma andmefail, mis tuleb korrektsete analüüsitulemuste saamiseks vormistada järgmiste nõuete kohaselt.", style="font-size: 20px;"),
                
                h5(HTML('<ol><li>Andmetabeli esimese rea peavad moodustama veergude pealkirjad.</li><br>
                        <li>Andmetabeli esimese veeru pealkirjaks peab olema teineteisest allkriipsuga eraldatuna uuritava sõna sõnaliik ja uuritav sõna ise (nt <em>OMADUSSÕNA_VANA</em>).</li><br>
                        <li>Andmetabeli esimene veerg ja teise kuni eelviimase veeru pealkirjad peavad sisaldama siltide nimetusi, mida kasutada joonistel vastavatele katselausetele viitamisel (nt <em>1_kuu, 2_töövõte</em> jne).</li><br>
                        <li>Arvud andmetabelis peavad näitama distantse (ehk tajutud erinevusi) erinevates katselausetes esinenud uuritava sõna tähenduste vahel. Kümnendkoha eraldajaks arvudes peab olema punkt ja distantsimaatriks ise peab olema sümmeetriline (st ülalpool 0-dest koosnevat peadiagonaali paiknevad arvud peavad olema võrdsed vastavate allpool peadiagonaali paiknevate arvudega).</li><br>
                        <li>Katselaused peavad olema esitatud andmetabeli viimases veerus, kusjuures uuritav sõna võiks olla igas lauses pandud #-märkide vahele (nt <em>Hoidke au sees #vanu# töövõtteid, tehke ise juustu ja leiba</em>).</li><br>
                        <li>Andmefail peab olema salvestatud CSV formaadis (UTF-8 kodeeringus ja väljade eraldajaks reas semikoolon).</li></ol>'), style="font-size: 20px;"),
                
                br(),
                br()
                
                ), # tabItem(tabName = "tutvustus" ... lõpp
        
        ## TEINE tab üksus ----
        
        tabItem(tabName = "korpus",
                
                fluidPage(
                  
                  # Pealkiri
                  #titlePanel("Korpusandmed"),
                  
                  # Sidebar layout koos sisendi (input) ja väljundi (output) defineerimisega
                  sidebarLayout(
                    
                    ## Sidebar panel SISENDI jaoks ----
                    sidebarPanel(
                      
                      h1("SÄTTED", align="center"),
                      
                      fluidRow(column(12, h4("Sisesta uuritav sõna")), 
                               column(12, textInput("text", 
                                                    label =NULL, 
                                                    value = ""
                                                   )
                                     )
                              ),
                      
                      fluidRow(column(12, h4("Uuritava sõna sõnaliik")), 
                               column(12, selectInput("liik", 
                                                      label =NULL, 
                                                      choices = c("Nimisõna"="S", 
                                                                  "Omadussõna"="A", 
                                                                  "Tegusõna"="V", 
                                                                  "Määrsõna"="Adv"
                                                                  )
                                                     )
                                     )
                              ),
                      
                      fluidRow(column(12, h4("Vali korpus(ed)")), 
                               column(12, checkboxGroupInput("korpusteValik", 
                                                             label = NULL, 
                                                             choices = c("Ilukirjandus"="ilu", 
                                                                         "Ajakirjandus"="aja", 
                                                                         "Teaduskirjandus"="tea"
                                                                        )
                                                            )
                                    )
                              ),
                      
                      sliderInput("n",
                                  h4("Lausete arv juhuvalimis"),
                                  value = 100,
                                  min = 0,
                                  max = 800,
                                  step = 100
                                 ),
                      
                      fluidRow(column(12, h4("Kontekstisõnade arv vasakul")), 
                               column(4, numericInput("vasak", 
                                                      label = NULL, 
                                                      value = 5, 
                                                      min = 0, 
                                                      max = 10
                                                     )
                                     )
                              ),
                      
                      fluidRow(column(12, h4("Kontekstisõnade arv paremal")), 
                               column(4, numericInput("parem", 
                                                      label = NULL, 
                                                      value = 5, 
                                                      min = 0, 
                                                      max = 10
                                                     )
                                     )
                             ),
                      
                      fluidRow(column(12, h4("Sõnadevahelise seose tugevuse statistik")), 
                               column(12, selectInput("seos", 
                                                      label =NULL, 
                                                      choices = c("Log-tõepära funktsioon "="log", 
                                                                  "Vastastikuse informatsiooni väärtus "="viv", 
                                                                  "Minimaalne tundlikkus"="mt"
                                                                 )
                                                     )
                                     )
                             ),
                      
                      
                      fluidRow(column(12, h4("Sarnasusmõõdik")), 
                               column(12, selectInput("sarnasus", 
                                                      label =NULL, 
                                                      choices = c("Koosinussarnasus"="cos", 
                                                                  "Eukleidiline kaugus"="eucl"
                                                                 )
                                                     )
                                     )
                              ),
                      
                      fluidRow(column(12, h4("Klasterdusmeetod")), 
                               column(12, selectInput("meetod", 
                                                      label =NULL, 
                                                      choices = c("Keskmise kauguse meetod"="average", 
                                                                  "Täieliku seose meetod"="complete", 
                                                                  "Ühe seose meetod"="single",
                                                                  "Wardi meetod"="ward"
                                                                 )
                                                     )
                                     )
                              ),
                      
                      
                      fluidRow(column(12, h4("Klastrite arv")), 
                               column(12, textInput("klast", 
                                                    label =NULL, 
                                                    value = "Vaikimisi"
                                                   )
                                     )
                              ),
                      
                      helpText(span("Märkus. Pärast muude sätete väärtuste muutmist on soovitatav jätta esmasel analüüsil klastrite arv \"Vaikimisi\" väärtuse peale."), 
                               style = "font-style: italic; 
                               font-size: 18px;"
                              ),
                      
                      br(),
                      br(),
                      
                      
                      fluidRow(actionButton("update1", 
                                           span("Esita analüüs", 
                                               style="margin-left: 5px;"), 
                                               icon("paper-plane"), 
                                               style="color: #fff; 
                                               background-color: #337ab7; 
                                               border-color: #2e6da4; 
                                               margin-top: 20px; 
                                               font-size: 20px; 
                                               margin-bottom: 30px;"
                                      
                                           ), align="center"
                               )
                      
                    ), # sidebarPanel lõpp
                    
                    
                    ## Main panel VÄLJUNDITE jaoks ----
                    mainPanel(
                      
                      p("Vali kõrvaloleval paneelil \"SÄTTED\" sobivad näitajad päringu jaoks ning klõpsa tulba lõpus asuval nupul \"Esita analüüs\".", style="font-size: 20px;"),
                      
                      br(),
                      
                      h1("ANALÜÜSITULEMUSED", align="center"),
                      
                      p("Kahjuks see osa rakendusest veel ei tööta.",
                         align="center",
                         style="color:red; font-size: 20px;")
                      
                      ) # mainPanel lõpp 
                    
                  ) # sidebarLayout lõpp
                  
              ) # fluidPage lõpp
                
        ), # tabItem(tabName = "korpus" ... lõpp
        
        ## KOLMAS tab üksus ----
        
        tabItem(tabName = "katse",
                
                fluidPage(
                  
                  # Pealkiri
                  #titlePanel("Katseandmed"),
                  
                  # Sidebar layout koos sisendi (input) ja väljundi (output) defineerimisega
                  sidebarLayout(
                    
                    ## Sidebar panel SISENDI jaoks ----
                    sidebarPanel(
                      
                      helpText(div("Lae üles oma andmefail või kasuta näidistulemustega tutvumiseks näiteandmestikku."), 
                               style = "font-style: italic; 
                               font-size: 18px;"
                              ),
                      
                      #eemalda ingliskeelne kiri "Upload complete" üleslaadimise sinisetriibuliselt ribalt:
                      tags$style(".progress-bar {color: transparent!important}"), 
                      
                      fileInput("fail", 
                                h4("Vali CSV-fail"), 
                                buttonLabel = "Sirvi...", 
                                placeholder = "Faili pole valitud", 
                                multiple = FALSE, 
                                accept = c(".csv")
                               ), 
                      
                      
                      tags$style("input[type=checkbox] {
                                 transform: scale(1.2);
                                 }"
                                ),
                      
                      h4(prettyCheckbox(inputId = "checkbox5",
                                        #bigger = F,
                                        label = "Vali näiteandmestik",
                                        icon = icon("check"),
                                        #animation = "tada",
                                        status = "primary",
                                        inline = TRUE
                                       )
                        ),
                      
                      
                      fluidRow(column(12, h4("Klasterdusmeetod")), 
                               column(12, selectInput("klmeetod", 
                                                      label =NULL, 
                                                      choices = c("Keskmise kauguse meetod"="average", 
                                                                  "Täieliku seose meetod"="complete", 
                                                                  "Ühe seose meetod"="single",
                                                                  "Wardi meetod"="ward.D2"
                                                                 )
                                                     )
                                     )
                              ),
                      
                      
                      
                      
                      fluidRow(column(12, h4("Klastrite arv")),
                               column(12, selectInput("klastnr",
                                                      label =NULL,
                                                      choices = c("Vaikimisi"="Vaikimisi")
                                                     )
                                     )
                              ),
                      
                      
                      helpText(span("Märkus. Pärast muude sätete väärtuste muutmist on soovitatav jätta esmasel analüüsil klastrite arv \"Vaikimisi\" väärtuse peale."), 
                               style = "font-style: italic; 
                               font-size: 18px;"
                              ),
                      
                      
                      br(),
                      br(),
                      
                      fluidRow(actionButton("update", 
                                            span("Esita analüüs", 
                                                 style="margin-left: 5px;"), 
                                            icon("paper-plane"), 
                                            style="color: #fff; 
                                            background-color: #337ab7; 
                                            border-color: #2e6da4; 
                                            margin-top: 20px; 
                                            font-size: 20px; 
                                            margin-bottom: 30px;"
                                            
                                           ), align="center"
                              )
                      
                    ), #sidebarPanel lõpp
                    
                    ## Main panel VÄLJUNDITE jaoks ----
                    mainPanel(
                      
                      h1(htmlOutput("pealkiri_katse1"), 
                         align="center"),
                      
                      
                      h4(textOutput("teade"), 
                         align="center", 
                         style="color:red"),
                      
                      
                      fluidRow(id="myBox1", 
                               h2(id="myBox8", "1. Sobiv klastrite arv"),
                               
                               box(tableOutput('view'), width = 4, height=475),
                               
                               box(h5(span(textOutput("sona"), 
                                           style="font-weight: bold;"), 
                                      align="center"), 
                                   div(style='max-height:420px;
                                       overflow-x: hidden;
                                       overflow-y: scroll', 
                                       plotOutput("joonis")),
                                   width = 8, 
                                   height=475)
                              ),
                      
                      fluidRow(id="myBox2", 
                               h2("2. Iga klastrielemendi sobivus oma klastrisse"),
                               
                               box(div(style='max-height:420px;
                                       overflow-x: hidden;
                                       overflow-y: scroll;
                                       position: relative', 
                                       plotlyOutput("joonis1")),
                                   width = 12),
                               
                               box(DT::DTOutput('tbl'), width = 12)
                              ), 
                      
                      
                      fluidRow(id="myBox3", 
                               h2("3. Klastrid multidimensionaalse skaleerimise lahendusel"),
                               
                               box(plotlyOutput("joonis2"), 
                                   width = 12)
                               
                              )
                      
                       ) # mainPanel lõpp
                    
                  ) # sidebarLayout lõpp
                  
              ) # fluidPage lõpp
                
           ) # tabItem(tabName = "katse" ... lõpp
        
        ) # tabItems lõpp
      
     ) # dashboardBody lõpp
    
  ), # dashboardPage lõpp
  
  
  tags$footer("Kontakt: aimi.pikksaar@ut.ee", align = "center", style = "
              font-size: 22px;
              position:relative; #absolute
              bottom:0;
              width:100%;
              height:50px;
              color: white;
              padding: 10px;
              background-color: black;
              z-index: 1000;")
  
 ) # tagList lõpp



## server ----

server <- function(input, output, session) {
  
  
  andmefail <- reactive({
    inFile <- input$fail
    
    if (!is.null(inFile)){
      andmefail <- read.csv(inFile$datapath, 
                            sep = ";", 
                            encoding = "UTF-8", 
                            header=T)
    }
    
    if (input$checkbox5 == T){
      andmefail <- read.csv("katsetulemused.csv", 
                            sep = ";", 
                            encoding = "UTF-8", 
                            header=T)
    }
    
    
    if (is.null(inFile) & input$checkbox5 == F){
      andmefail <- NULL
    }
    
    andmefail
  })
  
  
  
  observe({  
    
    x <- "Vaikimisi"
    
   if (!(is.null(andmefail()))) {
     
     if ((nrow(andmefail())-1)<21){
       x <- c("Vaikimisi", as.character(2:(nrow(andmefail())-1)))
     }else{
       x <- c("Vaikimisi", as.character(2:20))
     }
     
    }
    
    
    updateSelectInput(session, "klastnr",
                      label = "Klastrite arv",
                      choices = x
    )
  })
  
  

  
  #############################################-
  
  andmed.d <- eventReactive(input$update,{
    if (is.null(andmefail())) return(NULL)
    andmed <- andmefail()[,-length(andmefail())]
    laused <- andmefail()[, length(andmefail())]
    rownames(andmed) <- andmed[, 1]
    andmed <- andmed[, -1]
    andmed.d <- as.dist(andmed)
    andmed.d
  })
  
  
  m <- eventReactive(input$update, {
    input$klmeetod
  })
  
  
  
  hca <-try({ eventReactive(input$update, {
    if (is.null(andmefail())) return(NULL)
    hclust(andmed.d(), m())
  })
  })
  
  
  asw <- eventReactive(input$update, {
    if (is.null(andmefail())) return(NULL)
    n <- nrow(andmefail())-1
    if (n<21){
      sapply(2:n, function(x) summary(silhouette(cutree(hca(), k = x), andmed.d()))$avg.width)
    }else{
      sapply(2:20, function(x) summary(silhouette(cutree(hca(), k = x), andmed.d()))$avg.width)
    }
     
  }) # leiab 2- kuni 20- (või katselausete arv - 1)-klastrilise lahenduse siluettide keskmise laiuse
  
  
  asw_tabel <- eventReactive(input$update, {
    if (is.null(andmefail())) return(NULL)
    
    n <- nrow(andmefail())-1 
    
    if (n<21){
      data.frame(klastrite_arv=2:n, asw=asw())
    }else{
      data.frame(klastrite_arv=2:20, asw=asw())
    }
   
  })
  
  
  k <- eventReactive(input$update, {
    if (is.null(andmefail())) {
      NULL
    
    } else if (input$klastnr %in% c("Vaikimisi")){ 
           asw_tabel()[asw_tabel()$asw==max(asw_tabel()$asw),1]
    
    } else {
      as.integer(input$klastnr)
    }
  })
 
  
  indeksid <- eventReactive(input$update, {
    if (is.null(andmefail())) return(NULL)
    sort(asw(), decreasing = T, index.return=T)$ix+1
  })
  
  
  siluetteide_laiused <- eventReactive(input$update, {
    if (is.null(andmefail())) return(NULL)
    round(sort(asw(), decreasing = T), 2)
  })
  
  
  klastrite_numbrid <- eventReactive(input$update, {
    if (is.null(andmefail())) return(NULL)
    cutree(hca(), k = k())
  })
  
  
  
  #############################################-
  
  laused <- eventReactive(input$update, {
    if (is.null(andmefail())) return(NULL)
    laused <- as.character(andmefail()[, length(andmefail())])
    
    
    uued_laused <- c()
    for (i in 1:length(laused)) {
      lause_vektorina <- strsplit(laused[i], " ")
      
      uus_lause <- c()
      rea_pikkus <- 0
      for (j in 1:length(lause_vektorina[[1]])) {
        sona <- lause_vektorina[[1]][j]
        
        
        rea_pikkus <- sum(rea_pikkus, (nchar(sona)+1))
        
        if (startsWith(sona, "#")) {
          sona <- paste0("<b>", gsub("#", "", sona), "</b>")
        }
        
        
        if (rea_pikkus>22) {
          sona <- paste0(sona, "\n")
          rea_pikkus <- 0
        }
        
        uus_lause <- c(uus_lause, sona)
      }
      
      uus_lause <- paste(uus_lause, collapse=" ")
      uus_lause <- gsub("\n ", " \n", uus_lause)
      uus_lause <- paste0("<span style='font-family: Verdana;'>", uus_lause, "</span>")
      uued_laused <- c(uued_laused, uus_lause)
    }
    
    uued_laused
    
  })
  
  
  lausete_tabel <- eventReactive(input$update, {
    if (is.null(andmefail())) return(NULL)
    
    datatable(data.frame(LAUSE=laused()), 
              escape=F, 
              options = list(lengthChange = F, 
                             lengthMenu = c(1, 10, 100), 
                             pageLength = 1, 
                             dom = 'ft', 
                             language = list(search = 'Otsi järjekorranumbri järgi:')))
  }) 
  
  
  
  #############################################-
  
  siluettide_tabel <- eventReactive(input$update,{
    if (is.null(andmefail())) return(NULL)
    
    t <- tibble(`Klastrite arv`=as.integer(indeksid()), 
                `Siluettide keskmine laius`= siluetteide_laiused())
    
    t %>% knitr::kable("html", align = "c") %>% 
      kable_styling() %>% 
      scroll_box(height = "450px")
  })
  
  
  
  #############################################-
  
  klaster_joonis <- eventReactive(input$update,{
    if (is.null(andmefail())) return(NULL)
    
    #library(cluster)
    #library(ggplot2)
    #library(ggdendro)     #dendro_data(...) jaoks
    clust <- cutree(hca(),k=k())
    dendr <- dendro_data(hca(), type="rectangle")
    clust.df <- data.frame(label=as.character(andmefail()[,1]), cluster=factor(clust))
    dendr[["labels"]]   <- merge(dendr[["labels"]], clust.df, by="label")
    rect <- aggregate(x~cluster,label(dendr),range)
    rect <- data.frame(rect$cluster,rect$x)
    ymax <- mean(hca()$height[length(hca()$height)-((k()-2):(k()-1))])
    
    
    joonis <- ggplot() +
      geom_segment(data=segment(dendr), 
                   aes(x=x, y=y, xend=xend, yend=yend)) +
      geom_text(data=label(dendr), 
                aes(x, y, label=label, hjust=0, color=cluster), 
                size=4, 
                nudge_y = 0.2) +
      geom_rect(data=rect, 
                aes(xmin=X1-.3, xmax=X2+.3, ymin=0, ymax=ymax),
                color=NA, 
                fill="black", 
                alpha=0.2) +  
      coord_flip() +
      scale_y_reverse(expand=c(0.8, 0)) +
      theme_dendro() +
      scale_color_manual(values = c("tomato", "yellow3", "green",  "cornflowerblue",
                                    "cyan", "mediumorchid2","grey51", "yellow", 
                                    "orange", "lightsteelblue1", "plum1", "moccasin", 
                                    "gray80", "palevioletred3", "olivedrab1", "peru", 
                                    "turquoise", "lightpink", "darkseagreen1", "lemonchiffon")) +
      theme(legend.position="none",
            plot.margin = margin(-0.5, 1.5, 0, -3, unit="cm"))
      
      
    
    joonis
    
  })
  
  #############################################-
  
  klaster_joonis_pealkiri <- eventReactive(input$update,{
    if (is.null(andmefail())) return(NULL)
    
    if (m() %in% c("average")) met="Keskmise kauguse meetod"
    if (m() %in% c("complete")) met="Täieliku seose meetod"
    if (m() %in% c("single")) met="Ühe seose meetod"
    if (m() %in% c("ward.D2")) met="Wardi meetod"
    
    paste(met, as.character(k()), "klastriga")
    
  })
  
  #############################################-
  
  siluettide_joonis <- eventReactive(input$update,{
    if (is.null(andmefail())) return(NULL)
    
    #library(factoextra)
    #library(cluster)
    
    sil <- silhouette(klastrite_numbrid(), andmed.d())
    
    sil_dat <- data.frame(silt = as.character(andmefail()[,1]), 
                          klaster = sil[,1], 
                          laius = sil[,3], 
                          lause = laused())
    
    sil_dat2 <- sil_dat[with(sil_dat, order(klaster, -laius)),]
    
    sil_dat2$silt <- as.character(sil_dat2$silt)
    sil_dat2$lause <- as.character(sil_dat2$lause)
    
    d_cl <- fviz_silhouette(sil, label = T, print.summary = F) +
      scale_x_discrete(labels=sil_dat2$silt) +
      coord_flip() +
      theme(axis.text.x = element_text(angle=0, hjust=1), 
            legend.position="none", 
            plot.title = element_text(hjust=0.5), 
            plot.margin = margin(1, 0, 0, 0, unit="cm")) +
      scale_fill_manual(values = c("tomato", "yellow3", "green",  "cornflowerblue",
                                   "cyan", "mediumorchid2","grey51", "yellow", 
                                   "orange", "lightsteelblue1", "plum1", "moccasin", 
                                   "gray80", "palevioletred3", "olivedrab1", "peru", 
                                   "turquoise", "lightpink", "darkseagreen1", "lemonchiffon")) +
      scale_color_manual(values = c("tomato", "yellow3", "green",  "cornflowerblue",
                                    "cyan", "mediumorchid2","grey51", "yellow", 
                                    "orange", "lightsteelblue1", "plum1", "moccasin", 
                                    "gray80", "palevioletred3", "olivedrab1", "peru", 
                                    "turquoise", "lightpink", "darkseagreen1", "lemonchiffon")) +
      labs(y=NULL, title=NULL)
    
    
    
    korgus <- 100 + 21 * nrow(andmefail())
    
    d_cl <- ggplotly(d_cl) %>% layout(autosize = T, height =korgus)
    
    p <- plotly_build(d_cl)
    
    i <- k()+1
    
    p$x$data[[i]] <- NULL #eemaldab yintercepti trace-i
    
    
    for (i in 1:length(p$x$data)){
      siltide_vektor <- character()
      for (j in 1:length(p$x$data[[i]]$x)){
        siltide_vektor <- c(siltide_vektor, 
                            sil_dat2$lause[sil_dat2$laius==p$x$data[[i]]$x[j]])
      }
      
      
      p$x$data[[i]]$text <- siltide_vektor
    }
    
    p #joonis1
    
  })
  
  
  #############################################-
  
  mds_joonis <- eventReactive(input$update,{
    if (is.null(andmefail())) return(NULL)
    
    #library(MASS)
    andmed.sammon = sammon(andmed.d())
    andmed.sammon
    
    x = andmed.sammon$points[,1]
    y = andmed.sammon$points[,2]
    
    mdsout <- data.frame(andmed.sammon$points)
    mdsout$klastri_nr <- klastrite_numbrid()
    mdsout$sildid <- rownames(mdsout)
    mdsout$laused <- as.character(laused())
    
    j <- ggplot(mdsout, aes(x = X1, y = X2)) +
      geom_point(aes(color = as.factor(klastri_nr))) +
      scale_color_manual(values = c("tomato", "yellow3", "green",  "cornflowerblue",
                                    "cyan", "mediumorchid2","grey51", "yellow", 
                                    "orange", "lightsteelblue1", "plum1", "moccasin", 
                                    "gray80", "palevioletred3", "olivedrab1", "peru", 
                                    "turquoise", "lightpink", "darkseagreen1", "lemonchiffon")) +
      theme_dendro() +
      scale_x_continuous(limits = c((min(x)-2), (max(x)+2))) +
      scale_y_continuous(limits = c((min(y)-2), (max(y)+2))) +
      theme(legend.position="none",
            plot.margin = margin(0, 0, 0, 0, unit="cm"))
    
    p <- plotly_build(j) 
    
    
    for (i in 1:length(p$x$data)){
      siltide_vektor <- character()
      for (j in 1:length(p$x$data[[i]]$x)){
        siltide_vektor <- c(siltide_vektor, 
                            mdsout$laused[mdsout$"X1"==p$x$data[[i]]$x[j]])
      }
      
      
      p$x$data[[i]]$text <- siltide_vektor
    }
    
    joonis2 <- p %>% add_trace(x = x,
                               y = y+0.2, 
                               type = "scatter",
                               #size=0.1,
                               mode = "text", 
                               hoverinfo="none", 
                               textfont = list(size = 11),
                               text = mdsout$sildid
    )
    
    joonis2
    
  })
  
  
  
  
  
  
  ######################################################-
  
  empty <- function(x){
    ifelse(x %in% c(NA, NaN, NULL, 0, ""), TRUE, FALSE)
  }
  
  
  sonaliik <- eventReactive(input$update,{
    tolower(unlist(strsplit(gsub("X.U.FEFF.", "", colnames(andmefail())[1]), "_"))[1])
  })
  
  
  uuritav_sona <- eventReactive(input$update,{
   tolower(unlist(strsplit(gsub("X.U.FEFF.", "", colnames(andmefail())[1]), "_"))[2])
  })
  
  
  observe(output$pealkiri_katse1 <- renderUI({HTML("ANALÜÜSITULEMUSED")}))
  
  
  observeEvent(input$update,{
    
    if (!is.null(andmefail())){
      output$pealkiri_katse1 <- renderUI({
        HTML(paste0("ANALÜÜSITULEMUSED: ", sonaliik(), " <em>", uuritav_sona(), "</em>"))
      })
      
    if (empty(sonaliik())){output$pealkiri_katse1 <- renderUI({HTML("ANALÜÜSITULEMUSED")})}
    if (empty(uuritav_sona())){output$pealkiri_katse1 <- renderUI({HTML("ANALÜÜSITULEMUSED")})}
    if(colnames(andmefail())[1]=="X.U.FEFF."){output$pealkiri_katse1 <- renderUI({HTML("ANALÜÜSITULEMUSED")})}
    
      } 
  })
  
  ######################################################-
  
  t <- eventReactive(input$update,{
  
    if (is.null(andmefail())){  
      return("Andmestik on valimata!")
   }
    
    
    if ((class(try(klaster_joonis_pealkiri()[1]))=="try-error") | (class(try(lausete_tabel()[1]))=="try-error") | (class(try(siluettide_tabel()[1]))=="try-error") | (class(try(klaster_joonis()[1]))=="try-error") | (class(try(siluettide_joonis()[1]))=="try-error") | (class(try(mds_joonis()[1]))=="try-error")) {
      return("Andmefail ei ole vormistatud nõuetekohaselt! Vt nõudeid täpsemalt jaotise \"Tutvustus\" lõpust.")
    }
  
  }) 
  
  
  
  
  output$teade <- renderText({t()})
  
  
  
  ######################################################-
  
  observe(
    if (input$update == 0) {
      shinyjs::hide(id="myBox1")
      shinyjs::hide(id="myBox2")
      shinyjs::hide(id="myBox3")
    }
  )
  
  
  observeEvent(input$update,{
    if (is.null(andmefail())){  
      shinyjs::hide(id="myBox1")
      shinyjs::hide(id="myBox2")
      shinyjs::hide(id="myBox3")
     
    }else if ((class(try(klaster_joonis_pealkiri()[1]))=="try-error") | (class(try(lausete_tabel()[1]))=="try-error") | (class(try(siluettide_tabel()[1]))=="try-error") | (class(try(klaster_joonis()[1]))=="try-error") | (class(try(siluettide_joonis()[1]))=="try-error") | (class(try(mds_joonis()[1]))=="try-error")) {
      
      shinyjs::hide(id="myBox1")
      shinyjs::hide(id="myBox2")
      shinyjs::hide(id="myBox3")
     
    }else{
      shinyjs::show("myBox1")
      shinyjs::show(id="myBox2")
      shinyjs::show(id="myBox3")
      
    }
    
  }
)


  
  ## VÄLJUNDITEKS ----
  
  
  output$sona <- renderText({
    klaster_joonis_pealkiri()
  })
  
  
  output$tbl = renderDT(
    lausete_tabel()
  )
  
  
  output$view <- function(){
    siluettide_tabel()
  }
  
  
  korgus <- eventReactive(input$update, {
    100 + 21 * nrow(andmefail())
  })
  
  
  observeEvent(input$update, {
    output$joonis <- renderPlot(
      klaster_joonis(), height = korgus()
    )
  })
  
  
  
  output$joonis1 <- renderPlotly({
    siluettide_joonis()
  })
  
  
  
  output$joonis2 <- renderPlotly({
    mds_joonis()
  })
  
  
  
  
  
} # server lõpp



shinyApp(ui, server)
