
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Projekt Statistiline andmeteadus ja visualiseerimine"),
  h3("Kristi Krebs ja Ahto Salumets"), # Taavi lisatud
  sidebarLayout(
    sidebarPanel(
      p("Siin saab valida kuvatavaid arve ja isikuid "),
      radioButtons("valik2", "Vali, kas soovid vaadata rikaste või vaeste top-i (teadusrahastuse alusel) absoluutskaalal",
                   choices = c("Rikkad", "Vaesed"), selected = "Rikkad"),
      sliderInput("top2",
                  "Vali väljastatavad rahastuse saajat:",
                  min = 10,
                  max = 50,
                  value = 15),
      radioButtons("valik", "Vali, kas soovid vaadata rikaste või vaeste top-i (teadusrahastuse alusel) ühe grandi tasemel",
                   choices = c("Rikkad", "Vaesed"), selected = "Rikkad"),
      sliderInput("top",
                   "Vali väljastatavad rahastuse saajat:",
                   min = 10,
                   max = 50,
                   value = 15),
      textInput("vastutajad", label= h3("Vali, kelle projektide arvu soovid teada:"), value= "Sisesta nimi"),
      textInput("asutuse_valik", label = h3("Vali asutus, kelle rahastust soovid täpsemalt kuvada"), value = "Sisesta asutus"),
      tags$head(tags$script(src = "message-handler.js")),
      actionButton("Uuendan", label = "Uuenda andmestikku"),
      helpText("Andmestiku uuendamine võtab omajagu aega...",
               "Peale andmestiku uuendamist tee programmile",
               "restart")
    ),

    
    # Show a plot of the generated distribution
    mainPanel(
      h1("Eesti teadusinfosüsteemis olevate projektiandmete analüüs"),
      br(),
      h2("Kuvatav graafik näitab vastavalt kasutaja valikul kas rikaste või vaeste top-i absoluutskaalal"),
      textOutput("valitud_top2"),
      plotOutput("plot_top2"),
      h2("Kuvatav graafik näitab vastavalt kasutaja valikul kas rikaste või vaeste top-i ühe toetuse skaalal (kestvad projektid)"),
      textOutput("valitud_top"),
      plotOutput("plot_top"),
      h2("Kuvatav tabel näitab kasutaja valitud isiku projekte ja rahastusi"),
      textOutput("vastutaja"),
      tableOutput("tabel_vastutaja"),
      h4("Kuvatav graafik annab ülevaate 25 peamise asutuse kestvatest ja lõppenud projektidest"),
      plotOutput("plot_asutus"),
      textOutput("valitud_asutus"),
      tableOutput("valitud_asutus_tabel")
    )
  ),
  hr(),
  HTML('<footer><b>IUT</b>- Institutsionaalse uurimistoetus,
       <b>PUT</b>- personaalne uurimistoetus (JD- järeldoktor, SP-stardiprojekt, OP- otsinguprojekt), 
       <b>SF</b>- Sihtfinantseering, <b>ETF</b>- Eesti Teadusfond, <b>MOB</b>- Mobilitas Pluss (ERC- ERC granditaotlemise toetus, 
       JD-järeldoktoritoetus, TP- Tagasipöörduva teadlase toetus, TT- tippteadlase toetus),
       <b>EKKTT</b>- Eesti keele keeletehnoloogiline tugi,<b>EKKM</b>- Eesti keel ja kultuurimälu,
       <b>EKRM</b>- Eesti keel ja rahvuslik mälu,<b>EKT</b>-Eesti keeletehnoloogia,  <b>HLK</b>- Humanitaar- ja loodusteaduslikud kogud, 
       <b>ERMOS</b>- Estonian Research Mobility Scheme, <b>MJD</b>- Mobilitas järeldoktori uurimistoetus, <b>MTT</b> - Mobiilitas tippteadlase uurimistoetus, 
       <b>MEMRUP</b>- Maaeluministeeriumi rakendusuuringute programm, <b>EMP</b>-Norra ja EMP finantsmehhanismide teaduskoostöö toetus,
       <b>NSP</b>- Nutika spetsialiseerumise rakendusuuringud, <b>RITA1</b>- Valdkondliku teadus-ja arendustegevuse tugevdamine (RITA) tegevus1,
       <b>TAP</b>- teadusaparatuuri ja -seadmete kaasajastamine, <b>AP</b>- Aparatuuri toetus, <b>TK</b>-tippkeskus,
       <b>RP</b>- raamprogramm</footer>')
))
