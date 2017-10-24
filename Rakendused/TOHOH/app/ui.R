library(shiny)
library(shinydashboard)


# Header elements for the visualization
header <- dashboardHeader(title = "TOHOH!", disable = FALSE)

# Sidebar elements for the search visualizations
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sõnade ülevaade", tabName = "dashboard", icon = icon("bar-chart")),
    menuItem("Pealkirjade analüüs", icon = icon("area-chart"), tabName = "widgets"),
    menuItem("Meist", tabName = "info", icon = icon("info"))
  )
)

#Body elements for the search visualizations.
body <- dashboardBody(
  tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "custom.js"),
    tags$style(HTML('/* main sidebar */
                    .skin-purple .main-sidebar {
                    background-color:  grey;
                    }
                    '))
  ),
  tabItems(
    tabItem(tabName = "info",
            h3("Tähelepanu!"),
            br(),
            div(p("Tegemist on Tartu Ülikooli aine \"Statistiline andmeteadus ja visualiseerimine\" raames loodud rakendusega.")),
            br(),
            p("Vahemikus 12-28.aprill 2017 on kord päevas kogutud artiklite pealkirju neljast Eesti uudiseportaalist:"),
            HTML("<ul><li>Delfi</li><li>ERR</li><li>Õhtuleht</li><li>Postimees</li></ul>"),
            p("Kogutud lausetest on esitatud mitmesugused ülevaatlikud visualisatsioonid."),
            p(HTML("Kogutud pealkirjades esinevad sõnad on töötlemise ja võrdlemise lihtsustamiseks viidud algvormide tasemele kasutades <b>estnltk</b> paketti (<a href='https://estnltk.github.io'>https://estnltk.github.io</a>).")),
            br(),br(),
            p(HTML("<b>Kaebuste tekkimise korral pidage nõu Liis Kolbergi või Mari-Liis Allikiviga.</b>"))
    ),
    tabItem(tabName = "dashboard",
            h2("Pealkirjades esinevate sõnade ülevaade"),
            br(),
            p("Näitame sõnade proportsionaalset jaotust vastavalt valitud sõnaliigile ning visualiseerime sõnapilvena pealkirjades enim esinevaid sõnu. 
              Kuvatud on ka 10 populaarseimat sõna uudiseportaali kaupa ning sõnad, mis unikaalselt on esinenud ainult valitud portaali pealkirjades. Eriti huvitavad sõnaliigid on näiteks hüüdsõnad ja omadussõna ülivõrded."),
            p("Aga lase käia ja uudista ise!"),
            fluidRow(
              box(
                selectInput("sonaliik", "Vali sõnaliik mida kuvada:",
                            c("Kõik sõnad" = "koik",
                              "Nimisõna" = "S",
                              "Tegusõna" = "V",
                              "Asesõna" = "P",
                              "Hüüdsõna" = "I",
                              "Järgarvsõna" = "O",
                              "Käändamatu omadussõna" = "G",
                              "Kaassõna" = "K",
                              "Lausemärk" = "Z",
                              "Lühend" = "Y",
                              "Määrsõna" = "D",
                              "Omadussõna algvõrre" = "A",
                              "Omadussõna keskvõrre" = "C",
                              "Omadussõna ülivõrre" = "U",
                              "Pärisnimi" = "H",
                              "Põhiarvsõna" = "N",
                              "Sidesõna" = "J",
                              "Verbi juurde kuuluv sõna" = "X"
                            )
                )
              )
            ),
            fluidRow(
              box(title="Sõnaliigi jaotus", plotOutput("jaotus"), background = "purple"),
              box(title="Sõnapilv", plotOutput("sonapilv"), background = "purple")
            ),
            fluidRow(
              tabBox(
                title = "10 populaarseimat sõna",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1",
                tabPanel("Delfi", plotOutput("top_delfi")),
                tabPanel("ERR", plotOutput("top_err")),
                tabPanel("Õhtuleht", plotOutput("top_ohtuleht")),
                tabPanel("Postimees", plotOutput("top_postimees"))
              ),
              tabBox(
                title = "Unikaalsed sõnad",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset2",
                tabPanel("Delfi", plotOutput("uni_delfi")),
                tabPanel("ERR", plotOutput("uni_err")),
                tabPanel("Õhtuleht", plotOutput("uni_ohtuleht")),
                tabPanel("Postimees", plotOutput("uni_postimees"))
              )
            )
            
    ),
    tabItem(tabName = "widgets",
              h2("Pealkirjade analüüs"),
              br(),
              p("Alloleval joonisel on näha üks võimalik uudisteportaalide pealkirjade klasterdus."),
                p("Uudista ja vaata, milliseid pealkirjad näivad meie meetodi arvates sarnased!*"),
            br(),
              box(title=NULL, height="auto", width="auto", plotlyOutput("pca", height="auto", width="auto"), background = "purple"),
            br(),
            h4("Huvitatutele:"),
            p(HTML("<ul><li>Et saada sõnadele vastavad numbrilised vektorid, kasutasime eesti keele jaoks valmis treenitud <b>Word2Vec’i</b> mudeleid algvormis sõnade jaoks (<a href='https://github.com/estnltk/word2vec-models/blob/master/README.md'>https://github.com/estnltk/word2vec-models/blob/master/README.md</a>, täpsemalt lemmas.sg.s100.w2v.bin.gz).</li>
                   <li>Kuna klasterdada oli vaja lauseid, siis lausete vektorite jaoks summeerisime lauses esinevate sõnade vektorid.</li>
                   <li>Niimoodi saadud lausete vektoritele rakendasime PCA meetodit ja voila!</li></ul>"))
    )
  )
  
) # /dashboardBody


dashboardPage(header, sidebar, body, skin = "purple")
