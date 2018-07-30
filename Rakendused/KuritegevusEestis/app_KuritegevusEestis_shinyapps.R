library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(leaflet)
library(geosphere)
library(Hmisc)
library(stringr)
library(shinyjs)
library(shinyBS) #?

#Statistilise andmeteaduse projekt
#Autoorid: Diana Sokurova ja Linnet Puskar
#Shinyapps.io jaoks kohendanud Taavi Unt



# Tekitan uued tunnused, kus on tavapäärased koordinaadid
lest_geo = function(x, y){
  # Konverteerib L-EST97 koordinaadid kaardirakendusele sobivaks (nn tavapäärasteks koordinaatideks)
  # Kasutatud on maa-ameti veebilehel olevat php koodi:  
  # http://www.maaamet.ee/rr/geo-lest/files/geo-lest_function_php.txt
  
  
  a = 6378137
  F = 1 / 298.257222100883
  ESQ = F + F - F * F
  B0 = (57 + 31 / 60 + 3.194148 / 3600) / (180/pi)
  L0 = 24 / (180/pi)
  FN = 6375000
  FE = 500000
  B2 = (59 + 20 / 60) / (180/pi)
  B1 = 58 / (180/pi)
  xx = x - FN
  yy = y - FE
  t0 = sqrt((1 - sin(B0)) / (1 + sin(B0)) * ((1 + sqrt(ESQ) * sin(B0)) / (1 - sqrt(ESQ) * sin(B0))) ** sqrt(ESQ))
  t1 = sqrt((1 - sin(B1)) / (1 + sin(B1)) * ((1 + sqrt(ESQ) * sin(B1)) / (1 - sqrt(ESQ) * sin(B1))) ** sqrt(ESQ))
  t2 = sqrt((1 - sin(B2)) / (1 + sin(B2)) * ((1 + sqrt(ESQ) * sin(B2)) / (1 - sqrt(ESQ) * sin(B2))) ** sqrt(ESQ))
  m1 = cos(B1) / (1 - ESQ * sin(B1) * sin(B1)) ** 0.5
  m2 = cos(B2) / (1 - ESQ * sin(B2) * sin(B2)) ** 0.5
  n1 = (log(m1) - log(m2)) / (log(t1) - log(t2))
  FF = m1 / (n1 * t1 ** n1)
  p0 = a * FF * t0 ** n1
  p = (yy * yy + (p0 - xx) * (p0 - xx)) ** 0.5
  t = (p / (a * FF)) ** (1 / n1)
  FII = atan(yy / (p0 - xx))
  LON = FII / n1 + L0
  u = (pi / 2) - (2 * atan(t))
  LAT = (u + (ESQ / 2 + (5 * ESQ ** 2 / 24) + (ESQ ** 3 / 12) + (13 * ESQ ** 4 / 360)) * sin(2 * u) +
           ((7 * ESQ ** 2 / 48) + (29 * ESQ ** 3 / 240) + (811 * ESQ ** 4 / 11520)) * sin(4 * u) +
           ((7 * ESQ ** 3 / 120) + (81 * ESQ ** 4 / 1120)) * sin(6 * u) + (4279 * ESQ ** 4 / 161280) * sin(8 * u))
  LAT = LAT * 180/pi
  LON = LON * 180/pi
  
  return(data.frame(lat = LAT, lon = LON))
}

lest_geo_square = function(x_sq, y_sq){
  # PPA andmestikus on sündmused paigutatud ruudustikku
  # Konverteerib ruudustiku LEST kooridnaadid tavapärasesse koordinaadistikku
  x_l = str_split_fixed(x_sq, "-", 2)
  x_l = data.frame(x_l, stringsAsFactors = FALSE)
  x_l[,1] = as.numeric(x_l[,1])
  x_l[,2] = as.numeric(x_l[,2])
  colnames(x_l) = c("x1","x2")
  y_l = str_split_fixed(y_sq, "-", 2)
  y_l = data.frame(y_l, stringsAsFactors = FALSE)
  y_l[,1] = as.numeric(y_l[,1])
  y_l[,2] = as.numeric(y_l[,2])
  colnames(y_l) = c("y1","y2")
  lat_lon1 = lest_geo(x_l$x1, y_l$y1)
  lat_lon2 = lest_geo(x_l$x2, y_l$y2)
  colnames(lat_lon1) = paste0(colnames(lat_lon1), "1")
  colnames(lat_lon2) = paste0(colnames(lat_lon2), "2")
  out = cbind(lat_lon1, lat_lon2)
  return(out)
}

add_coords = function(dataset){
  # lisab andmestikule uute tunnustena konverteeritud koordinaadid
  return(cbind(dataset,lest_geo_square(dataset$Lest_X, dataset$Lest_Y)))
}

#liikluse ikoonide hulk
iconSet = awesomeIconList(
  buss = makeAwesomeIcon(icon='fa-bus', library='fa', markerColor = 'red'),
  jalgrattas = makeAwesomeIcon(icon='fa-bicycle', library='fa', markerColor = 'red'),
  traktor = makeAwesomeIcon(icon='fa-wrench', library='fa', markerColor = 'red'),
  motor = makeAwesomeIcon(icon='fa-motorcycle', library='fa', markerColor = 'red'),
  auto = makeAwesomeIcon(icon='fa-car', library='fa', markerColor = 'red'),
  veoauto = makeAwesomeIcon(icon='fa-truck', library='fa', markerColor = 'red'),
  muu = makeAwesomeIcon(icon='fa-road', library='fa', markerColor = 'red'),
  haagis = makeAwesomeIcon(icon='fa-cubes', library='fa', markerColor = 'red')
)



# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "purple",
                    # Application title
                    dashboardHeader(title="Kuritegevus Eestis"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Pealeht", tabName = "pealeht", icon = icon("map")),
                        menuItem("Programmist", tabName = "programmist", icon = icon("align-left"))
                      )
                    ),#dashboardSidebar lõpp
                    dashboardBody(
                      useShinyjs(),
                      
                      # First tab content
                      tabItems(
                        tabItem(
                          tabName="pealeht",
                          # Boxes need to be put in a row (or column)
                          fluidRow(
                            tabBox(
                              title = "Valikud", id="tabset1",
                              tabPanel( "Kuupäev ja süütegu",
                                        dateRangeInput('dateRange',
                                                       label = 'Kuupäev kujul aaaa-kk-pp',
                                                       start = Sys.Date() - 14, end = Sys.Date(), 
                                                       min = as.Date("2017-01-01"), max = Sys.Date(), 
                                                       weekstart = 1, language = "et", separator = "kuni"
                                        ),
                                        
                                        checkboxGroupInput("checkGroup", label = "Milliseid süütegusid kuvada?", 
                                                           choices = list("Liiklusjärelvalve käigus avastatud süüteod" = "liiklus", 
                                                                          "Avaliku korra vastased süüteod" = "avalik", 
                                                                          "Varavastased süüteod" = "vara"),
                                                           selected = NULL),
                                        bsAlert("andmete_laadimine")
                              ),
                              tabPanel("Liiklus",
                                       #Täiendav informatsioon. Avab ainult siis, kui esimene valik on tehtud
                                       conditionalPanel(condition=" input.checkGroup.indexOf('liiklus') > -1 ",
                                                        checkboxGroupInput("checkGroup_liiklus", 
                                                                           label ="Sõiduki liik")
                                       )
                              ),
                              tabPanel("Avalik",
                                       conditionalPanel(condition=" input.checkGroup.indexOf('avalik') > -1 ",
                                                        checkboxGroupInput("checkGroup_avalik", label ="Avaliku koha süüteo paragrahv")
                                       )
                              ), 
                              tabPanel("Vara",
                                       conditionalPanel(condition=" input.checkGroup.indexOf('vara') > -1 ",
                                                        checkboxGroupInput("checkGroup_vara", 
                                                                           label ="Varavastase kuriteo paragrahv")
                                       )
                              )
                              ,width = 3), #box'i lõpp
                            
                            
                            box(
                              title = "Kuritegevuse kaardistamine",
                              leafletOutput("Kaart", height = 800), height = "auto", width = 9
                            ) #box'i lõpp
                            
                          ) #Fluigrow lõpp
                        ), #TabItem "pealeht" lõpp
                        
                        tabItem(tabName = "programmist",
                                h2("Selle projekti tutvustus"),
                                p("Tere!"),
                                tags$p("Teie ees on Tartu Ülikooli kursuse „Statistiline andmeteadus ja visualiseerimine“ lõpuprojekt. 
                                       Projekti eesmärgiks oli luua kaardirakendus, mis kujutaks mingi ajaperioodi jooksul Eestis 
                                       asetleidnud kuritegevust ja seaduserikkumisi. 
                                       Kaart on valmistatud Politsei- ja Piirivalveameti avaandmete põhjal, mida uuendatakse igal neljapäeval ning mis on saadaval siin:",br(),
                                       a("https://www2.politsei.ee/et/organisatsioon/analuus-ja-statistika/avaandmed.dot"),"."),
                                
                                p("Kaardi kasutamiseks tuleb määrata kaks kuupäeva, mille vahel toimunud sündmused kaardile kuvatakse.
                                  Seejärel tuleb valida, mis tüüpi süüteod kaardile kantakse:"),
                                tags$ul(
                                  tags$li("liiklusjärelvalve käigus avastatud
                                          süüteod;"), 
                                  tags$li("avaliku korra vastased ja avalikus kohas toime pandud varavastased süüteod;"),
                                  tags$li("varavastased süüteod;"),
                                  tags$li("kõik koos.")),
                                tags$p("Pärast seda saab valida kuvatavaid andmeid rikutud avaliku korra
                                       ja varavastase süüteo paragrahvi ning sõiduki liigi järgi. Kaardil kuvatud märgisele vajutades
                                       kuvatakse süüteoga seotud informatsioon. Asukohamärgistel kasutatud pildid pärinevad siit:", br(),
                                       a("https://fontawesome.com/"),"."),
                                p("Soovime Teile head kasutamist!"),
                                p("Diana Sokurova ja Linnet Puskar")
                                )#TabItem "programmist" lõpp
                                ) #TabItems lõpp
                    ) #dashboardBody lõpp
                    ) #dashboardPage lõpp


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    if ("liiklus" %in% input$checkGroup){
      show(selector = "#tabset1 li a[data-value=Liiklus]", anim = TRUE, animType = "fade")
    } else{
      hide(selector = "#tabset1 li a[data-value=Liiklus]", anim = TRUE, animType = "fade")
    }
  })
  
  observe({
    if ("avalik" %in% input$checkGroup){
      show(selector = "#tabset1 li a[data-value=Avalik]", anim = TRUE, animType = "fade")
    } else{
      hide(selector = "#tabset1 li a[data-value=Avalik]", anim = TRUE, animType = "fade")
    }
  })
  
  observe({
    if ("vara" %in% input$checkGroup){
      show(selector = "#tabset1 li a[data-value=Vara]", anim = TRUE, animType = "fade")
    } else{
      hide(selector = "#tabset1 li a[data-value=Vara]", anim = TRUE, animType = "fade")
    }
  })
  
  output$Kaart<-renderLeaflet({
    
    map<-leaflet()
    
    #Seadistan kaardi, et avanedes paistaks Eesti.
    map<-map %>% 
     setView(lng = 25.031667, lat = 58.648333, zoom = 7) %>% 
     addProviderTiles("Esri.WorldImagery") %>%
     addProviderTiles(providers$Esri.WorldStreetMap)
    
    #Kui on tehtud linnuke liikluse süütegude kasti juurde, siis kantakse graafikule liiklusega seotud süüteod.
    if("liiklus" %in% input$checkGroup){
      
      if (!exists("liiklusjarelevalve_1")){
        createAlert(session, "andmete_laadimine", "liiklus_laadimine", title = "Andmete laadimine",
                    content = "Liiklusjärelvalve andmestiku laadimine....", append = FALSE,
                    dismiss = FALSE, style = "info")
        
        liiklusjarelevalve_1 = read_delim("https://opendata.smit.ee/ppa/csv/liiklusjarelevalve_1.csv", 
                                          delim = "\t", escape_double = FALSE, trim_ws = TRUE,
                                          col_types = "-c--------------ccc-------" )
        liiklusjarelevalve_1$ToimKpv = as.Date(liiklusjarelevalve_1$ToimKpv)
        liiklusjarelevalve_1<-add_coords(liiklusjarelevalve_1)
        
        #ikoonid, mis määrab milline ikoon tuleb iga sõiduki korral
        #SoidukLiik->SoidukLiik_ilus
        #Informatsioon
        liiklusjarelevalve_1<-mutate(liiklusjarelevalve_1,
                                     ikoonid = case_when(
                                       SoidukLiik == "BUSS" ~ "buss",
                                       SoidukLiik == "VEOAUTO" ~ "veoauto",
                                       SoidukLiik == "JALGRATAS" ~ "jalgrattas",
                                       SoidukLiik == "SOIDUAUTO" ~ "auto",
                                       SoidukLiik %in% c(NA,"MUU_SOIDUK","TUNDMATU") ~ "muu", #ikoon- tee
                                       SoidukLiik %in% c("MOOTORRATAS","MOPEED","NELIRATAS","PISIMOPEED") ~ "motor", #ikoon - motorratas
                                       SoidukLiik %in% c("TRAKTOR","LIIKURMASIN","MAASTIKUSOIDUK") ~ "traktor", # ikoon - mutrivõti
                                       TRUE ~ "haagis" # ikoon - kuubid
                                     ), SoidukLiik_ilus= ifelse(is.na(SoidukLiik),"Pole määratud",capitalize(tolower(SoidukLiik)))
        )
        
        liiklusjarelevalve_1$SoidukLiik_ilus[liiklusjarelevalve_1$SoidukLiik=="SOIDUAUTO"]<-"Sõiduauto"
        liiklusjarelevalve_1$SoidukLiik_ilus[liiklusjarelevalve_1$SoidukLiik=="MUU_SOIDUK"]<-"Muu sõiduk"
        liiklusjarelevalve_1$SoidukLiik_ilus[liiklusjarelevalve_1$SoidukLiik=="MAASTIKUSOIDUK"]<-"Maastikusõiduk"
        
        liiklusjarelevalve_1 <- dplyr::mutate(liiklusjarelevalve_1, 
                                              Informatsioon = paste(SoidukLiik_ilus, format(ToimKpv,"%d.%m.%Y"), sep=", "))
        
        liiklusjarelevalve_1 <<- liiklusjarelevalve_1
        
        closeAlert(session, "liiklus_laadimine")
        updateCheckboxGroupInput(session, "checkGroup_liiklus", label ="Sõiduki liik", 
                                 choices =unique(liiklusjarelevalve_1$SoidukLiik_ilus),
                                 selected=unique(liiklusjarelevalve_1$SoidukLiik_ilus))
      }
      
      alamandmestik<-filter(liiklusjarelevalve_1, liiklusjarelevalve_1$ToimKpv>=input$dateRange[1], liiklusjarelevalve_1$ToimKpv<=input$dateRange[2],
                            liiklusjarelevalve_1$SoidukLiik_ilus %in% input$checkGroup_liiklus )
      map<-map %>%
        addAwesomeMarkers(lng = alamandmestik$lon1, lat = alamandmestik$lat1, icon = iconSet[alamandmestik$ikoonid],  popup=alamandmestik$Informatsioon)
      #addCircleMarkers(lng = alamandmestik$lon1,lat = alamandmestik$lat1,color = 'red', radius=8, weight = 1,  popup=alamandmestik$Informatsioon)
    }
    #Kui on tehtud linnuke avalike süütegude kasti juurde, siis kantakse graafikule avalikes kohtades toimepandud süüteod.
    if("avalik" %in% input$checkGroup){
      
      if (!exists("avalik_1")){
        createAlert(session, "andmete_laadimine", "avalik_laadimine", title = "Andmete laadimine",
                    content = "Avalike rikkumiste andmestiku laadimine....", append = FALSE,
                    dismiss = FALSE, style = "info")
        avalik_1 <- read_delim("https://opendata.smit.ee/ppa/csv/avalik_1.csv", 
                               delim = "\t", escape_double = FALSE, trim_ws = TRUE,
                               col_types = "-c--c---c------cc-")
        avalik_1$ToimKpv = as.Date(avalik_1$ToimKpv)
        
        avalik_1<-add_coords(avalik_1)
        
        #Tekitan uue tunnuse "Informatsioon", kus on sõne, mida kuvatakse, kui kaardile olevale ringile vajutatakse.
        #Tunnus SyndmusLiik -> SyndmusLiik_ilus
        avalik_1<-mutate(avalik_1, 
                         SyndmusLiik_ilus=ifelse(is.na(SyndmusLiik),"Pole määratud",capitalize(tolower(SyndmusLiik))))
        
        avalik_1$SyndmusLiik_ilus<-gsub("_", " ", avalik_1$SyndmusLiik_ilus)
        avalik_1$SyndmusLiik_ilus<-gsub("oovimine", "öövimine", avalik_1$SyndmusLiik_ilus)#röövimine
        avalik_1$SyndmusLiik_ilus<-gsub("vaarkohtlemine", "väärkohtlemine", avalik_1$SyndmusLiik_ilus)
        avalik_1$SyndmusLiik_ilus<-gsub("liiklusonnetus", "liiklusõnnetus", avalik_1$SyndmusLiik_ilus)
        avalik_1$SyndmusLiik_ilus<-gsub("soiduk", "sõiduk", avalik_1$SyndmusLiik_ilus)
        avalik_1$SyndmusLiik_ilus<-gsub("muuk", "müük", avalik_1$SyndmusLiik_ilus)
        avalik_1$SyndmusLiik_ilus<-gsub("ahvardus", "ähvardus", avalik_1$SyndmusLiik_ilus)
        avalik_1$SyndmusLiik_ilus<-gsub("koolivagivald", "koolivägivald", avalik_1$SyndmusLiik_ilus)
        avalik_1$SyndmusLiik_ilus<-gsub("oig rikk", "õigusrikkumine", avalik_1$SyndmusLiik_ilus)
        avalik_1$SyndmusLiik_ilus<-gsub("lohkekeha", "lõhkekeha", avalik_1$SyndmusLiik_ilus)
        
        avalik_1<-mutate(avalik_1, 
                         Informatsioon=paste(SyndmusLiik_ilus, format(ToimKpv,"%d.%m.%Y"), sep=", "))
        
        avalik_1 <<- avalik_1
        closeAlert(session, "avalik_laadimine")
        
        updateCheckboxGroupInput(session, "checkGroup_avalik", label ="Avaliku koha süüteo paragrahv", 
                                 choices =unique(avalik_1$ParagrahvTais),
                                 selected=unique(avalik_1$ParagrahvTais))
      }  
      
      alamandmestik <- filter(avalik_1, avalik_1$ToimKpv>=input$dateRange[1], avalik_1$ToimKpv<=input$dateRange[2], 
                              avalik_1$ParagrahvTais %in% input$checkGroup_avalik )
      map<-map %>%
        addAwesomeMarkers(lng = alamandmestik$lon1, lat = alamandmestik$lat1, icon = awesomeIcons(icon = 'fa-users', library = 'fa', markerColor = 'green'),  
                          popup=alamandmestik$Informatsioon)
      #addCircleMarkers(lng = alamandmestik$lon1,lat = alamandmestik$lat1,color = 'yellow', radius=8, weight = 1,  popup=alamandmestik$Informatsioon)
    }
    #Kui on tehtud linnuke vara kasti juurde, siis kantakse graafikule varaga seotud süüteod.
    if("vara" %in% input$checkGroup){
      if (!exists("vara_1")){
        createAlert(session, "andmete_laadimine", "vara_laadimine", title = "Andmete laadimine",
                    content = "Varavastaste rikkumiste andmestiku laadimine....", append = FALSE,
                    dismiss = FALSE, style = "info")
        vara_1 = read_delim("https://opendata.smit.ee/ppa/csv/vara_1.csv", 
                            delim = "\t", escape_double = FALSE, trim_ws = TRUE,
                            col_types = "-c----c-----c--------cc----")
        vara_1$ToimKpv = as.Date(vara_1$ToimKpv)
        vara_1<-add_coords(vara_1)
        
        #SyndmusLiik->SyndmusLiik_ilus
        vara_1<-mutate(vara_1, 
                       SyndmusLiik_ilus=ifelse(is.na(SyndmusLiik),"Pole määratud",capitalize(tolower(SyndmusLiik))))
        
        vara_1$SyndmusLiik_ilus<-gsub("_", " ", vara_1$SyndmusLiik_ilus)
        vara_1$SyndmusLiik_ilus<-gsub("oovimine", "öövimine", vara_1$SyndmusLiik_ilus)#röövimine
        vara_1$SyndmusLiik_ilus<-gsub("vaarkohtlemine", "väärkohtlemine", vara_1$SyndmusLiik_ilus)
        vara_1$SyndmusLiik_ilus<-gsub("onnetus", "õnnetus", vara_1$SyndmusLiik_ilus)
        vara_1$SyndmusLiik_ilus<-gsub("soiduk", "sõiduk", vara_1$SyndmusLiik_ilus)
        vara_1$SyndmusLiik_ilus<-gsub("muuk", "müük", vara_1$SyndmusLiik_ilus)
        vara_1$SyndmusLiik_ilus<-gsub("ahvardus", "ähvardus", vara_1$SyndmusLiik_ilus)
        vara_1$SyndmusLiik_ilus<-gsub("koolivagivald", "koolivägivald", vara_1$SyndmusLiik_ilus)
        vara_1$SyndmusLiik_ilus<-gsub("oig rikk", "õigusrikkumine", vara_1$SyndmusLiik_ilus)
        vara_1$SyndmusLiik_ilus<-gsub("lohkekeha", "lõhkekeha", vara_1$SyndmusLiik_ilus)
        vara_1<-mutate(vara_1, 
                       Informatsioon=paste(SyndmusLiik_ilus, format(ToimKpv,"%d.%m.%Y"), sep=", "))
        
        vara_1 <<- vara_1
        closeAlert(session, "vara_laadimine")
        
        updateCheckboxGroupInput(session, "checkGroup_vara", label ="Varavastase kuriteo paragrahv",
                                 choices =sort(unique(vara_1$ParagrahvTais)),
                                 selected=unique(vara_1$ParagrahvTais))
      }
      
      alamandmestik<-filter(vara_1, vara_1$ToimKpv>=input$dateRange[1], vara_1$ToimKpv<=input$dateRange[2],
                            vara_1$ParagrahvTais %in% input$checkGroup_vara)
      map<-map %>%
        addAwesomeMarkers(lng = alamandmestik$lon1, lat = alamandmestik$lat1, icon = awesomeIcons(icon = 'fa-user-secret', library = 'fa', markerColor = 'blue'),  popup=alamandmestik$Informatsioon)
      #addCircleMarkers(lng = alamandmestik$lon1,lat = alamandmestik$lat1,color = 'blue', radius=8, weight = 1,  popup=alamandmestik$Informatsioon)
    }
    
    #Seadistan kaardi, et avanedes paistaks Eesti.
    # map<-map %>% 
    #   setView(lng = 25.031667, lat = 58.648333, zoom = 7) %>% 
    #   addProviderTiles("Esri.WorldImagery") %>%
    #   addProviderTiles(providers$Esri.WorldStreetMap)
    map
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

