###########################################################
#Looja: Meelis Utt
#Viimati muudetud: 11.05.19
###########################################################

########
#Paketid
########

library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)
library(htmltools)
library(readr)

#Leaflet tiles
#http://leaflet-extras.github.io/leaflet-providers/preview/index.html


#################################
#Seame faili asukoha tookaustaks.
#################################

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################################################################################################################3

########################################
#Loeme sisse andmestiku,
#eeldusel, et see on app.R samas kaustas
########################################
andmestik_terrorism <- read.csv("globalterrorismdb_0617dist.csv",header=TRUE,stringsAsFactors = F,encoding="UTF-8")%>%
  dplyr::select(iyear,imonth,iday,country_txt,latitude,longitude,
         attacktype1_txt,targtype1_txt,weaptype1_txt,weapdetail,addnotes,propextent_txt,
         nkill,nwound,propcomment,scite1,scite2,scite3,gname,motive) %>%
  filter(!is.na(longitude) & !is.na(latitude))
  

#############################################################################################
#Funktsiooni alamtabeli leidmiseks, mille infot kaardile kanda.
#andmestik - Andmestik, millest infot võtma hakkame.
#riik - Mis riigi kohta andmeid tahame.
#algus - Aasta, millest alates infot vaatame.
#lopp - Aasta, milleni infot vaatame.
#mitmes_rida_algus - Alates mitmendast reast vaatlusi kaardile kuvame (info alamandmestiust).
#mitmes_rida_lopp - Mitmenda reani infot kaardile kuvame (info alamandmestiust).
#############################################################################################

rynnakud_fun <- function(andmestik,riik,algus,lopp,mitmes_rida_algus,mitmes_rida_lopp){#
  return(andmestik %>%
           filter(iyear>=algus & iyear<lopp & country_txt == riik) %>%
           slice(mitmes_rida_algus:(mitmes_rida_lopp)))
}

#####################################################################################################################

#Kirjutame css, mille abil laadimisekraani ja tühje vaatlusi kuvada.

laadimise_ekraani_css <- "
#ootamine {
position: absolute;
background: #000000;
opacity: 1;
left: 0;
right: 0;
height: 700px;
white-space: pre;
text-align: center;
color: #FFFFFF;
}
"

alustamise_ekraani_css <- "
#alustamine {
position: absolute;
background: #000000;
opacity: 1;
left: 0;
right: 0;
height: 700px;
white-space: pre;
text-align: center;
color: #FFFFFF;
}
"

tyhi_vaatluse_css <- "
#tyhi_vaatlus {
position: absolute;
background: #FFFFFF;
opacity: 0.6;
left: 0;
right: 0;
height: 15px;
text-align: center;
color: #000000;
}
"
############
#    UI    #
############

ui <- fluidPage(
  #Kasutame shinyjs ja laeme defineeritud css-d ui-sse
  useShinyjs(),
  inlineCSS(laadimise_ekraani_css),
  inlineCSS(tyhi_vaatluse_css),
  inlineCSS(alustamise_ekraani_css),
  #Kuvame laadimisekraani ja peidame kaardi jt ekraanid.
  div(
    id = "ootamine",
    h1("Laen kaarti..."),
    p("Väike tutvustus. Kaardil on kujutatud aastatel 1970-2016 aset leidnud terrorünnakute asukohta ning 
        nendega seonduvat lisainfot. Kuna andmestik on suur, siis tuleb valida millise
        riigi, aastate vahemiku ning mitmendaid rünnakuid kuvatakse."),
    p("Andmed on saadud leheküljelt: https://www.kaggle.com/ecodan/global-terrorism-db"),
    p("HOIATUS: Teatud juhtudel jookseb programm kokku.
      Nt Afganisthan [2010,2015) 0-500, kuna programmi jaoks on kuvatavat infot liiga palju.")
  ),
  hidden(
    div(id= "alustamine",
        h1("Vajuta nupule, et kaarti näha."),
        p("Väike tutvustus. Kaardil on kujutatud aastatel 1970-2016 aset leidnud terrorünnakute asukohta ning 
        nendega seonduvat lisainfot. Kuna andmestik on suur, siis tuleb valida millise
          riigi, aastate vahemiku ning mitmendaid rünnakuid kuvatakse."),
        p("Andmed on saadud leheküljelt: https://www.kaggle.com/ecodan/global-terrorism-db"),
        p("HOIATUS: Teatud juhtudel jookseb programm kokku.
          Nt Afganisthan [2010,2015) 0-500, kuna programmi jaoks on kuvatavat infot liiga palju."),
        actionButton(inputId = "alustamine_nupp", label =  "VAJUTA SIIA")
    )
  ),
  hidden(
    div(id= "kaart",
      leafletOutput("map",height="700px"),
      absolutePanel(top = 10, right = 20,
                    selectInput(inputId = "riik",label="Vali riik:",
                                choices = sort(andmestik_terrorism$country_txt),width=150),
                    selectInput("aasta", "Vali ajavahemik:",
                                choices=c(paste("[",seq(1970,2010,5),",",seq(1975,2015,5),")",sep=""),"[2015,2017)"),
                                selected = "[2015,2017)",width=125),
                    sliderInput("mitu", "Mitmendad rünnakud kuvatakse:",min=0,max=11000,
                                value = c(0,500), step=250,width=175)
          )
    )
  ),
  hidden(
    div(id= "tyhi_vaatlus",
        h2("Valitud parameetrite korral puuduvad vaatlused.")
    )
  )
)


################
#    SERVER    #
################

server <- function(input, output,session) {
  #Loome kaardi põhja.
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addProviderTiles("Esri.WorldStreetMap",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng=0,lat=40,zoom=2)
  })
  #Peidame ooteekraani ja anname kasutajale võimaluse veel tutvustust lugeda.
  hide(id = "ootamine")
  show(id = "alustamine")
  
  #Vajutades nupule peidame tutvustuse ekraani ning näitame kaarti.
  observeEvent(input$alustamine_nupp, {
    hide(id = "alustamine", anim = TRUE, animType = "fade")
    show("kaart")
  })
  
  #Funktsioon selleks kui kasutaja vahetab valikuid (riiki, aastavahemikku või rünnakute arvu).
  #Siis muutub alamandmestik, millest võetakse kaardile kuvatavate punktide infot.
  punktid <- reactive({
    rynnakud_fun(andmestik = andmestik_terrorism,riik = input$riik,
                 algus = strsplit(input$aasta,",")[[1]][1] %>% parse_number(),
                 lopp = strsplit(input$aasta,",")[[1]][2] %>% parse_number(),
                 mitmes_rida_algus=input$mitu[1],
                 mitmes_rida_lopp= input$mitu[2])
  })
  
  #Kui kasutaja vahetab valikuid (riiki, aastavahemikku või rünnakute arvu).
  #siis jälgitakse muutusi ning kuvatakse uus info kaardile.
  observe({
    #Uue info muutujatesse määramine
    kuupaev <- paste("<b>Date:</b> ",punktid()[,3],"-",punktid()[,2],"-",punktid()[,1],sep="")
    tyyp <- paste("<b>Attack type:</b> ",punktid()[,7], sep="")
    sihtmark <- paste("<b>Target:</b> ",punktid()[,8], sep="")
    relv <- paste("<b>Weapon type:</b> ",punktid()[,9], sep="")
    relva_tapsustus <- paste("<b>Weapon type specification:</b> ",punktid()[,10], sep="")
    surmasid <- paste("<b>Dead:</b> ",ifelse(punktid()[,13]=="" |is.na(punktid()[,13]),"",punktid()[,13]), sep="")
    haavatuid <- paste("<b>Wounded:</b> ",ifelse(punktid()[,14]==""|is.na(punktid()[,14]),"",punktid()[,14]), sep="")
    kahju <- paste("<b>Property damage:</b> ",ifelse(punktid()[,12]=="","",punktid()[,12]), sep="")
    kahju_seletus <-  paste("<b>Property damage description:</b> ",
                            ifelse(punktid()[,15]=="","",punktid()[,15]), sep="")
    markus <- paste("<b>Notes:</b> ",punktid()[,11])
    # viide_1 <- paste("<b>Cite:</b> ",punktid()[,16])
    # viide_2 <- paste("<b>Cite:</b> ",punktid()[,17])
    # viide_3 <- paste("<b>Cite:</b> ",punktid()[,18])
    rundajad <- paste("<b>Responsible for the attack:</b> ",punktid()[,19])
    motiiv <- paste("<b>Motive:</b> ",ifelse(punktid()[,20]=="" | punktid()[,20]=="The specific motive for the attack is unknown.",
                                             "",punktid()[,20]))
      
    
    #Kui on üle 500 vaatluse, siis märkust ei kuvata, kuna muidu jookseb programm kokku.
    if(nrow(punktid()) > 500){
      motiiv <- ""
    }
    #Kui on rohkem kui 0 punkti, siis liigutakse vastava riigi juurde ja kuvatakse uued punktid
    if(nrow(punktid()) != 0){
      hide(id = "ootamine", anim = TRUE, animType = "fade")
      hide(id = "tyhi_vaatlus", anim = TRUE, animType = "fade")
      show("kaart")
      leafletProxy("map", data = punktid())%>%
        clearMarkers() %>%
        addMarkers(popup = paste(kuupaev,rundajad,tyyp,sihtmark,relv,relva_tapsustus,surmasid,haavatuid,
                                 kahju,kahju_seletus,motiiv,sep="<br/>")) %>%
      
        flyTo(lng=mean(punktid()[,6]),
              lat=mean(punktid()[,5]),
              zoom=4)
      
    }
    #Kui on 0 punkti, siis kuvatakse, et vaadeldavatel väärtustel vaatlused puuduvad
    else{
      show(id = "tyhi_vaatlus", anim = TRUE, animType = "fade")
      leafletProxy("map", data = punktid())%>%
        clearMarkers() %>%
        flyTo(lng=0,
              lat=40,
              zoom=2)
    }
  })
  #Kui rakendus kinni pannakse, siis programm lõpetab töö
  session$onSessionEnded(function() {
    stopApp()
  })
}


#Käivitame rakenduse.
shinyApp(ui=ui,server=server)
