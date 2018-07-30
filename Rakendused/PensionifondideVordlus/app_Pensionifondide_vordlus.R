## PROJEKT - Pensionifondide võrdlus

library(shinydashboard)

library("foreign")  
library("xml2")
library("XML")
library("zoo")
library("rvest")
library("ggplot2")
library("plotly")
library("dplyr")
library("tidyr")
library("reshape2")
library("eurostat")
library("lubridate")


faili_tootlus=function(andmed){
  data <- andmed
  algus_kp = format('2003-01-01')
  lopp_kp = Sys.Date()
  
  ## Isiklikud andmed (XML fail eesti.ee lehelt)
  
  #### OSAKUD ja nende ostmine on esimene tabeli osa, 
  pf_nimetus<-xpathSApply(data,'//OSAKUD//PF_NIMETUS',xmlValue)
  kuupaev<-xpathSApply(data,'//OSAKUD//KUUPAEV',xmlValue)
  hind<-xpathSApply(data,'//OSAKUD//HIND',xmlValue)
  valuuta_kood<-xpathSApply(data,'//OSAKUD//VALUUTA_KOOD',xmlValue)
  kogus<-xpathSApply(data,'//OSAKUD//KOGUS',xmlValue)
  puhasvaartus<-xpathSApply(data,'//OSAKUD//PUHASVAARTUS',xmlValue)
  selgitus<-xpathSApply(data,'//OSAKUD//SELGITUS',xmlValue)
  
  dfosakud <- data.frame(fond=pf_nimetus, kp=as.Date(kuupaev, "%Y%m%d"), hind=as.numeric(hind),
                         ostusumma=as.numeric(hind)*as.numeric(kogus), puhasvaartus=as.numeric(puhasvaartus), 
                         osakud=as.numeric(kogus), valuuta_kood,selgitus)
  
  #### agregeerime üle päevade - andmetes olid juhud, et ühel kuupäeval oli mitu ostmist
  dfosakud<-data.frame(dfosakud %>%
                         group_by(kp, fond,selgitus)  %>%
                         dplyr::summarize(ostusumma=sum(ostusumma), osakud=sum(osakud), puhasvaartus=mean(puhasvaartus)) %>%
                         select(kp, fond, ostusumma, osakud, puhasvaartus, selgitus))
  
  #### väiketähtedega fondi nimetus kahe faili sidumiseks. Tekkis olukord, et Swedbanki üks fond oli eri viisil kirjas kahes failis
  dfosakud$fond.lower<-tolower(dfosakud$fond)
  
  #### summad eurodesse (kuni 2011 olid EEKides)
  dfosakud$ostusumma_eur<-dfosakud$ostusumma
  dfosakud$ostusumma_eur[dfosakud$kp<as.Date("1/1/2011", "%d/%m/%Y")]<-dfosakud$ostusumma[dfosakud$kp<as.Date("1/1/2011", "%d/%m/%Y")]/15.6466
  return(dfosakud)
}

df_ostetudosakud=function(dfosakud){
  #### ostetud osakud eraldi tabelisse
  dfostetudosakud=filter(dfosakud,selgitus=="")
  return(dfostetudosakud)
}

pensionikeskuse_andmed=function(){

  fondide_nr=c(78,73,47,39,46,38,59,48,57,49,50,56,75,60,51,61,58,36,37,52,74,77,76)  # Fondide nimekiri

  algus_kp = format('2003-01-01')
  lopp_kp = Sys.Date()
  
  #### failide allalaadimine, data.frame-de list
  ### Varu natukene kannatust...
  
  fondide_nav<- lapply(fondide_nr, function(x) {
    url=paste0("http://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/?date_from=",algus_kp,"&date_to=",lopp_kp,"&f%5B%5D=", x, "&download=xls")
    print(paste0("Fondi ",x," laadimine..."))
    read.csv(url,sep="\t", fileEncoding="UTF-16LE",dec=",")})
  return(fondide_nav)
}

pensionikeskuse_andmed_kp=function(fondid, alguskp, loppkp){
    algus_kp = format(alguskp)
    lopp_kp = format(loppkp)
  
  fondide_nav<- lapply(fondid, function(x) {
    url=paste0("http://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/?date_from=",algus_kp,"&date_to=",lopp_kp,"&f%5B%5D=", x, "&download=xls")
    print(paste0("Fondi ",x," laadimine..."))
    read.csv(url,sep="\t", fileEncoding="UTF-16LE",dec=",")})
  return(fondide_nav)
}

valjalaske_tasu=function(){
  ## Fondide väljalasketasud (algusaastatel; vaja, kui soovida simuleerida hüpoteetilisi oste)
  valjalasketasu=data.frame(year=2002:2009)
  valjalasketasu$LLK50=1
  valjalasketasu$LXK75=2
  valjalasketasu$SPK50=1
  valjalasketasu$SPK25=1
  valjalasketasu$LMK25=2
  valjalasketasu$SWK50=1.5
  valjalasketasu$LSK00=1
  valjalasketasu$SEK50=c(1.5,1.5,1.5,1.5,1.5,1,1,0)
  valjalasketasu$SWK25=1.5
  valjalasketasu$NPK50=c(rep(3,7),0)
  valjalasketasu$SEK00=c(1.2,1.2,1.2,1.2,1.2,1,1,0)
  valjalasketasu$LXK00=1
  valjalasketasu$SPK00=1
  valjalasketasu$SWK00=1.5
  valjalasketasu$NPK00=c(rep(3,7),0)
  
  #### teeme pikaks, et oleks parem siduda fondide andmetega
  
  valjalasketasu.long=melt(valjalasketasu,id.vars="year",variable.name = "symbol",value.name = "valjatasu")
  valjalasketasu.long$year=as.numeric(valjalasketasu.long$year)
  valjalasketasu.long=dplyr::arrange(valjalasketasu.long,symbol,year)
  return(valjalasketasu.long)
}

###############################################################################################################
# Vajaminevad funktsioonid (tootluse arvutamiseks)

fondide.IRR.fn=function(nav){
  fondide_irr=unique(select(nav,fond,symbol))
  fondide_irr$IRR=(sapply(fondide_irr$symbol,function(x) IRR.fn(filter(nav,symbol==x))))*100
  return(arrange(fondide_irr,desc(IRR)))
}

IRR.fn=function(fond){
  kanded=filter(fond,!is.na(ostusumma_eur))
  start_date=min(kanded$kp)
  calc_date=max(fond$kp)
  calc_value=fond$vaartus[fond$kp==calc_date]
  diff_days=as.numeric(calc_date-start_date)
  kanded$t=as.numeric(calc_date-kanded$kp)/365.25
  res=uniroot(function(x) (sum(kanded$ostusumma_eur*(1+x)**kanded$t)-calc_value),interval=c(-1,100), tol = 1e-8)$root
  return(round(res,4))
}

andmete_tootlus=function(dfosakud, dfostetudosakud, fondide_nav, valjalasketasu.long){
  colnames=c("kp","fond","symbol","isin","nav","muutus")
  #### kuupäevade vektor alates esimeses ostetud osaku kuupäevast -1 kuni tänaseni
  kp_vec=data.frame(kp=seq((min(dfostetudosakud$kp)-1),Sys.Date(),by="1 day"))
  
  #### fondide tunnustele nimed juurde
  fondide_nav=lapply(fondide_nav, setNames, colnames)
  
  #### andmete ühendamine
  fondide_nav=lapply(fondide_nav, function (x) {x$kp=as.Date(x$kp,format="%d.%m.%Y")
  x=merge(x,kp_vec,by="kp",all=T)
  x=tidyr::fill(x,c(fond,symbol,nav),.direction="down")
  x$fond.lower<-tolower(as.character(x$fond))
  x=x[x$kp>=(min(dfostetudosakud$kp)-1),c("kp","fond", "fond.lower","symbol","nav")]
  x$year<-as.numeric(format(x$kp,'%Y'))
  x=merge(x,select(valjalasketasu.long,symbol,year, valjatasu),by=c("symbol", "year"), all.x=TRUE)
  x$valjatasu[is.na(x$valjatasu)]=0
  dplyr::arrange(x,kp)})
  
  #### Sissemaksed kumulatiivselt
  sissemaksed<-data.frame(kp=seq((min(dfostetudosakud$kp)-1),Sys.Date(),by="1 day"))
  sissemaksed$year<-as.numeric(format(sissemaksed$kp,'%Y'))
  sissemaksed$fond<-as.factor("Sissemaksed")
  sissemaksed$fond.lower<-"sissemaksed"
  sissemaksed$symbol<-as.factor("Raha")
  sissemaksed$nav<-1
  sissemaksed$valjatasu<-0
  fondide_nav[[length(fondide_nav)+1]]=sissemaksed
  
  #### Hüpoteetiline väärtus fondis päevaselt
  fondide_nav=lapply(fondide_nav, function (x) {x=merge(x,select(dfostetudosakud,kp,ostusumma_eur),by="kp",all=T);
  x$nav_eile=c(NA,x$nav[-length(x$nav)])
  x$osakud=x$ostusumma_eur*(1-x$valjatasu/100)/x$nav_eile
  x$osakud[is.na(x$osakud)]=0
  x$cumosakud=cumsum(x$osakud)
  x$vaartus=x$nav*x$cumosakud
  filter(x,!is.na(vaartus))})
  
  nav <- do.call("rbind", fondide_nav)
  nav$fond=as.character(nav$fond)  
  
  
  ## Tegelik seis
  #### Personaalne konto; arvesse on võetud fondide vahetus
  
  tegelik=select(nav,kp,symbol,year,fond,fond.lower,nav,valjatasu)
  ####jätame alles need fondid, kus on ka tegelikud ostud; moodustame neist listi
  tegelik=lapply(1:length(unique(dfosakud$fond.lower)), function(x) filter(tegelik,fond.lower==unique(dfosakud$fond.lower)[x]))
  #### seome juurde osakud
  tegelik=lapply(tegelik, function (x) {x=merge(x,select(dfosakud,kp,fond,fond.lower,osakud),by=c("kp","fond.lower"),all.x=T)
  x$osakud[is.na(x$osakud)]=0
  x$cumosakud=round(cumsum(x$osakud),5)
  x$vaartus=x$nav*x$cumosakud
  filter(x,!is.na(vaartus))})
  
  tegelik <- do.call("rbind", tegelik)
  tegelik=dplyr::summarise(group_by(tegelik,kp),vaartus=sum(vaartus))
  tegelik$fond="Tegelik portfell"
  tegelik$symbol="Tegelik"
  tegelik=merge(tegelik,select(dfostetudosakud,kp,ostusumma_eur),by="kp",all=T)
  
  
  ## Inlatsiooniga rahavoo korrigeerimine
  
  dfrahavood=filter(dfosakud,selgitus=="")
  
  # cpi eurostatist
  k <- get_eurostat("prc_hicp_midx", filters = list(geo = "EE", unit = "I15", coicop = "CP00"))
  cpi<-data.frame(kp=k$time, cpi=k$values)
  
  cpi$infl=c(NA,cpi$cpi[-1]/cpi$cpi[-nrow(cpi)])
  kp_vec=data.frame(kp=seq((min(dfrahavood$kp)-1),Sys.Date(),by="1 day"))
  dfrahavood=merge(select(dfrahavood,kp,ostusumma_eur),kp_vec,by="kp",all=T)
  dfrahavood$ostusumma_eur[is.na(dfrahavood$ostusumma_eur)]=0
  dfrahavood$AastaKuu=as.Date(paste0(substr(dfrahavood$kp,start=1,stop=7),"-01"))
  dfrahavood=merge(dfrahavood,filter(cpi,kp>=min(dfrahavood$kp)),by.x="AastaKuu",by.y="kp",all=T)
  month(dfrahavood$AastaKuu)=month(dfrahavood$AastaKuu)+1
  day(dfrahavood$AastaKuu)=day(dfrahavood$AastaKuu)-1
  dfrahavood$infl[is.na(dfrahavood$infl)]=1
  dfrahavood=arrange(dfrahavood,kp)
  dfrahavood$vaartus=dfrahavood$ostusumma_eur
  
  for(i in 2:nrow(dfrahavood)){
    dfrahavood$vaartus[i]=dfrahavood$vaartus[i-1]*dfrahavood$infl[i]**(1/day(dfrahavood$AastaKuu[i]))+dfrahavood$ostusumma_eur[i]
  }
  
  dfrahavood$fond="Inflatsiooniga korrigeeritud sissemaksed"
  dfrahavood$symbol="Inflatsioon"

  ## Lõplik tulemus
  
  koondtabel=rbind(select(nav,kp,symbol,fond,vaartus,ostusumma_eur),
                   tegelik,
                   select(dfrahavood,kp,symbol,fond,vaartus,ostusumma_eur))
  
  koondtabel$fond=factor(koondtabel$fond,levels=c("LHV Pensionifond Eesti",
                                                  "LHV Pensionifond Indeks",
                                                  "LHV Pensionifond L",
                                                  "LHV Pensionifond M",
                                                  "LHV Pensionifond S",
                                                  "LHV Pensionifond XL",
                                                  "LHV Pensionifond XS",
                                                  "Luminor A Pensionifond",
                                                  "Luminor A Pluss Pensionifond",
                                                  "Luminor B Pensionifond",
                                                  "Luminor C Pensionifond",
                                                  "SEB Energiline Pensionifond",
                                                  "SEB Energiline Pensionifond Indeks",
                                                  "SEB Konservatiivne Pensionifond",
                                                  "SEB Optimaalne Pensionifond",
                                                  "SEB Progressiivne Pensionifond",
                                                  "Swedbank Pensionifond K1 (Konservatiivne strateegia)",
                                                  "Swedbank Pensionifond K2 (Tasakaalustatud strateegia)",
                                                  "Swedbank Pensionifond K3 (Kasvustrateegia)",
                                                  "Swedbank Pensionifond K4 (Aktsiastrateegia)",
                                                  "Swedbank Pensionifond K90-99 (Elutsükli strateegia)",
                                                  "Tuleva Maailma Aktsiate Pensionifond",
                                                  "Tuleva Maailma Võlakirjade Pensionifond",
                                                  "Tegelik portfell",
                                                  "Sissemaksed",
                                                  "Inflatsiooniga korrigeeritud sissemaksed"))
  return(koondtabel)
}

fondide_alguskp=function(koondtabel, dfosakud){
  fondide_algus_kp=dplyr::summarise(group_by(koondtabel,fond),algus_kp=min(kp))
  fondide_algus_kp=filter(fondide_algus_kp,algus_kp<=min(dfosakud$kp))
  return(fondide_algus_kp)
}


#Kuna allalaetud NAV-tabelid miskipärast sisaldavad muutuse väärtust NA, kui NAv võrreldes eelneva päevaga ei muutunud, siis vajame lisafunktsiooni 
# asendamaks NA väärtuse viimase olemasolnud väärtusega.
repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}                               # they need to be repeated


uld_tootlus=function(fondide_nav, alguskp, loppkp){
  
  colnames=c("kp","fond","symbol","isin","nav","muutus")
  
  #### kuupäevade vektor alates esimeses ostetud osaku kuupäevast -1 kuni tänaseni
  kp_vec=data.frame(kp=seq(as.Date(alguskp, "%d/%m/%Y")-1,as.Date(loppkp,"%d/%m/%Y"),by="day"))
  
  #### fondide tunnustele nimed juurde
  fondide_nav=lapply(fondide_nav, setNames, colnames)
  
  fondide_nav=lapply(fondide_nav, function (x) {x$kp=as.Date(x$kp,format="%d.%m.%Y")
  x=merge(x,kp_vec,by="kp",all=T)
  x=tidyr::fill(x,c(fond,symbol,nav),.direction="down")
  x$fond.lower<-tolower(as.character(x$fond))
  x$year<-as.numeric(format(x$kp,'%Y'))
  dplyr::arrange(x,kp)})
  
  koondtabel=do.call("rbind", fondide_nav)
  koondtabel=koondtabel %>% drop_na(fond) # kuupäevad, mil fond ei olnud veel aktiivne
  koondtabel$muutus=repeat.before(koondtabel$muutus) #andmestikus juhul, kui NAV ei muutunud, faktorlevel NA - muudame eelnevaks olemas olnud väärtuseks
  koondtabel$muutus=round(as.numeric(levels(koondtabel$muutus))[koondtabel$muutus],2) #muudame faktortunnuse numbriliseks
  koondtabel$muutus=koondtabel$muutus+100
  return(koondtabel)
}


# RAKENDUSE VAADE

sidebar=dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Pensionifondide isiklik võrdlus", tabName = "isiklik", icon = icon("bar-chart")),
    menuItem("Pensionifondide üldine võrdlus", tabName = "uldine", icon = icon("bar-chart")),
    menuItem("Info", tabName = "info", icon = icon("info"))
  ))

body=dashboardBody(
  tabItems(
    tabItem(tabName="info",
            h3("Tere tulemast!"),
            br(),
            p("Antud rakendus on välja töötatud Tartu Ülikooli aine \"Statistiline andmeteadus ja visualiseerimine\" raames."),
            br(),
            p("Pensionifondide võrdluseks loodud rakenduse idee autoriteks on Andres Võrk, Taavi Unt ja Märten Veskimäe, 
              kes on sel teemal kirjutanud ka artikli."),
            uiOutput("tab"),
            p("Antud rakenduse loomisel on kasutatud nende poolt väljatöötatud R-i programmikoodi 
              ning modifitseeritud vastavalt vajadusele."),
            p("Vahelehel Pensionifondi isiklik võrdlus on võimalik võrrelda erinevate pensionifondide tulemusi 
              vastavalt kasutaja ajaloolistele sissemaksetele. Pensionikoguja jaoks on oluline näha, kuidas tema
              poolt sissemakstud raha ajas kasvab, võttes arvesse summasid ning maksete ajahetki, mitte
              niivõrd fondide NAV, mida tavapäraselt kuvatakse. "),
            p("Teisel vahelehel on võimalus valida huvipakkuvad pensionifondid ning kuupäevavahemik ning seejärel
              näha fondide muutust ajas ehk NAV graafikut."),
            br(),
            p("Rakenduse autor Kätrin Suvi")
    ),
    
    tabItem(tabName="isiklik",
            h3("Pensionifondide võrdlus isiklikel andmeil"),
            br(),
            p("Siin lehel on võimalus üles laadida väljavõte enda ajaloolistest sissemaksetest kohustuslikku pensioniskeemi. 
              Isikliku väljavõtte saab kergesti kätte Riigiportaali kaudu, sisenedes eesti.ee keskkonda ning 
              sealt huvipakkuva perioodi  kohta XML-formaadis faili alla laadides."),
            uiOutput("eesti.ee"),
            br(),
            p("Palun varuda veidikene aega! Rakendusel läheb pensionikeskuse leheküljelt kõikide fondide andmete laadimiseks
              omajagu aega. Aitäh!"),
            fluidRow(
              box(title="Vali kuvatavad pensionifondid:", collapsible=TRUE, status="warning", solidHeader = T,
                  checkboxGroupInput("fondid","",
                                     choices=c("LHV Pensionifond Eesti",
                                       "LHV Pensionifond Indeks",
                                       "LHV Pensionifond L",
                                       "LHV Pensionifond M",
                                       "LHV Pensionifond S",
                                       "LHV Pensionifond XL",
                                       "LHV Pensionifond XS",
                                       "Luminor A Pensionifond",
                                       "Luminor A Pluss Pensionifond",
                                       "Luminor B Pensionifond",
                                       "Luminor C Pensionifond",
                                       "SEB Energiline Pensionifond",
                                       "SEB Energiline Pensionifond Indeks",
                                       "SEB Konservatiivne Pensionifond",
                                       "SEB Optimaalne Pensionifond",
                                       "SEB Progressiivne Pensionifond",
                                       "Swedbank Pensionifond K1 (Konservatiivne strateegia)",
                                       "Swedbank Pensionifond K2 (Tasakaalustatud strateegia)",
                                       "Swedbank Pensionifond K3 (Kasvustrateegia)",
                                       "Swedbank Pensionifond K4 (Aktsiastrateegia)",
                                       "Swedbank Pensionifond K90-99 (Elutsükli strateegia)",
                                       "Tuleva Maailma Aktsiate Pensionifond",
                                       "Tuleva Maailma Võlakirjade Pensionifond",
                                       "Tegelik portfell",
                                       "Sissemaksed",
                                       "Inflatsiooniga korrigeeritud sissemaksed"
                                     ),
                                     selected=c("LHV Pensionifond XL","SEB Energiline Pensionifond","Luminor A Pluss Pensionifond", "Tegelik portfell","Sissemaksed",
                                                "Inflatsiooniga korrigeeritud sissemaksed")
                  )
              ),
              box(title="Kasutatavad andmed", p="Andmed failist",
                  fileInput("fail1", "Lae üles enda XML-formaadis fail:",  placeholder = "Andmeid pole sisestatud", accept=".xml")
              )
            ),
            fluidRow(
              box(width=12, title="Pensionivara tegelik ja hüpoteetiline väärtus ning tootlus", 
                  p("Sel interaktiivsel graafiku on võimalus näha sissemakstud raha ajas kasvamist ning võrdlust üleval valikmenüüs linnukesega märgitud
                    pensionifondidega, samuti inflatsiooniga korrigeeritud sissemakseid."),
                  plotlyOutput("graafik_isik"), status="warning", solidHeader = T)),
            fluidRow(
              box(width = 12,title="Tootlused - IRR",
                  p("Järgnevas tabelis on välja arvutatud Teie sissemaksete tootlus, mida on võimalik võrrelda inflatsiooniga ning võimalike 
                    tootlustega teistes pensionifondides. Tootlus  on leitud kasutades TÜ CITISe meeskonna väljatöötatud 
                    kalkulaatorit. "),
                  br(),
                  p("IRR näitab protsendiliselt, kui suur on olnud sissemaksete tootlus olenevalt sellest, kuhu on investeeritud.
                    Näiteks kui IRR näitaja väärtuseks on 2.86, siis tähendab see, et sissemaksete tootlus on olnud 2.86%."),
                  p("Sisaldab vaid fonde, mis eksisteerisid juba failis sisalduva esimese sissemakse kuupäeval."),
                  dataTableOutput("IRR_isik"), status="warning", solidHeader = T))
    ),#tabitem
    
    tabItem(tabName="uldine",
            h3("Pensionifondide üldine võrdlus"),
            br(),
            br(),
            p("Siin lehel võimalik jälgida huvipakkuvate fondide osakute puhasväärtuste muutusi valitud ajaperioodil."),
            br(),
            p("Osaku puhasväärtus ehk NAV (inglise keeles Net Asset Value) on võrdne fondi varade väärtusega, 
              millest on lahutatud fondi kohustused. Selle areng iseloomustab fondijuhi töö tulemuslikkust.
              NAV on fondidde võrdlemiseks laialt kasutatav väärtus."),
            fluidRow(
              box(title="Vali kuvatavad pensionifondid:", collapsible=TRUE, status="warning", solidHeader = T,
                  checkboxGroupInput("fondid2", "",
                                     c("LHV Pensionifond Eesti"=78,
                                       "LHV Pensionifond Indeks"=73,
                                       "LHV Pensionifond L"=47,
                                       "LHV Pensionifond M"=39,
                                       "LHV Pensionifond S"=46,
                                       "LHV Pensionifond XL"=38,
                                       "LHV Pensionifond XS"=59,
                                       "Luminor A Pensionifond"=48,
                                       "Luminor A Pluss Pensionifond"=57,
                                       "Luminor B Pensionifond"=49,
                                       "Luminor C Pensionifond"=50,
                                       "SEB Energiline Pensionifond"=56,
                                       "SEB Energiline Pensionifond Indeks"=75,
                                       "SEB Konservatiivne Pensionifond"=60,
                                       "SEB Optimaalne Pensionifond"=51,
                                       "SEB Progressiivne Pensionifond"=61,
                                       "Swedbank Pensionifond K1 (Konservatiivne strateegia)"=58,
                                       "Swedbank Pensionifond K2 (Tasakaalustatud strateegia)"=36,
                                       "Swedbank Pensionifond K3 (Kasvustrateegia)"=37,
                                       "Swedbank Pensionifond K4 (Aktsiastrateegia)"=52,
                                       "Swedbank Pensionifond K90-99 (Elutsükli strateegia)"=74,
                                       "Tuleva Maailma Aktsiate Pensionifond"=77,
                                       "Tuleva Maailma Võlakirjade Pensionifond"=76),
                                     selected=c(38, 48, 75, 58)
                  )
              ),
              box(title="Kuupäevad", p("Sisesta huvipakkuvad kuupäevad:"),
                  dateInput("alguskp", label="Alguskuupäev:", value = Sys.Date()-10, format="dd/mm/yyyy"),
                  dateInput("loppkp", label="Lõppkuupäev:", value = Sys.Date()-2, format="dd/mm/yyyy"))
                  ),
            fluidRow(
              box(width = 12,title="Kogumispensioni fondide NAV", 
                  plotlyOutput("graafik_uld"), 
                  status="warning", solidHeader = T)
            )
    )
  ))

ui <- dashboardPage(
  dashboardHeader(title = "Pensionifondid"),
  sidebar,
  body,
  skin="yellow"
)

server <- function(input, output) {
  
  url <- a("Link artiklile.", href="http://citis.ut.ee/research/other/pensionifondide-tootluse-uhest-analuusivoimalusest/")
  output$tab <- renderUI({
    tagList(url)})
  eesti <- a("Link sissemaksete väljavõtte laadimislehele portaalis eesti.ee", 
             href="https://www.eesti.ee/est/teenused/kodanik/toetused_ja_sotsiaalabi_1/pensionikonto_valjavote_1")
  output$eesti.ee <- renderUI({
    tagList(eesti)})
  
  fondide_nav=pensionikeskuse_andmed()
  valjalasketasu.long=valjalaske_tasu()
  
  output$graafik_isik<- renderPlotly({
    if (is.null(input$fail1))
      return(NULL)
    
    data=xmlParse(input$fail1$datapath)
    dfosakud=faili_tootlus(data)
    dfostetudosakud=df_ostetudosakud(dfosakud)
    koondtabel=andmete_tootlus(dfosakud, dfostetudosakud, fondide_nav, valjalasketasu.long)
    vali_fondid=input$fondid
    
    p=ggplot(filter(koondtabel,fond %in% vali_fondid),aes(x=kp,y=vaartus,color=fond))+
      geom_line()+
      scale_x_date("Aeg",breaks = scales::pretty_breaks(n = 10))+
      scale_y_continuous("Väärtus (EUR)",breaks = scales::pretty_breaks(n = 10))
    
    ggplotly(p)
  })
  
  output$graafik_uld<- renderPlotly({
    
    fondide_nav=pensionikeskuse_andmed_kp(input$fondid2, input$alguskp, input$loppkp)
    koondtabel=uld_tootlus(fondide_nav, input$alguskp, input$loppkp)
    p=ggplot(koondtabel,aes(x=kp,y=muutus,color=fond),
      text = paste('<br>Kuupäev: ', as.Date(kp),
                   '<br>Osaku väärtus algväärtusest: ', muutus/100))+
      geom_line()+scale_x_date("Aeg",breaks = scales::pretty_breaks(n = 10))+
      scale_y_continuous("NAV muutus (%)",breaks = scales::pretty_breaks(n = 10), labels = scales::percent)
    
    ggplotly(p,tooltip = c("text"))
  })
  
  output$IRR_isik<- renderDataTable({
    if (is.null(input$fail1))
      return(NULL)
    data=xmlParse(input$fail1$datapath)
    dfosakud=faili_tootlus(data)
    dfostetudosakud=df_ostetudosakud(dfosakud)
    koondtabel=andmete_tootlus(dfosakud, dfostetudosakud, fondide_nav, valjalasketasu.long)
    fondide_algus_kp=fondide_alguskp(koondtabel, dfosakud)
    
    tabel=(fondide.IRR.fn(filter(koondtabel,fond %in% fondide_algus_kp$fond)))
  
    tabel
  })
  }

shinyApp(ui, server)