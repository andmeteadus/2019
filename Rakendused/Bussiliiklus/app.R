
#projekt

library(shiny)
library(shinydashboard)
library(plyr)
library(dplyr)
library(DT)
library(knitr)
library(leaflet)
library(sp)
library(ggplot2)
library(ggmap)
library(maps)
library(geosphere)
library(leaflet)

agency=read.table("data/agency.txt",sep=',',header=TRUE)
calendar=read.table("data/calendar.txt",sep=',',header=TRUE)
calendar_dates=read.table("data/calendar_dates.txt",sep=',',header=TRUE)
fare_attributes=read.table("data/fare_attributes.txt",sep=',',header=TRUE)
fare_rules=read.table("data/fare_rules.txt",sep=',',header=TRUE)
feed_info=read.table("data/feed_info.txt",sep=',',header=TRUE)
routes=read.table("data/routes.txt",sep=',',header=TRUE)
stop_times=read.table("data/stop_times.txt",sep=',',header=TRUE)
shapes=read.table("data/shapes.txt",sep=',',header=TRUE)
stops=read.table("data/stops.txt",sep=',',header=TRUE)
trips=read.table("data/trips.txt",sep=',',header=TRUE)

#täpitähed
stops$stop_name <- gsub('Ć¤', 'ä', stops$stop_name)
stops$stop_name <- gsub('Ćµ', 'õ', stops$stop_name)
stops$stop_name <- gsub('Ć¼', 'ü', stops$stop_name)
stops$stop_name <- gsub('Ć•', 'Õ', stops$stop_name)


calendar_dates$kuup=as.Date(ISOdate(substr(calendar_dates$date,1,4),substr(calendar_dates$date,5,6),substr(calendar_dates$date,7,8)))
stop_times$arrival_time=substr(stop_times$arrival_time,1,5)
stop_times$departure_time=substr(stop_times$departure_time,1,5)
stops$peatus=ifelse(stops$stop_desc=='',stops$stop_name,paste(stops$stop_name,stops$stop_desc,sep='-'))

#funktsioon bussi leidmiseks
number=function(algkoht,loppkoht,kuupaev,kella,liin){
  algus=stops[stops$peatus==algkoht,c(1,3)]
  lopp=stops[stops$peatus==loppkoht,c(1,3)]
  alg_trip=stop_times[stop_times$stop_id %in% algus$stop_id &stop_times$departure_time>= kella,c(1,3,4,5)]
  lopp_trip=stop_times[stop_times$stop_id %in% lopp$stop_id,c(1,4,5)]
  alg_trip=plyr::rename(alg_trip,replace= c("stop_id"="alg_id", "stop_sequence"="alg_jrk"))
  lopp_trip=plyr::rename(lopp_trip, replace=c("stop_id"="lopp_id", "stop_sequence"="lopp_jrk"))
  #yhised
  koos=merge(alg_trip, lopp_trip, by = intersect(names(alg_trip), names(lopp_trip)))
  #vaatame et õigetpidi läheks
  koos1=filter(koos,alg_jrk<lopp_jrk)
  #siit saame mis tripid ja service läheb
  t1=trips[trips$trip_id %in% koos1$trip_id,c(1,2,3,5)]
  #siit vaatame kas ta sõidab sellel päeval
  p=ifelse(as.POSIXlt(as.Date(kuupaev))$wday==0,7,as.POSIXlt(as.Date(kuupaev))$wday)
  paev1=calendar[calendar$service_id %in% t1$service_id,c(1,p+1)]
  paev=paev1[paev1[,2]==1,]
  #siit vaatame kas ta erand pole sellel päeval
  erand=calendar_dates[calendar_dates$service_id %in% t1$service_id & calendar_dates$kuup==kuupaev &calendar_dates$exception_type==1,1]
  #paneme kokku ja võtame aint need serviced, mis siis sõidavad
  kuupa=rbind(erand,paev)
  tr=t1[t1$service_id %in% kuupa$service_id,]
  kellad=merge(koos1[,c(1,2)],tr[,c(1,2,3)], by = intersect(names(koos1), names(tr)))
  #siit peaks saama bussinumbri
  ka=liin[liin$route_id %in% tr$route_id,c(1,3,4)]
  kk=merge(kellad,ka,by="route_id",all.x=TRUE)
  return(kk)
}

#funktsioon linna busside leidmiseks
linna=function(linn){
  r=routes[routes$competent_authority==linn,c(1,3,4,7)]
  t=trips[trips$route_id %in% r$route_id,c(3)] #siit vaja trip_id
  s=stop_times[stop_times$trip_id %in% t,c(2,3,4,5)]
  peatused=stops[stops$stop_id %in% s$stop_id,c(1,3,13)]
  return(peatused)
}

#joonis

joon <- function(algus,l6pp){
  m=leaflet()
  m=addTiles(m)
  alguss=stops[stops$peatus==algus,c(1,13)]
  loppp=stops[stops$peatus==l6pp,c(1,13)]
  alg_trip=stop_times[stop_times$stop_id %in% alguss$stop_id,c(1,4,5)]
  lopp_trip=stop_times[stop_times$stop_id %in% loppp$stop_id,c(1,4,5)]
  alg_trip=plyr::rename(alg_trip,replace= c("stop_id"="alg_id", "stop_sequence"="alg_jrk"))
  lopp_trip=plyr::rename(lopp_trip, replace=c("stop_id"="lopp_id", "stop_sequence"="lopp_jrk"))
  #yhised
  koos=merge(alg_trip, lopp_trip, by = intersect(names(alg_trip), names(lopp_trip)))
  #vaatame et Ãµigetpidi lÃ¤heks
  koos1=filter(koos,alg_jrk<lopp_jrk)
  #siit saame mis tripid ja service lÃ¤heb
  t1=trips[trips$trip_id %in% koos1$trip_id,c(1,2,3,5)]
  teekonnad=unique(t1$route_id)
  teekond=teekonnad[1]
  
  alguselon=stops[stops$peatus==algus,]$stop_lon[1]
  alguselat=stops[stops$peatus==algus,]$stop_lat[1]
  lopulon=stops[stops$peatus==l6pp,]$stop_lon[1]
  lopulat=stops[stops$peatus==l6pp,]$stop_lat[1]
  zuum=abs(alguselat-lopulat)+abs(alguselon-lopulon)
  if (zuum<0.04){
    zuum2=13
  } else if (zuum>2){
    zuum2=7
  } else if (zuum>1.2){
    zuum2=8
  } else if (zuum>0.7){
    zuum2=9
  } else if (zuum>0.3){
    zuum2=10
  } else if (zuum>0.1){
    zuum2=11
  } else {
    zuum2=12
  }
  
  shapeid = trips[trips$route_id==toString(teekond),]$shape_id[1]
  sheipid = shapes[shapes$shape_id==shapeid,]
  vahe1=c()
  vahe2=c()
  for (i in 1:nrow(sheipid)){
    vahe1=c(vahe1,(abs(sheipid$shape_pt_lat[i]-alguselat)+abs(sheipid$shape_pt_lon[i]-alguselon)))
    vahe2=c(vahe2,(abs(sheipid$shape_pt_lat[i]-lopulat)+abs(sheipid$shape_pt_lon[i]-lopulon)))
  }
  shapealgus=which.min(vahe1)
  shapelopp=which.min(vahe2)
  
  if (shapealgus>shapelopp){
    for (i in shapealgus:(shapelopp+1)){
      gc_points = gcIntermediate(c(sheipid$shape_pt_lon[i], sheipid$shape_pt_lat[i]), 
                                 c(sheipid$shape_pt_lon[i-1], sheipid$shape_pt_lat[i-1]), 
                                 n=50, addStartEnd=TRUE)
      gc = data.frame(gc_points)
      m = m %>% #joonte lisamine kaardile
        addPolylines(lng = gc$lon, lat = gc$lat, weight = 2, color = paste("#",toString(routes[routes$route_id==toString(teekond),]$route_color), sep=""))
    }
    
    m = m %>% 
      setView(lng = (sheipid$shape_pt_lon[1]+sheipid$shape_pt_lon[nrow(sheipid)])/2, lat = (sheipid$shape_pt_lat[1]+sheipid$shape_pt_lat[nrow(sheipid)])/2, zoom = zuum2) %>% 
      addProviderTiles("Esri.WorldImagery")
    m
  } else {
    for (i in shapealgus:(shapelopp-1)){
      gc_points = gcIntermediate(c(sheipid$shape_pt_lon[i], sheipid$shape_pt_lat[i]), 
                                 c(sheipid$shape_pt_lon[i+1], sheipid$shape_pt_lat[i+1]), 
                                 n=50, addStartEnd=TRUE)
      gc = data.frame(gc_points)
      m = m %>% #joonte lisamine kaardile
        addPolylines(lng = gc$lon, lat = gc$lat, weight = 2, color = paste("#",toString(routes[routes$route_id==toString(teekond),]$route_color), sep=""))
    }
    
    m = m %>% 
      setView(lng = (sheipid$shape_pt_lon[shapealgus]+sheipid$shape_pt_lon[shapelopp])/2, lat = (sheipid$shape_pt_lat[shapealgus]+sheipid$shape_pt_lat[shapelopp])/2, zoom = zuum2) %>% 
      addProviderTiles("Esri.WorldImagery")
    m
  }
}




#milline agency pakub kõige rohkem reise
rout_ag=merge(agency[,c(1,2)],routes[,c(1,2)],by='agency_id',all.y=TRUE)
ag_tri=merge(rout_ag,trips[,c(1,2,3,8)],by='route_id',all.y=TRUE)
agt=data.frame(ag_tri %>% group_by(agency_name)%>%summarise(kok=n(),rat=sum(wheelchair_accessible)))
#milline firma kõige ratastooli sõbralikum
agt$rat1=ifelse(agt$rat>0,'Jah','Ei')



#milline agency pakub kõige rohkem reise
rout_ag=merge(agency[,c(1,2)],routes[,c(1,2)],by='agency_id',all.y=TRUE)
ag_tri=merge(rout_ag,trips[,c(1,2,3,8)],by='route_id',all.y=TRUE)
agt=data.frame(ag_tri %>% group_by(agency_name)%>%summarise(kok=n(),rat=sum(wheelchair_accessible)))
#milline firma kõige ratastooli sõbralikum
agt$rat1=ifelse(agt$rat>0,'Jah','Ei')


#app

sidebar = dashboardSidebar(
  width=200,
  sidebarMenu(
    menuItem("Esileht", tabName = "Esileht"),
    menuItem("Maaliinid", tabName = "Maaliinid",icon=icon('road')),
    menuItem("Linnaliinid", tabName = "Linnaliinid",icon=icon('bus')),
    menuItem('Statistika', tabName='Statistika',badgeLabel='new',icon=icon("area-chart")),
    br(),br(),br(),
    dateInput("Kuup",label="Vali kuupäev")
    
  )
)


## Body content
body = dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "Esileht",
            fluidRow(
              h1('Tere tulemast!',align='center'),
              h3('See on app, kus kasutaja saab vaadata bussiliiklust.',align='center'),
              br(),
              p('Kui soovid liigelda erinevate linnade vahel, siis vali külgribalt "Maaliinid". Lisaks bussinumbrile ja 
                ja kellaegadele saad vaadata ka oma teekonda kaardil.'),
              p('Linnaliinibusse saab vaadata külgribalt "Linnaliinide" alt.'),
              p('Numbrite huviliste jaoks on "Statistika" ribal välja toodud mõned huvitavamad graafikud.'),
              br(),
              p('Autorid: Birgit Kadastik ja Markus Ellisaar'),
              br(),
              br(),
              img(src='fun.png',height=300,weight=300,
                  style="display: block; margin-left: auto; margin-right: auto;")
            )),
    
    # Second tab content
    tabItem(tabName = "Maaliinid",
            tabsetPanel(
              tabPanel('Buss',br(),
                       box(
                         textInput("alg",label='Sisesta algpeatus'),
                         textInput("siht",label='Sisesta sihtkoht'),
                         textInput('kell',label='Sisesta kellaaeg kujul 00:00')
                       ), 
                       box(
                         dataTableOutput('liinid'))),
              tabPanel('Kaart',icon=icon('map-marker'),
                       leafletOutput('kaart_joonis')
              )
            )),
    #third tab content
    tabItem(tabName = "Linnaliinid",
            tabsetPanel(
              tabPanel('Buss',
                       box(
                         selectInput('linn',label='Vali linn',
                                     choices = sort(unique(routes[routes$competent_authority %in% c('Kohtla-JĆ¤rve LV','Tartu LV','Tallinna TA','PĆ¤rnu LV','Narva LV','SillamĆ¤e LV'),7]),
                                                    selected='Tartu LV')),
                         uiOutput('algpeatus'),
                         uiOutput('loppeatus')
                       ),
                       box(
                         uiOutput('nr'), br(),
                         textOutput('linnliin'))),
              tabPanel('Kaart',icon=icon('map-marker'),
                       leafletOutput('linn_kaart')
              )
              
            )),
    tabItem(tabName='Statistika',
            box(title='Bussifirmade reiside arv',collapsible=TRUE,status='warning',
                solidHeader=TRUE,
                plotOutput('reisid'),
                'Välja on jäetud Tallinna Linnatranspordi AS, mis tegi 20159 reisi kokku ja oli ratastooli sõbralik'),
            box(title='Bussiliinid ja nende sõidupäevad', collapsible=TRUE, status='primary',
                solidHeader = TRUE,
                plotOutput('soidupaev')),
            box(title='Bussifirmade piletihinnad',collapsible = TRUE, status='warning',
                solidHeader = TRUE,
                plotOutput('pilet'))
    )
  ))

ui = dashboardPage(
  skin='green',
  dashboardHeader(title = "Bussiliiklus"),
  sidebar,
  body
)


server <- function(input, output) {
  
  output$liinid <- renderDataTable({
    kk=number(input$alg,input$siht,input$Kuup,input$kell,routes)
    datatable(kk[,c(3,5,6)],colnames = c("Väljumisaeg","Bussi number","Liini nimi"),rownames=FALSE,style = 'bootstrap')
  })
  output$algpeatus <- renderUI({
    selectInput('algpt',label='Vali algpeatus',choices = sort(unique(linna(input$linn)[,3])))
  })
  output$loppeatus<- renderUI({
    #vaja leida peatus kuhu minna saab algpeatusest
    al1=linna(input$linn)
    al=al1[al1$peatus==input$algpt,]
    nim=merge(stop_times,stops[,c(1,13)],by='stop_id',all.x=TRUE)
    ti=nim[nim$stop_id %in% al$stop_id,c(1,2,5)]
    lo=nim[nim$trip_id %in% ti$trip_id,c(1,2,4,5,8)]
    ti=plyr::rename(ti,replace= c("stop_sequence"="alg_seq"))
    loti=merge(lo,ti[,c(2,3)],by='trip_id',all.x=TRUE)
    lot1=filter(loti, stop_sequence>=alg_seq)
    selectInput('lopppt',label='Vali lopppeatus',choices = sort(unique(lot1$peatus)))
  })
  output$nr <- renderUI({
    ke=number(input$algpt,input$lopppt,input$Kuup,"00:00",routes[routes$competent_authority==input$linn,])
    selectInput('bussinr',label='Vali bussinumber',choices = unique(ke$route_short_name))
  })
  output$linnliin <- renderText({
    ke1=number(input$algpt,input$lopppt,input$Kuup,"00:00",routes[routes$competent_authority==input$linn,])
    ck=arrange(ke1[ke1$route_short_name==input$bussinr,],departure_time)
    if (identical(ck[,3], character(0))){
      print('Bussi ei sõida sellel marsuudil antud kuupäeval')
    }
    else {
      print(ck[,3])
    }
  })
  
  output$kaart_joonis <- renderLeaflet({
    joon(input$alg,input$siht)
  })
  
  output$linn_kaart <- renderLeaflet({
    joon(input$algpt,input$lopppt)
  })
  
  output$reisid <- renderPlot({
    j=ggplot(agt[agt$agency_name != 'Tallinna Linnatranspordi AS',],aes(agency_name,kok,fill=rat1))+
      geom_bar(stat='identity')+coord_flip()+ylim(0,1750)+theme_bw()+ylab('Reiside arv')+
      xlab('Bussifirma')+scale_fill_discrete(name="Ratastooli sõbralik")
    j
  })
  
  output$soidupaev <- renderPlot({
    calendar$kokku=calendar$monday+calendar$tuesday+calendar$wednesday+calendar$thursday+calendar$friday+calendar$saturday+calendar$sunday
    counts <- table(calendar$kokku)
    barplot(counts,
            xlab="Liinide tööpäevade arv.", col="Darkblue")
  })
  
  output$pilet <- renderPlot({
   fare=merge(fare_attributes[,c(2,6)],agency[,c(1,2)],by='agency_id',all.x=TRUE)
    k=ggplot(fare,aes(agency_name,price))+geom_boxplot()+coord_flip()+theme_bw()+xlab('Bussifirma')+ylab('Hind')
    k})
  
}

shinyApp(ui=ui,server= server)



