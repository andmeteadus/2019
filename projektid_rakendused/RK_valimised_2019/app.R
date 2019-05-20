library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(envalysis)
library(reshape2)
library(ggsci)
library(ggplot2)
library(htmltab)
library(forcats)

options(scipen=99)
theme_set(theme_publish())

#ANDMETE SISSELUGEMINE ja ÜHENDAMINE

#reklaamikulu andmed
reklaam18 <- read.csv("~/sajvprojekt/reklaam18.csv", encoding="UTF-8", sep=";", row.names=1,stringsAsFactors=FALSE)[-c(4,6,10,12,14,15),]
reklaam18 %>% mutate(Erakond=row.names(reklaam18), 
                     Kvartal="2018. a IV kvartal") -> reklaam18
reklaam <- read.csv("~/sajvprojekt/reklaam.csv", encoding="UTF-8", row.names=1, sep=";", stringsAsFactors=FALSE)[-c(5,10,12,13),]
reklaam %>% mutate(Erakond=row.names(reklaam), 
                   Kvartal="2019. a I kvartal") -> reklaam

merge(reklaam,reklaam18,all=T) %>% arrange(Kvartal,Erakond) %>%
  mutate(Värv=rep(c("#007d5a","#8B4513","#FFDE00",
                "#800080","#489e65","#31758a", 
                "#8db751","#009FE3","#E10600"),2),
         Lühend=rep(c("KESK","EKRE","REF","Vabaerakond","Elurikkus","E200","Rohelised","Isamaa","SDE"),2),
         Kokku=gsub("\\s+","",Kokku)%>%as.numeric(),
         Telereklaam=gsub("\\s+","",Telereklaam)%>%as.numeric(),
         Raadioreklaam=gsub("\\s+","",Raadioreklaam)%>%as.numeric(),
         Internetireklaam=gsub("\\s+","",Internetireklaam)%>%as.numeric(),
         Välireklaam=gsub("\\s+","",Välireklaam)%>%as.numeric(),
         Ajakirjandusreklaam=gsub("\\s+","",Ajakirjandusreklaam)%>%as.numeric(),
         Reklaamtrükised=gsub("\\s+","",Reklaamtrükised)%>%as.numeric(),
         Erakond=ifelse(Erakond=="ISAMAA Erakond","Isamaa Erakond",Erakond)) %>% dplyr::rename(Reklaamikulud=Kokku) -> reklaamid

reklaam_koos = reklaamid %>% 
  group_by(Erakond) %>% 
  summarise(Reklaamikulud_koos = sum(Reklaamikulud))

erakonnad<-melt(data = reklaamid, Erakond.vars = "Erakond", 
                measure.vars = c("Telereklaam", "Raadioreklaam","Internetireklaam", 
                                 "Välireklaam", "Ajakirjandusreklaam","Reklaamtrükised")) %>% 
  dplyr::select("Lühend", "Kvartal", "variable", "value", "Värv", "Erakond") %>%
  dplyr::rename(Reklaamikulu=value, Reklaam=variable)

#valimiste andmed
suppressMessages(rk2019.full <- read_excel("~/rk2019-full.xlsx",  col_names = FALSE) %>% .[,c(1,3:4)] %>%
  dplyr::rename(Reg.nr=...1,Kandidaat=...3,Erakond=...4))

rk2019.tulemused <- read.csv("~/rk2019-tulemused.csv",encoding="UTF-8", stringsAsFactors=FALSE)

haaled_eraldi = merge(rk2019.full,rk2019.tulemused, by=c("Reg.nr","Kandidaat")) %>% 
  filter(!Erakond%in%c("Üksikkandidaadid","Eestimaa Ühendatud Vasakpartei")) %>%
  mutate(Kandidaat=as.character(Kandidaat))

url = "https://rk2019.valimised.ee/et/election-result/election-result.html"
suppressMessages(haaled19 <- htmltab(doc = url, which=1)[-c(6,11,12),1:2])
colnames(haaled19)=c("Erakond","Hääli")
haaled19 = haaled19 %>% mutate(Aasta=2019,Hääli=as.numeric(Hääli),
                               Protsent=round(Hääli/561141,2),
                               Erakond=as.character(Erakond),
                               Värv=c("#FFDE00","#007d5a","#8B4513","#009FE3",
                                      "#E10600","#31758a","#8db751",
                                      "#489e65","#800080"),
                               Lühend=c("REF","KESK","EKRE","Isamaa","SDE","E200",
                                        "Rohelised","Elurikkus","Vabaerakond"))
haaled19_reklaam = merge(reklaam_koos,haaled19) %>% 
  mutate(Ühe_hääle_hind=round(Reklaamikulud_koos/Hääli,2))

#RAKENDUSE UI

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

#SERVER
server <- function(input, output) {
  output$joonis <- renderPlot({
    haaled_eraldi %>% 
      filter(Erakond==input$erakond) %>%
      top_n(input$n,Kokku) %>%
      ggplot(aes(y=Kokku,x=fct_reorder(Kandidaat,Kokku),label=Kokku)) +
      xlab("")+
      theme(axis.text.y = element_text(size=15))+
      ylab("Hääli") +
      geom_col(fill=(haaled19%>%filter(Erakond==input$erakond))$Värv) + 
      geom_label(size = 6, hjust=1.1, fill="white") +
      coord_flip()
  }, height=650)
  
  output$joonis_vahelduv <- renderPlot({
    if (input$tyyp == "yldine") {
      reklaamid %>%
        ggplot(aes(y=Reklaamikulud, x=Lühend, fill=reklaamid$Erakond)) + 
        geom_col()+
        facet_wrap(~Kvartal)+
        scale_fill_manual(values=reklaamid$Värv)+
        theme(axis.title.y = element_blank(),
              legend.position = "none", 
              legend.title = element_blank()) + coord_flip()
    }
    else {
      reklaamid %>%
        melt(measure.vars=c("Telereklaam","Raadioreklaam","Internetireklaam","Välireklaam",
                            "Ajakirjandusreklaam","Reklaamtrükised"), value.name="Reklaamikulu") %>%
        ggplot(aes(y=Reklaamikulu, x=Lühend, fill=variable)) + 
        geom_col()+
        facet_wrap(~Kvartal)+
        scale_fill_uchicago()+
        theme(axis.title.y = element_blank(),
              legend.title = element_blank(),
              legend.justification = c(1,1),
              legend.position = c(0.5,1),
              legend.text = element_text(size=15)) + 
        coord_flip()
    }
  })
  
  output$joonis3 <- renderPlot({
    data1 %>% 
      ggplot(aes(y=Reklaamikulud, x=Erakond, fill=Erakond)) + 
      geom_histogram(stat="identity")+
      scale_fill_manual(values=data1$Värv, labels=data1$Lühend) + 
      theme(axis.text.x=element_blank(), 
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "bottom", legend.title = element_blank(), legend.spacing.x = unit(0.1, 'cm')) +
      guides(fill = guide_legend(nrow = 1))
  })
  
  output$reklaam_protsent <- renderPlot({
    haaled19_reklaam %>% 
      ggplot(aes(x=Reklaamikulud_koos,y=Protsent, color=Erakond)) +
      scale_color_manual(values=haaled19_reklaam$Värv, labels=haaled19_reklaam$Lühend) +
      xlab("Kogu reklaamikulu")+
      ylab("Valimistulemus") + 
      labs(size="Ühe hääle hind") +
      geom_point(aes(size=Ühe_hääle_hind))+ scale_size(range = c(2,12))+
      scale_y_continuous(labels = scales::percent)
  })
  
  output$plot5 <- renderPlot({
    haaled19_reklaam %>% 
      ggplot(aes(x=fct_reorder(Lühend,Hääli), y=Hääli, fill=Erakond)) + 
      geom_bar(stat="identity") + 
      coord_flip() + 
      scale_fill_manual(values=haaled19_reklaam$Värv) + 
      theme(legend.position = "none", axis.title.y=element_blank()) + 
      geom_label(aes(label=Hääli), fill="white", size=6, hjust=-0.1) +
      ylim(0,200000)
  }, height=450)
  
  output$plot3 <- renderPlot({
    erakonnad %>%
      filter(Reklaam==input$kulutus, Kvartal==input$kvartal2) %>%
      ggplot(aes(x=Lühend, y=Reklaamikulu, fill=Erakond)) +
      geom_bar(stat="identity") +  
      scale_fill_manual(values=arrange(filter(erakonnad, Reklaam==input$kulutus, Kvartal==input$kvartal2), Erakond)%>% .[,"Värv"]) + 
      coord_flip() +
      theme(legend.position = "none", axis.title.y=element_blank())
  })
  
  output$plot1 <- renderPlot({
    erakonnad %>%
      filter(Erakond==input$erakond2 , Kvartal==input$kvartal) %>%
      ggplot(aes(x=Reklaam, y=Reklaamikulu, fill=Lühend)) +
      geom_bar(stat="identity") +  
      scale_fill_manual(values=filter(erakonnad, Erakond==input$erakond2, Kvartal==input$kvartal)%>% .[,"Värv"]) + 
      coord_flip() + theme(legend.position = "none", axis.title.y=element_blank())+ 
      ylim(0, 500000)
  })
  output$plot2 <- renderPlot({
    erakonnad %>%
      filter(Erakond==input$erakond2 , Kvartal==input$kvartal) %>%
      ggplot(aes(x=Reklaam, y=Reklaamikulu, fill=Lühend)) +
      geom_bar(stat="identity") +  
      scale_fill_manual(values=filter(erakonnad, Erakond==input$erakond2, Kvartal==input$kvartal)%>% .[,"Värv"]) +
      coord_flip() + theme(legend.position = "none", axis.title.y=element_blank())
  })
  
  output$plot6 <- renderPlot({
    ggplot(haaled19_reklaam, aes(x=Lühend, y=Reklaamikulud_koos, fill=Erakond)) +  
      geom_bar(stat="identity") + scale_fill_manual(values = arrange(haaled19_reklaam, Erakond)$Värv) + 
      theme(axis.text.x = element_text(angle = 40, hjust = 1)) + 
      theme(legend.position = "none", axis.title.x=element_blank())  + ylab("Reklaamikulud")
  }, height=450)
  output$plot7 <- renderPlot({
    ggplot(haaled19_reklaam, aes(x=Lühend, y=Protsent, fill=Erakond)) +
      geom_bar(stat="identity") + scale_fill_manual(values = arrange(haaled19_reklaam, Erakond)$Värv) + 
      theme(axis.text.x = element_text(angle = 40, hjust = 1)) + 
      theme(legend.position = "none", axis.title.x=element_blank()) +
      ylab("Valimistulemus")+
      scale_y_continuous(labels = scales::percent)
  }, height=450)
}

shinyApp(ui, server)
