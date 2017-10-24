
library(shiny)
library(doParallel)
library(rvest)
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(scales)

load("andmestik_Etis.RData")
colnames(andmestik) = iconv(colnames(andmestik), from="UTF-8", to = "UTF-8") # Taavi lisatud 
colnames(andmestik)[colnames(andmestik) == "Vastutav.täitja"] = "Vastutav.taitja" # Taavi lisatud
colnames(andmestik)[colnames(andmestik) == "Projekti.lõpp"] = "Projekti.lopp" # Taavi lisatud
colnames(andmestik)[colnames(andmestik) == "Vastutav.täitja.Pro"] = "Vastutav.taitja.Pro" # Taavi lisatud

########################################### ANDMETE SAAMINE ###################################

# Andmete kättesaamise funktsioon

hangiAndmestik <- function() {
  # lk-de koguarv
  url <- "https://www.etis.ee/Portal/Projects/Index?Page=1&PageSize=100"
  page <- read_html(url,encoding = "UTF-8")
  page_nr_vect <- page %>% 
    html_nodes(".default") %>%
    html_nodes("a") %>%
    html_text()
  
  max_page_nr <- 0
  for (i in 1:length(page_nr_vect)) {
    if (!is.na(as.numeric(page_nr_vect[i])) & as.numeric(page_nr_vect[i]) > max_page_nr) {
      max_page_nr <- as.numeric(page_nr_vect[i])
    }
  }
  
  # eelneva saaks lühemalt 
  # as.numeric(page_nr_vect[which(page_nr_vect=="»")[1]-1])
  # peaks töötama ka siis, kui lehti alla 100
  
  # hangi andmed
  max_page_nr = 3
  data <- foreach (i = 1:max_page_nr, .combine=rbind) %do% {
    print(paste("lk:", i, sep=" "))
    url <- paste("https://www.etis.ee/Portal/Projects/Index?Page=", i, "&PageSize=100", sep = "")
    page=html_session(url)
    tabel <- page %>%
      html_nodes(".table")%>%
      html_table()
    data.frame(tabel)
  }
  colnames(data)[colnames(data) == "Vastutav.täitja"] = "Vastutav.taitja" # Taavi lisatud
  # andmestik korda (raha, asutuse nimi ja programmi tüüp)
  data <- data[,2:ncol(data)]
  data <- data[!apply(data, 1, function(x) any(x=="")),]
  data <- apply(data, c(1,2), function(x) {ifelse(x == "-", NA, x)})
  data <- as.data.frame(data)
  rahad <- as.vector(data$Rahastamine.kokku)
  rahad <- strsplit(rahad, " E")
  rahad <- sapply(rahad, "[", 1)
  rahad <- gsub(",", ".", rahad)
  rahad <- gsub(" ", "", rahad)
  rahad <- as.numeric(rahad)
  data$rahastus <- rahad
  data <- data %>% filter(rahastus > 0)
  
  for(i in 1:nrow(data)){
    sep1 <- strsplit(as.character(data$Asutus[i]), ",")[[1]][1]
    sep2 <- strsplit(as.character(data$Programm[i]), ";")[[1]][1]
    sep3 <- strsplit(sep1,";")[[1]][1]
    data$asutus_uus[i] <- sep3
    data$programm_uus[i] <- sep2
  }
  
  # Leiame projekti kestvuse ja kas projekt kestab veel
  data$projekti_kestvus <- as.Date(data$Projekti.lõpp, format = "%d.%m.%Y")- as.Date(data$Projekti.algus, format = "%d.%m.%Y")
  data$kestev_projekt <- ifelse((as.Date(data$Projekti.lõpp, format = "%d.%m.%Y")-Sys.Date()) >0, 1,0)
  data$ID <- seq.int(nrow(data))
  data <- data %>%
    mutate(nimi_id = paste(Vastutav.taitja, ID, sep = " "))
  
  save(data, file = "andmestik_Etis4.RData")
  #write.table(data, "andmestik_etis.txt", sep = "\t", col.names = T, row.names = F,quote = F)
}

########################################### VAJALIKUD FUNKTSIOONID KUVAMISEKS #####################

# funktsioon kõige enam rahastust saanud teadlaste leidmiseks

plot_rikkad <- function(data, n, järjestus) {
  
  max_money <- data %>%
    group_by(Vastutav.taitja, asutus_uus) %>%
    summarise(raha_kokku = sum(rahastus)) %>%
    arrange(desc(raha_kokku)) %>%
    ungroup()
  
  if(järjestus == "Rikkad") {
    max_money <- max_money %>% top_n(n, raha_kokku)
  } else {
    max_money <- max_money %>% top_n(n, -raha_kokku)
  }

  #show_col(c(brewer_pal(palette = "Dark2",direction = -1)(8), brewer_pal(palette = "Paired")(6)))
  return(ggplot(max_money, aes(reorder(Vastutav.taitja, raha_kokku), raha_kokku, fill = asutus_uus))+
           geom_bar(stat = "identity") + 
           scale_y_continuous(labels = comma) +
           labs(x="Vastutav taitja", y="Rahastus EUR")+
           scale_fill_manual(name = "Asutus",values = c(brewer_pal(palette = "Dark2",direction = -1)(8), brewer_pal(palette = "Paired")(6)))+
           theme(panel.background = element_blank(), axis.line.y = element_line(color="black", size = 0.5),axis.text.y = element_text(colour = "black", size = 10), axis.text.x = element_text(colour = "black", size = 10))+
           coord_flip())
}

# funktioon, mis kuvaks andmestikust vastava arvu top rahastusega välja ning valida saab, kas kõrgemad
#rahastuse saajad või madalamad

kuva_n = function(data, n, top){
  data %>% filter(kestev_projekt == 1) %>%
    group_by(nimi_id, asutus_uus) %>%
  arrange(desc(rahastus)) %>% ungroup()-> t
  
  if(top == "Rikkad") {
    t <- t %>% top_n(n, rahastus) 
  } else {
    t <- t %>% top_n(n, -rahastus)  
  }
  #top_n(n, rahastus)->t
  p <- ggplot(t, aes(reorder(nimi_id, rahastus), rahastus, fill=asutus_uus))+
    geom_bar(stat = "identity", position = "dodge") + 
    scale_y_continuous(labels = comma) +
    labs(x="Vastutav taitja", y="Rahastus EUR")+
    scale_fill_brewer(name = "Asutus",palette = "Spectral")+
    theme(panel.background = element_blank(), 
          axis.line.y = element_line(color="black", size = 0.5),
          axis.text.y = element_text(colour = "black", size = 10),
          axis.text.x = element_text(colour = "black", size = 10))+
    coord_flip()
  return(p)
}


# andmestiku loomine, kus on erinevate vastutajate projektide nimed ja nende rahastus

andmestik %>%
  group_by(Vastutav.taitja,kestev_projekt,Nimi)%>%
  summarise(n_projekt = length(Nimi), raha= sum(rahastus)) %>%
  arrange(desc(n_projekt))-> vastutajad

# funktsioon, mis leiab vastutajad andmestikust sisestatud nimega väljad 

vastutaja_a = function(data,nimi){
  data = data %>%
    filter(Vastutav.taitja == nimi)
  data$kestev_projekt <- ifelse(data$kestev_projekt == 0, "Ei", "Jah")
  colnames(data) <- c("Vastutav taitja", "Kestev projekt", "Projekti nimi", "Projektide arv", "Rahastus")
  return(data)
}

# andmestiku loomine 25 peamise asutuste kogurahastuse graafiku kuvamiseks

andmestik %>%
  group_by(asutus_uus, kestev_projekt) %>%
  summarise(rahastus_kokku = sum(rahastus,na.rm = T)) %>%
  arrange(desc(rahastus_kokku)) %>%
  ungroup() %>%
  top_n(25) -> t3

# vastav barplot

p2<- ggplot(t3[-47,], aes(reorder(asutus_uus, rahastus_kokku), rahastus_kokku, fill=factor(kestev_projekt)))+
  geom_bar(stat = "identity", position = "dodge") + 
  labs(y="Rahastus kokku EUR", x="Asutus") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(name = "Kestev projekt", labels = c("Ei", "Jah"), values = c("purple4","steelblue2") )+
  theme(panel.background = element_blank(), 
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.y = element_text(colour = "black", size = 10), 
        axis.text.x = element_text(colour = "black", size = 10))+
  coord_flip()

# funktsioon, mis kuvan andmestikust sisestatud asutuse projektide arvu, kogu rahastuse ja vastutavate taitjate arvu

asutus_raha = function(data, asutus){
  data %>%
    filter(asutus_uus == asutus)%>%
    group_by(kestev_projekt, programm_uus) %>%
    summarise(projekte_kokku = length(Nimi), rahastus_kokku = sum(rahastus), 
              vastutavaid_taitjaid = length(Vastutav.taitja))->tabel
  tabel$kestev_projekt <- ifelse(tabel$kestev_projekt == 0, "Ei", "Jah")
  colnames(tabel) <- c("Projekt kestev", "Programm", "Projektid kokku", "Rahastus kokku", "Vastutavaid taitjaid")

  return(tabel)
}

####################################### SHINY SERVER #####################################

shinyServer(function(input, output) {
  
   output$valitud_top2 <- renderText({ 
     paste("Kasutaja valitud väärtus: Kuva n rahastuse saajat:", input$top2)})
   
   # Peaks tulema uue ploti jaoks
   output$plot_top2 <- renderPlot({
     kasutaja_n2 <- input$top2
     järjestus2 <- input$valik2
     plot_rikkad(andmestik, kasutaja_n2, järjestus2)
   },width = 1200)
  
   output$valitud_top <- renderText({ 
     paste("Kasutaja valitud väärtus: Kuva n rahastuse saajat:", input$top)})
   
  output$plot_top <- renderPlot({
    kasutaja_n <- input$top
    järjestus <- input$valik
     kuva_n(andmestik, kasutaja_n, top = järjestus)
   # barplot_n(andmed)
  },width = 1200)
  
  output$vastutaja <- renderText({
    paste("Kasutaja valitud väärtus: Projekti andmed järgnevale vastutajale:", input$vastutajad)
    
  })
  
  output$tabel_vastutaja <- renderTable({
    nimi = input$vastutajad
    vastutaja_a(vastutajad, nimi)
  })
  
  output$plot_asutus <- renderPlot({
    p2
  }, width = 1200)

  output$valitud_asutus <- renderText({
    paste("Kasutaja valitud asutus: Täpsemad projekti andmed kuvatakse järgnevale asutusele:", input$asutuse_valik)})
  
  output$valitud_asutus_tabel <- renderTable({
    asutus = input$asutuse_valik
    asutus_raha(andmestik, asutus = asutus)
  }, height=500)
  
  observe({
    # do_something_button is your actionButton id.
    if (input$Uuendan > 0) {
      # perform behind the scenes analysis here.
      hangiAndmestik()
      print("laetud")
    }
  })
  
})
