# Andmed saime lehelt: 
# https://datasets.imdbws.com/ ,
# kust tuleb alla tõmmata failid nimega
# 1)  title.basics.tsv.gz,
# 2)  title.akas.tsv.gz,
# 3)  title.ratings.tsv.gz.


library(dplyr)
library(readr)
#Basic andmestik
data <- read_delim("data.tsv", 
                   "\t", escape_double = FALSE, trim_ws = TRUE)

data = filter(data,titleType=="movie")
data = filter(data,isAdult==0)
data = select(data,-5,-7)

#region, language andestik
data2 <- read_delim("data2", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

#ratingu andmestik
data3 <- read_delim("data3", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

data2US = data2 %>% filter(region=="US")

data_m = inner_join(data,data3,by="tconst")
data_m_US = inner_join(data_m,data2US,by=c("tconst"="titleId"))
data_final = data_m_US[data_m_US$primaryTitle == data_m_US$originalTitle,]

DF = data_final %>%
  group_by(tconst) %>%
  top_n(1,-ordering) %>%
  ungroup() %>%
  filter(genres!="\\N", runtimeMinutes!="\\N") %>%
  select(1,3,5:9)

DF = DF %>% 
  group_by(primaryTitle)%>%
  top_n(1,startYear) %>%
  ungroup()

zanrid = DF$genres[DF$genres!="\\N"]
zanrid = strsplit(zanrid, split=",")
DF[,8:10] = do.call(rbind.data.frame, zanrid)
colnames(DF)[8:10] = c("zanr1","zanr2","zanr3")
z = unique(unlist(zanrid))
for(i in 1:length(z)) DF =  mutate(DF, !!z[i]:= (z[i]==zanr1 | z[i]==zanr2 | z[i]==zanr3)*1)
DF = select(DF, -zanr1,-zanr2,-zanr3)


library(qdap)
s?nad = DF$primaryTitle[DF$primaryTitle!="\\N"]
s?nad = strsplit(s?nad, split=" ")
sonad_vec = character()
for(i in s?nad){
  i = strip(i)
  sonad_vec = c(sonad_vec, i)
}
tabel = as.data.frame(table(sonad_vec))
tabel = dplyr::arrange(tabel, desc(Freq))
colnames(tabel) = c("x", "freq")
keelatud = c("the","of","a", "an","","in","and","to")
tabel2 = tabel[!tabel$x %in% keelatud, ]

#save(DF,list="DF", file = "DF.RData")
#save(tabel2,list="tabel2", file = "tabel2.RData")