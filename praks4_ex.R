#---------------------------------------------------------------------------
#Näide 1
library(maps)
library(geosphere)

map("world", col="#f2f2f2", fill=TRUE, border=NA)

lat_liivi = 58.379491
lon_liivi = 26.713199
lat_fb = 37.485245
lon_fb = -122.148807

gc_points = gcIntermediate(c(lon_liivi, lat_liivi), c(lon_fb, lat_fb), n=50, addStartEnd=TRUE)
lines(gc_points)






#---------------------------------------------------------------------------
#Näide 2
library(ggmap)
library(geosphere)

lat_liivi = 58.379491
lon_liivi = 26.713199
lat_eyjafjallajokull = 63.37120
lon_eyjafjallajokull = -19.36480

map <- get_map(location = 'Europe', zoom = 3 ,source = "google")
a <- ggmap(map)

gc_points = gcIntermediate(c(lon_liivi, lat_liivi), c(lon_eyjafjallajokull, lat_eyjafjallajokull), n=50, addStartEnd=TRUE)
gc = data.frame(gc_points)

a <- a + geom_path(data = gc, aes(x = lon, y = lat, colour = 'red'))
a





#---------------------------------------------------------------------------
#Näide 3
library(leaflet)
library(geosphere)

lat_liivi = 58.379491
lon_liivi = 26.713199
lat_pitsa = 41.535158
lon_pitsa = 12.306798

m = leaflet() #koordinaatsüsteemis sisse lugemine

m = addTiles(m) #kaardikihtide lisamine

#suurringjoone punktid
gc_points = gcIntermediate(c(lon_liivi, lat_liivi), 
                           c(lon_pitsa, lat_pitsa), 
                           n=50, addStartEnd=TRUE)
gc = data.frame(gc_points)

m = m %>% #punktide lisamine kaardile
  addCircles(lng = lon_liivi, lat = lat_liivi, color = 'red', weight = 3) %>%
  addCircles(lng = lon_pitsa, lat = lat_pitsa, color = 'red', weight = 3)

m = m %>% #joonte lisamine kaardile
  addPolylines(lng = gc$lon, lat = gc$lat, weight = 3, color = 'red')

m = m %>% 
  setView(lng = 0, lat = 50, zoom = 3) %>% 
  addProviderTiles("Esri.WorldImagery") #geoloogiline
m





#---------------------------------------------------------------------------
#Kodune töö 
lennujaamad <- read.table("C:\\Users\\oliver17\\Desktop\\Business\\Visualiseerimine\\4. Neljas\\airports.csv", header=T,sep = ",")
lennud <- read.table("C:\\Users\\oliver17\\Desktop\\Business\\Visualiseerimine\\4. Neljas\\flights.csv", header=T,sep = ",")

tallinn = lennujaamad[lennujaamad$id==415,]
tallinnast_lennud = lennujaamad[lennujaamad$id %in% lennud[lennud$source_airport_id==415,]$dest_airport_id, ]


m = leaflet() #koordinaatsüsteemis sisse lugemine

m = addTiles(m) #kaardikihtide lisamine

for (i in 1:length(tallinnast_lennud$lat)){ 
  gc_points = gcIntermediate(c(tallinn$lon, tallinn$lat), 
                             c(tallinnast_lennud$lon[i], tallinnast_lennud$lat[i]), 
                             n=50, addStartEnd=TRUE)
  gc = data.frame(gc_points)
  m = m %>% #joonte lisamine kaardile
    addPolylines(lng = gc$lon, lat = gc$lat, weight = 3, color = 'red')
}

m = m %>% 
  #clearBounds() %>% #should be world map
  setView(lng = 10, lat = 30, zoom = 2) %>% #Algne zoom kontinendi tasemele, kaardi keskkoht umbes Euroopa keskele
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") #Muudab google vaadet: lahedad variandid, nt teedekaart, geoloogiline jne
  #addProviderTiles("Esri.WorldImagery") #geoloogiline
  #addProviderTiles("Stamen.TonerHybrid")
m




























sihtpunktid = function(lennujaama_id){
  sihtpunktid = lennujaamad[lennujaamad$id %in% lennud[lennud$source_airport_id==lennujaama_id,]$dest_airport_id,]
  return(sihtpunktid)
}

for (i in 1:length(tallinnast_lennud$lat)){
  alguspunkt = lennujaamad[lennujaamad$id==tallinnast_lennud$id[i],]
  siht_lennujaamad = sihtpunktid(tallinnast_lennud$id[i])
  for (i in 1:length(siht_lennujaamad$lat)){
    gc_points = gcIntermediate(c(alguspunkt$lon, alguspunkt$lat), 
                               c(siht_lennujaamad$lon[i], siht_lennujaamad$lat[i]), 
                               n=50, addStartEnd=TRUE)
    gc = data.frame(gc_points)
    
    m = m %>%
      addPolylines(lng = gc$lon, lat = gc$lat, weight = 0.7, opacity = 0.3, color = 'blue') #'#7DF9FF'
  }
}

m = m %>% 
  #clearBounds() %>% #should be world map
  setView(lng = 10, lat = 30, zoom = 2) %>% #Algne zoom kontinendi tasemele, kaardi keskkoht umbes Euroopa keskele
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") #Muudab google vaadet: lahedad variandid, nt teedekaart, geoloogiline jne
#addProviderTiles("Esri.WorldImagery") #geoloogiline
#addProviderTiles("Stamen.TonerHybrid")
m




