#' ---
# Disciplina: Ecologia de paisagem 
# https://github.com/EduvOliveira/UFS_Ecologia_Paisagem_ECO0015
# Eduardo V. S. Oliveira
# 20/09/2024
#' ---

# Introducao ao SIG com R-----------------------

### instalando pacotes

if(!require(raster)) install.packages("raster")
if(!require(maps)) install.packages("maps")
if(!require(mapproj)) install.packages("mapproj")

# Plotando pontos de ocorrencia

spp<-choose.files()

spp <- read.csv(spp)

# Inserindo vetores

setwd(choose.dir()) 

pol1 <-shapefile("brasil.shp")
pol2 <- shapefile("mata_atlantica.shp")
pol3 <- shapefile("caatinga.shp")
pol4 <- shapefile("cerrado.shp")
pol5 <- shapefile("amazonia.shp")

# Fazendo o mapa

par(mar=c(1,1,1,1))

plot(pol1, col = "white") 
plot (pol2, add = TRUE, col = "green")
plot (pol3, add = TRUE, col = "orange")
plot (pol4, add = TRUE, col = "yellow")
plot (pol5, add = TRUE, col = "gray")

points(spp$lon, spp$lat, col='red', pch=20, cex=0.75) 

map.scale(ratio = F, cex = 0.7)

legend("bottomright", legend= c("Mata Atlantica", "Caatinga", "Cerrado", "Amazonia"), 
       fill=c ("green", "orange", "yellow", "gray"), horiz = F,bty = "1",
       title = "Biomas", box.col = "white")

dev.copy(device = jpeg, file = "PtosBiomas.jpeg", width = 2100, height = 1500, res = 300)
dev.off()

# Abrir um raster

setwd(choose.dir()) 

r1 <- raster("cob_veg.asc")
plot(r1)

# Cortando um raster por um vetor

r2 <- crop(r1, extent(pol1), snap='out')
plot(r2)
plot(pol1, add = T)

r3 <- mask(r2, pol1)

writeRaster(r3,"cob_veg_Br.asc",format="ascii")

plot(r3)
points(spp$lon, spp$lat, col='red', pch=20, cex=0.75) 

dev.copy(device = jpeg, file = "Ptos_cob.jpeg", width = 3300, height = 2000, res = 300)
dev.off()

#converter o raster numa matriz de dados

matrix<-as.data.frame(r3) 
write.csv(matrix, "cob_veg_Br.csv")

# Agregar celulas (i.e., diminuir a resolucao)

raster.new <- aggregate(r3, fact=8, fun=mean) 
plot(raster.new)

#Informacoes da celula
click(raster.new)

# Criando um conjunto de arquivos raster

setwd(choose.dir()) 
clim <- stack("bio1.bil", "bio4.bil", "bio12.bil", "bio15.bil")
clim
plot(clim)

#camadas animadas (sequenciadas)
animate(clim, pause=0.50, n = 5)

# Desenhando um vetor

setwd(choose.dir()) 
r4 <- raster("aju.tif")
plot(r4)

d1<-drawPoly(sp=TRUE, col='red', lwd=2)
d2<-drawPoly(sp=TRUE, col='red', lwd=2)
d3<-drawPoly(sp=TRUE, col='red', lwd=2)

l1<-drawLine(sp=TRUE, col='red', lwd=2)

plot(d1)

# Unir vetores

x <- bind(d1, d2, d3)
plot(x) 

shapefile(x, "mangue.shp")

mangue<-shapefile("mangue.shp", warnPRJ = FALSE)
plot(mangue)

# Elaborando um mapa 

map("world", "Brazil", col = "lightgray", fill = TRUE)
map("world", "Brazil", col = "lightgray", fill = TRUE, xlim = c(-50, -36), 
    ylim = c(-25, -15)) 
plot(pol2, add = TRUE, col = "red") 
box()

par(mar=c(1,1,1,1)) 
map("world","Brazil")
map.axes()
map.scale(ratio = F, cex = 0.7) 
abline(h = 0, lty = 2) 

map("world","Brazil", fill=T, col="grey90")
map(add=T) 
map.axes()
map.scale(ratio=F, cex=0.7)
abline(h=0, lty = 2)
map.cities(country = "Brazil",minpop = 2000000,pch=19, cex=1.2)

m <- map("world","Brazil", fill=T, col="grey95")
map(add=T)
map.axes()
map.scale(ratio=F, cex=0.7)
abline(h=0, lty = 2)
map.grid(m, nx = 5, ny = 5, col="grey50", font=1, cex=0.7, pretty = T)


###FIM###