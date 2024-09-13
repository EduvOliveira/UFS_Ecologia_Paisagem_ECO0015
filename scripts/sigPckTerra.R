#' ---
# Disciplina: Ecologia de paisagem 
# https://github.com/EduvOliveira/UFS_Ecologia_Paisagem_ECO0015
# Eduardo V. S. Oliveira
# 20/09/2024
#' ---

# Introducao ao SIG com R-----------------------

### instalando pacotes

if(!require(terra)) install.packages("terra")

# download dados ----
download.file(url = "https://github.com/EduvOliveira/UFS_Ecologia_Paisagem_ECO0015/raw/main/dados.zip", destfile = "dados.zip")

# unzip
unzip(zipfile = "dados.zip")

# 1. Visualizando pontos de ocorrencia

spp<-choose.files()

spp <- read.csv(spp)

# Inserindo vetores

setwd(choose.dir()) 

pol1 <-vect("brasil.shp")
pol2 <- vect("mata_atlantica.shp")
pol3 <- vect("caatinga.shp")
pol4 <- vect("cerrado.shp")
pol5 <- vect("amazonia.shp")

# Calculo de area
pol4$area_km2 <- expanse(pol4, unit="km")
head(pol4$area_km2)

# Fazendo o mapa

par(mar=c(1,1,1,1))

plot(pol1, col = "white",axes = FALSE, box = FALSE,main = "Biomas") 
plot (pol2, add = TRUE, col = "green")
plot (pol3, add = TRUE, col = "orange")
plot (pol4, add = TRUE, col = "yellow")
plot (pol5, add = TRUE, col = "gray")

graphics::points(spp$lon, spp$lat, col='red', pch=20, cex=0.75) 

dev.copy(device = jpeg, file = "PtosBiomas.jpeg", width = 2100, height = 1500, res = 300)
dev.off()


# 2. Carregamento e manipulacao do raster

setwd(choose.dir()) 

r1<-rast("cob_veg.asc")
plot(r1)

r1

# Resumo estatistico do raster
summary(r1)
  
res(r1)  # Resolucao (tamanho de celula)
#~17,7 m
ext(r1)  # Extensao geografica

# 3. Cortando um raster por um vetor

r2 <- terra::crop(r1, pol1)
plot(r2)
plot(pol1, add = T)

r3 <- terra::mask(r2, pol1)

writeRaster(r3, "cob_veg_Br.tif", overwrite=TRUE)

plot(r3)
points(spp$lon, spp$lat, col='red', pch=20, cex=0.75) 

dev.copy(device = jpeg, file = "Ptos_cob.jpeg", width = 3300, height = 2000, res = 300)
dev.off()

# 4. converter o raster numa matriz de dados

matrix<-as.data.frame(r3) 
write.csv(matrix, "cob_veg_Br.csv")

# 5. Agregar celulas (i.e., diminuir a resolucao)

raster.new <- terra::aggregate(r3, fact=8, fun=mean) 

plot(raster.new)

# Desagregar celulas

rast10 <- disagg(r3, fact = 2, method = "bilinear")

plot(rast10)

# Reclassificando a cobertura em classes
cobclass <- classify(r3, c(0, 20, 40, 60, 80,100), include.lowest=TRUE)

plot(cobclass)

# 6. Desenhando um vetor

setwd(choose.dir()) 
r4 <- rast("aju.tif")
plot(r4)

# Desenhar um poligono interativamente sobre o mapa
poly<-draw(x="polygon", col="red", lwd=2, id=FALSE)
plot(poly)

p2<-draw(x="polygon", col="red", lwd=2, id=FALSE)
plot(p2)

line<-draw(x="line", col="red", lwd=2, id=FALSE)
plot(line)

point<-draw(x="points", col="red", lwd=2, id=FALSE)
plot(point)

# Salvando o shape

writeVector(poly, "mangue.shp")

mangue<-vect("mangue.shp")
plot(mangue)

# Elaborando um mapa 

map("world", "Brazil", col = "lightgray", fill = TRUE)
map("world", "Brazil", col = "lightgray", fill = TRUE, xlim = c(-50, -36), 
    ylim = c(-25, -15)) 
plot(pol2, add = TRUE, col = "red") 

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
