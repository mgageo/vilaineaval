# <!-- coding: utf-8 -->
#
# lecture d'un fichier ogr
parcours_get <- function(d) {
  library(RCurl)
  dsn <- 'http://bretagne-vivante-dev.org/bvo35rva/bvo35rva.php?action=get_parcours'
  dsn <- 'http://bv/bvo35rva/bvo35rva.php?action=get_parcours'
  txt <- getURL(dsn)
  dsn <- sprintf("%s/parcours.geojson", varDir)
  write(txt,file=dsn);
}
# lecture d'un fichier ogr
parcours_lire <- function(d) {
  require(rgdal)
  require(rgeos)
  dsn <- sprintf("%s/parcours.geojson", varDir)
  print(sprintf("parcours_lire() dsn:%s", dsn))
  layer <- ogrListLayers(dsn)
  print(sprintf("parcours_lire() %s %s", layer, dsn))
  spdf <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE, use_iconv=TRUE, encoding="UTF-8")
  spdf <- spTransform(spdf, CRS("+init=epsg:2154"))
  spdf@data$longueur <- as.integer(gLength(spdf, byid=TRUE))
  obseurs.df <- parcours_obseurs()
  spdf@data <- merge(x=spdf@data, y=obseurs.df, by="email", all.x = TRUE)
  return(spdf)
}
parcours_carte <- function() {
  print(sprintf("parcours_carte()"))
  spdf <- parcours_lire()
  fonds_raster()
  fonds_grille()
  plot(spdf, add=TRUE, lwd=4, col="orange")
}
# http://gis.stackexchange.com/questions/119993/convert-line-shapefile-to-raster-value-total-length-of-lines-within-cell
parcours_stat_champ <- function(champ= 'parcours') {
  require(rgdal)
  require(rgeos)
  require(ggplot2)
  print(sprintf("parcours_stat_champ() champ:%s", champ))
  spdf <- parcours_lire()
  df <-spdf@data
  df <- df[order(df$longueur),]
#  print(head(df, 80))
  nb.df <- data.frame(table(df[, champ]))
  colnames(nb.df) <- c("critere", "nb")
#  print(nb.df$critere)
  p <- ggplot(data=nb.df, aes(x=critere, y=nb)) +
    coord_flip() +
    scale_x_discrete("") +
    geom_bar(stat="identity", position=position_dodge(), colour="black")
  print(p)
  return(p)
}
#
# pb d'encodage
parcours_obseurs_v1 <- function() {
  print(sprintf("parcours_obseurs()"))
  df <- read.table(text="email|prenom
mga.gauthier@yahoo.fr|Marc
famillebeaufils@wanadoo.fr|Matthieu
jorigne.bastien@gmail.com|Bastien
regis.morel@bretagne-vivante.org|Régis
", header=TRUE, sep="|", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="", encoding="UTF-8")
  return(df)
}
parcours_obseurs <- function() {
  print(sprintf("parcours_obseurs()"))
  library("xlsx")
  dsn <- sprintf("%s/obseurs.xls", serenaDir)
  df <- read.xlsx2(dsn, 1, header=TRUE, colClasses=NA)
  return(df)
}
# http://www.sthda.com/english/wiki/ggpubr-r-package-ggplot2-based-publication-ready-plots#at_pco=smlwn-1.0&at_si=585b88460948f752&at_ab=per-2&at_pos=0&at_tot=1
# https://www.r-bloggers.com/avoid-overlapping-labels-in-ggplot2-charts/
# https://rstudio-pubs-static.s3.amazonaws.com/106330_a239701c8d2045969e58d4110e9a8c48.html
parcours_stat_email_longueur <- function() {
  library(ggplot2)
  print(sprintf("parcours_stat_email_longueur()"))
  parcours_get()
  spdf <- parcours_lire()
  df <- spdf@data
  p <- ggplot(data=df, aes(x = prenom, y = longueur)) +
    geom_boxplot() +
    geom_text(stat="count", aes(label=..count..), y=-10) +
    scale_x_discrete(name="")
  print(p)
  return(p)
}