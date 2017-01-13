# <!-- coding: utf-8 -->
#
# la partie données faune-bretagne
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# lecture des données, export de faune-bretagne
faune_lire <- function() {
  if ( exists("faune.spdf") ) {
    return(faune.spdf)
  }
  switch(faune_donnees,
  'txt' = {
    spdf <- faune_lire_txt()
  },
  'xls' = {
     spdf <- faune_lire_xls()
  },
  'ogr' = {
     spdf <- faune_lire_ogr()
  },
  'hiver' = {
     spdf <- faune_lire_ogr()
  },
  'hiver_atlas' = {
     spdf <- faune_lire_ogr()
  }
  )
  spdf@data$observateur <- sprintf("%s %s", spdf@data$SURNAME, spdf@data$NAME)
# la date
#  spdf@data$d <- as.Date(spdf@data$DATE, "%d.%m.%Y")
  spdf@data$d <- as.Date(as.numeric(spdf@data$DATE), origin="1899-12-30")
  inconnuDF <- subset(spdf@data, is.na(spdf@data$d))
  if ( length(inconnuDF$d) > 0 ) {
    print(sprintf("faune_lire() date invalide"))
    print(head(inconnuDF))
    stop("faune_lire")
  }
  spdf@data$annee <- format(spdf@data$d, "%Y")
  spdf@data$mois <- format(spdf@data$d, "%m")
  if ( faune_donnees == "hiver" ) {
    spdf <- faune_lire_hiver(spdf)
  }
  if ( faune_donnees == "hiver_atlas" ) {
    spdf <- faune_lire_hiver(spdf)
    spdf <- faune_lire_atlas(spdf)
  }
  faune.spdf <<- spdf
  return(spdf)
}
#
# que les données atlas
faune_lire_atlas <- function(spdf) {
  library("raster")
  zone.spdf <- fonds_grille_lire()
#  print(head(zone.spdf@data))
  spdf@data$NUMERO <- over(spdf, zone.spdf)$NUMERO
  df <-spdf@data
  inconnuDF <- subset(df, is.na(df$NUMERO))
  if ( length(inconnuDF$NUMERO) > 0 ) {
    print(sprintf("faune_lire_atlas() maille invalide"))
    print(head(inconnuDF))
#    stop("faune_lire_atlas()")
  }
  spdf <- spdf[!(is.na(spdf@data$NUMERO)),]
  print(sprintf("faune_lire_atlas() nrow: %d", nrow(spdf@data)))
#  stop("***")
  return(spdf)
}
# que les données en hiver
faune_lire_hiver <- function(spdf) {
  spdf@data$quantieme <- format(spdf@data$d, "%j")
  date_from <- as.Date("01/12/2016", "%d/%m/%Y")
  date_to <- as.Date("20/01/2017", "%d/%m/%Y")
  quantieme_from <- format(date_from, "%j")
  quantieme_to <- format(date_to, "%j")
  spdf <- spdf[spdf@data$quantieme >= quantieme_from | spdf@data$quantieme <= quantieme_to,]
  print(sprintf("faune_lire_hiver() nrow: %d", nrow(spdf@data)))
#  stop("***")
  return(spdf)
}
faune_lire_txt <- function(dsn='geo/VILAINEAVAL/donnees.txt') {
  print(sprintf("faune_lire_txt() dsn: %s", dsn))
  spdf <- faune_export_lire(dsn)
  print(sprintf("faune_lire_txt() nrow: %d", nrow(spdf@data)))
  return(spdf)
}
faune_lire_ogr <- function(dsn="geo/VILAINEAVAL/donnees_prec.geojson") {
  print(sprintf("faune_lire_ogr() dsn: %s", dsn))
  spdf <- ogr_lire(dsn)
  print(sprintf("faune_lire_ogr() nrow: %d", nrow(spdf@data)))
  return(spdf)
}
faune_lire_xls <- function() {
  library("xlsx")
  library("raster")
  dsn <- sprintf("%s/export_07012017_073803.xls", fauneDir)
  dsn <- sprintf("%s/donnees_prec.xls", fauneDir)
  print(sprintf("faune_lire_xls() dsn: %s", dsn))
  df <- read.xlsx2(dsn, 1, header=TRUE, colClasses=NA)
  print(head(df[,c('COORD_LAT', 'COORD_LON')]))
  df <- df[, c('ID_SIGHTING', 'ID_SPECIES', 'NAME_SPECIES', 'FAMILY_NAME', 'DATE', 'PLACE', 'MUNICIPALITY', 'INSEE', 'COORD_LAT', 'COORD_LON', 'COMMENT', 'ESTIMATION_CODE',	'TOTAL_COUNT', 'ATLAS_CODE', 'PRECISION', 'NAME', 'SURNAME')]
# la première ligne en moins
  df <- df[-1,]
# transformation en spatial
  df [,"COORD_LAT"] <- sapply(df[,"COORD_LAT"], as.character)
  df [,"COORD_LAT"] <- sapply(df[,"COORD_LAT"], as.numeric)
  df [,"COORD_LON"] <- sapply(df[,"COORD_LON"], as.character)
  df [,"COORD_LON"] <- sapply(df[,"COORD_LON"], as.numeric)
  bug.df <- subset(df, COORD_LAT < 48)
  if ( nrow(bug.df) > 0 ) {
    print(head(bug.df))
    stop("***")
  }

  coordinates(df) = ~ COORD_LON + COORD_LAT
#  print(sapply(df, class))
  spdf <- SpatialPointsDataFrame(df,data.frame(df[,]))
  proj4string(spdf) <- CRS("+init=epsg:4326")
  spdf <- spTransform(spdf, CRS("+init=epsg:2154"))
  print(sprintf("faune_lire_xls() nrow: %d", nrow(spdf@data)))
  dsn <- sprintf("%s/donnees_prec.geojson", fauneDir)
  ogr_ecrire(spdf, dsn, "donnees", driver="GeoJSON")
#  stop("====")
  return()
}
#
# pour faire la carte
faune_carte <- function() {
  spdf <- faune_lire()
  fonds_raster()
  plot(spdf, add=TRUE, pch=19, col="orange", cex=cex)
}
#
# pour faire des stat sur un champ
faune_stat_champ <- function(champ = "NAME_SPECIES") {
#  library(sqldf)
  library(ggplot2)
  spdf <- faune_lire()
  df <- spdf@data
  df <- faune_taxref()
  nb.df <- data.frame(table(df[, champ]))
  colnames(nb.df) <- c("critere", "nb")
  print(head(nb.df,20))
  nb <-sum(nb.df$nb)
#
#  print(nb.df$critere)
  p <- ggplot(data=nb.df, aes(x=critere, y=nb)) +
    coord_flip() +
    scale_x_discrete("") +
    ylab(sprintf("nb : %s", nb)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black")
  print(p)
  return(p)
}
#
# pour faire des stat sur un champ
faune_prec_stat_champ <- function(champ = "NAME_SPECIES") {
#  library(sqldf)
  library(ggplot2)
  spdf <- faune_lire()
  df <- spdf@data
  df <- faune_taxref()
  nb.df <- data.frame(table(df[, champ]))
  colnames(nb.df) <- c("critere", "nb")
  print(head(nb.df,20))
  nb <-sum(nb.df$nb)
#
#  print(nb.df$critere)
  p <- ggplot(data=nb.df, aes(x=critere, y=nb)) +
    coord_flip() +
    scale_x_discrete("") +
    ylab(sprintf("nb : %s", nb)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black")
  print(p)
  return(p)
}
#
# on complète avec les données taxref
faune_taxref <-function() {
  print(sprintf("faune_taxref()"))
  version <- "v10.0"
  dsn <- sprintf("geo/BIOLOVISION/especes_taxref_%s.csv", version)
  especes_taxref.df <-  read.table(dsn, header=TRUE, sep=";", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="", encoding="UTF-8")
#  print(head(taxref.df))
  spdf <- faune_lire()
  df <- spdf@data
  df <- merge(df, especes_taxref.df, by.x="ID_SPECIES", by.y="id", all.x=TRUE, all.y=FALSE, sort=FALSE)
#  print(head(df))
  dsn <- sprintf("geo/BIOLOVISION/AVES_%s.csv",version)
  taxref.df <- df_lire(dsn)
  df <- merge(df, taxref.df, by.x="CD_NOM", by.y="CD_NOM", all.x=TRUE, all.y=FALSE, sort=FALSE)
#  print(head(df,220))
  f <- subset(df, FAMILY_NAME != FAMILLE)
#  print(head(f[,c("NAME_SPECIES", "FAMILY_NAME", "FAMILLE", "NOM_VERN")], 20))
#  stop("****")
  return(df)
}