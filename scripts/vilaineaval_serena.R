# <!-- coding: utf-8 -->
#
# la partie données serena
# ===============================================================
#
# lecture des données, export de serena en format excel
serena_lire <- function() {
  library("xlsx")
  library("raster")
  dsn <- sprintf("%s/serena_20161222.xls", serenaDir)
  dsn <- sprintf("%s/serena_20170102.xls", serenaDir)
  df <- read.xlsx2(dsn, 1, header=TRUE, colClasses=NA)
  df$OBSE_LON <- as.numeric(gsub(",", ".", df$OBSE_LON))
  df$OBSE_LAT <- as.numeric(gsub(",", ".", df$OBSE_LAT))
#  print(head(df))
#  print(sapply(df, class))
  coordinates(df) = ~ OBSE_LON + OBSE_LAT
  spdf <- SpatialPointsDataFrame(df,data.frame(df[,]))
  proj4string(spdf) <- CRS("+init=epsg:4326")
  spdf <- spTransform(spdf, CRS("+init=epsg:2154"))
  return(spdf)
}
#
# normalisation du champ OBSE_PLACE
# ajout de la maille déterminée à partir de OBSE_PLACE
serena_place <- function() {
  print(sprintf("serena_place()"))
  spdf <- serena_lire()
  spdf <- subset(spdf, ! grepl("^Hors", spdf@data$OBSE_PLACE) )
  spdf <- subset(spdf, ! grepl("^35650", spdf@data$OBSE_PLACE) )
  spdf@data$OBSE_PLACE <- gsub("^PVA", "", spdf@data$OBSE_PLACE)
  spdf@data$OBSE_PLACE <- toupper(spdf@data$OBSE_PLACE)
  spdf@data$OBSE_PLACE <- gsub("(^\\d+[A-Z]).*", "\\1", spdf@data$OBSE_PLACE)
  spdf@data$place <- gsub("[^0-9]", "", spdf@data$OBSE_PLACE)
  spdf@data$place <- gsub("^0", "", spdf@data$place)
  return(spdf)
}
# détermination de la maille à partir des coordonnées géographiques
serena_maille <- function() {
  library("raster")
  library("sqldf")
  spdf <- serena_place()
  zone.spdf <- fonds_grille_lire()
#  print(head(zone.spdf@data))
  spdf@data$NUMERO <- over(spdf, zone.spdf)$NUMERO
  df <-spdf@data
  inconnuDF <- subset(df, is.na(df$NUMERO))
  if ( length(inconnuDF$NUMERO) > 0 ) {
    print(sprintf("donnees_maille() maille invalide"))
    print(head(inconnuDF))
#    stop("serena_maille()")
  }
  df <- df[!(is.na(df$NUMERO)),]
  print(head(df))
  mailles_especes.df <- sqldf("SELECT NUMERO, TAXO_VERNACUL, COUNT(*) as nb FROM df GROUP BY NUMERO, TAXO_VERNACUL;")
  mailles_especes.df$TAXO_VERNACUL <- iconv(mailles_especes.df$TAXO_VERNACUL, "UTF-8")
  champs <- c("NUMERO", "TAXO_VERNACUL")
#  mailles_especes.df <- data.frame(table(df[, champs]))
#  mailles_especes.df$espece_c <- camel2(mailles_especes.df$TAXO_VERNA)
#  mailles_especes.df <- mailles_especes.df[with(mailles_especes.df, order("NUMERO")), ]
#  print(head(mailles_especes.df))
#  stop("****")
  dsn <- sprintf("%s/serena_mailles_especes.csv", varDir)
  df_ecrire(mailles_especes.df, dsn);
  print(sprintf("serena_maille() mailles_especes nb: %s", nrow(mailles_especes.df)))
}
serena_mailles_especes_lire <- function() {
  print(sprintf("serena_mailles_especes_lire()"))
  dsn <- sprintf("%s/serena_mailles_especes.csv", varDir)
  mailles_especes.df <<- df_lire(dsn);
  mailles_especes.df$TAXO_VERNACUL <- iconv(mailles_especes.df$TAXO_VERNACUL, "UTF-8")
  mailles_especes.df <- especes_camel(mailles_especes.df)
  print(head(mailles_especes.df))
  return(mailles_especes.df)
}
#
# détermination des espèces
serena_especes_lire <- function() {
  print(sprintf("serena_especes_lire()"))
  dsn <- sprintf("%s/serena_mailles_especes.csv", varDir)
  mailles.df <<- df_lire(dsn);
  mailles.df$TAXO_VERNACUL <- iconv(mailles.df$TAXO_VERNACUL, "UTF-8")
#  mailles.df <<- especes_camel(mailles.df)
  especes <- mailles.df[!duplicated(mailles.df$TAXO_VERNACUL), c('TAXO_VERNACUL') ]
  especes.df <- as.data.frame(especes)
  colnames(especes.df) <- c("TAXO_VERNACUL")
  especes.df$espece <- gsub(",.*", "", especes.df$TAXO_VERNACUL)
  especes.df$espece_c <- camel2(especes.df$espece)
  especes.df <- especes.df[with(especes.df, order(espece_c)), ]
#  print(head(especes.df))
  return(especes.df)
}
#
# carte
serena_carte <- function() {
  print(sprintf("serena_carte()"))
  spdf <- serena_place()
  spdf@data$couleur <- "black"
  spdf@data$forme <- 20
  fonds_raster()
  fonds_grille()
  zone.spdf <- fonds_grille_lire()
#  print(head(zone.spdf@data))
  spdf@data$NUMERO <- over(spdf, zone.spdf)$NUMERO
  spdf@data[is.na(spdf@data)] <- "0"
  spdf@data[spdf@data$place == spdf@data$NUMERO, "couleur"] <- "orange"
#  plot(spdf, add=TRUE, col='grey', pch=spdf@data$forme, cex=3)
  print(head(spdf@data[spdf@data$couleur == "black", c("OBSE_ID", "TAXO_VERNACUL", "OBSE_PLACE", "NUMERO")], 60))
  text(coordinates(spdf), labels=spdf@data$OBSE_PLACE, cex=1, col=spdf@data$couleur, font=2)
}
#
# carte avec stat
serena_carte_stat <- function() {
  print(sprintf("serena_carte_stat()"))
  spdf <- serena_place()
  zone.spdf <- fonds_grille_lire()
  spdf@data$NUMERO <- over(spdf, zone.spdf)$NUMERO
  spdf@data[is.na(spdf@data)] <- "0"
  df <- spdf@data
#  print(head(df[df$NUMERO == 59, c("OBSE_ID", "TAXO_VERNACUL", "OBSE_PLACE", "NUMERO")], 60))
#  stop("***")
  champs <- c("NUMERO")
  donnees.df <- data.frame(table(df[, champs]))
  colnames(donnees.df) <- append(champs, "donnees")
  print(head(donnees.df))
  unique.df <- unique(df[c("NUMERO", "OBSE_PLACE")])
  print(head(unique.df))
  champs <- c("NUMERO")
  parcours.df <- data.frame(table(unique.df[, champs]))
  colnames(parcours.df) <- append(champs, "parcours")
  print(head(parcours.df))
#  stop("***")
  unique.df <- unique(df[c("NUMERO", "TAXO_VERNACUL")])
  print(head(unique.df))
  champs <- c("NUMERO")
  especes.df <- data.frame(table(unique.df[, champs]))
  colnames(especes.df) <- append(champs, "especes")
  print(head(especes.df))
  zone.df <- zone.spdf@data
  zone.df <- merge(x=zone.df, y=donnees.df, by.x="NUMERO", by.y="NUMERO", all.x=TRUE, all.y=FALSE, sort=FALSE)
  zone.df <- merge(x=zone.df, y=parcours.df, by.x="NUMERO", by.y="NUMERO", all.x=TRUE, all.y=FALSE, sort=FALSE)
  zone.df <- merge(x=zone.df, y=especes.df, by.x="NUMERO", by.y="NUMERO", all.x=TRUE, all.y=FALSE, sort=FALSE)
  zone.df[is.na(zone.df)] <- ""
  zone.df$libelle <- sprintf("%s\n%s\n%s", zone.df$parcours, zone.df$especes, zone.df$donnees)
  print(head(zone.df))
  zone.spdf <-merge_spdf_df(zone.spdf, zone.df, "NUMERO", "NUMERO")
#  stop("***")
  fonds_raster()
  fonds_grille()
  text(coordinates(zone.spdf), labels=zone.spdf$libelle, cex=1, font=2, col="darkred")
}
#
# fusion d'un spdf et d'un df avec préservation de l'ordre des lignes
merge_spdf_df <-function(spdf, df, spdf.key, df.key) {
# Création d'une variable temporaire pour récuperer l'ordre initial après le merge (Joel Gombin)
  spdf@data$nrow <- 1:nrow(spdf@data)
  spdf@data <- merge(x=spdf@data, y=df, by.x=spdf.key, by.y=df.key, all.x=TRUE, all.y=FALSE, sort=FALSE)
  spdf@data <- spdf@data[order(spdf@data$nrow, na.last = TRUE),]
  return(spdf)
}
#
# pour faire des stat sur un champ
serena_stat_champ <- function(champ = "TAXO_VERNACUL") {
  library(ggplot2)
  spdf <- serena_place()
  df <- spdf@data
  df <- especes_camel(df)
#  print(df$PLACE)
#  print(sapply(df, class))
#  print(summary(df))
# http://stackoverflow.com/questions/32761875/sqldf-changes-a-numeric-column-into-character-one-when-ordered-by-it
#  sql <- sprintf("SELECT %s as critere, COUNT(*) as nb FROM df GROUP BY %s;", champ, champ)
#  nb.df <- sqldf(sql,method = c("character"))
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
# la carte pour une espèce
serena_atlas_espece <- function(espece, espece_c, titre=TRUE) {
  print(sprintf("serena_atlas_espece() espece:%s espece_c:%s", espece, espece_c))
  # le fond de carte
  plot(mailles.spdf, border = NA, col = NA, bg = "white")
  espece.df <- mailles_especes.df[mailles_especes.df$espece_c == espece_c,]
  if ( nrow(espece.df) == 0 ) {
    print(head(mailles_especes.df))
    stop("***")
    return()
  }
  if ( titre ) {
    title(espece)
  }
  espece.df$couleur <- 'red'
# on fait une copie de travail
  mailles <- mailles.spdf
#  print(head(mailles@data))
#  print(head(espece.df))
  mailles@data <- data.frame(mailles.spdf@data, espece.df[match(mailles.spdf@data[, "NUMERO"], espece.df[, "NUMERO"]), ])
#  print(head(mailles@data))
  plot(mailles, col = mailles@data$couleur, border = "black", lwd=0.4, add=TRUE)
}
#
# l'atlas pour toutes les espèces, la version Tex
serena_atlas_especes <- function() {
  print(sprintf("serena_atlas_especes()"))
  texFic <- sprintf("%s\\%s", texDir, "serena_cartes_espece.tex")
  TEX <- file(texFic, encoding="UTF-8")
  tex <- "% <!-- coding: utf-8 -->"
  especes.df <<- serena_especes_lire()
  mailles.spdf <<- fonds_grille_lire()
  mailles_especes.df <<- serena_mailles_especes_lire()
  i_plots <- 0
  for ( i in 1:nrow(especes.df) ) {
    espece <- especes.df[i,"espece"]
    espece_c <- especes.df[i,"espece_c"]
    tex <- append(tex, sprintf("\\subsection{%s}", espece))
    tex <- append(tex, sprintf("\\includegraphics[width=\\malargeurgraphique]{atlas_serena/%s.pdf}", espece_c))
    especeFic <- sprintf("%s\\atlas_serena\\%s.pdf", texDir, espece_c)
    if ( 2 == 2 ) {
      pdf(especeFic, width=4, height=4)
      par(mar=c(0,0,0,0), oma=c(0,0,0,0))
      serena_atlas_espece(espece, espece_c, titre = FALSE)
      dev.off()
    }
  }
  write(tex, file = TEX, append = FALSE)
  print(sprintf("serena_atlas_especes() texFic: %s", texFic))

}