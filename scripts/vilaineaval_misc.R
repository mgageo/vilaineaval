# <!-- coding: utf-8 -->
#
# sauvegarde d'un dataframe en csv
df_ecrire <- function(df,f) {
  print(sprintf("df_ecrire() f:%s",f))
  if ( ('point' %in% colnames(df)) ) {
    dfTmp <- subset(df, select = -(point) )
    print(sprintf("df_ecrire() -point"))
  } else {
    print(sprintf("df_ecrire() #point"))
    dfTmp <- df
  }
  write.table(dfTmp, file = f, sep = ";", row.names=FALSE, col.names = TRUE,qmethod = "double", quote = FALSE, fileEncoding="utf8")
}
df_lire <- function(f) {
  print(sprintf("df_lire f:%s",f))
  df <- read.table(f, header=TRUE, sep=";", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="", encoding="UTF-8")
  return(df)
}
# lecture d'un fichier ogr
ogr_lire <- function(dsn) {
  require(rgdal)
  require(rgeos)
  layer <- ogrListLayers(dsn)
  print(sprintf("ogr_lire() %s %s", layer, dsn))
  dfSP <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE, use_iconv=TRUE, encoding="UTF-8")
  dfSP <- spTransform(dfSP, CRS("+init=epsg:2154"))
  return(dfSP)
}
# écriture d'un fichier ogr
ogr_ecrire <- function(spdf, dsn, layer, driver="ESRI Shapefile") {
  require(rgdal)
  nom <- dsn
  nom <- gsub("\\..*$","", nom)
  nom <- gsub(".*/", "", nom)
  writeOGR(spdf, dsn, layer, driver=driver, overwrite_layer=TRUE)
}
extent2spdf <- function(e) {
  library(raster)
  sp <- as(e, 'SpatialPolygons')
  proj4string(sp) <- CRS("+init=epsg:2154")
  spdf <- SpatialPolygonsDataFrame(sp, data.frame(ID=1:length(sp)))
  return(spdf)
}
extent2sp <- function(e) {
  library(raster)
  sp <- as(e, 'SpatialPolygons')
  proj4string(sp) <- CRS("+init=epsg:2154")
  return(sp)
}
zip_dl <- function(les_url, base="") {
  for(i in 1:nrow(les_url) ) {
    url <- les_url$url[i]
    zip <-sub(".*/", "", url)
    zip <- les_url$libelle[i]
    url <- sprintf("%s%s", base, url)
    print(sprintf("zip_dl() url:%s %s %s", url, zip, les_url$libelle[i]))
    file <- sprintf("%s/%s", dl_dir, zip)
    if( ! file.exists(file) ) {
      print(sprintf("zip_dl() dl file:%s", file ))
      download.file(url, file, quiet = FALSE, mode = "wb")
    }
    if( ! file.exists(file) ) {
      print(sprintf("zip_dl() absent : file:%s", file))
	    stop()
    }
    zip_dir  <-sub("\\..*$", "", zip)
    zip_dir <- sprintf("%s/%s", dl_dir, zip_dir)
    if( ! file.exists(zip_dir) ) {
      print(sprintf("zip_dl() absent : zip_dir:%s", zip_dir))
      unzip(file, overwrite = TRUE,junkpaths = TRUE, exdir = zip_dir, unzip = "internal", setTimes = TRUE)
    } else {
      print(sprintf("zip_dl() present : zip_dir:%s", zip_dir))
    }
    files <- list.files(zip_dir, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
    if ( length(files) != 1) {
      stop("zip_dl() .shp")
      next;
    }
    dsn <- files[1]
    layers <- ogrListLayers(dsn)
    print(sprintf("zip_dl() layers:%s", layers))
#    print(sprintf("inpn() info:%s", OGRSpatialRef(dsn,layers)))
    sp <- readOGR(dsn,layers)
    print(summary(sp))
  }
}
especes_camel <- function(df) {
  df$espece <- gsub(",.*", "", df$TAXO_VERNACUL)
  df$espece_c <- camel2(df$espece)
  df <- df[with(df, order(espece_c)), ]
}
# http://stackoverflow.com/questions/11672050/how-to-convert-not-camel-case-to-camelcase-in-r
camel2 <- function(x) {
  x <- iconv(x, to='ASCII//TRANSLIT')
  gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", x, perl = TRUE)
}
camel3 <- function(x) {
  x <- iconv(x, to='ASCII//TRANSLIT')
  x <- gsub("[^[:alnum:]]", " ", x, perl = TRUE)
  x <- tolower(x)
}