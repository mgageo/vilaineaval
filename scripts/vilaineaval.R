# <!-- coding: utf-8 -->
#
# quelques fonction pour VilaineAval
# auteur: Marc Gauthier
#
#
#243500139;communauté d’agglomération de Rennes Métropole
#243500675;communauté de communes du Pays d’Aubigné
#243500659;communauté de communes du Pays de Châteaugiron
#243500774;communauté de communes du Pays de Liffré
#243500667;communauté de communes du Val d’Ille
#
# http://stackoverflow.com/questions/19226816/how-can-i-view-the-source-code-for-a-function
# http://informatique-mia.inra.fr/r4ciam/node/197 la ligne de commande
mga  <- function() {
  source("geo/scripts/vilaineaval.R");
}
nord <- function() {
  varDir <<- sprintf("%s/bvi35/CouchesVilaineAval", Drive);
  secteur <<- "Nord"
  nrows <<- 3000
  ncols <<- 3250
}
sud <- function() {
  varDir <<- sprintf("%s/bvi35/CouchesVilaineAvalSud", Drive);
  Secteur <<- "Sud"
  secteur <<- "sud"
}
#
# production d'une carto du territoire d'études, version Vilaine Aval Nord
territoire <- function() {
  require(raster)
  mailleFic <- sprintf("%s/territoire_%s.pdf", texDir, secteur);
  pdf(mailleFic, width=8, height=7)
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  rasterFic <-  sprintf("%s/%s_GP_GM_15_2154.tif", varDir, Secteur);
  rasterFic <-  sprintf("%s/carte.tif", varDir, Secteur);
  img <<-  brick(rasterFic)
  dsn <- sprintf("%s/%s.shp", varDir, secteur)
  territoire.spdf <- ogr_lire(dsn)
  emprise <- as.vector (bbox(territoire.spdf))
  emprise <- englobante(emprise, marge=500, ratio=0, grille=3)
  xmin <- emprise[1]
  ymin <- emprise[2]
  xmax <- emprise[3]
  ymax <- emprise[4]
  mailleImg <- crop(img, extent(xmin, xmax, ymin, ymax))
  plotRGB(mailleImg,axes=FALSE,add=FALSE)
  plot(territoire.spdf, lwd = 4, add=TRUE)
  dsn <- sprintf("%s/bvi35/CouchesPaysderennes/MNIE2014_Loc.shp", Drive);
  mnie.spdf <- ogr_lire(dsn)
  color <- rgb(0,1,0,alpha=0.3)
  plot(mnie.spdf, lwd = 3, col = color, add=TRUE)
  text(coordinates(mnie.spdf), labels=mnie.spdf@data$CODE_MNIE, font = 2, cex= 1, col = "darkgreen")
  scalebar(1000, type='bar', divs=2)
  dev.off()
  print(sprintf("territoire() mailleFic: %s", mailleFic))
}
#
# production d'une carto du territoire d'études, version Vilaine Aval Nord
territoire_nord <- function() {
  require(raster)
  mailleFic <- sprintf("%s/territoire_%s.pdf", texDir, secteur);
  pdf(mailleFic, width=8, height=7)
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  rasterFic <-  sprintf("%s/%s_GP_GM_15_2154.tif", varDir, secteur);
#  rasterFic <-  sprintf("%s/carte.tif", varDir, secteur);
  img <<-  brick(rasterFic)
  dsn <- sprintf("%s/%s.shp", varDir, secteur)
  territoire.spdf <- ogr_lire(dsn)
  dsn <- sprintf("%s/nord_lambert_500.json", varDir)
  spdf <<- ogr_lire(dsn)
  emprise <- as.vector (bbox(spdf))
  emprise <- englobante(emprise, marge=500, ratio=0, grille=3)
  xmin <- emprise[1]
  ymin <- emprise[2]
  xmax <- emprise[3]
  ymax <- emprise[4]
  mailleImg <- crop(img, extent(xmin, xmax, ymin, ymax))
  plotRGB(mailleImg,axes=FALSE,add=FALSE)
  plot(spdf, lwd = 4, add=TRUE)
  text(coordinates(spdf), labels=spdf@data$NUMERO, font = 2, cex= 1,)
  dsn <- sprintf("%s/bvi35/CouchesPaysderennes/MNIE2014_Loc.shp", Drive);
  mnie.spdf <- ogr_lire(dsn)
  color <- rgb(0,1,0,alpha=0.3)
  plot(mnie.spdf, lwd = 3, col = color, add=TRUE)
  text(coordinates(mnie.spdf), labels=mnie.spdf@data$CODE_MNIE, font = 2, cex= 1, col = "darkgreen")
  scalebar(1000, type='bar', divs=2)
  dev.off()
  print(sprintf("territoire() mailleFic: %s", mailleFic))
}

territoire_grille <- function() {
  varDir <<- sprintf("%s/bvi35/CouchesVilaineAval", Drive);
  imgFic <- sprintf("%s/territoire_stat.jpg", varDir);
  dsn <- sprintf("%s/Nord.shp", varDir)
  territoire.spdf <- ogr_lire(dsn)
  pas <- 500
  print(sprintf("territoire_grille() surface:%d ha", as.integer(rgeos::gArea(territoire.spdf)/pas) ))
  emprise <- as.vector(bbox(territoire.spdf))
  print(sprintf("territoire_grille() emprise:%s %s %s %s", emprise[1], emprise[2], emprise[3], emprise[4]))
  largeur <- round((emprise[3] -  emprise[1]) / pas)
  hauteur <- round((emprise[4] -  emprise[2]) / pas)
  print(sprintf("territoire_grille() %s %s", largeur, hauteur))
#  emprise <- englobante(emprise, ratio=0, grille=1, marge=pas/2)
  grille_lambert2(dsn, pas)

}
territoire_grille0 <- function() {
  require(rgeos)
  varDir <<- sprintf("%s/bvi35/CouchesVilaineAval", Drive);
  imgFic <- sprintf("%s/territoire_stat.jpg", varDir);
  dsn <- sprintf("%s/Nord.shp", varDir)
  territoire.spdf <- ogr_lire(dsn)
  pas <- 500
  print(sprintf("territoire_grille() surface:%d ha", as.integer(rgeos::gArea(territoire.spdf)/pas) ))
  emprise <- as.vector(bbox(territoire.spdf))
  print(sprintf("territoire_grille() emprise:%s %s %s %s", emprise[1], emprise[2], emprise[3], emprise[4]))
  largeur <- round((emprise[3] -  emprise[1]) / pas)
  hauteur <- round((emprise[4] -  emprise[2]) / pas)
  print(sprintf("territoire_grille() %s %s", largeur, hauteur))
  emprise <- englobante(emprise, ratio=0, grille=1, marge=pas/2)
  grille_lambert(territoire.spdf, pas)

}

# lecture des MNIE
mnie_lire <- function(dsn) {
  require(rgdal)
  require(rgeos)
  layer <- ogrListLayers(dsn)
  dfSP <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE)
  dfSP <- spTransform(dfSP, CRS("+init=epsg:2154"))
  return(dfSP)
}
epci <- function() {
  library(cartography)
  library(raster)
  les_departements <- "35"
  departement.spdf <- osm_departement_lire(les_departements)
  les_epcis <- c("243500139","243500675","243500659","243500774","243500667")
  epci.spdf <- osm_epci_lire(les_epcis)
  summary(epci.spdf)
  class(epci.spdf)
  nrow(epci.spdf)
  length(epci.spdf)
  nrow(na.omit(epci.spdf))
  epci.spdf@data$nom_epci <- iconv(epci.spdf@data$nom_epci, "UTF-8")
  opar <- par(mar = c(0,0,1.2,0))
  # Layout plot
  layoutLayer(title = "Carte des epci", # title of the map
              author = "",  # no author text
              sources = "", # no source text
              scale = NULL, # no scale
              col = NA, # no color for the title box
              coltitle = "black", # color of the title
              frame = FALSE,  # no frame around the map
              bg = "#A6CAE0", # background of the map
              extent = departement.spdf) # set the extent of the map
  les_couleurs <- c('#fee6ce','#fdae6b','#e6550d')
  plot(departement.spdf, col  = les_couleurs[1], border=NA, add=FALSE)
  plot(epci.spdf, col = les_couleurs[2], border = "white", lwd=1, add=TRUE)
  labelLayer(spdf = epci.spdf, # SpatialPolygonsDataFrame used to plot he labels
             df = epci.spdf@data, # data frame containing the lables
             txt = "nom_epci", # label field in df
             col = "#690409", # color of the labels
             cex = 0.9, # size of the labels
             font = 2) # label font
}
couches_ogr2ogr <- function() {
  gdal_lib()
  les_shp <- read.table(text="dsn|couche
D:/bvi35/CouchesVilaineAval/izh_sage_vilaine.geojson|izh_sage_vilaine.json
D:/web.var/geo/PaysDeRennes/MNIE2014_Hab.shp|MNIE2014_Hab.json
D:/web.var/geo/PaysDeRennes/MNIE2014_Loc.shp|MNIE2014_Loc.json
", header=TRUE, sep="|", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="")
  for(i in 1:nrow(les_shp) ) {
    dsn <- les_shp$dsn[i]
    couche <- les_shp$couche[i]
    couche <- sprintf("%s/%s", varDir, couche)
    print(sprintf("couches_ogr2ogr() couche: %s", couche))
    if ( file.exists(couche) ) {
      next;
    }
    layer <- ogrListLayers(dsn)
    spdf <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE, use_iconv=TRUE, encoding="UTF-8")
    spdf <- spTransform(spdf, CRS("+init=epsg:4326"))
    writeOGR(spdf, couche, layer=layer, driver="GeoJSON", overwrite_layer=TRUE)
  }
}
# http://www.faune-bretagne.org/index.php?m_id=94&p_c=1&p_cc=-1&sp_tg=1&sp_DChoice=range&sp_DFrom=01.12.2016&sp_DTo=15.12.2016&sp_DSeasonFromDay=1&sp_DSeasonFromMonth=1&sp_DSeasonToDay=31&sp_DSeasonToMonth=12&sp_DOffset=5&speciesFilter=&sp_S=462&sp_SChoice=category&sp_Cat%5Bnever%5D=1&sp_Cat%5Bveryrare%5D=1&sp_Cat%5Brare%5D=1&sp_Cat%5Bunusual%5D=1&sp_Cat%5Bescaped%5D=1&sp_Cat%5Bcommon%5D=1&sp_Cat%5Bverycommon%5D=1&sp_Family=1&sp_cC=0010&sp_cCO=0&sp_CommuneCounty=36&sp_Commune=13859&sp_Info=&sp_P=0&sp_PChoice=coord&sp_Coord%5BW%5D=-1.775795&sp_Coord%5BS%5D=48.060601&sp_Coord%5BE%5D=-1.711859&sp_Coord%5BN%5D=48.105037&sp_Grid=1&sp_AltitudeFrom=-5&sp_AltitudeTo=387&sp_CommentValue=&sp_ObserverNameString=&sp_ObserverName=0&sp_OnlyAH=0&sp_Ats=-00000&sp_F=0&sp_FChoice=list&sp_FDisplay=DATE_PLACE_SPECIES&sp_DFormat=DESC&sp_FOrderListSpecies=ALPHA&sp_FListSpeciesChoice=DATA&sp_DateSynth=15.12.2016&sp_FOrderSynth=ALPHA&sp_FGraphChoice=DATA&sp_FGraphFormat=auto&sp_FAltScale=250&sp_FAltChoice=DATA&sp_FMapFormat=none&submit=Chercher

territoire_emprise <- function() {
  dsn <- sprintf("%s/%s.shp", varDir, secteur)
  spdf <- ogr_lire(dsn)
  emprise <- as.vector (bbox(spdf))
  emprise <- englobante(emprise, marge=500, ratio=0, grille=1)
  xmin <- emprise[1]
  ymin <- emprise[2]
  xmax <- emprise[3]
  ymax <- emprise[4]
  print(sprintf("territoire_emprise() %s %s", ymax-ymin, xmax-xmin))
  spdf <- spTransform(spdf, CRS("+init=epsg:4326"))
  emprise <- as.vector (bbox(spdf))
  print(emprise)
}
#
# génération des fichiers pour tex
tex_faune <-function() {
  if ( exists("faune.spdf") ) {
    rm("faune.spdf")
  }
  faune_donnees <<- "txt"
  les_tex <- read.table(text="fonction|mode|args
faune_stat_champ|ggplot|FAMILY_NAME
faune_stat_champ|ggplot|FAMILLE
faune_stat_champ|plotgg(6,12)|NAME_SPECIES
faune_stat_champ|ggplot|ORDRE
faune_stat_champ|ggplot|d
faune_stat_champ|ggplot|observateur
faune_stat_champ|print|PLACE
faune_stat_champ|plotgg(4,1)|PRECISION
faune_carte|carte|
", header=TRUE, sep="|", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="")
  tex_les(les_tex)
}
#
# génération des fichiers pour tex
tex_faune_prec <-function() {
  if ( exists("faune.spdf") ) {
    rm("faune.spdf")
  }
  faune_donnees <<- "ogr"
  les_tex <- read.table(text="fonction|mode|args
faune_prec_stat_champ|plotgg(4,8)|FAMILY_NAME
faune_prec_stat_champ|plotgg(4,8)|annee
faune_prec_stat_champ|plotgg(4,4)|mois
faune_prec_stat_champ|plotgg(6,16)|PLACE
faune_prec_stat_champ|plotgg(4,1)|PRECISION
", header=TRUE, sep="|", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="")
  tex_les(les_tex)
}
# génération des fichiers pour tex
tex_serena <-function() {
  les_tex <- read.table(text="fonction|mode|args
serena_stat_champ|plotgg(4,8)|espece
serena_stat_champ|ggplot|OBSE_DATE
serena_stat_champ|ggplot|OBSV_LIBEL
serena_carte|carte|
serena_carte_stat|carte|
", header=TRUE, sep="|", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="")
  tex_les(les_tex)
}
tex_parcours <-function() {
  les_tex <- read.table(text="fonction|mode|args
parcours_carte|carte|
parcours_stat_champ|ggplot|prenom
parcours_stat_email_longueur|ggplot|
", header=TRUE, sep="|", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="")
  tex_les(les_tex)
}
faune <- function() {
  tex_faune()
}
parcours <- function() {
  parcours_get()
  tex_parcours()
}
serena <- function() {
  serena_maille()
  serena_atlas_especes()
#  serena_carte()
  tex_serena()
}
#
# un peu de nettoyage
graphics.off()
#
# les commandes permettant le lancement
Drive <- substr( getwd(),1,2)
baseDir <- sprintf("%s\\web", Drive)
bviDir <<- sprintf("%s/bvi35", Drive);
varDir <<- sprintf("%s/bvi35/CouchesVilaineAval", Drive);
serenaDir <<- sprintf("%s/geo/VILAINEAVAL", baseDir);
fauneDir <<- sprintf("%s/geo/VILAINEAVAL", baseDir);
secteur <<- 'Nord'
sud()
nord()
texDir <- sprintf("%s\\web\\geo\\VILAINEAVAL", Drive)
pdfFic <- sprintf("%s\\outputFic.pdf", texDir)
pngFic <- sprintf("%s\\outputFic.png", texDir)
pas <- 500
setwd(baseDir)
source("geo/scripts/misc.R");
source("geo/scripts/misc_faune.R");
source("geo/scripts/misc_geo.R");
source("geo/scripts/misc_grille.R");
source("geo/scripts/misc_tex.R");
source("geo/scripts/misc_mnhn.R");
source("geo/scripts/couches_gdal.R");
#source("geo/scripts/couches_osm.R");
source("geo/scripts/vilaineaval_cartes.R");
source("geo/scripts/vilaineaval_faune.R");
source("geo/scripts/vilaineaval_fonds.R");
source("geo/scripts/vilaineaval_misc.R");
source("geo/scripts/vilaineaval_parcours.R");
source("geo/scripts/vilaineaval_serena.R");
DEBUG <- FALSE
if ( interactive() ) {
  print(sprintf("vilaineaval.R interactive"))
} else {
  print(sprintf("vilaineaval.R console"))
}
# l10n_info
#
# options(encoding="utf-8")
# options(encoding="native.enc")
# options(stringsAsFactors=FALSE)
#situation()
#territoire_emprise()
#territoire_grille0()
#territoire_nord()
#cartes_mailles_tex()
#couches_ogr2ogr()
#faune_plot()
#faune_stat()
tex_faune()
#tex_faune_prec()
#faune_stat_champ('PLACE');
#faune()
#serena_lire()
#serena_maille()
#serena_carte_stat()
#serena_stat_champ()
#serena_atlas_especes()
#serena()
#tex_serena()
#parcours_stat_email_longueur()
#parcours_carte()
#tex_parcours()
#parcours()
#mnhn_taxref_aves()
#faune_taxref()