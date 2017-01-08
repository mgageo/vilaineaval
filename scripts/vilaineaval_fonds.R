# <!-- coding: utf-8 -->
#
# la partie fonds de carte

fonds_raster <- function() {
  require(raster)
  rasterFic <-  sprintf("%s/%s_GP_GM_15_2154.tif", varDir, secteur);
  print(sprintf("fonds_raster() rasterFic: %s", rasterFic))
#  rasterFic <-  sprintf("%s/carte.tif", varDir, secteur);
  imgGM <<-  brick(rasterFic)
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
  img <- crop(imgGM, extent(xmin, xmax, ymin, ymax))
  w <- img@ncols
  h <- img@nrows
  maxW <- 2048
  print(sprintf("fonds_raster() >%s w:%.0f h:%.0f", maxW, w, h))
  if ( maxW > 0 ) {
    if ( w > maxW ) {
      ratio <- w /maxW
      w <- ceiling(w / ratio)
      h <- ceiling(h / ratio)
    }
  }
  if ( interactive() ) {
    dev.new( width = w, height = h, units = "px", pointsize = 12)
  } else {
    xmin <- par("usr")[1]
    xmax <- par("usr")[2]
    ymin <- par("usr")[3]
    ymax <- par("usr")[4]
    w <- xmax - xmin
  }
# on calcule en fonction de la largeur du graphique
# https://chitchatr.wordpress.com/2012/09/18/parusr-is-my-new-friend-for-inserting-legends-in-plots/

  cex <<- round(w / 300, 1)
  plotRGB(img,axes=FALSE,add=FALSE)
  print(sprintf("fonds_raster() x1: %s x2: %s, w: %s cex: %s", xmin, xmax, w, cex))
}
fonds_grille_lire <- function() {
  dsn <- sprintf("%s/nord_lambert_500.json", varDir)
  spdf <- ogr_lire(dsn)
  return(spdf)
}
fonds_grille <- function() {
  dsn <- sprintf("%s/nord_lambert_500.json", varDir)
  spdf <- ogr_lire(dsn)
  plot(spdf, lwd = 4, add=TRUE)
}