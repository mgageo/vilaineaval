# <!-- coding: utf-8 -->
#
# la partie cartes
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# la carte pour une maille
# version carte de situation
# fonds de carte type scan25
cartes_maille_gm <- function(maille, titre=TRUE) {
  print(sprintf("cartes_maille_gm() maille:%s", maille))
  spdf <-  mailles.spdf[ mailles.spdf@data$NUMERO == maille, ]
  if ( nrow(spdf) == 0 ) {
    print(head(mailles.spdf@data$NUMERO))
    stop("***")
  }
  emprise <- as.vector (bbox(spdf))
  emprise <- englobante(emprise, marge=500, ratio=0, grille=3)
  xmin <- emprise[1]
  ymin <- emprise[2]
  xmax <- emprise[3]
  ymax <- emprise[4]
#  print(sprintf("cartes_maille() maille:%s ullr: %.0f %.0f %.0f %.0f lh: %.0fx%.0f", maille, xmin, ymax, xmax, ymin,xmax-xmin,ymax-ymin))
  mailleImg <- crop(imgGM, extent(xmin, xmax, ymin, ymax))
  plotRGB(mailleImg,axes=FALSE,add=FALSE, maxpixels=2000000)
  plot(spdf, lwd = 3, add=TRUE)
  scalebar(500, type='bar', divs=2)
}
# la carte pour une maille
# version détaillée
# fonds de carte type pva
# données représentées
cartes_maille_pva <- function(maille, titre=TRUE) {
  print(sprintf("cartes_maille_pva() maille:%s", maille))
  spdf <-  mailles.spdf[ mailles.spdf@data$NUMERO == maille, ]
  print(sprintf("cartes_maille_pva() mailles nrow:%s", nrow(spdf)))
  if ( nrow(spdf) == 0 ) {
    print(head(mailles.spdf@data$NUMERO))
    stop("***")
  }
  emprise <- as.vector (bbox(spdf))
  emprise <- englobante(emprise, marge=200, ratio=0, grille=3)
  xmin <- emprise[1]
  ymin <- emprise[2]
  xmax <- emprise[3]
  ymax <- emprise[4]
#  print(sprintf("cartes_maille() maille:%s ullr: %.0f %.0f %.0f %.0f lh: %.0fx%.0f", maille, xmin, ymax, xmax, ymin,xmax-xmin,ymax-ymin))
  e <- extent(xmin, xmax, ymin, ymax)
  mailleImg <- crop(imgPVA, e)
  plotRGB(mailleImg, axes=FALSE, add=FALSE, maxpixels=10000000)
  plot(spdf, lwd = 3, add=TRUE)
  scalebar(500, type='bar', divs=2)
#  print(head(donnees.spdf@data))
}
#
# lecture des mailles  et fonds de carte
cartes_mailles_lire <- function() {
  library(raster)
  print(sprintf("cartes_mailles_lire()"))
  dsn <- sprintf("%s/nord_lambert_500.json", varDir)
  mailles.spdf <<- ogr_lire(dsn);
#  print(head(mailles.spdf@data))
  if ( ! exists("imgGM") ) {
    rasterFic <- sprintf("%s/Nord_GP_GM_16_2154.tif", varDir)
    print(sprintf("cartes_mailles_lire() rasterFic:%s", rasterFic))
    imgGM <<-  brick(rasterFic)
  }
  if ( ! exists("imgPVA") ) {
    rasterFic <- sprintf("%s/Nord_GP_PVA_16_2154.tif", varDir)
    print(sprintf("cartes_mailles_lire() rasterFic:%s", rasterFic))
    imgPVA <<-  brick(rasterFic)
  }
#  print(head(mailles.spdf))
}
#
# les cartes pour l'ensemble des mailles, la version Tex
cartes_mailles_tex <- function() {
  texFic <- sprintf("%s\\%s", texDir, "cartes_mailles.tex")
  TEX <- file(texFic, encoding="UTF-8")
  tex <- "% <!-- coding: utf-8 -->"
  cartes_mailles_lire()
  i_plots <- 0
  test <- 3
  for ( i in 1:nrow(mailles.spdf) ) {
    maille <- mailles.spdf@data[i,"NUMERO"]
#    print(sprintf("cartes_mailles_tex() maille:%s", maille))
    tex <- append(tex, sprintf("\\subsection{Maille %s}", maille))
    tex <- append(tex, sprintf("\\begin{tabular}{ll}"))
    tex <- append(tex, sprintf("\\includegraphics[width=\\malargeurgraphique]{cartes/%s_gm.pdf}", maille))
    mailleFic <- sprintf("%s\\cartes\\%s_gm.pdf", texDir, maille)
    if ( test == 2 ) {
      pdf(mailleFic, width=4, height=4)
      par(mar=c(0,0,0,0), oma=c(0,0,0,0))
      cartes_maille_gm(maille, titre = FALSE)
      dev.off()
    }
    tex <- append(tex, sprintf("&"))

    tex <- append(tex, sprintf("\\includegraphics[width=\\malargeurgraphique]{cartes/%s_pva.pdf}", maille))
    mailleFic <- sprintf("%s\\cartes\\%s_pva.pdf", texDir, maille)
    if ( test == 2) {
      pdf(mailleFic, width=4, height=4)
      par(mar=c(0,0,0,0), oma=c(0,0,0,0))
      cartes_maille_pva(maille, titre = FALSE)
      dev.off()
    }
    tex <- append(tex, sprintf("\\end{tabular}"))
    tex <- append(tex, sprintf("\\newpage"))
    if ( i > 100 ) {
      break
    }
  }
  write(tex, file = TEX, append = FALSE)
  print(sprintf("cartes_mailles_tex() texFic: %s", texFic))
}