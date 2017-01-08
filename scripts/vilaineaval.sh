#!/bin/sh
# <!-- coding: utf-8 -->
#T Vilaine Aval
# auteur: Marc Gauthier
[ -f ../win32/scripts/misc.sh ] && . ../win32/scripts/misc.sh
[ -f ../win32/scripts/misc_pkg.sh ] && . ../win32/scripts/misc_pkg.sh
#f CONF:
CONF() {
  LOG "CONF debut"
  ENV
  CFG="VILAINEAVAL"
  [ -d "${CFG}" ] || mkdir "${CFG}"
  LOG "CONF fin"
}

#F e: edition des principaux fichiers
e() {
  LOG "e debut"
  E scripts/vilaineaval.sh
  for f in scripts/vilaineaval*.R ; do
    E $f
  done
  LOG "e fin"
}
#f wm:
wm() {
  LOG "wm debut"
  WM d:/web/bv d:/web.heb/bv &
  LOG "wm fin"
}
#f T:
T() {
  LOG "T debut"
  cd /d/tmp
  _ENV_im
  convert carte.pdf carte.png
  LOG "T fin"
}
#f JOUR:
JOUR() {
  LOG "JOUR debut"
  perl scripts/biolovision.pl RvaExport
  LOG "JOUR fin"
}
#F FB_pl: recup des donnees Faune Bretagne
FB_pl() {
  LOG "FB_pl debut"
  perl scripts/biolovision.pl RvaExport
  wc -l ${CFG}/donnees.txt
  LOG "FB_pl fin"
}
#F FB_R: traitement des donnees Faune Bretagne
FB_R() {
  LOG "FB_R debut"
  _ENV_R
#  R --help
#  ( cd ..; R --vanilla   -e "source('geo/scripts/miscInstall.R');" )
  ( cd ..; R --vanilla   -e "source('geo/scripts/vilaineaval.R'); faune()" )
  ( cd ${CFG}/images; explorer . &)
  LOG "FB_R fin"
}
#f FB_TEX: mise en page
FB_TEX() {
  LOG "FB_TEX debut"
  _ENV_tex
  file=faune
  (
    cd ${CFG}
    rm $file*aux
    rm $file*bbl
    pdflatex ${file}.tex
    pdflatex ${file}.tex
  )
  LOG "FB_TEX fin"
}
#F PARCOURS_R:
PARCOURS_R() {
  LOG "PARCOURS_R debut"
  _ENV_R
  ( cd ..; R --vanilla   -e "source('geo/scripts/vilaineaval.R'); parcours()" )
  ( cd ${CFG}/images; explorer . &)
  LOG "PARCOURS_R fin"
}
#F GIT: pour mettre à jour le dépot git
GIT() {
  LOG "GIT debut"
  _git_lst
  bash ../win32/scripts/git.sh INIT
  LOG "GIT fin"
}
#f _git_lst: la liste des fichiers pour le dépot
_git_lst() {
  Local="${DRIVE}/web/geo";  Depot=vilaineaval; Remote=github
  export Local
  export Depot
  export Remote
  cat  <<'EOF' > /tmp/git.lst
scripts/vilaineaval.sh
EOF
  ls -1 scripts/vilaineaval*.R >> /tmp/git.lst
  ls -1 VILAINEAVAL/*.tex >> /tmp/git.lst
  ls -1 VILAINEAVAL/images/*.pdf >> /tmp/git.lst
  cat  <<'EOF' > /tmp/README.md
# vilaineaval : Bretagne Vivante et Rennes Vilaine Aval

Scripts en environnement Windows 10 : MinGW R

Ces scripts exploitenet des données en provenance des Serena et Biolovision
EOF
}
[ $# -eq 0 ] && ( HELP )
CONF
while [ "$1" != "" ]; do
  case $1 in
    -c | --conf )
      shift
      Conf=$1
      ;;
    * )
      $*
      exit 1
  esac
  shift
done