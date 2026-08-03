#!/bin/bash
DEST=/srv/shiny-server/Lamprey-Dashboard
SRC=/home/chas/Lamprey/lamprey-dashboard

mkdir -p "$DEST"
cp "$SRC/app/app.R" "$DEST/app.R"
cp -r "$SRC/data" "$DEST/"
cp -r "$SRC/config" "$DEST/"
cp "$SRC/DESCRIPTION" "$DEST/"
chown -R shiny:shiny "$DEST"
echo "Done. Contents:"
ls "$DEST"
