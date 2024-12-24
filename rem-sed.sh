for i in *.vrt; do sed -i "" "s/<\//<SLASH/g" "$i"; done;
for i in *.vrt; do sed -i "" "s/\///g" "$i"; done;
for i in *.vrt; do sed -i "" "s/<SLASH/<\//g" "$i"; done