#!/bin/bash
#Short scriptto convert .md files in the markdown folder to HTML for AmyDoc
OUTPUTFOLDER="amydoc"
INPUTFOLDER="markdown"
FORMAT="markdown"
OUT="html5"
FLAGS=-s

#Check for pandoc
if test ! -x /usr/bin/pandoc 
  then
    echo 'Error: pandoc is not installed.' >&2
    exit 1
fi

#Create folder if it does not already exist
if test ! -e "$OUTPUTFOLDER"
  then
    mkdir "$OUTPUTFOLDER"
fi

for module in "$INPUTFOLDER"/*.md
do
  SUBSTRING=$(echo "$module"| cut -d '.' -f 1)
  SUBSTRING=$(echo "$SUBSTRING" | cut -d '/' -f 2)
  echo "Processing $SUBSTRING" 
  pandoc $FLAGS "$module" > $OUTPUTFOLDER/$SUBSTRING.$OUT
done

exit 0
