#!/bin/bash
# make temporary copy of preprocessing folder with all data we need in build
cp -r analysis tmp
# switch to gh-pages branch
git checkout gh-pages
# copy over index file (the processed preprocessing.Rmd) from master branch
cp tmp/main.html index.html
# clean
rm -rf rscript/
mkdir rscript
# copy over necessary scripts from master branch 
cp tmp/main.Rmd rscript

mkdir rscript/output
# copy over other nessecary output files from master branch

# copy over other necessary input files from master branch
cp -r tmp/input rscript/

mkdir rscript/scripts
# copy over necessary script files from master branch
cp -r tmp/scripts rscript/

# zip the rscript folder
zip -r rscript.zip rscript
# remove the rscript folder
rm -rf rscript
# remove temporary folder
rm -rf tmp
# add everything for committing
git add .
git add -u
# commit in gh-pages
git commit -m "analysis: build and deploy to gh-pages"
# push to remote:gh-pages
git push origin gh-pages 
# checkout master again
git checkout master