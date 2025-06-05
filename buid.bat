@echo off
REM ================================
REM Build script for Pythia package
REM ================================

echo Running documentation...
REM Rscript -e "devtools::document()"

echo Rebuilding README...
Rscript -e "devtools::build_readme()"

echo Cleaning old site...
Rscript -e "pkgdown::clean_site()"

echo Building site...
Rscript -e "pkgdown::build_site()"

echo Preview site...
Rscript -e "pkgdown::preview_site()"

echo Done.
REM pause
