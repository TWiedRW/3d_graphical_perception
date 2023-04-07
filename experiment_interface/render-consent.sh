#!/bin/bash
quarto render graphics-consent-218.qmd --to html
quarto render graphics-consent-218.qmd --to pdf
quarto render graphics-consent-dept.qmd --to html
quarto render graphics-consent-dept.qmd --to pdf


# mv graphics-consent-*.html www/
cp graphics-consent-*.pdf ../IRB/
mv graphics-consent-*.pdf www/
rm graphics-consent-*.tex graphics-consent-*.aux graphics-consent-*.log 
rm -rf graphics-consent-*_files
