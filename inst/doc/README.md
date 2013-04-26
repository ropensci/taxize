# taxize manuscript readme

You can change this workflow if you want, but it seems to work well for me. 

### The workflow

+ Use the taxize_withcode.Rnw file to make any changes to the manuscript. This makes it easy to test changes in R code quickly. 
+ Then `knit("taxize_withcode.Rnw")` to make the .tex file 
+ optionally `knit2pdf("taxize_withcode.Rnw")` to make a pdf, or I have the .tex file open in Texpad or other latex editor to view compiled pdf.