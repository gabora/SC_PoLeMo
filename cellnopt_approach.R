# apply CELLNOPTR to AIDAN's datafiles
library(CellNOptR)
require(devtools)
require(stringr)
require(readxl)
require(plyr)
require(ggplot2)
require(dplyr)

setwd("~/Documents/SaezGrp/SignallingSingleCell/Aidan/macroInput")
document(".")
load_all(".")

# reduced 
cnolist = CNOlist("MIDAS/fret3/test2.csv")