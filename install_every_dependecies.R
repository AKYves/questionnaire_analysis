# I assume you have R (and Rstudio)

install.packages("tidyverse")
install.packages("openxlsx") # requires installation of rtools on windows
install.packages("devtools")
install.packages(c("googledrive", "googlesheet4", "gt", "FactoMineR"))
devtools::install_github('cttobin/ggthemr')
#just source this file and everything should work fine.
