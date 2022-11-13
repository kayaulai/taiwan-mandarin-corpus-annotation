install.packages(c("tidyverse", "spacyr"))

#You should install Anaconda before these steps: https://www.anaconda.com/
library(spacyr)
spacy_install(lang_models="zh_core_web_sm")
#You will be asked if you want to proceed. Type '2' to proceed.

install.packages("devtools")
library(devtools)
install_github("rezonators/rezonateR")
