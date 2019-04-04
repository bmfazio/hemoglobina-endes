library(haven)
library(tidyverse)
library(here)
library(brms)
options(mc.cores = parallel::detectCores())

DATADIR <- "D:/datasets/inei/endes/"

`%p0%` <- function(x, y){
  paste0(x, y)
}