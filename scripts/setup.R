library(haven)
library(tidyverse)
library(here)
library(brms)
options(mc.cores = parallel::detectCores())

DATADIR <- ini::read.ini("config.ini")$local$DATADIR

`%p0%` <- function(x, y){
  paste0(x, y)
}