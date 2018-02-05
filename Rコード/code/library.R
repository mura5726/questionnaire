#library--------------------
library(gridExtra)
library(tidyverse)
library(scales)
library(data.table)
library(readxl)
library(rstan)

# install.packages('readxl')
library(readxl)

# install.packages("xlsx")
# library(xlsx)

# install.packages('rJava')
# library(rJava)

#install.packages("rex")
library(rex)

fread2     <- function(filename){fread(filename, data.table = F, stringsAsFactors = F) %>% as_tibble()}
fread2utf8 <- function(filename){fread(filename, data.table = F, stringsAsFactors = F, encoding = "UTF-8") %>% as_tibble()}
