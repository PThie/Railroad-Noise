############################################################
# Load Libraries                                           #
############################################################

pacman::p_load(rio)
pacman::p_load(tidyverse)
pacman::p_load(openxlsx)
pacman::p_load(sf)
pacman::p_load(tmap)
pacman::p_load(lubridate)
pacman::p_load(fixest)
pacman::p_load(fst)
pacman::p_load(psych)
pacman::p_load(zoo)
pacman::p_load(data.table)

############################################################
# Path Specification                                       #
############################################################

dataPath <- "N:/Schienenlaerm/data/"
dataGebiete <- "M:/_FDZ/interne Daten/Gebietseinheit/"
outputPath <- "N:/Schienenlaerm/output/"
workdir <- "N:/Schienenlaerm/"
codePath <- "N:/Schienenlaerm/code/"

############################################################
# set working directory                                    #
############################################################

setwd(workdir)

############################################################
# Stage 01: Preparation Files                              #
############################################################


# laermstatistik ----------------------------------------------------------
# mainly serves as motivation of the topic
# displays the number of people affected by the railroad system
# displays a map with noise indicator (LKZ)
source(file.path(codePath, "01_01_laermstatistik.r"), encoding = "UTF-8")


# transported goods -------------------------------------------------------
# mainly serves as motivation of the topic
# displays the transporation capacity of railraods in Germany
source(file.path(codePath, "01_02_transported_goods.r"), encoding = "UTF-8")


# noise environment -------------------------------------------------------
# mainly for panel analysis
# prepares the data from the measuring stations
# plots descriptives of noise data
source(file.path(codePath, "01_03_noise_environment.r"), encoding = "UTF-8")

# track environment -------------------------------------------------------
# prepares the stations and the noise barrier data
# needed for the housing tpye preparation files
source(file.path(codePath, "01_04_preparation_track_environment.r"), encoding = "UTF-8")


# HK preparation ----------------------------------------------------------
# prepares the house purchases data
# outputs: hk_basic_prep; hk_complete_prep 
source(file.path(codePath, "01_05_preparation_hk.r"), encoding = "UTF-8")


# buffering ---------------------------------------------------------------
# buffers around the main tracks for each housing type
# outputs: hk_buffered; wm_buffered; wk_buffered
source(file.path(codePath, "01_06_main_tracks_buffer.r"), encoding = "UTF-8")


############################################################
# Stage 02: Descriptives                                   #
############################################################

source(file.path(codePath, "02_descriptives.r"), encoding = "UTF-8")


############################################################
# Stage 03: Estimation                                     #
############################################################