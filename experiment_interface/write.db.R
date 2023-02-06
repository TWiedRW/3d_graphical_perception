## ---------------------------
##
## Script name: write.db.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2023-02-05
##
## ---------------------------
##
## Notes: Script to create database code
##   
##
## ---------------------------

library(RSQLite)
con <- dbConnect(SQLite(), 'graphics_group_study.db')

dbReadTable(con, 'NAME OF DATASET')

