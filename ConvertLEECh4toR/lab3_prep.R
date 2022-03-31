# Title:  Lab3_Prep.R
# 9 Feb 2022
# Author: Tim Assal
# code to needed for Chapter 4 (Understanding Landscape Metrics) of the Learning Landscape Ecology text
# In the LEE text, Parts 1 and 2 are hand calcs; parts 3 and 4 were completed in Fragstats
# This script converts ascii to rasters needed for Parts 3 and 4 so the lab can be done in R
# the data can be provided to students so they can begin their R script at that point  
# See Lab3_student_script.R

#load packages
library(raster)
library(landscapemetrics)
library(tidyverse)

#create rasters for example landscapes from ascii files
# original ascii data is not in SourceData

#####
## Part 3 of Learning Landscape Ecology
#####
#note: we'll import the ascii files needed for Part 3 and export them as tifs so they can be used in a clean R script for students

###
#Early settlement raster
#load the ascii file from LEE
hist.txt<-read_delim("Original_LEEData/esett.asc",col_names=FALSE) #load ascii file
class(hist.txt)
hist.txt<-as.matrix(hist.txt) #convert to matrix
class(hist.txt)

# NOTE - these do not have a projection; I simply set the ymx and xmx based on the number of rows and columns
# multipled by 30 (30 m cells). I don't care about the projection, I simply want the area calculations to be correct.
#first create a raster with proper res; then assign values from ascii
hist.R <- raster(xmn = 0,   # set minimum x coordinate
                 xmx = 10000,    # set maximum x coordinate
                 ymn = 0,     # set minimum y coordinate
                 ymx = 10000,     # set maximum y coordinate
                 res = c(1000,1000)) # resolution in c(x,y) direction
hist.R
res(hist.R)
hist.R <- setValues(hist.R, hist.txt)
hist.R
plot(hist.R)

#get some calcs to make sure I have the proper units
lsm_c_ca(hist.R) #total area by class (hectares)
lsm_c_pland(hist.R) #proportion of landscape by class (percent)

#get mean patch size - note: output is in hectares
#compare with LEE - yes, these are correct!
lsm_c_area_mn(hist.R, directions = 4) 
###

###
#Post-settlement raster
#load the ascii file from LEE
post.txt<-read_delim("Original_LEEData/psett.asc",col_names=FALSE) #load ascii file
class(post.txt)
post.txt<-as.matrix(post.txt) #convert to matrix
class(post.txt)

#first create a raster with proper res; then assign values from ascii
post.R <- raster(xmn = 0,   # set minimum x coordinate
                 xmx = 10000,    # set maximum x coordinate
                 ymn = 0,     # set minimum y coordinate
                 ymx = 10000,     # set maximum y coordinate
                 res = c(1000,1000)) # resolution in c(x,y) direction
post.R
res(post.R)
post.R <- setValues(post.R, post.txt)
post.R
plot(post.R)

#get some calcs to make sure I have the proper units
lsm_c_ca(post.R) #total area by class (hectares)
lsm_c_pland(post.R) #proportion of landscape by class (percent)

#get mean patch size - note: output is in hectares
#compare with LEE - yes, these are correct!
lsm_c_area_mn(post.R, directions = 4) 
###


#####
## Part 4 of Learning Landscape Ecology
#####
#note: we'll import the ascii files needed for Part 4 and export them as tifs so they can be used in a clean R script for students

#New England 1 raster
#load the ascii file from LEE
ne1.txt<-read_delim("Original_LEEData/ne1.asc",col_names=FALSE) #load ascii file
#it has one column too many - delete it
#delete the last column
ne1.txt<-select(ne1.txt, -last_col())
class(ne1.txt)
ne1.txt<-as.matrix(ne1.txt) #convert to matrix
class(ne1.txt)

#first create a raster with proper res; then assign values from ascii
ne1.R <- raster(xmn = 0,   # set minimum x coordinate
                 xmx = 6480,    # set maximum x coordinate
                 ymn = 0,     # set minimum y coordinate
                 ymx = 6480,     # set maximum y coordinate
                 res = c(30,30)) # resolution in c(x,y) direction
ne1.R
res(ne1.R)
ne1.R <- setValues(ne1.R, ne1.txt)
ne1.R
plot(ne1.R)

#x-check with spreadsheet
lsm_l_pd(ne1.R, direction=8) #units = number per 100 ha
#edge density
lsm_l_ed(ne1.R) #meters per ha
#correct!

#new england 2 raster
#load the ascii file from LEE
ne2.txt<-read_delim("Original_LEEData/ne2.asc",col_names=FALSE) #load ascii file
#it has one column too many - delete it
#delete the last column
ne2.txt<-select(ne2.txt, -last_col())
class(ne2.txt)
ne2.txt<-as.matrix(ne2.txt) #convert to matrix
class(ne2.txt)

#first create a raster with proper res; then assign values from ascii
ne2.R <- raster(xmn = 0,   # set minimum x coordinate
                xmx = 6480,    # set maximum x coordinate
                ymn = 0,     # set minimum y coordinate
                ymx = 6480,     # set maximum y coordinate
                res = c(30,30)) # resolution in c(x,y) direction
ne2.R
res(ne2.R)
ne2.R <- setValues(ne2.R, ne2.txt)
ne2.R
plot(ne2.R)

#x-check with spreadsheet
lsm_l_pd(ne2.R, direction=8) #units = number per 100 ha
#edge density
lsm_l_ed(ne2.R) #meters per ha
#correct!


#new england 3 raster
#load the ascii file from LEE
ne3.txt<-read_delim("Original_LEEData/ne3.asc",col_names=FALSE) #load ascii file
#it has one column too many - delete it
#delete the last column
ne3.txt<-select(ne3.txt, -last_col())
class(ne3.txt)
ne3.txt<-as.matrix(ne3.txt) #convert to matrix
class(ne3.txt)

#first create a raster with proper res; then assign values from ascii
ne3.R <- raster(xmn = 0,   # set minimum x coordinate
                xmx = 6480,    # set maximum x coordinate
                ymn = 0,     # set minimum y coordinate
                ymx = 6480,     # set maximum y coordinate
                res = c(30,30)) # resolution in c(x,y) direction
ne3.R
res(ne3.R)
ne3.R <- setValues(ne3.R, ne3.txt)
ne3.R
plot(ne3.R)

#x-check with spreadsheet
lsm_l_pd(ne3.R, direction=8) #units = number per 100 ha
#edge density
lsm_l_ed(ne3.R) #meters per ha
#correct!

######now convert the madison grids
# note they are a different size

#madison 1 raster
#load the ascii file from LEE
mad1.txt<-read_delim("Original_LEEData/mad1.asc",col_names=FALSE) #load ascii file
#it has one column too many - delete it
#delete the last column
mad1.txt<-select(mad1.txt, -last_col()) #see LEE book for number of rows and columns
class(mad1.txt)
mad1.txt<-as.matrix(mad1.txt) #convert to matrix
class(mad1.txt)

# NOTE - these do not have a projection; I simply set the ymx and xmx based on the number of rows and columns
# multipled by 30 (30 m cells). I don't care about the projection, I simply want the area calculations to be correct. 
#first create a raster with proper res; then assign values from ascii
mad1.R <- raster(xmn = 0,   # set minimum x coordinate
                xmx = 24000,    # set maximum x coordinate
                ymn = 0,     # set minimum y coordinate
                ymx = 17250,     # set maximum y coordinate
                res = c(30,30)) # resolution in c(x,y) direction
mad1.R
res(mad1.R)
mad1.R <- setValues(mad1.R, mad1.txt)
mad1.R
plot(mad1.R)

#x-check with spreadsheet
#patch density
lsm_l_pd(mad1.R, direction=8) #units = number per 100 ha
#note - this is correct; the master xls had it calculated for 4 neighbors
#edge density
lsm_l_ed(mad1.R) #meters per ha
#correct!



#madison 2 raster
#load the ascii file from LEE
mad2.txt<-read_delim("Original_LEEData/mad2.asc",col_names=FALSE) #load ascii file
#it has one column too many - delete it
#delete the last column
mad2.txt<-select(mad2.txt, -last_col()) #see LEE book for number of rows and columns
class(mad2.txt)
mad2.txt<-as.matrix(mad2.txt) #convert to matrix
class(mad2.txt)

# NOTE - these do not have a projection; I simply set the ymx and xmx based on the number of rows and columns
# multipled by 30 (30 m cells). I don't care about the projection, I simply want the area calculations to be correct. 
#first create a raster with proper res; then assign values from ascii
mad2.R <- raster(xmn = 0,   # set minimum x coordinate
                 xmx = 24000,    # set maximum x coordinate
                 ymn = 0,     # set minimum y coordinate
                 ymx = 17250,     # set maximum y coordinate
                 res = c(30,30)) # resolution in c(x,y) direction
mad2.R
res(mad2.R)
mad2.R <- setValues(mad2.R, mad2.txt)
mad2.R
plot(mad2.R)

#x-check with spreadsheet
#patch density
lsm_l_pd(mad2.R, direction=8) #units = number per 100 ha
#note - this is correct; the master xls had it calculated for 4 neighbors
#edge density
lsm_l_ed(mad2.R) #meters per ha
#correct!


####

###now export them to SourceData - so the students have them
writeRaster(hist.R, filename="SourceData/esett.tif", 
            format="GTiff", datatype='INT4U', overwrite=TRUE)
writeRaster(post.R, filename="SourceData/psett.tif", 
            format="GTiff", datatype='INT4U', overwrite=TRUE)
writeRaster(mad1.R, filename="SourceData/mad1.tif", 
            format="GTiff", datatype='INT4U', overwrite=TRUE)
writeRaster(mad2.R, filename="SourceData/mad2.tif", 
            format="GTiff", datatype='INT4U', overwrite=TRUE)
writeRaster(ne1.R, filename="SourceData/ne1.tif", 
            format="GTiff", datatype='INT4U', overwrite=TRUE)
writeRaster(ne2.R, filename="SourceData/ne2.tif", 
            format="GTiff", datatype='INT4U', overwrite=TRUE)
writeRaster(ne3.R, filename="SourceData/ne3.tif", 
            format="GTiff", datatype='INT4U', overwrite=TRUE)

