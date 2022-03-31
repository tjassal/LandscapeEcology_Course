# Title:  Lab3_script.R
# 9 Feb 2022
# Author: Tim Assal
# code to needed to run Lab 3 (converted from Fragstats in LEE textbook)

#load the packages you'll need
library(raster)
library(landscapemetrics)
library(tidyverse)

### 
# !!! Be sure to set up an .RProj file for Lab3
# once you do that, your working directory should be set to Lab3
# This is KEY, along with proper data management
getwd() #check the location of your working dir

# Also, I suggest you make a copy of this script so you'll have the original 
# after you make changes

#################
##   Part 3    ##
#################

########
# Landscape Metrics using the 4-neighbor rule
########

###
#Early-settlement landscape
###
#load the raster
esett.R<-raster("SourceData/esett.tif")
esett.R #check some info on it
plot(esett.R) #this should match the figure in the LEE book

##Calculate landscape metrics
# we'll calculate the # of patches, mean patch size, SHEI and contagion

#Note: we'll write each of the values to an object, so we can call them later to wrap into a table

##First use the 4 neighbor rule##
#Number of patches (note: .E4 indicates early settlement, 4 neighbors)
np.E4<-lsm_l_np(esett.R, directions=4)
np.E4 #show the value
#if you'd like to get a visual of the patches by cover type
show_patches(esett.R, class = c(1:3),directions=4, labels = TRUE) 

#Mean patch size - across all cover types
mps.E4<-lsm_l_area_mn(esett.R, directions = 4) #note: output is in hectares
mps.E4

#Shannon's eveness index
shei.E4<-lsm_l_shei(esett.R) 
shei.E4

#Contagion
cont.E4<-lsm_l_contag(esett.R, verbose = TRUE) 
cont.E4 #note, the value returned is percent
#for help on any function, simply type
?lsm_l_contag

#Compare these calcs with the hand calcs you made earlier. 
#note: the contagion value is a little different than the hand calculation (~4% different)

#wrap the variables above into a table that we'll use later
early4.DF<-rbind(np.E4, mps.E4, shei.E4, cont.E4)
#rename the value column to EarlyPeriod - so we can delineate
early4.DF<-early4.DF %>% 
  rename(EarlyPeriod = value) 

#have a look at your new table:
view(early4.DF)

###
#End Early-settlement landscape
### 

###
#Post-settlement landscape
###
#load the raster
psett.R<-raster("SourceData/psett.tif")
psett.R #check some info on it
plot(psett.R) #this should match the figure in the LEE book

##Calculate landscape metrics
# we'll calculate the # of patches, mean patch size, SHEI and contagion

##First use the 4 neighbor rule##
#Number of patches (note: .P4 indicates post settlement, 4 neighbors)
np.P4<-lsm_l_np(psett.R, directions=4)
np.P4 #show the value
#if you'd like to get a visual of the patches by cover type
show_patches(psett.R, class = c(1:3),directions=4, labels = TRUE) 

#Mean patch size - across all cover types
mps.P4<-lsm_l_area_mn(psett.R, directions = 4) #note: output is in hectares
mps.P4

#Shannon's eveness index
shei.P4<-lsm_l_shei(psett.R) 
shei.P4

#Contagion
cont.P4<-lsm_l_contag(psett.R, verbose = TRUE) 
cont.P4 #note, the value returned is percent

#wrap the variables above into a table that we'll use later
post4.DF<-rbind(np.P4, mps.P4, shei.P4, cont.P4)
#rename the value column to PostPeriod - so we can delineate
post4.DF<-post4.DF %>% 
  rename(PostPeriod = value) 

###
#End Post-settlement landscape
### 

#Now join the early and late dataframes to create one 4-neighbor dataframe
full4.DF<-early4.DF %>% 
  full_join(post4.DF) %>% 
  mutate(NeighborRule= 4)

#round EarlyPeriod and PostPeriod columns to include 3 sig digits for clarity
full4.DF$EarlyPeriod<-round(full4.DF$EarlyPeriod, digits=3) 
full4.DF$PostPeriod<-round(full4.DF$PostPeriod, digits=3) 

#have a look:
view(full4.DF)

#let's drop columns we don't need
full4.DF<-full4.DF %>% 
  select(-layer, -class, -id)

#have a look now, better
view(full4.DF)

#export the dataframe to a table (csv)
write_csv(full4.DF, "DerivedData/part3_4neighbors.csv")
###

########
# End Landscape Metrics using the 4-neighbor rule
########

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!! Write code here to calculate the same metrics as above using the 8-neighbor rule
# HINT: copy/paste the entire code block, then change the notation, but take your time!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#################
##  END Part 3  ##
#################


#################
##  Part 4  ##
#################
# Calc Landscape Metrics for real landscapes 

# See p. 59-60 of the LEE text for details
# We will load 5 landscapes (two classifications of Madison, WI from different analysts
# and three different NE landscapes). See handout for maps of the landscapes
# We'll calculate 5 metrics using the 8-neighbor rule:
# • Contagion
# • Patch Density (the average number of patches per 100 ha)
# • Edge Density (an expression of edge:area relationships)
# • Landscape Shape Index (a measure of shape complexity)
# • Largest Patch Index (an indicator of connectivity)
# • Patch Richness (the number of patch types)


###
# Mad1 Landscape
###
mad1.R<-raster("SourceData/mad1.tif") #load the raster
mad1.R #check some info on it
plot(mad1.R) #this should match the figure in the LEE book

#Contagion
cont.mad1<-lsm_l_contag(mad1.R, verbose = TRUE) 
cont.mad1 #note, the value returned is percent

#patch density
pd.mad1<-lsm_l_pd(mad1.R, direction=8) #units = number per 100 ha
pd.mad1

#edge density
ed.mad1<-lsm_l_ed(mad1.R) #meters per ha
ed.mad1

#landscape shape index 
lsi.mad1<-lsm_l_lsi(mad1.R) #no units
lsi.mad1

#largest patch index - correct!
lpi.mad1<-lsm_l_lpi(mad1.R, direction=8) #units percentage
lpi.mad1

#patch richness - correct!
pr.mad1<-lsm_l_pr(mad1.R)
pr.mad1

#wrap the variables above into a table that we'll use later
mad1.DF<-rbind(cont.mad1, pd.mad1, ed.mad1, lsi.mad1, lpi.mad1, pr.mad1)
#rename the value column to mad1 - so we can delineate
mad1.DF<-mad1.DF %>% 
  rename(mad1 = value) 
#round columns to include 2 sig digits for clarity
mad1.DF$mad1<-round(mad1.DF$mad1, digits=2) 

###
# End Mad1 Landscape
###

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!! Write code here to calculate the same metrics as above for the other 4 landscapes
# be sure to use the prefix: mad2, ne1, ne2, ne3 - then you can pick up the code below
# HINT: copy/paste each code block, then change the notation, but take your time!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#######
# Join dataframes into one master table
#######
#Now join all dataframes into one
part4.DF<-mad1.DF %>% 
  full_join(mad2.DF) %>% 
  full_join(ne1.DF) %>% 
  full_join(ne2.DF) %>% 
  full_join(ne3.DF) 

#export the dataframe to a table (csv)
write_csv(part4.DF, "DerivedData/part4.csv")

############
# Let's visualize the data to make easy comparisons
############

#first we need to reshape the data so we can visualize it in a clear way

#convert the data table from wide to long
part4.DF_gather <- part4.DF %>%
  gather(key = landscape, value = value, mad1, mad2, ne1, ne2, ne3)
#have a look at the two dataframes to see how we changed them

head(part4.DF_gather)

#create a basic bar plot that is faceted by landscape metric
ggplot(part4.DF_gather, aes(x=landscape, y=value)) + 
  geom_bar(stat = "identity")+
  facet_wrap(facets = vars(metric))

#not bad, but let's make it look nicer
good.plot<-ggplot(part4.DF_gather, aes(x=landscape, y=value)) + 
  geom_bar(stat = "identity", fill="dark green")+
  facet_wrap(facets = vars(metric))+
  labs(title = "Landscape Metrics on 'Real' Landscapes",
       subtitle = "!!! put your name here !!!",
       caption = "#DrAssalIsMyFavProf", 
       x = "Landscape", 
       y = "Metric Value") +
  theme_bw()
good.plot #since we wrote it an object, we need to call it to plot it

#that looks decent, but the y-axis has all the same values and some of the metrics are 
#hard to see; let's modify and create a free y-axis (note: values will be different)
better.plot<-ggplot(part4.DF_gather, aes(x=landscape, y=value)) + 
  geom_bar(stat = "identity", fill="dark green")+
  facet_wrap(. ~ metric, scales = "free_y")+ #note the diff code here
  labs(title = "Landscape Metrics on 'Real' Landscapes",
       subtitle = "!!! put your name here !!!",
       caption = "#DrAssalIsMyFavProf", 
       x = "Landscape", 
       y = "Metric Value") +
  theme_bw()
better.plot #since we wrote it an object, we need to call it to plot it
#Note: y-axis are now different for each metric

#this is better, but let's set each landscape to the same color
# so it will be easier to interpret
#let's use a custom package
library(wesanderson) #load the Wes Anderson Color package; I like Wes Anderson movies
#I know the guy who create this package. He's a strange dude (no offense Karthik); 

names(wes_palettes) #have a look at the palletes
wes_palette("IsleofDogs1") #I've never seen this movie, but the colors are nice

#now plot; note there are a few diffs in the code
best.plot<-ggplot(part4.DF_gather, aes(x=landscape, y=value, fill=landscape)) + 
  geom_bar(stat = "identity", alpha=1)+
  #geom_bar(position = "dodge", stat = "identity", alpha=1) +
  facet_wrap(. ~ metric, scales = "free_y")+ 
  scale_fill_manual(values=wes_palette("IsleofDogs1"))+ 
  labs(title = "Landscape Metrics on 'Real' Landscapes",
       subtitle = "!!! put your name here !!!",
       caption = "#DrAssalIsMyFavProf", 
       x = "Landscape", 
       y = "Metric Value") +
  theme_bw()
best.plot #since we wrote it an object, we need to call it to plot it
#Note: y-axis are now different for each metric; each landscape has it's own color - easy interp

#save the plot- now it can be broutht into your lab word document
ggsave("DerivedData/Part4_landscape_metrics.jpg", best.plot, width = 7.5, height = 4)

#############
### Assess correlation of metrics
############
#we want to compare the metrics for each landscape against each other to assess correlation strength

#convert the data table from wide to long
part4.DF_gather <- part4.DF %>%
  gather(key = landscape, value = value, mad1, mad2, ne1, ne2, ne3)
#have a look at the two dataframes to see how we changed them

head(part4.DF_gather)


#first create a subset of variables that we want to assess
head(part4.DF)

#subset the data and drop columns that are not needed
part4.DF.sub<-part4.DF%>%
  select(metric:ne3) 

head(part4.DF.sub)

# We need to transpose the data frame
library(data.table) #load the package; remember to install if first!
t.part4.DF.sub <- transpose(part4.DF.sub)
#promote first row to column names
names(t.part4.DF.sub) <- t.part4.DF.sub[1,] 
#now delete the extra first row
t.part4.DF.sub <- t.part4.DF.sub[-1,]
head(t.part4.DF.sub) #looks good
str(t.part4.DF.sub) #for some reason, the columns are character
#convert all to numeric
t.part4.DF.sub <- as.data.frame(sapply( t.part4.DF.sub, as.numeric ))


library(GGally) #load the package; remember to install if first!
cor1 <- ggpairs(t.part4.DF.sub)
cor1  

#this is a Pearson's correlation coefficient. 

#What does this table tell us?
#Simply crosswalk two variables to get the correlation value. 

#Note: this line from the paper we read (Assal et al. 2015):
#Multicollinearity among all potential explanatory variables was assessed 
#prior to model calibration using the Pearson’s correlation coefficient. 
#Variables with a correlation coefficient greater than 0.8 or less than −0.8 
#were removed from consideration within the same model.

#With that in mind, now many of these metrics would we include as model variables?

#############
### End Assess correlation of metrics
############
