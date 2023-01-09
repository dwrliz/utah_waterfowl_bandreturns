# PIJA Sample design for Utah and Colordo, 2019 Pilot season

# 02/19/2019
# LM UDWR

#load packages
library(spsurvey)
library(SDraw)
library(dplyr)
library(mapview)
library(ggplot2)
library(spData)
library(st_geometry)
library(rgeos)
library(sf)

#############################################################################

#load sample area (grid)
area_raw<-st_read("C:/Users/lizmoore/Documents/Local_Working/Pinyon_Jay/GRTS/grids/CO_UT_Grid20190220.shp")

# filter to Include = Y
area<-filter(area_raw, Include == "Y")

# # Histogram overlaid with kernel density curve
# ggplot(area, aes(x=FREQUENCY)) + geom_histogram(aes(y=..density..),     # Histogram with density instead of count on y-axis
#   binwidth= 1,
#   colour="black", fill="white") +
#   geom_density(alpha=.2, fill="#FF6666") + # Overlay with transparent density plot
#   geom_vline(aes(xintercept=mean(FREQUENCY, na.rm=T)),   # Ignore NA values for mean
#              color="red", linetype="dashed", size=1)+
#   theme_minimal()

#create a field for frequency strata
#0 observations
area$freq_strata[area$FREQUENCY == 0]<-0
# 1 observation
area$freq_strata[area$FREQUENCY == 1]<-1
# greater than 1 observation
area$freq_strata[area$FREQUENCY > 1]<-2

#Frequency strata histogram
# ggplot(area, aes(x=freq_strata)) + geom_histogram(aes(y=..count..),
#   binwidth= 1,
#   colour="black", fill="white")+ stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)

#Split Categories by State
for ( i in 1: nrow(area)){
  area$strata[i] <- paste(area$state[i],"_",area$freq_strata[i], sep = "")
}


#############################################################################
#############################################################################
#Step 1: How many surveys? Weights for Strata? Enter input parameters here!!
# See Russ' calculator: "C:\Users\lizmoore\Documents\Local_Working\Pinyon_Jay\GRTS\Site_Number_Calculator_Russ\Draft Survey Effort Calculator_15jan2019.xlsx"

#Assign variables Based on calculator 01/15/2019
#colorodo crew size
co_crw<-1.7
#utah crew size
ut_crw<-4.25
#total survey sites based on site calculator
surveys_tot<- 81
#number of backups per target sample site
number_backups<-5

#percent of resample
twice<- 0.333
thrice<-0.333

#percent of each frequency strata
freq_strat_0<-0.333
freq_strat_1<-0.333
freq_strat_2<-0.333

###########################################################################
###########################################################################
#target survey calculations
tot_crw<-co_crw+ut_crw
surveys_ut<- (surveys_tot*ut_crw)/tot_crw
surveys_co<- (surveys_tot*co_crw)/tot_crw
#Strata traget sample breakdown totals by state
#Colorado
co_strata_0<-round((surveys_co*freq_strat_0), 0)
co_strata_1<-round((surveys_co*freq_strat_1), 0)
co_strata_2<-round((surveys_co*freq_strat_1), 0)

#Utah
ut_strata_0<-round((surveys_ut*freq_strat_0), 0)
ut_strata_1<-round((surveys_ut*freq_strat_0), 0)
ut_strata_2<-round((surveys_ut*freq_strat_0), 0)

#total(Rounded up total)
total_strata<-sum(co_strata_0, co_strata_1, co_strata_2, ut_strata_0, ut_strata_1, ut_strata_2)


##########################################################################
##########################################################################
#Apply Unequal Unstratified GRTS based on previous observation frequencies

#read in as data frame
df_grid<-data.frame(area)

#read in as sp
sp_grid<-as(area, "Spatial")

#Unequal, unstratified GRTS design -FIRST PICK FOR TARGET SAMPLE
Unequaldsgn <- list(None=list(panel=c(PanelOne=total_strata), #total sample number
                              seltype="Unequal",
                              caty.n=c("CO_0"=co_strata_0, #sample CO freq 0
                                       "CO_1"=co_strata_1,
                                       "CO_2"=co_strata_2,
                                       "UT_0"=ut_strata_0,
                                       "UT_1"=ut_strata_1,
                                       "UT_2"=ut_strata_2))) #needs to add up to total sample
#select the unequal unstratfied sites, inputing the design
Unequalsites <- grts(design=Unequaldsgn,
                     DesignID="TARGET",
                     type.frame="area",
                     src.frame = "sp.object",
                     #src.frame="shapefile",
                     #in.shape="shp_grid",
                     sp.object  =sp_grid,
                     att.frame= df_grid,
                     mdcaty="strata",
                     shapefile=FALSE)
#change the sp to an sf
sample <- st_as_sf(Unequalsites)
#assign the epsg from input to the output
st_crs(sample) <- st_crs(area)
#view it
#mapview(sample)+mapview(area)

#Create a cleaned up version (dropped columns) of sites
sample_sites<-subset(sample, select=c("siteID"))#, "unique_gri", "mdcaty"))
#spatial join points to grid polygons
target_grid<-st_join(area, sample_sites, join = st_intersects)
#drop duplicate sites
target_grid<-distinct(target_grid, unique_gri, .keep_all = TRUE)
target_grid$Site_Rank<-target_grid$siteID
#Drop all non target cells- create a layer with just TARGETS
target_cells<-target_grid[!is.na(target_grid$siteID),]
#Grab all non-target cells- create a layer for SELECTING BACKUPS
remaining_cells<-target_grid[is.na(target_grid$siteID),]
#map it
#mapview(target_cells, zcol= "mdcaty")+ mapview(remaining_cells)


##############################################################################
##############################################################################
#Create Backup Point Selection, omitting already selected target cells from the area

#read in as data frame
df_grid_bu<-data.frame(remaining_cells)

#read in as sp
sp_grid_bu<-as(remaining_cells, "Spatial")

#Unequal, unstratified GRTS design -SECOND PICK FOR BACKUP SAMPLES
Unequaldsgn_bu <- list(None=list(panel=c(PanelOne=(total_strata*number_backups)), #total sample number
                              seltype="Unequal",
                              caty.n=c("CO_0"=(co_strata_0*number_backups), #sample CO freq 0
                                       "CO_1"=(co_strata_1*number_backups),
                                       "CO_2"=(co_strata_2*number_backups),
                                       "UT_0"=(ut_strata_0*number_backups),
                                       "UT_1"=(ut_strata_1*number_backups),
                                       "UT_2"=(ut_strata_2*number_backups)))) #needs to add up to total sample
#select the unequal unstratfied sites, inputing the design
Unequalsites_bu <- grts(design=Unequaldsgn_bu,
                     DesignID="BACKUP",
                     type.frame="area",
                     src.frame = "sp.object",
                     #src.frame="shapefile",
                     #in.shape="shp_grid",
                     sp.object  =sp_grid_bu,
                     att.frame= df_grid_bu,
                     mdcaty="strata",
                     shapefile=FALSE)


#change the sp to an sf
sample_bu <- st_as_sf(Unequalsites_bu)
#assign the epsg from input to the output
st_crs(sample_bu) <- st_crs(area)
#view it
#mapview(sample_bu)+mapview(remaining_cells)

#Create a cleaned up version (dropped columns) of sites
sample_bu_sites<-subset(sample_bu, select=c("siteID"))#, "unique_gri", "mdcaty"))
#spatial join points to grid polygons
target_grid<-st_join(target_grid, sample_bu_sites, join = st_intersects)
#drop duplicate sites
target_grid<-distinct(target_grid, unique_gri, .keep_all = TRUE)

#######################Add spatial balance list of sites surveyed twice and thrice
########################################################################################
#select which sites should be surveyed twice by using GRTS on the target GRTS pull
num_twice<- round(total_strata*twice,0)
resample<-subset(target_cells, select=c("unique_gri"))
target_grid_resample<- sdraw(as(resample, "Spatial"), n = num_twice, type="GRTS")
# Coerce random points back to an sf object 
target_grid_resample<- st_as_sf(target_grid_resample)
#assign repeat of 1
target_grid_resample$Repeat_Visit<-1
target_grid_resample<-subset(target_grid_resample, select=c("unique_gri", "Repeat_Visit"))
target_cells<-full_join(target_cells, target_grid_resample%>% as.data.frame(), by="unique_gri")
  
#select which sites should be surveyed thrice by using GRTS on the target GRTS pull
num_thrice<- round(total_strata*thrice, 0)
resample<-subset(target_cells, select=c("unique_gri", "Repeat_Visit"))
resample1<-resample[!is.na(resample$Repeat_Visit),]
#create a subset removing twice repeats
resample2<-resample[is.na(resample$Repeat_Visit),]
resample2<-subset(resample2, select=c("unique_gri"))
target_grid_resample2<- sdraw(as(resample2, "Spatial"), n = num_thrice, type="GRTS")
# Coerce random points back to an sf object 
target_grid_resample2<- st_as_sf(target_grid_resample2)
#assign repeat of 2
target_grid_resample2$Repeat_Visit<-2
resample2<-subset(target_grid_resample2, select=c("unique_gri", "Repeat_Visit"))
target_repeats<-bind_rows(resample1, resample2)
#create a simple data frame for joining back to spatial target dataset
#target_repeats<-subset(target_repeats, select=c("unique_gri", "Repeat_Visit"))
#repeats_combined<-full_join(target_cells, target_repeats%>% as.data.frame(), by="unique_gri")
#repeats_combined<-subset(as.data.frame(repeats_combined), select=c("unique_gri", "Repeat_Visit.y.y"))

#Combine with repeat row with final datset
inter<-full_join(target_grid, target_repeats%>% as.data.frame(), by="unique_gri")

#######################Clean up and wrangle
########################################################################################

#Clean up original target and backup columns and combine into Site_rank
#Add target rank
for ( i in 1: nrow(inter)){
  inter$Site_Rank[i] <-inter$siteID.x[i] 
}
#Add target rank
for ( i in 1: nrow(inter)){
  if (is.na(inter$Site_Rank[i])) {
    inter$Site_Rank[i] <-inter$siteID.y[i] 
  }
  else {NULL}
}
inter<-as.data.frame(subset(inter, select=c("unique_gri", "Site_Rank", "Repeat_Visit", "strata")))

#Add symbology
for ( i in 1: nrow(inter)){
  if (grepl("TARGET*", inter$Site_Rank[i])){
    inter$symbol[i] <-"Target" 
  }
  else if (grepl("BACKUP*", inter$Site_Rank[i])){
    inter$symbol[i] <-"Backup" 
  }
  else {
    inter$symbol[i] <-"Not Selected" 
  }
  
}

#Join back to original shapefile and clean columns
inter_2<-full_join(area_raw, inter, by="unique_gri")
inter_2<-distinct(inter_2, unique_gri, .keep_all = TRUE)

#Add symbology
for ( i in 1: nrow(inter_2)){
  if (grepl("TARGET*", inter_2$Site_Rank[i])){
    inter_2$symbol[i] <-"Target" 
  }
  else if (grepl("BACKUP*", inter_2$Site_Rank[i])){
    inter_2$symbol[i] <-"Backup" 
  }
  else if (grepl("No*", inter_2$Include[i])){
    inter_2$symbol[i] <-"Excluded" 
  }
  else {
    inter_2$symbol[i] <-"Not Selected" 
  }
  
}

# #Assign backups
# 
# #subset of only backup cells
# tar_inter<-subset(inter_2,grepl("TARGET*", Site_Rank))
# bu_inter<-subset(inter_2,grepl("BACKUP*", Site_Rank))
# bu_inter_ut0<-filter(bu_inter, strata == "UT_0")
# bu_inter_ut1<-filter(bu_inter, strata == "UT_1")
# bu_inter_ut2<-filter(bu_inter, strata == "UT_2")
# bu_inter_co0<-filter(bu_inter, strata == "CO_0")
# bu_inter_co1<-filter(bu_inter, strata == "CO_1")
# bu_inter_co2<-filter(bu_inter, strata == "CO_2")
# 
# #TRYING
# #just one cell
# test_c<-subset(tar_inter, tar_inter$Site_Rank == "TARGET-19")
# 
# ## Set up container for results
# n <- length(tar_inter)
# nearestBackups <- character(n)
# 
# for (i in seq_along(nearestBackups)) {
#   nearestBackups[i] <- bu_inter$Site_Rank[which.min(gDistance(bu_inter[i,], pUTM, byid=TRUE))]
# }

#######################Export
########################################################################################

#clean for export to shapefile
GRTS_CO_UT_GridRanks<-subset(inter_2, select=c("unique_gri","KMs", "Include", "Site_Rank","Repeat_Visit", "REGION", "strata", "FREQUENCY", "SUM_Length", "Conifer_Ha", "SUM_Area_S", "Z_Mean", "symbol" ))
#mapview(GRTS_CO_UT_GridRanks, zcol = "Site_Rank")
st_write(GRTS_CO_UT_GridRanks, "C:/Users/lizmoore/Documents/Local_Working/Pinyon_Jay/GRTS/GRTS_CO_UT_GridRanks_V4_20190221.shp")

