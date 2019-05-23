##########################################################################################################################
## Code for generating choropleth map of NZ from Stats NZ meshblock/census data                                         ##
##                                                                                                                      ##
## -- The stats NZ 2013 census meshblock data is avaliable from                                                         ##
## http://archive.stats.govt.nz/Census/2013-census/data-tables/meshblock-dataset.aspx                                   ##
##                                                                                                                      ##
## There are datasets avaliable for each region and multiple csv files in the zips that can be downloaded.              ##
## These provide estimates for the number of residents within each meshblock and info on gender, age, ethnicity ect     ##
## (all listed on the site). This kind of information is what will colour/fill our choropleth.                          ##
##                                                                                                                      ##                                                                                                                   
## --- The stats NZ 2013 census meshblock shapefiles are avaliable from                                                 ##
## http://archive.stats.govt.nz/browse_for_stats/Maps_and_geography/Geographic-areas/digital-boundary-files.aspx        ##
##                                                                                                                      ##
## For the 2013 census you only have the option to download the shape file for the entire of NZ (New Zealand 2013 under ##
## ESRI shapefiles header) rather than just certain regions. This shape file has lat and long info that will draw the   ##
## outlines for our polygons (i.e. meshblocks). This file also contains other area sizes e.g territory area, regional   ##
## council area ect.                                                                                                    ##
##                                                                                                                      ##
## Note: 2013 census data used as it is more reliable than the most recent 2018 census (which had low response rate)    ##
##########################################################################################################################

## loading appropriate packages ##
library(openxlsx)
library(rgdal)
library(broom)
library(dplyr)
library(ggplot2)

################################ 
## Code dealing to shape file ##
################################

## reading in the NZ shape file --- dsn = set the path to the folder where the shp file is ##
shp <- readOGR(dsn ="H:/R/Mapping R/NZ_maps/2013 Digital Boundaries High Def Clipped" ,layer="MB2013_HD_Clipped") # produces a large spatialpolygonsDataframe (basically a massive list of lists)

# Turning the spatialpolygon df which has the long and lat information into a tidy tibble -- note 1.many tutorials use fortify() which does the same thing as below
# but the R help says the fortify() function may be deprecated in future, so better to use tidy() from the broom package. 2. Since the df contains info
# on all of nz this line of code can take a little while to run (~5mins).
tidy_nzdf <- tidy(shp)

# Pulling out the polygon id to match on when trying to merge with meshblock id 
shp$polyID <- sapply(slot(shp, "polygons"), function(x) slot(x, "ID"))
# Merge to get meshblock ids (again can take a couple mins)
nz_df <- merge(tidy_nzdf, shp, by.x = "id", by.y="polyID")

## To make things easier when it comes to plotting best to filter out parts of the dataframe that aren't the region of interest ##

## I want to look at Palmerston North and Christchurch (my old home and new home) ##
## remove everything that is not Palmy ##
Man_df <- nz_df[which(nz_df$TA2013_NAM=="Palmerston North City"),]

## remove everything that is not Christchurch ##
Chch_df <- nz_df[which(nz_df$TA2013_NAM=="Christchurch City"),]

####################################################
## Code incorporating census data with shape file ##
####################################################

## Christchurch ##
pop_data_Chch <- read.xlsx("./2013_mb_dataset_Canterbury_Region/2013-mb-dataset-Canterbury-Region-individual-part-2.xlsx", sheet=4, startRow=9) # format not the nicest
pop_Income_chch <- pop_data_Chch[,c(1,184)] # pulling out just meshblock and median personal income
# cleaning # 
colnames(pop_Income_chch) <- c("MB2013", "Median_Income")
pop_Income_chch <- pop_Income_chch[-1,]
pop_Income_chch$MB2013 <- gsub("[^0-9]", "", pop_Income_chch$MB2013) # removing MB from infront of id number 
# merging with the shape file #
Chch_full <- merge(Chch_df, pop_Income_chch, by.x='MB2013', by.y='MB2013', all.x=TRUE)
Chch_full$Median_Income <- as.numeric(Chch_full$Median_Income) # warning NA's introduced -- due to ..C and * in dataset that were used instead of NA 

# Palmy #
pop_data_Man <- read.xlsx("./2013_mb_dataset_Manawatu_Wanganui_Region/2013-mb-dataset-Manawatu-Wanganui-Region-individual-part-2.xlsx", sheet=4, startRow=9)
pop_Income_Man <- pop_data_Man[,c(1,184)] # pulling out just meshblock and median personal income
# cleaning # 
colnames(pop_Income_Man) <- c("MB2013", "Median_Income")
pop_Income_Man <- pop_Income_Man[-1,]
pop_Income_Man$MB2013 <- gsub("[^0-9]", "", pop_Income_Man$MB2013) # removing MB from infront of id number 
# merging with the shape file #
Man_full <- merge(Man_df, pop_Income_Man, by.x='MB2013', by.y='MB2013', all.x=TRUE)
Man_full$Median_Income <- as.numeric(Man_full$Median_Income)

###=======================================================================================================================================#####

########################
## Plotting the maps! ## 
########################

# Christchurch #

# map with just the meshblocks #
Chchmap <- ggplot(data=Chch_df, aes(x=long, y=lat, group=group)) + geom_polygon(colour='black', fill='gray', size=0.1) 
  theme(line = element_blank(), axis.text=element_blank(),axis.title=element_blank(), panel.background = element_blank())

# map zoomed in on Chch city (removing banks pepeninsula area) and coloured by median income #
Income_Chchmap <- ggplot(data=Chch_full, aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Median_Income), colour='gray', 
                  size=0.1) + theme(line = element_blank(), axis.text=element_blank(),axis.title=element_blank(), 
                  panel.background = element_blank()) +
                  coord_cartesian(ylim=c(5190000,5170000), xlim=c(1560000, 1585000)) +
                  scale_fill_gradient2("#4d4dff", mid = "white", high = "#ff4d4d") + guides(fill=guide_legend(title="Median Income"))
Income_Chchmap

# Palmerston North # 
Income_Manmap <- ggplot(data=Man_full, aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Median_Income), colour='gray', 
                 size=0.1) + theme(line = element_blank(), axis.text=element_blank(),axis.title=element_blank(), 
                 panel.background = element_blank()) +# coord_cartesian(ylim=c(5525000,5535000), xlim=c(1817500, 1825000)) +
                 scale_fill_gradient2("#4d4dff", mid = "white", high = "#ff4d4d") +
                 guides(fill=guide_legend(title="Median Income"))
Income_Manmap

