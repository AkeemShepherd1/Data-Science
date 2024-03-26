
####read in parcel exposure to repeated tidal flooding information
setwd("")
require(data.table)

flood_data <- fread("fliq_data.csv", 
                    stringsAsFactors = F, 
                    data.table = F, 
                    colClasses=list(character=c(1)))

names(flood_data)
head(flood_data)
summary(flood_data)

####subset the flood data file to only look at parcels in the state of NY FIPS code = "36" 
####see:https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
#install.packages("stringr")
require(stringr)
head(flood_data$fsid)
flood_data$state <- substr(flood_data$fsid,1,2)

head(flood_data$state)
table(flood_data$state)

flood_data_NY <- subset(flood_data, state != "00")
rm(flood_data)
gc()


####read in neighborhood information for each parcel

parcel_data<-fread("nynjct_census_amen_IDW_join.csv", 
                   stringsAsFactors = F, 
                   data.table = F, 
                   colClasses=list(character=c(1)))

head(parcel_data)
tail(parcel_data)

####create subset of only neghborhood data that exists in the flood stats data

head(parcel_data)
head(flood_data_NY)

parcel_data_sub <- subset(parcel_data, fsid.x %in% flood_data_NY$fsid)
head(parcel_data_sub)
summary(parcel_data_sub)

parcel_data_sub_noNA <- subset(parcel_data_sub, !is.na(race_divbg))
summary(parcel_data_sub_noNA)
rm(parcel_data,parcel_data_sub)
gc()

####Merge flood data with cleaned parcel data
names(parcel_data_sub_noNA)
names(flood_data_NY)


full_data <- merge(parcel_data_sub_noNA,
                   flood_data_NY,
                   by.x="fsid.x",
                   by.y="fsid")

head(full_data)
tail(full_data)
summary(full_data)


####Replace missing loss data with real zero's

full_data$best_lot_loss_num <- ifelse(is.na(full_data$best_lot_loss_num),
                                     0,
                                     full_data$best_lot_loss_num)

full_data$best_road_loss_num <- ifelse(is.na(full_data$best_road_loss_num),
                                       0,
                                       full_data$best_road_loss_num)

summary(full_data)

full_data$tot_loss <- full_data$best_lot_loss_num + full_data$best_road_loss_num


####Aggregate total loss to the county level 

head(full_data$fsid.x)
full_data$county <- substr(full_data$fsid.x,1,5)
head(full_data)
table(full_data$county)
table(full_data$state)

aggregate_loss_county <- aggregate(full_data$tot_loss,
                                   by=list(full_data$county),
                                   FUN=mean,
                                   na.rm = T)
head(aggregate_loss_county)


####Create loss Dataframe with county names attached and sorted

county_loss <- as.data.frame(aggregate_loss_county)
names(county_loss) <- c("CountyID","TotalLoss")
View(county_loss)

summary(county_loss$TotalLoss)
hist(county_loss$TotalLoss)

##Install the "bit64" package to give R the ability to properly manage 64bit integer data
##it's a good idea to have the package installed before you read in any text file, but
##in this case there are very large integer fields that can'd be read properly without it.
install.packages("bit64")

county_names <- fread("county_names.csv", 
                    stringsAsFactors = F, 
                    data.table = F, 
                    colClasses=list(character=c(5)))

summary(county_names)
head(county_names)
names <- names(county_names)
county_names <- county_names[,c("GEOID","LSAD")]

head(county_names)
head(county_loss)
head(county_names)

county_loss_w_name <- merge(county_loss,
                            county_names,
                            by.x="CountyID",
                            by.y="GEOID")
View(county_loss_w_name)

county_loss_w_name <- county_loss_w_name[order(county_loss_w_name$TotalLoss), ]
View(county_loss_w_name)

fwrite(county_loss_w_name, "loss_totals_by_county_ny.csv")
                      

####Assignment 2 (Due 09/20): 
#### 1) Recreate the code to produce a table of total loss by county for the state of NJ 
#### 2) Recreate the code to produce a table of total loss at the state level (aggregated to NY, NJ, CT in one table)
#### 3) Create a table showing the most diverse counties in NY using the "race_divbg" variable (racial diversity index)
#### 4) Write the new tables out to new .csv files

