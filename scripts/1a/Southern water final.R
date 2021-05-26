

# *Note: This code was poorly commented, written due to poor coding practices of times past. Apologies!*


# load libraries --------------------------------------------

library(plyr)
library(data.table)
library(sp)
library(sf)
library(raster)
library(maptools)
library(leaflet)
library(tmap)
library(rgdal)
library(units)
library(stringr)
library(dplyr)
library(magrittr)
library(purrr)
library(DescTools)
library(lubridate)
library(zoo)
library(scales)
library(gridExtra)


# set the plot theme and colours --------------------------

gg <- theme_bw() +
  theme(legend.position = "bottom")
SEAMScolours <-
  c(
    "#1F497D",
    "#C2332F",
    "#FFC000",
    "#00B050",
    "#F5750B",
    "#399BB6",
    "#999999",
    "#0072B2",
    "#D55E00",
    "#E69F00",
    "#009E73",
    "#56B4E9",
    "#CC79A7",
    "#F0E442"
  )



# PART 1 ----------------------------------------------------------------------------

# load the data

setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 1")
gis <-
  readOGR(dsn = "DMA Spatial Data/DistrictMeterAreas.tab", layer = "DistrictMeterAreas")
#writeOGR(gis, dsn=".", layer="gis", driver="ESRI Shapefile")

gis <- st_read("gis.shp")
gis <- st_as_sf(gis, crs = 27700)
str(gis)
gis <- gis[!duplicated(gis$DMA_Dsc), ]
tmap_mode("view")
#qtm(gis, basemaps = "Esri.WorldStreetMap") + tm_style("beaver")

dma.11 <- read.csv("DMA category analysis.csv")
dma.1 <- dma.11[, c(1, 54)]
non_dup <- dma.1[!duplicated(dma.1$DMA_Description), ]

dma.1$DMA_Description <- as.character(dma.1$DMA_Description)
str(dma.1$DMA_Description)
dim(gis)
dim(non_dup)

dma.2 <-
  inner_join(x = gis,
             y = non_dup,
             by = c("DMA_Dsc" = "DMA_Description"))
dma.2 <- st_as_sf(dma.2, crs = 27700)
st_crs(dma.2) <- 27700
tmap_mode("view")
#qtm(dma.2,fill="Category", basemaps = "Esri.WorldStreetMap") + tm_style("beaver")


#plot 3 categorical samples

dma.3 <-
  dma.2 %>%  left_join(dma.11, by = c("DMA_Dsc" = "DMA_Description"))
dma.4 <-
  dma.3 %>%  dplyr::filter(DMA_Dsc %in% c("Silverhill Parkview", "Hollington 1", "The Green"))
dma.4 <- dma.4[, c(2, 18:66, 70)]
dma.4 <- st_set_geometry(dma.4, NULL)

# convert a few wide columns to long format
dma.5 <- gather(dma.4, Date, Value, Jan.15:Jan.19, factor_key = TRUE)
dma.5$nDate <- as.yearmon(dma.5$Date, format = "%b.%y")
dma.5$nDate <- paste("01 ", dma.5$nDate, sep = "")
dma.5$nDate  <- dmy(dma.5$nDate)

# create the plots

p <- ggplot(dma.5) +
  geom_line(aes(x = nDate, y = Value, col = DMA_Dsc)) +
  labs(title = "title", y = "Volume", x = "Month") +
  theme(legend.position = "bottom")

p + scale_color_manual(
  labels = c(
    "Hollington 1 (Jumper)",
    "Silverhill Parkview (Creeper)",
    "The Green (Stable and Others)"
  ),
  values = c("blue", "red", "green")
)




# PART 2 ----------------------------------------------------------------------------

# load data

raw <-
  data.table(read.csv(
    "LeakageDetectionEffort.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))
raw2 <-
  data.table(read.csv(
    "Nightflow.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

# clean and transform the data

raw2 <- raw2[, c(2, 5)]
new <- left_join(x = raw,
                 y = raw2,
                 by = c("DMA.Code" = "DMA_Ref"))
colnames(new)[colnames(new) == "Eastings"] <- "X"
colnames(new)[colnames(new) == "Northings"] <- "Y"
new$X <- str_pad(new$X, 6, side = "left", pad = "0")
new$Y <- str_pad(new$Y, 6, side = "left", pad = "0")
new$X <- substr(new$X, 1, 6)
new$Y <- substr(new$Y, 1, 6)

new$X <- as.numeric(new$X)
new$Y <- as.numeric(new$Y)
summary(new$X)
summary(new$Y)
new <- na.omit(new)
new <- st_as_sf(new, coords = c("X", "Y"), crs = 27700)
tmap_mode("view")
new1 <- new %>%  filter(Ellipse.Work.Order == "9680118")
new2 <- new %>%  filter(Ellipse.Work.Order == "8275328")

#qtm(new, basemaps = "Esri.WorldStreetMap") + tm_style("beaver")

#write.csv(new, "LeakageDetectionEffort_v1.csv")



# PART 3 ----------------------------------------------------------------------------

# load data

setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 1")
mc1 <-
  data.table(read.csv(
    "MeterCoordinates.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

# clean, transform the data

mc <- mc1
mc$longitude <- mc$X_Latitude
mc$latitude <- mc$Y_Longitude
mc$X_Latitude <- NULL
mc$Y_Longitude <- NULL
mc$Y_longitude <- NULL

summary(mc)
mc <- mc %>% filter(!is.na(X_BNG))
mc <- mc %>% filter(!is.na(Y_BNG))
mc <- st_as_sf(mc, coords = c("X_BNG", "Y_BNG"), crs = 27700)
mc1 <- mc[10000:10100, ]
tmap_mode("view")
#qtm(mc1, basemaps = "Esri.WorldStreetMap") + tm_style("beaver")


# load data

setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 1")
new <-
  data.table(read.csv(
    "LeakageDetectionEffort_v1.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))
new <- new %>% filter(!is.na(X))
new <- new %>% filter(!is.na(Y))
new <- st_as_sf(new, coords = c("X", "Y"), crs = 27700)

# plot on a map

tmap_mode("view")
qtm(new, basemaps = "Esri.WorldStreetMap") + tm_style("beaver")

# load the data
setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 1")
gis <- st_read("gis.shp")

# remove dups and transofrm the crs

gis <- gis[!duplicated(gis$DMA_Dsc), ]
gis <- st_as_sf(gis)
st_crs(gis) <- 27700

# Calculate_Intersect

intersect <- st_intersects(mc, gis)
intersect <- as.data.frame(intersect)
mc$row.id <- seq.int(nrow(mc))
gis$col.id <- seq.int(nrow(gis))

# Get the names of the names of neighborhoods in buffer

mc_col.id <- left_join(intersect, as.data.frame(mc), by = "row.id")
combine <- left_join(mc_col.id, as.data.frame(gis), by = "col.id")
raw2 <-
  data.table(read.csv(
    "Nightflow.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))
raw2 <- raw2[, c(2, 5)]
combine2 <- left_join(combine, raw2, by = "DMA_Ref")
combine2 <- combine2[, c(3:66)]

# write the output data

setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 1")
combine2 <-
  as.data.frame(combine2) %>%  select(-c(geometry.x, geometry.y, DMA_Description))
write.csv(combine2, "MeterCoordinates_DMA.csv")

# load data

MeteredBy2015 <- read.csv("MeteredBy2015.csv")
dma.2 <- st_read("DMA_Category_Geometry.shp")

# merge datasets and transform the crs to the british grid 27700

gis_combine2 <-
  left_join(MeteredBy2015, as.data.frame(dma.2), by = "DMA_Ref")
gis_combine2$Category <- as.character(gis_combine2$Category)
gis_combine2$Category[which(is.na(gis_combine2$Category))] <-
  "Insufficient Data"
gis_combine2$Category[gis_combine2$Category == "No Data"] <-
  "Insufficient Data"
gis_combine2 <- st_as_sf(gis_combine2, crs = 27700)

# plot on a map

tmap_mode("view")
tm_shape(gis_combine2) + tm_polygons("Category",
                                     palette = c("orange", "grey", "coral2", "aquamarine2")) + tm_basemap("Esri.WorldStreetMap")


# bar plot of gis_combine

plot98 <- ggplot(gis_combine2, aes(Category)) +
  geom_bar(fill = c("orange", "coral2", "grey", "aquamarine2"),
           color = "black") +
  labs(title = "DMA Grouping", y = "Count", x = "Category") +
  theme(legend.position = "bottom")

str(gis_combine2)


# PART 4 ---------------------------------------------------------------------------------

# Load data

setwd("I:\\Projects\\Client\\1892\\Analytics")
west <-
  data.table(
    read.csv(
      "Southern temperatures_v1_West.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )
east <-
  data.table(
    read.csv(
      "Southern temperatures_v1_East.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )
central <-
  data.table(
    read.csv(
      "Southern temperatures_v1_Central.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )

# clean, transform and merge datasets

west$LTA.max <- NULL
west$LTA.min <- NULL
west$ID <- "west"

east$LTA.max <- NULL
east$LTA.min <- NULL
east$ID <- "east"
central$LTA.max <- NULL
central$LTA.min <- NULL
central$ID <- "central"
both <- rbind.data.frame(east, west, central)


both2 <- gather(both, Date1, Value, X2018:X2016, factor_key = FALSE)
both2$Date1 <- substring(both2$Date1, 2, 5)
both2$Date1 <- paste(both2$X, sep = "-", both2$Date1)
both2$X <- NULL
both3 <- both2
both3$Date1  <- dmy(both3$Date1)
both3 <- na.omit(both3)

both3$rowguid <- seq.int(nrow(both3))
both3$Freezing.Month <- "No"
both3 <- both3 %>%  mutate(Months1 = month(Date1))
both4 <-
  both3 %>%  filter(Value < 0) %>% mutate(Freezing.Month = "Yes")
both5 <- anti_join(both3, both4, by = "rowguid")

# calculate for freezing

both4x <- both4 %>% select(everything()) %>%
  filter(Freezing.Month == "Yes") %>%
  arrange(desc(Months1)) %>%
  #group_by(Months1) %>%
  summarise(averages = sum(Value) / length(Value))


# calculate for no freezing

both6 <- both5 %>% select(everything()) %>%
  filter(Freezing.Month == "No") %>%
  filter(Months1 <= 3) %>%
  arrange(desc(Months1)) %>%
  #group_by(Months1) %>%
  summarise(averages = sum(Value) / length(Value))

# clean, transform and merge datasets

both6$Freeze.events <- "No"
both4x$Freeze.events <- "Yes"
bothss <- rbind(both4x, both6)


# bar plot

as.data.frame(bothss) %>%
  ggplot(aes(x = Freeze.events, y = averages, group = 1),  col = SEAMScolours[1]) +
  labs(title = "Nightflow Tempearture Averages", x = "Freeze Events", y = "Average Temparature ?C")  +
  geom_bar(
    stat = "identity",
    show.legend = FALSE,
    fill = c("#FF6666", "blue")
  ) +
  ylim(-3, 7) + theme_bw() + coord_flip()




# Validated Nightflow ------------------------------------------------------
# PART 4

# load data
setwd("I:\\Projects\\Client\\1892\\Analytics")
vn.11 <-
  data.table(read.csv(
    "Validated Nightflow v2.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

# clean, transform data
vn.11 <- vn.11[, c(1:49)]
non_dup <- vn.11[!duplicated(vn.11$DMA.Code), ]
vn.11$DMA.Code <- as.character(vn.11$DMA.Code)

# check out the shape of your data

dim(gis)
dim(non_dup)

# add new columns via a join and plot on a map
vn.2 <-
  inner_join(x = gis[, c(1, 2, 14)],
             y = non_dup,
             by = c("DMA_Ref" = "DMA.Code"))
vn.2 <- st_as_sf(vn.2, crs = 27700)
tmap_mode("view")
qtm(dma.2, fill = "Category", basemaps = "Esri.WorldStreetMap") + tm_style("beaver")


#plot 3 categorical samples
#vn.3 <- vn.2 %>%  left_join(vn.11, by = c("DMA_Dsc" = "DMA_Description"))
#non_dup2 <- vn.3[!duplicated(vn.3$DMA_Description),]
#vn.4 <- vn.3 %>%  dplyr::filter(DMA_Dsc %in% c("Silverhill Parkview", "Hollington 1", "The Green"))
#vn.4 <- vn.4[,c(2,18:66,70)]
vn.2 <- st_set_geometry(vn.2, NULL)

# convert a few wide columns to long format

vn.5 <- gather(vn.2, Date1, Value, Jan.15:Dec.18, factor_key = TRUE)
vn.5$Date1 <- as.yearmon(vn.5$Date1, format = "%b.%y")
vn.5$Date1 <- paste("01 ", vn.5$Date1, sep = "")
vn.5$Date1  <- dmy(vn.5$Date1)
vn.6 <- vn.5 %>%
  mutate(Region = ifelse(Divisin == "H", "West",
                         (ifelse(
                           Divisin == "K", "East",
                           (ifelse(Divisin == "I", "West",
                                   (
                                     ifelse(Divisin == "S", "Central", "Other")
                                   )))
                         )))) %>%
  mutate(Divisin = NULL)

# load data

setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 1")
west2 <-
  data.table(
    read.csv(
      "Southern temperatures_v2_West.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )
east2 <-
  data.table(
    read.csv(
      "Southern temperatures_v2_East.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )
central2 <-
  data.table(
    read.csv(
      "Southern temperatures_v2_Central.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )


# transform data

west2$LTA.max <- NULL
west2$LTA.min <- NULL
west2$ID <- "west"
east2$LTA.max <- NULL
east2$LTA.min <- NULL
east2$ID <- "east"
central2$LTA.max <- NULL
central2$LTA.min <- NULL
central2$ID <- "central"
both_1 <- rbind.data.frame(east2, west2, central2)
both_1$X2019 <- NULL


both_2 <-
  gather(both_1, Date1, Value, X2018:X2015, factor_key = FALSE)
both_2$Date1 <- substring(both_2$Date1, 2, 5)
both_2$Date1 <- paste(both_2$X, sep = "-", both_2$Date1)
both_2$X <- NULL
both_3 <- both_2
both_3$Date1  <- dmy(both_3$Date1)
both_3 <- na.omit(both_3)

both_3$rowguid <- seq.int(nrow(both_3))
both_3$Freezing.Month <- "No"
both_3 <-
  both_3 %>% mutate(Freezing.Month = ifelse(Value < 0, "Yes", "No"))
both_3 <- both_3 %>%  mutate(Months1 = month(Date1))
both_3 <- both_3 %>%  mutate(Year1 = year(Date1))
both_3$MonthYear <- paste(both_3$Months1, sep = "-", both_3$Year1)


# bring in Validated Nightflow data
# extract month for both datset

vn.6$Region <- tolower(vn.6$Region)
vn.7 <- vn.6
vn.7$Value[which(is.na(vn.7$Value))] <- 0
vn.8 <-
  vn.7 %>% select(Date1, Value, Region) %>% group_by(Date1, Region) %>% summarise(ValueSum = sum(Value))
vn.8 <- vn.8 %>%  mutate(Months1 = month(Date1))
vn.8 <- vn.8 %>%  mutate(Year1 = year(Date1))
vn.8$MonthYear <- paste(vn.8$Months1, sep = "-", vn.8$Year1)
vn.9 <-  both_3 %>% filter(Value < 0)
vn.10 <-  both_3 %>% filter(Value > 0)


vn.9 <-
  vn.9 %>%  group_by(MonthYear, ID) %>% summarise(AvgTemperature = mean(Value)) %>%
  left_join(vn.8[, c(6, 2, 3)], by = c("MonthYear", "ID" = "Region"))
vn.9$MonthYear <- paste("01", vn.9$MonthYear, sep = "-")
vn.9$MonthYear  <- dmy(vn.9$MonthYear)

vn.10 <-
  vn.10 %>%  group_by(MonthYear, ID) %>% summarise(AvgTemperature = mean(Value)) %>%
  left_join(vn.8[, c(6, 2, 3)], by = c("MonthYear", "ID" = "Region"))
vn.10$MonthYear <- paste("01", vn.10$MonthYear, sep = "-")
vn.10$MonthYear  <- dmy(vn.10$MonthYear)


# calcuate for freezing

vn.9x <- vn.9 %>% select(everything()) %>%
  mutate(Months1 = month(MonthYear)) %>%
  group_by(Months1) %>%
  summarise(averages = sum(ValueSum) / length(ValueSum))
dfs2 <- data.frame(month = c(1, 2, 3))
mymonths2 <- c("Jan", "Feb", "Mar")

# add abbreviated month name

dfs2$MonthAbb <- mymonths2[dfs2$month]
vn.9x$Months1 <- dfs2$MonthAbb
as.data.frame(vn.9x) %>%
  mutate(Months1 = fct_relevel(Months1, "Jan", "Feb", "Mar")) %>%
  ggplot(aes(x = Months1, y = averages, group = 1),  col = SEAMScolours[1]) + geom_bar(stat = "identity")


# calculate for no freezing

vn.10x <- vn.10 %>% select(everything()) %>%
  mutate(Months1 = month(MonthYear)) %>%
  group_by(Months1) %>%
  summarise(averages = sum(ValueSum) / length(ValueSum))
dfs <- data.frame(month = c(1:12))
mymonths <-
  c("Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec")

# add abbreviated month name

dfs$MonthAbb <- mymonths[dfs$month]
vn.10x$Months1 <- dfs$MonthAbb
as.data.frame(vn.10x) %>%
  mutate(
    Months1 = fct_relevel(
      Months1,
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    )
  ) %>%
  ggplot(aes(x = Months1, y = averages, group = 1),  col = SEAMScolours[1]) + geom_bar(stat = "identity")


vn.10x$Freeze.events <- "No"
vn.9x$Freeze.events <- "Yes"
both_vn <- rbind(vn.9x, vn.10x)
both_vn <-  as.data.frame(both_vn[c(1:6), ])

both_vn %>%
  group_by(Freeze.events) %>%
  summarise(averages = sum(averages) / length(averages)) %>%
  ggplot(aes(x = Freeze.events, y = averages, group = 1),  col = SEAMScolours[1]) +
  labs(title = "Nightflow Averages", x = "Freeze Events", y = "Nightflow Value")  +
  geom_bar(
    stat = "identity",
    show.legend = FALSE,
    fill = c("#FF6666", "blue")
  ) + theme_bw()# + coord_flip()










# Extra work ---------------------------------------

# load data
hmr <-
  data.table(read.csv(
    "Household_meter_reads.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

# transform data

hmr$rowguid <- seq.int(nrow(hmr))
hmr$Post.Code <- gsub("[[:blank:]]", "", hmr$Post.Code)
combine3 <-
  combine2 %>% select(PostCode, County, Divisin) %>%  unique()
hmr_combine3 <-
  left_join(hmr, combine3, by = c("Post.Code" = "PostCode"))
hmr_combine3 <- hmr_combine3 %>%
  mutate(Region = ifelse(Divisin == "H", "West",
                         (ifelse(
                           Divisin == "K", "East",
                           (ifelse(Divisin == "I", "West",
                                   (
                                     ifelse(Divisin == "S", "Central", "Other")
                                   )))
                         ))))
hmr_combine3 <- st_as_sf(hmr_combine3, crs = 27700)


# plot on a map
tmap_mode("view")
tm_shape(hmr_combine3) + tm_polygons("Region", palette = c("orange", "coral2", "aquamarine2")) + tm_basemap("Esri.WorldStreetMap")




# PART 5 ----------------------------------------------------------

# load data
mhh1 <-
  data.table(read.csv(
    "mHH_consumption_2016-17.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))
mhh2 <-
  data.table(read.csv(
    "mHH_consumption_2017-18.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

# transform, merge data

mhh12 <- left_join(mhh1, mhh2, by = "Serial.Number")
mhh <- mhh12 %>%
  select(
    Serial.Number,
    Post.Code.x,
    MinOfReading.Date,
    Water_reads_Reading,
    MaxOfReading.Date,
    Water_reads_1_Reading,
    Initial.Reading.Date,
    Initial.Reading,
    Final.Reading.Date,
    Final.Read
  ) %>%
  rename(Initial.Reading.Date.2016 = MinOfReading.Date) %>%
  rename(Initial.Reading.2016 = Water_reads_Reading) %>%
  rename(Final.Reading.Date.2016 = MaxOfReading.Date) %>%
  rename(Final.Reading.2016 = Water_reads_1_Reading) %>%
  rename(Initial.Reading.Date.2017 = Initial.Reading.Date) %>%
  rename(Initial.Reading.2017 = Initial.Reading) %>%
  rename(Final.Reading.Date.2017 = Final.Reading.Date) %>%
  rename(Final.Reading.2017 = Final.Read) %>%
  rename(Post.Code = Post.Code.x) #rename column
mhh$Post.Code <- gsub("[[:blank:]]", "", mhh$Post.Code)
mhh_combine3 <-
  left_join(mhh, combine3, by = c("Post.Code" = "PostCode"))
mhh_combine3 <- mhh_combine3 %>%
  mutate(Region = ifelse(Divisin == "H", "West",
                         (ifelse(
                           Divisin == "K", "East",
                           (ifelse(Divisin == "I", "West",
                                   (
                                     ifelse(Divisin == "S", "Central", "Other")
                                   )))
                         ))))
mhh_combine3 <- st_as_sf(mhh_combine3, crs = 27700)
mhh_combine4 <- mhh_combine3
mhh_combine4$geometry.y <- NULL

# plot on a map

tmap_mode("view")
tm_shape(hmr_combine3) + tm_polygons("Region", palette = c("orange", "coral2", "aquamarine2")) + tm_basemap("Esri.WorldStreetMap")


# average mhh consumption

mhh$Final.Reading.Date.2016 <- dmy_hm(mhh$Final.Reading.Date.2016)
mhh$Initial.Reading.Date.2017 <-
  dmy_hm(mhh$Initial.Reading.Date.2017)
avg_mhh <-
  mhh %>% mutate(time.interval = difftime(Initial.Reading.Date.2017, Final.Reading.Date.2016, units =
                                            "days")) %>%
  mutate(reading.interval = Initial.Reading.2017 - Final.Reading.2016) %>%
  mutate(avg.daily.consumption = reading.interval / as.numeric(time.interval))
#fwrite(avg_mhh, "average mhh.csv")

readingNA <- is.na(avg_mhh$reading.interval)
avg_mhh <- avg_mhh %>% filter(!is.na(reading.interval))

zeros <- avg_mhh$reading.interval <= 0
avg_mhh <- avg_mhh[!zeros, ]

duped <- duplicated(avg_mhh$Serial.Number)
dupednames <- avg_mhh$Serial.Number[duped]

matches <- filter(avg_mhh, Serial.Number %in% dupednames)
consumption <-
  matches %>% select(
    Serial.Number,
    Final.Reading.Date.2016,
    Initial.Reading.Date.2017,
    reading.interval,
    time.interval
  ) %>%
  group_by(Serial.Number) %>%
  distinct(Initial.Reading.Date.2017, .keep_all = TRUE) %>%
  arrange(Final.Reading.Date.2016)  %>%
  filter(reading.interval < 500)

"%ni%" <- Negate("%in%")

# removing all of the entries for each meter that contains at least 1 negative 'Used'

negused <- consumption %>% filter(reading.interval < 0) %>%
  select(Serial.Number) %>% unlist()
nonegs <- consumption %>% filter(Serial.Number %ni% negused)

# making data frame with everyday of financial year 2018-2019 - and will then append daily rates

x <- seq.Date(ymd("2016-04-01"), by = 1, length.out = 711)
result <- data.frame(day = x)
avgdailyuse <- c()
consumption <- as.data.frame(consumption)
for (i in seq_along(result[["day"]])) {
  avgdailyuse[i] <-
    (sum(consumption[consumption$Final.Reading.Date.2016 < result[["day"]][i] &
                       consumption$Initial.Reading.Date.2017 >= result[["day"]][i], ][, 4])
     / length(consumption[consumption$Final.Reading.Date.2016 < result[["day"]][i] &
                            consumption$Initial.Reading.Date.2017 >= result[["day"]][i], ][, 4]))
}
result$avgdailyuse <- avgdailyuse



# filter to July - Dec 2016, Jan - June 2017

resultx1 <-
  result %>% filter(day >= "2016-07-01" & day <= "2016-12-31")
resultx2 <-
  result %>% filter(day >= "2017-01-01" & day <= "2017-06-30")
result1 <- rbind(resultx1, resultx2)


# plot results

w1 <- ggplot(result1) +
  geom_line(aes(x = day, y = avgdailyuse, col = "SEAMScolours[1]"), size =
              1.3) +
  labs(title = "Derived Consumption per Day (Household Customers)", y = "Average Daily Use /m3", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")

w1 + scale_color_manual(values = c(SEAMScolours[1]),
                        labels = c("Meters"))

# plot along side the temperature data

both_result <- left_join(result1, both3, by = c("day" = "Date1"))
w2 <- ggplot(both_result) +
  geom_line(aes(x = day, y = Value, col = "SEAMScolours[1]"), size = 1.3) +
  labs(title = "AMR Meter vs. Non-AMR Daily Temperature", y = "Temperature ?C", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")

w2 + scale_color_manual(values = c(SEAMScolours[1]),
                        labels = c("Temperature"))



# PART 6 ---------------------------------------------------------

hmr_combine2 <-
  left_join(hmr, unique(combine2[, c(17, 46, 47)]), by = c("Post.Code" =
                                                             "PostCode"))
res <- hmr_combine2[grep("Residential", hmr_combine2$Premise.Type),]


# Non residential

non_res <- anti_join(hmr_combine2, res, by = "Premise.Type")
non_res$Reading.Date <- substring(non_res$Reading.Date, 1, 10)
non_res$Reading.Date <- dmy(non_res$Reading.Date)
non_res <-
  non_res %>%  filter(!is.na(Reading.Date)) %>%  filter(!is.na(Reading))

# remove instances where reading = 0 & keep duplicate serial numbers

zeros <- non_res$Reading == 0
non_res <- non_res[!zeros, ]
duped <- duplicated(non_res$Serial.Number)
dupednames <- non_res$Serial.Number[duped]
matches1 <- filter(non_res, Serial.Number %in% dupednames)

consumption <-
  matches1 %>% select(Serial.Number, Reading.Date, Reading) %>%
  group_by(Serial.Number) %>%
  distinct(Reading.Date, .keep_all = TRUE) %>% # removing multiple readings on same day
  arrange(Reading.Date) %>% # ensure all entries are in right order
  mutate(Used = lead(Reading) - (Reading)) %>%
  mutate(Period = as.numeric(lead(Reading.Date) - Reading.Date)) %>%
  mutate(Rate = Used / Period) %>%
  mutate(Lead_date = lead(Reading.Date)) %>%
  filter(Period > 14) %>% # removing very short time periods - also removes NA
  filter(between(Used, 0, 20000)) # removing unusually large value - also removes NA (only keeps TRUE)

# making data frame with everyday of financial year 2018-2019 - and will then append daily rates

x <- seq.Date(ymd("2018-04-01"), by = 1, length.out = 362)
result <- data.frame(day = x)
avgdailyuse <- c()
max(consumption$Lead_date) - min(consumption$Reading.Date) # date difference
consumption <- as.data.frame(consumption)

# calculating sum of `Used` given condition divided by the number of occurences that satisfy this condition

calculate_used <- function(output, input) {
  for (i in seq_along(output[["day"]])) {
    avgdailyuse[i] <-
      (sum(input[input$Reading.Date < output[["day"]][i] &
                   input$Lead_date >= output[["day"]][i], ][, 4])
       / length(input[input$Reading.Date < output[["day"]][i] &
                        input$Lead_date >= output[["day"]][i], ][, 4]))
  }
  output$avgdailyuse <- avgdailyuse
  print(output)
}

calculate_used(result, consumption)


# Residential ppts

res$Reading.Date <- substring(res$Reading.Date, 1, 10)
res$Reading.Date <- dmy(res$Reading.Date)
res <-
  res %>%  filter(!is.na(Reading.Date)) %>%  filter(!is.na(Reading))

# remove instances where reading = 0 & keep duplicate serial numbers
zeros <- res$Reading == 0
res <- res[!zeros, ]
duped <- duplicated(res$Serial.Number)
dupednames <- res$Serial.Number[duped]
matches2 <- filter(res, Serial.Number %in% dupednames)

consumption1 <-
  matches2 %>% select(Serial.Number, Reading.Date, Reading) %>%
  group_by(Serial.Number) %>%
  distinct(Reading.Date, .keep_all = TRUE) %>% # removing multiple readings on same day
  arrange(Reading.Date) %>% # ensure all entries are in right order
  mutate(Used = lead(Reading) - (Reading)) %>%
  mutate(Period = as.numeric(lead(Reading.Date) - Reading.Date)) %>%
  mutate(Rate = Used / Period) %>%
  mutate(Lead_date = lead(Reading.Date)) %>%
  filter(Period > 14) %>% # removing very short time periods - also removes NA
  filter(between(Used, 0, 20000)) # removing unusually large value - also removes NA (only keeps TRUE)

# making data frame with everyday of financial year 2018-2019 - and will then append daily rates

x <- seq.Date(ymd("2018-04-01"), by = 1, length.out = 365)
result1 <- data.frame(day = x)
avgdailyuse <- c()
max(consumption1$Lead_date) - min(consumption1$Reading.Date) # date difference
consumption1 <- as.data.frame(consumption1)

# calculating sum of `Used` given condition divided by the number of occurences that satisfy this condition

calculate_used(result1, consumption1)
vn..1 <-  consumption1$vn_Ref[k] #then remove duplicates afterwards
calculate_used(result1, vn..1)


# plot on a line chart

w2 <- ggplot(result1) +
  geom_line(aes(x = month(), y = avgdailyuse, col = "SEAMScolours[1]"), size =
              1.3) +
  labs(title = "AMR Meter vs. Non-AMR Average Daily Use (non_Household Customers)", y = "Average Monthly Use /m3", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")
w2 + scale_color_manual(values = c(SEAMScolours[1]),
                        labels = c("Meters"))




# PART 7 test 17-18* --------------------------------------------------

# load data

mhh2 <-
  data.table(read.csv(
    "mHH_consumption_2017-18.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

# transform data

mhh2 <- mhh2 %>%
  select(
    Serial.Number,
    Post.Code,
    Initial.Reading.Date,
    Initial.Reading,
    Final.Reading.Date,
    Final.Read
  ) %>%
  rename(Initial.Reading.Date.2017 = Initial.Reading.Date) %>%
  rename(Initial.Reading.2017 = Initial.Reading) %>%
  rename(Final.Reading.Date.2017 = Final.Reading.Date) %>%
  rename(Final.Reading.2017 = Final.Read)
mhh2$Post.Code <- gsub("[[:blank:]]", "", mhh2$Post.Code)
mhh_combine3 <-
  left_join(mhh2, combine3, by = c("Post.Code" = "PostCode"))
mhh_combine3 <- mhh_combine3 %>%
  mutate(Region = ifelse(Divisin == "H", "West",
                         (ifelse(
                           Divisin == "K", "East",
                           (ifelse(Divisin == "I", "West",
                                   (
                                     ifelse(Divisin == "S", "Central", "Other")
                                   )))
                         ))))
mhh_combine3 <- st_as_sf(mhh_combine3, crs = 27700)

# calculate average mhh consumption

mhh2$Final.Reading.Date.2017 <- dmy_hm(mhh2$Final.Reading.Date.2017)
mhh2$Initial.Reading.Date.2017 <-
  dmy_hm(mhh2$Initial.Reading.Date.2017)
avg_mhh <-
  mhh2 %>% mutate(time.interval = difftime(Final.Reading.Date.2017, Initial.Reading.Date.2017, units =
                                             "days")) %>%
  mutate(reading.interval =  Final.Reading.2017 - Initial.Reading.2017) %>%
  mutate(avg.daily.consumption = reading.interval / as.numeric(time.interval))


readingNA <- is.na(avg_mhh$reading.interval)
avg_mhh <- avg_mhh %>% filter(!is.na(reading.interval))

zeros <- avg_mhh$reading.interval <= 0
avg_mhh <- avg_mhh[!zeros, ]

duped <- duplicated(avg_mhh$Serial.Number)
dupednames <- avg_mhh$Serial.Number[duped]

matches <- filter(avg_mhh, Serial.Number %in% dupednames)
consumption <-
  matches %>% select(
    Serial.Number,
    Initial.Reading.Date.2017,
    Final.Reading.Date.2017,
    reading.interval,
    time.interval
  ) %>%
  group_by(Serial.Number) %>%
  distinct(Initial.Reading.Date.2017, .keep_all = TRUE) %>%
  arrange(Final.Reading.Date.2017) %>%
  mutate(Rate = reading.interval / as.numeric(time.interval))

"%ni%" <- Negate("%in%")

# making data frame with everyday of financial year 2018-2019 - and will then append daily rates

x <- seq.Date(ymd("2017-01-01"), by = 1, length.out = 365)
result <- data.frame(day = x)
avgdailyuse <- c()
consumption <- as.data.frame(consumption)
for (i in seq_along(result[["day"]])) {
  avgdailyuse[i] <-
    (sum(consumption[consumption$Initial.Reading.Date.2017 < result[["day"]][i] &
                       consumption$Final.Reading.Date.2017 >= result[["day"]][i], ][, 6])
     / length(consumption[consumption$Initial.Reading.Date.2017 < result[["day"]][i] &
                            consumption$Final.Reading.Date.2017 >= result[["day"]][i], ][, 6]))
}
result$avgdailyuse <- avgdailyuse

# load data

setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 1")
west2 <-
  data.table(
    read.csv(
      "Southern temperatures_v2_West.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )
east2 <-
  data.table(
    read.csv(
      "Southern temperatures_v2_East.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )
central2 <-
  data.table(
    read.csv(
      "Southern temperatures_v2_Central.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )

# transform data

west2$LTA.max <- NULL
west2$LTA.min <- NULL
west2$ID <- "west"
east2$LTA.max <- NULL
east2$LTA.min <- NULL
east2$ID <- "east"
central2$LTA.max <- NULL
central2$LTA.min <- NULL
central2$ID <- "central"
both_1 <- rbind.data.frame(east2, west2, central2)
both_1$X2019 <- NULL
both_1 <- both_1[, c(1, 4, 6)]

both_2 <- gather(both_1, Date1, Value, X2017, factor_key = FALSE)
both_2$Date1 <- substring(both_2$Date1, 2, 5)
both_2$Date1 <- paste(both_2$X, sep = "-", both_2$Date1)
both_2$X <- NULL
both_3 <- both_2
both_3$Date1  <- dmy(both_3$Date1)
both_3 <- na.omit(both_3)

consumption1718 <-
  both_3 %>%  group_by(Date1) %>% summarise(AvgTemperature = mean(Value)) %>%
  left_join(result, by = c("Date1" = "day"))


# check out the plots

ggplot(consumption1718[c(120:300), c(1, 2)]) +
  geom_line(aes(x = Date1, y = AvgTemperature, col = "SEAMScolours[1]"), size =
              1.3) +
  labs(title = "Average Daily Temperature", y = "Temperature ?C", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")
ggplot(consumption1718[c(120:300), c(1, 3)]) +
  geom_line(aes(x = Date1, y = avgdailyuse, col = "SEAMScolours[1]"), size =
              1.3) +
  labs(title = "Average Daily Consumption", y = "Average Daily Consumption", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")

consumption1718 %>%
  ggplot(both_result) +
  geom_line(aes(x = day, y = Value, col = "SEAMScolours[1]"), size = 1.3) +
  labs(title = "AMR Meter vs. Non-AMR Daily Temperature", y = "Temperature ?C", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")

# long format plot
h <- consumption1718[c(120:300), ] %>%
  gather(Category, Value, AvgTemperature:avgdailyuse, factor_key = FALSE)

ggplot(h) +
  geom_line(aes(x = Date1, y = Value, col = Category)) +
  labs(title = "title", y = "Temp and Consumption", x = "Month") +
  gg +  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")
#+ facet_wrap(~ Freeze.events) + theme_bw() + coord_flip()




# PART 7 test 16-17 ----------------------------------------------

# load data

mhh <-
  data.table(read.csv(
    "mHH_consumption_2016-17.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

# transform data

mhh <- mhh %>%
  select(
    Serial.Number,
    Post.Code,
    MinOfReading.Date,
    Water_reads_Reading,
    MaxOfReading.Date,
    Water_reads_1_Reading
  ) %>%
  rename(Initial.Reading.Date.2016 = MinOfReading.Date) %>%
  rename(Initial.Reading.2016 = Water_reads_Reading) %>%
  rename(Final.Reading.Date.2016 = MaxOfReading.Date) %>%
  rename(Final.Reading.2016 = Water_reads_1_Reading)
mhh$Post.Code <- gsub("[[:blank:]]", "", mhh$Post.Code)
mhh_combine3 <-
  left_join(mhh, combine3, by = c("Post.Code" = "PostCode"))
mhh_combine3 <- mhh_combine3 %>%
  mutate(Region = ifelse(Divisin == "H", "West",
                         (ifelse(
                           Divisin == "K", "East",
                           (ifelse(Divisin == "I", "West",
                                   (
                                     ifelse(Divisin == "S", "Central", "Other")
                                   )))
                         ))))
mhh_combine3 <- st_as_sf(mhh_combine3, crs = 27700)

# average mhh consumption

mhh$Final.Reading.Date.2016 <- dmy_hm(mhh$Final.Reading.Date.2016)
mhh$Initial.Reading.Date.2016 <-
  dmy_hm(mhh$Initial.Reading.Date.2016)
avg_mhh <-
  mhh %>% mutate(time.interval = difftime(Final.Reading.Date.2016, Initial.Reading.Date.2016, units =
                                            "days")) %>%
  mutate(reading.interval =  Final.Reading.2016 - Initial.Reading.2016)


readingNA <- is.na(avg_mhh$reading.interval)
avg_mhh <- avg_mhh %>% filter(!is.na(reading.interval))

zeros <- avg_mhh$reading.interval <= 0
avg_mhh <- avg_mhh[!zeros, ]

duped <- duplicated(avg_mhh$Serial.Number)
dupednames <- avg_mhh$Serial.Number[duped]

matches1 <- filter(avg_mhh, Serial.Number %in% dupednames)
consumption1 <-
  matches1 %>% select(
    Serial.Number,
    Initial.Reading.Date.2016,
    Final.Reading.Date.2016,
    reading.interval,
    time.interval
  ) %>%
  group_by(Serial.Number) %>%
  distinct(Initial.Reading.Date.2016, .keep_all = TRUE) %>%
  arrange(Final.Reading.Date.2016) %>%
  mutate(Rate = reading.interval / as.numeric(time.interval)) #%>%
#filter(reading.interval < 500)
#filter(time.interval > 14) %>% # removing very short time periods - also removes NA

"%ni%" <- Negate("%in%")

# making data frame with everyday of financial year 2018-2019 - and will then append daily rates

x <- seq.Date(ymd("2016-01-01"), by = 1, length.out = 365)
result1 <- data.frame(day = x)
avgdailyuse1 <- c()
consumption1 <- as.data.frame(consumption1)
for (i in seq_along(result[["day"]])) {
  avgdailyuse1[i] <-
    (sum(consumption1[consumption1$Initial.Reading.Date.2016 < result[["day"]][i] &
                        consumption1$Final.Reading.Date.2016 >= result[["day"]][i], ][, 6])
     / length(consumption1[consumption1$Initial.Reading.Date.2016 < result[["day"]][i] &
                             consumption1$Final.Reading.Date.2016 >= result[["day"]][i], ][, 6]))
}
result1$avgdailyuse <- avgdailyuse1



# load data

setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 1")
west2 <-
  data.table(
    read.csv(
      "Southern temperatures_v2_West.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )
east2 <-
  data.table(
    read.csv(
      "Southern temperatures_v2_East.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )
central2 <-
  data.table(
    read.csv(
      "Southern temperatures_v2_Central.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )

# transform data

west2$LTA.max <- NULL
west2$LTA.min <- NULL
west2$ID <- "west"
east2$LTA.max <- NULL
east2$LTA.min <- NULL
east2$ID <- "east"
central2$LTA.max <- NULL
central2$LTA.min <- NULL
central2$ID <- "central"
both_1 <- rbind.data.frame(east2, west2, central2)
both_1$X2019 <- NULL
both_1 <- both_1[, c(1, 4, 6)]

both_2 <- gather(both_1, Date1, Value, X2016, factor_key = FALSE)
both_2$Date1 <- substring(both_2$Date1, 2, 5)
both_2$Date1 <- paste(both_2$X, sep = "-", both_2$Date1)
both_2$X <- NULL
both_3 <- both_2
both_3$Date1  <- dmy(both_3$Date1)
both_3 <- na.omit(both_3)

consumption1617 <-
  both_3 %>%  group_by(Date1) %>% summarise(AvgTemperature = mean(Value)) %>%
  left_join(result1, by = c("Date1" = "day"))

# plot on line charts

ggplot(consumption1617[c(120:300), c(1, 2)]) +
  geom_line(aes(x = Date1, y = AvgTemperature, col = "SEAMScolours[1]"), size =
              1.3) +
  labs(title = "Average Daily Temperature", y = "Temperature ?C", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")
ggplot(consumption1617[c(120:300), c(1, 3)]) +
  geom_line(aes(x = Date1, y = avgdailyuse, col = "SEAMScolours[1]"), size =
              1.3) +
  labs(title = "Average Daily Consumption", y = "Average Daily Consumption", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")

consumption1617 %>%
  ggplot(both_result) +
  geom_line(aes(x = day, y = Value, col = "SEAMScolours[1]"), size = 1.3) +
  labs(title = "AMR Meter vs. Non-AMR Daily Temperature", y = "Temperature ?C", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")

# long format plot

h1 <- consumption1617[c(120:300), ] %>%
  gather(Category, Value, AvgTemperature:avgdailyuse, factor_key = FALSE)

ggplot(h1) +
  geom_line(aes(x = Date1, y = Value, col = Category)) +
  labs(title = "title", y = "Temp and Consumption", x = "Month") +
  gg +  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")
#+ facet_wrap(~ Freeze.events) + theme_bw() + coord_flip()


consumption161718 <-  rbind(consumption1617, consumption1718)
ggplot(consumption161718[, c(1, 3)]) +
  geom_line(aes(x = Date1, y = avgdailyuse, col = "SEAMScolours[1]"), size =
              1.3) +
  labs(title = "Average Daily Consumption", y = "Average Daily Consumption", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")



# PART 7 ALL of them >> 16-19 ---------------------------------------------------

# average consumption per customer for each date between 1 July 2016 and 31 December 2017

# load data first

mhh1 <-
  data.table(read.csv(
    "mHH_consumption_2016-17.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))
mhh2 <-
  data.table(read.csv(
    "mHH_consumption_2017-18.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

# transform data

mhh12 <- left_join(mhh1, mhh2, by = "Serial.Number")
mhh <- mhh12 %>%
  select(
    Serial.Number,
    Post.Code.x,
    MinOfReading.Date,
    Water_reads_Reading,
    MaxOfReading.Date,
    Water_reads_1_Reading,
    Initial.Reading.Date,
    Initial.Reading,
    Final.Reading.Date,
    Final.Read
  ) %>%
  rename(Initial.Reading.Date.2016 = MinOfReading.Date) %>%
  rename(Initial.Reading.2016 = Water_reads_Reading) %>%
  rename(Final.Reading.Date.2016 = MaxOfReading.Date) %>%
  rename(Final.Reading.2016 = Water_reads_1_Reading) %>%
  rename(Initial.Reading.Date.2017 = Initial.Reading.Date) %>%
  rename(Initial.Reading.2017 = Initial.Reading) %>%
  rename(Final.Reading.Date.2017 = Final.Reading.Date) %>%
  rename(Final.Reading.2017 = Final.Read) %>%
  rename(Post.Code = Post.Code.x) #rename column
mhh$Final.Reading.Date.2016 <- dmy_hm(mhh$Final.Reading.Date.2016)
mhh$Initial.Reading.Date.2017 <-
  dmy_hm(mhh$Initial.Reading.Date.2017)
mhh$Final.Reading.Date.2017 <- dmy_hm(mhh$Final.Reading.Date.2017)
mhh$Initial.Reading.Date.2016 <-
  dmy_hm(mhh$Initial.Reading.Date.2016)
mhh <- mhh %>%  select(everything()) %>%
  group_by(Serial.Number) %>%
  #  filter(Serial.Number == "*AM071157") %>%    #sample
  arrange(Final.Reading.Date.2016)  %>%
  mutate(Date_2016 = Final.Reading.Date.2016) %>%
  mutate(Reading_2016 = Final.Reading.2016) %>%
  mutate(Date_2017 = Initial.Reading.Date.2017) %>%
  mutate(Reading_2017 = Initial.Reading.2017)

# final reading date 2016 + 1 and initial reading date 2017 - 1

avg_mhhx <-
  avg_mhhxx %>% mutate(Date_2016 = Date_2016 + days(1)) %>%
  mutate(Date_2017 = Date_2017 - days(1))
avg_mhhx <-
  mhh %>% rename(Initial.Date = Initial.Reading.Date.2016) %>%
  rename(Initial.Reading = Initial.Reading.2016) %>%
  rename(Final.Date = Final.Reading.Date.2016) %>%
  rename(Final.Reading = Final.Reading.2016) %>%
  rename(Initial.Date2 = Initial.Reading.Date.2017) %>%
  rename(Initial.Reading2 = Initial.Reading.2017) %>%
  rename(Final.Date2 = Final.Reading.Date.2017) %>%
  rename(Final.Reading2 = Final.Reading.2017)

# read in data from here

avg_mhhx <- fread("avg_mhhxx.csv")

# transform data

avg_mhh1 <- avg_mhhx[, c(1:6)]

avg_mhh2 <- avg_mhhx[, c(1:2, 7:10)]
avg_mhh2 <- avg_mhh2 %>% rename(Initial.Date = Initial.Date2) %>%
  rename(Initial.Reading = Initial.Reading2) %>%
  rename(Final.Date = Final.Date2) %>%
  rename(Final.Reading = Final.Reading2)

avg_mhh3 <- avg_mhhx[, c(1:2, 11:14)]
avg_mhh3 <- avg_mhh3 %>% rename(Initial.Date = Date_2016) %>%
  rename(Initial.Reading = Reading_2016) %>%
  rename(Final.Date = Date_2017) %>%
  rename(Final.Reading = Reading_2017)

avg_mhh4 <- rbind(avg_mhh1, avg_mhh2, avg_mhh3)
avg_mhh4 <-  avg_mhh4 %>%  arrange(Serial.Number)


#avg_mhh4 %>%  filter(Serial.Number == "*AM071157")
avg_mhh4$Post.Code <- gsub("[[:blank:]]", "", avg_mhh4$Post.Code)
mhh_combine41 <-
  left_join(avg_mhh4, unique(combine2[, c(16, 17, 46, 47)]), by = c("Post.Code" =
                                                                      "PostCode"))
mhh_combine41 <- mhh_combine41 %>% filter(!is.na(Final.Reading))
zeros1 <- mhh_combine41$Final.Reading <= 0
mhh_combine41 <- mhh_combine41[!zeros1, ]

consumption3 <-
  mhh_combine41 %>% select(Serial.Number,
                           Final.Date,
                           Initial.Date,
                           Initial.Reading,
                           Final.Reading) %>%
  group_by(Serial.Number) %>%
  unique() %>%
  arrange(Initial.Date)  %>%
  mutate(Used = Final.Reading - Initial.Reading) %>%
  mutate(Period = as.numeric(Final.Date - Initial.Date)) %>%
  mutate(Rate = Used / Period)  %>%
  filter(Period > 14) %>% # removing very short time periods - also removes NA
  filter(between(Used, 0, 2000)) # removing unusually large value - also removes NA (only keeps TRUE)
x3 <- seq.Date(ymd("2016-11-01"), by = 1, length.out = 365)
result2 <- data.frame(day = x3)
avgdailyuse <- c()
max(consumption3$Initial.Date) - min(consumption3$Final.Date) # date difference
consumption3 <- as.data.frame(consumption3)

# calculating sum of `Used` given condition divided by the number of occurences that satisfy this condition

calculate_AvgUsed <- function(output, input) {
  for (i in seq_along(output[["day"]])) {
    avgdailyuse[i] <-
      (sum(input[input$Initial.Date < output[["day"]][i] &
                   input$Final.Date >= output[["day"]][i], ][, 8])
       / length(input[input$Initial.Date < output[["day"]][i] &
                        input$Final.Date >= output[["day"]][i], ][, 8]))
  }
  output$avgdailyuse <- avgdailyuse
  print(output)
}

number1 <- calculate_AvgUsed(result2, consumption3)
w3 <- ggplot(number1) +
  geom_line(aes(x = day, y = avgdailyuse, col = "SEAMScolours[1]"), size =
              1.3) +
  labs(title = "Derived Consumption per day", y = "Average Daily Use /m3", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")
w3 + scale_color_manual(values = c(SEAMScolours[1]),
                        labels = c("Meters"))






# Residential

mhh_combine41res <-
  mhh_combine41[grep("Residential", mhh_combine41$Premise.Type),]
mhh_combine41nonres <-
  anti_join(mhh_combine41, mhh_combine41res, by = "Premise.Type")
consumption4 <-
  mhh_combine41res %>% select(Serial.Number,
                              Final.Date,
                              Initial.Date,
                              Initial.Reading,
                              Final.Reading) %>%
  group_by(Serial.Number) %>%
  unique() %>%
  arrange(Initial.Date)  %>%
  mutate(Used = Final.Reading - Initial.Reading) %>%
  mutate(Period = as.numeric(Final.Date - Initial.Date)) %>%
  mutate(Rate = Used / Period)  %>%
  filter(Period > 14) %>% # removing very short time periods - also removes NA
  filter(between(Used, 0, 500)) # removing unusually large value - also removes NA (only keeps TRUE)
x4 <- seq.Date(ymd("2016-10-01"), by = 1, length.out = 365)
result4 <- data.frame(day = x4)
avgdailyuse <- c()
max(consumption3$Initial.Date) - min(consumption3$Final.Date) # date difference
consumption4 <- as.data.frame(consumption4)
# calculating sum of `Used` given condition divided by the number of occurences that satisfy this condition
calculate_AvgUsed <- function(output, input) {
  for (i in seq_along(output[["day"]])) {
    avgdailyuse[i] <-
      (sum(input[input$Initial.Date < output[["day"]][i] &
                   input$Final.Date >= output[["day"]][i], ][, 8])
       / length(input[input$Initial.Date < output[["day"]][i] &
                        input$Final.Date >= output[["day"]][i], ][, 8]))
  }
  output$avgdailyuse <- avgdailyuse
  print(output)
}

number1_a <- calculate_AvgUsed(result4, consumption4)
w4 <- ggplot(number1_a) +
  geom_line(aes(x = day, y = avgdailyuse, col = "SEAMScolours[1]"), size =
              1.3) +
  labs(title = "Derived Consumption per day", y = "Average Daily Use /m3", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")
w4 + scale_color_manual(values = c(SEAMScolours[1]),
                        labels = c("Meters"))

# Non Residential

mhh_combine41nonres <-
  anti_join(mhh_combine41, mhh_combine41res, by = "Premise.Type")
consumption5 <-
  mhh_combine41nonres %>% select(Serial.Number,
                                 Final.Date,
                                 Initial.Date,
                                 Initial.Reading,
                                 Final.Reading) %>%
  group_by(Serial.Number) %>%
  unique() %>%
  arrange(Initial.Date)  %>%
  mutate(Used = Final.Reading - Initial.Reading) %>%
  mutate(Period = as.numeric(Final.Date - Initial.Date)) %>%
  mutate(Rate = Used / Period)  %>%
  filter(Period > 14) %>% # removing very short time periods - also removes NA
  filter(between(Used, 0, 500)) # removing unusually large value - also removes NA (only keeps TRUE)
x5 <- seq.Date(ymd("2016-11-15"), by = 1, length.out = 330)
result5 <- data.frame(day = x5)
avgdailyuse <- c()
consumption5 <- as.data.frame(consumption5)

# calculating sum of `Used` given condition divided by the number of occurences that satisfy this condition

calculate_AvgUsed <- function(output, input) {
  for (i in seq_along(output[["day"]])) {
    avgdailyuse[i] <-
      (sum(input[input$Initial.Date < output[["day"]][i] &
                   input$Final.Date >= output[["day"]][i], ][, 8])
       / length(input[input$Initial.Date < output[["day"]][i] &
                        input$Final.Date >= output[["day"]][i], ][, 8]))
  }
  output$avgdailyuse <- avgdailyuse
  print(output)
}

number1_b <- calculate_AvgUsed(result5, consumption5)
w5 <- ggplot(number1_b) +
  geom_line(aes(x = day, y = avgdailyuse, col = "SEAMScolours[1]"), size =
              1.3) +
  labs(title = "Derived Consumption per day", y = "Average Daily Use /m3", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")
w5 + scale_color_manual(values = c(SEAMScolours[1]),
                        labels = c("Meters"))



















# PART 8 - Extrapolate -------------------------------------------------------

# HH metering

# load data

household <-
  data.table(
    read.csv(
      "Unmetered_properties_201904 1.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )

# clean and transform data

colnames(household)[colnames(household) == "Post.Code"] <-
  "Postcode"
household$Postcode <- gsub("[[:blank:]]", "", household$Postcode)
household <- household[!grep("-", household$DMA),]
household_dma <-
  household %>% select(DMA, Installation) %>% group_by(DMA) %>%
  tally()

mhh_combine41res <-
  mhh_combine41[grep("Residential", mhh_combine41$Premise.Type),]
consumptiondma <-
  mhh_combine41res %>% select(DMA_Ref,
                              Serial.Number,
                              Final.Date,
                              Initial.Date,
                              Initial.Reading,
                              Final.Reading) %>%
  group_by(Serial.Number) %>%
  unique() %>%
  arrange(Initial.Date)  %>%
  mutate(Used = Final.Reading - Initial.Reading) %>%
  mutate(Period = as.numeric(Final.Date - Initial.Date)) %>%
  mutate(Rate = Used / Period)  %>%
  filter(Period > 14) %>% # removing very short time periods - also removes NA
  filter(between(Used, 0, 500)) # removing unusually large value - also removes NA (only keeps TRUE)

#groupeddf <- group_split(consumption11dma,DMA_Ref)
dmx <- seq.Date(ymd("2017-04-01"), by = 1, length.out = 365)
resultdmx <- data.frame(day = dmx)

avgdailyuse <- c()
#consumptiondma <- as.dataframe(consumptiondma)
calculate_AvgUsed2 <- function(output, input) {
  for (i in seq_along(output[["day"]])) {
    avgdailyuse$avgdailyuse[i] <-
      (sum(input[input$Initial.Date < output[["day"]][i] &
                   input$Final.Date >= output[["day"]][i], ][, 9]))
    # / length(input[input$Initial.Date < output[["day"]][i] &
    #              input$Final.Date >= output[["day"]][i],][,9]))
    output$length[i] <-
      length(input[input$Initial.Date < output[["day"]][i] &
                     input$Final.Date >= output[["day"]][i], ][, 9])
  }
  output$DMA_Ref <- input$DMA_Ref[1]
  output$avgdailyuse <- avgdailyuse$avgdailyuse
  return(output)
}

split1 <-  split(consumptiondma, consumptiondma$DMA_Ref)
for (j in 1:length(split1)) {
  test <- data.frame(split1[[j]])
  test.output <- calculate_AvgUsed2(resultdmx, test)
  if (j == 1) {
    output.data <-
      test.output
  } else{
    output.data <- rbind(output.data, test.output)
  }
}
xplate1 <- output.data


## test result
# AD10calc <- calculate_AvgUsed2(resultdmx,AD10)
# AD10calc %>% group_by(DMA_Ref) %>% summarise(sum = sum(avgdailyuse))
# AD10 <- consumptiondma %>% filter(DMA_Ref == "AD10") %>% data.frame()
# consumptiondma %>% filter(DMA_Ref == "AD10") %>% group_by(DMA_Ref) %>% summarise(sum = sum(Rate))
# sum(AD10[AD10$Initial.Date < resultdmx[["day"]][1] & AD10$Final.Date >= resultdmx[["day"]][1],][,9])
# length(AD10[AD10$Initial.Date < resultdmx[["day"]][1] & AD10$Final.Date >= resultdmx[["day"]][1],][,9])
## test ends


xplate2 <-
  xplate1 %>% left_join(household_dma, by = c("DMA_Ref" = "DMA")) %>% rename(Properties = n) %>%
  mutate(factor = avgdailyuse / length) %>% mutate(UnmeteredUsage = factor * Properties) %>%
  mutate(ConsumptionforDate = avgdailyuse + UnmeteredUsage)


# NHH

# load data

nonhousehold <-
  data.table(
    read.csv(
      "Unmetered_properties_201904 2.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )

# trasform data

nonhousehold$Postcode <-
  gsub("[[:blank:]]", "", nonhousehold$Postcode)
#nonhousehold <- nonhousehold %>%  filter(DMA == "")
nonhousehold <- nonhousehold[!(nonhousehold$DMA == ""),]
nonhousehold_dma <-
  nonhousehold %>% select(DMA, SPID) %>% group_by(DMA) %>%
  tally() %>% arrange(desc(n))

mhh_combine41nonres <-
  anti_join(mhh_combine41, mhh_combine41res, by = "Premise.Type")
consumptiondma2 <-
  mhh_combine41nonres %>% select(DMA_Ref,
                                 Serial.Number,
                                 Final.Date,
                                 Initial.Date,
                                 Initial.Reading,
                                 Final.Reading) %>%
  group_by(Serial.Number) %>%
  unique() %>%
  arrange(Initial.Date)  %>%
  mutate(Used = Final.Reading - Initial.Reading) %>%
  mutate(Period = as.numeric(Final.Date - Initial.Date)) %>%
  mutate(Rate = Used / Period)  %>%
  filter(Period > 14) %>% # removing very short time periods - also removes NA
  filter(between(Used, 0, 500)) # removing unusually large value - also removes NA (only keeps TRUE)
#groupeddf <- group_split(consumption11dma,DMA_Ref)
dmx <- seq.Date(ymd("2017-04-01"), by = 1, length.out = 365)
resultdmx2 <- data.frame(day = dmx)
avgdailyuse <- c()
#consumptiondma <- as.dataframe(consumptiondma)
calculate_AvgUsed2 <- function(output, input) {
  for (i in seq_along(output[["day"]])) {
    avgdailyuse$avgdailyuse[i] <-
      (sum(input[input$Initial.Date < output[["day"]][i] &
                   input$Final.Date >= output[["day"]][i], ][, 9]))
    # / length(input[input$Initial.Date < output[["day"]][i] &
    #              input$Final.Date >= output[["day"]][i],][,9]))
    output$length[i] <-
      length(input[input$Initial.Date < output[["day"]][i] &
                     input$Final.Date >= output[["day"]][i], ][, 9])
  }
  output$DMA_Ref <- input$DMA_Ref[1]
  output$avgdailyuse <- avgdailyuse$avgdailyuse
  return(output)
}

split2 <-  split(consumptiondma2, consumptiondma2$DMA_Ref)
for (j in 1:length(split2)) {
  test <- data.frame(split2[[j]])
  test.output <- calculate_AvgUsed2(resultdmx2, test)
  if (j == 1) {
    output.data2 <-
      test.output
  } else{
    output.data2 <- rbind(output.data2, test.output)
  }
}
xplate3 <- output.data2


# testing stuff -------------------------------------------------- 

xplate4 <-
  xplate3 %>% left_join(nonhousehold_dma, by = c("DMA_Ref" = "DMA")) %>% rename(Properties = n) %>%
  mutate(factor = avgdailyuse / length) %>% mutate(UnmeteredUsage = factor * Properties) %>%
  mutate(ConsumptionforDate = avgdailyuse + UnmeteredUsage)


# HH and NHH

xplate5 <-
  xplate2 %>%  left_join(xplate4, by = c("day", "DMA_Ref")) %>%
  rename(ConsumptionforDateHH = ConsumptionforDate.x) %>%
  rename(ConsumptionforDateNHH = ConsumptionforDate.y) %>%
  select(day, DMA_Ref, ConsumptionforDateHH, ConsumptionforDateNHH) %>%
  mutate(ConsumptionforDateHH = replace_na(ConsumptionforDateHH, 0)) %>%
  mutate(ConsumptionforDateNHH = replace_na(ConsumptionforDateNHH, 0)) %>%
  group_by(day, DMA_Ref) %>%
  mutate(summation = sum(ConsumptionforDateHH + ConsumptionforDateNHH)) %>%
  arrange(desc(summation))

xplate5 %>% filter(DMA_Ref == "DD10") %>% arrange(day) %>%
  ggplot() +
  geom_line(aes(x = day, y = summation), col = "coral", size = 1.3) +
  labs(title = "Derived Consumption per day", y = "Average Daily Use /m3", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month")








# Household Metered

household_metered <-
  xplate2[, c(1, 3, 4)] %>%  select(everything()) %>% group_by(day) %>%
  summarise(avgdailyuse = sum(avgdailyuse))
ggplot()
geom_line(aes(x = day, y = avgdailyuse), col = "coral", size = 1.3) +
  labs(title = "Total Consumption per day - Household Metered", y = "Average Daily Use /m3", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "2 months")

# Household Unmetered

household_unmetered <-
  xplate2[, c(1, 3, 7)] %>%  select(everything()) %>% group_by(day) %>%
  mutate(UnmeteredUsage = replace_na(UnmeteredUsage, 0)) %>%
  summarise(UnmeteredUsage = sum(UnmeteredUsage))
ggplot()
geom_line(aes(x = day, y = UnmeteredUsage), col = "aquamarine2", size =
            1.3) +
  labs(title = "Total Consumption per day - Household Unmetered", y = "Average Daily Use /m3", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "2 months")


# Non Household Metered

nonhousehold_metered <-
  xplate4[, c(1, 3, 4)] %>%  select(everything()) %>% group_by(day) %>%
  summarise(avgdailyuse = sum(avgdailyuse))
ggplot()
geom_line(aes(x = day, y = avgdailyuse), col = "coral", size = 1.3) +
  labs(title = "Total Consumption per day - NonHousehold Metered", y = "Average Daily Use /m3", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "2 months")

# Non Household Unmetered
nonhousehold_unmetered <-
  xplate4[, c(1, 3, 7)] %>%  select(everything()) %>% group_by(day) %>%
  mutate(UnmeteredUsage = replace_na(UnmeteredUsage, 0)) %>%
  summarise(UnmeteredUsage = sum(UnmeteredUsage))
ggplot()
geom_line(aes(x = day, y = UnmeteredUsage), col = "aquamarine2", size =
            1.3) +
  labs(title = "Total Consumption per day - NonHousehold Unmetered", y = "Average Daily Use /m3", x = "Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg +
  scale_x_date(labels = date_format("%b-%y"), breaks = "2 months")

#grid.arrange(household_metered,household_unmetered,nonhousehold_metered,nonhousehold_unmetered, nrow = 2)
household_metered$Category <- "Household Metered"
household_unmetered$Category <- "Household Unmetered"
household_unmetered <-
  household_unmetered %>%  rename(avgdailyuse = UnmeteredUsage)
nonhousehold_metered$Category <- "Nonhousehold Metered"
nonhousehold_unmetered$Category <- "Nonhousehold Unmetered"
nonhousehold_unmetered <-
  nonhousehold_unmetered %>%  rename(avgdailyuse = UnmeteredUsage)
combination <-
  rbind(
    household_metered,
    household_unmetered,
    nonhousehold_metered,
    nonhousehold_unmetered
  )
combination$Category <- fct_relevel(
  decile.summary2$Category,
  "Jumper - Increase",
  "Creeper - Increase",
  "Stable",
  "Creeper - Decrease",
  "Jumper - Decrease",
  "Other",
  "Insufficient Data"
)

combination1 <-  combination
combination1$day <- substring(combination1$day, 1, 7)
combination2 <-
  combination1 %>% group_by(day, Category) %>% mutate(avgdailyuse = sum(avgdailyuse)) %>%  unique()
combination2$day <- paste(combination2$day, "-01")
combination2$day <- gsub("[[:blank:]]", "", combination2$day)
combination2$day <-  ymd(combination2$day)
combination2 %>%
  ggplot(aes(day, avgdailyuse), position = "dodge") +
  geom_bar(aes(fill = Category), stat = "identity") +
  #scale_x_continuous(name = "Day", breaks = c(1:10)) +
  scale_y_continuous(name = "Total Consumption by Category") +
  scale_fill_manual(values = c("red3", "orange2", "gold1", "forestgreen")) +
  labs(title = "Total Consumption per day") +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 months")



# PART 9 - total estimated annual consumption (per DMA) ------------------------------------
# against the total nightflow in the same year

# metered dmas

consumptiondma3 <-
  mhh_combine41 %>% select(DMA_Ref,
                           Serial.Number,
                           Final.Date,
                           Initial.Date,
                           Initial.Reading,
                           Final.Reading) %>%
  group_by(Serial.Number) %>%
  unique() %>%
  arrange(Initial.Date)  %>%
  mutate(Used = Final.Reading - Initial.Reading) %>%
  mutate(Period = as.numeric(Final.Date - Initial.Date)) %>%
  mutate(Rate = Used / Period)  %>%
  filter(Period > 14) %>% # removing very short time periods - also removes NA
  filter(between(Used, 0, 500)) # removing unusually large value - also removes NA (only keeps TRUE)
dmx3 <- seq.Date(ymd("2017-04-01"), by = 1, length.out = 365)
resultdmx3 <- data.frame(day = dmx3)
avgdailyuse <- c()
#consumptiondma <- as.dataframe(consumptiondma)
calculate_AvgUsed2 <- function(output, input) {
  for (i in seq_along(output[["day"]])) {
    avgdailyuse$avgdailyuse[i] <-
      (sum(input[input$Initial.Date < output[["day"]][i] &
                   input$Final.Date >= output[["day"]][i], ][, 9]))
    # / length(input[input$Initial.Date < output[["day"]][i] &
    #              input$Final.Date >= output[["day"]][i],][,9]))
    output$length[i] <-
      length(input[input$Initial.Date < output[["day"]][i] &
                     input$Final.Date >= output[["day"]][i], ][, 9])
  }
  output$DMA_Ref <- input$DMA_Ref[1]
  output$avgdailyuse <- avgdailyuse$avgdailyuse
  return(output)
}
split3 <-  split(consumptiondma3, consumptiondma3$DMA_Ref)
for (j in 1:length(split3)) {
  test <- data.frame(split3[[j]])
  test.output <- calculate_AvgUsed2(resultdmx3, test)
  if (j == 1) {
    output.data3 <-
      test.output
  } else{
    output.data3 <- rbind(output.data3, test.output)
  }
}
xplate6 <- output.data3
hh_nhh <-
  rbind(household_dma, nonhousehold_dma) %>% arrange(DMA) %>% group_by(DMA) %>% summarise(n = sum(n))
xplate7 <-
  xplate6 %>% left_join(hh_nhh, by = c("DMA_Ref" = "DMA")) %>% rename(Properties = n) %>%
  mutate(factor = avgdailyuse / length) %>% mutate(UnmeteredUsage = factor * Properties) %>%
  mutate(ConsumptionforDate = avgdailyuse + UnmeteredUsage) %>%
  mutate(ConsumptionforDate = replace_na(ConsumptionforDate, 0)) %>%
  group_by(DMA_Ref) %>% summarise(totalconsumption = sum(ConsumptionforDate))


# load data

setwd("I:\\Projects\\Client\\1892\\Analytics")
in.11 <-
  data.table(read.csv(
    "Interpolated Nightflow.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

# clean, transform data

in.11 <- select(in.11, DMA_Description, Apr.17:Mar.18)
non_dup <- in.11[!duplicated(in.11$DMA_Description), ]
in.11$DMA_Description <- as.character(in.11$DMA_Description)
dim(gis)
dim(non_dup)
in.2 <-
  inner_join(
    x = gis[, c("DMA_Ref", "DMA_Dsc", "Divisin")],
    y = non_dup,
    by = c("DMA_Dsc" = "DMA_Description")
  )
tmap_mode("view")
in.2 <- st_set_geometry(in.2, NULL)
in.2 <- na.omit(in.2)

# convert a few wide columns to long format

in.5 <- gather(in.2, Date1, Value, Apr.17:Mar.18, factor_key = TRUE)
in.5$Date1 <- as.yearmon(in.5$Date1, format = "%b.%y")
in.5$Date1 <- paste("01 ", in.5$Date1, sep = "")
in.5$Date1  <- dmy(in.5$Date1)
in.6 <-
  in.5 %>% group_by(DMA_Ref) %>%  summarise(totalnightflow = sum(Value))

scatter <- xplate7 %>%  left_join(in.6, by = "DMA_Ref")

# write the output data

fwrite(scatter, "scatter.csv")

# plot using a scatter plot

ggplot(na.omit(scatter), aes(totalconsumption, totalnightflow)) +
  geom_jitter(aes(), col = "blue") +
  geom_smooth(aes(), col = "red", method = "lm") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  scale_x_continuous(
    name = "Estimated DMA Consumption",
    limits = c(0, 500000),
    labels = function(x) {
      paste0(x / 1000, 'K')
    }
  ) +
  scale_y_continuous(
    name = "Total Nightflow",
    limits = c(0, 750000),
    labels = function(x) {
      paste0(x / 1000, 'K')
    }
  ) +
  #scale_color_manual(values=c("red3","orange2","gold1","forestgreen","blue3","gray90","grey")) +
  labs(title = "Estimated DMA Consumption vs. Total Nightflow (April 2017 - March 2018)") + gg


scatter2 <-
  na.omit(scatter) %>%  inner_join(gis[, c("DMA_Ref", "geometry")], by = "DMA_Ref") %>%
  arrange(desc(totalnightflow)) %>% filter(between(row_number(), 1, 20))
scatter2 <- st_as_sf(scatter2)
st_crs(scatter2) <- 27700
scatter3 <-  scatter2
scatter3 <- st_set_geometry(scatter3, NULL)

# plot on a map

tmap_mode("view")
qtm(scatter2, fill = "blue", basemaps = "Esri.WorldStreetMap") + tm_style("beaver")
