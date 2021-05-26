
# *Note: This code was poorly commented, written due to poor coding practices of times past. Apologies!*


# load libraries --------------------------------------------

library(data.table)
library(sp)
library(sf)
library(maptools)
library(leaflet)
library(tmap)
library(rgdal)
library(units)
library(stringr)
library(gganimate)
library(DescTools)
library(lubridate)
library(scales)
library(gridExtra)
library(readxl)
library(xlsx)
library(plyr)
library(zoo)

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


# HYP 1.2 ---------------------------------

# load data
setwd("I:\\Projects\\Client\\1892\\Analytics")
vn.11 <-
  data.table(
    read.csv(
      "I:\\Projects\\Client\\1915\\Analytics\\NightFlow-Combined-v2.2.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )
dma.lookup <-
  data.table(read.csv("I:\\Projects\\Client\\1892\\Analytics\\DMA Lookup.csv"))
dma.classification <-
  data.table(read.csv(
    "I:\\Projects\\Client\\1892\\Analytics\\DMA category analysis v5.csv"
  ))
mlp <-
  data.table(
    read.csv(
      "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/Christina/DMA Material Leakage Property NF Data.csv"
    )
  )

# clean, transform data
vn.11 <- vn.11[, c(1:97)]
non_dup <- vn.11[!duplicated(vn.11$DMA.Code), ]

# load data
setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 1")
gis <- st_read("gis.shp")

# clean, transform data
gis <- gis[!duplicated(gis$DMA_Dsc), ]
gis <- st_as_sf(gis)
st_crs(gis) <- 27700
vn.11$DMA.Code <- as.character(vn.11$DMA.Code)
dim(gis)
dim(non_dup)

vn.2 <-
  inner_join(x = gis[, c(1, 2, 14)],
             y = non_dup,
             by = c("DMA_Ref" = "DMA.Code"))
#vn.2 <- st_as_sf(vn.2, crs = 27700)
tmap_mode("view")


vn.2 <- st_set_geometry(vn.2, NULL)
vn.3 <- vn.2 %>%
  mutate(Region = ifelse(Divisin == "H", "West",
                         (ifelse(
                           Divisin == "K", "East",
                           (ifelse(Divisin == "I", "West",
                                   (
                                     ifelse(Divisin == "S", "Central", "Other")
                                   )))
                         )))) %>%
  mutate(Divisin = NULL)

# convert to (long format) DMA MNF
vn.4 <- gather(vn.3, Date1, Value, Jan.15:Dec.18, factor_key = TRUE)
vn.4$Date1 <- as.yearmon(vn.4$Date1, format = "%b.%y")
write.csv(
  vn.4,
  "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/Validated Nightflow v5.csv",
  row.names = F
)
vn.4$Date1 <- paste("01 ", vn.4$Date1, sep = "")
vn.4$Date1  <- dmy(vn.4$Date1)
write.csv(
  vn.4,
  "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/Validated Nightflow v4.csv",
  row.names = F
)
vn.west <- vn.4 %>% filter(Region == "West")
vn.east <- vn.4 %>% filter(Region == "East")
vn.central <- vn.4 %>% filter(Region == "Central")


# bring in the freeze thaw data
setwd("I:\\Projects\\Client\\1892\\Analytics")
both_3 <-
  data.table(read.csv(
    "freeze events 2 TO.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

# clean, transform data
substr(both_3$ID, 1, 1) <- toupper(substr(both_3$ID, 1, 1))
both_3$Date1 <- dmy(both_3$Date1)
both_3 <- both_3 %>%  mutate(Months1 = month(Date1))
both_3 <- both_3 %>%  mutate(Year1 = year(Date1))
both_3$MonthYear1 <- paste(both_3$Months1, sep = "-", both_3$Year1)
both_3 <-
  both_3 %>% select(-c(MonthYear)) %>% dplyr::rename(MonthYear = MonthYear1)
both.west <- both_3 %>% filter(ID == "West")
both.east <- both_3 %>% filter(ID == "East")
both.central <- both_3 %>% filter(ID == "Central")


# plot west
both.west.yes <- both.west %>% filter(Freezing.Month == "Yes")
vn.west.before1 <-
  vn.west %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, (ymd("2017-01-01") - months(6)), ymd("2017-01-01"))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "Before 1st Event")


vn.west.after1 <-
  vn.west %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, ymd("2017-01-21"), (ymd("2017-01-21") + months(7)))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "After 1st Event")
vn.west.before2 <-
  vn.west %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, (ymd("2018-02-26") - months(6)), ymd("2018-01-26"))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "Before 2nd Event")
vn.west.during2 <-
  vn.west %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, ymd("2018-02-01"), ymd("2018-03-17"))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "Beast from the East")
vn.west.after2 <-
  vn.west %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, ymd("2018-03-18"), (ymd("2018-03-18") + months(8)))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "After 2nd Event")

vn.west.all <-
  rbind(vn.west.before1,
        vn.west.before2,
        vn.west.after1,
        vn.west.after2,
        vn.west.during2)
vn.west.all$Category <-
  fct_relevel(
    vn.west.all$Category,
    "Before 1st Event",
    "After 1st Event",
    "Before 2nd Event",
    "Beast from the East",
    "After 2nd Event"
  )
vn.west.all$Region <- "West"

ggplot(vn.west.all) +
  geom_line(aes(x = Date1, y = Average), size = 1.3) +
  labs(title = "Nightflow Averages Before and After Freeze/Thaw Events, August 2016 - November 2018 (West Region)", y = "Total Nightflow", x = "") + gg +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month") +
  scale_y_continuous(
    limits = c(5850, 10500),
    name = "Average Nightflow",
    labels = function(x) {
      paste0(x / 10 ^ 3, 'K')
    }
  ) +
  # facet_wrap( ~ Category,
  #             nrow = 1,
  #             ncol = 5,
  #             scales = "free_x") +
  scale_color_manual(values = c("#eb5e34", "#34eb80", "#e66c80", "orange", "#259c41"))

vn.west.all %>% as.data.frame()

# plot east
both.east.yes <- both.east %>% filter(Freezing.Month == "Yes")
vn.east.before1 <-
  vn.east %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, (ymd("2017-01-01") - months(6)), ymd("2017-01-01"))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "Before 1st Event")
vn.east.after1 <-
  vn.east %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, ymd("2017-01-21"), (ymd("2017-01-21") + months(7)))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "After 1st Event")
vn.east.before2 <-
  vn.east %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, (ymd("2018-02-26") - months(6)), ymd("2018-01-26"))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "Before 2nd Event")
vn.east.during2 <-
  vn.east %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, ymd("2018-02-01"), ymd("2018-03-17"))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "Beast from the East")
vn.east.after2 <-
  vn.east %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, ymd("2018-03-18"), (ymd("2018-03-18") + months(8)))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "After 2nd Event")

vn.east.all <-
  rbind(vn.east.before1,
        vn.east.before2,
        vn.east.after1,
        vn.east.after2,
        vn.east.during2)
vn.east.all$Category <-
  fct_relevel(
    vn.east.all$Category,
    "Before 1st Event",
    "After 1st Event",
    "Before 2nd Event",
    "Beast from the East",
    "After 2nd Event"
  )
vn.east.all$Region <- "East"

ggplot(vn.east.all) +
  geom_line(aes(x = Date1, y = Average, col = Category), size = 1.3) +
  labs(title = "Nightflow Averages Before and After Freeze/Thaw Events, August 2016 - November 2018 (East Region)", y = "Total Nightflow", x = "") + gg +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month") +
  scale_y_continuous(
    limits = c(5850, 10500),
    name = "Average Nightflow",
    labels = function(x) {
      paste0(x / 10 ^ 3, 'K')
    }
  ) +
  facet_wrap(~ Category,
             nrow = 1,
             ncol = 5,
             scales = "free_x") +
  scale_color_manual(values = c("#eb5e34", "#34eb80", "#e66c80", "orange", "#259c41"))

vn.east.all %>% as.data.frame()



# plot central
both.central.yes <- both.central %>% filter(Freezing.Month == "Yes")
vn.central.before1 <-
  vn.central %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, (ymd("2017-01-01") - months(6)), ymd("2017-01-01"))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "Before 1st Event")
vn.central.after1 <-
  vn.central %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, ymd("2017-01-21"), (ymd("2017-01-21") + months(7)))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "After 1st Event")
vn.central.before2 <-
  vn.central %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, (ymd("2018-02-26") - months(6)), ymd("2018-01-26"))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "Before 2nd Event")
vn.central.during2 <-
  vn.central %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, ymd("2018-02-01"), ymd("2018-03-17"))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "Beast from the East")
vn.central.after2 <-
  vn.central %>% arrange(Date1) %>% filter(Value > 0) %>%
  filter(between(Date1, ymd("2018-03-18"), (ymd("2018-03-18") + months(8)))) %>%
  group_by(Date1) %>% summarise(Average = mean(Value, na.rm =
                                                 T)) %>% mutate(Category = "After 2nd Event")

vn.central.all <-
  rbind(
    vn.central.before1,
    vn.central.before2,
    vn.central.after1,
    vn.central.after2,
    vn.central.during2
  )
vn.central.all$Category <-
  fct_relevel(
    vn.central.all$Category,
    "Before 1st Event",
    "After 1st Event",
    "Before 2nd Event",
    "Beast from the East",
    "After 2nd Event"
  )
vn.central.all$Region <- "Central"

ggplot(vn.central.all) +
  geom_line(aes(x = Date1, y = Average, col = Category), size = 1.3) +
  labs(title = "Nightflow Averages Before and After Freeze/Thaw Events, August 2016 - November 2018 (Central Region)", y = "Total Nightflow", x = "") + gg +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month") +
  scale_y_continuous(
    limits = c(5850, 10500),
    name = "Average Nightflow",
    labels = function(x) {
      paste0(x / 10 ^ 3, 'K')
    }
  ) +
  facet_wrap(~ Category,
             nrow = 1,
             ncol = 5,
             scales = "free_x") +
  scale_color_manual(values = c("#eb5e34", "#34eb80", "#e66c80", "orange", "#259c41"))

vn.central.all %>% as.data.frame()
plot1 <- rbind(vn.east.all, vn.central.all, vn.west.all)

write.csv(plot1, "plot1.csv", row.names = F)

ggplot(plot1) +
  geom_line(aes(x = Date1, y = Average, col = Region), size = 1.3) +
  labs(title = "Nightflow Averages Before and After Freeze/Thaw Events, July 2016 - November 2018", y = "Total Nightflow", x = "") + gg +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month") +
  scale_y_continuous(
    limits = c(5850, 10500),
    name = "Average Nightflow",
    labels = function(x) {
      paste0(x / 10 ^ 3, 'K')
    }
  ) +
  facet_wrap(~ Region,
             nrow = 3,
             ncol = 1,
             scales = "free_y") +
  scale_color_manual(values = c("#e66c80", "orange", "#259c41"))


ggplot(plot1) +
  geom_line(aes(x = Date1, y = Average, col = Region), size = 1.3) +
  labs(title = "Nightflow Averages Before and After Freeze/Thaw Events, July 2016 - November 2018", y = "Total Nightflow", x = "") + gg +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  scale_x_date(labels = date_format("%b-%y"), breaks = "1 month") +
  scale_y_continuous(
    limits = c(5850, 10500),
    name = "Average Nightflow",
    labels = function(x) {
      paste0(x / 10 ^ 3, 'K')
    }
  ) +
  scale_color_manual(values = c("#e66c80", "orange", "#259c41"))
#"#eb5e34", "#34eb80",

# DMAs that did not recover ---------------------------------
# summarise each dma to showcase
vn.4 <- vn.4 %>%  mutate(Months1 = month(Date1))
vn.4 <- vn.4 %>%  mutate(Year1 = year(Date1))
vn.4$MonthYear <- paste(vn.4$Months1, sep = "-", vn.4$Year1)
wd <-
  both_3 %>% select(ID, Value, Freezing.Month, MonthYear) %>% select(ID, MonthYear, Freezing.Month)
avg <-
  vn.4 %>% group_by(DMA_Ref, Region, MonthYear) %>% dplyr::summarise(Total = sum(Value, na.rm = T)) %>% distinct()
ok <-
  avg %>% inner_join(wd, by = c("Region" = "ID", "MonthYear" = "MonthYear"))

ok$MonthYear <- paste("01-", ok$MonthYear, sep = "")
ok$MonthYear  <- dmy(ok$MonthYear)
ok2 <-
  ok %>% distinct() %>% filter(Total > 0) %>% group_by(DMA_Ref) %>% arrange(DMA_Ref, MonthYear)

# MAY 18 - NOV 17
# select the 3 important months for the difference (and the least NF month, if not there)
ok.3 <- ok2 %>% group_by(DMA_Ref) %>%
  filter(MonthYear == "2017-11-01" |
           MonthYear == "2018-05-01") %>%
  add_tally() %>% filter(n > 1)
ok.3 <- split(ok.3, ok.3$DMA_Ref)

for (j in 1:length(ok.3)) {
  test <- data.table(ok.3[[j]])
  test.output <-
    (test[(which(test$MonthYear == "2018-05-01")), 4] / test[(which(test$MonthYear == "2017-11-01")), 4]) - 1
  test.output$DMA <- test$DMA_Ref[[1]]
  if (j == 1) {
    output.final <- test.output
  } else{
    output.final <- rbind(output.final, test.output)
  }
}

vv <-
  as.data.frame(cbind(DMA.Code = output.final$DMA, diff1 = output.final$Total)) %>%
  mutate(diff1 = round(as.numeric(as.character(diff1)), 3))
str(vv)
vv__2 <-
  vv %>% filter(diff1 > 0) %>%  left_join(dma.lookup, by = "DMA.Code") %>%
  left_join(vn.3[, c("DMA_Ref", "Region")], by = c("DMA.Code" = "DMA_Ref")) %>% dplyr::rename(`May18-Nov17.proportion` = diff1)
write.csv(vv__2, "Unrecovered DMAs.csv", row.names = F)

vv %>% filter(diff1 > 0) %>%
  left_join(dma.lookup, by = "DMA.Code") %>%
  left_join(vn.3[, c("DMA_Ref", "Region")], by = c("DMA.Code" = "DMA_Ref")) %>%
  group_by(Region) %>%
  tally()

unique(vv$DMA.Code)
print("526 out of 911,
  Central-187,
  East-127,
  West-212")


# add Nov 2017 values
ok2.nov <- ok2 %>% group_by(DMA_Ref) %>%
  filter(MonthYear == "2017-11-01") %>%
  select(DMA_Ref, Total) %>% dplyr::rename(Nov.17.value = Total)

vv2 <-
  vv %>% left_join(ok2.nov, by = c("DMA.Code" = "DMA_Ref")) %>% dplyr::rename(May18.Nov17 = diff1)


# load data
leakage.detection.effort <-
  data.table(
    read.csv(
      "I:\\Projects\\Client\\1892\\Analytics\\LeakageDetectionEffort v2.csv"
    )
  )

# clean, transform data
lde1 <- select(
  leakage.detection.effort,
  County,
  Standard.Job.No,
  DMA.Code,
  Actual.Labour.Hours,
  Maintainance.Type,
  Completion.D.Date
)
lde1$Completion.D.Date <- dmy(lde1$Completion.D.Date)
lde1.1 <-
  lde1 %>% filter(between(Completion.D.Date, ymd("2017-11-01"), ymd("2018-05-31"))) %>% group_by(DMA.Code) %>%
  dplyr::summarise(ALC.Hours.Nov17.May18 = sum(Actual.Labour.Hours))
lde1.2 <-
  lde1 %>% filter(between(Completion.D.Date, ymd("2017-11-01"), ymd("2018-11-30"))) %>% group_by(DMA.Code) %>%
  dplyr::summarise(ALC.Hours.Nov17.Nov18 = sum(Actual.Labour.Hours))
lde2 <- lde1.1 %>% left_join(lde1.2, by = "DMA.Code")

# May 18 - Nov 17
# ggplot
table1 <- vv2 %>% left_join(lde2, by = "DMA.Code") %>%
  left_join(dma.classification[, c("DMA.Code", "DMA_Description", "Category", "Material_profile")], by = "DMA.Code") %>%
  select(1, 5:7, 2:4) %>%
  mutate(May18.Nov17 = May18.Nov17 * 100)
table1$Category <-
  fct_relevel(
    table1$Category,
    "Jumper - Increase",
    "Creeper - Increase",
    "Stable",
    "Creeper - Decrease",
    "Jumper - Decrease",
    "Other",
    "Insufficient Data"
  )
annotations <- data.frame(
  xpos = c(-Inf, -Inf, Inf, Inf),
  ypos =  c(-Inf, Inf, -Inf, Inf),
  annotateText = c(
    "Decrease due to other causes?",
    "Increase due to Less Effort?",
    "Decrease With Effort",
    "Increase Despite Effort"
  ),
  hjustvar = c(0, 0, 1, 1) ,
  vjustvar = c(-0.5, 1.2,-0.5, 1.2)
)



out.table <-
  table1 %>% mutate(
    Quadrant = ifelse(
      ALC.Hours.Nov17.May18 <= 50 &
        May18.Nov17 > 0,
      "Increase due to Less Effort?",
      (
        ifelse(
          ALC.Hours.Nov17.May18 > 50 &
            May18.Nov17 <= 0,
          "Decrease With Effort",
          (
            ifelse(
              ALC.Hours.Nov17.May18 > 50 &
                May18.Nov17 > 0,
              "Increase Despite Effort",
              (
                ifelse(
                  ALC.Hours.Nov17.May18 <= 50 &
                    May18.Nov17 <= 0,
                  "Decrease due to other causes?",
                  "Other"
                )
              )
            )
          )
        )
      )
    )
  ) %>%
  mutate(Quadrant = replace_na(Quadrant, "Other")) %>%
  left_join(mlp[, c("DMA", "property_discrepancy")], by = c("DMA.Code" =
                                                              "DMA")) %>%
  dplyr::rename(May18.Nov17.percentage = May18.Nov17)

out.table %>% filter(Quadrant == "Increase due to Less Effort?") %>% arrange(desc(May18.Nov17.percentage)) %>% head()
out.table %>% filter(Quadrant == "Increase Despite Effort") %>% arrange(desc(May18.Nov17.percentage)) %>% head()
out.table %>% filter(Quadrant == "Decrease With Effort") %>% arrange(desc(ALC.Hours)) %>% head()

out.table.x <-
  out.table %>% #filter(property_discrepancy < 1000) %>%
  mutate(`Property Category` = ifelse(property_discrepancy < -250, "<-250",
                                      (
                                        ifelse(
                                          property_discrepancy <= 0 & property_discrepancy > -250,
                                          "-250 to 0",
                                          (
                                            ifelse(
                                              property_discrepancy <= 250 & property_discrepancy > 0,
                                              "1 to 250",
                                              (
                                                ifelse(
                                                  property_discrepancy <= 500 &
                                                    property_discrepancy > 250,
                                                  "251 to 500",
                                                  (
                                                    ifelse(
                                                      property_discrepancy <= 1000 &
                                                        property_discrepancy > 500,
                                                      "501 to 1000",
                                                      ">1000"
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )))
out.table.x$`Property Category` <-
  fct_relevel(
    out.table.x$`Property Category`,
    "<-250",
    "-250 to 0",
    "1 to 250",
    "251 to 500",
    "501 to 1000",
    ">1000"
  )



# NOV - NOV 2018
# select the 3 important months for the difference (and the least NF month, if not there) -- 2nd one
ok.4 <- ok2 %>% group_by(DMA_Ref) %>%
  filter(MonthYear == "2017-11-01" |
           MonthYear == "2018-11-01") %>% #same process as above
  add_tally() %>% filter(n > 1)
ok.4 <- split(ok.4, ok.4$DMA_Ref)

for (j in 1:length(ok.4)) {
  test <- data.table(ok.4[[j]])
  test.output <-
    (test[(which(test$MonthYear == "2018-11-01")), 4] / test[(which(test$MonthYear == "2017-11-01")), 4]) - 1
  test.output$DMA <- test$DMA_Ref[[1]]
  if (j == 1) {
    output.final2 <- test.output
  } else{
    output.final2 <- rbind(output.final2, test.output)
  }
}


vv. <-
  as.data.frame(cbind(DMA.Code = output.final2$DMA, diff1 = output.final2$Total)) %>%
  mutate(diff1 = round(as.numeric(as.character(diff1)), 3))

# add Nov 2017 values

ok2.nov <- ok2 %>% group_by(DMA_Ref) %>%
  filter(MonthYear == "2017-11-01") %>%
  select(DMA_Ref, Total) %>% dplyr::rename(Nov.17.value = Total)

vv.2 <-
  vv. %>% left_join(ok2.nov, by = c("DMA.Code" = "DMA_Ref")) %>% dplyr::rename(Nov18.Nov17 = diff1)


# Nov 18 - Nov 17
# ggplot
table.1 <- vv.2 %>% left_join(lde2, by = "DMA.Code") %>%
  left_join(dma.classification[, c("DMA.Code", "DMA_Description", "Category", "Material_profile")], by = "DMA.Code") %>%
  select(1, 5:7, 2:4) %>% mutate(Nov18.Nov17 = Nov18.Nov17 * 100)
table.1$Category <-
  fct_relevel(
    table.1$Category,
    "Jumper - Increase",
    "Creeper - Increase",
    "Stable",
    "Creeper - Decrease",
    "Jumper - Decrease",
    "Other",
    "Insufficient Data"
  )
annotations <- data.frame(
  xpos = c(-Inf, -Inf, Inf, Inf),
  ypos =  c(-Inf, Inf, -Inf, Inf),
  annotateText = c(
    "Decrease due to other causes?",
    "Increase due to Less Effort?",
    "Decrease With Effort",
    "Increase Despite Effort"
  ),
  hjustvar = c(0, 0, 1, 1) ,
  vjustvar = c(-0.5, 1.2,-0.5, 1.2)
)




# for data saving
# load data
data <-
  read_csv(
    "C:/Users/TOsosanya/Desktop/Southern Water/Phase 1/Nightflow Mastersheet_sheet_6.csv"
  )

# clean, transform data
data1 <- data %>%
  select(
    County,
    Management.area = `Management Area`,
    DMA.Code = DMA,
    Phase,
    Property.count = `Property Count`,
    Length = `Mains Length NEW`,
    Asbestos:Unknown,
    Length.after.2000 = `New Mains Installed after 2000 (m)`,
    Leakage.m3.hr = `Reported Leakage (m3/hr)`,
    march.april.leakage.18 = `March/April_18`,
    march.april.leakage.19 = `March/April_19`,
    DMA.desc = `DMA Description`
  )


out.table2 <-
  table.1 %>% mutate(
    `Quadrant (Nov18-Nov17)` = ifelse(
      ALC.Hours.Nov17.Nov18 <= 50 &
        Nov18.Nov17 > 0,
      "Increase due to Less Effort?",
      (
        ifelse(
          ALC.Hours.Nov17.Nov18 > 50 &
            Nov18.Nov17 <= 0,
          "Decrease With Effort",
          (
            ifelse(
              ALC.Hours.Nov17.Nov18 > 50 &
                Nov18.Nov17 > 0,
              "Increase Despite Effort",
              (
                ifelse(
                  ALC.Hours.Nov17.Nov18 <= 50 &
                    Nov18.Nov17 <= 0,
                  "Decrease due to other causes?",
                  "Other"
                )
              )
            )
          )
        )
      )
    )
  ) %>%
  mutate(`Quadrant (Nov18-Nov17)` = replace_na(`Quadrant (Nov18-Nov17)`, "Other")) %>%
  left_join(mlp[, c("DMA", "group.name", "property_discrepancy")], by = c("DMA.Code" =
                                                                            "DMA")) %>%
  dplyr::rename(Nov18.Nov17.percentage = Nov18.Nov17) %>%
  dplyr::rename(Material_profile = group.name) %>%
  select(DMA.Code,
         Nov18.Nov17.percentage,
         Material_profile,
         `Quadrant (Nov18-Nov17)`)



out.table.final <-
  out.table %>% left_join(out.table2, by = "DMA.Code") %>%
  dplyr::rename(`Quadrant (May18-Nov17)` = Quadrant) %>%
  left_join(data1[, c("County", "DMA.Code", "Length")], by = "DMA.Code") %>%
  mutate(`May18.Nov17.percentage/km` = May18.Nov17.percentage / Length) %>%
  mutate(`Nov18.Nov17.percentage/km` = Nov18.Nov17.percentage / Length) %>%
  mutate(`ALC.Hours.Nov17.May18/km` = ALC.Hours.Nov17.May18 / Length) %>%
  mutate(`ALC.Hours.Nov17.Nov18/km` = ALC.Hours.Nov17.Nov18 / Length)

# write the output to a csv file
write.csv(
  out.table.final,
  "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/DMA Profiles for HYP1_2 v3.csv",
  row.names = FALSE
)

# plots
ggplot(
  out.table.final %>%  filter(!is.na(property_discrepancy)),
  aes(`ALC.Hours.Nov17.May18/km`, `May18.Nov17.percentage`)
) +
  geom_point(aes(colour = Category, size = property_discrepancy)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  gg + scale_x_continuous(name = "ALC Hours/km", limits = c(0, 75)) +
  scale_y_continuous(name = "Nightflow Change (%)", limits = c(-100, 250)) +
  scale_color_manual(values = c(
    "red3",
    "orange2",
    "gold1",
    "forestgreen",
    "blue3",
    "gray90",
    "grey"
  )) +
  labs(title = "DMA Leakage Detection Effort compared to % Nightflow Change, November 2017 - May 2018") +
  geom_hline(yintercept = 0, colour = "darkred") + geom_vline(xintercept = 10, colour = "darkred") +
  geom_text(data = annotations,
            aes(
              x = xpos,
              y = ypos,
              hjust = hjustvar,
              vjust = vjustvar,
              label = annotateText
            ))

ggplot(
  out.table.final %>%  filter(!is.na(property_discrepancy)),
  aes(`ALC.Hours.Nov17.May18/km`, `May18.Nov17.percentage`)
) +
  geom_point(aes(colour = Material_profile, size = property_discrepancy)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  gg + scale_x_continuous(name = "ALC Hours/km", limits = c(0, 75)) +
  scale_y_continuous(name = "Nightflow Change (%)", limits = c(-100, 250)) +
  scale_color_manual(values = c(
    "red3",
    "orange2",
    "gold1",
    "forestgreen",
    "blue3",
    "gray90",
    "grey"
  )) +
  labs(title = "DMA Leakage Detection Effort compared to % Nightflow Change, November 2017 - May 2018") +
  geom_hline(yintercept = 0, colour = "darkred") + geom_vline(xintercept = 10, colour = "darkred") +
  geom_text(data = annotations,
            aes(
              x = xpos,
              y = ypos,
              hjust = hjustvar,
              vjust = vjustvar,
              label = annotateText
            ))

ggplot(
  out.table.final %>%  filter(!is.na(property_discrepancy)),
  aes(`ALC.Hours.Nov17.May18/km`, `May18.Nov17.percentage`)
) +
  geom_point(aes(size = property_discrepancy), colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  gg + scale_x_continuous(name = "ALC Hours/km", limits = c(0, 75)) +
  scale_y_continuous(name = "Nightflow Change (%)", limits = c(-100, 250)) +
  scale_color_manual(values = c(
    "red3",
    "orange2",
    "gold1",
    "forestgreen",
    "blue3",
    "gray90",
    "grey"
  )) +
  labs(title = "DMA Leakage Detection Effort compared to % Nightflow Change, November 2017 - May 2018") +
  geom_hline(yintercept = 0, colour = "darkred") + geom_vline(xintercept = 10, colour = "darkred") +
  geom_text(data = annotations,
            aes(
              x = xpos,
              y = ypos,
              hjust = hjustvar,
              vjust = vjustvar,
              label = annotateText
            ))

# ggplot(out.table.x %>%  filter(!is.na(`Property Category`)), aes(ALC.Hours, May18.Nov17.percentage)) +
#   geom_point(aes(colour = Category,size = `Property Category`, shape = Material_profile)) +
#   gg + scale_x_continuous(name = "ALC Hours", limits = c(0, 200)) +
#   scale_y_continuous(name = "Nightflow Change (%)", limits = c(-100, 250)) +
#   scale_color_manual(values=c("red3","orange2","gold1","forestgreen","blue3","gray90","grey")) +
#   labs(title = "DMA Leakage Detection Effort compared to % Nightflow Change, November 2017 - May 2018") +
#   geom_hline(yintercept = 0, colour = "darkred") + geom_vline(xintercept = 50, colour = "darkred") +
#   geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "right")


ggplot(
  out.table.final %>%  filter(!is.na(property_discrepancy)),
  aes(`ALC.Hours.Nov17.Nov18/km`, `Nov18.Nov17.percentage`)
) +
  geom_point(aes(colour = Category, size = property_discrepancy)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  gg + scale_x_continuous(name = "ALC Hours/km", limits = c(0, 75)) +
  scale_y_continuous(name = "Nightflow Change (%)", limits = c(-100, 250)) +
  scale_color_manual(values = c(
    "red3",
    "orange2",
    "gold1",
    "forestgreen",
    "blue3",
    "gray90",
    "grey"
  )) +
  labs(title = "DMA Leakage Detection Effort compared to % Nightflow Change, November 2017 - November 2018") +
  geom_hline(yintercept = 0, colour = "darkred") + geom_vline(xintercept = 10, colour = "darkred") +
  geom_text(data = annotations,
            aes(
              x = xpos,
              y = ypos,
              hjust = hjustvar,
              vjust = vjustvar,
              label = annotateText
            ))

ggplot(
  out.table.final %>%  filter(!is.na(property_discrepancy)),
  aes(`ALC.Hours.Nov17.Nov18/km`, `Nov18.Nov17.percentage`)
) +
  geom_point(aes(colour = Material_profile, size = property_discrepancy)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  gg + scale_x_continuous(name = "ALC Hours/km", limits = c(0, 75)) +
  scale_y_continuous(name = "Nightflow Change (%)", limits = c(-100, 250)) +
  scale_color_manual(values = c(
    "red3",
    "orange2",
    "gold1",
    "forestgreen",
    "blue3",
    "gray90",
    "grey"
  )) +
  labs(title = "DMA Leakage Detection Effort compared to % Nightflow Change, November 2017 - November 2018") +
  geom_hline(yintercept = 0, colour = "darkred") + geom_vline(xintercept = 10, colour = "darkred") +
  geom_text(data = annotations,
            aes(
              x = xpos,
              y = ypos,
              hjust = hjustvar,
              vjust = vjustvar,
              label = annotateText
            ))


#out.table.final <- out.table.final[!is.na(out.table.final$ALC.Hours),]
out.table.final.gis <-  out.table.final %>%
  left_join(gis[, 1], by = c("DMA.Code" = "DMA_Ref"))
out.table.final.gis <- st_as_sf(out.table.final.gis)
st_crs(out.table.final.gis) <- 27700
st_write(out.table.final.gis,
         "DMA Profiles for HYP1_2 v2.shp",
         layer_options = "GEOMETRY=AS_WKT")


# map plot
table2 <- out.table.final.gis %>%
  mutate(`May18.Nov17.group` = ifelse(May18.Nov17.percentage <= -50, "-100% to -50%",
                                      (
                                        ifelse(
                                          May18.Nov17.percentage <= -30 &
                                            May18.Nov17.percentage > -50,
                                          "-50% to -30%",
                                          (
                                            ifelse(
                                              May18.Nov17.percentage <= -15 &
                                                May18.Nov17.percentage > -30,
                                              "-30% to -15%",
                                              (
                                                ifelse(
                                                  May18.Nov17.percentage <= 0 &
                                                    May18.Nov17.percentage > -15,
                                                  "-15% to 0%",
                                                  (
                                                    ifelse(
                                                      May18.Nov17.percentage <= 15 &
                                                        May18.Nov17.percentage > 0,
                                                      "0% to 15%",
                                                      (
                                                        ifelse(
                                                          May18.Nov17.percentage <= 30 &
                                                            May18.Nov17.percentage > 15,
                                                          "15% to 30%",
                                                          (
                                                            ifelse(
                                                              May18.Nov17.percentage <= 50 &
                                                                May18.Nov17.percentage > 30,
                                                              "30% to 50%",
                                                              "> 50%"
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      ))) %>%
  #arrange(desc(Proportion)) %>%
  ungroup()
table2$`May18.Nov17.group` <- fct_relevel(
  table2$`May18.Nov17.group`,
  "-100% to -50%",
  "-50% to -30%",
  "-30% to -15%",
  "-15% to 0%",
  "0% to 15%",
  "15% to 30%",
  "30% to 50%",
  "> 50%"
)

tmap_mode("view")
plot21 <-
  tm_shape(table2) + tm_polygons(
    col = "May18.Nov17.group",
    alpha = 0.75,
    title = "Percentage Increase in Nightflow, November 2017 to May 2018",
    palette = c("darkgreen", "forestgreen", "gold", "red3"),
    style = "pretty",
    n = 8,
    legend.reverse = F,
    auto.palette.mapping = T,
    lwd = 1
  ) + tm_basemap("Esri.WorldStreetMap")
plot21



# map plot
table.2 <-
  out.table.final.gis %>% filter(!is.na(Nov18.Nov17.percentage)) %>%
  mutate(`Nov18.Nov17.group` = ifelse(Nov18.Nov17.percentage <= -50, "-100% to -50%",
                                      (
                                        ifelse(
                                          Nov18.Nov17.percentage <= -30 &
                                            Nov18.Nov17.percentage > -50,
                                          "-50% to -30%",
                                          (
                                            ifelse(
                                              Nov18.Nov17.percentage <= -15 &
                                                Nov18.Nov17.percentage > -30,
                                              "-30% to -15%",
                                              (
                                                ifelse(
                                                  Nov18.Nov17.percentage <= 0 &
                                                    Nov18.Nov17.percentage > -15,
                                                  "-15% to 0%",
                                                  (
                                                    ifelse(
                                                      Nov18.Nov17.percentage <= 15 &
                                                        Nov18.Nov17.percentage > 0,
                                                      "0% to 15%",
                                                      (
                                                        ifelse(
                                                          Nov18.Nov17.percentage <= 30 &
                                                            Nov18.Nov17.percentage > 15,
                                                          "15% to 30%",
                                                          (
                                                            ifelse(
                                                              Nov18.Nov17.percentage <= 50 &
                                                                Nov18.Nov17.percentage > 30,
                                                              "30% to 50%",
                                                              "> 50%"
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      ))) %>%
  #arrange(desc(Proportion)) %>%
  ungroup()
table.2$`Nov18.Nov17.group` <- fct_relevel(
  table.2$`Nov18.Nov17.group`,
  "-100% to -50%",
  "-50% to -30%",
  "-30% to -15%",
  "-15% to 0%",
  "0% to 15%",
  "15% to 30%",
  "30% to 50%",
  "> 50%"
)

tmap_mode("view")
plot22 <-
  tm_shape(table.2) + tm_polygons(
    col = "Nov18.Nov17.group",
    alpha = 0.75,
    title = "Percentage Increase in Nightflow, November 2017 to November 2018",
    palette = c("darkgreen", "forestgreen", "gold", "red3"),
    style = "pretty",
    n = 8,
    legend.reverse = F,
    auto.palette.mapping = T,
    lwd = 1
  ) + tm_basemap("Esri.WorldStreetMap")
plot22

tmap_arrange(plot21, plot22)












# HYPOTHESIS 2.1 ------------------------------------------

normalise <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

# load data
setwd("C:\\Users\\TOsosanya\\Desktop\\Southern Water\\Phase 2\\Christina")
nnf.ac <-
  read.csv(
    "C:\\Users\\TOsosanya\\Desktop\\Southern Water\\Phase 2\\Validated Nightflow v3.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  )
dma.lookup <-
  data.table(read.csv("I:\\Projects\\Client\\1892\\Analytics\\DMA Lookup.csv"))
mr <-
  data.table(read.csv(
    "I:\\Projects\\Client\\1892\\Analytics\\replaced_length.csv"
  ))

# clean, transform data
mr$Month <- as.character(mr$Month)
mr$Month <- paste(mr$Month, "-01", sep = "")
mr$Month  <- ymd(mr$Month)

mr2 <- mr %>% ungroup() %>%  dplyr::group_by(DMA_Ref) %>%
  dplyr::mutate(row = dplyr::row_number()) %>%
  mutate(new = ifelse(row == 1, 0,
                      (ifelse(
                        row != 1  & Month - months(3) < lag(Month), 0, 1
                      )))) %>%
  mutate(new2 = new + 1)

mr3 <- split(mr2, mr2$DMA_Ref)

diagonal_sum <- function(x) {
  for (j in 1:nrow(x)) {
    onerow <- data.table(x[j, ])
    onerow.output <- onerow[, 6]
    if (j == 1) {
      onerow.output.1 <- onerow.output + 1
      final <- onerow.output.1
    } else{
      final.2 <- final[j - 1] + onerow.output
      final <- rbind(final, final.2)
    }
  }
  final$DMA_Ref <- x$DMA_Ref
  final$row <- x$row
  return(final)
}

#lapply(diagonal_sum(mr3[[9]]))
mr11 <- lapply(mr3, diagonal_sum)
mr12 <- rbindlist(mr11, use.names = TRUE, fill = TRUE)
mr13 <-
  mr12 %>% left_join(mr2[, c("DMA_Ref" , "Month", "Length", "row")], by = c("DMA_Ref", "row")) %>%
  select(DMA_Ref, Month, Length, row, new) %>%
  dplyr::group_by(DMA_Ref, new) %>%
  dplyr::mutate(First.Date = min(Month)) %>%
  dplyr::mutate(Last.Date = max(Month)) %>%
  dplyr::mutate(Replaced_Length = sum(Length)) %>%
  select(-c(Month, Length, row)) %>% distinct() %>%
  ungroup() %>% group_by(DMA_Ref) %>%
  dplyr::mutate(Total_DMA_Length = sum(Replaced_Length)) %>%
  arrange(First.Date) %>%
  as.data.frame()


nnf.ac2 <-
  nnf.ac %>% select(DMA.Code, Feb.15:Dec.18) %>% gather(MonthYear1, Value, Feb.15:Dec.18, factor_key =
                                                          FALSE)
nnf.ac2$MonthYear1 <- paste("01.", nnf.ac2$MonthYear1, sep = "")
nnf.ac2$MonthYear1  <- dmy(nnf.ac2$MonthYear1)






# Before & After freeze thaw ----------------------------------------------

mr20 <-
  mr13 %>% select(DMA_Ref,
                  new,
                  Replaced_Length,
                  Total_DMA_Length,
                  First.Date,
                  Last.Date) %>%
  gather(Category, Date1, First.Date:Last.Date, factor_key = FALSE)

mr21 <- mr20 %>%
  #rename(MonthYear1 = Date1) %>%
  mutate(MR.Data = "Yes") %>%
  left_join(nnf.ac2, by = c("DMA_Ref" = "DMA.Code")) %>%
  filter(!is.na(Value)) #%>%
#ungroup() %>%
#group_by(DMA_Ref)

mr22 <-
  mr21 %>% select(-c(new, Replaced_Length, Total_DMA_Length)) %>%
  filter(Category == "First.Date") %>%
  distinct() %>% group_by(DMA_Ref) %>% arrange(MonthYear1)

mr22.1 <-
  mr21 %>% select(-c(new, Replaced_Length, Total_DMA_Length)) %>%
  filter(Category == "Last.Date") %>%
  distinct() %>% arrange(MonthYear1)
mr23 <- split(mr22, mr22$DMA_Ref)
mr23.1 <- split(mr22.1, mr22.1$DMA_Ref)


x1 <- mr23[[8]] %>% arrange(MonthYear1, Date1) %>% as.data.table()
x2 <- mr23.1[[8]] %>% arrange(MonthYear1, Date1) %>% as.data.table()


for (j in 1:length(unique(x1$Date1))) {
  test <- data.frame(x1)
  test.output <-
    unique(test[which(unique(test[which(test$Date1[[j]] == test$MonthYear1), 5]) -
                        months(1) == test$MonthYear1), 6]) -
    unique(test[which(unique(test[which(test$Date1[[j]] == test$MonthYear1), 5]) -
                        months(3) == test$MonthYear1), 6])
  #test.output$DMA_Ref <- unique(test$DMA_Ref[[1]])
  if (j == 1) {
    output.data11 <-  test.output
  } else{
    output.data11 <- rbind(output.data11, test.output)
  }
}

# calculate for before
Before_3months_LC <- function(x) {
  for (j in 1:length(unique(x$Date1))) {
    test <- data.frame(x)
    test.output <-
      (unique(test[which(unique(test[which(test$Date1[j] == test$MonthYear1), 5]) -
                           months(1) == test$MonthYear1), 6]) +
         unique(test[which(unique(test[which(test$Date1[j] == test$MonthYear1), 5]) -
                             months(2) == test$MonthYear1), 6]) +
         unique(test[which(unique(test[which(test$Date1[j] == test$MonthYear1), 5]) -
                             months(3) == test$MonthYear1), 6])) / 3
    if (j == 1) {
      output.data11 <-  test.output
    } else{
      output.data11 <- rbind(output.data11, test.output)
    }
  }
  output.data11$DMA_Ref <- x$DMA_Ref[1]
  return(output.data11)
}

Before_3months_LC(x1)


mr31 <- lapply(mr23, Before_3months_LC)
mr32 <- rbindlist(mr31, use.names = TRUE, fill = TRUE) %>%
  mutate(Event = "3 Months Before") %>%
  select(DMA_Ref, V1, V2, V3, Event)

select_vars <- mr32 %>% select(V1:V3)
mr33 <-
  mr32 %>% mutate(mean_sel = rowMeans(select_vars, na.rm = T)) %>%
  select(-c(V1:V3))
mr33$mean_sel[is.nan(mr33$mean_sel)] <- 0

# calculate for after
After_3months_LC <- function(x) {
  for (j in 1:length(unique(x$Date1))) {
    test <- data.frame(x)
    test.output <-
      (unique(test[which(unique(test[which(test$Date1[j] == test$MonthYear1), 5]) +
                           months(3) == test$MonthYear1), 6]) +
         unique(test[which(unique(test[which(test$Date1[j] == test$MonthYear1), 5]) +
                             months(2) == test$MonthYear1), 6]) +
         unique(test[which(unique(test[which(test$Date1[j] == test$MonthYear1), 5]) +
                             months(1) == test$MonthYear1), 6])) / 3
    if (j == 1) {
      output.data12 <-  test.output
    } else{
      output.data12 <- rbind(output.data12, test.output)
    }
  }
  output.data12$DMA_Ref <- x$DMA_Ref[1]
  return(output.data12)
}

After_3months_LC(x2)

mr31.1 <- lapply(mr23.1, After_3months_LC)
mr32.1 <- rbindlist(mr31.1, use.names = TRUE, fill = TRUE) %>%
  mutate(Event = "3 Months After") %>%
  select(DMA_Ref, V1, V2, V3, Event)

select_vars_2 <- mr32.1 %>% select(V1:V3)
mr33.1 <-
  mr32.1 %>% mutate(mean_sel = rowMeans(select_vars_2, na.rm = T)) %>%
  select(-c(V1:V3))
mr33.1$mean_sel[is.nan(mr33.1$mean_sel)] <- 0

out <- rbind(mr33.1, mr33)
# NOTE: out has the before and after leakage change data
# mr13 has the first and last dates as well as total dma length data
# but we need mr32 and mr32.1 in long format (add Leakage Before, Leakage After) to join to mr13

mr13 %>% arrange(DMA_Ref)
mr41 <-
  mr32 %>% gather(new, Leakage.Before, V1:V3) %>% mutate(new = ifelse(new == "V1", 1,
                                                                      (ifelse(
                                                                        new == "V2", 2,
                                                                        (ifelse(new == "V3", 3, 4))
                                                                      ))))
mr41.1 <-
  mr32.1 %>% gather(new, Leakage.After, V1:V3) %>% mutate(new = ifelse(new == "V1", 1,
                                                                       (ifelse(
                                                                         new == "V2", 2,
                                                                         (ifelse(new == "V3", 3, 4))
                                                                       ))))
mr42 <- mr41 %>% left_join(mr41.1, by = c("DMA_Ref", "new")) %>%
  group_by(DMA_Ref) %>% arrange(DMA_Ref) %>% filter(!is.na(Leakage.Before)) %>%
  select(-c(Event.x, Event.y)) %>%
  left_join(mr13, by = c("DMA_Ref", "new")) %>% #join with mr13
  dplyr::rename(Programme = new) %>%
  as.data.frame()
mr42 <- mr42 %>%
  mutate(
    `Leakage.Difference/Replaced_length` = (Leakage.After - Leakage.Before) /
      Replaced_Length
  ) %>%
  mutate(
    `Leakage.Difference/Total_length` = (Leakage.After - Leakage.Before) / Total_DMA_Length
  )

# write the output to a csv file
write.csv(
  mr42,
  "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/Average MNF before and after Mains Replacement v2.csv",
  row.names = FALSE
)

# for further analyses and plots
mr43 <-
  mr42 %>%  #filter(Total_DMA_Length > 50) %>% #group_by(Category) %>%
  filter(!is.na(Leakage.After)) %>%
  dplyr::summarise(Before = sum(Leakage.Before),
                   After = sum(Leakage.After)) %>%
  gather(Category, Total, Before:After)
mr43$Category <- fct_relevel(mr43$Category, "Before", "After")
mr43 %>%
  ggplot(aes(Category, Total), position = "dodge") +
  geom_bar(aes(fill = Category), col = "black", stat = "identity") +
  #scale_x_continuous(name = "Day", breaks = c(1:10)) +
  scale_y_continuous(
    #limits = c(2500000,3000000),
    breaks = c(0, 500000, 1000000, 1500000, 2000000, 2500000),
    name = "Total Nightflow",
    labels = function(x) {
      paste0(x / 10 ^ 3, 'K')
    }
  ) +
  scale_fill_manual(values = SEAMScolours[c(2, 4)]) + gg +
  labs(title = "Average MNF prior to and following Mains Replacement")


# HYPOTHESIS 2.7  -------------------------------------------------

# UMP
# load data
setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/UMP")
files <-
  list.files(path = "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/UMP",
             pattern = "*.xlsx",
             full.names = T)
tbl <- sapply(files, read_excel)  %>%
  plyr::rbind.fill()
str(tbl)

# clean, transform data
tbl$Postcode <- gsub("[[:blank:]]", "", tbl$Postcode)

tbl2 <- tbl %>%
  mutate(Date1 = substring(tbl$`Actual Completion Date`, 1, 7)) %>%
  filter(`Ellipse User status (Long description)` == "WORK COMPLETE") %>%
  group_by(Date1) %>%
  tally(name = "Total.UMP")

tbl2$Date1 <- paste(tbl2$Date1, "-01",  sep = "")
tbl2$Date1  <- ymd(tbl2$Date1)
UMP0 <-
  tbl2 %>% filter(!is.na(Date1) &
                    Total.UMP > 50) %>% arrange(Date1) %>%
  as.data.frame()


# sort the dma postcode thing..
# calculate the proportion of ump installation per dma per month i.e. dma, month, amount installed,
# total installed = dma, month, proportion ; month, sum proportion

# load data
comms10_16 <-
  read.csv("C://Users//TOsosanya//Desktop//Southern Water//Phase 1//comms10_16.csv")

# clean, transform data
tbl_1 <-
  tbl %>% #mutate(`District Meter Area`=as.factor(`District Meter Area`)) %>%
  left_join(unique(comms10_16[, 3:4]), by = c("Postcode" = "Post.Code")) #%>%
#mutate(DMA_Ref = ifelse(is.na(District.Meter.Area),District.Meter.Area,`District Meter Area`))
summary(tbl_1)
ump_installed_year <- tbl_1 %>%
  mutate(Date1 = substring(tbl_1$`Actual Completion Date`, 1, 7)) %>%
  filter(`Ellipse User status (Long description)` == "WORK COMPLETE") %>%
  dplyr::rename(DMA_Ref = District.Meter.Area) %>%
  group_by(Date1) %>%
  tally(name = "Total.UMP") %>%
  ungroup() %>%
  dplyr::select(DMA_Ref, Date1, Total.UMP) %>%
  spread(key = Date1, Total.UMP) %>%
  #filter(DMA_Ref != "") %>%
  dplyr::select(-c(`2019`))

# write the output to the csv file
write.csv(
  ump_installed_year,
  "C://Users//TOsosanya//Desktop//Southern Water//Phase 2//UMP Meters Installed per Year.csv",
  row.names = F
)

tbl_2 <- tbl_1 %>%
  mutate(Date1 = substring(tbl_1$`Actual Completion Date`, 1, 7)) %>%
  filter(`Ellipse User status (Long description)` == "WORK COMPLETE") %>%
  dplyr::rename(DMA_Ref = District.Meter.Area) %>%
  group_by(Date1, DMA_Ref) %>%
  tally(name = "Total.UMP") %>%
  ungroup() %>%
  group_by(DMA_Ref) %>%
  dplyr::mutate(sum.ump = sum(Total.UMP)) %>%
  group_by(Date1, DMA_Ref) %>%
  dplyr::summarise(Percentage = (Total.UMP / sum.ump) * 100) %>%
  filter(!is.na(DMA_Ref)) %>%
  ungroup() %>%
  group_by(Date1) %>%
  dplyr::summarise(Total.UMP = mean(Percentage, na.rm = T))

tbl_2$Date1 <- paste(tbl_2$Date1, "-01",  sep = "")
tbl_2$Date1  <- ymd(tbl_2$Date1)
UMP <- tbl_2 %>% filter(!is.na(Date1)) %>% arrange(Date1) %>%
  filter(between(Date1, ymd("2011-01-01"), ymd("2019-01-01"))) %>%
  as.data.frame()

UMP0_ <-
  UMP %>% dplyr::rename(Total.UMP.Proportion = Total.UMP) %>%
  left_join(UMP0, by = "Date1")


# comms - jobs promoted

# load data
comm <-
  read.csv(
    "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/combined comms pipe filtered.csv"
  )

# clean, transform data
comm$Completion.D.Date <- dmy(comm$Completion.D.Date)
comm_2 <-
  comm %>% filter(between(Completion.D.Date, ymd("2010-12-01"), ymd("2019-12-01")))
comm_2$CommsMonth <- substring(comm_2$Completion.D.Date, 1, 7)
comm_2 <-
  comm_2 %>% select(
    Work.Order.No,
    Address,
    Post.Code,
    District.Meter.Area,
    CommsMonth,
    Standard.Job.Desc
  )
comm_2$Job.promted <-
  revalue(
    comm_2$Standard.Job.Desc,
    c(
      "COMM PIPE-REPAIR LEAK 15-32MM" = "Comms Pipe or stop tap",
      "WL COMM PIPE REPLACE" = "Comms Pipe or stop tap",
      "WN COMM PIPE DEFECT" = "Comms Pipe or stop tap",
      "WN COMM PIPE REPAIR" = "Comms Pipe or stop tap",
      "RECH: CUSTOMER SUPPLY PIPE RENEW" = "Customer supply pipe",
      "RECH: CUSTOMER SUPPLY PIPE REPAIR" = "Customer supply pipe",
      "WL STOP TAP REPLACE" = "Comms Pipe or stop tap",
      "WL STOP TAP REPAIR" = "Comms Pipe or stop tap",
      "WN STOP TAP REPLACE" = "Comms Pipe or stop tap",
      "WN STOP TAP REPAIR" = "Comms Pipe or stop tap",
      "WN MAIN REPLACE" = "Mains",
      "WN MAIN REPAIR" = "Mains",
      "WN MAINS FITTING REPLACE" = "Mains",
      "WN MAINS FITTING REPAIR" = "Mains",
      "WL MAINS FITTING REPAIR" = "Mains",
      "WL MAINS FITTING REPLACE" = "Mains",
      "WL MAIN REPLACE" = "Mains",
      "WL MAIN REPAIR" = "Mains"
    )
  )
comm_3 <- comm_2 %>%
  group_by(CommsMonth, Job.promted) %>%
  tally(name = "Jobs") %>%
  mutate(Jobs = replace_na(Jobs, 0)) %>%
  spread(key = Job.promted, Jobs) %>%
  select(-c(`MAIN - CUT AND CAP`))
comm_3$Date1 <-
  paste(comm_3$CommsMonth, "-01")
comm_3$CommsMonth <- NULL
comm_3$Date1 <- ymd(comm_3$Date1)
comms_ump <- comm_3 %>%
  left_join(UMP0_, by = "Date1") %>%
  filter(between(Date1, ymd("2011-01-01"), ymd("2019-01-01"))) %>%
  mutate(`Comms Pipe or stop tap` = replace_na(`Comms Pipe or stop tap`, 0)) %>%
  mutate(Mains = replace_na(Mains, 0)) %>%
  mutate(`Customer supply pipe` = replace_na(`Customer supply pipe`, 0))


cor(comms_ump[, -4], method = "pearson")


# mlm
mfit = lm(Total.UMP ~ `Comms Pipe` + Mains + `Customer supply pipe or stop tap`,
          data = comms_ump)#,weights = Total.UMP)
summary(mfit)
mco = coef(summary(mfit))
mco[, 1]
plot(mfit)

mfit = lm(Total.UMP.Proportion ~ `Comms Pipe` + Mains + `Customer supply pipe or stop tap`,
          data = comms_ump)#,weights = Total.UMP)
summary(mfit)
mco = coef(summary(mfit))
mco[, 1]
plot(mfit)


# ump proportion <- mains(-)
# ump <- comms and mains(-)


# alc
alc <-
  lde1 %>% mutate(Date1 = substring(lde1$Completion.D.Date, 1, 7)) %>%
  dplyr::group_by(Date1) %>% dplyr::summarise(ALC.Hours = sum(Actual.Labour.Hours, na.rm = TRUE)) %>%
  na.omit()

alc$Date1 <- paste(alc$Date1, "-01", sep = "")
alc$Date1  <- ymd(alc$Date1)
ALC <- alc %>% filter(!is.na(Date1)) %>% arrange(Date1) %>%
  as.data.frame()


# Mains Replacement

# load data
setwd("I://Projects//Client//1892//Analytics")
new.mains <-
  data.table(read.csv("New_Mains_DATE_LAID_1999_DMAv2.csv"))

# New Growth
new.mains.filtered <-
  new.mains[Replace == 0][substr(DATE_LAID, nchar(as.character(DATE_LAID)) - 12, nchar(as.character(DATE_LAID))
                                 - 9) %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)][DMA_Ref != " "]
new.mains.filtered$Month <-
  paste0(
    substr(
      new.mains.filtered$DATE_LAID,
      nchar(as.character(new.mains.filtered$DATE_LAID)) - 12,
      nchar(as.character(new.mains.filtered$DATE_LAID)) - 9
    ),
    "-",
    substr(
      new.mains.filtered$DATE_LAID,
      nchar(as.character(new.mains.filtered$DATE_LAID)) - 15,
      nchar(as.character(new.mains.filtered$DATE_LAID)) - 14
    )
  )

# sum of pipe length by DMA
new.mains.filtered.by.dma <-
  new.mains.filtered[, .(Total_DMA_Length = sum(PIPE_LENGT)), by = .(DMA_Ref)]
new.mains.filtered.by.dma.by.month <-
  new.mains.filtered[, .(Length = sum(PIPE_LENGT)), by = .(DMA_Ref, Month)]

setkey(new.mains.filtered.by.dma, DMA_Ref)
setkey(new.mains.filtered.by.dma.by.month, DMA_Ref)
new.mains.filtered.by.dma.by.month <-
  new.mains.filtered.by.dma[new.mains.filtered.by.dma.by.month, ][, .(DMA_Ref, Month, Length, Total_DMA_Length)]
setkey(new.mains.filtered.by.dma.by.month, DMA_Ref, Month)

names(new.mains.filtered.by.dma.by.month) <-
  paste("new.", names(new.mains.filtered.by.dma.by.month), sep = "")
New_Length <- new.mains.filtered.by.dma.by.month %>%
  dplyr::group_by(new.Month) %>% dplyr::summarise(Total.newlength = sum(new.Length, na.rm = TRUE))
#new.Total_DMA_Length = sum(new.Total_DMA_Length,na.rm = TRUE))




# Replaced Pipes
replaced.mains.filtered <-
  new.mains[Replace > 0][substr(DATE_LAID, nchar(as.character(DATE_LAID)) - 12, nchar(as.character(DATE_LAID))
                                - 9) %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)][DMA_Ref != " "]
replaced.mains.filtered$Month <-
  paste0(
    substr(
      replaced.mains.filtered$DATE_LAID,
      nchar(as.character(replaced.mains.filtered$DATE_LAID)) - 12,
      nchar(as.character(replaced.mains.filtered$DATE_LAID)) - 9
    ),
    "-",
    substr(
      replaced.mains.filtered$DATE_LAID,
      nchar(as.character(replaced.mains.filtered$DATE_LAID)) - 15,
      nchar(as.character(replaced.mains.filtered$DATE_LAID)) - 14
    )
  )

replaced.mains.filtered.by.dma <-
  replaced.mains.filtered[, .(Total_DMA_Length = sum(PIPE_LENGT)), by = .(DMA_Ref)]
replaced.mains.filtered.by.dma.by.month <-
  replaced.mains.filtered[, .(Length = sum(PIPE_LENGT)), by = .(DMA_Ref, Month)]

setkey(replaced.mains.filtered.by.dma, DMA_Ref)
setkey(replaced.mains.filtered.by.dma.by.month, DMA_Ref)
replaced.mains.filtered.by.dma.by.month <-
  replaced.mains.filtered.by.dma[replaced.mains.filtered.by.dma.by.month, ][, .(DMA_Ref, Month, Length, Total_DMA_Length)]
setkey(replaced.mains.filtered.by.dma.by.month, DMA_Ref, Month)

replaced.mains.filtered.by.dma
replaced.mains.filtered.by.dma.by.month
names(replaced.mains.filtered.by.dma.by.month) <-
  paste("replaced.",
        names(replaced.mains.filtered.by.dma.by.month),
        sep = "")
Replaced_Length <- replaced.mains.filtered.by.dma.by.month %>%
  dplyr::group_by(replaced.Month) %>% dplyr::summarise(Total.replacedlength = sum(replaced.Length, na.rm = TRUE))
#replaced.Total_DMA_Length = sum(replaced.Total_DMA_Length,na.rm = TRUE))


Mains.Replacement <-
  Replaced_Length %>% left_join(New_Length, by = c("replaced.Month" = "new.Month"))
Mains.Replacement$replaced.Month <-
  paste(Mains.Replacement$replaced.Month, "-01", sep = "")
Mains.Replacement$replaced.Month  <-
  ymd(Mains.Replacement$replaced.Month)
Mains.Replacement.New.Growth <- Mains.Replacement %>%
  filter(!is.na(replaced.Month)) %>%
  dplyr::rename(Date1 = replaced.Month) %>%
  dplyr::rename(Mains.Replacement = Total.replacedlength) %>%
  dplyr::rename(New.Growth = Total.newlength) %>%
  arrange(Date1) %>%
  as.data.frame()


plot(Mains.Replacement$Date1,
     Mains.Replacement$Total.replacedlength)
plot(Mains.Replacement$Date1, Mains.Replacement$Total.newlength)


# Monthly temperature
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

# clean, transform data
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
temp <- gather(both_1, Date1, Value, X2019:X2015, factor_key = FALSE)
temp$Date1 <- substring(temp$Date1, 2, 5)

temp$Date1 <- paste(substring(temp$X, 4, 6), sep = ".", temp$Date1)
temp$X <- NULL

temp3 <-
  temp %>% ungroup() %>% #mutate(MonthYear1 = as.character(MonthYear1)) %>%
  dplyr::group_by(Date1) %>%
  dplyr::summarise(Average.Temperature = mean(Value, na.rm = T))
temp3$Date1 <- paste("01.", temp3$Date1, sep = "")
temp3$Date1  <- dmy(temp3$Date1)
Temperature <- temp3 %>% arrange(Date1) %>%
  as.data.frame()



# Monthly Nightflow
# load data
nn <-
  data.table(
    read.csv(
      "I:\\Projects\\Client\\1915\\Analytics\\NightFlow-Combined-v2.2.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )

# clean, transform data
nn.1 <- gather(nn, Date1, Value, Jan.11:Oct.19, factor_key = TRUE)
nn.1$Date1 <- as.yearmon(nn.1$Date1, format = "%b.%y")
nn.1$Date1 <- paste("01 ", nn.1$Date1, sep = "")
nn.1$Date1  <- dmy(nn.1$Date1)

Leakage <- nn.1 %>% ungroup() %>% filter(!is.na(Value)) %>%
  dplyr::group_by(Date1) %>% #mutate(Date1 = as.character(Date1)) %>%
  dplyr::summarise(Average.Nightflow = mean(Value, na.rm = T))

# create a leakage/km DMA/year
Leakage2 <- nn.1 %>% ungroup() %>% #filter(!is.na(Value)) %>%
  mutate(Date1 = substring(Date1, 1, 7)) %>%
  left_join(
    replaced.mains.filtered.by.dma.by.month[, 1:3],
    by = c("DMA_Ref" = "replaced.DMA_Ref", "Date1" = "replaced.Month")
  ) %>%
  left_join(
    new.mains.filtered.by.dma.by.month[, 1:3],
    by = c("DMA_Ref" = "new.DMA_Ref", "Date1" = "new.Month")
  ) %>%
  dplyr::group_by(DMA_Ref, Date1) %>% #mutate(Date1 = as.character(Date1)) %>%
  dplyr::summarise(Average.Nightflow = mean(Value, na.rm = T),
                   Length = replaced.Length + new.Length)

test <- nn.1 %>% ungroup() %>% #filter(!is.na(Value)) %>%
  mutate(Date1 = substring(Date1, 1, 7)) %>%
  left_join(
    replaced.mains.filtered.by.dma.by.month[, 1:3],
    by = c("DMA_Ref" = "replaced.DMA_Ref", "Date1" = "replaced.Month")
  ) %>%
  left_join(
    new.mains.filtered.by.dma.by.month[, 1:3],
    by = c("DMA_Ref" = "new.DMA_Ref", "Date1" = "new.Month")
  ) %>%
  dplyr::group_by(DMA_Ref, Date1)



# Bring all together
all <- Mains.Replacement.New.Growth %>%
  left_join(ALC, by = "Date1") %>%
  left_join(UMP, by = "Date1") %>%
  left_join(Leakage, by = "Date1") %>%
  left_join(Temperature, by = "Date1")
all.together <- #Mains.Replacement.New.Growth %>%
  #left_join(ALC,by = "Date1") %>%
  UMP %>%
  left_join(Leakage, by = "Date1") #%>%
#left_join(Temperature,by = "Date1") #%>%
#mutate(UMP = ifelse(between(row_number())))
#all.together[62:97,5] <- NA #clean the ump data that look anomalous

# write output to csv file
write.csv(
  all.together,
  "C:\\Users\\TOsosanya\\Desktop\\Southern Water\\Phase 2\\Monthly measure values for dashboard.csv",
  row.names = F
)

all.together[, -1] <- lapply(all.together[, -1], normalise)

leakage.ump <-
  all.together[4:97, c("Date1", "Total.UMP", "Average.Nightflow")] # testing extra
leakage.ump_ <- leakage.ump[, -1]
rownames(leakage.ump_) <- leakage.ump[, 1]
Total.UMP.plot.2 <-
  ts(
    leakage.ump_$Total.UMP,
    start = c(2011, 1),
    end = c(2019, 1),
    freq = 12
  )
Average.Nightflow.plot.2 <-
  ts(leakage.ump_$Average.Nightflow,
     start = c(2011, 4),
     freq = 12)


# set 1st column as row name
all.together_ <- all.together[, -1]
rownames(all.together_) <- all.together[, 1]


# check for correlation
cor(all.together_[complete.cases(all.together_),], method = "pearson")


# check for time series correlation
Mains.Replacement.plot <-
  ts(all.together_$Mains.Replacement,
     start = c(2011, 1),
     freq = 12)
New.Growth.plot <-
  ts(all.together_$New.Growth,
     start = c(2011, 1),
     freq = 12)
ALC.Hours.plot <-
  ts(all.together_$ALC.Hours,
     start = c(2015, 4),
     freq = 12)
Total.UMP.plot <-
  ts(
    all.together_$Total.UMP,
    start = c(2011, 1),
    end = c(2019, 1),
    freq = 12
  )
Average.Nightflow.plot <-
  ts(all.together_$Average.Nightflow,
     start = c(2011, 4),
     freq = 12)
Average.Temperature.plot <-
  ts(all.together_$Average.Temperature,
     start = c(2015, 1),
     freq = 12)

time(Mains.Replacement.plot)
time(ALC.Hours.plot)
plot(Mains.Replacement.plot)
abline(reg = lm(Mains.Replacement.plot ~ time(Mains.Replacement.plot)))
decompose(Mains.Replacement.plot)
plot(decompose(Mains.Replacement.plot))

ts.plot(all.together_,
        col = c("red", "orange", "yellow", "green", "blue", "black"))
ts.plot(Mains.Replacement.plot,
        New.Growth.plot,
        col = c("red", "orange"))
ts.plot(all[, c(5, 6)], col = c("red", "orange"))
ts.plot(all[, 6], col = c("red"))
ts.plot(all.together[, 4])
ts.plot(all.together[, 7])


ts.plot(Average.Nightflow.plot.2,
        Total.UMP.plot.2,
        col = c("red", "green3")) # testing
acf(ts.intersect(Average.Nightflow.plot.2, Total.UMP.plot.2, dframe = TRUE)) # testing
ggplot(leakage.ump_, aes(Total.UMP, Average.Nightflow)) +
  geom_jitter(aes(), col = "blue") +
  geom_smooth(aes(), col = "red", method = "lm") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

all.together %>%
  gather(Category, Value, Total.UMP:Average.Nightflow) %>%
  ggplot() +
  geom_line(aes(x = Date1, y = Value, colour = Category), size = 1.3) +
  labs(title = "UMP Penetration vs Nightflow", y = "Value", x = "") + gg +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  scale_x_date(labels = date_format("%b-%y"), breaks = "3 months")

ts.plot(Mains.Replacement.plot, New.Growth.plot, col = c("blue", "red"))
acf(New.Growth.plot)
acf(Mains.Replacement.plot, lag.max = 12, plot = F)
acf(ALC.Hours.plot, na.action = na.pass)
acf(ALC.Hours.plot, na.action = na.pass, plot = F)
acf(Total.UMP.plot, type = "correlation", plot = F)
acf(Average.Nightflow.plot, na.action = na.pass)
acf(Average.Temperature.plot, na.action = na.pass)
acf(
  ts.intersect(
    Mains.Replacement.plot,
    New.Growth.plot,
    ALC.Hours.plot,
    Total.UMP.plot,
    Average.Nightflow.plot,
    Average.Temperature.plot
  )
)
pacf(ts.intersect(Mains.Replacement.plot, New.Growth.plot, dframe = TRUE))


# ump vs comms work

# load data
newdata <-
  read.csv("C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/Hyp 2_7 data.csv")

# clean, transform data
newdataH <-
  newdata %>% filter(County == "Hampshire") %>% select(-4) %>% na.omit()
UMPH <- ts(newdataH$UMP, start = c(2011))
Comms.and.stop.tapsH <-
  ts(newdataH$Comms.and.stop.taps, start = c(2011))

ts.plot(UMPH, Comms.and.stop.tapsH, col = c("red", "green3")) # testing
acf(ts.intersect(UMPH, Comms.and.stop.tapsH, dframe = TRUE)) # testing


newdataK <-
  newdata %>% filter(County == "Kent") %>% select(-4) %>% na.omit()
UMPK <- ts(newdataH$UMP, start = c(2011))
Comms.and.stop.tapsK <-
  ts(newdataK$Comms.and.stop.taps, start = c(2011))

ts.plot(UMPK, Comms.and.stop.tapsK, col = c("red", "green3")) # testing
acf(ts.intersect(UMPK, Comms.and.stop.tapsK, dframe = TRUE)) # testing


# mlm
mfit = lm(
  Average.Nightflow ~ Mains.Replacement + New.Growth + ALC.Hours + Total.UMP + Average.Temperature,
  data = all.together_
)#,weights = Total.UMP)
summary(mfit)
mco = coef(summary(mfit))
mco[, 1]
plot(mfit)


# HYPOTHESIS 2.3 -----------------------------------------

# load data
files <-
  list.files(path = "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/Comms",
             pattern = "*.xlsx",
             full.names = T)

# file_list <- list.files()
# comms <- Reduce(rbind.fill, sapply(files, read_excel, sheet = 2))

selecta <- function(x) {
  for (i in 1:length(x)) {
    a <-
      read_excel(files[[1]], sheet = 2, col_types = "text") %>% select(
        `Work Order No`,
        `Work Order description`,
        `Work Request No`,
        `Creation Date`,
        `Completion Date`,
        `Completion D Date`,
        `Standard Job Desc`,
        `Standard Job No`,
        `Originator Emp Id`,
        `Address`,
        `Town or City`,
        `Post Code`,
        `User Status - desc`,
        `District Meter Area`,
        `Revised Job Number`
      )
  }
  
}


a <-
  read_excel(files[[1]], sheet = 2, col_types = "text") %>% select(
    `Work Order No`,
    `Work Order description`,
    `Work Request No`,
    `Creation Date`,
    `Completion Date`,
    `Completion D Date`,
    `Standard Job Desc`,
    `Standard Job No`,
    `Originator Emp Id`,
    `Address`,
    `Town or City`,
    `Post Code`,
    `User Status - desc`,
    `District Meter Area`,
    `Revised Job Number`
  )
b <-
  read_excel(files[[2]], sheet = 2, col_types = "text") %>% select(
    `Work Order No`,
    `Work Order description`,
    `Work Request No`,
    `Creation Date`,
    `Completion Date`,
    `Completion D Date`,
    `Standard Job Desc`,
    `Standard Job No`,
    `Originator Emp Id`,
    `Address`,
    `Town or City`,
    `Post Code`,
    `User Status - desc`,
    `District Meter Area`,
    `Revised Job Number`
  )
c <-
  read_excel(files[[3]], sheet = 2, col_types = "text") %>% select(
    `Work Order No`,
    `Work Order description`,
    `Work Request No`,
    `Creation Date`,
    `Completion Date`,
    `Completion D Date`,
    `Standard Job Desc`,
    `Standard Job No`,
    `Originator Emp Id`,
    `Address`,
    `Town or City`,
    `Post Code`,
    `User Status - desc`,
    `District Meter Area`,
    `Revised Job Number`
  )
d <-
  read_excel(files[[4]], sheet = 2, col_types = "text") %>% select(
    `Work Order No`,
    `Work Order description`,
    `Work Request No`,
    `Creation Date`,
    `Completion Date`,
    `Completion D Date`,
    `Standard Job Desc`,
    `Standard Job No`,
    `Originator Emp Id`,
    `Address`,
    `Town or City`,
    `Post Code`,
    `User Status - desc`,
    `District Meter Area`,
    `Revised Job Number`
  )
e <-
  read_excel(files[[5]], sheet = 2, col_types = "text") %>% select(
    `Work Order No`,
    `Work Order description`,
    `Work Request No`,
    `Creation Date`,
    `Completion Date`,
    `Completion D Date`,
    `Standard Job Desc`,
    `Standard Job No`,
    `Originator Emp Id`,
    `Address`,
    `Town or City`,
    `Post Code`,
    `User Status - desc`,
    `District Meter Area`,
    `Revised Job Number`
  )
f <-
  read_excel(files[[6]], sheet = 2, col_types = "text") %>% select(
    `Work Order No`,
    `Work Order description`,
    `Work Request No`,
    `Creation Date`,
    `Completion Date`,
    `Completion D Date`,
    `Standard Job Desc`,
    `Standard Job No`,
    `Originator Emp Id`,
    `Address`,
    `Town or City`,
    `Post Code`,
    `User Status - desc`,
    `District Meter Area`,
    `Revised Job Number`
  )
g <-
  read_excel(files[[7]], sheet = 2, col_types = "text") %>% select(
    `Work Order No`,
    `Work Order description`,
    `Work Request No`,
    `Creation Date`,
    `Completion Date`,
    `Completion D Date`,
    `Standard Job Desc`,
    `Standard Job No`,
    `Originator Emp Id`,
    `Address`,
    `Town or City`,
    `Post Code`,
    `User Status - desc`,
    `District Meter Area`,
    `Revised Job Number`
  )
h <-
  read_excel(files[[8]], sheet = 2, col_types = "text") %>% select(
    `Work Order No`,
    `Work Order description`,
    `Work Request No`,
    `Creation Date`,
    `Completion Date`,
    `Completion D Date`,
    `Standard Job Desc`,
    `Standard Job No`,
    `Originator Emp Id`,
    `Address`,
    `Town or City`,
    `Post Code`,
    `User Status - desc`,
    `District Meter Area`,
    `Revised Job Number`
  )
i <-
  read_excel(files[[9]], sheet = 2, col_types = "text") %>% select(
    `Work Order No`,
    `Work Order description`,
    `Work Request No`,
    `Creation Date`,
    `Completion Date`,
    `Completion D Date`,
    `Standard Job Desc`,
    `Standard Job No`,
    `Originator Emp Id`,
    `Address`,
    `Town or City`,
    `Post Code`,
    `User Status - desc`,
    `District Meter Area`,
    `Revised Job Number`
  )

# clean, transform data
j <- rbind.fill(a, b, c, d, e, f, g, h, i)
summary(j)
k <- j[!duplicated(j$`Work Order No`), ]

k$`User Status - desc` %>% unique()
f__k <-
  k %>% filter(
    `User Status - desc` != "NA" |
      `User Status - desc` != "CANCELLED" | !is.na(`User Status - desc`)
  )
unique(f__k$`User Status - desc`)
k.1 <- k %>% select(-c(`Work Order No`)) %>% distinct()
dim(k.1)

# write output as csv file
write.csv(
  f__k,
  "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/combined comms pipe data.csv"
)


# load data
ump <-
  read.csv(
    "I:\\Projects\\Client\\1892\\Analytics\\Proportion of UMP Meters Installed by Year.csv"
  )

# clean, transform data
ump1 <- ump %>% gather(Year1, Value, X2011:X2015, factor_key = FALSE)
ump2 <-
  ump1 %>%  filter(Value > 0.5) %>% select(DMA_Ref, Year1) %>% unique()
ump2$Year1 <-  substring(ump2$Year1, 2, 5)
ump2$Year1 <- as.numeric(ump2$Year1)

# compare the number of jobs in the Comms Pipe data in the year before the UMP work, to the number of jobs the year after

# load data
comm <-
  read.csv(
    "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/combined comms pipe filtered.csv"
  )

# clean, transform data
comm$Completion.D.Date <- dmy(comm$Completion.D.Date)
comm <-
  comm %>% filter(between(Completion.D.Date, ymd("2010-12-01"), ymd("2019-12-01")))
comm$CommsYear <- substring(comm$Completion.D.Date, 1, 4)
comm$CommsMonthYear <- substring(comm$Completion.D.Date, 1, 7)
comm2 <-
  comm %>% dplyr::select(
    Work.Order.No,
    Address,
    Post.Code,
    District.Meter.Area,
    CommsYear,
    Standard.Job.Desc
  )
comm2$CommsYear <- as.numeric(comm2$CommsYear)

# plot of change in comms pipe jobs before and after ump programme
comm_ump <- comm2 %>%
  left_join(ump2, by = c("District.Meter.Area" = "DMA_Ref")) %>%
  filter(!is.na(Year1)) %>%  filter(Year1 > 2011)
#comm_ump$date1 <- substring(comm_ump$date1, 6,9)
before <-
  comm_ump %>% group_by(District.Meter.Area)  %>%  filter(CommsYear == Year1 - 1) %>%
  dplyr::summarise(comm.pipes.jobs = n()) %>%  mutate(Category = "Before")
after <-
  comm_ump %>% group_by(District.Meter.Area) %>%  filter(CommsYear == Year1 + 1)  %>%
  dplyr::summarise(comm.pipes.jobs = n()) %>%  mutate(Category = "After")
overall <-  rbind(before, after)
overall2 <-
  spread(overall, key = Category, value = comm.pipes.jobs) %>%
  mutate(Before = replace_na(Before, 0)) %>%
  mutate(After = replace_na(After, 0)) %>%
  select(District.Meter.Area, Before, After)
overall$Category <- fct_relevel(overall$Category, "Before", "After")
overall %>%  group_by(Category) %>% dplyr::summarise(Total = sum(comm.pipes.jobs)) %>%
  ggplot(aes(Category, Total), position = "dodge") +
  geom_bar(aes(fill = Category), col = "black", stat = "identity") +
  #scale_x_continuous(name = "Day", breaks = c(1:10)) +
  #scale_y_continuous(name = "Total Consumption by Category /m3") +
  scale_fill_manual(values = SEAMScolours[c(1, 5)]) + gg +
  labs(title = "Change in comm Pipes Job in Years prior to and following UMP work")


# alc per year

bj_lde <- lde1
bj_lde$Year <- substring(bj_lde$Completion.D.Date, 1, 4)

bj_lde1 <-
  bj_lde %>% group_by(DMA.Code, Year) %>% dplyr::summarise(Total.ALC.Hours = sum(Actual.Labour.Hours))

# write output as csv file
write.csv(bj_lde1,
          "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/ALC Hours per year.csv")

# plot stacked bar chart of total leak size (NF) grouped by type of repair (Detect and Repair Mains/Comms, 4 in total)
comm2$`Post.Code` <- gsub("[[:blank:]]", "", comm2$`Post.Code`)
bj_comm2 <- comm2 %>%
  left_join(unique(comms10_16[, 3:4]), by = "Post.Code") %>%
  mutate(DMA.Code = ifelse(
    is.na(as.character(District.Meter.Area.x)),
    as.character(District.Meter.Area.y),
    as.character(District.Meter.Area.x)
  )) %>%
  dplyr::select(-c(District.Meter.Area.x, District.Meter.Area.y)) %>%
  unique()

bj_comm2 %>% filter(`Address` == "53 DUCK ST ABBOTTS ANN")
bj_comm2$row <- 1
cad <- read.csv("county and dma.csv")
bj_comm3H <- bj_comm2 %>%
  left_join(cad, by = "DMA.Code") %>%
  dplyr::group_by(County, Standard.Job.Desc, CommsYear) %>%
  dplyr::summarise(n = sum(row))

bj_comm3H <- bj_comm3H %>% spread(key = CommsYear, n)
bj_comm3H <-
  bj_comm3H[, c(
    "County",
    "Standard.Job.Desc",
    "2011",
    "2012",
    "2013",
    "2014",
    "2015",
    "2016",
    "2017",
    "2018"
  )]

# write output files as csv
setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 2")
write.csv(
  bj_comm3H %>% filter(County == "HAMPSHIRE") %>% ungroup() %>% select(-c(1)),
  file = "Hampshire - Comms Jobs per year.csv",
  row.names = FALSE
)
write.csv(
  bj_comm3H %>% filter(County == "ISLE OF WIGHT") %>% ungroup() %>% select(-c(1)),
  file = "IOW - Comms Jobs per year.csv",
  row.names = FALSE
)
write.csv(
  bj_comm3H %>% filter(County == "KENT") %>% ungroup() %>% select(-c(1)),
  file = "Kent - Comms Jobs per year.csv",
  row.names = FALSE
)
write.csv(
  bj_comm3H %>% filter(County == "SUSSEX") %>% ungroup() %>% select(-c(1)),
  file = "Sussex - Comms Jobs per year.csv",
  row.names = FALSE
)




# checking for duplicate jobs (comms and stop tap)

address1 <- comm2 %>%
  filter(grepl('STOP TAP|SUPPLY', Standard.Job.Desc)) %>%
  select(Address) %>% distinct()

address2 <- comm2 %>%
  filter(grepl('COMM', Standard.Job.Desc)) %>%
  select(Address) %>% distinct() %>% unlist() %>% as.character()

dim(address1)
dim(address2)

address3 <- address1 %>% anti_join(address2, by = "Address")
dim(address3)

dup <- address1 %>% filter(Address %in% address2)


dim(address1)
dim(address2)
dim(dup)



# HYPOTHESIS 7.4 ---------------------------------------

# load data
bj_nhh <-
  read_excel(path = "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/NHH_reads_since_April_2016.xlsx")

# clean, transform data
bj_nhh$`Meter Read Date` <-
  substring(bj_nhh$`Meter Read Date`, 1, 10)
bj_nhh$`Meter Read Date` <- ymd(bj_nhh$`Meter Read Date`)
bj_nhh2 <-
  bj_nhh %>%  filter(!is.na(`Meter Read Date`)) %>%  filter(!is.na(`Meter Read Value`))

# remove instances where reading = 0 & keep duplicate serial numbers
zeros <- bj_nhh2$`Meter Read Value` == 0
bj_nhh2 <- bj_nhh2[!zeros, ]
consumption_ <-
  bj_nhh2 %>% dplyr::select(`SPID`, `Meter Read Value`, `Meter Read Date`, `Post Code`) %>%
  group_by(`SPID`) %>%
  distinct(`Meter Read Date`, .keep_all = TRUE) %>%
  arrange(`Meter Read Date`)
consumption_$`Post Code` <-
  gsub("[[:blank:]]", "", consumption_$`Post Code`)
consumption.split <- split(consumption_, consumption_$`SPID`)

test.output <- NULL
output.final <- NULL
Calculate_Reading_Difference <-
  function(x) {
    # maximum reading and date - minimum reading and date
    for (j in 1:length(x)) {
      test <- data.table(x[[j]])
      test.output$Used <-
        max(test$`Meter Read Value`) - min(test$`Meter Read Value`)
      test.output$Time.diff <-
        max(test$`Meter Read Date`) - min(test$`Meter Read Date`)
      test.output$`SPID` <-
        test$`SPID`[1]
      test.output$`Post Code` <- test$`Post Code`[1]
      if (j == 1) {
        output.final <- test.output
      } else{
        output.final <- rbind(output.final, test.output)
      }
    }
    return(as.data.table(output.final))
  }

Calculate_Reading_Difference(consumption.split[1:9])

consumption.split.1 <-
  Calculate_Reading_Difference(consumption.split)
consumption.split.2 <-
  lapply(consumption.split.1, unlist) %>% as.data.table
str(consumption.split.2)
consumption.split.3 <- consumption.split.2 %>%
  left_join(unique(comms10_16[, 3:4]), by = c("Post Code" = "Post.Code")) %>%
  ungroup() %>%
  group_by(District.Meter.Area) %>%
  dplyr::summarise(Total.Used = sum(Used),
                   Total.Period = sum(Time.diff))

consumption.split.3b <- consumption.split.2 %>%
  left_join(unique(comms10_16[, 3:4]), by = c("Post Code" = "Post.Code")) %>%
  ungroup() %>%
  distinct(SPID, .keep_all = TRUE) %>% # removing multiple SPIDs
  #unique() %>%
  group_by(District.Meter.Area) %>%
  tally(name = "Number.of.NHH") %>%
  left_join(consumption.split.3, by = "District.Meter.Area") %>%
  filter(!is.na(District.Meter.Area) &
           District.Meter.Area != "") %>%
  mutate(Average_Daily_Usage = Total.Used / as.numeric(Total.Period)) %>%
  arrange(desc(Average_Daily_Usage))

# write the outputs as csv file
write.csv(
  consumption.split.3b,
  "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/NHH DMA Average Daily Consumption v2.csv",
  row.names = F
)
write.csv(
  unique(comms10_16[, 3:4]),
  "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/Postcodes and DMAs mapping.csv",
  row.names = F
)



# HYPOTHESIS 3.1 -----------------------------------------

# load data
dist <- read.csv("Distribution_of_leakage_across_length.csv")
data1 <- read.csv("DMA Leakage raw.csv")
discre <- read.csv("Estimated Consumption vs Nightflow.csv")

# use this function below
normalise <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

# clean, transform data
data1$Length <- normalise(data1$Length)
a1 <- data1 %>%
  mutate(Leakage.per.m = Leakage.m3.hr / Length) %>%
  arrange(desc(Leakage.per.m))

a2 <- data1 %>%
  filter(!is.na(Leakage.m3.hr), !is.na(Length)) %>%
  mutate(Leakage = Leakage.m3.hr / Length)

# plot

a2 %>%
  arrange(Leakage) %>%
  mutate(row = row_number(),
         group = (as.numeric(row) %/% (914 / 10)) + 1) %>%
  group_by(group) %>%
  summarise(Leakage = 100 * (sum(Leakage, na.rm = T) / (sum(a2$Leakage, na.rm = T)))) %>%
  ggplot(aes(x = factor(group), y = Leakage)) +
  geom_bar(stat = "identity", fill = SEAMScolours[1]) +
  scale_x_discrete(labels = lab) +
  gg +
  labs(x = "Percentile of DMAs ordered by leakage", y = "Proportion of total leakage (%)")


a3 <- as.data.frame(a2) %>%
  arrange(Leakage) %>%
  mutate(row = row_number(),
         group = (as.numeric(row) %/% (914 / 10)) + 1) %>%
  group_by(group) %>%  filter (group == "10")



# cluster a3 based on material type and add property discrepancy data

a3$id <- seq.int(nrow(a3))
discre2 <- as.data.table(discre[, c(1, 4)] %>%
                           inner_join(a3, by = c("DMA_Ref" = "DMA")))



set.seed(10)
clusters <- kmeans(as.matrix(na.omit(a3[, 7:13])), 4)
group.index.naming <-
  data.frame(
    group.index = c(2, 3, 1, 4),
    group.name = c("Mixed", "Largely PE", "Largely Iron/PE", "Largely Iron")
  )

clusters[["centers"]] %>%
  as.data.frame() %>%
  mutate(group.index = row_number()) %>%
  gather("Material", "Value", -group.index) %>%
  left_join(group.index.naming) %>%
  ggplot(aes(x = Material, y = Value, fill = Material)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap( ~ group.name) +
  scale_fill_manual(values = SEAMScolours) + gg +
  guides(fill = FALSE) +
  labs(y = "Proportion", title = "Clustered group composition")

grp <- as.data.table(x = clusters[["cluster"]])
grp2 <-
  grp %>% left_join(group.index.naming, by = c("V1" = "group.index"))
grp2$id <- seq.int(nrow(grp2))
grp2$V1 <- NULL
grp3 <- discre2 %>% left_join(grp2, by = "id")


# explore the data further using plots
ggplot(grp3, aes(property_discrepancy, Leakage)) + geom_point(aes(colour = group.name, size = Length)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg + scale_x_continuous(name = "Property discrepancy") +
  scale_y_continuous(name = "Leakage Change (2015 to 2018)") + scale_color_manual(values =   c("red3",
                                                                                               "orange2",                                                                                                                             "forestgreen",
                                                                                               "blue3",
                                                                                               "gray90")) +
  labs(title = "Impact of Mains 'patchwork quilt' Material Profiles on Leakage level") +
  geom_hline(yintercept = 100, colour = "darkred") + geom_vline(xintercept = 500, colour = "darkred")

ggplot(grp3, aes(Length, Leakage)) + geom_point(aes(colour = group.name)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") + gg + scale_x_continuous(name = "Normalised Mains Length") +
  scale_y_continuous(name = "Leakage Change (2015 to 2018)") + scale_color_manual(values =   c("red3",
                                                                                               "orange2",                                                                                                                             "forestgreen",
                                                                                               "blue3",
                                                                                               "gray90")) +
  labs(title = "DMA Leakage and Material Composition in Property discrepancy areas")






# Extras --------------------------------------

# load data
setwd("I:\\Projects\\Client\\1892\\Analytics")
data <- read_csv("Nightflow Mastersheet_sheet_6.csv")
category.sheet <- read_csv("DMA category analysis v4.csv")
replacement <- read_csv("Replacement Pipe DMA Summary.csv")
replacement.length.nightflow.change.summary <-
  read_csv("Replacement_Length_Nightflow_Change_Summary.csv")
gis <- st_read("gis.shp")

# set the colours to be used in plots
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

# setting ggplot
gg <- theme_bw() +
  theme(legend.position = "bottom")


# Pulling potentially useful factors
data1 <- data %>%
  select(
    County,
    Management.area = `Management Area`,
    DMA,
    Phase,
    Property.count = `Property Count`,
    Length = `Mains Length NEW`,
    Asbestos:Unknown,
    Length.after.2000 = `New Mains Installed after 2000 (m)`,
    Leakage.m3.hr = `Reported Leakage (m3/hr)`,
    march.april.leakage.18 = `March/April_18`,
    march.april.leakage.19 = `March/April_19`,
    DMA.desc = `DMA Description`
  )


data2 <- data1 %>%
  filter(!is.na(Leakage.m3.hr), !is.na(Length)) %>%
  mutate(Leakage = Leakage.m3.hr / Length)

# 10 percentile 40%
data3 <- data2 %>%
  arrange(Leakage) %>%
  mutate(row = row_number(),
         group = (as.numeric(row) %/% (924 / 10)) + 1) %>%
  group_by(group) %>%
  filter(group == 10)




# link ALC effort to the above

replacement %>% inner_join(data3, by = "DMA")
replacement2 <-
  replacement %>% mutate(difference = (Average_Nightflow_After - Average_Nightflow_Before))
replacement2 %>%
  arrange(difference) %>%
  mutate(row = row_number(),
         group = (as.numeric(row) %/% (109 / 10)) + 1) %>%
  group_by(group) %>% summarise(totals = sum(difference, na.rm = TRUE))

replacement2 <-
  replacement2 %>% inner_join(as.data.frame(gis[, "DMA_Ref"]), by = c("DMA" = "DMA_Ref"))
replacement2 <- st_as_sf(replacement2, crs = 27700)
tmap_mode("view")
tm_shape(replacement2) + tm_polygons(alpha = 0.75, col = "difference") + tm_basemap("Esri.WorldStreetMap")
head(leakage.detection.effort, 100)

l1 <-
  select(
    leakage.detection.effort[substr(as.character(Completion.D.Date),
                                    nchar(as.character(Completion.D.Date)) - 3,
                                    nchar(as.character(Completion.D.Date))) %in% c(2016, 2017)],
    County,
    Standard.Job.No,
    DMA.Code,
    Actual.Labour.Hours,
    Maintainance.Type,
    Completion.D.Date
  )


# analysing mixed

data1 %>%
  filter(!is.na(Asbestos)) %>%
  mutate(group.index = clusters$cluster,
         leakage.rate = Leakage.m3.hr / Length) %>%
  left_join(group.index.naming)



# combine all the data together

set.seed(27)
clusters1 <- kmeans(as.matrix(na.omit(dist[, 7:13])), 4)
group.index.naming1 <-
  data.frame(
    group.index = c(1, 2, 3, 4),
    group.name = c("Largely PE", "Largely Iron", "Largely PVC", "Mixed")
  )


clusters1[["centers"]] %>%
  as.data.frame() %>%
  mutate(group.index = row_number()) %>%
  gather("Material", "Value", -group.index) %>%
  left_join(group.index.naming1)



clusters1[["centers"]] %>%
  as.data.frame() %>%
  mutate(group.index = row_number()) %>%
  gather("Material", "Value", -group.index) %>%
  left_join(group.index.naming1) %>%
  ggplot(aes(x = Material, y = Value, fill = Material)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap( ~ group.name) +
  scale_fill_manual(values = SEAMScolours) + gg +
  guides(fill = FALSE) +
  labs(y = "Proportion", title = "Clustered group composition")

grp_ <- as.data.table(x = clusters1[["cluster"]])
grp_2 <-
  grp_ %>% left_join(group.index.naming1, by = c("V1" = "group.index"))
grp_2$id <- seq.int(nrow(grp_2))
grp_2$V1 <- NULL
grp_3 <- dist %>% left_join(grp_2, by = c("row" = "id"))

grp_4 <- grp_3 %>% left_join(discre, by = c("DMA" = "DMA_Ref"))
grp_4$row <- NULL
grp_4$id <- NULL

# write output as csv file
write.csv(grp_4, "DMA Material Leakage Property NF Data.csv")










# Extras 2 --------------------------

# load data
nhh <-
  fread("C:\\Users\\TOsosanya\\Downloads\\Household_meter_reads.txt",
        sep = ",")

# clean, transform data
summary(nhh)
nhh$`Reading Date` <- dmy_hms(nhh$`Reading Date`)



# Extras 3 ------------------------------
# validated NF to long

# load data
setwd("I:\\Projects\\Client\\1892\\Analytics")
vn.11 <-
  data.table(
    read.csv(
      "I:\\Projects\\Client\\1915\\Analytics\\NightFlow-Combined-v2.2.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )
dma.lookup <-
  data.table(read.csv("I:\\Projects\\Client\\1892\\Analytics\\DMA Lookup.csv"))
dma.classification <-
  data.table(read.csv(
    "I:\\Projects\\Client\\1892\\Analytics\\DMA category analysis v5.csv"
  ))
mlp <-
  data.table(
    read.csv(
      "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/Christina/DMA Material Leakage Property NF Data.csv"
    )
  )

# clean, transform data
vn.11 <- vn.11[, c(1:103)] %>% dplyr::rename("DMA.Code" = "DMA_Ref")
non_dup <- vn.11[!duplicated(vn.11$DMA.Code), ]

# load data
setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 1")
gis <- st_read("gis.shp")

# clean, transform data
gis <- gis[!duplicated(gis$DMA_Dsc), ]
gis <- st_as_sf(gis)
st_crs(gis) <- 27700
vn.11$DMA.Code <- as.character(vn.11$DMA.Code)
dim(gis)
dim(non_dup)

vn.2 <-
  inner_join(x = gis[, c(1, 2, 14)],
             y = non_dup,
             by = c("DMA_Ref" = "DMA.Code"))
#vn.2 <- st_as_sf(vn.2, crs = 27700)
tmap_mode("view")


vn.2 <- st_set_geometry(vn.2, NULL)
vn.3 <- vn.2 %>%
  mutate(Region = ifelse(Divisin == "H", "West",
                         (ifelse(
                           Divisin == "K", "East",
                           (ifelse(Divisin == "I", "West",
                                   (
                                     ifelse(Divisin == "S", "Central", "Other")
                                   )))
                         )))) %>%
  mutate(Divisin = NULL)

# convert to (long format) DMA MNF
vn.4 <- gather(vn.3, Date1, Value, Jan.11:Jun.19, factor_key = TRUE)
vn.4$Date1 <- as.yearmon(vn.4$Date1, format = "%b.%y")

# write to csv
write.csv(
  na.omit(vn.4[, c(1, 2, 7, 8, 9)]),
  "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/Validated Nightflow v5.csv",
  row.names = F
)



# write the data using st/ogr to view on qgis
unique(gis$DMA_Ref)
gis_ <- gis[, 1:2] %>% filter(!is.na(DMA_Ref))
st_write(
  na.omit(gis_),
  "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/DMA GIS v2.csv",
  layer_options = "GEOMETRY=AS_WKT"
)
writeOGR(gis_,
         dsn = ".",
         layer = "gis_",
         driver = "CSV")
gis_ <-
  st_read("C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/DMA GIS v2.csv")
gis_ <- st_as_sf(gis_, crs = 27700)

# plot on a map
tmap_mode("view")
tm_shape(gis_) + tm_polygons() + tm_basemap("Esri.WorldStreetMap")



# HYPOTHESIS 2.7  testing stuff -----------------------------------------

#UMP

# load data
setwd("C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/UMP")
files <-
  list.files(path = "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/UMP",
             pattern = "*.xlsx",
             full.names = T)
tbl <- sapply(files, read_excel)  %>%
  plyr::rbind.fill()
str(tbl)

# clean, transform data
tbl$Postcode <- gsub("[[:blank:]]", "", tbl$Postcode)
tbl2 <- tbl %>%
  left_join(unique(comms10_16[, 3:4]), by = c("Postcode" = "Post.Code")) %>%
  dplyr::rename("DMA Code" = "District.Meter.Area")
tbl2$Date1 <- substring(tbl2$`Actual Completion Date`, 1, 7)
tbl2 <- tbl2 %>%
  filter(`Ellipse User status (Long description)` == "WORK COMPLETE") %>%
  group_by(Date1, `DMA Code`) %>%
  tally(name = "Total.UMP")

tbl2$Date1 <- paste(tbl2$Date1, "-01",  sep = "")
tbl2$Date1  <- ymd(tbl2$Date1)
UMP0 <-
  tbl2 %>% filter(!is.na(Date1) &
                    Total.UMP > 50) %>% arrange(Date1) %>%
  as.data.frame()


# sort the dma postcode thing..
# calculate the proportion of ump installation per dma per month i.e. dma, month, amount installed,
# total installed = dma, month, proportion ; month, sum proportion

# load data
comms10_16 <-
  read.csv("C://Users//TOsosanya//Desktop//Southern Water//Phase 1//comms10_16.csv")

# clean, transform data
tbl_1 <-
  tbl %>% #mutate(`District Meter Area`=as.factor(`District Meter Area`)) %>%
  left_join(unique(comms10_16[, 3:4]), by = c("Postcode" = "Post.Code")) #%>%
#mutate(DMA_Ref = ifelse(is.na(District.Meter.Area),District.Meter.Area,`District Meter Area`))
summary(tbl_1)
ump_installed_year <- tbl_1 %>%
  mutate(Date1 = substring(tbl_1$`Actual Completion Date`, 1, 4)) %>%
  filter(`Ellipse User status (Long description)` == "WORK COMPLETE") %>%
  dplyr::rename(DMA_Ref = District.Meter.Area) %>%
  group_by(Date1, DMA_Ref) %>%
  tally(name = "Total.UMP") %>%
  ungroup() %>%
  dplyr::select(DMA_Ref, Date1, Total.UMP) %>%
  spread(key = Date1, Total.UMP) %>%
  filter(DMA_Ref != "") %>%
  dplyr::select(-c(`2019`))

# write the output to csv
write.csv(
  ump_installed_year,
  "C://Users//TOsosanya//Desktop//Southern Water//Phase 2//UMP Meters Installed per Year v2.csv",
  row.names = F
)

# do some more analyses
tbl_2 <- tbl_1 %>%
  mutate(Date1 = substring(tbl_1$`Actual Completion Date`, 1, 7)) %>%
  filter(`Ellipse User status (Long description)` == "WORK COMPLETE") %>%
  dplyr::rename(DMA_Ref = District.Meter.Area) %>%
  group_by(Date1, DMA_Ref) %>%
  tally(name = "Total.UMP") %>%
  ungroup() %>%
  group_by(DMA_Ref) %>%
  dplyr::mutate(sum.ump = sum(Total.UMP)) %>%
  group_by(Date1, DMA_Ref) %>%
  dplyr::summarise(Percentage = (Total.UMP / sum.ump) * 100) %>%
  filter(!is.na(DMA_Ref)) %>%
  ungroup() %>%
  group_by(DMA_Ref, Date1) %>%
  dplyr::summarise(Total.UMP = mean(Percentage, na.rm = T))

tbl_2$Date1 <- paste(tbl_2$Date1, "-01",  sep = "")
tbl_2$Date1  <- ymd(tbl_2$Date1)
UMP <- tbl_2 %>% filter(!is.na(Date1)) %>% arrange(Date1) %>%
  filter(between(Date1, ymd("2011-01-01"), ymd("2019-01-01"))) %>%
  as.data.frame()

UMP0_ <-
  UMP %>% dplyr::rename(Total.UMP.Proportion = Total.UMP) %>%
  left_join(UMP0, by = "Date1")


# comms - jobs promoted

# load data
comm <-
  read.csv(
    "C:/Users/TOsosanya/Desktop/Southern Water/Phase 2/combined comms pipe filtered.csv"
  )

# clean, transform data
comm$Completion.D.Date <- dmy(comm$Completion.D.Date)
comm_2 <-
  comm %>% filter(between(Completion.D.Date, ymd("2010-12-01"), ymd("2019-12-01")))
comm_2$CommsMonth <- substring(comm_2$Completion.D.Date, 1, 7)
comm_2 <-
  comm_2 %>% dplyr::select(
    Work.Order.No,
    Address,
    Post.Code,
    District.Meter.Area,
    CommsMonth,
    Standard.Job.Desc
  )
comm_2$Job.promted <-
  revalue(
    comm_2$Standard.Job.Desc,
    c(
      "COMM PIPE-REPAIR LEAK 15-32MM" = "Comms Pipe or stop tap",
      "WL COMM PIPE REPLACE" = "Comms Pipe or stop tap",
      "WN COMM PIPE DEFECT" = "Comms Pipe or stop tap",
      "WN COMM PIPE REPAIR" = "Comms Pipe or stop tap",
      "RECH: CUSTOMER SUPPLY PIPE RENEW" = "Customer supply pipe",
      "RECH: CUSTOMER SUPPLY PIPE REPAIR" = "Customer supply pipe",
      "WL STOP TAP REPLACE" = "Comms Pipe or stop tap",
      "WL STOP TAP REPAIR" = "Comms Pipe or stop tap",
      "WN STOP TAP REPLACE" = "Comms Pipe or stop tap",
      "WN STOP TAP REPAIR" = "Comms Pipe or stop tap",
      "WN MAIN REPLACE" = "Mains",
      "WN MAIN REPAIR" = "Mains",
      "WN MAINS FITTING REPLACE" = "Mains",
      "WN MAINS FITTING REPAIR" = "Mains",
      "WL MAINS FITTING REPAIR" = "Mains",
      "WL MAINS FITTING REPLACE" = "Mains",
      "WL MAIN REPLACE" = "Mains",
      "WL MAIN REPAIR" = "Mains"
    )
  )
comm_3 <- comm_2 %>%
  group_by(District.Meter.Area, CommsMonth, Job.promted) %>%
  tally(name = "Jobs") %>%
  mutate(Jobs = replace_na(Jobs, 0)) %>%
  spread(key = Job.promted, Jobs) %>%
  dplyr::select(-c(`MAIN - CUT AND CAP`))
comm_3$Date1 <-
  paste(comm_3$CommsMonth, "-01")
comm_3$CommsMonth <- NULL
comm_3$Date1 <- ymd(comm_3$Date1)
comms_ump <- comm_3 %>%
  left_join(UMP0_, by = "Date1") %>%
  filter(between(Date1, ymd("2011-01-01"), ymd("2019-01-01"))) %>%
  mutate(`Comms Pipe or stop tap` = replace_na(`Comms Pipe or stop tap`, 0)) %>%
  mutate(Mains = replace_na(Mains, 0)) %>%
  mutate(`Customer supply pipe` = replace_na(`Customer supply pipe`, 0))


# comms jobs
comm_2$Post.Code <- gsub("[[:blank:]]", "", comm_2$Post.Code)
comm_jobs <- comm_2 %>%
  filter(!is.na(District.Meter.Area)) %>%
  group_by(District.Meter.Area, CommsMonth) %>%
  tally(name = "Jobs") %>%
  mutate(Jobs = replace_na(Jobs, 0))

comm_jobs2 <- comm_2 %>%
  filter(is.na(District.Meter.Area)) %>%
  left_join(unique(comms10_16[, 3:4]), by = c("Post.Code" = "Post.Code")) %>%
  dplyr::rename(District.Meter.Area = District.Meter.Area.y) %>%
  group_by(District.Meter.Area, CommsMonth) %>%
  tally(name = "Jobs") %>%
  mutate(Jobs = replace_na(Jobs, 0))
comm_jobs <-  rbind(comm_jobs, comm_jobs2) %>% unique() %>%
  dplyr::rename(DMA_Ref = District.Meter.Area) %>% na.omit

comm_jobs$CommsMonth <- paste(comm_jobs$CommsMonth, "-01", sep = "")
comm_jobs$CommsMonth  <- ymd(comm_jobs$CommsMonth)


# alc
alc <-
  lde1 %>% mutate(Date1 = substring(lde1$Completion.D.Date, 1, 7)) %>%
  dplyr::rename(DMA_Ref = DMA.Code) %>%
  dplyr::group_by(DMA_Ref, Date1) %>% dplyr::summarise(ALC.Hours = sum(Actual.Labour.Hours, na.rm = TRUE)) %>%
  na.omit()

alc$Date1 <- paste(alc$Date1, "-01", sep = "")
alc$Date1  <- ymd(alc$Date1)
ALC <- alc %>% filter(!is.na(Date1)) %>% arrange(Date1) %>%
  as.data.frame()


# Mains Replacement

# load data
setwd("I://Projects//Client//1892//Analytics")
new.mains <-
  data.table(read.csv("New_Mains_DATE_LAID_1999_DMAv2.csv"))

# New Growth
new.mains.filtered <-
  new.mains[Replace == 0][substr(DATE_LAID, nchar(as.character(DATE_LAID)) - 12, nchar(as.character(DATE_LAID))
                                 - 9) %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)][DMA_Ref != " "]
new.mains.filtered$Month <-
  paste0(
    substr(
      new.mains.filtered$DATE_LAID,
      nchar(as.character(new.mains.filtered$DATE_LAID)) - 12,
      nchar(as.character(new.mains.filtered$DATE_LAID)) - 9
    ),
    "-",
    substr(
      new.mains.filtered$DATE_LAID,
      nchar(as.character(new.mains.filtered$DATE_LAID)) - 15,
      nchar(as.character(new.mains.filtered$DATE_LAID)) - 14
    )
  )

# sum of pipe length by DMA
new.mains.filtered.by.dma <-
  new.mains.filtered[, .(Total_DMA_Length = sum(PIPE_LENGT)), by = .(DMA_Ref)]
new.mains.filtered.by.dma.by.month <-
  new.mains.filtered[, .(Length = sum(PIPE_LENGT)), by = .(DMA_Ref, Month)]

setkey(new.mains.filtered.by.dma, DMA_Ref)
setkey(new.mains.filtered.by.dma.by.month, DMA_Ref)
new.mains.filtered.by.dma.by.month <-
  new.mains.filtered.by.dma[new.mains.filtered.by.dma.by.month, ][, .(DMA_Ref, Month, Length, Total_DMA_Length)]
setkey(new.mains.filtered.by.dma.by.month, DMA_Ref, Month)

names(new.mains.filtered.by.dma.by.month) <-
  paste("new.", names(new.mains.filtered.by.dma.by.month), sep = "")
New_Length <- new.mains.filtered.by.dma.by.month %>%
  dplyr::group_by(new.DMA_Ref, new.Month) %>% dplyr::summarise(Total.newlength = sum(new.Length, na.rm = TRUE))
#new.Total_DMA_Length = sum(new.Total_DMA_Length,na.rm = TRUE))




# Replaced Pipes
replaced.mains.filtered <-
  new.mains[Replace > 0][substr(DATE_LAID, nchar(as.character(DATE_LAID)) - 12, nchar(as.character(DATE_LAID))
                                - 9) %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)][DMA_Ref != " "]
replaced.mains.filtered$Month <-
  paste0(
    substr(
      replaced.mains.filtered$DATE_LAID,
      nchar(as.character(replaced.mains.filtered$DATE_LAID)) - 12,
      nchar(as.character(replaced.mains.filtered$DATE_LAID)) - 9
    ),
    "-",
    substr(
      replaced.mains.filtered$DATE_LAID,
      nchar(as.character(replaced.mains.filtered$DATE_LAID)) - 15,
      nchar(as.character(replaced.mains.filtered$DATE_LAID)) - 14
    )
  )

replaced.mains.filtered.by.dma <-
  replaced.mains.filtered[, .(Total_DMA_Length = sum(PIPE_LENGT)), by = .(DMA_Ref)]
replaced.mains.filtered.by.dma.by.month <-
  replaced.mains.filtered[, .(Length = sum(PIPE_LENGT)), by = .(DMA_Ref, Month)]

setkey(replaced.mains.filtered.by.dma, DMA_Ref)
setkey(replaced.mains.filtered.by.dma.by.month, DMA_Ref)
replaced.mains.filtered.by.dma.by.month <-
  replaced.mains.filtered.by.dma[replaced.mains.filtered.by.dma.by.month, ][, .(DMA_Ref, Month, Length, Total_DMA_Length)]
setkey(replaced.mains.filtered.by.dma.by.month, DMA_Ref, Month)

replaced.mains.filtered.by.dma
replaced.mains.filtered.by.dma.by.month
names(replaced.mains.filtered.by.dma.by.month) <-
  paste("replaced.",
        names(replaced.mains.filtered.by.dma.by.month),
        sep = "")
Replaced_Length <- replaced.mains.filtered.by.dma.by.month %>%
  dplyr::group_by(replaced.DMA_Ref, replaced.Month) %>% dplyr::summarise(Total.replacedlength = sum(replaced.Length, na.rm = TRUE))
#replaced.Total_DMA_Length = sum(replaced.Total_DMA_Length,na.rm = TRUE))


Mains.Replacement <-
  Replaced_Length %>% full_join(New_Length,
                                by = c("replaced.Month" = "new.Month", "replaced.DMA_Ref" = "new.DMA_Ref"))
Mains.Replacement$replaced.Month <-
  paste(Mains.Replacement$replaced.Month, "-01", sep = "")
Mains.Replacement$replaced.Month  <-
  ymd(Mains.Replacement$replaced.Month)
Mains.Replacement.New.Growth <- Mains.Replacement %>%
  #filter(!is.na(replaced.Month)) %>%
  dplyr::rename(Date1 = replaced.Month) %>%
  dplyr::rename(Mains.Replacement = Total.replacedlength) %>%
  dplyr::rename(New.Growth = Total.newlength) %>%
  dplyr::rename(DMA_Ref = replaced.DMA_Ref) %>%
  arrange(Date1) %>%
  as.data.frame() %>%
  unique()


plot(Mains.Replacement$Date1,
     Mains.Replacement$Total.replacedlength)
plot(Mains.Replacement$Date1, Mains.Replacement$Total.newlength)


# Monthly temperature

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

# clean, transform data
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
temp <- gather(both_1, Date1, Value, X2019:X2015, factor_key = FALSE)
temp$Date1 <- substring(temp$Date1, 2, 5)

temp$Date1 <- paste(substring(temp$X, 4, 6), sep = ".", temp$Date1)
temp$X <- NULL
temp3 <-
  temp %>% ungroup() %>% #mutate(MonthYear1 = as.character(MonthYear1)) %>%
  dplyr::group_by(ID, Date1) %>%
  dplyr::summarise(Average.Temperature = mean(Value, na.rm = T))
temp3$Date1 <- paste("01.", temp3$Date1, sep = "")
temp3$Date1  <- dmy(temp3$Date1)

# load data
vn.11 <-
  data.table(
    read.csv(
      "I:\\Projects\\Client\\1915\\Analytics\\NightFlow-Combined-v2.2.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )
gis <-
  st_read("C:/Users/TOsosanya/Desktop/Southern Water/Phase 1/gis.shp")

# clean, transform data
non_dup <- vn.11[!duplicated(vn.11$DMA_Ref), ]
gis <- gis[!duplicated(gis$DMA_Dsc), ]
gis <- st_as_sf(gis)
st_crs(gis) <- 27700
vn.11$DMA_Ref <- as.character(vn.11$DMA_Ref)
vn.2 <- inner_join(x = gis[, c(1, 2, 14)],
                   y = non_dup,
                   by = c("DMA_Ref"))
vn.2 <- st_set_geometry(vn.2, NULL)
vn.3 <- vn.2 %>%
  mutate(Region = ifelse(Divisin == "H", "West",
                         (ifelse(
                           Divisin == "K", "East",
                           (ifelse(Divisin == "I", "West",
                                   (
                                     ifelse(Divisin == "S", "Central", "Other")
                                   )))
                         )))) %>%
  mutate(Divisin = NULL)
vn.3[, c(1, 109)]$Region <- str_to_lower(vn.3[, c(1, 109)]$Region)
Temperature <- temp3 %>% arrange(Date1) %>%
  as.data.frame() %>%
  left_join(vn.3[, c(1, 109)], by = c("ID" = "Region")) %>%
  unique()


# Monthly Nightflow

# load data
nn <-
  data.table(
    read.csv(
      "I:\\Projects\\Client\\1915\\Analytics\\NightFlow-Combined-v2.2.csv",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  )

# clean, transform data
nn.1 <-
  gather(nn, Date1, `MNF Value`, Jan.11:Oct.19, factor_key = TRUE)
nn.1$Date1 <- as.yearmon(nn.1$Date1, format = "%b.%y")
nn.1$Date1 <- paste("01 ", nn.1$Date1, sep = "")
nn.1$Date1  <- dmy(nn.1$Date1)

Leakage <- nn.1 %>% ungroup()




# bring all together
all.together <- Mains.Replacement.New.Growth %>%
  full_join(ALC, by = c("Date1", "DMA_Ref")) %>%
  full_join(UMP0, by = c("Date1", "DMA_Ref" = "DMA Code")) %>%
  full_join(comm_jobs, by = c("Date1" = "CommsMonth", "DMA_Ref")) %>%
  full_join(Leakage, by = c("Date1", "DMA_Ref")) %>%
  full_join(Temperature, by = c("Date1", "DMA_Ref")) %>%
  dplyr::select(-(ID)) %>%
  arrange(Date1)
#all.together$Date1 <- substring(all.together$Date1,1,7)
all.together$Date1 <-
  as.yearmon(all.together$Date1, format = "%y/%m")
Timestep <- all.together %>% dplyr::select(Date1) %>% unique()
Timestep$Timestep <- seq.int(nrow(Timestep))
all.together <- all.together %>% left_join(Timestep, by = "Date1")


# write output
write.csv(
  all.together,
  "C:\\Users\\TOsosanya\\Desktop\\Southern Water\\Phase 2\\Monthly measure values for dashboard v2.csv",
  row.names = F
)
