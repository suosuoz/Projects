#Matthew Matsuo
#Code for Politics + R0/Stay At Home

setwd("C:/Users/Matt Matsuo/Google Drive/a Stat 141SL/Final Project/dat")

#Import Libraries
library(readr)
library(dplyr)
library(states)
library(lubridate)
library(ggplot2)
library(tidyr)
library(lubridate)
library(data.table)
library(states)
library(ggalt)


#Import Datasets
dat <- read_csv("Hospitalization_all_locs.csv")
dat_dates <- read_csv("Summary_stats_all_locs.csv")
r0 <- read.csv("rt.csv")
pol <- readxl::read_xlsx("political.xlsx")

#Filter US States
us <- dat[dat$location_name %in% state.name,]
dat_dates2 <- dat_dates[dat_dates$location_name %in% state.name,]
us$region <- state.abb[match(us$location_name,state.name)]


#Main Dataset Cleaning
combined <- inner_join(us,r0, by=c("date","region"))
combined2 <- inner_join(dat_dates2,pol, by="location_name")

combined <- combined %>% 
  select(-region,-state,-V1)

combined2 <- combined2 %>% 
  mutate(poliSum = `2016 Presidential Election`+Governor+`State Senate`+`State House`+`Senior Senator`+`Junior Senator`+`House of Representatives`+`Partisan split (as of 2017)`)

combined2 <- combined2 %>% 
  mutate(PoliLean = ifelse(poliSum == 0 | poliSum == 1,"Democratic",ifelse(poliSum == 2 | poliSum == 3,"Democratic Leaning",ifelse(poliSum == 4,"No Lean",ifelse(poliSum == 5 | poliSum == 6,"Republican Leaning","Republican")))))

combined2$Population <- c(4903185	,                          731545	,                          7278717	,                          3017804	,
                          39512223	,                          5758736	,                          3565287	,                          973764	,
                          21477737	,                          10617423	,                          1415872	,                          1787065	,
                          12671821	,                          6732219	,                          3155070	,                          2913314	,
                          4467673	,                          4648794	,                          1344212	,                          6045680	,
                          6892503	,                          9986857	,                          5639632	,                          2976149	,
                          6137428	,                          1068778	,                          1934408	,                          3080156	,
                          1359711	,                          8882190	,                          2096829	,                          19453561	,
                          10488084	,                          762062	,                          11689100	,                          3956971	,
                          4217737	,                          12801989	,                          1059361	,                          5148714	,
                          884659	,                          6829174	,                          28995881	,                          3205958	,
                          623989	,                          8535519	,                          7614893	,                          1792147	,
                          5822434	,                          578759)

for(i in 1:nrow(combined)){
  x1 <- combined[i, "location_name"]
  x1 <- as.character(x1)
  x2 <- which(combined2$location_name == x1)
  combined$PoliLean[i] <- as.character(combined2[x2,38])
  combined$Population[i] <- as.character(combined2[x2,39])
  combined$stay_home_start_date[i] <- as.character(combined2[x2,19])
  combined$any_gathering_restrict_start_date[i] <- as.character(combined2[x2,23])
}

combined$Population <- as.numeric(combined$Population)

combined3 <- combined2

for(i in 1:nrow(combined3)) {
  if(is.na(combined3$stay_home_start_date[i])){
    combined3$stay_home_start_date[i] <- combined3$any_gathering_restrict_start_date[i]
  }
}





#1 Exploratory Visualizations
myColors <- c("#4c34eb", "#34a5eb" , "#000000","#f54263","#ff0000")
ggplot(combined2, aes(x=peak_icu_bed_day_mean, y=stay_home_start_date, col=as.factor(PoliLean), size=4)) +
  geom_point() +
  scale_color_manual(values=myColors) +
  scale_x_discrete("Projected Peak ICU Bed Date", breaks=1)

ggplot(combined2, aes(x=peak_icu_bed_day_mean, y=any_gathering_restrict_start_date, col=as.factor(PoliLean), size=4)) +
  geom_point() +
  scale_color_manual(values=myColors) +
  scale_x_discrete("Projected Peak ICU Bed Date", breaks=1)

ggplot(combined2, aes(x=any_gathering_restrict_start_date, y=stay_home_start_date, col=as.factor(PoliLean), size=4)) +
  geom_point() +
  scale_color_manual(values=myColors)

ggplot(combined2) +
  geom_point(aes(x=any_gathering_restrict_start_date, y=location_name, col=as.factor(PoliLean), size=2)) +
  geom_point(aes(x=stay_home_start_date, y=location_name, col=as.factor(PoliLean), size=2)) +
  geom_point(aes(x=`all_non-ess_business_start_date`, y=location_name, col=as.factor(PoliLean), size=2)) +
  geom_line(aes(x=any_gathering_restrict_start_date, y=stay_home_start_date, group=location_name)) +
  scale_color_manual(values=myColors)

ggplot(combined, aes(x=date,y=allbed_mean,group=location_name,col=as.factor(PoliLean))) +
  geom_line()

combinedx <- combined %>% 
  mutate(allbedpercap = allbed_mean / Population)

ggplot(combinedx, aes(x=date,y=allbedpercap,group=location_name,col=as.factor(PoliLean))) +
  geom_line()





#2
##Dumbbell Plot
combineddb <- combined3

#Remove North Dakota, which doesn't have a stay home start date
combineddb <- combineddb[-34,]

ggplot(combineddb, aes(x=any_gathering_restrict_start_date,y=reorder(location_name, desc(any_gathering_restrict_start_date)), group=location_name, col=as.factor(PoliLean))) +
  geom_point() +
  labs(col = "Political Leaning",
       x=NULL, 
       y=NULL, 
       title="Mobility Restrictions by State") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank()) +
  scale_color_manual(values=myColors)

ggplot(combineddb, aes(x=stay_home_start_date,xend=any_gathering_restrict_start_date, y=reorder(location_name, desc(any_gathering_restrict_start_date)), group=location_name, col=as.factor(PoliLean))) +
  geom_dumbbell(size=0.75) + 
  labs(col = "Political Leaning",
       x=NULL, 
       y=NULL, 
       title="Mobility Restrictions by State",
       subtitle = "Left Point: Gathering Restriction Date     Right Point: Stay Home Start Date") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank()) +
  scale_color_manual(values=myColors)


#Simplify Political Affiliation
combined2 <- combined2 %>% 
  mutate(PoliLean2 = ifelse(PoliLean == "Democratic","Democratic",ifelse(PoliLean == "Democratic Leaning","Democratic",ifelse(PoliLean == "No Lean", "Democratic",ifelse(PoliLean == "Republican Leaning","Republican","Republican")))))


#T Test to see if there was a political difference between how quickly states reacted (min of Stay home/gathering restriction) Date from 3/11
daystayhome <- as.numeric(mdy(combined2$stay_home_start_date))-18332
dayanygath <- as.numeric(mdy(combined2$any_gathering_restrict_start_date))-18332
daystot <- numeric()

#rm north dakota
daystayhome <- daystayhome[-34]
dayanygath <- dayanygath[-34]
combinedno <- combined2[-34,]

#find earliest date of the two
for(i in 1:49){
  aa <- daystayhome[i]
  bb <- dayanygath[i]
  if(is.na(aa)){
    aa <- 10000
  }
  if(is.na(bb)){
    bb <- 10000
  }
  if(aa <= bb){
    daystot[i] <- aa
  } else{
    daystot[i] <- bb
  }
}

combinedno <- cbind(combinedno, daystot)

summary(combinedno %>% filter(PoliLean2 == "Democratic") %>% .$daystot)
summary(combinedno %>% filter(PoliLean2 == "Republican") %>% .$daystot)


#Use Nonparametric b/c data does not seem normal
ggplot(combinedno, aes(daystot)) +
  geom_histogram(stat = "count") +
  labs(title="Frequency Histogram",x="Days since 3/11/20")

t.test(daystayhome ~ PoliLean2, data=combinedno)
wilcox.test(daystayhome ~ PoliLean2, data=combinedno, correct=FALSE)






#3
#14 Days from the (stay home start date/any gathering restriction start date if no stay home date)
cc <- combined %>% 
  select(location_name,date,meanR0,medianR0, new_cases, PoliLean,Population,stay_home_start_date,any_gathering_restrict_start_date)

#Reformat Dates
cc$stay_home_start_date <- mdy(cc$stay_home_start_date)
cc$any_gathering_restrict_start_date <- mdy(cc$any_gathering_restrict_start_date)
cc$date2 <- mdy(cc$date)

#If there is no stay home date, make it gathering restrict date
for(i in 1:nrow(cc)){
  if(is.na(cc$stay_home_start_date[i])){
    cc$stay_home_start_date[i] <- cc$any_gathering_restrict_start_date[i]
  }
}

#Add 14 day lag
for(i in 1:nrow(cc)){
  cc$stay_home_start_date_lag[i] <- cc$stay_home_start_date[i] + days(14)
}
cc$stay_home_start_date_lag <- as_date(cc$stay_home_start_date_lag)


#Find the rows that match the stay home date, stay home lag, combine into cc2
cee2 <- which(cc$stay_home_start_date == cc$date2)
cee <- which(cc$stay_home_start_date_lag == cc$date2)

states1 <- c("California","Illinois","Louisiana","New Jersey","New York","Ohio","Oregon","Washington","Connecticut","Iowa","Kentucky","Massachusetts","Nebraska","New Mexico","Utah","Wyoming") 


r3 <- numeric()
for(i in 1:length(states1)){
  a <- which(cc$location_name == states1[i])[1]
  r3 <- c(r3, a)
}

cee2 <- c(cee2, r3)
cee2 <- sort(cee2)
cee3 <- c(cee, cee2)
cee3 <- sort(cee3)
cc2 <- cc[cee3,]
cc2$last <- rep(c("A","B"),49) #


#Update cc2 with dates 2 weeks later if we are missing some date information
for(i in 1:length(states1)){
  cc2 <- cc2[-which(cc2$location_name == states1[i])[2],]
  cc2$stay_home_start_date_lag[which(cc2$location_name == states1[i])] <- cc2$date2[which(cc2$location_name == states1[i])] + days(14)
  y <- which(cc$location_name == states1[i] & cc$date2 == cc2$stay_home_start_date_lag[which(cc2$location_name == states1[i])])
  y2 <- cc[y,]
  y2$last <- "B" 
  cc2 <- rbind(cc2,y2)
}

cc2 <- cc2 %>% 
  arrange(location_name, last)

#Update stay home start date
for(i in 1:length(states1)){
  ind <- which(states1[i] == cc2$location_name)
  date1 <- cc2[ind[1],10]
  cc2$stay_home_start_date[ind[1]] <- date1[[1]]
  cc2$stay_home_start_date[ind[2]] <- date1[[1]]
}

  
#Find the difference in R0 values between the earliest day and the lag period
gg1 <- cc2 %>% 
  group_by(location_name) %>% 
  mutate(diff = meanR0[last == "A"] - meanR0) %>% 
  filter(last == "B")


#Graph of 14 Days After
ggplot(gg1, aes(x=diff, y=stay_home_start_date, col=as.factor(PoliLean))) +
  geom_point() +
  scale_y_date(breaks = as.Date(c("2020-03-24","2020-04-01","2020-04-07"))) +
  xlim(0,.40) +
  scale_color_manual(values=myColors) +
  labs(col = "Political Leaning",
       x="Difference in R0", 
       y="Adj Earliest Action Date", 
       title="R0 Changes from Earliest Date to 2 Weeks Later") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())

#T Test by Governor
z <- combined2$PoliLean2[-which(combined2$location_name == "North Dakota")]
gg1$PoliLean2 <- rep("y",49)
for(i in 1:49){
  gg1$PoliLean2[i] <- z[i]
}

summary(gg1 %>% filter(PoliLean2 == "Democratic") %>% .$diff)
summary(gg1 %>% filter(PoliLean2 == "Republican") %>% .$diff)

wilcox.test(diff ~ PoliLean2, data=gg1)





#28 Days After - Only Run once 14 Days is finished
#4 28 Days from the (stay home start date/any gathering restriction start date if no stay home date)
cc <- combined %>% 
  select(location_name,date,meanR0,medianR0, new_cases, PoliLean,Population,stay_home_start_date,any_gathering_restrict_start_date)

#Reformat Dates
cc$stay_home_start_date <- mdy(cc$stay_home_start_date)
cc$any_gathering_restrict_start_date <- mdy(cc$any_gathering_restrict_start_date)
cc$date2 <- mdy(cc$date)

#If there is no stay home date, make it gathering restrict date
for(i in 1:nrow(cc)){
  if(is.na(cc$stay_home_start_date[i])){
    cc$stay_home_start_date[i] <- cc$any_gathering_restrict_start_date[i]
  }
}

#Add 28 day lag
for(i in 1:nrow(cc)){
  cc$stay_home_start_date_lag[i] <- cc$stay_home_start_date[i] + days(28)
}
cc$stay_home_start_date_lag <- as_date(cc$stay_home_start_date_lag)


#Find the rows that match the stay home date, stay home lag, combine into cc2
cee2 <- which(cc$stay_home_start_date == cc$date2)
cee <- which(cc$stay_home_start_date_lag == cc$date2)

states1 <- c("California","Illinois","Louisiana","New Jersey","New York","Ohio","Oregon","Washington","Connecticut","Iowa","Kentucky","Massachusetts","Nebraska","New Mexico","Utah","Wyoming") 


r3 <- numeric()
for(i in 1:length(states1)){
  a <- which(cc$location_name == states1[i])[1]
  r3 <- c(r3, a)
}

cee2 <- c(cee2, r3)
cee2 <- sort(cee2)
cee3 <- c(cee, cee2)
cee3 <- sort(cee3)
cc2 <- cc[cee3,]
cc2$last <- rep(c("A","B"),49) #


#Update cc2 with dates 2 weeks later if we are missing some date information
for(i in 1:length(states1)){
  cc2 <- cc2[-which(cc2$location_name == states1[i])[2],]
  
  
  cc2$stay_home_start_date_lag[which(cc2$location_name == states1[i])] <- cc2$date2[which(cc2$location_name == states1[i])] + days(28)
  
  
  y <- which(cc$location_name == states1[i] & cc$date2 == cc2$stay_home_start_date_lag[which(cc2$location_name == states1[i])])
  y2 <- cc[y,]
  y2$last <- "B" 
  cc2 <- rbind(cc2,y2)
}

cc2 <- cc2 %>% 
  arrange(location_name, last)

#Update stay home start date
for(i in 1:length(states1)){
  ind <- which(states1[i] == cc2$location_name)
  date1 <- cc2[ind[1],10]
  cc2$stay_home_start_date[ind[1]] <- date1[[1]]
  cc2$stay_home_start_date[ind[2]] <- date1[[1]]
}


#Find the difference in R0 values between the earliest day and the lag period
gg1 <- cc2 %>% 
  group_by(location_name) %>% 
  mutate(diff = meanR0[last == "A"] - meanR0) %>% 
  filter(last == "B")



#Graph of 28 Days After
ggplot(gg1, aes(x=diff, y=stay_home_start_date, col=as.factor(PoliLean))) +
  geom_point() +
  scale_y_date(breaks = as.Date(c("2020-03-24","2020-04-01","2020-04-07","2020-04-14","2020-04-28"))) +
  scale_color_manual(values=myColors) +
  labs(col = "Political Leaning",
       x="Difference in R0", 
       y="Adj Earliest Action Date", 
       title="R0 Changes from Earliest Date to 4 Weeks Later") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())

#T Test by Governor
z <- combined2$PoliLean2[-which(combined2$location_name == "North Dakota")]
gg1$PoliLean2 <- rep("y",49)
for(i in 1:49){
  gg1$PoliLean2[i] <- z[i]
}

summary(gg1 %>% filter(PoliLean2 == "Democratic") %>% .$diff)
summary(gg1 %>% filter(PoliLean2 == "Republican") %>% .$diff)

wilcox.test(diff ~ PoliLean2, data=gg1)



