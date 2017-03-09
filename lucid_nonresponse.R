library("memisc")
library("survey")
library("lme4")

library("foreign")
library("haven")

library("dplyr")
library("tables")
library("lubridate")
library("reshape")
library("zoo")

library("ggplot2")
library("ggrepel")


# Build up ggtheme
theme_pew <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.line = element_line(colour = "black"),
      axis.text         = element_text(size = rel(0.8)),
      axis.ticks        = element_line(colour = "black"),
      legend.key        = element_rect(colour = "grey80"),
      panel.background = element_blank(),
      panel.border      = element_rect(fill = NA, colour = "grey50"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color="black", size = .3),
      axis.line.y = element_line(color="black", size = .3),
      strip.background  = element_rect(fill = "grey80", colour = "grey50", size = 0.2)
    )
}


dta<-read.csv("/Users/AHughes/Downloads/Election-2016-Polling-Data-master/polling-data.csv")
dta$dem<-0
dta$dem[which(dta$party_affiliation_code==2)] <-1
dta$rep<-0
dta$rep[which(dta$party_affiliation_code==1)] <-1
summary(dta$rep)
summary(dta$dem)
summary(dta$survey)
dta <- subset(dta, survey=="Presidential Tracker")

summary(dta$complete_datetime)
dta$y <- as.POSIXlt(dta$complete_datetime, format = "%m/%d/%y")
dta$date<-as.Date(dta$y)
dta$y <- NULL
summary(dta$date)

#collapse the data for graphs
dayta <- dta %>% group_by(date) %>% summarise(
  sdem = sum(dem),
  srep = sum(rep),
  count = n())
#label the tape
#dayta$lab <-NULL
#dayta$lab[dayta$date == "2016-07-10"] <- "Trump Tape"

dayta$dem <- dayta$sdem / dayta$count
dayta$rep <- dayta$srep / dayta$count

#restrict range
dayta <- subset(dayta, date>="2016-09-01" & date<="2016-11-01")

#plot the  average
ggplot(dayta, aes(date),  label = lab) +
  geom_line(aes(y = rep), colour = "red") +
  geom_line(aes(y = dem), colour = "blue") +
  theme_pew()  + theme(legend.position="none") +   
  ylab("Proportion of Daily Responses")
ggsave("avg_partisannonresponse.pdf" , w=8, h=4) 

ggplot(dayta, aes(date),  label = lab) +
  geom_line(aes(y = srep), colour = "red") +
  geom_line(aes(y = sdem), colour = "blue") +
  theme_pew()  + theme(legend.position="none") +   
  ylab("Total Daily Responses") 
ggsave("tot_partisannonresponse.pdf" , w=8, h=4) 
