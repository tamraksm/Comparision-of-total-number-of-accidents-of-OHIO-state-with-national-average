#load all the necessary packages
library(grid)
library(ggplot2)
library(maps)
library(mapdata)
library(tidyverse)

## Create data frames for the US
accident <- read.csv("accident.csv")

## Drunk data
accident_Drunk <- accident %>%
  select(STATE, DRUNK_DR) %>%
  filter(DRUNK_DR != 0) %>%
  mutate(drunknum = 1) %>%
  group_by(STATE) %>%
  summarize(totaldrunk = sum(drunknum))

## State data
accident_STATE <- accident %>%
  select(STATE) %>%
  mutate(num = 1) %>%
  group_by(STATE) %>%
  summarize(totalnum = sum(num))

## Merge the two data frames we want to choose 
accident_Total <- merge(accident_STATE, accident_Drunk,  by = "STATE")

## Clean it up for building an U.S. map
accident_Total <- accident_Total %>%
  select(STATE, totalnum, totaldrunk) %>%
  mutate(proportion = totaldrunk/totalnum) %>%
  mutate(rank = rank(proportion)) %>%
  mutate(state_name = c("alabama","alaska","arizona","arkansas","california","colorado",
                        "connecticut","delaware","district of columbia","florida","georgia",
                        "hawaii","idaho","illinois","indiana","iowa","kansas","kentucky",
                        "louisiana","maine","maryland","massachusetts","michigan","minnesota",
                        "mississippi","missouri","montana","nebraska","nevada","new hampshire",
                        "new jersey","new mexico","new york","north carolina","north dakota",
                        "ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina",
                        "south dakota","tennessee","texas","utah","vermont","virginia",
                        "washington","west virginia","wisconsin","wyoming"))

names(accident_Total) <- c('state','totalnum','totaldrunk','proportion','Rank','region')
accident_Total[,1] <-  tolower(accident_Total[,1])

## Create US map to work with
states <- data.frame(state.center, state.abb)
states <- states[!(states$state.abb %in% c("AK", "HI")),] 
## Since Alaska and Hawaii aren't part of states_map
states_map <- map_data("state")
unique(states_map$region)
## AK and HI aren't part of states_map


## Add population to the accident_Total dataframe
# Source: http://api.census.gov/data/2015/pep/population?get=POP,GEONAME&for=state
# Uses .json file, so we need the jsonlite package
library(jsonlite)
json_file <- "population2015.json"
json_data <- fromJSON("population2015.json", flatten = TRUE)
json_data <- json_data[-c(1,53),]
accident_Total$population <- json_data
str(accident_Total)
accident_Total$population <- accident_Total$population[,1]
accident_Total$population <- as.numeric(accident_Total$population)
accident_Total$population

## Use population to adjust each state's rank appropriately
accident_Total <- accident_Total %>%
  select(state, totalnum, totaldrunk, region, population) %>%
  mutate(drunkoverpopulation = totaldrunk/population) %>%
  mutate(Rank = rank(drunkoverpopulation))

# We want to highlight Ohio on the map, so we need to create no_ohio
no_ohio<-states[states$state.abb !="OH",]


# USA map plot
ggplot()+
  geom_map(data=states_map, map=states_map,
           aes(x=long, y=lat, map_id=region), color="white", size=0.15) +
  geom_map(data=accident_Total, map=states_map,
           aes(fill=Rank, map_id=region),color="white", size=0.15) +
  geom_text(data=no_ohio, 
            aes(x=x, y=y, label=state.abb, group=NULL), size=4) +
  geom_text(data=states, 
            aes(x=-82.5963, y= 40.2210, label="OH", group=NULL), size=8, color="white") +
  coord_map("albers", lat0=39, lat1=45) +
  scale_fill_gradient2(low="#f7f4f9", mid="#df65b0", high="#67001f") +
  labs(title="2015: States ranked by Alcohol-Related Accidents, Adjusted for Population",
       x = NULL, y = NULL, caption = "Source: http://api.census.gov/data/2015/pep/population?get=POP,GEONAME&for=state") +
  theme_bw() +
  theme(panel.grid=element_blank(),panel.border=element_blank(),
        axis.ticks=element_blank(),axis.text=element_blank(),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 23),
        title = element_text(size = 25),
        legend.key.height = unit(2.75, "cm")) +
  scale_fill_distiller(palette=1,direction=1) +
  theme(plot.title = element_text(hjust = 0.5))


## Load the data into workspace
setwd("C:/Users/sanjay/Desktop/Data-VIzz/project/solo project")
cols <- c ("year","male_total","male_bac1","male_bac2","female_total","female_bac1",
           "female_bac2")
usa_sex <- read_excel("usa_sex.xlsx", sheet = "usa_Sex", col_names = cols, skip=2)
cols1 <- c ("year","male_total","male_bac1","male_bac2","female_total","female_bac1",
            "female_bac2")
ohio_sex <- read_excel("ohio_sex.xlsx", sheet = "ohio_Sex",col_names = cols1, skip=2)


## Get population for US for 1994:2014
pop9414 <- read.csv("populationlast20.csv")
pop9414 <- pop9414[c(36,45),c(2:22)]
colnames(pop9414) <- c(1994:2014)
pop9414 <- t(pop9414)
pop9414[,1:2] <- as.numeric(gsub(",", "", pop9414[,1:2]))
## US population column
us <- as.numeric(pop9414[,2])

## Ohio population column
oh <- as.numeric(pop9414[,1])

## Add population to usa_sex, ohio_sex, and no_ohio_sex for 1994:2014

## usa_sex
usa_sex <- usa_sex %>%
  mutate(population = us) %>%
  mutate(total = (male_total+female_total)) %>%
  mutate(proportion = total/population) %>%
  mutate(percent = proportion*100) %>%  # Change proportion to percentage for interpretability
  mutate(people = percent*10000)

## ohio_sex
ohio_sex <- ohio_sex %>%
  mutate(population = oh) %>%
  mutate(total = (male_total+female_total)) %>%
  mutate(proportion = total/population) %>%
  mutate(percent = proportion*100) %>% # Change proportion to percentage for interpretability
  mutate(people = percent*10000)


## Create Accident per 10,000 people by Year plot
all_plot <- ggplot() +
  labs(title = "Alcohol-Related Accidents per 10,000 People from 1994-2014",
       x = "Year", y = "# of Alcohol-Related Accidents (per 10,000 people)") +
  geom_line(aes(x = year, y = people), 
            data = usa_sex, color = "blue", size = 1.25) +
  geom_text(aes(x = 2012, y = 180, label = "National Average"),
            data = usa_sex, color = "blue", size = 10) +
  geom_line(aes(x = year, y = people), 
            data = ohio_sex, color = "red", size = 1.25) +
  geom_text(aes(x = 2012, y = 170, label = "Ohio"), 
            data = ohio_sex, color = "red", size = 10) +
  scale_x_continuous(breaks = c(1994, 1999, 2004, 2005, 2009, 2014)) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 17),
        title = element_text(size = 25),
        axis.line = element_line(color = "lightgrey")) +
  geom_vline(xintercept = c(2005, 2009))
all_plot

## Third visual
## gridExtra helps us to view multiple plots on the same graph
library(gridExtra)

## readxl helps us to read the excel data
library(readxl)

## ggplot2 helps us to create plots and graphs
library(ggplot2)

## dplyr is  fast, consistent tool for working with data frame like objects,
## both in memory and out of memory.
library(dplyr)

## tidyr is a package used to tidy our data
library(tidyr)

##For USA DATA
setwd("C:/Users/sanjay/Desktop/Data-VIzz/project/solo project")
cols <- c ("year","bac_0_num","bac_0_per","bac_1_num","bac_1_per","bac_2_num",
           "bac_2_per","total","bpp","bpp1")
usa_bac<- read_excel("usa_alcohal.xlsx", sheet = "usa_alcohal",col_names = cols, skip=2)


blood_alcohal<-usa_bac %>%
  select(year, bac_0_num,bac_1_num,bac_2_num,total)%>%
  mutate(bac_0=(bac_0_num/total)*100,bac_2=(bac_2_num/total)*100,
         bac_1=100-bac_0-bac_2)


blood_alcohal1<- blood_alcohal %>%
  select(year,bac_0,bac_1,bac_2)

##For OHIO DATA
setwd("C:/Users/sanjay/Desktop/Data-VIzz/project/solo project")
cols1 <- c ("year1","bac1_0_num","bac1_0_per","bac1_1_num","bac1_1_per",
            "bac1_2_num","bac1_2_per","total1","bpp.1","bpp1.1")
ohio_bac<- read_excel("ohio_alcohal.xlsx", sheet = "ohio_alcohal",
                      col_names = cols1, skip=2)


blood_alcohal.1<-ohio_bac %>%
  select(year1, bac1_0_num,bac1_1_num,bac1_2_num,total1)%>%
  mutate(bac1_0=(bac1_0_num/total1)*100,bac1_2=(bac1_2_num/total1)*100,
         bac1_1=100-bac1_0-bac1_2)

## select only the required columns for analysis
blood_alcohal1.1<- blood_alcohal.1 %>%
  select(year1,bac1_0,bac1_1,bac1_2)

blood_alcohal1.1<- blood_alcohal.1 %>%
  select(year1,bac1_0,bac1_1,bac1_2)

## gather all the bac groups together for both the USA and Ohio data
b3_alcohol1 <- gather(blood_alcohal1,Group,va,-year)
b_alcohol2 <- gather(blood_alcohal1.1,Group,va,-year1)  


## area plot for USA
p1<- ggplot() + 
  geom_area(data=b_alcohol1, aes(x=year,y=va,group=Group,fill=Group),position="fill")+
  scale_fill_manual(labels=c("BAC = 0","0 < BAC < 0.08","BAC > 0.08"),values=c("red", "darkorchid", "green"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Percentages of Fatal Accidents in the US",
       x="Year of Accidents",y="Percentage")+
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.00),
                     labels = paste0(c(0, 25, 50, 75, 100), "%", collapse = NULL)) +
  scale_x_continuous(breaks = c(1994, 1999, 2004, 2009, 2014)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

## area plot for Ohio
p2<- ggplot() + 
  geom_area(data=b_alcohol2, aes(x=year1,y=va,group=Group,fill=Group),position="fill")+
  scale_fill_manual(labels=c("BAC = 0","0 < BAC < 0.08","BAC > 0.08"),values=c("red", "darkorchid", "green"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Percentages of Fatal Accidents in Ohio",
       x="Year of Accidents",y="Percentage")+
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.00),
                     labels = paste0(c(0, 25, 50, 75, 100), "%", collapse = NULL)) +
  scale_x_continuous(breaks = c(1994, 1999, 2004, 2009, 2014)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17))

## make a list of two plots and name it as gs
gs <- list(p1, p2)

## arange the graphs with different widths to show the same size as legend is removed for
## one of the graphs
grid.arrange(grobs = gs, ncol = 2, widths = 5:6,
             heights = unit(7, "in"))
## Sources
# http://www.randstatestats.org/us/stats/historical-population-estimates-(states-only).html?dbc=cmFuZF91c2E
# http://api.census.gov/data/2015/pep/population?get=POP,GEONAME&for=state:*
# "Auto Crashes."III. N.p., n.d. Web. 07 June 2017.
# http://www.randstatestats.org/us/stats/historical-population-estimates-(states-only).html?dbc=cmFuZF91c2E
# https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars




