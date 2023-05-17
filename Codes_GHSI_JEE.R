###################The Global Health Security index and Joint External Evaluation score for health preparedness are not correlated with countries' COVID-19 detection response time and mortality outcome###################
#                                                                                         Mohammad Nayeem Hasan                                                                                                            #
############################################################################################################################################################################################################################

library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(dplyr)


#Dec20

setwd('E:\\WHOPAHO')

COVID <- read.csv("owid-covid-data.csv")

COVID<-COVID[(COVID$location=="Antigua and Barbuda" |
                COVID$location=="Dominica" | COVID$location=="Panama" |
                COVID$location=="Argentina" | COVID$location=="Dominican Republic" |
                COVID$location=="Paraguay" | COVID$location=="Bahamas" |
                COVID$location=="Ecuador" | COVID$location=="Peru" |
                COVID$location=="Barbados" |  COVID$location=="El Salvador" | 
                COVID$location=="Saint Kitts and Nevis" |  COVID$location=="Belize" | 
                COVID$location=="Grenada" | COVID$location=="Saint Lucia" | 
                COVID$location=="Bolivia" | COVID$location=="Guatemala" | 
                COVID$location=="Saint Vincent and the Grenadines" | COVID$location=="Brazil" |
                COVID$location=="Guyana" | COVID$location=="Suriname" | 
                COVID$location=="Canada" | COVID$location=="Haiti" |
                COVID$location=="Trinidad and Tobago" | COVID$location=="Chile" |
                COVID$location=="Honduras" | COVID$location=="United States" |
                COVID$location=="Colombia" | COVID$location=="Jamaica" |
                COVID$location=="Uruguay" |  COVID$location=="Costa Rica" | 
                COVID$location=="Mexico" |  COVID$location=="Venezuela" | 
                COVID$location=="Cuba" | COVID$location=="Nicaragua"),]


Day <- read.csv("Day.csv")
GHSI <- read.csv("GHSI.csv")
JEE_All3 <- read.csv("JEE_All3.csv")
WGI <- read.csv("WGI.csv")

finaldt1 <- merge(Day, GHSI,  by="location")
finaldt2 <- merge(JEE_All3, WGI,  by="location")

finaldt3 <- merge(finaldt1, finaldt2,  by="location")

finaldt <- merge(COVID, finaldt3,  by="location")

finaldt$GHSI[finaldt$GHSI == 0] <- NA
finaldt$WGI[finaldt$WGI == 0] <- NA
finaldt$RS[finaldt$RS == 0] <- NA

finaldt$total_deaths_per_million[is.na(finaldt$total_deaths_per_million)] <- 0

finaldt$date

finaldt_Dec20 <- subset(finaldt, finaldt$date == "2020-12-31")

finaldt_Dec20$total_deaths_per_million
finaldt_Dec20$GHSI
finaldt_Dec20$RS
finaldt_Dec20$location


#Creating CFR
finaldt_Dec20$CFR_Dec20 <- (finaldt_Dec20$total_deaths/finaldt_Dec20$total_cases)*100

t.test(finaldt_Dec20$CFR_Dec20)


t.test(finaldt_Dec20[finaldt_Dec20$GHSI <= 50, 'CFR_Dec20'])
t.test(finaldt_Dec20[finaldt_Dec20$GHSI > 50, 'CFR_Dec20'])


model.3nb <- glm.nb(total_deaths_per_million ~ GHSI + total_cases_per_million + aged_65_older + WGI + 
                      gdp_per_capita + population_density + 
                      offset(log(DayDec20)), data = finaldt_Dec20)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))


#Plot

# install.packages("ggplot2")
library(ggplot2)

finaldt_Dec20 <- finaldt_Dec20 %>%
  mutate(location2 = case_when(location == "Argentina" ~ 'Argentina',
                               location == "Brazil" ~ 'Brazil',
                               location == "Canada" ~ 'Canada',
                               location == "Chile" ~ 'Chile',
                               location == "Colombia" ~ 'Colombia',
                               location == "Costa Rica" ~ 'Costa Rica',
                               location == "Ecuador" ~ 'Ecuador',
                               location == "El Salvador" ~ 'El Salvador',
                               location == "Mexico" ~ 'Mexico',
                               location == "Peru" ~ 'Peru',
                               location == "United States" ~ 'United States'))


a <- ggplot(finaldt_Dec20, aes(x = GHSI, y = total_deaths_per_million)) + ggtitle("December 31, 2020") + 
  theme(plot.title = element_text(hjust = 0.5,size=20,face = "bold"),text = element_text(size = 20)) +
  geom_point(aes(colour = GHSI > 44 & GHSI < 84), size = 4,
             show.legend = F) +
  #geom_vline(xintercept = 44.2, linetype = "dashed") + 
  #geom_vline(xintercept = 83.5, linetype = "dashed") +  
  geom_text_repel(aes(colour = GHSI > 44 & GHSI < 84), 
            show.legend = F, 
            size = 8, 
            label=finaldt_Dec20$location2, 
            hjust = -0.5,
            vjust= 0.1)+
  xlab("") + ylab("Total Deaths per million") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 3000))

a


#June21

setwd('E:\\WHOPAHO')

COVID <- read.csv("owid-covid-data.csv")

COVID<-COVID[(COVID$location=="Antigua and Barbuda" |
                COVID$location=="Dominica" | COVID$location=="Panama" |
                COVID$location=="Argentina" | COVID$location=="Dominican Republic" |
                COVID$location=="Paraguay" | COVID$location=="Bahamas" |
                COVID$location=="Ecuador" | COVID$location=="Peru" |
                COVID$location=="Barbados" |  COVID$location=="El Salvador" | 
                COVID$location=="Saint Kitts and Nevis" |  COVID$location=="Belize" | 
                COVID$location=="Grenada" | COVID$location=="Saint Lucia" | 
                COVID$location=="Bolivia" | COVID$location=="Guatemala" | 
                COVID$location=="Saint Vincent and the Grenadines" | COVID$location=="Brazil" |
                COVID$location=="Guyana" | COVID$location=="Suriname" | 
                COVID$location=="Canada" | COVID$location=="Haiti" |
                COVID$location=="Trinidad and Tobago" | COVID$location=="Chile" |
                COVID$location=="Honduras" | COVID$location=="United States" |
                COVID$location=="Colombia" | COVID$location=="Jamaica" |
                COVID$location=="Uruguay" |  COVID$location=="Costa Rica" | 
                COVID$location=="Mexico" |  COVID$location=="Venezuela" | 
                COVID$location=="Cuba" | COVID$location=="Nicaragua"),]


Day <- read.csv("Day.csv")
GHSI <- read.csv("GHSI.csv")
JEE_All3 <- read.csv("JEE_All3.csv")
WGI <- read.csv("WGI.csv")

finaldt1 <- merge(Day, GHSI,  by="location")
finaldt2 <- merge(JEE_All3, WGI,  by="location")

finaldt3 <- merge(finaldt1, finaldt2,  by="location")

finaldt <- merge(COVID, finaldt3,  by="location")

finaldt$GHSI[finaldt$GHSI == 0] <- NA
finaldt$WGI[finaldt$WGI == 0] <- NA
finaldt$RS[finaldt$RS == 0] <- NA

finaldt$total_deaths_per_million[is.na(finaldt$total_deaths_per_million)] <- 0

finaldt$date

finaldt_Jun21 <- subset(finaldt, finaldt$date == "2021-06-30")

finaldt_Jun21$total_deaths_per_million
finaldt_Jun21$GHSI

#Creating CFR
finaldt_Jun21$CFR_Jun21 <- (finaldt_Jun21$total_deaths/finaldt_Jun21$total_cases)*100
t.test(finaldt_Jun21$CFR_Jun21)

t.test(finaldt_Jun21[finaldt_Jun21$GHSI <= 50, 'CFR_Jun21'])
t.test(finaldt_Jun21[finaldt_Jun21$GHSI > 50, 'CFR_Jun21'])

#JEE 

model.3nb <- glm.nb(total_deaths_per_million ~ GHSI + total_cases_per_million + aged_65_older + WGI + 
                      gdp_per_capita + population_density + 
                      offset(log(DayJun21)), data = finaldt_Jun21)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))



#Plot

# install.packages("ggplot2")
library(ggplot2)
View(finaldt_Jun21)

finaldt_Jun21 <- finaldt_Jun21 %>%
  mutate(location2 = case_when(location == "Argentina" ~ 'Argentina',
                               location == "Brazil" ~ 'Brazil',
                               location == "Canada" ~ 'Canada',
                               location == "Chile" ~ 'Chile',
                               location == "Colombia" ~ 'Colombia',
                               location == "Costa Rica" ~ 'Costa Rica',
                               location == "Ecuador" ~ 'Ecuador',
                               location == "El Salvador" ~ 'El Salvador',
                               location == "Mexico" ~ 'Mexico',
                               location == "Peru" ~ 'Peru',
                               location == "United States" ~ 'United States'))


b <- ggplot(finaldt_Jun21, aes(x = GHSI, y = total_deaths_per_million)) + ggtitle("June 30, 2021") + 
  theme(plot.title = element_text(hjust = 0.5,size=20,face = "bold"),text = element_text(size = 20))+
  geom_point(aes(colour = GHSI > 44 & GHSI < 84), size = 4,
             show.legend = F) +
  #geom_vline(xintercept = 44.2, linetype = "dashed") + 
  #geom_vline(xintercept = 83.5, linetype = "dashed") +  
  geom_text_repel(aes(colour = GHSI > 44 & GHSI < 84), 
                  show.legend = F, 
                  size = 8, 
                  label=finaldt_Jun21$location2, 
                  hjust = -0.2,
                  vjust= 0.5) +
  xlab("") + ylab("Total Deaths per million") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 6000))


b
#Dec21

setwd('E:\\WHOPAHO')

COVID <- read.csv("owid-covid-data.csv")

COVID<-COVID[(COVID$location=="Antigua and Barbuda" |
                COVID$location=="Dominica" | COVID$location=="Panama" |
                COVID$location=="Argentina" | COVID$location=="Dominican Republic" |
                COVID$location=="Paraguay" | COVID$location=="Bahamas" |
                COVID$location=="Ecuador" | COVID$location=="Peru" |
                COVID$location=="Barbados" |  COVID$location=="El Salvador" | 
                COVID$location=="Saint Kitts and Nevis" |  COVID$location=="Belize" | 
                COVID$location=="Grenada" | COVID$location=="Saint Lucia" | 
                COVID$location=="Bolivia" | COVID$location=="Guatemala" | 
                COVID$location=="Saint Vincent and the Grenadines" | COVID$location=="Brazil" |
                COVID$location=="Guyana" | COVID$location=="Suriname" | 
                COVID$location=="Canada" | COVID$location=="Haiti" |
                COVID$location=="Trinidad and Tobago" | COVID$location=="Chile" |
                COVID$location=="Honduras" | COVID$location=="United States" |
                COVID$location=="Colombia" | COVID$location=="Jamaica" |
                COVID$location=="Uruguay" |  COVID$location=="Costa Rica" | 
                COVID$location=="Mexico" |  COVID$location=="Venezuela" | 
                COVID$location=="Cuba" | COVID$location=="Nicaragua"),]


Day <- read.csv("Day.csv")
GHSI <- read.csv("GHSI.csv")
JEE_All3 <- read.csv("JEE_All3.csv")
WGI <- read.csv("WGI.csv")

finaldt1 <- merge(Day, GHSI,  by="location")
finaldt2 <- merge(JEE_All3, WGI,  by="location")

finaldt3 <- merge(finaldt1, finaldt2,  by="location")

finaldt <- merge(COVID, finaldt3,  by="location")

finaldt$GHSI[finaldt$GHSI == 0] <- NA
finaldt$WGI[finaldt$WGI == 0] <- NA
finaldt$RS[finaldt$RS == 0] <- NA

finaldt$total_deaths_per_million[is.na(finaldt$total_deaths_per_million)] <- 0

finaldt$date

finaldt_Dec21 <- subset(finaldt, finaldt$date == "2021-12-31")

finaldt_Dec21$total_deaths_per_million
finaldt_Dec21$GHSI

#Creating CFR
finaldt_Dec21$CFR_Dec21 <- (finaldt_Dec21$total_deaths/finaldt_Dec21$total_cases)*100
t.test(finaldt_Dec21$CFR_Dec21)

t.test(finaldt_Dec21[finaldt_Dec21$GHSI <= 50, 'CFR_Dec21'])
t.test(finaldt_Dec21[finaldt_Dec21$GHSI > 50, 'CFR_Dec21'])

#JEE 

model.3nb <- glm.nb(total_deaths_per_million ~ GHSI + total_cases_per_million + aged_65_older + WGI + 
                      gdp_per_capita + population_density + 
                      offset(log(DayDec21)), data = finaldt_Dec21)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))


#Plot

# install.packages("ggplot2")
library(ggplot2)

finaldt_Dec21 <- finaldt_Dec21 %>%
  mutate(location2 = case_when(location == "Argentina" ~ 'Argentina',
                               location == "Brazil" ~ 'Brazil',
                               location == "Canada" ~ 'Canada',
                               location == "Chile" ~ 'Chile',
                               location == "Colombia" ~ 'Colombia',
                               location == "Costa Rica" ~ 'Costa Rica',
                               location == "Ecuador" ~ 'Ecuador',
                               location == "El Salvador" ~ 'El Salvador',
                               location == "Mexico" ~ 'Mexico',
                               location == "Peru" ~ 'Peru',
                               location == "United States" ~ 'United States'))


c <- ggplot(finaldt_Dec21, aes(x = GHSI, y = total_deaths_per_million)) + ggtitle("December 31, 2021") + 
  theme(plot.title = element_text(hjust = 0.5,size=20,face = "bold"),text = element_text(size = 20))+
  geom_point(aes(colour = GHSI > 44 & GHSI < 84), size = 4,
             show.legend = F) +
  #geom_vline(xintercept = 44.2, linetype = "dashed") + 
  #geom_vline(xintercept = 83.5, linetype = "dashed") +  
  geom_text_repel(aes(colour = GHSI > 44 & GHSI < 84), 
                  show.legend = F, 
                  size = 8, 
                  label=finaldt_Dec21$location2, 
                  hjust = -0.3,
                  vjust= 0.4) +
  xlab("Global Health Security Index") + ylab("Total Deaths per million") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 6500))


c

#Jul22

setwd('E:\\WHOPAHO')

COVID <- read.csv("owid-covid-data.csv")

COVID<-COVID[(COVID$location=="Antigua and Barbuda" |
                COVID$location=="Dominica" | COVID$location=="Panama" |
                COVID$location=="Argentina" | COVID$location=="Dominican Republic" |
                COVID$location=="Paraguay" | COVID$location=="Bahamas" |
                COVID$location=="Ecuador" | COVID$location=="Peru" |
                COVID$location=="Barbados" |  COVID$location=="El Salvador" | 
                COVID$location=="Saint Kitts and Nevis" |  COVID$location=="Belize" | 
                COVID$location=="Grenada" | COVID$location=="Saint Lucia" | 
                COVID$location=="Bolivia" | COVID$location=="Guatemala" | 
                COVID$location=="Saint Vincent and the Grenadines" | COVID$location=="Brazil" |
                COVID$location=="Guyana" | COVID$location=="Suriname" | 
                COVID$location=="Canada" | COVID$location=="Haiti" |
                COVID$location=="Trinidad and Tobago" | COVID$location=="Chile" |
                COVID$location=="Honduras" | COVID$location=="United States" |
                COVID$location=="Colombia" | COVID$location=="Jamaica" |
                COVID$location=="Uruguay" |  COVID$location=="Costa Rica" | 
                COVID$location=="Mexico" |  COVID$location=="Venezuela" | 
                COVID$location=="Cuba" | COVID$location=="Nicaragua"),]


Day <- read.csv("Day.csv")
GHSI <- read.csv("GHSI.csv")
JEE_All3 <- read.csv("JEE_All3.csv")
WGI <- read.csv("WGI.csv")

finaldt1 <- merge(Day, GHSI,  by="location")
finaldt2 <- merge(JEE_All3, WGI,  by="location")

finaldt3 <- merge(finaldt1, finaldt2,  by="location")

finaldt <- merge(COVID, finaldt3,  by="location")

finaldt$GHSI[finaldt$GHSI == 0] <- NA
finaldt$WGI[finaldt$WGI == 0] <- NA
finaldt$RS[finaldt$RS == 0] <- NA

finaldt$total_deaths_per_million[is.na(finaldt$total_deaths_per_million)] <- 0

finaldt$date

finaldt_Jul22 <- subset(finaldt, finaldt$date == "2022-07-31")

finaldt_Jul22$total_deaths_per_million
finaldt_Jul22$GHSI

#Creating CFR
finaldt_Jul22$CFR_Jul22 <- (finaldt_Jul22$total_deaths/finaldt_Jul22$total_cases)*100

t.test(finaldt_Jul22$total_vaccinations_per_hundred)
t.test(finaldt_Jul22[finaldt_Jul22$GHSI <= 50, 'total_vaccinations_per_hundred'])
t.test(finaldt_Jul22[finaldt_Jul22$GHSI > 50, 'total_vaccinations_per_hundred'])


t.test(finaldt_Jul22$CFR_Jul22)

t.test(finaldt_Jul22[finaldt_Jul22$GHSI <= 50, 'CFR_Jul22'])
t.test(finaldt_Jul22[finaldt_Jul22$GHSI > 50, 'CFR_Jul22'])


t.test(finaldt_Dec20$CFR_Dec20, finaldt_Jul22$CFR_Jul22, paired = TRUE, alternative = "two.sided")

t.test(finaldt_Dec20[finaldt_Dec20$GHSI <= 50, 'CFR_Dec20'], finaldt_Jul22[finaldt_Jul22$GHSI <= 50, 'CFR_Jul22'], paired = TRUE, alternative = "two.sided")
t.test(finaldt_Dec20[finaldt_Dec20$GHSI > 50, 'CFR_Dec20'], finaldt_Jul22[finaldt_Jul22$GHSI > 50, 'CFR_Jul22'], paired = TRUE, alternative = "two.sided")


#JEE 

model.3nb <- glm.nb(total_deaths_per_million ~ GHSI + total_cases_per_million + aged_65_older + WGI + 
                      gdp_per_capita + population_density + 
                      offset(log(DayJul22)), data = finaldt_Jul22)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))


#Plot

# install.packages("ggplot2")
library(ggplot2)
library(ggrepel)

finaldt_Jul22 <- finaldt_Jul22 %>%
  mutate(location2 = case_when(location == "Argentina" ~ 'Argentina',
                               location == "Brazil" ~ 'Brazil',
                               location == "Canada" ~ 'Canada',
                               location == "Chile" ~ 'Chile',
                               location == "Colombia" ~ 'Colombia',
                               location == "Costa Rica" ~ 'Costa Rica',
                               location == "Ecuador" ~ 'Ecuador',
                               location == "El Salvador" ~ 'El Salvador',
                               location == "Mexico" ~ 'Mexico',
                               location == "Peru" ~ 'Peru',
                               location == "United States" ~ 'United States'))


d <- ggplot(finaldt_Jul22, aes(x = GHSI, y = total_deaths_per_million)) + ggtitle("July 15, 2022") + 
  theme(plot.title = element_text(hjust = 0.5,size=20,face = "bold"),text = element_text(size = 20))+
  geom_point(aes(colour = GHSI > 44 & GHSI < 84), size = 4,
             show.legend = F) +
  #geom_vline(xintercept = 44.2, linetype = "dashed") + 
  #geom_vline(xintercept = 83.5, linetype = "dashed") +  
  geom_text_repel(aes(colour = GHSI > 44 & GHSI < 84), 
                  show.legend = F, 
                  size = 8, 
                  label=finaldt_Jul22$location2, 
                  hjust = -0.2,
                  vjust= 0.5) +
  xlab("Global Health Security Index") + ylab("Total Deaths per million") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 6500))


d

tiff("Graph.tiff", units="in", width=22, height=17, res=300)
gridExtra::grid.arrange(a,b,c,d)
dev.off()



#Excess Mortality

###################The Global Health Security index and Joint External Evaluation score for health preparedness are not correlated with countries' COVID-19 detection response time and mortality outcome###################
#                                                                                         Mohammad Nayeem Hasan                                                                                                            #
############################################################################################################################################################################################################################

library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(dplyr)


#Dec20

setwd('E:\\WHOPAHO')

COVID <- read.csv("excess-deaths-daily-per-100k-economist.csv")
COVID$location <- COVID$Entity 
COVID$date <- COVID$Day

COVID<-COVID[(COVID$location=="Antigua and Barbuda" |
                COVID$location=="Dominica" | COVID$location=="Panama" |
                COVID$location=="Argentina" | COVID$location=="Dominican Republic" |
                COVID$location=="Paraguay" | COVID$location=="Bahamas" |
                COVID$location=="Ecuador" | COVID$location=="Peru" |
                COVID$location=="Barbados" |  COVID$location=="El Salvador" | 
                COVID$location=="Saint Kitts and Nevis" |  COVID$location=="Belize" | 
                COVID$location=="Grenada" | COVID$location=="Saint Lucia" | 
                COVID$location=="Bolivia" | COVID$location=="Guatemala" | 
                COVID$location=="Saint Vincent and the Grenadines" | COVID$location=="Brazil" |
                COVID$location=="Guyana" | COVID$location=="Suriname" | 
                COVID$location=="Canada" | COVID$location=="Haiti" |
                COVID$location=="Trinidad and Tobago" | COVID$location=="Chile" |
                COVID$location=="Honduras" | COVID$location=="United States" |
                COVID$location=="Colombia" | COVID$location=="Jamaica" |
                COVID$location=="Uruguay" |  COVID$location=="Costa Rica" | 
                COVID$location=="Mexico" |  COVID$location=="Venezuela" | 
                COVID$location=="Cuba" | COVID$location=="Nicaragua"),]

COVID_Dec20 <- subset(COVID, COVID$date == "2020-12-28")

Day <- read.csv("Day.csv")
GHSI <- read.csv("GHSI.csv")
JEE_All3 <- read.csv("JEE_All3.csv")
WGI <- read.csv("WGI.csv")

finaldt1 <- merge(Day, GHSI,  by="location")
finaldt2 <- merge(JEE_All3, WGI,  by="location")

finaldt3 <- merge(finaldt1, finaldt2,  by="location")

finaldt4 <- merge(COVID_Dec20, finaldt3,  by="location")

COVIDf <- read.csv("owid-covid-data.csv")
COVIDf <- subset(COVIDf, COVIDf$date == "2020-12-28")

finaldt_Dec20 <- merge(finaldt4, COVIDf,  by="location")

finaldt_Dec20$GHSI[finaldt_Dec20$GHSI == 0] <- NA
finaldt_Dec20$WGI[finaldt_Dec20$WGI == 0] <- NA
finaldt_Dec20$RS[finaldt_Dec20$RS == 0] <- NA
finaldt_Dec20

t.test(finaldt_Dec20$estimated_daily_excess_deaths_per_100k)

t.test(finaldt_Dec20[finaldt_Dec20$GHSI <= 50, 'estimated_daily_excess_deaths_per_100k'])
t.test(finaldt_Dec20[finaldt_Dec20$GHSI > 50, 'estimated_daily_excess_deaths_per_100k'])

#June21

setwd('E:\\WHOPAHO')

COVID <- read.csv("excess-deaths-daily-per-100k-economist.csv")
COVID$location <- COVID$Entity 
COVID$date <- COVID$Day

COVID<-COVID[(COVID$location=="Antigua and Barbuda" |
                COVID$location=="Dominica" | COVID$location=="Panama" |
                COVID$location=="Argentina" | COVID$location=="Dominican Republic" |
                COVID$location=="Paraguay" | COVID$location=="Bahamas" |
                COVID$location=="Ecuador" | COVID$location=="Peru" |
                COVID$location=="Barbados" |  COVID$location=="El Salvador" | 
                COVID$location=="Saint Kitts and Nevis" |  COVID$location=="Belize" | 
                COVID$location=="Grenada" | COVID$location=="Saint Lucia" | 
                COVID$location=="Bolivia" | COVID$location=="Guatemala" | 
                COVID$location=="Saint Vincent and the Grenadines" | COVID$location=="Brazil" |
                COVID$location=="Guyana" | COVID$location=="Suriname" | 
                COVID$location=="Canada" | COVID$location=="Haiti" |
                COVID$location=="Trinidad and Tobago" | COVID$location=="Chile" |
                COVID$location=="Honduras" | COVID$location=="United States" |
                COVID$location=="Colombia" | COVID$location=="Jamaica" |
                COVID$location=="Uruguay" |  COVID$location=="Costa Rica" | 
                COVID$location=="Mexico" |  COVID$location=="Venezuela" | 
                COVID$location=="Cuba" | COVID$location=="Nicaragua"),]

COVID_Jun21 <- subset(COVID, COVID$date == "2021-06-28")

Day <- read.csv("Day.csv")
GHSI <- read.csv("GHSI.csv")
JEE_All3 <- read.csv("JEE_All3.csv")
WGI <- read.csv("WGI.csv")

finaldt1 <- merge(Day, GHSI,  by="location")
finaldt2 <- merge(JEE_All3, WGI,  by="location")

finaldt3 <- merge(finaldt1, finaldt2,  by="location")

finaldt4 <- merge(COVID_Jun21, finaldt3,  by="location")

COVIDf <- read.csv("owid-covid-data.csv")
COVIDf <- subset(COVIDf, COVIDf$date == "2021-06-28")

finaldt_Jun21 <- merge(finaldt4, COVIDf,  by="location")

finaldt_Jun21$GHSI[finaldt_Jun21$GHSI == 0] <- NA
finaldt_Jun21$WGI[finaldt_Jun21$WGI == 0] <- NA
finaldt_Jun21$RS[finaldt_Jun21$RS == 0] <- NA
finaldt_Jun21

t.test(finaldt_Jun21$estimated_daily_excess_deaths_per_100k)

t.test(finaldt_Jun21[finaldt_Jun21$GHSI <= 50, 'estimated_daily_excess_deaths_per_100k'])
t.test(finaldt_Jun21[finaldt_Jun21$GHSI > 50, 'estimated_daily_excess_deaths_per_100k'])

#Dec21

setwd('E:\\WHOPAHO')

COVID <- read.csv("excess-deaths-daily-per-100k-economist.csv")
COVID$location <- COVID$Entity 
COVID$date <- COVID$Day

COVID<-COVID[(COVID$location=="Antigua and Barbuda" |
                COVID$location=="Dominica" | COVID$location=="Panama" |
                COVID$location=="Argentina" | COVID$location=="Dominican Republic" |
                COVID$location=="Paraguay" | COVID$location=="Bahamas" |
                COVID$location=="Ecuador" | COVID$location=="Peru" |
                COVID$location=="Barbados" |  COVID$location=="El Salvador" | 
                COVID$location=="Saint Kitts and Nevis" |  COVID$location=="Belize" | 
                COVID$location=="Grenada" | COVID$location=="Saint Lucia" | 
                COVID$location=="Bolivia" | COVID$location=="Guatemala" | 
                COVID$location=="Saint Vincent and the Grenadines" | COVID$location=="Brazil" |
                COVID$location=="Guyana" | COVID$location=="Suriname" | 
                COVID$location=="Canada" | COVID$location=="Haiti" |
                COVID$location=="Trinidad and Tobago" | COVID$location=="Chile" |
                COVID$location=="Honduras" | COVID$location=="United States" |
                COVID$location=="Colombia" | COVID$location=="Jamaica" |
                COVID$location=="Uruguay" |  COVID$location=="Costa Rica" | 
                COVID$location=="Mexico" |  COVID$location=="Venezuela" | 
                COVID$location=="Cuba" | COVID$location=="Nicaragua"),]

COVID_Dec21 <- subset(COVID, COVID$date == "2021-12-27")

Day <- read.csv("Day.csv")
GHSI <- read.csv("GHSI.csv")
JEE_All3 <- read.csv("JEE_All3.csv")
WGI <- read.csv("WGI.csv")

finaldt1 <- merge(Day, GHSI,  by="location")
finaldt2 <- merge(JEE_All3, WGI,  by="location")

finaldt3 <- merge(finaldt1, finaldt2,  by="location")

finaldt4 <- merge(COVID_Dec20, finaldt3,  by="location")

COVIDf <- read.csv("owid-covid-data.csv")
COVIDf <- subset(COVIDf, COVIDf$date == "2021-12-27")

finaldt_Dec21 <- merge(finaldt4, COVIDf,  by="location")

finaldt_Dec21$GHSI[finaldt_Dec21$GHSI == 0] <- NA
finaldt_Dec21$WGI[finaldt_Dec21$WGI == 0] <- NA
finaldt_Dec21$RS[finaldt_Dec21$RS == 0] <- NA
finaldt_Dec21

t.test(finaldt_Dec21$estimated_daily_excess_deaths_per_100k)

t.test(finaldt_Dec21[finaldt_Dec21$GHSI <= 50, 'estimated_daily_excess_deaths_per_100k'])
t.test(finaldt_Dec21[finaldt_Dec21$GHSI > 50, 'estimated_daily_excess_deaths_per_100k'])

#Jul22
setwd('E:\\WHOPAHO')

COVID <- read.csv("excess-deaths-daily-per-100k-economist.csv")
COVID$location <- COVID$Entity 
COVID$date <- COVID$Day

COVID<-COVID[(COVID$location=="Antigua and Barbuda" |
                COVID$location=="Dominica" | COVID$location=="Panama" |
                COVID$location=="Argentina" | COVID$location=="Dominican Republic" |
                COVID$location=="Paraguay" | COVID$location=="Bahamas" |
                COVID$location=="Ecuador" | COVID$location=="Peru" |
                COVID$location=="Barbados" |  COVID$location=="El Salvador" | 
                COVID$location=="Saint Kitts and Nevis" |  COVID$location=="Belize" | 
                COVID$location=="Grenada" | COVID$location=="Saint Lucia" | 
                COVID$location=="Bolivia" | COVID$location=="Guatemala" | 
                COVID$location=="Saint Vincent and the Grenadines" | COVID$location=="Brazil" |
                COVID$location=="Guyana" | COVID$location=="Suriname" | 
                COVID$location=="Canada" | COVID$location=="Haiti" |
                COVID$location=="Trinidad and Tobago" | COVID$location=="Chile" |
                COVID$location=="Honduras" | COVID$location=="United States" |
                COVID$location=="Colombia" | COVID$location=="Jamaica" |
                COVID$location=="Uruguay" |  COVID$location=="Costa Rica" | 
                COVID$location=="Mexico" |  COVID$location=="Venezuela" | 
                COVID$location=="Cuba" | COVID$location=="Nicaragua"),]

COVID_Jul22 <- subset(COVID, COVID$date == "2022-07-26")

Day <- read.csv("Day.csv")
GHSI <- read.csv("GHSI.csv")
JEE_All3 <- read.csv("JEE_All3.csv")
WGI <- read.csv("WGI.csv")

finaldt1 <- merge(Day, GHSI,  by="location")
finaldt2 <- merge(JEE_All3, WGI,  by="location")

finaldt3 <- merge(finaldt1, finaldt2,  by="location")

finaldt4 <- merge(COVID_Jul22, finaldt3,  by="location")

COVIDf <- read.csv("owid-covid-data.csv")
COVIDf <- subset(COVIDf, COVIDf$date == "2022-07-26")

finaldt_Jul22 <- merge(finaldt4, COVIDf,  by="location")

finaldt_Jul22$GHSI[finaldt_Jul22$GHSI == 0] <- NA
finaldt_Jul22$WGI[finaldt_Jul22$WGI == 0] <- NA
finaldt_Jul22$RS[finaldt_Jul22$RS == 0] <- NA
finaldt_Jul22

t.test(finaldt_Jul22$estimated_daily_excess_deaths_per_100k)

t.test(finaldt_Jul22[finaldt_Jul22$GHSI <= 50, 'estimated_daily_excess_deaths_per_100k'])
t.test(finaldt_Jul22[finaldt_Jul22$GHSI > 50, 'estimated_daily_excess_deaths_per_100k'])

t.test(finaldt_Jul22$estimated_daily_excess_deaths_per_100k, finaldt_Dec20$estimated_daily_excess_deaths_per_100k, paired = TRUE, alternative = "two.sided")


t.test(finaldt_Dec20[finaldt_Dec20$GHSI <= 50, 'estimated_daily_excess_deaths_per_100k'], finaldt_Jul22[finaldt_Jul22$GHSI <= 50, 'estimated_daily_excess_deaths_per_100k'], paired = TRUE, alternative = "two.sided")
t.test(finaldt_Dec20[finaldt_Dec20$GHSI > 50, 'estimated_daily_excess_deaths_per_100k'], finaldt_Jul22[finaldt_Jul22$GHSI > 50, 'estimated_daily_excess_deaths_per_100k'], paired = TRUE, alternative = "two.sided")
