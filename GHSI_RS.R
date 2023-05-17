library(MASS)
require(foreign)
require(ggplot2)
require(maptools)

COVID <- read.csv("F:\\ResearchProject\\Jamal Sir\\COVID19-Factors\\Published - GHSI&JEE\\Covid19.csv")
head(COVID)

COVID_july1 <- subset(COVID, COVID$date == "7/1/2020")

Day <- read.csv("F:\\ResearchProject\\Jamal Sir\\COVID19-Factors\\Published - GHSI&JEE\\Day.csv")
GHSI <- read.csv("F:\\ResearchProject\\Jamal Sir\\COVID19-Factors\\Published - GHSI&JEE\\GHSI.csv")
JEE_All3 <- read.csv("F:\\ResearchProject\\Jamal Sir\\COVID19-Factors\\Published - GHSI&JEE\\JEE_All3.csv")
WGI <- read.csv("F:\\ResearchProject\\Jamal Sir\\COVID19-Factors\\Published - GHSI&JEE\\WGI.csv")

finaldt1 <- merge(Day, GHSI,  by="location")
finaldt2 <- merge(JEE_All3, WGI,  by="location")

finaldt3 <- merge(finaldt1, finaldt2,  by="location")

finaldt <- merge(COVID_july1, finaldt3,  by="location")

finaldt$GHSI[finaldt$GHSI == 0] <- NA
finaldt$WGI[finaldt$WGI == 0] <- NA
finaldt$RS[finaldt$RS == 0] <- NA
view(finaldt)

############NB regression model###########


#JEE 

model.3nb <- glm.nb(total_deaths_per_million ~ RS + population_density + aged_65_older
                    + gdp_per_capita + WGI + 
                      total_cases_per_million  + 
                      offset(log(Day)), data = finaldt)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))


#GHSI

model.3nb <- glm.nb(total_deaths_per_million ~ GHSI + population_density + aged_65_older
                    + gdp_per_capita + WGI + 
                      total_cases_per_million  +
                      offset(log(Day)), data = finaldt)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))





