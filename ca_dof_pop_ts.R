# MAKE CA DOF COUNTY POPULATION TIME SERIES

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

## 1981 - 1990

xls1 <- read_excel(".\\90e-4.xls")
xls1 <- xls1[c(7:75, 77),]
xls1.cols <- c("county", seq(from = 1981, to = 1990, by = 1)) 
colnames(xls1) <- xls1.cols 
xls1$county[xls1$county == "CALIFORNIA"] <- "State Total" # renaming as State Total to match later data 
rm(xls1.cols)
xls1 <- xls1[!is.na(xls1$county),] 
xls1[2:11] <- apply(xls1[2:11], 2, as.numeric)
xls1$county <- str_trim(xls1$county)
str(xls1)
xls1 <- gather(xls1, key = year, value = population.estimate, 2:11)


## 1991 - 2000

xls2 <- read_excel(".\\E-4_90-00_Rpt.xls")
xls2 <- xls2[, - c(2, 13)] # remove 4/1/1990 and 4/1/2000 census estimates
xls2 <- xls2[c(4:61, 63),] # CALIFORNIA == State Total    
xls2.cols <- c("county", seq(from = 1991, to = 2000, by = 1))    
colnames(xls2) <- xls2.cols
rm(xls2.cols)
xls2$county <- str_trim(xls2$county)
str(xls2)
xls2 <- gather(xls2, key = year, value = population.estimate, 2:11)

## 2001 - 2010

xls3 <- read_excel(".\\E4_2000-2010_Report_Final_EOC_000.xls", sheet = "Table 1 County State")
xls3 <- xls3[, - c(2, 13)] # remove 4/1/2000 and 4/1/2010 census estimates
xls3 <- xls3[c(5:62, 64),]    
xls3.cols <- c("county", seq(from = 2001, to = 2010, by = 1))   
colnames(xls3) <- xls3.cols
rm(xls3.cols)
xls3$county <- str_trim(xls3$county)
str(xls3)
xls3 <- gather(xls3, key = year, value = population.estimate, 2:11)

## 2010 - 2016

xls4 <- read_excel(".\\E-42016InternetVersion.xls", sheet = "Table 1 County State")
xls4 <- xls4[, - 2] # remove 4/1/2010 census estimates
xls4 <- xls4[c(5:62, 64),]    
xls4.cols <- c("county", seq(from = 2011, to = 2016, by = 1))   
colnames(xls4) <- xls4.cols
rm(xls4.cols)
xls4$county <- str_trim(xls4$county)
str(xls4)
xls4 <- gather(xls4, key = year, value = population.estimate, 2:7)

## Merge 

ca_pop <- data.frame(do.call(rbind.data.frame, list(xls1, xls2, xls3, xls4))) 
stopifnot(nrow(ca_pop) == 59*(2017-1981))
ca_pop$county <- as.factor(ca_pop$county)
ca_pop$year <- as.numeric(ca_pop$year)
str(ca_pop)
rm(xls1, xls2, xls3, xls4) 

sum.na <- function(x) {
  sum(is.na(x))
}

apply(ca_pop, 2, sum.na)

## Add vars
## Per year, fraction of total by county
## Per yes, percentage change

ca_pop <- ca_pop %>%
  group_by(year) %>%
  mutate(population.quintile = ifelse(county == "State Total", NA, ntile(population.estimate, 5))) %>%
  mutate(state.frac = ifelse(county == "State Total", NA, population.estimate/population.estimate[county == "State Total"])) %>%
  mutate(state.frac.quintile = ifelse(county == "State Total", NA, ntile(state.frac, 5))) %>%
  ungroup() %>%
  group_by(county) %>%
  mutate(pop.per.chg = 100*(population.estimate - lag(population.estimate))/lag(population.estimate)) %>%
  mutate(pop.per.chg.quintile = ifelse(county == "State Total", NA, ntile(pop.per.chg, 5))) %>%
  ungroup() %>%
  arrange(county)  

apply(ca_pop, 2, sum.na)

save(ca_pop, file = "ca_dof_pop_1981_2016.Rdata")
write.csv(ca_pop, file = "ca_dof_pop_1981_2016.csv", row.names = FALSE)
