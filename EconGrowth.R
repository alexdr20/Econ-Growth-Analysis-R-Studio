#ploting the graph of romania 
idx_rou=econ$countrycode=="ROU"&!is.na(econ$countrycode)
econ_ro=econ[idx_rou,] #creating the subset
View(econ_ro)  #opening and visualise the elements(observations of the subset) 
dev.new() #open a new window for the graph
plot(econ_ROU$yr_sch, econ_ROU$rgdpo)
text(econ_ROU$yr_sch, econ_ROU$rgdpo, econ_ROU$year) #visualise the years on the graph 

#doing a simple linear regression on Romania 
sr_rou=lm(rgdpo~yr_sch,data=econ_ROU)
summary(sr_rou)
abline(sr_rou,col="blue")

#addind year dummies and multiple regression 
econ$years_=factor(econ$year) 
ROU=ifelse(econ$countrycode=="ROU",1,0)
#new model regression introducing the year dummies and country dummy rou
mr_rou=lm(rgdpo~yr_sch+ROU+years_,data=econ)
summary(mr_rou) 

#checking jointly significance
library(car)
linearHypothesis(mr_rou,c("yr_sch=0","years_1955=0","years_1960=0","years_1965=0","years_1970=0","years_1975=0","years_1980=0","years_1985=0","years_1990=0","years_1995=0","years_2000=0","years_2005=0","years_2010=0","years_2015=0"))
        
#adding region dummies 
econ$r_=factor(econ$region_code)
#regression model adding both time and region dummies
mr_world=lm(rgdpo~yr_sch+r_+years_,data=econ)
summary(mr_world)
library(car)
dev.new()
library(ggplot2)
mr_w=lm(rgdpo~yr_sch+region_code+year,data=econ)
avPlots(mr_w)
ggplot(data = econ_regions, aes( avg_yr_sch,avg_rgdpo, colour = region_code)) +
  geom_point(size=3)

#transforming the gdp from milion to miliard
econ_ROU$rgdpo_mld=econ_ROU$rgdpo/1000
#doing regresssion on employment and gdp
plot(rgdpo_mld~year,type="b",main="employment and gdp over time",lwd=2,col="red",data=econ_ROU,ylab="",xlab="time")
lines(emp~year,lwd=2,col="blue",data=econ_ROU)

#creating lag for employment
library(dplyr)
econ_ROU=econ_ROU %>%
group_by(year) %>% # create grouping by countries
  mutate(emp_lag =  lag(emp, order_by=year), # introduce new variables
         emp_diff =  emp - emp_lag)
View(econ_ROU)
  #years of schooling and employment

#regression of employment and schooling on gdp
mr_w1=lm(rgdpo~yr_sch+emp+years_,data=econ)
summary(mr_w1)
mr_w2=lm(rgdpo~yr_sch+emp,data=econ)
summary(mr_w2)

mr_w3=lm(rgdpo~yr_sch+emp+r_+years_,data=econ)
summary(mr_w3)
library(car)
dev.new()
mr_rou2=lm(rgdpo~yr_sch+emp_rate,data=econ_ROU)

View(econ)
library(car)
linearHypothesis(mr_w1,c("yr_sch=0","years_1955=0","years_1960=0","years_1965=0","years_1970=0","years_1975=0","years_1980=0","years_1985=0","years_1990=0","years_1995=0","years_2000=0","years_2005=0","years_2010=0","years_2015=0"))

#plots for employment
#creating the plot
#create new var for empl rate
econ$empl_rate=(econ$emp/econ$pop)*100
econ_regions <- econ %>%
  ungroup() %>%
  group_by(region_code,year) %>%
  summarise(avg_empl_r = mean(empl_rate,na.rm=TRUE),avg_yr_sch = mean(yr_sch,na.rm=TRUE), 
            avg_rgdpo = mean(rgdpo,na.rm=TRUE),avg_empl=mean(emp,na.rm=TRUE)) 
View(econ_regions)
library(ggplot2)
ggplot(data = econ_regions, aes( avg_empl_r,avg_rgdpo, colour = region_code)) +
  geom_point(size=3)

library(ggplot2)
ggplot(data = econ_regions, aes( year,avg_empl_r, colour = region_code)) +
  geom_point(size=3) + geom_smooth()
