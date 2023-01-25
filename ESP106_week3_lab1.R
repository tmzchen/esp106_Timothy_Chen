##########################

# ESP 106
# Lab 3 (Monday) - graphing

##########################

#In this lab we will start by reading merging in data on economic development and indoor and outdoor air pollution. Then we will practice making some graphs with it.



#1. First read in the three csv files: gdppercapitaandgini and airpollution
setwd("C:/Users/timmy/OneDrive/Desktop/ESP106/lab3/ESP106_week3_lab1")
GDP = read.csv('gdppercapiandgini.csv')
file.exists('gdppercapiandgini.csv')
air = read.csv('airpollution.csv')
colnames(GDP) = c('countries', 'code', 'year', 'population', 'continent', 'gini', 'GDP per capita')
colnames(GDP)
colnames(air) = c('countries', 'code', 'year', 'particulate matter', 'household', 'ozone', 'air pollution')
#Both datasets are from Our World in Data: ourworldindata.org
#The GDP dataset has GDP per capita and the GINI index (a measure of income inequality: https://en.wikipedia.org/wiki/Gini_coefficient)
#The air pollution dataset has death rates from indoor and outdoor air pollution - units are in deaths per 100,000 people
#Indoor air pollution is the Household Air Pollution from Solid Fules
#Outdoor air pollution is split into particulate matter and ozone

unique(file1$Entity)
tw = airpollution[airpollution$countries=="Taiwan",]
ch = subset(airpollution, countries=="China")
totaldeaths = air$`particulate matter` + air$ozone
airpollution = cbind(air,totaldeaths)

#Hint: The column names are long and cumbersome (because they contain information about units et) - you might want to rename some of the columns to make them easier to work with

#2. Chose two countries that you are interested in and make a plot showing the death rates from indoor air pollution and outdoor air pollution (sum of particulate matter and ozone) over time
#Distinguish the countries using different colored lines and the types of pollution using different line types
#Make sure to add a legend and appropriate titles for the axes and plot 

par(mfrow=c(1,1))
par(mar=c(5,4,2,2))
plot(tw$year, tw$household, ylim = c(0, 120),
     ylab = "Deaths per 100,000 people", 
     xlab ="Year", type = "l", 
     main = "Death rates from indoor and outdoor air pollution (1990-2015)", 
     las = 1, lwd = 3, col = "skyblue"
     )
lines(tw$year, tw$totaldeaths, type = 'l', lwd = 3, lty = 3, col = "skyblue")
lines(tw$year, ch$household, type = 'l', lwd = 3, col = "coral")
lines(tw$year, ch$totaldeaths, type = 'l', lwd = 3, lty = 3, col = 'coral')
legend("topright", c("Taiwan indoor", "Taiwan outdoor", "China indoor", "China outdoor"),
        col = c('skyblue','skyblue','coral','coral'), lty = c(1, 3, 1, 3), lwd = 2)

#Hint: you can see all the different country names using unique(x$Entity) where x is the data frame containing the air pollution data
#Then create two new data frames that countain only the rows corresponding to each of the two countries you want to look at
#Create a new column of total outdoor air pollution deaths by summing death rates from particulate matter and ozone
#Use these to make your plot and add the lines you need

#Hint: you might have to set the y scale manually to make sure your plot is wide enough to show both countries. You can do this using the "ylim" argument in plot


#3. Merge the air pollution data with the gdp data using merge()
# Merge is a function that combines data across two data frames by matching ID rows
#By default merge will identify ID rows as those where column names are the same between datasets, but it is safer to specify the columns you want to merge by yourself using "by"
#In our case, we want to merge both by country (either the "Entity" or "Code" columns) and year columns
#Note that by default, the merge function keeps only the entries that appear in both data frames - that is fine for this lab. If you need for other applications, you can change using the all.x or all.y arguments to the function - check out the documentation at ?merge

colnames(airpollution)
colnames(GDP)
GDP
airGDP = merge(GDP, airpollution, by = c("countries", "code", "year"))
colnames(airGDP)
#4. Make a plot with two subplots - one showing a scatter plot between log of per-capita GDP (x axis) and indoor air pollution death rate (y axis) and one showing log of per-capita GDP (x axis) and outdoor air pollution (y axis)
#Make sure to add appropraite titles to the plots and axes
#Use ylim to keep the range of the y axis the same between the two plots - this makes it easier for the reader to compare across the two graphs
#STRECTH GOAL CHALLENGE - color the points based on continent. NOT REQUIRED FOR FULL POINTS - a challenge if you want to push yourself - continent info is included in the GDP dataset, but it is only listed for the year 2015
#If you are trying this and getting stuck ASK FOR HELP - there are some tips and tricks for making it easier 
colnames(airGDP)

par(mfrow=c(1,2))
par(mar=c(5,4,4,2))
indoor = plot(log(airGDP$`GDP per capita`), airGDP$household, type = "p", pch=20, cex=0.5, 
     xlab = "log per Capita GDP", ylab = "deaths per 100,000 people", main = "Indoor air pollution",
     las = 1, col="seagreen", ylim = c(0,300))
out = plot(log10(airGDP$`GDP per capita`), airGDP$totaldeaths, type = "p", pch=20, cex=0.5,
       xlab = "log per Capita GDP", ylab = "deaths per 100,000 people", main = "Outdoor air pollution",
       las = 1, col = "orange1", ylim = c(0,300))