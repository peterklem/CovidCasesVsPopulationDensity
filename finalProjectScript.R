library("xlsx")

setwd("C:\\Users\\Peter\\Documents\\School\\Spring 2021\\Repositories\\CovidCasesVsPopulationDensity")


covidPopDensityData <- read.xlsx("final_project_covid_data.xlsx", 1, header=TRUE) #Import data
attach(covidPopDensityData)

# Q1.a
# Find the mean, median, mode, variance, and standard deviation of the sample
# Variable: Percentage of population in each county that has contracted COVID19
countyCovidMean = mean(PercentagePositive)
countyCovidMedian = median(PercentagePositive)
countyCovidVariance = var(PercentagePositive)
countyCovidSD = sd(PercentagePositive)

# Q1.b 
# Create a histogram for this variable
hist(PercentagePositive,
     xlab="Percentage of population that tested COVID positive in each New England county",
     ylab="Amount",
     breaks=10)
# The spread is skewed right, with most counties within the 0-10% range with a few outliers above this range.

# Q1.c
# Create a boxplot for the variable and describe the spread
boxplot(PercentagePositive,
        xlab="Percentage of population that tested COVID positive in each New England county",
        horizontal=TRUE)
# The spread is skewed right heavily, with the average from the sample size being just greater than 4.

# Q1.d 
# create a bar graph of the data
barplot(PercentagePositive,
        xlab="Counties in New England",
        ylab="Percentage of county that has tested COVID positive")

# I chose the barplot to visualize the data because it shows each individual 
# county's percentage of people who have tested positive for COVID. It is a good
# visual indicator to show there are only 5 or 6 counties in New England that 
# have over 10% positivity rate.