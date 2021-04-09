library("xlsx")
setwd("~/Classes/Prob and Stat/Final/CovidCasesVsPopulationDensity")
#setwd("C:\\Users\\Peter\\Documents\\School\\Spring 2021\\Repositories\\CovidCasesVsPopulationDensity")


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


#independent : Population Density
#dependent : Infection %
#Q2.A
summary(lm(PercentagePositive~PplPerSqKm))
plot(PplPerSqKm,PercentagePositive,xlab = "People Per Sq Km",ylab = "Percentage Postive")
#Q2.B
abline(lm(PercentagePositive~PplPerSqKm))
text(x=4000,y=6,label="y=0.0023035x+3.8")
#equation: y=187.46x-447.97
#Q2.B.1
# y=0.0023035x+3.8
# y=0.0023035(1000)+3.8
# y=6.1035 % 
#Q2.B.2
# y=0.0023035x+3.8
# (8)=0.0023035x+3.8
# x=1823.31 Ppl/SqKm
#Q2.C
r_value <- cor(PplPerSqKm,PercentagePositive)
#The r value shows moderate and positive correlation between strength and temperature as .657 falls within the 0.5-0.8 range.

#part E
rsquared_value <- (r_value*r_value)
#The r squared value shows a 43% variability that can be attributed to the linear relationship between population density and total infections. 

#Q2.E
qqnorm(PercentagePositive)
qqline(PercentagePositive)
#The data shown in the QQ norm plot displays a linear curve providing confidence that population density is correlated with the total covid infection count. 


#Question 3
#A. i Margin of Error

n_percentage = nrow(covidPopDensityData) #Find population size
X_percentage = mean(PercentagePositive) #Find Population mean

sample_var_percentage = var(PercentagePositive) #Finds the sample variance
sample_sd_percentage = sd(PercentagePositive) #Finds the sample standard deviation

#For Z, we need POPULATION var/sd, so we will switch using formulas:
population_var_percentage = (((n_percentage-1)/n_percentage)*sample_var_percentage)
population_sd_percentage = sqrt(population_var_percentage)

#Find Critical Value
alpha_95 = 0.05 #95% = (1-alpha)*100%
z_critical_value_95 = qnorm(1-(alpha_95/2))

#Find Margin of Error
z_ME_95 = z_critical_value_95 * (population_sd_percentage/(sqrt(n_percentage)))

#A. ii FIND 95% CI
lower_bound_z_95 = X_percentage - z_ME_95
upper_bound_z_95 = X_percentage + z_ME_95

#A. iii Interpretation
cat("The 95% confidence interval for the true population mean Percentage that tested positive  is: (", 
    lower_bound_z_95,
    ", ",
    upper_bound_z_95,
    ").")

#The 95% confidence interval for the true population mean Percentage that tested positive  is: ( 4.21067 ,  5.547157 )


#B. i Margin of Error

n_popdensity = nrow(covidPopDensityData) #Find population size
X_popdensity = mean(PplPerSqMi) #Find Population mean

sample_var_popdensity = var(PplPerSqMi) #Finds the sample variance
sample_sd_popdensity = sd(PplPerSqMi) #Finds the sample standard deviation

#For Z, we need POPULATION var/sd, so we will switch using formulas:
population_var_popdensity = (((n_popdensity-1)/n_popdensity)*sample_var_popdensity)
population_sd_popdensity = sqrt(population_var_popdensity)

#Find Critical Value
alpha_95 = 0.05 #95% = (1-alpha)*100%
z_critical_value_95 = qnorm(1-(alpha_95/2))

#Find Margin of Error
z_ME_95 = z_critical_value_95 * (population_sd_popdensity/(sqrt(n_popdensity)))

#B. ii FIND 95% CI
lower_bound_z_95 = X_popdensity - z_ME_95
upper_bound_z_95 = X_popdensity + z_ME_95

#B. iii Interpretation
cat("The 95% confidence interval for the true population mean population density  is: (", 
    lower_bound_z_95,
    ", ",
    upper_bound_z_95,
    ").")

#The 95% confidence interval for the true population mean population density  is: ( 183.522 ,  1002.442 ).




