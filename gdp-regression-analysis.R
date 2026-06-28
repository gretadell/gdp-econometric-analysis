##ASSIGNMENT 1


#PART 1 
#We import the dataset from the World Bank, from Excel.
gdppc <- gdp_per_capita #gdp per capita
sav_rate <- gross_savings #gross savings as percentage of gdp 
pop_growth <- population_growth_ #population growth (annual %)

#we proceed by creating our final dataset 

{
  library(data.table)
  #transform in format data.table 
  gdp_dt <- as.data.table(gdppc)
  saving_rate_dt <- as.data.table(sav_rate)
  population_growth_dt <- as.data.table(pop_growth)
  
  melted_gdp <- melt(gdp_dt)
  melted_sav_rate <- melt(saving_rate_dt)
  melted_pop_growth <- melt(population_growth_dt)
  
  # Merge all melted data.tables
  data_dt <- merge(melted_gdp, melted_sav_rate, by = c("Country Name", "variable"), suffixes = c(".GDP", ".Savings"))
  data_dt <- merge(data_dt, melted_pop_growth, by = c("Country Name", "variable"), suffixes = c("", ".Population"))
  
  # Rename columns
  setnames(data_dt, old = c("value.GDP", "value.Savings", "value"), 
           new = c("GDP per capita (current US$)", "Gross savings (% of GDP)", 
                   "Population growth (annual %)"))
  
  # Remove unnecessary columns
  data_dt[, c("Indicator Name.GDP", "Indicator Name.Savings", "Indicator Name") := NULL]
  
  # Compute logarithms
  data_dt[, `Log GDP per capita` := log(`GDP per capita (current US$)`)]
  data_dt[, `Log Gross savings` := log(`Gross savings (% of GDP)`)]
  data_dt[, `Log Population growth` := log(`Population growth (annual %)`)]
  
}

# View the resulting data table
View(data_dt)

objects_to_keep <- c("data_dt")
objects_to_remove <- setdiff(ls(), objects_to_keep)
rm(list = objects_to_remove)
rm("objects_to_remove")

#Then we removed from the data set all those data points that are not countries' data points but
#are representative of broader areas. We removed these elements to avoid 
#potential multicollinearity problems 

elements_to_remove <- c("Africa Eastern and Southern", "Africa Western and Central", "Arab World", "East Asia & Pacific (excluding high income)", "Early-demographic dividend", 
                        "East Asia & Pacific","East Asia & Pacific (IDA & IBRD countries)"
                        , "Europe & Central Asia (excluding high income)", "Europe & Central Asia
", "Euro area
", "European Union
", "Fragile and conflict affected situations
", "Heavily indebted poor countries (HIPC)
", "IBRD only
", "IDA & IBRD total
", "IDA total
", "IDA blend
", "IDA only
", "Not classified
", "Latin America & Caribbean (excluding high income)
", "Latin America & Caribbean
", "Least developed countries: UN classification
", "Low income
", "Lower middle income
", "Low & middle income
", "Late-demographic dividend
", "Middle East & North Africa
", "Middle income
", "Middle East & North Africa (excluding high income)
", "OECD members
", "Other small states
", "Pre-demographic dividend
", "Pacific island small states
", "Post-demographic dividend
", "Sub-Saharan Africa (excluding high income)
", "Sub-Saharan Africa
", "Small states
","Europe & Central Asia (IDA & IBRD countries)
", "Latin America & the Caribbean (IDA & IBRD countries)
", "Middle East & North Africa (IDA & IBRD countries)
", "South Asia (IDA & IBRD)
", "Sub-Saharan Africa (IDA & IBRD countries)
", "Upper middle income
", "World") 
#Now we remove all the elements above
data_dt <- data_dt[!data_dt$"Country Name" %in% elements_to_remove, ]

#FIRST LINEAR REGRESSION 
#We run the regression where: 
# - GDP per capita is the dependent variable 
# - Gross savings and Population growth are the independent variables 
#We decided to apply the Log transformation to these variables, for its many 
#advantages (improvements in: homoscedasticity, linearity, normality distributions..)

#we run the first regression: 
model <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth`, data = data_dt)
summary(model)

# Results of the regression: all the variables are significant, (low p value of the t-test)
# R-squared adjusted: 0.3085 

#We made two attempts to remove the data that worsen our regression
# 1. Removing data whose residual are outliers 
# 2. Removing data with high leverage 


#1. Now we decided to remove all those data points whose residuals are outliers, in
#this way we are not removing countries or years but just some outlier observations 

#Analysis of residuals 
residuals <- rstandard(model)
summary(residuals)
outliers <- boxplot(residuals)$out #identification of residuals that are outliers 
outlier_indices <- as.numeric(names(outliers))

outliers_data_dt <- data_dt[outlier_indices, ]
filtered_data_dt <- data_dt[-outlier_indices, ] #we remove the outliers observations from our dataset 

#We run the regression on the filtered dataset 
model_filtered <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth`, data = filtered_data_dt)
summary(model_filtered)

# Results of the regression: all the variables are significant, (low p value of the t-test)
# R-squared adjusted: 0.3304

#2. Leverage: it measures how far an observed value is from the mean of the data. 
#Observations with high leverage can potentially distort the model's parameter estimates.

#leverage 
leverage <- hatvalues(model) #to get the leverage values
summary(leverage)

treshold  = mean(leverage) + 2*sd(leverage) #we chose this as a treshold to understand 
#when an element  has a too high leverage 
high_leverage_indices <- as.numeric(names(leverage[leverage > treshold])) #we select the observations with high leverage 

high_leverage_data_dt <- data_dt[high_leverage_indices , ]
leverage_filtered_data_dt <- data_dt[- high_leverage_indices , ]#remove data with high leverage
View(leverage_filtered_data_dt)

model_filtered_leverage <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth`, data = leverage_filtered_data_dt)
summary(model_filtered_leverage)
#Results of the regression: all the variables are significant, (low p value of the t-test)
# R-squared adjusted: 0.3395

#We run an analysis to compare the results obtained from the initial data set 
#and the ones from the filtered data sets

#Analysis of the residuals

#Residuals from the initial data set - normality distribution
histogram <- hist(residuals, breaks = 200, main = "Residuals Distribution", xlab = "Residuals", ylab = "Frequency", col = "lightblue", border = "black")

mu <- mean(residuals)
sigma <- sd(residuals)
xfit <- seq(min(residuals), max(residuals), length = 200)
yfit <- dnorm(xfit, mean = mu, sd = sigma) * length(residuals) * diff(histogram$breaks[1:2])
lines(xfit, yfit, col = "red", lwd = 2)#overlay normal distribution

#The fit to the normal curve is decent, but there are some notable deviations, 
#especially at the tails.

#Residuals from the filtered (residuals) data set - normality distribution
residuals_filtered <- rstandard(model_filtered) #residuals from the filtered model 
summary(residuals_filtered)
histogram_filtered <- hist(residuals_filtered, breaks = 200, main = "Residuals Distribution, residual filtered model", xlab = "Residuals", ylab = "Frequency", col = "lightblue", border = "black")

mu_filtered <- mean(residuals_filtered)
sigma_filtered <- sd(residuals_filtered)
xfit_filtered <- seq(min(residuals_filtered), max(residuals_filtered), length = 200)
yfit_filtered <- dnorm(xfit_filtered, mean = mu_filtered, sd = sigma_filtered) * length(residuals_filtered) * diff(histogram_filtered$breaks[1:2])
lines(xfit_filtered, yfit_filtered, col = "red", lwd = 2) #overlay normal distribution

#The fit to the normal curve is quite good, and the distribution appears more 
#compact and closer to the normal curve than in the first model

#Residuals from the filtered data set by leverage - normality distribution
residuals_filtered_leverage <- rstandard(model_filtered_leverage) #residuals from the filtered (leverage) model 
summary(residuals_filtered_leverage)
histogram_filtered_leverage <- hist(residuals_filtered_leverage, breaks = 200, main = "Residuals Distribution, leverage filtered model", xlab = "Residuals", ylab = "Frequency", col = "lightblue", border = "black")

mu_filtered_lev<- mean(residuals_filtered_leverage)
sigma_filtered_lev <- sd(residuals_filtered_leverage)
xfit_filtered_lev <- seq(min(residuals_filtered_leverage), max(residuals_filtered_leverage), length = 200)
yfit_filtered_lev <- dnorm(xfit_filtered_lev, mean = mu_filtered_lev, sd = sigma_filtered_lev) * length(residuals_filtered_leverage) * diff(histogram_filtered_leverage$breaks[1:2])
lines(xfit_filtered_lev, yfit_filtered_lev, col = "red", lwd = 2)#overlay normal distribution

#The fit to the normal curve is generally good, even if there's a slight asymmetry on the right tail

#From this visual analysis of the normality of the residuals, we notice that all of the residuals
#of the three models approximately follow a normal distribution.

#We analyze the linearity of the models: 

plot(model, which = 1, main = "Linearity of the model")
plot(model_filtered, which = 1,main = "Linearity of the residuals filtered model" )
plot(model_filtered_leverage, which = 1, main = "Linearity of the residuals filtered model")

#The analysis of these graph suggests that the last model is the one that respects 
#the assumption of linearity in the best way, even though neither graph 
#appears to fully satisfy the assumption of linearity, suggesting
#that further investigation may be necessary or more complex models might be required.

#Homoscedasticity of the models 

plot(model, which = 3, main = "homoscedasticity of the model")
plot(model_filtered, which = 3,  main = "homoscedasticity of the residuals filtered model")
plot(model_filtered_leverage, which = 3, main  = "homoscedasticity of the residuals filtered model")

#The first two graphs appear quite similar, in both graphs there's a noticeable 
#pattern where the residuals fan out as the fitted values increase, indicating potential heteroscedasticity. 
#The leverage filtered model seems to respect more the assumption of homoscedasticity than
#the other two models, even if it has a certain degree of heteroscedasticity

#Comparison of the normality

plot(model, which = 2, main = "normality of the model")
plot(model_filtered, which = 2,  main = "normality of the residuals filtered model")
plot(model_filtered_leverage, which = 2, main = "normality of the leverage filtered model")

#The first Normal Q-Q plot, the points deviate from the reference line, particularly 
#in the tails. This indicates that the residuals have heavier tails than would be 
#expected in a normal distribution. This effect is less pronounced with the residuals 
#filtered model and improves even more in the last model

#Comparison of Fitted values - real values
#In the following analysis, we plot three graphs (one for each data set). 
#The objective of our analysis is visually understanding if the fitted values obtained
#from a model are close to the actual data we have on GDP per capita

data_dt_clean <- na.omit(data_dt) #removal of lines with missing data 
model_clean <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth`, data = data_dt_clean)
predicted_values <- model_clean$fitted.values

par(cex = 0.8)
{
  # Plot of expected data and real data 
  plot(data_dt_clean$`Log GDP per capita`, predicted_values, 
       xlab = "Log GDP per capita", ylab = "Fitted value", 
       main = "Real value vs Fitted value",
       ylim = c(5, 12), xlim = c(5, 12)
  )
  
  abline(0, 1, col = "red") # 45 degree line 
  
}


filtered_data_dt_clean <- na.omit(filtered_data_dt)
model_filtered_clean <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth`, data = filtered_data_dt_clean)
filtered_predicted_values <- model_filtered_clean$fitted.values

par(cex = 0.8)
{
  # Plot of expected data and real data
  plot(filtered_data_dt_clean$`Log GDP per capita`, filtered_predicted_values, 
       xlab = "Log GDP per capita", ylab = "Fitted value", 
       main = "Real value vs Fitted value",
       ylim = c(5, 12), xlim = c(5, 12)
  )
  
  abline(0, 1, col = "red") # 45 degree line
  
}

leverage_filtered_data_dt_clean <- na.omit(leverage_filtered_data_dt)
model_leverage_filtered_clean <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth`, data = leverage_filtered_data_dt_clean)
leverage_filtered_predicted_values <- model_leverage_filtered_clean$fitted.values

par(cex = 0.8)
{
  # Plot of expected data and real data
  plot(leverage_filtered_data_dt_clean$`Log GDP per capita`, leverage_filtered_predicted_values, 
       xlab = "Log GDP per capita", ylab = "Fitted value", 
       main = "Real value vs Fitted value",
       ylim = c(5, 12), xlim = c(5, 12)
  )
  
  abline(0, 1, col = "red") # 45 degree line
  
}

#From this visual analysis we can conclude that none of the models show a perfect 
# alignment with the diagonal, but this is common in real-world data. 
#Overall,  the third graph seems to have a slightly better concentration of points
#around the line, especially in the middle range of Log GDP per capita values, 
#in fact this model is the one with the highest R^2. 

#FINAL  OBSERVATIONS: 
#Even  if the residual-filtering method improves some of the elements of our model 
# We concluded that the most efficient method is the one that removes values with 
#high leverage, in this way our model has an higher coefficient of determination and 
#approximately respects the assumptions of an OLS model

#PART 2 

#In this stage of our analysis, we decided to add some variables that we thought 
#could have an impact in the determination of gdp per capita: 

#variables we decided to analyze: 
RD <- RD_expenses #research and development expenses, as percentage of gdp
edu_expenditure <- educational_expenditure #educational expenditure, as percentage of gdp
rule_of_law <- rule_of_law 
#Rule of law captures perceptions of the extent to which agents have confidence
#in and abide by the rules of society, and in particular the quality of contract
#enforcement, property rights (etc..). 

#We create a new data set that contains also these variables 

{
  #transform in format data.table 
  RD_dt <- as.data.table(RD) 
  edu_expenditure_dt <- as.data.table(edu_expenditure)
  rule_of_law_dt <- as.data.table(rule_of_law)
  
  #Now we remove all the non-country elements as before
  RD_dt <- RD_dt[!RD_dt$"Country Name" %in% elements_to_remove, ]
  edu_expenditure_dt <- edu_expenditure_dt[!edu_expenditure_dt$"Country Name" %in% elements_to_remove, ]
  rule_of_law_dt <- rule_of_law_dt[!rule_of_law_dt$"Country Name" %in% elements_to_remove, ]
  
  melted_RD <- melt(RD_dt) 
  melted_edu_expenditure <- melt(edu_expenditure_dt)
  melted_rule_of_law <- melt(rule_of_law_dt)
  
  
  # Merge all melted data.tables
  data_dt2 <- data_dt
  data_dt2 <- merge(data_dt2, melted_RD, by = c("Country Name", "variable"), suffixes = c("", ".RD"))
  data_dt2 <- merge(data_dt2, melted_edu_expenditure, by = c("Country Name", "variable"), suffixes = c("", ".Education"))
  data_dt2 <- merge(data_dt2, melted_rule_of_law, by = c("Country Name", "variable"), suffixes = c("", ".RuleOfLaw"))
  
  # Rename columns
  setnames(data_dt2, old = c("value", "value.Education", "value.RuleOfLaw"), 
           new = c("R&D expenses (% of GDP)", 
                   "Government expenditure on education, total (% of GDP)",
                   "Rule of law"))
  
  # Remove unnecessary columns
  data_dt2[, c("Indicator Name", "Indicator Name.Education", "Indicator Name.RuleOfLaw") := NULL]
  
  # Compute logarithms
  data_dt2[, `Log RD expenses` := log(`R&D expenses (% of GDP)`)] 
  data_dt2[, `Log Education Expenditure` := log(`Government expenditure on education, total (% of GDP)`)] 
  
  # View the resulting data table
  View(data_dt2)
}

#We created seven models, each model contains a different combination of the variables we 
#added. Then we ran a regression for each model. 
#These are the variables we added to the standard model(gross savings and population growth rate) 
#in each model: 
#1: RD
#2: Educational expenditure 
#3: Rule of law
#4: RD + educational expenditure 
#5: RD + Rule Of Law 
#6: Educational expenditure + Rule Of Law
#7: RD+ Educational expenditure + Rule of law 

#we run the regression for each model: 

model1 <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth` + `R&D expenses (% of GDP)`, data = data_dt2)
summary(model1)
#R^2 adj: 0.447, all variables significant 

model2 <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth` + `Government expenditure on education, total (% of GDP)`, data = data_dt2)
summary(model2)
#R^2 adj: 0.3766, all variables significant 

model3 <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth` + `Rule of law`, data = data_dt2)
summary(model3)
#R^2 adj: 0.665, all variables significant 

model4 <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth` + `R&D expenses (% of GDP)`  + `Government expenditure on education, total (% of GDP)`, data = data_dt2)
summary(model4)
#R^2 adj: 0.507, all variables significant 

model5 <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth` + `R&D expenses (% of GDP)`  + `Rule of law`, data = data_dt2)
summary(model5)
#R^2 adj: 0.69, all variables significant 

model6 <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth` + `Government expenditure on education, total (% of GDP)`  + `Rule of law`, data = data_dt2)
summary(model6)
#R^2 adj: 0.71, expenditure on education not significant  

model7 <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth` + `Government expenditure on education, total (% of GDP)`  + `Rule of law`  + `R&D expenses (% of GDP)`, data = data_dt2)
summary(model7)
#R^2 adj: 0.73, educational expenditure not significant 

#We decided to use a log-level type of model for the three new variables as it showed better results
#Looking at the Adjusted R^2, the models 6 (0.7117) and 7 (0.7302) appear to have 
#the highest value. However, the p-value of the t-test of Education Expenditure 
#in both model is quite high, hence the non-significance hypothesis isn't rejected. 
#Therefore, we will exlcude them from our analyis. 
#After those two, the model with highest adjusted R^2 is the fifth (0.6967), where all the 
#non-significance hypotheses are rejected at a 0.1% significance level.

#As before, we decided to test the OLS hypothesis for the first five models, we 
#decided to not considerate model 6 and 7 since they have some variables that 
#are insignificant with a quite high probability. 

#Residual analysis 

residuals1 <- rstandard(model1) #residuals from model 1
histogram1 <- hist(residuals1, breaks = 200, main = "Residuals Distribution, model 1", xlab = "Residuals", ylab = "Frequency", col = "lightblue", border = "black")
mu1 <- mean(residuals1)
sigma1 <- sd(residuals1)
xfit1 <- seq(min(residuals1), max(residuals1), length = 200)
yfit1 <- dnorm(xfit1, mean = mu1, sd = sigma1) * length(residuals1) * diff(histogram1$breaks[1:2])
lines(xfit1, yfit1, col = "red", lwd = 2)
#Clear deviations from a normal distribution: lack of symmetry and presence of outliers 
#Normality assumption is not met 

residuals2 <- rstandard(model2) #residuals from model 2 
histogram2 <- hist(residuals2, breaks = 200, main = "Residuals Distribution, model 2", xlab = "Residuals", ylab = "Frequency", col = "lightblue", border = "black")
mu2 <- mean(residuals2)
sigma2 <- sd(residuals2)
xfit2 <- seq(min(residuals2), max(residuals2), length = 200)
yfit2 <- dnorm(xfit2, mean = mu2, sd = sigma2) * length(residuals2) * diff(histogram2$breaks[1:2])
lines(xfit2, yfit2, col = "red", lwd = 2)
#This model better respects the assumption of normality than the previous one, 
#however there are many imperfections 

residuals3 <- rstandard(model3) #residuals from  model 3
histogram3 <- hist(residuals3, breaks = 200, main = "Residuals Distribution, model 3", xlab = "Residuals", ylab = "Frequency", col = "lightblue", border = "black")
mu3 <- mean(residuals3)
sigma3 <- sd(residuals3)
xfit3 <- seq(min(residuals3), max(residuals3), length = 200)
yfit3 <- dnorm(xfit3, mean = mu3, sd = sigma3) * length(residuals3) * diff(histogram3$breaks[1:2])
lines(xfit3, yfit3, col = "red", lwd = 2)
#Residuals seem to follow a fairly normal distribution, even though the graph appears slightly skewed 

residuals4 <- rstandard(model4) #residuals from model 4
histogram4 <- hist(residuals4, breaks = 200, main = "Residuals Distribution, model 4", xlab = "Residuals", ylab = "Frequency", col = "lightblue", border = "black")
mu4 <- mean(residuals4)
sigma4 <- sd(residuals4)
xfit4 <- seq(min(residuals4), max(residuals4), length = 200)
yfit4 <- dnorm(xfit4, mean = mu4, sd = sigma4) * length(residuals4) * diff(histogram4$breaks[1:2])
lines(xfit4, yfit4, col = "red", lwd = 2)
# From the graph we notice several deviations from normality, including possible 
# and irregular tails. This suggests that the assumptions for normality might not be fully met. 

residuals5 <- rstandard(model5) #residuals from model 5
histogram5 <- hist(residuals5, breaks = 200, main = "Residuals Distribution, model 5", xlab = "Residuals", ylab = "Frequency", col = "lightblue", border = "black")
mu5 <- mean(residuals5)
sigma5 <- sd(residuals5)
xfit5 <- seq(min(residuals5), max(residuals5), length = 200)
yfit5 <- dnorm(xfit5, mean = mu5, sd = sigma5) * length(residuals5) * diff(histogram5$breaks[1:2])
lines(xfit5, yfit5, col = "red", lwd = 2)

#The gistogram suggets a quite symmetric distribution, that the residuals from model 5 
#may be close to normal, with some possible exceptions in the tails residuals 

# Analysis of the linearity for each model

plot(model1, which = 1, main = "Linearity of model 1")
plot(model2, which = 1, main = "Linearity of model 2")
plot(model3, which = 1, main = "Linearity of model 3")
plot(model4, which = 1, main = "Linearity of model 4")
plot(model5, which = 1, main = "Linearity of model 5")

#From this graphical analysis model 3 and model 5 are those that better respect
#the assumptions of linearity 

#Analysis of the homoscedasticity for each model 

plot(model1, which = 3, main = "Homoscedasticity of model 1")
plot(model2, which = 3, main = "Homoscedasticity of model 2")
plot(model3, which = 3, main = "Homoscedasticity of model 3")
plot(model4, which = 3, main = "Homoscedasticity of model 4")
plot(model5, which = 3, main = "Homoscedasticity of model 5")

#The models that better respect the homoscedasticity assumption are the third 
#and the fifth 

#Analysis of the normality for each model 

plot(model1, which = 2, main = "Normality of model 1")
plot(model2, which = 2, main = "Normality of model 2")
plot(model3, which = 2, main = "Normality of model 3")
plot(model4, which = 2, main = "Normality of model 4")
plot(model5, which = 2, main = "Normality of model 5")

#The models that better respect the normality assumptions are model 2, model 3 and model 5

#From the previous analysis we concluded that model 3( rule of law) and model 5 
#(RD expenses and rule of law) are the ones that better explain GDP per capita. Now we are going 
#to filter this data with the leverage method, as we did in part one, and test 
#the results obtained from this new model 

#As in part 1, we exclude data whose leverage is too high

#MODEL 3 
leverage3 <- hatvalues(model3) #to get the leverage values
summary(leverage3)
treshold3  = mean(leverage3) + 2*sd(leverage3) #we chose this as a treshold
high_leverage_indices3 <- as.numeric(names(leverage3[leverage3 > treshold3])) #we select the observations with high leverage 

high_leverage_data_dt3 <- data_dt2[hih_leverage_indices3, ]
leverage_filtered_data_dt3 <- data_dt2[- high_leverage_indices3, ]#remove data with high leverage
View(leverage_filtered_data_dt3)

model_filtered_leverage3 <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth` + `Rule of law`, data = leverage_filtered_data_dt3) 
summary(model_filtered_leverage3)
#Results from the regression. R^2 adjusted: 0.667, all variables are significant 


#MODEL 5
leverage5 <- hatvalues(model5) #to get the leverage values
summary(leverage5)
treshold5  = mean(leverage5) + 2*sd(leverage5) #we chose this as a treshold 
high_leverage_indices5 <- as.numeric(names(leverage5[leverage5 > treshold5])) #we select the observations with high leverage 

high_leverage_data_dt5 <- data_dt2[high_leverage_indices5, ]
leverage_filtered_data_dt5 <- data_dt2[- high_leverage_indices5, ]#remove data with high leverage
View(leverage_filtered_data_dt5)

model_filtered_leverage5 <- lm(`Log GDP per capita` ~ `Log Gross savings` + `Log Population growth` + `R&D expenses (% of GDP)` + `Rule of law`, data = leverage_filtered_data_dt5) 
summary(model_filtered_leverage5)
#Results from the regression: R^2 adjusted: 0.6902, the R&D expenses variable has 
#a lower significance level(non-significant with probability 0.001), all the other variables 
#have the same significance as before

#As before, we compare the OLS assumptions as before 

#Normality of residuals: graphical analysis

residuals_filtered_leverage3 <- rstandard(model_filtered_leverage3) #residuals from the filtered (leverage) model 
histogram_filtered_leverage3 <- hist(residuals_filtered_leverage3, breaks = 200, main = "Residuals Distribution", xlab = "Residuals", ylab = "Frequency", col = "lightblue", border = "black")
mu_filtered_leverage3 <- mean(residuals_filtered_leverage3)
sigma_filtered_leverage3 <- sd(residuals_filtered_leverage3)
xfit_filtered_leverage3 <- seq(min(residuals_filtered_leverage3), max(residuals_filtered_leverage3), length = 200)
yfit_filtered_leverage3 <- dnorm(xfit_filtered_leverage3, mean = mu_filtered_leverage3, sd = sigma_filtered_leverage3) * length(residuals_filtered_leverage3) * diff(histogram_filtered_leverage3$breaks[1:2])
lines(xfit_filtered_leverage3, yfit_filtered_leverage3, col = "red", lwd = 2)

residuals_filtered_leverage5 <- rstandard(model_filtered_leverage5) #residuals from the filtered (leverage) model 
histogram_filtered_leverage5 <- hist(residuals_filtered_leverage5, breaks = 200, main = "Residuals Distribution", xlab = "Residuals", ylab = "Frequency", col = "lightblue", border = "black")
mu_filtered_leverage5 <- mean(residuals_filtered_leverage5)
sigma_filtered_leverage5 <- sd(residuals_filtered_leverage5)
xfit_filtered_leverage5 <- seq(min(residuals_filtered_leverage5), max(residuals_filtered_leverage5), length = 200)
yfit_filtered_leverage5 <- dnorm(xfit_filtered_leverage5, mean = mu_filtered_leverage5, sd = sigma_filtered_leverage5) * length(residuals_filtered_leverage5) * diff(histogram_filtered_leverage5$breaks[1:2])
lines(xfit_filtered_leverage5, yfit_filtered_leverage5, col = "red", lwd = 2)

#In both cases the histograms suggest that while the residuals may be approximately
#normally distributed, especially around the mean, there may be some issues with 
#the distribution's tails.

#We analyze the linearity of the model: 
plot(model_filtered_leverage3, which = 1, main = "linearity of the new model")
plot(model_filtered_leverage5, which = 1, main = "linearity of the new model")

#homoscedasticity
plot(model_filtered_leverage3, which = 3, main = "homoscedasticity of the model")
plot(model_filtered_leverage5, which = 3, main = "homoscedasticity of the model")

#normality
plot(model_filtered_leverage3, which = 2, main = "normality of the model")
plot(model_filtered_leverage5, which = 2, main = "normality of the model")

#From the analysis of these three sets of graphs, both models seems to well 
#respcet these three assumptions, even if there may be small differences between
#the two 

#In conclusion these two models are the ones that in our analysis explain better 
#the GDP per capita, even when we filtered data according to the leverage method. 
#In general there is an association (positive) between GDP per capita and both 
#RD expenses and Rule of law. This findings confirm our economic theory: 
#- R&D expenses can boost GDP per capita by driving innovation, productivity, 
#and creating high-quality jobs that contribute to economic growth.
#- If people believe that the institutions of a country work well and are trust-worthy, 
# economic agents will make investment in those countries and this will benefit 
# the country's GDP 

