setwd("C:\\Users\\Utente\\OneDrive - unibs.it\\Desktop\\Uni\\EAD\\tesi EAD")
getwd()

# libraries
library(readxl)
library(fixest)
library(modelsummary)
library(dplyr)
library(e1071)
library(lattice)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(grid)
library(kableExtra)
library(webshot2)

# data
data<-read_excel("thesis.xlsx",sheet='data')
# View(data)

data = 
  data %>% 
  mutate(
    log_emp = log(emp),
    log_ict = log(ict),
    log_val_add = log(val_add),
    log_lab_prod = log(lab_prod)
  )

#

# summary statistics with levels #### 

#data %>% str
#data %>% summary()

## mean
mean_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(mean_ict = mean(ict))

mean_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(mean_ict = mean(ict))

mean_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(mean_emp = mean(emp))

mean_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(mean_emp = mean(emp))

mean_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(mean_ict = mean(ict))

mean_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(mean_emp = mean(emp))

## median
median_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(median_ict = median(ict))

median_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(median_ict = median(ict))

median_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(median_emp = median(emp))

median_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(median_emp = median(emp))

median_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(median_ict = median(ict))

median_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(median_emp = median(emp))

## variance
var_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(var_ict = var(ict))

var_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(var_ict = var(ict))

var_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(var_emp = var(emp))

var_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(var_emp = var(emp))

var_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(var_ict = var(ict))

var_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(var_emp = var(emp))

## standard deviation
sd_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(sd_ict = sd(ict))

sd_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(sd_ict = sd(ict))

sd_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(sd_emp = sd(emp))

sd_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(sd_emp = sd(emp))

sd_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(sd_ict = sd(ict))

sd_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(sd_emp = sd(emp))

## range
range_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(range_ict = max(ict)-min(ict))

range_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(range_ict = max(ict)-min(ict))

range_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(range_emp = max(emp)-min(emp))

range_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(range_emp = max(emp)-min(emp))

range_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(range_ict = max(ict)-min(ict))

range_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(range_emp = max(emp)-min(emp))

## skewness
skew_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(skew_ict = skewness(ict))

skew_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(skew_ict = skewness(ict))

skew_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(skew_emp = skewness(emp))

skew_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(skew_emp = skewness(emp))

skew_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(skew_ict = skewness(ict))

skew_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(skew_emp = skewness(emp))

## kurtosis
kurtosis_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(kurtosis_ict = kurtosis(ict))

kurtosis_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(kurtosis_ict = kurtosis(ict))

kurtosis_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(kurtosis_emp = kurtosis(emp))

kurtosis_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(kurtosis_emp = kurtosis(emp))

kurtosis_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(kurtosis_ict = kurtosis(ict))

kurtosis_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(kurtosis_emp = kurtosis(emp))

  ## ict/gdp
mean_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(mean_ict = mean(ict_gdp))

median_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(median_ict = median(ict_gdp))

var_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(var_ict = var(ict_gdp))

sd_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(sd_ict = sd(ict_gdp))

range_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(range_ict = max(ict_gdp)-min(ict_gdp))

skew_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(skew_ict = skewness(ict_gdp))

kurtosis_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(kurtosis_ict = kurtosis(ict_gdp))

  ##employment rate (emp_rate)
mean_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(mean_emp = mean(emp_rate))

median_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(median_emp = median(emp_rate))

var_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(var_emp = var(emp_rate))

sd_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(sd_emp = sd(emp_rate))

range_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(range_emp = max(emp_rate)-min(emp_rate))

skew_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(skew_emp = skewness(emp_rate))

kurtosis_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(kurtosis_emp = kurtosis(emp_rate))


# ? scatterplot
#  xyplot( data$emp ~  data$ict  | data$country,
#          data=data,
#           main="Scatterplot of emp and ict, stratified by country")

# ? plot(data$ict, 
#      data$emp, 
#      type = "p",
#      main = "A Scatterplot of X and Y",
#      xlab = "ict",
#      ylab = "emp",
#      col = data$country)

  ## data per country
# ict_per_country <- mean_ict_per_country %>%
#  left_join(mean_emp_per_country, by = "country") %>%
#  left_join(median_ict_per_country, by = "country") %>%
#  left_join(median_emp_per_country, by = "country") %>%
#  left_join(var_ict_per_country, by = "country") %>%
#  left_join(var_emp_per_country, by = "country") %>%
#  left_join(sd_ict_per_country, by = "country") %>%
#  left_join(sd_emp_per_country, by = "country") %>%
#  left_join(range_ict_per_country, by = "country") %>%
#  left_join(range_emp_per_country, by = "country") %>%
#  left_join(skew_ict_per_country, by = "country") %>%
#  left_join(skew_emp_per_country, by = "country") %>%
#  left_join(kurtosis_ict_per_country, by = "country") %>%
#  left_join(kurtosis_emp_per_country, by = "country")
# View(per_country)

emp_per_country <- mean_emp_per_country %>%
    left_join(median_emp_per_country, by = "country") %>%
    left_join(var_emp_per_country, by = "country") %>%
    left_join(sd_emp_per_country, by = "country") %>%
    left_join(range_emp_per_country, by = "country") %>%
    left_join(skew_emp_per_country, by = "country") %>%
    left_join(kurtosis_emp_per_country, by = "country")

ict_per_country <- mean_ict_per_country %>%
  left_join(median_ict_per_country, by = "country") %>%
  left_join(var_ict_per_country, by = "country") %>%
  left_join(sd_ict_per_country, by = "country") %>%
  left_join(range_ict_per_country, by = "country") %>%
  left_join(skew_ict_per_country, by = "country") %>%
  left_join(kurtosis_ict_per_country, by = "country")

  ## data per year
emp_per_year <- mean_emp_per_year %>%
    left_join(median_emp_per_year, by = "year") %>%
    left_join(var_emp_per_year, by = "year") %>%
    left_join(sd_emp_per_year, by = "year") %>%
    left_join(range_emp_per_year, by = "year") %>%
    left_join(skew_emp_per_year, by = "year") %>%
    left_join(kurtosis_emp_per_year, by = "year")

ict_per_year <- mean_ict_per_year %>%
  left_join(median_ict_per_year, by = "year") %>%
  left_join(var_ict_per_year, by = "year") %>%
  left_join(sd_ict_per_year, by = "year") %>%
  left_join(range_ict_per_year, by = "year") %>%
  left_join(skew_ict_per_year, by = "year") %>%
  left_join(kurtosis_ict_per_year, by = "year")

  ## data per economic activity

emp_per_econ_act <- mean_emp_per_econ_act %>%
  left_join(median_emp_per_econ_act, by = "econ_act") %>%
  left_join(var_emp_per_econ_act, by = "econ_act") %>%
  left_join(sd_emp_per_econ_act, by = "econ_act") %>%
  left_join(range_emp_per_econ_act, by = "econ_act") %>%
  left_join(skew_emp_per_econ_act, by = "econ_act") %>%
  left_join(kurtosis_emp_per_econ_act, by = "econ_act")

ict_per_econ_act <- mean_ict_per_econ_act %>%
  left_join(median_ict_per_econ_act, by = "econ_act") %>%
  left_join(var_ict_per_econ_act, by = "econ_act") %>%
  left_join(sd_ict_per_econ_act, by = "econ_act") %>%
  left_join(range_ict_per_econ_act, by = "econ_act") %>%
  left_join(skew_ict_per_econ_act, by = "econ_act") %>%
  left_join(kurtosis_ict_per_econ_act, by = "econ_act")

View(emp_per_country)
#View(ict_per_country)
#View(emp_per_year)
#View(ict_per_year)
#View(emp_per_econ_act)
#View(ict_per_econ_act)


# summary statistics with ict_gdp and emp_rate #### 

## mean
mean_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(mean_ict = mean(ict_gdp))

mean_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(mean_ict = mean(ict_gdp))

mean_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(mean_emp = mean(emp_rate))

mean_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(mean_emp = mean(emp_rate))

mean_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(mean_ict = mean(ict_gdp))

mean_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(mean_emp = mean(emp_rate))

## median
median_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(median_ict = median(ict_gdp))

median_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(median_ict = median(ict_gdp))

median_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(median_emp = median(emp_rate))

median_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(median_emp = median(emp_rate))

median_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(median_ict = median(ict_gdp))

median_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(median_emp = median(emp_rate))

## variance
var_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(var_ict = var(ict_gdp))

var_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(var_ict = var(ict_gdp))

var_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(var_emp = var(emp_rate))

var_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(var_emp = var(emp_rate))

var_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(var_ict = var(ict_gdp))

var_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(var_emp = var(emp_rate))

## standard deviation
sd_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(sd_ict = sd(ict_gdp))

sd_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(sd_ict = sd(ict_gdp))

sd_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(sd_emp = sd(emp_rate))

sd_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(sd_emp = sd(emp_rate))

sd_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(sd_ict = sd(ict_gdp))

sd_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(sd_emp = sd(emp_rate))

## range
range_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(range_ict = max(ict_gdp)-min(ict_gdp))

range_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(range_ict = max(ict_gdp)-min(ict_gdp))

range_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(range_emp = max(emp_rate)-min(emp_rate))

range_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(range_emp = max(emp_rate)-min(emp_rate))

range_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(range_ict = max(ict_gdp)-min(ict_gdp))

range_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(range_emp = max(emp_rate)-min(emp_rate))

## skewness
skew_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(skew_ict = skewness(ict_gdp))

skew_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(skew_ict = skewness(ict_gdp))

skew_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(skew_emp = skewness(emp_rate))

skew_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(skew_emp = skewness(emp_rate))

skew_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(skew_ict = skewness(ict_gdp))

skew_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(skew_emp = skewness(emp_rate))

## kurtosis
kurtosis_ict_per_country <- data %>%
  group_by(country) %>%
  summarise(kurtosis_ict = kurtosis(ict_gdp))

kurtosis_ict_per_year <- data %>%
  group_by(year) %>%
  summarise(kurtosis_ict = kurtosis(ict_gdp))

kurtosis_emp_per_country <- data %>%
  group_by(country) %>%
  summarise(kurtosis_emp = kurtosis(emp_rate))

kurtosis_emp_per_year <- data %>%
  group_by(year) %>%
  summarise(kurtosis_emp = kurtosis(emp_rate))

kurtosis_ict_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(kurtosis_ict = kurtosis(ict_gdp))

kurtosis_emp_per_econ_act <- data %>%
  group_by(econ_act) %>%
  summarise(kurtosis_emp = kurtosis(emp_rate))

emp_per_country <- mean_emp_per_country %>%
  left_join(median_emp_per_country, by = "country") %>%
  left_join(var_emp_per_country, by = "country") %>%
  left_join(sd_emp_per_country, by = "country") %>%
  left_join(range_emp_per_country, by = "country") %>%
  left_join(skew_emp_per_country, by = "country") %>%
  left_join(kurtosis_emp_per_country, by = "country")

ict_per_country <- mean_ict_per_country %>%
  left_join(median_ict_per_country, by = "country") %>%
  left_join(var_ict_per_country, by = "country") %>%
  left_join(sd_ict_per_country, by = "country") %>%
  left_join(range_ict_per_country, by = "country") %>%
  left_join(skew_ict_per_country, by = "country") %>%
  left_join(kurtosis_ict_per_country, by = "country")

## data per year
emp_per_year <- mean_emp_per_year %>%
  left_join(median_emp_per_year, by = "year") %>%
  left_join(var_emp_per_year, by = "year") %>%
  left_join(sd_emp_per_year, by = "year") %>%
  left_join(range_emp_per_year, by = "year") %>%
  left_join(skew_emp_per_year, by = "year") %>%
  left_join(kurtosis_emp_per_year, by = "year")

ict_per_year <- mean_ict_per_year %>%
  left_join(median_ict_per_year, by = "year") %>%
  left_join(var_ict_per_year, by = "year") %>%
  left_join(sd_ict_per_year, by = "year") %>%
  left_join(range_ict_per_year, by = "year") %>%
  left_join(skew_ict_per_year, by = "year") %>%
  left_join(kurtosis_ict_per_year, by = "year")

## data per economic activity

emp_per_econ_act <- mean_emp_per_econ_act %>%
  left_join(median_emp_per_econ_act, by = "econ_act") %>%
  left_join(var_emp_per_econ_act, by = "econ_act") %>%
  left_join(sd_emp_per_econ_act, by = "econ_act") %>%
  left_join(range_emp_per_econ_act, by = "econ_act") %>%
  left_join(skew_emp_per_econ_act, by = "econ_act") %>%
  left_join(kurtosis_emp_per_econ_act, by = "econ_act")

ict_per_econ_act <- mean_ict_per_econ_act %>%
  left_join(median_ict_per_econ_act, by = "econ_act") %>%
  left_join(var_ict_per_econ_act, by = "econ_act") %>%
  left_join(sd_ict_per_econ_act, by = "econ_act") %>%
  left_join(range_ict_per_econ_act, by = "econ_act") %>%
  left_join(skew_ict_per_econ_act, by = "econ_act") %>%
  left_join(kurtosis_ict_per_econ_act, by = "econ_act")

#View(emp_per_country)
#View(ict_per_country)
#View(emp_per_year)
#View(ict_per_year)
#View(emp_per_econ_act)
#View(ict_per_econ_act)


# graphical analysis #### 

  ## boxplot ####
par(mfrow=c(1,2))
boxplot(data$ict)
boxplot(data$emp)
par(mfrow=c(1,1))

Q1_ict <- quantile(log(data$ict), 0.25)
Q3_ict <- quantile(log(data$ict), 0.75)
Q1_emp <- quantile(log(data$emp), 0.25)
Q3_emp <- quantile(log(data$emp), 0.75)

IQR_ict <- Q3_ict - Q1_ict
IQR_emp <- Q3_emp - Q1_emp

lb_ict <- Q1_ict - 1.5 * IQR_ict
ub_ict <- Q3_ict + 1.5 * IQR_ict
lb_emp <- Q1_emp - 1.5 * IQR_emp
ub_emp <- Q3_emp + 1.5 * IQR_emp

out_ict <- log(data$ict)[log(data$ict) < lb_ict | log(data$ict) > ub_ict]
out_emp <- log(data$emp)[log(data$emp) < lb_emp | log(data$emp) > ub_emp]

out_ict
out_emp

out_data<- data%>%
  filter(log(data$emp) > lb_emp & log(data$emp < ub_emp))

par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
boxplot(data$ict,
        main="Boxplot ict")
boxplot(data$emp,
        main="Boxplot emp")
boxplot(data$log_ict,
        main="Boxplot log ict")
boxplot(data$log_emp,
        main="Boxplot log emp")
mtext("COMPARISON OF LOGS AND LEVELS FOR MAIN VARIABLES", outer = TRUE, 
      cex = 1.2, font=2)
par(mfrow=c(1,1))

  ## density plot ####
# ict
ict_levels =
  data %>%  
  unique() %>% 
  ggplot() +
  aes(x = ict)  +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'black', fill = 'blue') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ict_levels =
  data %>%  
  unique() %>% 
  ggplot() +
  aes(x = ict)  +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.5, color = 'black', fill = 'blue') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


hist_ict <- ict_levels +
  labs(
    x = "ict",
    y = "Density"
  ) 

hist_logict <- ict_levels + scale_x_log10() + 
  labs(
    x = "Log-ict",
    y = "Density"
  ) +
  geom_density(linewidth = 1) 

emp_levels =
  data %>%  
  ggplot() +
  aes(x = emp) + 
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.5, color = 'black', fill = 'blue') + 
  theme_bw()   +
  theme(plot.title = element_text(hjust = 0.5))

hist_emp <- emp_levels +
  labs(
    x = "emp",
    y = "Density"
  ) 

hist_logemp <- emp_levels + scale_x_log10() + 
  labs(
    x = "Log-emp",
    y = "Density"
  ) +
  geom_density(linewidth = 1) 

grid.arrange(hist_ict, hist_logict, hist_emp, hist_logemp, nrow = 2, ncol = 2, 
             top = textGrob("DENSITY DISTRIBUTIONS FOR LEVELS AND LOGS OF MAIN VARIABLES", 
                            gp = gpar(fontface = "bold")))

# And this is normal!
# For several reasons, for instance
# 1) The size of countries and sectors changes a lot (eg. Germany vs Belgium, 
# services vs manufacturing in France)
# 2) Capital types used by sector are very heterogeneous (digital sectors vs 
# textile industry)
# 3) Some sectors are much more labor intensive than others (eg. construction)

  ## scatterplot ####
# We create four scatterplots in order to visualize the relation between 
# the dependent variable and the independent variables.

data_na =
  data %>% 
  filter(!is.na(emp), !is.na(ict), !is.na(val_add), !is.na(lab_prod),
         !is.na(log_emp), !is.na(log_ict), !is.na(log_val_add), 
         !is.na(log_lab_prod))
nrow(data_na)
nrow(data)

scatterplot_log_ict_emp =
  data_na %>%  ggplot() +
  aes(x = log_ict, y = log_emp) +
  geom_point() +
  labs(
    x = "Log ict",
    y = "Log emp",
    title = "Log-Log Relation between ict and emp"
  ) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

scatterplot_log_valadd_emp =
  data_na %>%  ggplot() +
  aes(x = log_val_add, y = log_emp) +
  geom_point() +
  labs(
    x = "Log val_add",
    y = "Log emp",
    title = "Log-Log Relation between val_add and emp"
  ) + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

scatterplot_log_labprod_emp =
  data_na %>%  ggplot() +
  aes(x = log_lab_prod, y = log_emp) +
  geom_point() +
  labs(
    x = "Log lab_prod",
    y = "Log-emp",
    title = "Log-Log Relation between lab_prod and emp"
  ) + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

scat1 <- scatterplot_log_ict_emp + geom_smooth(method = 'lm')
scat2 <- scatterplot_log_valadd_emp + geom_smooth(method = 'lm')
scat3 <- scatterplot_log_labprod_emp + geom_smooth(method = 'lm')

grid.arrange(scat1, scat2, scat3, nrow = 2, ncol = 2, 
             top = textGrob("SCATTERPLOTS OF emp AND ALL OTHER VARIABLES", 
                            gp = gpar(fontface = "bold")))

# Considerations:
# 1) Are the considered variables positively or negatively related with Y? Why?
# 2) What are the clusters of observations that you find on each plot?
# 3) What is the clusters of observations on the top/bottom right/left part of 
# the plots (if it there's)?

# correlation #### 
cor(x=data$ict, y=data$emp)
cor(x=data$log_ict, y=data$log_emp)

# data.na
#nrow(data_na)
#nrow(data)

corr = 
  data %>%
  select(
    emp,
    ict,
    val_add,
    lab_prod,
  ) %>% 
  cor(use = "complete.obs")

corrplot(corr, method="color", type = 'full', diag=FALSE, col=c("indianred1", "cornflowerblue"),  
         addgrid.col = 'white', addCoef.col = 'black',tl.col = "black",tl.srt = 45)

corr_log = 
  data_na %>%
  select(
    log_emp,
    log_ict,
    log_val_add,
    log_lab_prod,
  ) %>% 
  cor(use = "complete.obs")

corrplot(corr_log, method="color", type = 'full', diag=FALSE, col=c("indianred1", "cornflowerblue"),  
         addgrid.col = 'white', addCoef.col = 'black',tl.col = "black",tl.srt = 45)

# Everything is positively correlated, why?

ggplot(data = data, aes(x=ict, y=emp)) +
  geom_point(alpha=.5) + 
  geom_smooth(method = "lm", se = F, col = "blue") + 
  geom_smooth(method = "lm", formula = y~x+I(x^2), se = F, col = "red") + 
  theme_bw()

ggplot(data = data, aes(x=log_ict, y=log_emp)) +
  geom_point(alpha=.5) + 
  geom_smooth(method = "lm", se = F, col = "blue") + 
  geom_smooth(method = "lm", formula = y~x+I(x^2), se = F, col = "red") + 
  theme_bw()

# data models #### 

  ## levels vs logs

data_mod1 <- lm(data, 
                formula = emp ~ ict)

data_mod2 <- feols(data, 
                   fml = log_emp ~ log_ict)

mod_base <- modelsummary(list("Level-Level (1)"= data_mod1, 
                              "Log-Log (2)" = data_mod2),
                         stars = c('*' = .1, '**' = .05, '***' = .01), 
                         fmt = 3,
                         gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors",
                         output = "kableExtra")

mod_base %>%
  kable_styling(position = "center") %>%
  add_header_above(c("LEVELS vs LOGS" = 3))

  ## fixed effects

#data_mod2

data_mod3 <- feols(data, 
                   fml = log_emp ~ log_ict | 
                     country)

data_mod4 <- feols(data, 
                   fml = log_emp ~ log_ict | 
                     country + year)

data_mod5 <- feols(data, 
                   fml = log_emp ~ log_ict | 
                     country + year + econ_act)

mod_base <- modelsummary(list("Log-Log (2)"= data_mod2, 
                              "Entity FE (3)" = data_mod3,
                              "Entity + time FE (4)" = data_mod4, 
                              "Entity + time + sector FE (5)"=data_mod5),
                         stars = c('*' = .1, '**' = .05, '***' = .01), 
                         fmt = 3,
                         gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors",
                         output = "kableExtra")

mod_base %>%
  kable_styling(position = "center") %>%
  add_header_above(c("FIXED EFFECTS MODELS" = 5))

#it isn't needed to cluster, since n in the three dimensions are small

  ## OVB #### 

## val_add
data_mod5 <- feols(data = data_na, 
                   fml = log_emp ~ log_ict + log(val_add) | 
                     country + year + econ_act, 
                   cluster = "country")

## lab_prod
data_mod6 <- feols(data = data_na, 
                   fml = log_emp ~ log_ict + log(lab_prod) | 
                     country + year + econ_act, 
                   cluster = "country")

## val_add + lab_prod
data_mod7 <- feols(data = data_na, 
                   fml = log_emp ~ log_ict + log(val_add) + log(lab_prod) | 
                     country + year + econ_act, 
                   cluster = "country")

modelsummary(list("entity+time+sector (4)"= data_mod4, 
                  "log(val_add) (5)"= data_mod5, 
                  "log(lab_prod) (6)" = data_mod6,
                  "log(val_add)+log(lab_prod) (7)" = data_mod7),
             stars = c('*' = .1, '**' = .05, '***' = .01), 
             fmt = 3)

mod_base %>%
  kable_styling(position = "center") %>%
  add_header_above(c("MODEL with OVs" = 5))

  ## submodels #### 

# filtered data

  ## Manufacturing
data_1<-filter(data, econ_act=="Manufacturing")
  ## Information and communication
data_2<-filter(data, econ_act=="Information and communication")
  ## Financial and insurance activity
data_3<-filter(data, econ_act=="Financial and insurance activity")
  ## Professional, scientific and technical activities; administrative and 
    ## support service activities
data_4<-filter(data, econ_act=="Professional, scientific and technical activities; administrative and support service activities")

    ## data_1: Manufacturing
data_mod1 <- lm(data = data_1, 
                formula = log_emp ~ log_ict)

data_mod2 <- feols(data = data_1, 
                   fml = log_emp ~ log_ict | 
                     country, 
                   cluster = "country")

data_mod3 <- feols(data = data_1, 
                   fml = log_emp ~ log_ict | 
                     country + year, 
                   cluster = "country")

# data_mod4 <- feols(data = data_1, 
#                   fml = log_emp ~ log_ict + log_val_add + log_lab_prod | 
#                     country + year, 
#                   cluster = "country")

submodel <- modelsummary(list("Log-Log (2_A)" = data_mod1, 
                           "Entity FE (3_A)" = data_mod2, 
                           "Entity + time FE (4_A)" = data_mod3),
                     stars = c('*' = .1, '**' = .05, '***' = .01),
                     gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors",
                     output = "kableExtra")

submodel %>%
  kable_styling(position = "center") %>%
  add_header_above(c("MANUFACTURING" = 4))

  ## data_2: Information and communication
data_mod1 <- lm(data = data_2, 
                formula = log_emp ~ log_ict)

data_mod2 <- feols(data = data_2, 
                   fml = log_emp ~ log_ict | 
                     country, 
                   cluster = "country")

data_mod3 <- feols(data = data_2, 
                   fml = log_emp ~ log_ict | 
                     country + year, 
                   cluster = "country")

#data_mod4 <- feols(data = data_2, 
#                   fml = log_emp ~ log_ict + log_val_add + log_lab_prod | 
#                     country + year, 
#                   cluster = "country")

submodel <- modelsummary(list("Log-Log (2_B)" = data_mod1, 
                              "Entity FE (3_B)" = data_mod2, 
                              "Entity+time FE (4_B)" = data_mod3),
                         stars = c('*' = .1, '**' = .05, '***' = .01),
                         gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors",
                         output = "kableExtra")

submodel %>%
  kable_styling(position = "center") %>%
  add_header_above(c("INFORMATION AND COMMUNICATION" = 4))

  ## data_3: Financial and insurance activity
data_mod1 <- lm(data = data_3, 
                formula = log_emp ~ log_ict)

data_mod2 <- feols(data = data_3, 
                   fml = log_emp ~ log_ict | 
                     country, 
                   cluster = "country")

data_mod3 <- feols(data = data_3, 
                   fml = log_emp ~ log_ict | 
                     country + year, 
                   cluster = "country")

#data_mod4 <- feols(data = data_3, 
#                   fml = log_emp ~ log_ict + log_val_add + log_lab_prod | 
#                     country + year, 
#                   cluster = "country")

submodel <- modelsummary(list("Log-Log (2_C)" = data_mod1, 
                              "Entity FE (3_C)" = data_mod2, 
                              "Entity+time FE (4_C)" = data_mod3),
                         stars = c('*' = .1, '**' = .05, '***' = .01),
                         gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors",
                         output = "kableExtra")

submodel %>%
  kable_styling(position = "center") %>%
  add_header_above(c("FINANCIAL AND INSURANCE ACTIVITY" = 4))

  ## data_4: Professional, scientific and technical activities; administrative 
            ## and support service activities
data_mod1 <- lm(data = data_4, 
                formula = log_emp ~ log_ict)

data_mod2 <- feols(data = data_4, 
                   fml = log_emp ~ log_ict | 
                     country, 
                   cluster = "country")

data_mod3 <- feols(data = data_4, 
                   fml = log_emp ~ log_ict | 
                     country + year, 
                   cluster = "country")

#data_mod4 <- feols(data = data_4, 
#                   fml = log_emp ~ log_ict + log_val_add + log_lab_prod | 
#                     country + year, 
#                   cluster = "country")

submodel <- modelsummary(list("Log-Log (2_D)" = data_mod1, 
                              "Entity FE (3_D)" = data_mod2, 
                              "Entity+time FE (4_D)" = data_mod3),
                         stars = c('*' = .1, '**' = .05, '***' = .01),
                         gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors",
                         output = "kableExtra")

submodel %>%
  kable_styling(position = "center") %>%
  add_header_above(c("PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES;
  ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITY" = 4))
