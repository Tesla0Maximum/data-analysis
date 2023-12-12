library(tidyverse)
library(openintro)
library(dplyr)
library(ggplot2)
library(plotrix)
#using personal dataset found on keggle
data <- read.csv('C:/Users/tesla/Downloads/car_details_v3.csv')

###### DATA1 ##### 

#checking the dataset
head(data)
glimpse(data)
dim(data)
str(data)

#start by removing empty rows
count(data)
#from 8121 rows
sum(is.na(data))

row_status <- complete.cases(data)
data <- data[row_status, ]
sum(is.na(data))
#to 7907
count(data)

#converting engine column from char to integer value
str(data$engine)

engine_chr_values <- strsplit(data$engine, split=" ", fixed=TRUE)
engine_count <- length(engine_chr_values)
new_engine_values <- vector()

for (i in 1:engine_count){
new_engine_values[i] <- strtoi(engine_chr_values[[i]][1])
}

data$engine <- new_engine_values
str(data$engine)

#converting mileage from char to numeric vector
str(data$mileage)

mileage_chr_values <- strsplit(data$mileage, split = " ", fixed = TRUE)
mileage_count <- length(mileage_chr_values)
new_mileage_values <- vector()

for (i in 1:mileage_count){
  new_mileage_values[i] <- as.numeric(mileage_chr_values[[i]][1])
}

data$mileage <- new_mileage_values
str(data$mileage)
#converting max power from char to numeric vector
str(data$max_power)

max_power_chr_values <-strsplit(data$max_power, split=" ", fixed=TRUE)
max_power_count <- length(max_power_chr_values)
new_max_power_values <- vector()

for (i in 1:max_power_count){
  new_max_power_values[i] <- as.numeric(max_power_chr_values[[i]][1])
}

data$max_power <- new_max_power_values
str(data$max_power)

#prediction by selecting numeric data
pred_data <- data %>% select(selling_price, km_driven, owner, year, fuel, seller_type, transmission, mileage, engine)
#age calculation
year_count <- length(pred_data$year)
age <- vector()

for(i in 1:year_count){
  age[i] <- 2020 - pred_data$year[i]
}
pred_data <- cbind(pred_data, age)
str(pred_data$age)

pred_data <- subset(pred_data, select = -year)
str(pred_data)

#convert fuel types to binary
pred_data %>% count(fuel)

pred_fuel_count <- length(pred_data$fuel)

for (i in 1:pred_fuel_count ){
  temp_val <- trimws(pred_data$fuel[i])
    
  if(temp_val %in% "CNG") {
    pred_data <- pred_data[-c(i),]
  }
  else if(temp_val %in% "LPG") {
    pred_data <- pred_data[-c(i),]
  }
}

pred_data %>% count(fuel)
#petrol and diesel
fuel_type <- vector()
pred_fuel_count <- length(pred_data$fuel)

for (i in 1:pred_fuel_count){
  if(pred_data$fuel[i] %in% "Petrol") {
    fuel_type[i] <- 1
  }
  else {
    fuel_type[i] <- 0
    }
}
pred_data <- cbind(pred_data, fuel_type)
pred_data <- subset(pred_data, select = -fuel)

str(pred_data)

#convert transmission types to binary
pred_data %>% count(transmission)

transmission_type <- vector()
transmission_count <- length(pred_data$transmission)

for(i in 1:transmission_count){
  if(pred_data$transmission[i] %in% "Manual"){
    transmission_type[i] <- 1
  }
  else {
    transmission_type[i] <- 0
  }
}

pred_data <- cbind(pred_data, transmission_type)
pred_data <- subset(pred_data, select = -transmission)

str(pred_data)

#convert seller types to binary
pred_data %>% count(seller_type)

seller_count <- length(pred_data$seller_type)

for (i in 1:seller_count){
  temp_val <- trimws(pred_data$seller_type[i])
  
  if(temp_val %in% "Trustmark Dealer") {
    pred_data <- pred_data[-c(i),]
  }
}

pred_data %>% count(seller_type)
#individual and dealer
seller_type <- vector()
seller_count<- length(pred_data$seller_type)

for (i in 1:seller_count){
  if (pred_data$seller_type[i] %in% "Individual"){
    seller_type[i] <- 1
  }
  else{
    seller_type[i] <- 0
  }
}

pred_data <- subset(pred_data, select = -seller_type)
pred_data <- cbind(pred_data, seller_type)

str(pred_data)
#convert owner types to binary
pred_data %>% count(owner)

owner_count <- length(pred_data$owner)

for (i in 1:owner_count){
  temp_val <- trimws(pred_data$owner[i])
  
  if(temp_val %in% "Fourth & Above Owner"){
    pred_data <- pred_data[-c(i),]
  }
  else if(temp_val %in% "Third Owner"){
    pred_data <- pred_data[-c(i),]
  }
  else if(temp_val %in% "Test Drive Car"){
    pred_data <- pred_data[-c(i),]
  }
}

pred_data %>% count(owner)
#first and second owner
owner_type <- vector()
owner_count <- length(pred_data$owner)

for(i in 1:owner_count){
  if(pred_data$owner[i] %in% "First Owner"){
    owner_type[i] <- 1
  }
  else{
    owner_type[i] <- 0
  }
}

pred_data <- cbind(pred_data, owner_type)
pred_data <- subset(pred_data, select = -owner)

str(pred_data)

#histogram
ggplot(data, aes(x = seats)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", boundary = 0.5) +
  ggtitle("No. of Seats vs No. of Cars") +
  xlab("No. of Seats") +
  ylab("No. of Cars") +
  scale_x_continuous(breaks = seq(0, 10, 1))

#box plot
temp_data <- data %>%
  select(selling_price, seats) %>%
  filter(selling_price < 200000)

temp_data$seats <- factor(temp_data$seats)

ggplot(temp_data, aes(x = reorder(seats, seats), y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Nr of Seats vs Sell Price") +
  xlab("Nr of Seats") +
  ylab("Sell Price") 

#FUEL TYPES SOLD IN CAR MARKET
fuel_count = data %>% count(fuel)

# Calculate percentages
fuel_count <- fuel_count %>%
  mutate(percentage = n / sum(n) * 100)

# Create bar plot with ggplot2
ggplot(fuel_count, aes(x = fuel, y = percentage, fill = fuel)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5) +
  theme_minimal() +
  ggtitle("Fuel Types (Percentage)") +
  xlab("Fuel Type") +
  ylab("Percentage")

#transmission count types
transmission_count = data %>% count(transmission)
# Calculate percentages
transmission_count <- transmission_count %>%
  mutate(percentage = n / sum(n) * 100)

# Create bar plot with ggplot2
ggplot(transmission_count, aes(x = transmission, y = percentage, fill = transmission)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5) +
  theme_minimal() +
  ggtitle("Transmission Types (Percentage)") +
  xlab("Transmission Type") +
  ylab("Percentage")

#bar plot
barplot(table(data$seller_type), col= c('red','blue','green'), main = "Seller Type vs Nr of Cars")

#scatter plot
graph_data_1 <- data %>%
  select(selling_price, km_driven) %>%
  filter(selling_price < 5000000, km_driven < 400000)

ggplot(graph_data_1, aes(x=selling_price, y=km_driven)) + geom_point(alpha = 0.3) + geom_smooth() + labs(title='Selling Price vs Odometer', x='Selling Price', y='km')


#bar chart
ggplot(data, aes(y = fuel, fill = owner)) +
  geom_bar(position = "dodge", stat = "count") +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))),
            position = position_dodge(width = 0.9), hjust = -0.1, size = 3) +
  geom_hline(yintercept = seq(0.5, max(table(data$fuel)), 1), color = "black", size = 1) +
  labs(title = 'Nr of Cars/Owners vs Fuel Type', y = "Fuel Type", x = "Nr of Cars", fill = "Owner")

#smoothplot
graph_data_3 <- data %>%
  select(mileage, engine)

ggplot(graph_data_3, aes(x=engine, y=mileage)) +geom_smooth() +labs(title="Engine Capacity vs Mileage", x='cc', y='km/L')

#density plot
ggplot(data, aes(mileage)) + geom_density(aes(fill=fuel), alpha=0.3) +labs(title = "Car Mileage vs Density vs Fuel Type", x="Car Mileage km/L", y="Density", fuel="Fuel Type")

#predictions
#simple regression linear

#engine capacity vs mileage
ggplot(pred_data, aes(x = engine, y = mileage)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", col = "red", lwd = 1) +  
  labs(x = "Engine (cc)", y = "Mileage km/L", title = "Engine Capacity vs Mileage")

#age vs mileage
ggplot(pred_data, aes(x = age, y = mileage)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", col = "red", lwd = 1) +  
  labs(x = "Age (yr)", y = "Mileage km/L", title = "Age vs Mileage")

#Selling Price vs mileage
ggplot(pred_data, aes(x = selling_price, y = mileage)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", col = "red", lwd = 1) +  
  labs(x = "Price", y = "Mileage km/L", title = "Sell Price vs Mileage") +
  xlim(0, 500000) + ylim(0, 35) 

#multiple linear regression
mlr <- lm(selling_price ~ km_driven + owner_type + age + fuel_type + seller_type + transmission_type+mileage+engine, data=pred_data)
summary(mlr)


#selling price by transmission type
ggplot(pred_data, aes(x = transmission_type, y = selling_price, fill = factor(transmission_type))) +
  geom_boxplot() +
  labs(title = "Transmission Type Impact on Selling Price", x = "Transmission Type", y = "Selling Price")
