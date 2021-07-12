# Assignment: 5.2 Exercise
# Name: Phillips, Emily
# Date: 2010-07-10

#install.packages("tidyverse")
library(tidyverse)
library(readxl)

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/holmk/OneDrive/Desktop/DSC520 - Statistics/classRepo/dsc520/")

## Load the Housing dataset
housing_df <- read_excel("C:/Users/holmk/OneDrive/Desktop/DSC520 - Statistics/personalRepo/dsc520/completed/assignment04/week-6-housing.xlsx")

#Loading plyr and dplyr -- plyr must come before dplyr
library(plyr)
library(dplyr)

# Using the dplyr package, use the 6 different operations to analyze/transform the data - 
#   GroupBy, Summarize, Mutate, Filter, Select, and Arrange - 
# Remember this isn't just modifying data, you are learning about your data also - 
# so play around and start to understand your dataset in more detail

#prints as a tbl object
head(housing_df)
names(housing_df)

#Renaming quoted columns
housing_df <- housing_df %>% dplyr::rename(
  sale_date = 'Sale Date',
  sale_price = 'Sale Price'
)

#Select
housing_df %>% select(sale_date,sale_price,year_built,year_renovated,prop_type)
housing_df

#Filter

#Finding unique values of numeric categorical variables
unique(housing_df[c("sale_reason")])
unique(housing_df[c("sale_instrument")])
unique(housing_df[c("prop_type")])
unique(housing_df[c("present_use")])
unique(housing_df[c("sitetype")])

#Looking into sale_reason variable
housing_df %>% filter(sale_reason == 1)
#Home built before 1920 is considered "antique"
housing_df %>% filter(year_built < 1920)
#What does sale_warning == 12 pertain to? Need to look in code book
housing_df %>% filter(year_built < 1920 & sale_warning == 12)


#Mutate

#New column for price per quare foot
housing_df %>% select (sale_price,square_feet_total_living) %>% mutate(PricePerSquareFt = sale_price/square_feet_total_living)
housing_df <- housing_df %>% mutate(PricePerSquareFt = sale_price/square_feet_total_living)
housing_df$PricePerSquareFt

#Summarize
#Statistical values for new variable showing price per square foot
housing_df %>% summarize(AvgPriceSqFt = mean(PricePerSquareFt),MedianPriceSqFt = median(PricePerSquareFt))

#Group by
#Grouping by the site type of the house and finding the average sale price and median sale price per site type and year built
housing_df %>% dplyr::group_by(sitetype) %>% dplyr::summarize(AvgPrice = mean(sale_price),MedianPrice = median(sale_price), AvgBedrooms = mean(bedrooms))
housing_df %>% dplyr::group_by(year_built) %>% dplyr::summarize(AvgPrice = mean(sale_price),MedianPrice = median(sale_price))

#Arrange

#Sorting the median sales prices per year_built in descending order by price
#Want to see where the most business is occurring for older vs. newer houses
housing_df %>% dplyr::group_by(year_built) %>% dplyr::summarize(MedianPrice = median(sale_price)) %>% dplyr::arrange(desc(MedianPrice))

#Looking into information per ctyname
#Would need to handle NA values but just doing grouping for now to see varying price per square foot by city location
housing_df %>% dplyr::group_by(ctyname) %>% dplyr::summarize(MedianPrice = median(sale_price),AvgPriceFt = mean(PricePerSquareFt)) %>% dplyr::arrange(AvgPriceFt)


# Using the purrr package - perform 2 functions on your dataset.  
# You could use zip_n, keep, discard, compact, etc.
#install.packages("purrr")
library(purrr)

#keep function, didn't want to get means for the numeric categorical variables so used select function
#Summing to find total number of living spaces (bedrooms, bathrooms)
#Keeping where sum is greater than 20000
housing_df %>% dplyr::select(bedrooms,bath_full_count,bath_half_count,bath_3qtr_count) %>% map_dbl(sum) %>% keep (~ sum(.x) > 20000)

#Using the discard function to discard any square foot averages less than 20000
housing_df %>% dplyr::select(square_feet_total_living,sq_ft_lot) %>% map_dbl(mean) %>% discard (~ mean(.x) < 20000)


# Use the cbind and rbind function on your dataset

#Mock subset of housing_df to do with rbind
sale_date <- c("2020-12-20","2020-05-16","2019-08-05")
sale_price <- c(950000,567250,1980000)
zip5 <- c(98052,98052,98052)
square_feet_total_living <- c(4700,4000,7500)
bedrooms <- c(3,3,6)
year_built <- c(1987,2012,1942)
housing1 <- cbind(sale_date,sale_price,zip5,square_feet_total_living,bedrooms,year_built)

#Size of new dataftame
dim(housing1)

#Rbind with subset of housing_df
housing2 <- subset(housing_df,select = c("sale_date","sale_price","zip5","square_feet_total_living","bedrooms","year_built"))
housing2

#Size of subsetted housing_df
dim(housing2)

houses <- rbind(housing1,housing2)
houses

#Size of newly created datafame
dim(houses)

#Split a string, then concatenate the results back together
#install.packages("stringr")
library(stringr)

#Splitting the address column into street number and street name
address_df <- data.frame(str_split_fixed(housing_df$addr_full," ",2))
address_df <- address_df %>% dplyr::rename(
  street_num = 'X1',
  street_name = 'X2'
)

#New column with split parts of address back into full address string
address_df <- address_df %>% dplyr::mutate(full_addr = paste(street_num,street_name))
head(address_df)
