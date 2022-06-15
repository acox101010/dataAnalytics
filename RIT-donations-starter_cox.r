#GNEG 590V - Data Analytics; Assignment 2.2; 06/11/2022
#Group 2
#John Kimsey, Keerthana Jaganathan; Joshua Herringtonl Andrew Cox


#Import packages
library(tidyverse)
library(lattice)
library(ggplot2)

#Import dataset
don = read.csv("RIT-contribution.csv") #for loading file 

#create new data frames per gender: This code is not in the Ledolter code.

don_f <- don[don$Gender == "F",]   #Female only data frame
don_m <- don[don$Gender == "M",]   #Male only data frame

# Question 1 - summary statistics of everybody, females, males
summary(don)

summary(don_f) #summary stats for male donators

summary(don_m) #summary stats for female donators

# Question 2 - Bar chart of donations per year

barchart(table(don$Class.Year), horizontal = FALSE, xlab = "Class Year", 
         ylab = "No. of donations",main="Donations per year", col="blue")

# Question 3 - Bar chart of donations per year

barchart(table(don$Gender), horizontal = FALSE, xlab = "Gender", 
         ylab = "No. of donators", main="Donators per gender",col="blue")

# Question 4 - Bar chart of donations per year by gender

barplot(table(don$Gender, don$Class.Year), beside = TRUE, xlab = "Class Year", 
        ylab="Count of Alumni", legend.text = c("Female","Male"),
        args.legend = list(x="topleft"))

# Question 5 - histogram of donations per class year
years_vec <- c(1975, 1985, 1995, 2005, 2015)

#loop for everyone
for (val in years_vec) {
  hist(don$TGiving[don$Class.Year==val], breaks = 100,
       main = paste("Histogram for everyone in year: ", val))
} 

#split by gender
don_year_75 <- don[don$Class.Year == "1975",]
don_year_85 <- don[don$Class.Year == "1985",]
don_year_95 <- don[don$Class.Year == "1995",]
don_year_05 <- don[don$Class.Year == "2005",]
don_year_15 <- don[don$Class.Year == "2015",]

# df_list <- list(don_year_75,don_year_85,don_year_95,don_year_05, don_year_15)
# for (i in 1:length(df_list)) {
#   p <- df_list %>% 
#     ggplot( aes(x=TGiving)) +
#     geom_histogram(aes(color=Gender,fill=Gender), 
#                    bins = 50, alpha = 0.8)
#     show(p)
# } 

#histogram 1975
p <- don_year_75 %>% 
  ggplot( aes(x=TGiving)) +
  geom_histogram(aes(color=Gender,fill=Gender),
                 bins = 50, alpha = 0.8,) + ggtitle("Histogram 1975")
  show(p)

#histogram 1985
p <- don_year_85 %>% 
  ggplot( aes(x=TGiving)) +
  geom_histogram(aes(color=Gender,fill=Gender), 
                   bins = 50, alpha = 0.8,) + ggtitle("Histogram 1985")
  show(p)
  
#histogram 1995
p <- don_year_95 %>% 
  ggplot( aes(x=TGiving)) +
  geom_histogram(aes(color=Gender,fill=Gender), 
                   bins = 50, alpha = 0.8,) + ggtitle("Histogram 1995")
  show(p)
  
#histogram 2005
p <- don_year_05 %>% 
  ggplot( aes(x=TGiving)) +
  geom_histogram(aes(color=Gender,fill=Gender), 
                   bins = 50, alpha = 0.8,) + ggtitle("Histogram 2005")
  show(p)
  
#histogram 2015
p <- don_year_15 %>% 
  ggplot( aes(x=TGiving)) +
  geom_histogram(aes(color=Gender,fill=Gender), 
                   bins = 50, alpha = 0.8,) + ggtitle("Histogram 2015")
  show(p)  
  
# Question 6 - boxplot of donations per class year

#boxplot for everyone
boxplot(don$TGiving~don$Class.Year, 
        main = "Total Contributions by Class Year Boxplots", 
        xlab = "Class year", ylab = "Total Contributions",
        outline = FALSE)

#boxplot split by gender
boxplot(don$TGiving~don$Gender*don$Class.Year, data = don , 
        outline = FALSE, xlab="Gender/Class Year", ylab="TGiving", 
        names=c("75F","75M", "85F", "85M", "95F", "95M", "05F", "05M", "15F", "15M"))

crop <- ggplot(don, aes(x=factor(Class.Year), y=TGiving,fill=Gender))+geom_boxplot(position=position_dodge(1)) +ylim(0, 10000)+ggtitle("Boxplot of Donation Value per Year")
crop #prints the boxplot



# Question 7 - Boxplot of TGiving by Class year and Gender

boxplot(don$TGiving~don$Gender*don$Class.Year, data = don , 
        col = c('red', 'blue'),outline = FALSE, 
        xlab="Gender/Class Year", ylab="TGiving", 
        names=c("75F","75M", "85F", "85M", "95F", "95M", "05F", "05M", "15F", "15M"))

