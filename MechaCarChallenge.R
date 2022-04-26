### Dependency
library(tidyverse)
library(dplyr)

MechaCar <- read_csv("C:/Users/ryanc/OneDrive/Desktop/MD15/MechaCar_mpg.csv")

#perform a linear regression using MechaCar columns lm()
head(MechaCar)
lm(mpg ~ AWD + vehicle_weight + spoiler_angle + ground_clearance + vehicle_length,data=MechaCar) #generate multiple linear regression model
summary(lm(mpg ~ AWD + vehicle_weight + spoiler_angle + ground_clearance + vehicle_length,data=MechaCar)) #generate summary statistics
#r adjusted squared = 0.6825, p-value: 5.35e-11
sus <- read.csv('C:/Users/ryanc/OneDrive/Desktop/MD15/Suspension_Coil.csv') #import dataset
head(sus)
sus
total_summary1 <- group_by(sus, Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Med= median(PSI),Vari =var(PSI),SD= sd(PSI))
total_summary1


#Lot2 <- subset(group_by(sus, Manufacturing_Lot )%>% summarize(PSI), Manufacturing_Lot == "Lot2")
Lot1 <- sus[sus$Manufacturing_Lot == "Lot1",]
#Lot1
Lot2 <- sus[sus$Manufacturing_Lot == "Lot2",]
Lot3 <- sus[sus$Manufacturing_Lot == "Lot3",]
filteredLot1 <- total_summary1[total_summary1$Manufacturing_Lot == "Lot1",]
filteredLot1
filteredLot2 <- total_summary1[total_summary1$Manufacturing_Lot == "Lot2",]
filteredLot2
filteredLot3 <- total_summary1[total_summary1$Manufacturing_Lot == "Lot3",]
filteredLot3

#sample_table <- sus %>% sample_n(50)
t.test(log10(sus$PSI),mu=mean(log10(sus$PSI))) #compare all lots versus PSI means

t.test(log10(Lot1$PSI),mu=mean(log10(sus$PSI))) #compare Lot1 versus  PSI means
t.test(log10(Lot2$PSI),mu=mean(log10(sus$PSI))) #compare Lot1 versus  PSI means
t.test(log10(Lot3$PSI),mu=mean(log10(sus$PSI))) #compare Lot1 versus  PSI means