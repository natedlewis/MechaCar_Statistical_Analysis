library(tidyverse)

# Deliverable 1 ------------------------------------------------------------

# Load in data
mecha_df <- read.csv('data/MechaCar_mpg.csv')

# perform linear regression
lm(mpg ~ vehicle_length + vehicle_weight + 
     spoiler_angle + ground_clearance + AWD, mecha_df)

# perform summary
summary(lm(mpg ~ vehicle_length + vehicle_weight + 
             spoiler_angle + ground_clearance + AWD, mecha_df))

# Deliverable 2 ------------------------------------------------------------
suspension_df <- read.csv('data/Suspension_coil.csv')

#get a total summary
total_summary <- suspension_df %>% 
  summarise_at('PSI', list(mean, median, var, sd))

# create a lot summary
lot_summary <- suspension_df %>% 
  group_by(Manufacturing_Lot) %>% 
  summarise_at('PSI', list(mean, median, var, sd))

# Deliverable 3 -----------------------------------------------------------
# t test n all cars' PSI with a mean of 1500
t.test(suspension_df$PSI, mu=1500)

# determine if the PSI for each manufacturing lot is statistically different from the population mean of 1,500 pounds per square inch
t.test(subset(suspension_df,Manufacturing_Lot=="Lot1")$PSI, mu=1500) # lot 1
t.test(subset(suspension_df,Manufacturing_Lot=="Lot2")$PSI, mu=1500) # lot 2
t.test(subset(suspension_df,Manufacturing_Lot=="Lot3")$PSI, mu=1500) # lot 3
