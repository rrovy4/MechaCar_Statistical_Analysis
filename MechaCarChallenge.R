# Deliverable 1
library(dplyr)

# Import and Read csv
mech_file = read.csv('MechaCar_mpg.csv')

# Perform linear regression
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha)

# Determine p-value and r-squared value for the linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha))

# Deliverable 2
# Import and Read csv
s_coil = read.csv('Suspension_Coil.csv')

# Create Total Summary
total_summary = summarize(s_coil, Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI)) 

# Create Lot Summary
lot_summary <- s_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

# Deliverable 3
# T-Test for all manufacturing lots
t.test(s_coil$PSI,mu=mean(1500))

# T-Test for Lot 1
t.test(subset(s_coil,Manufacturing_Lot=="Lot1")$PSI,mu=mean(1500))

# T-Test for Lot 2
t.test(subset(s_coil,Manufacturing_Lot=="Lot2")$PSI,mu=mean(1500))

# T-Test for Lot 3
t.test(subset(s_coil,Manufacturing_Lot=="Lot3")$PSI,mu=mean(1500))