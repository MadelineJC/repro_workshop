#### A DOC STRING TELLS A READER WHAT THE FILE IS FOR ####
# This file reads in some data, cleans it, and exports it for analysis
# In accordance with the results presente in Smith et al. (2021)
# AUTHOR: Madeline E. Jarvis-Cross
# INITIATION DATE: 2021-05-13
### END DOC STRING ###

#### Set-up ####
library(here) # here() starts at /Users/mjarviscross/Desktop/GitHub/repro_workshop
# For example:
  # data <- read_csv(here("./pathname_after_end_of_here"))
# You can also write a csv this way
  # write_csv(data, here("./pathname_after_end_of_here"))
library(tidyverse)
# Rules for naming conventions
  # (1) Consistency 
    # Ex. objectOne: camel case
    # Ex. object_one: snake case
  # (2) Section and line documentation; Comment out what's going to happen and why, and explanations of weird lines

# Functional programming; how to write a function in R
adding_sum <- function(x,y) {
  # DocString; write what the function is supposed to do: this function takes in two numeric parms, and returns their sum
  # Parms:
    # x,y [num]: an object of type numeric
  # Returns:
    # z [num]: the sum of x and y
  z = x + y
  return(z)
}
adding_sum(1,2)  
# Output: 3
#### Now, my own stuff ####

#### Reproducing Antia et al.'s stuff ####

library(deSolve)
library(dplyr)
library(ggplot2)

#### Equations ####
# dP/dt = r*P - k*P*I, if P > D
# dI/dt = p*I*(P/(P + o))
# k << P(0) = I(0) = 1 ~ r ~ p << o << D
# Where:
## r = growth rate of parasite (0.1-10 in simulations)
## k = rate of destruction of P by I (<< 1; 10^(-3) in sims)
## p = maximum growth rate of IS (1 in simulations)
## o = parasite density at which growth rate of IS is half-max (1 << 0 << D; 10^3 in sims)
## D = lethal within-host parasite density (D >> o; 10^9 in sims)

#### Initial Simulation ####
Antia_Model <- function(t,y,p1){
  # Parms
  r <- p1[1]; k <- p1[2]; p <- p1[3]; o <- p1[4] # Had to make p in function distinct from p in parms
  # State vars
  P <- y[1]; I <- y[2]
  # ODEs
  dP = r*P - k*P*I
  dI = p*I*(P/(P + o))
  list(c(dP,dI))
}
# Parm vals
r <- 10; k <- 0.001; p <- 1; o <- 1000 # Note that r can range btw 0.1 and 10 in this model
parms <- c(r,k,p,o)
# Inits
P0 <- 1; I0 <- 1
N0 <- c(P0,I0)
# Time pt. sol'ns
TT <- seq(0.1,200,0.1)
# Sim
results <- lsoda(N0,TT,Antia_Model,parms,verbose = TRUE)
# Extract results
P <- results[,2]; I <- results[,3];
plot(results)
# Get output
Antia_Sim = data.frame(results)
write.csv(Antia_Sim,"Antia_Sim.csv", row.names = FALSE)
# Plot
Antia_Plot <- ggplot(Antia_Sim,aes(x=time))+
  geom_line(aes(y=X1, color="cornflowerblue"))+ # Parasite
  geom_line(aes(y=X2,color="springgreen4"))+ # Host
  labs(title="Antia_Sim",
       x="Time",
       y="Population Abundance")+
  scale_color_manual(name="Legend",
                     labels=c("Parasite",
                              "Host Immune Cells"),
                     values=c("springgreen4","cornflowerblue"))+
  theme_minimal()
Antia_Plot

#### Making it run through different values of r ####

# Could do this with a nested array with tx3 structure, which is nested in an array 


# Set-up
r_all <- seq(0.1,10,0.1) # All values of r to run through
TimePts <- data.frame() # Time points for simulation
Outputs <- data.frame(matrix(ncol = 201, nrow = 2000)) # 1 col for time point, 100 for P, 100 for I; 2000 time points (200 by 0.1); will be filled in with outputs
Outputs[,1] = c(1:2000)
Names <- character(length = 201) # Names of columns ("P_rval", for ex.)
Names[1] <- "time" # Name the first column "time" for time points
# Writing the model within a for loop that should run through all possible r values
for (i in 1:length(r_all)){ # r_all has length 100
  # Set iterator for names vector
  j = i+1 # For P
  q = i+101 # For I
  Antia_Model <- function(t,y,p1){ # Had to change "p" to "p1" because have a parm named p
    # Parms
    r <- p1[1]; k <- p1[2]; p <- p1[3]; o <- p1[4]
    # State vars
    P <- y[1]; I <- y[2]
    # ODEs
    dP = r*P - k*P*I
    dI = p*I*(P/(P + o)) # If P>D, could switch ODEs such that dP = P with some high mortality rate (large negative number), would approximate host dying quickly
    list(c(dP,dI))
  }
  # Parm vals
  r <- r_all[i]; k <- 0.001; p <- 1; o <- 1000
  parms <- c(r,k,p,o)
  # Inits
  P0 <- 1; I0 <- 1
  N0 <- c(P0,I0)
  # Time pt. sol'ns
  TT <- seq(0.1,200,0.1)
  # Sim
  results <- lsoda(N0,TT,Antia_Model,parms)
  # Extract results
  P <- results[,2]; I <- results[,3];
  Outputs[,j] = P; # Assign destination for extracted outputs
  Outputs[,q] = I
  # Get column names 
  Names[j] = paste0('P_',r_all[i]) # Add column names
  Names[q] = paste0('I_',r_all[i])
}
colnames(Outputs) = Names
# Raw output (haven't dealth with D yet)
write.csv(Outputs,"Antia_Sim_Range.csv", row.names = FALSE)

#### Wrangling output data to abide by P<D ####

# Could do this 


## Trying to get the P<D in there; if P>10^9 host dies, so P and I go to 0
# First make all P>10^9 = 0 (P is linear growth so previous time steps shouldn't matter?)
Outputs[is.na(Outputs)] <- 0 # Remove all NaN
Outputs_P <- lapply(Outputs[ ,2:101], function(x) ifelse(x > 10^9, 0, x)) # Make a df for P abundances, and specify that when abundance > 10^9, change it to 0
Outputs_P <- data.frame(Outputs_P)
Outputs_I <- Outputs[-c(1,2:101)] # Make a df for I abundances
time <- Outputs[-c(2:201)] # Take out "time" column
Outputs_Cleaned <- cbind(time,Outputs_P,Outputs_I) # Bind Outputs_P, Outputs_I, and time into one df
# Make all I = 0 when P at same position = 0; All P in 2-101; all I in 102-201
for (i in 1:2000){
  for (j in 2:101){
    Outputs_Cleaned[i,j+100] = ifelse(Outputs_Cleaned[i,j] == 0,0,Outputs_Cleaned[i,j+100])
    #print(Outputs[i,j])
  }
}
write.csv(Outputs_Cleaned,"Antia_Sim_Range_Cleaned.csv", row.names = FALSE)
# That should be the data stuff done; check this with Marty!!!

#### Plot ####
# With raw data
Antia_0.1_Raw_Plot <- ggplot(Outputs,aes(x=time))+
  geom_line(aes(y=P_0.1))+
  geom_line(aes(y=I_0.1))+ 
  labs(title="Abundances when r = 0.1",
       x="Time",
       y="Abundance")+
  theme_minimal()
Antia_0.1_Raw_Plot 
# With cleaned data
Antia_0.1_Cleaned_Plot <- ggplot(Outputs_Cleaned,aes(x=time))+
  geom_line(aes(y=P_0.1))+
  geom_line(aes(y=I_0.1))+ 
  labs(title="Abundances when r = 0.1",
       x="Time",
       y="Abundance")+
  theme_minimal()
Antia_0.1_Cleaned_Plot
# Okay, these look the same; good!