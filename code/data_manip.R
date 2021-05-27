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
library(ggplot2)
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