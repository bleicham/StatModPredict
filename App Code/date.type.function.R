#------------------------------------------------------------------------------#
#                                                                              #
#                         Determining the date type                            #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function takes in the crude data dates selected by the user, and        #
# determines if the data contains weekly, daily, yearly, or time index data.   #
# If there are missing dates or other issues with formatting, the function     #
# returns an error that alerts users to fix their dates. If there is no issue, #
# the function returns the term that relates to the time make-up of the data.  #
#------------------------------------------------------------------------------#
#                       Author: Amanda Bleichrodt                              #
#------------------------------------------------------------------------------#

date.type.function <- function(dates.input){
  
#------------------------------------------------------------------------------#
# Function Inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Below reads in the dates from the main dashboard code. The dates come #
# from the crude data selected by the user.                                    #
#------------------------------------------------------------------------------#
  
###########################################
# Reading in the dates from the main code #
###########################################
dates.input.T <<- dates.input

#------------------------------------------------------------------------------#
# Determining the "type" of dates ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines the length of the first entry in the date     #
# vector. It then uses that information to figure out if it is a year, time    #
# index, or week/day. If it corresponds to a week or day, it then uses the     #
# the number of days between the first and second entry in the vector to figure#
# out if its weekly or daily data. There are quality checks built into the code#
# to check for missing data or skips in dates.                                 #
#------------------------------------------------------------------------------#

########################################
# Determining the length of the "date" #
########################################
date.length <- base::nchar(as.character(dates.input.T[1]))

############################
# Working with yearly data # 
############################
if(date.length == 4){
  
  # Returning a date type of "year"
  date.type <- "year"
  
  # Checking to see if there are skips in years, or years missing
  for(i in 1:(length(dates.input.T) - 1)){
    
    # Determining what the next "year" should be 
    nextYear <- as.numeric(dates.input.T)[i] + 1
    
    # Checking if what the next year should be and actually is match
    if(nextYear != as.numeric(dates.input.T)[i + 1]){
      
      # Returning a warning 
      warning("Please check the time column of your data. There may be years missing.")
      return("Please check the time column of your data. There may be years missing.")
      
      # Stopping the script
      stop()
    }
  }
    
#############################
# Working with time-indexes #
#############################
}else if(date.length < 4){
  
  # Returning a date type of "index"
  date.type <- "index"
  
  # Checking to see if there are skips in time indexes, or time indexes missing
  for(i in 1:(length(dates.input.T) - 1)){
    
    # Determining what the next "year" should be 
    nextIndex <- as.numeric(dates.input.T)[i] + 1
    
    # Checking if what the next year should be and actually is match
    if(nextIndex != as.numeric(dates.input.T)[i + 1]){
      
      # Returning a warning 
      warning("Please check the time column of your data. There may be time-indexes missing.")
      return("Please check the time column of your data. There may be time-indexes missing.")
      
      # Stopping the script
      stop()
    }
  }
  
##############################
# Working with days or weeks #
##############################
}else{
  
  # Determining the amount of time between two dates 
  date.duration <- as.character(difftime(anytime::anydate(dates.input.T[2]), anytime::anydate(dates.input.T[1]), units = "days"))
  
  # Creating the date.type variable
  date.type <- switch(date.duration,
                            "7" = "week", # If there are seven days between dates
                            "1" = "day", # If there is one day between dates
                     return("Please check your dates. They do not appear to be in a format that this toolbox supports."))
  
  # Checking for missing or skipped dates
  for(i in 1:(length(dates.input.T) - 1)){
    
    # Determining what the next "year" should be 
    nextIndex <- anytime::anydate(dates.input.T)[i] + as.numeric(date.duration)
    
    # Checking if what the next year should be and actually is match
    if(nextIndex != anytime::anydate(dates.input.T)[i + 1] & 
       (i + as.numeric(date.duration)) < (length(dates.input.T) - 1)){
      
      # Returning a warning 
      warning("Please check the time column of your data. There may be dates missing.")
      return("Please check the time column of your data. There may be dates missing.")
      
      # Stopping the script
      stop()
    }
  }
}


###########################
# Returning the date.type #
###########################
return(date.type)

}
