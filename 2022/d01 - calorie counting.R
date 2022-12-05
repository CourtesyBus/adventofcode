#' 
#' Day 1: Calorie Counting
#' source: https://adventofcode.com/2022/day/1
#' 


# SETUP -------------------------------------------------------------------

# load packages
pacman::p_load(
  tidyverse,
  data.table
)

# placeholder for data.table .[]
.DT <- `[`

# load functions
source(file = "2022/functions/key_functions.R")



# READ DATA ---------------------------------------------------------------

# get files
lst_data <- f_getdata(day = 1)



# PART ONE ----------------------------------------------------------------

# start with data
lst_data |> 

  # loop over test set and puzzle set
  map(
    .f = ~ .x |> 
    
        # create helper column for each elf
        .DT(, V2 := 1L + cumsum(is.na(V1))) |> 
        
        # sum food volume by elf
        .DT(, .(V3 = sum(V1, na.rm = TRUE)), by = .(V2)) |> 
        
        # sort in descending order
        .DT(order(-V3)) |> 
        
        # take first row, and the number of calories
        .DT(1, V3)
  )



# PART TWO ----------------------------------------------------------------

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = ~ .x |> 
      
      # create helper column for each elf
      .DT(, V2 := 1L + cumsum(is.na(V1))) |> 
      
      # sum food volume by elf
      .DT(, .(V3 = sum(V1, na.rm = TRUE)), by = .(V2)) |> 
      
      # sort in descending order
      .DT(order(-V3)) |> 
      
      # take first three rows, and the number of calories across all
      .DT(1:3, sum(V3))
  )
