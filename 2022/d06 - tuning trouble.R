#' 
#' Day 6: Tuning Trouble
#' source: https://adventofcode.com/2022/day/6
#' 



# SETUP -------------------------------------------------------------------

# load packages
pacman::p_load(
  tidyverse,
  data.table,
  slider
)

# placeholder for data.table .[]
.DT <- `[`

# load functions
source(file = "2022/functions/key_functions.R")



# READ DATA ---------------------------------------------------------------

# get files
lst_data <- f_getdata(day = 6, read_lines = TRUE)
  


# PART ONE ----------------------------------------------------------------

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = ~ .x |> 
      
      strsplit(split = "") |> 
      
      unlist() |> 
      
      slider::slide_lgl(.f = ~ length(unique(.x)) == 4, .before = 3) |> 
      
      which() |> 
      
      head(1)
    
  )



# PART TWO ----------------------------------------------------------------

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = ~ .x |> 
      
      strsplit(split = "") |> 
      
      unlist() |> 
      
      slider::slide_lgl(.f = ~ length(unique(.x)) == 14, .before = 13) |> 
      
      which() |> 
      
      head(1)
    
  )
