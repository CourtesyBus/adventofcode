#' 
#' Day 4: Camp Cleanup
#' source: https://adventofcode.com/2022/day/4
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

# get filepaths
v_files <- f_getdata(day = 5, return_paths = TRUE)

v_files |> 
  map(.f = read_lines) |> 
  map(.f = as.data.table) |> 
  map(.f = ~ .DT(.x, ,V2 := paste0("C", 1L + cumsum(V1 == "")))) |> 
  map(.f = ~ split(.x, f = list(.x$V2)))
  



# PART ONE ----------------------------------------------------------------

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = ~ .x |> 
      
      # create copy
      data.table::copy() |> 
      
      glimpse()
    
  )



# PART TWO ----------------------------------------------------------------


