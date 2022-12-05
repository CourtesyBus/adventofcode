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

# get files
lst_data <- f_getdata(day = 4)



# PART ONE ----------------------------------------------------------------

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = ~ .x |> 
      
      # create copy
      data.table::copy() |> 
      
      # split start and end points of each cleaning range
      .DT(, c("V3a", "V3b") := tstrsplit(V1, split = "-", type.convert = TRUE)) |> 
      .DT(, c("V4a", "V4b") := tstrsplit(V2, split = "-", type.convert = TRUE)) |> 
      
      # filter to instances where ranges are a subset of the other
      .DT((V3a >= V4a & V3b <= V4b) | (V4a >= V3a & V4b <= V3b)) |>
      
      # count rows
      nrow()
    
  )



# PART TWO ----------------------------------------------------------------

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = ~ .x |> 
      
      # create copy
      data.table::copy() |> 
      
      # split start and end points of each cleaning range
      .DT(, c("V3a", "V3b") := tstrsplit(V1, split = "-", type.convert = TRUE)) |> 
      .DT(, c("V4a", "V4b") := tstrsplit(V2, split = "-", type.convert = TRUE)) |> 
      
      # filter to instances where ranges overlap
      .DT(!(V3b < V4a | V4b < V3a)) |>
      
      # count rows
      nrow()
    
  )
