#' 
#' Day 3: Rucksack Reorganization
#' source: https://adventofcode.com/2022/day/3
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
lst_data <- f_getdata(day = 3)

# create a lookup vector for letter values
v_vals <- set_names(
  1:52,
  nm = c(letters, LETTERS)
)



# PART ONE ----------------------------------------------------------------

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = ~ .x |> 
    
      # create copy
      data.table::copy() |> 
      
      # split into two compartments
      .DT(, V2 := lapply(V1, function(x) {strsplit(x, paste0("(?<=.{", str_length(x) / 2, "})"), perl = TRUE)})) |>
      
      # break into letters
      .DT(, V2 := lapply(V2, function(x) {lapply(x, strsplit, "")})) |>
      
      # get intersection
      .DT(, V3 := lapply(V2, function(x) {map_chr(x, purrr::reduce, intersect)})) |> 
      
      # get value for intersection
      .DT(, V4 := map_dbl(.x = V3, .f = ~ v_vals[[.x]])) |>

      # get sum of intersection values
      .DT(, sum(V4))
    
  )



# PART TWO ----------------------------------------------------------------

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = ~ .x |> 
    
      # create copy
      data.table::copy() |> 
      
      # create helper column to separate into badge groups
      .DT(, R1 := cumsum((.I - 1) %% 3 == 0)) |> 
      
      # break into letters
      .DT(, V2 := strsplit(V1, split = "")) |> 
      
      # get intersection for each badge group
      .DT(, V3 := purrr::reduce(.x = V2, .f = intersect), by = .(R1)) |> 
      
      # get unique badge group and intersection character
      .DT(, .(R1, V3)) |> 
      unique() |> 
      
      # get value for intersection
      .DT(, V4 := map_dbl(.x = V3, .f = ~ v_vals[[.x]])) |>  
      
      # get sum of intersection values
      .DT(, sum(V4))
    
  )
