#' 
#' Day 3: Rucksack Reorganization
#' source: https://adventofcode.com/2022/day/3
#' 


# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(data.table)

# placeholder for .
.DT <- `[`


# READ DATA ---------------------------------------------------------------

# get files
dir(path = "2022/data/", pattern = "d03.*\\.txt", full.names = TRUE) |> 
  
  # set names
  (\(.) purrr::set_names(x = ., nm = tools::file_path_sans_ext(basename(.))))() |> 
  
  # read data
  map(.f = data.table::fread, header = FALSE) |> 
  
  # assign to variable
  force() -> lst_data


# create a lookup vector for letter values
v_vals <- set_names(
  1:52,
  nm = c(letters, LETTERS)
)



# PART ONE ----------------------------------------------------------------

# loop over test set and puzzle set
map(
  .x = lst_data,
  .f = ~
    # start with data
    .x |> 
    
    # create copy
    data.table::copy() |> 
    
    # split into two compartments
    .DT(, V2 := lapply(V1, function(x) {strsplit(x, paste0("(?<=.{", str_length(x) / 2, "})"), perl = TRUE)})) |>
    
    # convert from list to character
    .DT(, .(V2 = as.character(unlist(V2))), by = .(V1)) |> 
    
    # break into letters
    .DT(, V2 := strsplit(V2, split = "")) |>
    
    # get intersection
    .DT(, V3 := purrr::reduce(V2, intersect), by = .(V1)) |>
    
    # get unique badge group and intersection character
    .DT(, .(V1, V3)) |> 
    unique() |> 
    
    # get value for intersection
    .DT(, V4 := map_dbl(.x = V3, .f = ~ v_vals[[.x]])) |>  
    
    # get sum of intersection values
    .DT(, sum(V4))
  
)



# PART TWO ----------------------------------------------------------------

# loop over test set and puzzle set
map(
  .x = lst_data,
  .f = ~
    # start with data
    .x |> 
    
    # create copy
    data.table::copy() |> 
    
    # create helper column for row order
    .DT(, R1 := .I) |> 
    
    # create helper column to separate into badge groups
    .DT(, R2 := cumsum((.I - 1) %% 3 == 0)) |> 
    
    # break into letters
    .DT(, V2 := strsplit(V1, split = "")) |> 
    
    # get intersection for each badge group
    .DT(, V3 := purrr::reduce(.x = V2, .f = intersect), by = .(R2)) |> 
    
    # get unique badge group and intersection character
    .DT(, .(R2, V3)) |> 
    unique() |> 
    
    # get value for intersection
    .DT(, V4 := map_dbl(.x = V3, .f = ~ v_vals[[.x]])) |>  
    
    # get sum of intersection values
    .DT(, sum(V4))
  
)
