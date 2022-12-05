#' 
#' Day 1: Calorie Counting
#' source: https://adventofcode.com/2022/day/1
#' 


# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(data.table)

# placeholder for .
.DT <- `[`


# READ DATA ---------------------------------------------------------------

# get files
dir(path = "2022/data/", pattern = "d01.*\\.txt", full.names = TRUE) |> 
  
  # set names
  (\(.) purrr::set_names(x = ., nm = tools::file_path_sans_ext(basename(.))))() |> 
  
  # read data
  map(.f = data.table::fread, header = FALSE) |> 
  
  # assign to variable
  force() -> lst_data





# PART ONE ----------------------------------------------------------------

# loop over test set and puzzle set
map(
  .x = lst_data,
  .f = ~
    # start with data
    .x |> 
  
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

# loop over test set and puzzle set
map(
  .x = lst_data,
  .f = ~
    # start with data
    .x |> 
    
    # create helper column for each elf
    .DT(, V2 := 1L + cumsum(is.na(V1))) |> 
    
    # sum food volume by elf
    .DT(, .(V3 = sum(V1, na.rm = TRUE)), by = .(V2)) |> 
    
    # sort in descending order
    .DT(order(-V3)) |> 
    
    # take first three rows, and the number of calories across all
    .DT(1:3, sum(V3))
)

