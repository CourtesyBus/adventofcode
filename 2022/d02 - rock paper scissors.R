#' 
#' Day 2: Rock Paper Scissors
#' source: https://adventofcode.com/2022/day/2
#' 


# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(data.table)

# placeholder for .
.DT <- `[`


# READ DATA ---------------------------------------------------------------

# get files
dir(path = "2022/data/", pattern = "d02.*\\.txt", full.names = TRUE) |> 
  
  # set names
  (\(.) purrr::set_names(x = ., nm = tools::file_path_sans_ext(basename(.))))() |> 
  
  # read data
  map(.f = data.table::fread, header = FALSE) |> 
  
  # assign to variable
  force() -> lst_data



# assign values
v_vals <- c(
  "A" = 1,
  "B" = 2,
  "C" = 3,
  "X" = 1,
  "Y" = 2,
  "Z" = 3
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
    
    # create helper columns with number conversion
    .DT(, c("V3", "V4") := lapply(.SD, function(x) v_vals[x])) |> 
    
    # calculate contest (modulus 3)
    .DT(, V5 := (V4 - V3) %% 3) |> 
    
    # calculate points and result
    .DT(, V6 := 
          V4 + fcase(
            V5 == 0, 3, 
            V5 == 1, 6,
            default = 0
          )
    ) |> 
    
    # sum total
    .DT(, sum(V6))
  
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
    
    # create helper columns with number conversion
    .DT(, c("V3", "V4") := lapply(.SD, function(x) v_vals[x])) |> 
    
    # replace values
    .DT(, V4 := (V3 + (V4 - 2) - 1) %% 3 + 1) |> 
    
    # calculate contest (modulus 3)
    .DT(, V5 := (V4 - V3) %% 3) |>

    # calculate points and result
    .DT(, V6 :=
          V4 + fcase(
            V5 == 0, 3,
            V5 == 1, 6,
            default = 0
          )
    ) |>
    
    # sum total
    .DT(, sum(V6))

)
