#' 
#' Day 2: Rock Paper Scissors
#' source: https://adventofcode.com/2022/day/2
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
lst_data <- f_getdata(day = 2)

# assign values to letters
v_vals <- c(
  "A" = 1,
  "B" = 2,
  "C" = 3,
  "X" = 1,
  "Y" = 2,
  "Z" = 3
)



# PART ONE ----------------------------------------------------------------

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = ~ .x |> 
    
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

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = ~ .x |> 
    
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
