#' 
#' Day 5: Supply Stacks
#' source: https://adventofcode.com/2022/day/5
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

# read lines
f_getdata(day = 5, read_lines = TRUE) |> 
  
  # loop over files to transform data
  map(
    
    # convert to data.table
    .f = ~ data.table(V1 = .x) |> 
      
      # add helper column to separate code segments
      .DT(, V2 := paste0("C", 1L + cumsum(V1 == ""))) |> 
      
      # split data into two segments
      (\(.) split(., f = list(.$V2)))() |> 
      
      # remove V2 column (no longer required)
      (\(.) map(., .f = ~ .DT(., , V2 := NULL)))()
      
  ) |> 
  
  # save variable
  force() -> lst_data_raw
  

# start with raw data
lst_data_raw |> 

  # loop over each dataset
  map(
    .f = ~ .x |>
      
      # modify first segment
      modify_at(
        .at = "C1",
        .f = ~ .x |> 
          
          # create copy
          data.table::copy() |> 
          
          # remove last row (contains column IDs)
          .DT(-.N) |> 
          
          # create helper column for row IDs
          .DT(, RX := .I) |> 
          
          # split line into individual components
          .DT(, V1 := strsplit(x = V1, split = "")) |> 
          
          # take the 2nd, 6th, 10th, ... element using modulo 4.
          .DT(, V1 := map(.x = V1, .f = ~ .x[(seq_along(.x) + 2) %% 4 == 0])) |> 
          
          # convert to long format
          .DT(, .(V2 = as.character(unlist(V1))), keyby = .(RX)) |>
          
          # create helper column for column IDs
          .DT(, RY := rowid(RX)) |> 
          
          # remove missing values
          .DT(V2 != " ") |> 
          
          # sort by RY then RX descending
          .DT(order(RY, RX)) |> 
          
          # update RX
          .DT(, RX := rowid(RY))
      ) |> 
      
      # modify second segment
      modify_at(
        .at = "C2", 
        .f = ~ .x |> 
          
          # create copy
          data.table::copy() |> 
          
          # remove row with missing entry (separator between code segments)
          .DT(V1 != "") |> 
          
          # extract numeric values
          .DT(, V1 := str_replace_all(string = V1, pattern = "^move (\\d+) from (\\d+) to (\\d+)$", replacement = "\\1;\\2;\\3")) |> 
          
          # split into separate columns of data.table
          .DT(, c("MOVE", "FROM", "TO") := tstrsplit(V1, split = ";", type.convert = TRUE)) |> 
          
          # remove initial column, now that data is separated.
          .DT(, V1 := NULL)
      )
  ) |> 
  
  # save variable
  force() -> lst_data



# PART ONE ----------------------------------------------------------------

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = function(x) {
      
      # run a for loop for each row of instructions
      for (i in seq_len(nrow(x$C2))) {
        
        # start with reference table
        x$C1 |> 
          
          # create copy
          data.table::copy() |> 
          
          # for elements that meet the criteria for FROM and MOVE, update columns
          .DT(
            RY == x$C2[i, FROM] & RX <= x$C2[i, MOVE], 
            `:=`(
              RZ = x$C2[i, TO], # helper column (to distinguish elements that have changed)
              RY = x$C2[i, TO], # reassign destination column ID
              RX = -RX          # reverse order of row ID (because loaded in reverse order)
            )
          ) |> 
          
          # order by column ID, then helper column (to bring changed elements to front, then new row ID)
          .DT(order(RY, RZ, RX)) |> 
          
          # update row ID and clear helper column
          .DT(, `:=`(RX = rowid(RY), RZ = NULL)) |> 
          
          # save variable
          force() -> x$C1
        
      }
      
      # start with final result set
      x$C1 |> 
        
        # get first row of each column ID
        .DT(, lapply(.SD, head, 1), keyby = .(RY)) |> 
        
        # extract element letter column as a vector
        .DT(, V2) |> 
        
        # paste into a single string
        paste0(collapse = "")
      
    }
    
  )        



# PART TWO ----------------------------------------------------------------

# start with data
lst_data |> 
  
  # loop over test set and puzzle set
  map(
    .f = function(x) {
      
      # run a for loop for each row of instructions
      for (i in seq_len(nrow(x$C2))) {
        
        # start with reference table
        x$C1 |> 
          
          # create copy
          data.table::copy() |> 
          
          # for elements that meet the criteria for FROM and MOVE, update columns
          .DT(
            RY == x$C2[i, FROM] & RX <= x$C2[i, MOVE], 
            `:=`(
              RZ = x$C2[i, TO], # helper column (to distinguish elements that have changed)
              RY = x$C2[i, TO]  # reassign destination column ID
            )
          ) |> 
          
          # order by column ID, then helper column (to bring changed elements to front, then new row ID)
          .DT(order(RY, RZ, RX)) |> 
          
          # update row ID and clear helper column
          .DT(, `:=`(RX = rowid(RY), RZ = NULL)) |> 
          
          # save variable
          force() -> x$C1
        
      }
      
      # start with final result set
      x$C1 |> 
        
        # get first row of each column ID
        .DT(, lapply(.SD, head, 1), keyby = .(RY)) |> 
        
        # extract element letter column as a vector
        .DT(, V2) |> 
        
        # paste into a single string
        paste0(collapse = "")
      
    }
    
  )        
