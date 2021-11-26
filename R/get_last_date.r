# prupose: get last date for file 
# date created: 21-09-21
# date last modified 21-09-21 

library(fs);library(stringr);library(dplyr)

most_recent_date <- 
        dir_ls("data/01_original_data/", regexp = "taxontable.rds") |> 
        str_remove("data/01_original_data/") |>  tibble() |> rename(date = "str_remove(...)") |> 
        mutate(date = ymd(date)) |>  
        slice_max(date) |> pull(date)
