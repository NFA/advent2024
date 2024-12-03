if(!exists("get_aoc_input", mode="function")) source("util.R")

input.file <- get_aoc_input(2024, 3)
input <- readLines(input.file)

library(stringr)
library(tibble)
library(dplyr)

# Part 1

muls <- do.call("rbind", lapply(str_match_all(input, "mul\\((\\d+),(\\d+)\\)"), as_tibble))

muls |> 
   mutate(mul = as.numeric(V2)*as.numeric(V3)) |> 
   summarize(sum = sum(mul))

# Part 2
input2 <- paste(input, collapse = "") # Former read was in six parts, was OK for first question

enabled_muls <- str_replace_all(input2, "(?<=don't\\(\\))(.*?)(?=do\\(\\))", "")

muls2 <- do.call("rbind", lapply(str_match_all(enabled_muls, "mul\\((\\d+),(\\d+)\\)"), as_tibble))
muls2 |> 
   mutate(mul = as.numeric(V2)*as.numeric(V3)) |> 
   summarize(sum = sum(mul))
