if(!exists("get_aoc_input", mode="function")) source("days/util.R")

input.file <- get_aoc_input(2024, 6)

input <- read.delim(input.file, header = FALSE, sep = "")
library(R6)

# Padding
input <- apply(input, 1, \(x) paste0("O", x, "O"))

line_length <- nchar(input[1])

input <- c(strrep("O", line_length), input, strrep("O", line_length))

mapdata <- do.call("rbind", strsplit(input, ""))

Map <- R6Class("Map",list(
   map_data = NULL,
   history = list(),
   guard_char = c("<", ">", "v", "^"),
   guard_pos = NULL,
   next_dir = NULL,
   initialize = function(map_data) {
      self$map_data <- map_data
      self$map_dims <- dim(map_data)
      
      self$update_guard_pos()
      self$update_guard_dir()
   },
   update_guard_pos = function() {
      self$guard_pos <- which(map_data %in% guard)
      self$guard_x <- guard_pos %% ncol(map_data)
      self$guard_y <- guard_pos %/% ncol(map_data) + 1
   },
   update_guard_dir = function() {
     dir <- c(0, 0)
     switch(map_data[self$guard_x, self$guard_y],
            "^" = { dir <- c(-1, 0)},
            "v" = { dir <- c(1, 0)},
            "<" = { dir <- c(0, -1)},
            ">" = { dir <- c(0, 1)})
     self$guard_dir <- dir
   },
   walk = function() {
      
   }
))

guard <- c("<", ">", "v", "^")

find_guard <- function(map) {
    pos <- which(map %in% guard)
    map_dims <- dim(map)
    
   c(pos %% ncol(map),  pos %/% ncol(map) + 1)
}

is_valid_step <- function(map) {
   
}

take_a_step <- function(map) {
   pos <- find_guard(map)
   x <- pos[1]
   y <- pos[2]
   
   dir <- c(0, 0)
   switch(map[x, y], 
          "^" = { dir <- c(-1, 0)},
          "v" = { dir <- c(1, 0)},
          "<" = { dir <- c(0, -1)},
          ">" = { dir <- c(0, 1)})
   
   drow <- dir[1]
   dcol <- dir[2]
   
   
   
   
}


walk until obstacle or out of bounds

turn
