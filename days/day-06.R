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
   map_data   = NULL,
   visit_data = NULL,
   map_dims   = NULL,
   history    = list(),
   history_n  = 1,
   guard_char = c("<", ">", "v", "^"),
   guard_dir  = NULL,
   guard_pos  = NULL,
   guard_x    = NULL,
   guard_y    = NULL,
   loop       = FALSE,
   
   initialize = function(map_data) {
      self$map_data <- map_data
      self$map_dims <- dim(map_data)
      
      self$visit_data <- matrix(0, nrow = self$map_dims[1], ncol = self$map_dims[2])
      
      self$initialize_guard()
   },
   
   initialize_guard = function() {
      self$guard_pos <- which(self$map_data %in% self$guard_char)
      self$guard_x <- self$guard_pos %% ncol(self$map_data)
      self$guard_y <- self$guard_pos %/% ncol(self$map_data) + 1
      
      dir <- c(0, 0)
      switch(self$map_data[self$guard_x, self$guard_y],
             "^" = { dir <- c(-1,  0)},
             "v" = { dir <- c( 1,  0)},
             "<" = { dir <- c( 0, -1)},
             ">" = { dir <- c( 0,  1)})
      self$guard_dir <- dir
      
      self$visit_data[self$guard_x, self$guard_y] <- self$calculate_angle()
      
      # Replace inital guard char with G
      self$map_data[self$guard_x, self$guard_y] <- "G"
   },
   
   calculate_angle = function() {
      b <- c(0, 1)
      acos(sum(self$guard_dir*b) / (sqrt(sum(self$guard_dir*self$guard_dir)) * sqrt(sum(b*b)))) / (2*pi) * 360
   },
   
   next_dir = function() {
      self$map_data[self$guard_x + self$guard_dir[1],
                    self$guard_y + self$guard_dir[2]]
   },
   
   move_guard = function() {
      self$guard_x <- self$guard_x + self$guard_dir[1]
      self$guard_y <- self$guard_y + self$guard_dir[2]
      self$map_data[self$guard_x, self$guard_y] <- "G"
      
      angle <- self$calculate_angle()
      if (self$visit_data[self$guard_x, self$guard_y] == angle) {
         self$loop <- TRUE
      } else {
         self$visit_data[self$guard_x, self$guard_y] <- angle   
      }
   },
   
   turn_right = function() {
      rot_mat <- matrix(c(0, -1, 1, 0), nrow = 2)
      
      self$guard_dir <- c(rot_mat %*% self$guard_dir)
   },
   
   walk = function() {
      if (self$next_dir() == "O") {
         return(-1)
      }
      if (self$loop) {
         return(-2)
      }
      
      while(self$next_dir() == "#") {
         self$turn_right()
      }
      # Need to check "0" again?
      
      self$history[[self$history_n]] <- self$map_data
      self$history_n <- self$history_n + 1
      
      self$move_guard()
   },
   
   unique_positions_visited = function() {
      length(which(self$map_data == "G"))
   },
   print_walk = function() {
      for (i in seq_along(self$history)) {
         print(self$map_data)
         cat("\014")
         cat(paste0(apply(self$history[[i]], 1, paste, collapse = ""), collapse = "\n"))
         invisible(readline(prompt = i))
      }
      
   }
))

part1 <- Map$new(mapdata)

while (part1$walk() > 0) { }

part1$print_walk()


walk until obstacle or out of bounds

turn
