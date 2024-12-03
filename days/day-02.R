if(!exists("get_aoc_input", mode="function")) source("util.R")

input.file <- get_aoc_input(2024, 2)
input <- read.delim(input.file, header = FALSE, sep = "")

# Part 1

is_safe <- function(report) {
   diffs <- diff(report)
   abs_diffs <- abs(diffs)
   
   check_1 <- all(diffs      > 0, na.rm = TRUE) || all(diffs     < 0, na.rm = TRUE)
   check_2 <- all(abs_diffs >= 1, na.rm = TRUE) && all(abs_diffs <=3, na.rm = TRUE)
      
   check_1 && check_2
}

sum(apply(input, 1, is_safe))


# Part 2

old_safe_reports <- apply(input, 1, is_safe)

old_unsafe_reports <- input[!old_safe_reports,]

is_safe2 <- function(report) {
   report <- report[!is.na(report)]
   any(combn(report, length(report) - 1, is_safe))
}

sum(old_safe_reports) +  sum(apply(old_unsafe_reports, 1, is_safe2))

