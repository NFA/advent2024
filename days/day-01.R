

if(!exists("get_aoc_input", mode="function")) source("util.R")

input.file <- get_aoc_input(2024, 1)
input <- read.table(input.file)

# Part 1
l1 <- sort(input[,1])
l2 <- sort(input[,2])
sum(abs(l1 - l2))

# Part 2
maxlen <- max(l1, l2)
l1.tab <- tabulate(l1)
l1.tab[l1.tab > 0] <-  seq_along(l1.tab)[l1.tab > 0]
l2.tab <- tabulate(l2)

length(l1.tab) <- maxlen
length(l2.tab) <- maxlen

sum(l1.tab * l2.tab, na.rm = TRUE)

