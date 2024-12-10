if(!exists("get_aoc_input", mode="function")) source("days/util.R")

input.file <- get_aoc_input(2024, 5)

input <- readChar(input.file, file.info(input.file)$size)
input <- unlist(strsplit(input, "\n\n"))

rules <- read.delim(textConnection(input[1]), sep = "|", header = FALSE)
pages <- read.delim(textConnection(input[2]), sep = ",", header = FALSE)

n_rules <- nrow(rules)

middle_page_sum <- 0

for (i in 1:nrow(pages)) {
   page <- as.numeric(pages[i,])
   page <- page[!is.na(page)]
   
   is_in_order <- TRUE
   
   page_length <- length(page)

   for (p in 1:page_length) {

      to_check <- page[-c(1:p)]
      rule <- rules[which(rules$V1 == page[p]),]$V2
      
      if (!all(to_check %in% rule)) is_in_order <- FALSE
      
   }
   if (is_in_order)  middle_page_sum <- middle_page_sum + page[page_length %/% 2 + 1]
}

middle_page_sum
