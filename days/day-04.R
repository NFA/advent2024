if(!exists("get_aoc_input", mode="function")) source("util.R")

input.file <- get_aoc_input(2024, 4)

input <- read.delim(input.file, header = FALSE, sep = "")

# Padding
input <- apply(input, 1, \(x) paste0("###", x, "###"))

line_length <- nchar(input[1])

input <- c(
   strrep("#", line_length),
   strrep("#", line_length),
   strrep("#", line_length),
   input,
   strrep("#", line_length),
   strrep("#", line_length),
   strrep("#", line_length)
)

mat <- do.call("rbind", strsplit(input, ""))

find_xmas <- function(mat, row, col) {
   nw <- all(mat[row - 1, col - 1] == "M",
             mat[row - 2, col - 2] == "A",
             mat[row - 3, col - 3] == "S")
   n <- all(mat[row - 1, col] == "M",
            mat[row - 2, col] == "A",
            mat[row - 3, col] == "S")
   ne <- all(mat[row - 1, col + 1] == "M",
             mat[row - 2, col + 2] == "A",
             mat[row - 3, col + 3] == "S")
   w <- all(mat[row, col - 1] == "M",
            mat[row, col - 2] == "A",
            mat[row, col - 3] == "S")
   e <- all(mat[row, col + 1] == "M",
            mat[row, col + 2] == "A",
            mat[row, col + 3] == "S")
   sw <- all(mat[row + 1, col - 1] == "M",
             mat[row + 2, col - 2] == "A",
             mat[row + 3, col - 3] == "S")
   s <- all(mat[row + 1, col] == "M",
            mat[row + 2, col] == "A", 
            mat[row + 3, col] == "S")
   se <- all(mat[row + 1, col + 1] == "M",
             mat[row + 2, col + 2] == "A",
             mat[row + 3, col + 3] == "S")
   
   sum(nw, n, ne, w, e, sw, s, se)
}

endrow <- dim(mat)[1] - 3
endcol <- dim(mat)[1] - 3

xmas_count <- 0

for (r in 3:endrow) {
   for (c in 3:endcol) {
      if (mat[r,c] == "X") {
         xmas_count <- xmas_count + find_xmas(mat, r, c)
      }
   }
}

xmas_count

# Part 2

find_mas <- function(mat, row, col) {
   all(
   # \
      any(
         all(mat[row - 1, col - 1] == "M", mat[row + 1, col + 1] == "S"),
         all(mat[row - 1, col - 1] == "S", mat[row + 1, col + 1] == "M")
      ),
   # /
      any(
         all(mat[row - 1, col + 1] == "M", mat[row + 1, col - 1] == "S"),
         all(mat[row - 1, col + 1] == "S", mat[row + 1, col - 1] == "M")
      )
   )
}

mas_count <- 0
for (r in 3:endrow) {
   for (c in 3:endcol) {
      if (mat[r,c] == "A") {
         mas_count <- mas_count + find_mas(mat, r, c)
      }
   }
}   

mas_count   
