require(httr2)
require(glue)

get_aoc_input <- function(year, day, verbose = FALSE, force_update = FALSE) {
   local_file = glue::glue("inputs/input-{day}.txt")
   uri <- glue::glue("https://adventofcode.com/{year}/day/{day}/input", year = year, day = day)
   sessionCookie <- Sys.getenv("AOC_SESSION")
   
   if (file.exists(local_file) && !force_update) {
      rlang::inform("Input already downloaded, returning its location")
      return(local_file)
   }
   
   if (sessionCookie == "") {
      rlang::abort("Session cookie not set in .Renviron file. Please set AOC_SESSION.")
   }
   sessionCookie <- glue::glue("session={session_cookie}", session_cookie = sessionCookie)
   
   req <- httr2::request(uri) |> httr2::req_headers(Cookie = sessionCookie)
   
   rlang::inform("Sending HTTPS request to download input.")
   resp <- req_perform(req, path = local_file)
   
   if (verbose) {
      print(resp)
   }
   
   return(local_file)
}
