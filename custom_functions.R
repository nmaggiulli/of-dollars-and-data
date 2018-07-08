# Create GIF function accepts path, filename, and speed argument
create_gif <- function(path, file_stub, speed_milliseconds, n_loops = 0, out_name = "all_plots.gif"){
  path <- gsub(" ", "\\\ ", path, fixed = TRUE)
  system(paste0("convert -delay ", speed_milliseconds, " -loop ", n_loops, " ", path, "/", file_stub ," ",  path, "/", out_name))
}