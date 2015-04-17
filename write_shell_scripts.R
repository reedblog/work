#
# write.sh()
# R function to write shell script files
#
# Reed Sorensen, March 2015
#


write.sh <- function(shell_file, run_file, notify_b = TRUE, notify_e = TRUE) {

  notify_params <- paste0(
    ifelse(notify_b, "b", ""),
    ifelse(notify_e, "e", ""))

  notify_txt <- ifelse(notify_params == "", "",
    paste0("#$ -m ", notify_params))

  find_periods <- gregexpr(pattern = "\\.", text = run_file)[[1]]
  last_period <- find_periods[length(find_periods)]
  file_suffix <- substr(run_file, start = last_period, stop = nchar(run_file))

  if (tolower(file_suffix) == ".r") { exe <- "/usr/local/bin/R --no-save <"}
  if (tolower(file_suffix) == ".do") { exe <- "/usr/local/stata11/stata-mp –b do" }
  if (tolower(file_suffix) == ".py") { exe <- "/usr/local/bin/python" }

  writeLines(
    c(
      "#!/bin/sh",
      notify_txt,
      paste(exe, run_file)
    ),
    con = file(shell_file))

}


params <- read.csv("~/prog/work_projects/shell_script_list.csv", stringsAsFactors = FALSE)
for (i in 1:nrow(params)) { do.call("write.sh", as.list(params[i, ])) }





