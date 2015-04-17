# J drive file path, conditional on operating system

j_drive <- ifelse(Sys.info()[["sysname"]] == "Windows", "J:/", "/snfs1/")

h_drive <- ifelse(Sys.info()[["sysname"]] == "Windows", "H:/",
  paste0("/homes/", Sys.info()[["user"]], "/"))

on_cluster <- ifelse(Sys.info()[["sysname"]] == "Windows", FALSE, TRUE)
on_windows <- ifelse(Sys.info()[["sysname"]] == "Windows", TRUE, FALSE)
