#
# code for connecting to IHME databases
#

require(RMySQL)
require(dplyr)

hpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "H:/", "/homes/rsoren/")
login <- readRDS(paste0(hpath, "prog/data/_utility_code/login.RDS"))

# dbListConnections(dbDriver(drv = "MySQL")) # list connections
# lapply(dbListConnections(dbDriver( drv = "MySQL")), dbDisconnect) # close all

#-- save new login info
#
# login <- rbind(login,
#   data.frame(
#     host = "",
#     user = "",
#     password = ""))
#
# saveRDS(login, paste0(hpath, "prog/data/_utility_code/login.RDS"))
#


#-- connect to 'newhalem' server and list databases

server1 <- dbConnect(MySQL(),
  host = "newhalem.ihme.washington.edu",
  port = 3306,
  user = as.character(login[login$host == "newhalem.ihme.washington.edu", "user"]),
  password = as.character(login[login$host == "newhalem.ihme.washington.edu", "password"])
)
dbGetQuery(server1, 'show databases;')


#-- connect to specific database, list tables and read one in

db1 <- dbConnect(MySQL(),
  dbname = "epi",
  host = "newhalem.ihme.washington.edu",
  port = 3306,
  user = as.character(login[login$host == "newhalem.ihme.washington.edu", "user"]),
  password = as.character(login[login$host == "newhalem.ihme.washington.edu", "password"])
)

dbListTables(db1)
df <- dbReadTable(db1, "covariates")


#-- connect to 'fbd' database for forecasting

db2 <- dbConnect(MySQL(),
  dbname = "fbd",
  host = "krheuton-dev.ihme.washington.edu",
  port = 3306,
  user = as.character(login[login$host == "krheuton-dev.ihme.washington.edu", "user"]),
  password = as.character(login[login$host == "krheuton-dev.ihme.washington.edu", "password"])
)

dbListTables(db2)
df2 <- dbReadTable(db2, "past_attributes")




