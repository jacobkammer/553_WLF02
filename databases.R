# <- alt dash to insert an assignment operator

#Load packages database
library(RSQLite)

R.version
?DBI

#consult vignettes ####
vignette("spec", package = "DBI")#explains how to use spec function in the DBI package

vignette(package= "DBI")#list vignettes available in the DBI package
vignette("DBI", package = "DBI")#101 version on how to use the package


#connect to dragon database ####
library(DBI)
dragons_db <- dbConnect(drv = RSQLite::SQLite(), "dragons.db")#second argument with quotes is the
#path to the database

#alternate method do not load DBI by use :: to indicate where the function
#dbConnect is coming from
#db_dragons <- DBI::dbConnect(drv = RSQLite::SQLite(, "dragons.db))

#Query data from the database ####
##Query n.1 #### 

#-----------------------------
#double ## nests query n.1 under connect to dragon database
# get all data 
#------------------------------------

dragons <-dbGetQuery(conn = dragons_db,
           statement = "SELECT * FROM dragons;")
class(dragons)

#get all data from captures

captures_sites <- dbGetQuery(conn = dragons_db,
                             statement = "SELECT * FROM capture_sites;")


DBI::dbListTables(dragons_db)#returns a list of all the table in the dragons database

dbGetQuery(dragons_db, "SELECT DISTINCT dragon_id from Dragons;")


db_testOne <- dbConnect(RSQLite::SQLite(), "testOne.db")
#create tables ####
dbExecute(dragons_db, 
          "CREATE TABLE fake_table (
          name varchar NOT NULL PRIMARY KEY,
          number real,
          animal varchar
          );")
?dbConnect

#dbWriteTable

#---------------------------------------

