if (!require("RMySQL")) install.packages("RMySQL")
if (!require("RSQLite")) install.packages("RSQLite")
library(RMySQL)
library(RSQLite)

####################################
#GLOBAL VARIABLES DECLARATION
####################################

dbConnectionMysql <- NA
dbConnection <- NA
year.df <- NA
journal.facts <- NA

#data frame to hold rows for date dimension tables
datedim.cols <- c("date_id","year","month","quarter")
date.df <- data.frame(matrix(nrow=0, ncol = length(datedim.cols)))
colnames(date.df) <- datedim.cols
date_id_count <- 0

#-----------------------------------
#End of global variables
#-----------------------------------


####################################################
#This function establishes connection with SQLite db
#####################################################

connectToSQLite<-function()
{
  #fpath is the path of the current working directory where the database will be created and stored
  fpath = getwd()
  
  dbfile = 'PublicationsDB.sqlite'
  #if database file already exists, it gets connected.
  #Otherwise new database gets created
  
  dbConnection <<- dbConnect(RSQLite::SQLite(),paste0(fpath,'/',dbfile))
}

##################################################################
#This function establishes MYSQL connection.
#It drops the database if it already exists, creates a new database.
#Sets the current database to the newly created database.
##################################################################

connectToMySQL<-function()
{
  # 1. Settings
  db_user <- 'root'
  db_password <- 'root'
  db_name <- 'journalDW'
  db_host <- 'localhost'
  db_port <- 3306
  
  # 2. Establish connection
  dbConnectionMysql <<-  dbConnect(MySQL(), user = db_user, password = db_password,host = db_host, port = db_port)
  dropDBqry <- paste0("DROP DATABASE IF EXISTS ",db_name)
  createDBqry <- paste0("CREATE DATABASE IF NOT EXISTS ",db_name)
  useDBqry <- paste0("USE ",db_name)
  dbExecute(dbConnectionMysql,dropDBqry)
  dbExecute(dbConnectionMysql,createDBqry)
  dbExecute(dbConnectionMysql,useDBqry)
}

#########################################################################################
#This functions kills the database connections if they 
#are openend in the background.This function is used only during testing.
#It needs to be used when user forgets to disconnect the database.
#The connection would still be open in the background. When there are more than
#16 connections opened like this in the background, you cannot create anymore connections.
#This functions ensures killing all suchh opened connections.
###################################################################

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

#############################################################
#This function extracts all the possible years present in the
#journal issues table
#############################################################
getYears <- function() 
{
  years_qry <- "select distinct(strftime('%Y',pub_date)) from journalIssues"
  year.df <<- dbGetQuery(dbConnection,years_qry)
  View(year.df)
}

############################################################
#This function constructs the date dimension table
#Here after getting the possible years from the function getYears,
#We create a date dimension by creating records for all the months 
#with that particular year.
############################################################

constructDfDateDimension <- function()
{
  for(z in 1:nrow(year.df))
  {
    year_t <- year.df[z, 1]
    if(!is.na(year_t)){
      for(y in 1:12)
      {
        date_id_count <<- date_id_count + 1
        date.df[date_id_count,1] <<- date_id_count
        date.df[date_id_count,2] <<- year_t
        date.df[date_id_count,3] <<- y
        
        if(y >= 1  & y <=3)
        {
          date.df[date_id_count,4] <<- 1
        }
        if(y >=4  & y <=6)
        {
          date.df[date_id_count,4] <<- 2
        }
        if(y >= 7  & y <=9)
        {
          date.df[date_id_count,4] <<- 3
        }
        if(y >= 10  & y <=12)
        {
          date.df[date_id_count,4] <<- 4
        }
      }
    }
  }
}

#########################################################
#Drop pre existing tables from Mysql database if they exist
##########################################################
dropPreExistingMysqlTables <- function()
{
  drop_time_dim_table <- "DROP TABLE IF EXISTS date_dim"
  dbExecute(dbConnectionMysql,drop_time_dim_table)
  
  drop_jfact <- "DROP TABLE IF EXISTS journalfact"
  dbExecute(dbConnectionMysql,drop_jfact)
}
####################################################################
#Create a mysql table for date dimension table and fill it with data
####################################################################
createPopulateDimTables <- function()
{
  create_date_dim_table <- "CREATE TABLE date_dim
  (
    date_id INTEGER NOT NULL,
    year INTEGER NOT NULL,
    month INTEGER NOT NULL,
    quarter INTEGER NOT NULL,
    PRIMARY KEY(date_id)
  )"
  dbExecute(dbConnectionMysql,create_date_dim_table)
  dbWriteTable(dbConnectionMysql, "date_dim", date.df,append=TRUE,row.names=F)
  dbWriteTable(dbConnection, "date_dim", date.df,append=TRUE,row.names=F)
}

#######################################################################
#execute a query to get journal facts by querying sqlite databases and stores
#the results in a data frame
#######################################################################
extractDataJournalFact <- function()
{
  journalfact_qry <- "select jid,jtitle,date_id,Article_count,Author_count from (select jid,jtitle,date_id,cast((strftime('%m',pub_date)) as INTEGER) as 'MONTH1',cast((strftime('%Y',pub_date)) as INTEGER) as 'YEAR1', COUNT(DISTINCT(a.article_id)) as 'Article_count',COUNT(DISTINCT(aid)) as 'Author_count' FROM journals j inner join journalIssues ji on ji.j_issue_id=j.journalIssue left join journalLinks l on j.jid=l.journal_id left join articles a on a.article_id= l.article_id left join authorship au on au.art_id = l.article_id join date_dim dt WHERE dt.month=MONTH1 AND dt.year=YEAR1 group by jid)"
  journal.facts <<- dbGetQuery(dbConnection,journalfact_qry)
  View(journal.facts)
}

#######################################################
#create mysql face table for journal fact
########################################################
createMysqlFactTable <- function()
{
  create_journal_fact <- "CREATE TABLE journalfact
  (
    jid INTEGER NOT NULL,
    jtitle TEXT NOT NULL,
    date_id INTEGER NOT NULL,
    Article_count INTEGER NOT NULL,
    Author_count INTEGER NOT NULL,
    PRIMARY KEY(jid),
    FOREIGN KEY(date_id) REFERENCES date_dim(date_id) ON DELETE CASCADE ON UPDATE CASCADE
  )"
  dbExecute(dbConnectionMysql,create_journal_fact)
}

#########################################################################################
#This functions writes the data extracted from the sqlite table stored in a data frame
#into mysql table
##########################################################################################
insertDataToMysqlFactTables <- function()
{
  dbWriteTable(dbConnectionMysql, "journalfact", journal.facts,append=TRUE,row.names=F)
}

################################################################################
#This function is used to verify the data in the SQLITE database tables
#It queries the SQLite tables and returns the sample data from every table
################################################################################

verifyDataLoadedSQLite <- function() {
  au_qry <- "SELECT * FROM authors LIMIT 100"
  verify_authors <- dbGetQuery(dbConnection,au_qry)
  View(verify_authors)
  
  j_qry <- "SELECT * FROM journals LIMIT 100"
  verify_journals <- dbGetQuery(dbConnection,j_qry)
  View(verify_journals)
  
  art_qry <- "SELECT * FROM articles LIMIT 100"
  verify_articles <- dbGetQuery(dbConnection,art_qry)
  View(verify_articles)
  
  j_art_qry <- "SELECT * FROM journalLinks LIMIT 100"
  verify_jlinks <- dbGetQuery(dbConnection,j_art_qry)
  View(verify_jlinks)
  
  art_jou_qry <- "SELECT * FROM authorship LIMIT 100"
  verify_authorship <- dbGetQuery(dbConnection,art_jou_qry)
  View(verify_authorship)
  
  jou_iss_qry <- "SELECT * FROM journalIssues LIMIT 100"
  verify_jou_issue_qry <- dbGetQuery(dbConnection,jou_iss_qry)
  View(verify_jou_issue_qry)
}

######################################################
#This function executes the sample queries for part 2 of the practicum
########################################################

sampleQueriesForFactTables <- function(){  
  
  #What the are number of articles published in every journal in 1975 and 1976?
  j_qry1 <- "SELECT jid,jtitle,year,Article_count FROM journalfact jf inner join date_dim on date_dim.date_id = jf.date_id WHERE year=1975 OR year=1976"
  query1 <- dbGetQuery(dbConnectionMysql,j_qry1)
  View(query1)
  
  #What is the number of articles published in every journal in each quarter of 1975 through 1976?
  j_qry2 <- "SELECT jid,jtitle,year,Article_count FROM journalfact jf inner join date_dim on date_dim.date_id = jf.date_id WHERE year BETWEEN 1975 AND 1978"
  query2 <- dbGetQuery(dbConnectionMysql,j_qry2)
  View(query2)
  
  #How many articles were published each quarter (across all years)?
  j_qry3 <- "SELECT Quarter,SUM(Article_count) FROM journalfact jf inner join date_dim on date_dim.date_id = jf.date_id Group by Quarter"
  query3 <- dbGetQuery(dbConnectionMysql,j_qry3)
  View(query3)
  
  #How many unique authors published articles in each year for which there is data?
  j_qry4 <- "SELECT Year,SUM(Author_count) FROM journalfact jf inner join date_dim on date_dim.date_id = jf.date_id Group by year"
  query4 <- dbGetQuery(dbConnectionMysql,j_qry4)
  View(query4)
}

#############################################
#This function closes the database connection
#############################################
closeConnection<-function()
{
  dbDisconnect(dbConnection)
  dbDisconnect(dbConnectionMysql)
}

loadDWhouseEngine <- function()
{
  print("Started loading data warehouse .....")
  print("........... In progress ................")
  #killDbConnections()
  connectToSQLite()
  connectToMySQL()
  getYears()
  constructDfDateDimension()
  dropPreExistingMysqlTables()
  createPopulateDimTables()
  extractDataJournalFact()
  createMysqlFactTable()
  insertDataToMysqlFactTables()
  #verifyDataLoadedSQLite()
  sampleQueriesForFactTables()
  closeConnection()
  print("Loading datawarehouse completed!")
}

#start the engine to load the data warehouse
loadDWhouseEngine()