if (!require("XML")) install.packages("XML")
if (!require("RSQLite")) install.packages("RSQLite")
if (!require("hash")) install.packages("hash")
library(XML)
library(RSQLite)
library(hash)


###############################################################
#GLOBAL VARIABLES
###############################################################

#xml file hosted on s3
xmlURL <- "http://cs5200practicum2.s3.amazonaws.com/pubmed22n0001-tf.xml"
#xmlURL <-"ttt.xml"
#sqlite database connection
dbConnection <- NA

#xml object which houses all the xml nodes
xmlObj <- NA
#hashes initialized to be later on used for parsing date fields
month.map <- hash()
month.map[["jan"]] <- '01'
month.map[["feb"]] <- '02'
month.map[["mar"]] <- '03'
month.map[["apr"]] <- '04'
month.map[["may"]] <- '05'
month.map[["jun"]] <- '06'
month.map[["jul"]] <- '07'
month.map[["aug"]] <- '08'
month.map[["sep"]] <- '09'
month.map[["oct"]] <- '10'
month.map[["nov"]] <- '11'
month.map[["dec"]] <- '12'


#hashes used for parsing season
season.map <- hash()
season.map[["summer"]] <- "jun"
season.map[["winter"]] <- "dec"
season.map[["fall"]] <- "sep"
season.map[["spring"]] <- "mar"

#global count variables to keep the row count for all the tables
authors.count <- 0
articles.count <- 0
journals.count <- 0
art.jou.link.count <- 0
art.au.link.count <- 0
journal.issue.count <- 0

# data frame for authors table
au.cols <- c("aid",'author_cmbnd_key',"fname","lname","initials","suffix","affiliation")
authors.df <- data.frame(matrix(nrow=0, ncol = length(au.cols)))
colnames(authors.df) <- au.cols

# data frame for articles table
articles.cols <- c("article_id","pmid","articleTitle","article_cmbnd_key")
articles.df <- data.frame(matrix(nrow=0, ncol = length(articles.cols)))
colnames(articles.df) <- articles.cols

# data frame for journals table
journal.cols <- c("jid","issn","issnType","jtitle","isoabbr","journalIssue","citedMedium","journal_cmbnd_key")
journals.df <- data.frame(matrix(nrow=0, ncol = length(journal.cols)))
colnames(journals.df) <- journal.cols

#data frame article-journal link table
art.jou.link.cols <- c("article_id","journal_id")
art.jou.link.df <- data.frame(matrix(nrow=0, ncol = length(art.jou.link.cols)))
colnames(art.jou.link.df) <- art.jou.link.cols

#data frame for article-author link
art.au.link.cols <- c("art_id","aid","arau_cmbnd_key")
art.au.link.df <- data.frame(matrix(nrow=0, ncol = length(art.au.link.cols)))
colnames(art.au.link.df) <- art.au.link.cols

#data frame for journal issue table
journal.issue.link.cols <- c("j_issue_id","volume","issue","pub_date","jissue_cmbnd_key")
journals.issue.df <- data.frame(matrix(nrow=0, ncol = length(journal.issue.link.cols)))
colnames(journals.issue.df) <- journal.issue.link.cols


#Extra columns would be added for every dataframes to check for duplicate identifier
#These extra columns will be removed from dataframes and the data will be put in below dataframes
#to exactly match the table formats

authors.sqlite.df <- data.frame(matrix(nrow=0, ncol = length(au.cols)-1))
articles.sqlite.df <- data.frame(matrix(nrow=0, ncol = length(articles.cols)-1))
journals.sqlite.df <- data.frame(matrix(nrow=0, ncol = length(journal.cols)-1))
art.au.sqlite.df <- data.frame(matrix(nrow=0, ncol = length(art.au.link.cols)-1))
journal.issue.sqlite.df <- data.frame(matrix(nrow=0, ncol = length(journal.issue.link.cols)-1))

#------------------------------------------
#Global variables declaration ENDS here
#------------------------------------------


###########################################################################################################
#This function parses the xml by validating it with dtd and returns xml Object which contains all the nodes
###########################################################################################################

loadXml <- function() {
  xmlObj <<- xmlParse(xmlURL,validate=T)
}

################################################################
#Helper function to create date from inputs in yyyy-mm-dd format
################################################################

constructDate <- function( month, day, year ) {
  
  if(is.na(month))
    month <- "dec"
  if(is.na(day))
    day<-"15"
  if(is.na(year))
    year<-"1111"
  
  if(isTRUE(has.key(month,month.map))){
    month_t <- month.map[[month]]
  }
  else
  {
    month_t <- month
  }
  
  if(nchar(day)==1) {
    day_t <- paste0('0',day)
  }
  else {
    day_t <- day
  }
  
  final_dt = paste(sep="-",year,month_t,day_t)
  
  return(final_dt)
}

#########################################################################
#This function iterates through all the article nodes one by one and parses
#data from each node into variables and later on inserts them into appropriate
#data frames accordingly
#########################################################################
parseArticleNodes <- function()
{
  
  #xpath to get all the article nodes
  xpath.articles <- paste0("//Publications/Article")
  article.nodes <- xpathSApply(xmlObj, xpath.articles)
  articles.length <- length(article.nodes)
  
  for(i in 1:articles.length)
  {
    article.node <- article.nodes[[i]]
    articles.attrs <- xmlAttrs(article.node)
    
    pmid <- trimws(articles.attrs[[1]])
    pubDetails <- article.node[[1]]
    title <- trimws(xmlValue(pubDetails[[2]]))
    
    #construct identifier to check for comparing duplicate records
    article_cmbnd_key <- paste(sep="_",pmid,title)
    article.res <- article_cmbnd_key %in% authors.df$article_cmbnd_key
    
    if(isFALSE((article.res)))
    {
      articles.count <<- articles.count + 1
      articles.df[articles.count, 1] <<- articles.count
      articles.df[articles.count, 2] <<- pmid
      articles.df[articles.count, 3] <<- title
      articles.df[articles.count, 4] <<- article_cmbnd_key
    }
    
    pubDetails.child.nodes <- xmlChildren(pubDetails)
    pubDetails.child.nodes.len <- length(pubDetails.child.nodes)
    
    journal.node <- pubDetails[[1]]
    journal.child.nodes <- xmlChildren(journal.node)
    journal.child.nodes.len <- length(journal.child.nodes)
    
    isoAbbr <- "-NA-"
    journalIssue <- "-NA-"
    jtitle <- "-NA-"
    issn <- "-NA-"
    issnType <- "-NA-"
  
    for(p in 1:journal.child.nodes.len) {
      journal <- journal.child.nodes[[p]]
      nodeName  <- xmlName(journal)
      
      if(nodeName == "Title") {
        jtitle <- trimws(xmlValue(journal))
      }
      
      if(nodeName == "ISSN") {
        issn <- trimws(xmlValue(journal))
        issn.attrs <- xmlAttrs(journal)
        issnType <- trimws(issn.attrs[[1]])
      }
      
      if(nodeName == "ISOAbbreviation") {
        isoAbbr <- trimws(xmlValue(journal))
      }
      
      if(nodeName == "JournalIssue") {
        volume <- "-NA-"
        issue <- "-NA-"
        
        journalIssue.child.nodes <- xmlChildren(journal)
        journalIssue.attrs <- xmlAttrs(journal)
        citedMedium <- journalIssue.attrs[[1]]
        journalIssue.child.nodes.len <- length(journalIssue.child.nodes)
        
        for(j in 1:journalIssue.child.nodes.len) {
          journalIssue <- journalIssue.child.nodes[[j]]
          nodeName  <- xmlName(journalIssue)
          
          if(nodeName == "Volume") {
            volume <- trimws(xmlValue(journalIssue))
          }
          if(nodeName == "Issue") {
            issue <- trimws(xmlValue(journalIssue))
          }
          
          if(nodeName == "PubDate") {
            
            pubdate.child.nodes <- xmlChildren(journalIssue)
            pubdate.child.nodes.len <- length(pubdate.child.nodes)
            
            year <- NA
            month <- NA
            day <- NA
            medline_dt <- NA
            
            for(k in 1:pubdate.child.nodes.len) {
              
              pubdate <- pubdate.child.nodes[[k]]
              nodeName  <- xmlName(pubdate)
              
              if(nodeName == "Year") {
                year <- trimws(xmlValue(pubdate))
              }
              
              if(nodeName == "Month") {
                month <- trimws(tolower(xmlValue(pubdate)))
              }
              
              if(nodeName == "Day") {
                day <- trimws(xmlValue(pubdate))
              }
              
              if(nodeName == "MedlineDate") {
                medline_dt <- trimws(xmlValue(pubdate))
              }
              
              if(nodeName == "Season") {
                season <- trimws(tolower(xmlValue(pubdate)))
                month <- season.map[[season]]
              }
            }
            
            if(isFALSE(is.na(medline_dt))) {
              
              medline_split <- strsplit(medline_dt,split=" ")
              year1 <- medline_split[[1]][1]
              year2<- strsplit(year1,split="-")
              year <- year2[[1]][1]
              
              len <- length(medline_split[[1]])
              
              if(len > 1) {
                month1 <- medline_split[[1]][2]
                month2 <- strsplit(month1,split="-")
                month3 <- tolower(month2[[1]][1])
                
                if(isTRUE(has.key(month3,season.map))) {
                  month <- season.map[[month3]]
                }
                else {
                  month <- month3
                }
                
                if(len > 2) {
                  day1 <- medline_split[[1]][3]
                  day2 <- strsplit(day1,split="-")
                  day3 <- tolower(day2[[1]][1])
                  
                  if(isTRUE(has.key(day3,month.map))) {
                    day <-"15" 
                  }
                  else {
                    day <- day3
                  }
                }
              }
            }
            final_dt <- constructDate(month,day,year)
          }
        }
        
        journal_issue_cmbnd_key <- paste(sep="_",volume,issue,final_dt)
        journal.issue.res <- journal_issue_cmbnd_key %in% journals.issue.df$jissue_cmbnd_key
           
        if(isFALSE((journal.issue.res)))
        {
          journal.issue.count <<- journal.issue.count + 1
          journals.issue.df[journal.issue.count, 1] <<- journal.issue.count
          journals.issue.df[journal.issue.count, 2] <<- volume
          journals.issue.df[journal.issue.count, 3] <<- issue
          journals.issue.df[journal.issue.count, 4] <<- final_dt
          journals.issue.df[journal.issue.count, 5] <<- journal_issue_cmbnd_key
        }
      }
    }
    
    journal_cmbnd_key <- paste(sep="_",issn,issnType,jtitle,isoAbbr,journal.issue.count,citedMedium)
    journal.res <- journal_cmbnd_key %in% journals.df$journal_cmbnd_key
  
    if(isFALSE(journal.res))
    {
      journals.count <<- journals.count + 1
      journals.df[journals.count,1] <<- journals.count
      journals.df[journals.count,2] <<- issn
      journals.df[journals.count,3] <<- issnType
      journals.df[journals.count,4] <<- jtitle
      journals.df[journals.count,5] <<- isoAbbr
      journals.df[journals.count,6] <<- journal.issue.count
      journals.df[journals.count,7] <<- citedMedium
      journals.df[journals.count,8] <<- journal_cmbnd_key
      art.jou.link.count <<- art.jou.link.count + 1
      art.jou.link.df[art.jou.link.count,1] <<- articles.count
      art.jou.link.df[art.jou.link.count,2] <<- journals.count
    }
    else
    {
      selected_jou <- journals.df[which(journals.df$journal_cmbnd_key == journal_cmbnd_key), ]
      art.jou.link.count <<- art.jou.link.count + 1
      art.jou.link.df[art.jou.link.count,1] <<- articles.count
      art.jou.link.df[art.jou.link.count,2] <<- selected_jou[1,1]
    }

    if(pubDetails.child.nodes.len==3){
    
    authorList.node <- pubDetails[[3]]
    authorList.child.nodes <- xmlChildren(authorList.node)
    authorList.child.nodes.len <- length(authorList.child.nodes)
    
    for(m in 1:authorList.child.nodes.len)
    {
      author = authorList.child.nodes[[m]]
      author.child.nodes <- xmlChildren(author)
      author.child.nodes.len <- length(author.child.nodes)
      author.attr <- xmlAttrs(author)
      validYN <- author.attr[[1]] 
      
      lname <- "-NA-"
      fname <- "-NA-"
      initials <- "-NA-"
      affiliation <- "-NA-"
      suffix <- "-NA-"
      cname <- "-NA-"
      
      for(n in 1:author.child.nodes.len)
      { 
        authorElement <- author.child.nodes[[n]]
        nodeName  <- xmlName(authorElement)
        
        if(nodeName == "LastName")
        {
          lname <- trimws(xmlValue(authorElement))
        }
        
        if(nodeName == "ForeName")
        {
          fname <- trimws(xmlValue(authorElement))
        }
        
        if(nodeName == "Initials")
        {
          initials <- trimws(xmlValue(authorElement))
        }
        
        if(nodeName == "CollectiveName")
        {
          collectiveName <-  trimws(xmlValue(authorElement))
        }
        
        if(nodeName == "Initials")
        {
          initials <- trimws(xmlValue(authorElement))
        }
        
        if(nodeName == "AffiliationInfo")
        {
          affInfo.child.nodes <- xmlChildren(authorElement)
          affiliation <- trimws(xmlValue(affInfo.child.nodes[[1]]))
        }
        
        if(nodeName == "Suffix")
        {
          suffix <-  trimws(xmlValue(authorElement))
        }
        
        if(nodeName == "CollectiveName")
        {
          cname <-  trimws(xmlValue(authorElement))
        }
      }
      
      if(is.na(lname))
      {
        lname=cname
      }
      
      author_cmbnd_key = paste(sep="_",fname,lname,initials,suffix,affiliation)
      res <- author_cmbnd_key %in% authors.df$author_cmbnd_key
      if(isFALSE(res))
      { 
        authors.count <<- authors.count + 1
        authors.df[authors.count,1] <<- authors.count
        authors.df[authors.count,3] <<- fname
        authors.df[authors.count,4] <<- lname
        authors.df[authors.count,5] <<- initials
        authors.df[authors.count,6] <<- suffix
        authors.df[authors.count,7] <<- affiliation
        authors.df[authors.count,2] <<- author_cmbnd_key
        selected_au <- authors.df[which(authors.df$author_cmbnd_key == author_cmbnd_key), ]
        arau_cmbnd_key = paste(sep="_",articles.count,selected_au[1,1])
        arau_res <- arau_cmbnd_key %in% art.au.link.df$arau_cmbnd_key
        if(isFALSE(arau_res))
        {
          art.au.link.count <<- art.au.link.count + 1
          art.au.link.df[art.au.link.count,1] <<- articles.count
          art.au.link.df[art.au.link.count,2] <<- selected_au[1,1]
          art.au.link.df[art.au.link.count,3] <<- arau_cmbnd_key
        }
        else
        {
          print(articles.count)
          print(arau_cmbnd_key)
        }
      }
      else
      {
        selected_au <- authors.df[which(authors.df$author_cmbnd_key == author_cmbnd_key), ]
        arau_cmbnd_key = paste(sep="_",articles.count,selected_au[1,1])
        arau_res <- arau_cmbnd_key %in% art.au.link.df$arau_cmbnd_key
        if(isFALSE(arau_res))
        {
          art.au.link.count <<- art.au.link.count + 1
          art.au.link.df[art.au.link.count,1] <<- articles.count
          art.au.link.df[art.au.link.count,2] <<- selected_au[1,1]
          art.au.link.df[art.au.link.count,3] <<- arau_cmbnd_key
        }
        else #tbr
        {
          print(articles.count)
          print(arau_cmbnd_key)
        }
      }
    }
  }
}
}

##############################################################################
#This function removes the duplicate row identifier column 
#from data frame and puts them into new dataframes, suitable for loading tables 
##############################################################################

removeDuplicateIdentifierColumns<-function()
{
  authors.sqlite.df <<- authors.df[,-2]
  articles.sqlite.df <<- articles.df[,-4]
  journals.sqlite.df <<- journals.df[,-8]
  art.au.sqlite.df <<- art.au.link.df[,-3]
  journal.issue.sqlite.df <<- journals.issue.df[,-5]
}

###############################################
#This function views all the data frames created
###############################################

viewDataframes<- function() 
{
  View(authors.sqlite.df)
  View(articles.sqlite.df)
  View(journals.sqlite.df)
  View(journal.issue.sqlite.df)
  View(art.jou.link.df)
  View(art.au.sqlite.df)
}

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

##########################################################
#This function drops the pre existing tables from database
##########################################################

dropPreExistingTables<-function() {
  drop_authors <- "DROP TABLE IF EXISTS authors"
  dbExecute(dbConnection,drop_authors)
  drop_articles <- "DROP TABLE IF EXISTS articles"
  dbExecute(dbConnection,drop_articles)
  drop_journals <- "DROP TABLE IF EXISTS journals"
  dbExecute(dbConnection,drop_journals)
  drop_art_jour_link <- "DROP TABLE IF EXISTS journalLinks"
  dbExecute(dbConnection,drop_art_jour_link)
  drop_art_au_link <- "DROP TABLE IF EXISTS authorship"
  dbExecute(dbConnection,drop_art_au_link)
  drop_journal_issues_link <- "DROP TABLE IF EXISTS journalIssues"
  dbExecute(dbConnection,drop_journal_issues_link)
}

##########################################################
#This function creates the tables in database
##########################################################

createTables <- function() {
  create_authors <- "CREATE TABLE authors
  (
    aid INTEGER PRIMARY KEY,
    lname TEXT NOT NULL,
    fname TEXT NOT NULL,
    initials TEXT NOT NULL,
    suffix TEXT NOT NULL, 
    affiliation TEXT NOT NULL
  )"
  dbExecute(dbConnection,create_authors)
    
  create_journal_issues <- "CREATE TABLE journalIssues
  (
    j_issue_id INTEGER PRIMARY KEY,
    volume TEXT NOT NULL,
    issue TEXT NOT NULL,
    pub_date DATE NOT NULL
  )"
  dbExecute(dbConnection,create_journal_issues)
    
  create_journals <- "CREATE TABLE journals
  (
    jid INTEGER PRIMARY KEY,
    issn TEXT NOT NULL DEFAULT '-NA-',
    issnType TEXT NOT NULL DEFAULT 'Print',
    jtitle TEXT NOT NULL DEFAULT '-NA-',
    isoabbr TEXT NOT NULL DEFAULT '-NA-', 
    journalIssue INTEGER NOT NULL,
    citedMedium TEXT NOT NULL DEFAULT 'Print',
    FOREIGN KEY(journalIssue) REFERENCES journalIssues(j_issue_id) ON DELETE CASCADE ON UPDATE CASCADE
  )"
  dbExecute(dbConnection,create_journals)
    
  create_articles <- "CREATE TABLE articles
  (
    article_id INTEGER PRIMARY KEY,
    pmid INTEGER NOT NULL DEFAULT '-1',
    articleTitle TEXT NOT NULL DEFAULT '-NA-'
  )"
  dbExecute(dbConnection,create_articles)
    
  create_art_au_link <- "CREATE TABLE authorship
  (
    art_id INTEGER NOT NULL,
    aid INTEGER NOT NULL,
    FOREIGN KEY(art_id) REFERENCES articles(article_id) ON DELETE CASCADE ON UPDATE CASCADE,
    FOREIGN KEY(aid) REFERENCES authors(aid) ON DELETE CASCADE ON UPDATE CASCADE,
    PRIMARY KEY(art_id, aid)
  )"
  dbExecute(dbConnection,create_art_au_link) 
    
  create_art_jour_link <- "CREATE TABLE journalLinks
  (
    article_id INTEGER NOT NULL,
    journal_id INTEGER NOT NULL,
    FOREIGN KEY(article_id) REFERENCES articles(article_id) ON DELETE CASCADE ON UPDATE CASCADE,
    FOREIGN KEY(journal_id) REFERENCES journals(journal_id) ON DELETE CASCADE ON UPDATE CASCADE,
    PRIMARY KEY(article_id, journal_id)
  )"
  dbExecute(dbConnection,create_art_jour_link)
}

############################################################################################
#This function writes the data from data frames to SQLite database tables created beforehand
############################################################################################

insertDataToTables <- function() {
  
  dbWriteTable(dbConnection,"authors",authors.sqlite.df,append=TRUE,row.names=F) 
  dbWriteTable(dbConnection,"articles",articles.sqlite.df,append=TRUE,row.names=F) 
  dbWriteTable(dbConnection,"journals",journals.sqlite.df,append=TRUE,row.names=F) 
  dbWriteTable(dbConnection,"journalLinks",art.jou.link.df,append=TRUE,row.names=F)
  dbWriteTable(dbConnection,"authorship",art.au.sqlite.df,append=TRUE,row.names=F)
  dbWriteTable(dbConnection,"journalIssues",journal.issue.sqlite.df,append=TRUE,row.names=F)
}

################################################################################
#This function is used to verify if the data has been written correctly in the database tables
#It queries the SQLite tables and returns the sample data from every table
################################################################################

verifyDataLoaded <- function() {
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

#############################################
#This function closes the database connection
#############################################
closeConnection<-function()
{
  dbDisconnect(dbConnection)
}
###########################################################################
###########################################################################
###########################################################################

###########################################################
#This is the main function which drives the entire program
###########################################################

xml2REngine <- function()
{
  print("xml2REngine has started processing the file .....")
  print("........... In progress ................")
  loadXml()
  parseArticleNodes()
  removeDuplicateIdentifierColumns()
  viewDataframes()
  connectToSQLite()
  dropPreExistingTables()
  createTables()
  insertDataToTables()
  verifyDataLoaded()
  closeConnection()
  print("xml2REngine has stopped! Execution completed!")
}

## Call the engine to start the program
xml2REngine()
