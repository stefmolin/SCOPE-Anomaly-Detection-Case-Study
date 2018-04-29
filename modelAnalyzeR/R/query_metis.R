#' @title Query Metis
#' @description Query Metis for case study data and classifications
#' @author Stefanie Molin
#'
#' @import DBI
#' @import RPostgreSQL
#' @import yaml
#'
#' @param query PostgreSQL query as a string
#' 
#' @return Dataframe with the results
#'
#' @export
query_metis <- function(query){
  # get URI
  db_config <- yaml::yaml.load_file("config.yml")[['metis_db']]
  
  # make connection
  conn <- DBI::dbConnect(DBI::dbDriver("PostgreSQL"), host = db_config[['host']], 
                         dbname = db_config[['dbname']], user = db_config[['username']], 
                         password = db_config[['password']], port = db_config[['port']])
  data <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)
  return(data)
}