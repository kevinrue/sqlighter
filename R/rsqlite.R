
# sqliteListTables ----

#' List table names in an SQLite database
#'
#' @param file Path to the SQLite database file
#'
#' @return A character vector of table names.
#' @export
#'
#' @importFrom RSQLite dbConnect SQLite dbListTables dbDisconnect
#'
#' @examples
#' # Setup ----
#' library(RSQLite)
#'
#' database_file <- tempfile()
#' conn <- dbConnect(SQLite(), dbname = database_file)
#' dbWriteTable(conn, "mtcars", mtcars[1:5, ])
#' dbWriteTable(conn, "USArrests", USArrests[1:5, ])
#' dbDisconnect(conn)
#'
#' # Example ----
#'
#' sqliteListTables(database_file)
sqliteListTables <- function(file){
    stopifnot(file.exists(file))
    conn <- dbConnect(SQLite(), dbname = file)
    table_names <- dbListTables(conn)
    dbDisconnect(conn)
    return(table_names)
}

# sqliteTableExists ----

#' Check if a table exists in the database
#'
#' @param table Name of the table to check for existence
#' @param file Database to query
#'
#' @return A logical value that indicates whether the table exists in the
#' database.
#' @export
#'
#' @examples
#' # Example data ----
#' library(RSQLite)
#'
#' database_file <- tempfile()
#' conn <- dbConnect(SQLite(), dbname = database_file)
#' dbWriteTable(conn, "mtcars", mtcars[1:5, ])
#' dbWriteTable(conn, "USArrests", USArrests[1:5, ])
#' dbDisconnect(conn)
#'
#'
#' # Example ----
#'
#' sqliteTableExists("status_summary", database_file)
sqliteTableExists <- function(table, file)
{
    database_tables <- sqliteListTables(file)
    return(table %in% database_tables)
}

# sqliteListFields ----

#' List fields available in database table
#'
#' @param table Name of the table to inspect
#' @param file Path to the SQLite database file
#' @param ... Other parameters passed on to methods.
#'
#' @return A character vector of field names.
#' @export
#'
#' @importFrom RSQLite dbListFields
#'
#' @examples
#' # Setup ----
#'
#' library(RSQLite)
#'
#' database_file <- tempfile()
#' conn <- dbConnect(SQLite(), dbname = database_file)
#' dbWriteTable(conn, "mtcars", mtcars[1:5, ])
#' dbWriteTable(conn, "USArrests", USArrests[1:5, ])
#' dbDisconnect(conn)
#'
#' # Example ----
#'
#' sqliteListFields("mtcars", database_file)
sqliteListFields <- function(table, file, ...){
    stopifnot(file.exists(file))
    stopifnot(sqliteTableExists(table, file))
    conn <- dbConnect(SQLite(), dbname = file)
    field_names <- dbListFields(conn, table, ...)
    dbDisconnect(conn)
    return(field_names)
}

# sqliteReadTable ----

#' Fetch a full table from an SQLite database
#'
#' @param table Name of the database table to fetch.
#' @param file Path to the SQLite database file
#'
#' @return The table as a data.frame.
#' @export
#'
#' @importFrom RSQLite dbConnect dbGetQuery dbDisconnect SQLite
#'
#' @examples
#' # Setup ----
#'
#' library(RSQLite)
#'
#' database_file <- tempfile()
#' conn <- dbConnect(SQLite(), dbname = database_file)
#' dbWriteTable(conn, "mtcars", mtcars[1:5, ])
#' dbWriteTable(conn, "USArrests", USArrests[1:5, ])
#' dbDisconnect(conn)
#'
#' # Example ----
#'
#' sqliteReadTable("mtcars", database_file)
sqliteReadTable <- function(table, file){
    stopifnot(file.exists(file))
    stopifnot(sqliteTableExists(table, file))
    conn <- dbConnect(RSQLite::SQLite(), dbname = file)
    query <- sprintf("SELECT * FROM %s", table)
    table_data <- dbGetQuery(conn, query)
    dbDisconnect(conn)
    return(table_data)
}

# sqliteQuery ----

#' Fetch the result of a specific query from an SQLite database
#'
#' @param query The SQL query to submit
#' @param file Path to the SQLite database file
#'
#' @return The result of the query as a data.frame.
#' @export
#'
#' @importFrom RSQLite dbConnect dbGetQuery SQLite dbDisconnect
#'
#' @author Kevin Rue-Albrecht and Stephen Sansom
#'
#' @examples
#' # Setup ----
#'
#' library(RSQLite)
#'
#' database_file <- tempfile()
#' conn <- dbConnect(SQLite(), dbname = database_file)
#' dbWriteTable(conn, "mtcars", mtcars[1:5, ])
#' dbDisconnect(conn)
#'
#' # Example ----
#'
#' sqliteQuery("select * from mtcars", database_file)
sqliteQuery <- function(query, file)
{
    stopifnot(file.exists(file))
    conn <- dbConnect(RSQLite::SQLite(), dbname = file)
    table_data <- dbGetQuery(conn, query)
    dbDisconnect(conn)
    return(table_data)
}
