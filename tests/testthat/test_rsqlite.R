
# Setup ----

stopifnot(require(RSQLite))

database_file <- tempfile()
con <- dbConnect(SQLite(), dbname = database_file)
# NOTE: dbWriteTable does not write the row names, by default (row.names=TRUE)
dbWriteTable(con, "mtcars", mtcars[1:5, ])
dbWriteTable(con, "USArrests", USArrests[1:5, ])
dbDisconnect(con)

# sqliteListTables ----

test_that("sqliteListTables works", {

    res <- sqliteListTables(database_file)

    expect_identical(res, c("USArrests", "mtcars"))

})

# sqliteReadTable ----

test_that("sqliteReadTable works", {

    res <- sqliteReadTable("mtcars", database_file)

    expected <- mtcars[1:5, ]
    rownames(expected) <- NULL

    expect_identical(res, expected)
})

# sqliteQuery ----

test_that("sqliteQuery works", {

    res <- sqliteQuery("select * from mtcars", database_file)

    expected <- mtcars[1:5, ]
    rownames(expected) <- NULL

    expect_identical(res, expected)
})

# sqliteTableExists ----

test_that("sqliteTableExists works", {

    expect_false(sqliteTableExists("absent", database_file))

    expect_true(sqliteTableExists("USArrests", database_file))
    expect_true(sqliteTableExists("mtcars", database_file))

})

# sqliteListFields ----

test_that("sqliteTableExists works", {

    expect_identical(
        sqliteListFields("USArrests", database_file),
        colnames(USArrests)
    )

    expect_identical(
        sqliteListFields("mtcars", database_file),
        colnames(mtcars)
    )

})
