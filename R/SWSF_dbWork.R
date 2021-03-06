########################
#------ dbWork functions

add_dbWork_index <- function(con) {
  prev_indices <- DBI::dbGetQuery(con, "SELECT * FROM sqlite_master WHERE type = 'index'")

  if (NROW(prev_indices) == 0L || !("i_runIDs" %in% prev_indices[, "name"])) {
    DBI::dbGetQuery(con, paste("CREATE INDEX i_runIDs ON work (runID_total, runID_sites,",
      "include_YN)"))
  }
}



#' Create a SQLite-database \code{dbWork} to manage runs fo a SWSF simulation project
#'
#' @param dbWork A character string. Path to the folder where the database will be created.
#' @param jobs An integer matrix. Each row corresponds to one call of the simulation
#'  function \code{do_OneSite}, i.e., \code{runsN_master} x \code{expN}. The columns
#'  \code{runID_total}, \code{runID_sites}, \code{include_YN} represent a running ID,
#'  the site_id (row number in master input file), and a flag whether site is being
#'  simulated or not. See \code{\link{indices}}.
#' @return Invisibly \code{TRUE}
create_dbWork <- function(dbWork, jobs) {
  stopifnot(colnames_job_df() %in% dimnames(jobs)[[2]])

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RWC)
  DBI::dbExecute(con,
    paste("CREATE TABLE work(runID_total INTEGER PRIMARY KEY,",
    "runID_sites INTEGER NOT NULL, include_YN INTEGER NOT NULL,",
    "completed INTEGER NOT NULL, failed INTEGER NOT NULL, inwork INTEGER NOT NULL,",
    "time_s REAL)"))

  RSQLite::dbWriteTable(con, "work", append = TRUE, value = as.data.frame(jobs))
  add_dbWork_index(con)

  RSQLite::dbDisconnect(con)
}

colnames_job_df <- function() {
  c("runID_total", "runID_sites", "include_YN", "completed", "failed", "inwork", "time_s")
}

create_job_df <- function(runsN_master, runsN_total, expN, include_YN) {
  temp <- colnames_job_df()
  jobs <- matrix(data = 0L, nrow = runsN_total, ncol = length(temp),
    dimnames = list(NULL, temp))

  jobs[, "runID_total"] <- seq_len(runsN_total)
  jobs[, "runID_sites"] <- rep(seq_len(runsN_master), times = max(expN, 1L))
  temp <- rep(include_YN, times = max(expN, 1L))
  jobs[temp, "include_YN"] <- 1L

  jobs
}


#' Setup or connect to SQLite-database \code{dbWork} to manage runs fo a SWSF simulation
#'  project
#'
#' @inheritParams create_dbWork
#' @param continueAfterAbort A logical value. If \code{TRUE} and \code{dbWork} exists,
#'  then function connects to the existing database. If \code{FALSE}, then a new database
#'  is created (possibly overwriting an existing one).
#' @return A logical value indicating success/failure of setting up/connecting to
#'  \code{dbWork} and initializing with \code{runIDs}.
setup_dbWork <- function(path, runsN_master, runsN_total, expN, include_YN,
  continueAfterAbort = FALSE) {

  dbWork <- file.path(path, "dbWork.sqlite3")
  success <- create <- FALSE
  jobs <- create_job_df(runsN_master, runsN_total, expN, include_YN)

  if (continueAfterAbort) {
    if (file.exists(dbWork)) {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)

      if (any(!(dimnames(jobs)[[2]] %in% DBI::dbListFields(con, "work")))) {
        stop("'setup_dbWork': dbWork is misspecified or outdated; you may fix ",
          "this by first calling the function 'recreate_dbWork'")
      }
      prev_work <- RSQLite::dbGetQuery(con, paste("SELECT runID_total, runID_sites,",
        "include_YN FROM work ORDER BY runID_total"))

      # check whether same design
      success <- all(sapply(c("runID_total", "runID_sites"), function(x)
        identical(prev_work[, x], as.integer(jobs[, x]))))

      if (success) {
        # clean-up potentially lingering 'inwork'
        DBI::dbExecute(con, "UPDATE work SET inwork = 0 WHERE inwork > 0")

        # check whether include_YN has been changed and update if necessary
        if (!identical(prev_work[, "include_YN"], jobs[, "include_YN"])) {
          # now excluded
          iwork <- which(prev_work[, "include_YN"] == 1L & jobs[, "include_YN"] == 0L)
          if (length(iwork) > 0) {
            rs <- DBI::dbSendStatement(con, paste("UPDATE work SET include_YN = 0",
              "WHERE runID_total = :x"))
            DBI::dbBind(rs, param = list(x = iwork))
          }
          # now included
          iwork <- which(prev_work[, "include_YN"] == 0L & jobs[, "include_YN"] == 1L)
          if (length(iwork) > 0) {
            rs <- DBI::dbSendStatement(con, paste("UPDATE work SET include_YN = 1",
              "WHERE runID_total = :x"))
            DBI::dbBind(rs, param = list(x = iwork))
          }

          DBI::dbClearResult(rs)
        }
      }
      RSQLite::dbDisconnect(con)

    } else {
      create <- TRUE
    }

  } else {
    unlink(dbWork)
    create <- TRUE
  }

  if (create) {
    temp <- create_dbWork(dbWork, jobs)
    success <- !inherits(temp, "try-error")
  }

  success
}


#' Extract identification numbers of runs of a SWSF simulation project which are
#'  uncompleted and not \code{inwork}
#'
#' @inheritParams create_dbWork
#' @return An integer vector of \code{runIDs}.
dbWork_todos <- compiler::cmpfun(function(path) {
  dbWork <- file.path(path, "dbWork.sqlite3")
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RO)
  runIDs_todo <- RSQLite::dbGetQuery(con, paste("SELECT runID_total FROM work ",
    "WHERE include_YN = 1 AND completed = 0 AND inwork = 0 ORDER BY runID_total"))
  RSQLite::dbDisconnect(con)

  as.integer(runIDs_todo[, 1])
})

#' Extract stored execution times of completed runs of a SWSF simulation project
#'
#' @inheritParams create_dbWork
#' @return A numeric vector of execution time in seconds.
dbWork_timing <- compiler::cmpfun(function(path) {
  dbWork <- file.path(path, "dbWork.sqlite3")
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RO)
  times <- RSQLite::dbGetQuery(con, paste("SELECT time_s FROM work WHERE include_YN = 1",
    "AND completed > 0"))
  RSQLite::dbDisconnect(con)

  as.numeric(times[, 1])})



#' Set runs information that need to be redone / simulated (again)
#'
#' @inheritParams create_dbWork
#' @return A logical vector indicating success.
dbWork_redo <- compiler::cmpfun(function(path, runIDs) {
  if (length(runIDs) > 0) {
    dbWork <- file.path(path, "dbWork.sqlite3")
    stopifnot(file.exists(dbWork))

    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)
    rs <- DBI::dbSendStatement(con, paste("UPDATE work SET completed = 0, failed = 0,",
      "inwork = 0, time_s = 0 WHERE include_YN = 1 AND runID_total = :x"))
    DBI::dbBind(rs, param = list(x = runIDs))
    DBI::dbClearResult(rs)
    RSQLite::dbDisconnect(con)

  } else {
    TRUE
  }
})




#' Re-create dbWork based on dbOutput
#'
#' @inheritParams create_dbWork
#' @param name.OutputDB
#' @return A logical vector indicating success.
recreate_dbWork <- function(path, name.OutputDB) {
  if (file.exists(name.OutputDB)) {
    dbWork <- file.path(path, "dbWork.sqlite3")

    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB, flags = RSQLite::SQLITE_RO)

    if (!all(sapply(c("runs", "sites"), function(x) DBI::dbExistsTable(con, x)))) {
      stop("'recreate_dbWork': OutputDB ", shQuote(name.OutputDB), " has ",
        "incomplete structure; dbWork cannot be recreated from it.")
    }

    table_runs <- DBI::dbReadTable(con, "runs")
    table_sites <- DBI::dbReadTable(con, "sites")
    RSQLite::dbDisconnect(con)

    # Infer design of simulation experiment
    infer_expN <- max(table_runs[, "treatment_id"])
    infer_scN <- max(table_runs[, "scenario_id"])
    infer_runIDs <- it_sim2(table_runs[, "P_id"], infer_scN)
    infer_runsN_total <- max(infer_runIDs)

    infer_runsN_master <- dim(table_sites)[1]
    infer_include_YN <- as.logical(table_sites[, "Include_YN"])
    infer_runsN_sites <- sum(infer_include_YN)

    do_new_dbWork <- TRUE

    if (file.exists(dbWork)) {
      # If dbWork present, check whether design is current
      con2 <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)
      if (DBI::dbExistsTable(con2, "work")) {
        has_work <- DBI::dbReadTable(con2, "work")

        if (all(c("runID_total", "runID_sites") %in% names(has_work))) {
          do_new_dbWork <- !(max(has_work[, "runID_total"]) == dim(has_work)[1] &&
            max(has_work[, "runID_total"]) == infer_runsN_total &&
            max(has_work[, "runID_sites"]) == infer_runsN_sites)
        }
      }

      RSQLite::dbDisconnect(con2)
    }

    if (do_new_dbWork) {
      # Create new dbWork
      setup_dbWork(path, runsN_master = infer_runsN_master,
        runsN_total = infer_runsN_total, expN = infer_expN, include_YN = infer_include_YN)

    } else {
      if (!identical(as.logical(is_work[, "include_YN"]), infer_include_YN)) {
        # Update include_YN
        con2 <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)
        rs <- DBI::dbSendStatement(con2, paste("UPDATE work SET include_YN = :x1",
          "WHERE runID_total = :x2"))
        DBI::dbBind(rs, param = list(x1 = infer_include_YN, x2 = infer_runIDs))
        DBI::dbClearResult(rs)
        RSQLite::dbDisconnect(con2)
      }
    }

    # Update completed
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB, flags = RSQLite::SQLITE_RO)
    tables <- dbOutput_ListOutputTables(con)
    # get Pids for which simulation output is in the outputDB
    has_pids <- lapply(tables, function(x) RSQLite::dbGetQuery(con,
      paste0("SELECT P_id FROM \"", x, "\""))[, 1])
    RSQLite::dbDisconnect(con)
    has_complete_pids <- intersect2(has_pids)
    has_complete_runIDs <- unique(it_sim2(has_complete_pids, infer_scN))

    if (length(has_complete_pids) > 0) {
      con2 <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)
      rs <- DBI::dbSendStatement(con2, paste("UPDATE work SET completed = 1, failed = 0,",
      "inwork = 0, time_s = 0 WHERE runID_total = :x"))
      DBI::dbBind(rs, param = list(x = has_complete_runIDs))
      DBI::dbClearResult(rs)
      RSQLite::dbDisconnect(con2)
    }

  } else {
    stop("OutputDB ", shQuote(name.OutputDB), " not found on disk.")
  }
}



#' Check run status
#'
#' @inheritParams create_dbWork
#' @return A data.frame with three columns 'completed', 'failed', and 'inwork'
dbWork_check <- compiler::cmpfun(function(path, runIDs) {
  if (length(runIDs) > 0) {
    dbWork <- file.path(path, "dbWork.sqlite3")
    stopifnot(file.exists(dbWork))

    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RO)
    rs <- DBI::dbSendStatement(con, paste("SELECT completed, failed, inwork FROM work",
      "WHERE runID_total = :x"))
    DBI::dbBind(rs, param = list(x = runIDs))
    res <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)
    RSQLite::dbDisconnect(con)

  } else {
    res <- data.frame(completed = numeric(0), failed = numeric(0), inwork = numeric(0))
  }

  res
})



#' Update run information of a SWSF simulation project
#'
#' @inheritParams create_dbWork
#' @param runID An integer value. The identification number of the current run,
#'  i.e., a value out of \code{runIDs_incl}, see \code{\link{indices}}.
#' @param status A character string. One of "completed", "failed", "inwork".
#' @param time_s A numeric value. The execution time in seconds; used if \code{status} is one of
#'  "completed" and "failed".
#' @param with_filelock A character string. The file path for locking access to
#'  \code{dbWork} with a file lock, i.e., to provide
#'  synchronization during parallel processing. If \code{NULL}, no file locking is used.
#' @param verbose A logical value. If \code{TRUE}, status messages about file lock and
#'  database access are printed
#'
#' @return A logical value whether the status was successfully updated.
dbWork_update_job <- compiler::cmpfun(function(path, runID, status = c("completed", "failed", "inwork"),
  time_s = "NULL", with_filelock = NULL, verbose = FALSE) {

  status <- match.arg(status)
  dbWork <- file.path(path, "dbWork.sqlite3")
  stopifnot(file.exists(dbWork))

  lock <- if (!is.null(with_filelock)) {
      lock_init(with_filelock, runID)
    } else {
      list(confirmed_access = TRUE)
    }

  success <- FALSE
  res <- 0L

  repeat {
    if (verbose)
      print(paste0("'dbWork_update_job': (", runID, "-", status, ") attempt to update"))

    lock <- lock_access(lock, verbose)
    con <- try(RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
      flags = RSQLite::SQLITE_RW), silent = TRUE)

    if (inherits(con, "SQLiteConnection")) {
      res <- DBI::dbWithTransaction(con, {
        if (verbose) {
          print(paste0("'dbWork_update_job': (", runID, "-", status,
            ") start transaction"))
        }

        temp <- if (status == "completed") {
            DBI::dbExecute(con, paste("UPDATE work SET completed = 1, failed = 0,",
              "inwork = 0, time_s =", time_s, "WHERE runID_total =", runID))

          } else if (status == "failed") {
            DBI::dbExecute(con, paste("UPDATE work SET completed = 0, failed = 1,",
              "inwork = 0, time_s =", time_s, "WHERE runID_total =", runID))

          } else if (status == "inwork") {
            prev_status <- RSQLite::dbGetQuery(con,
              paste("SELECT inwork FROM work WHERE runID_total =", runID))$inwork
            if (prev_status == 0) {
              DBI::dbExecute(con, paste("UPDATE work SET completed = 0, failed = 0,",
                "inwork = 1, time_s = 0 WHERE runID_total =", runID))
            } else {
              if (verbose)
                print(paste("'dbWork_update_job':", runID, "is already in work"))
              0L
            }
          } else {
            0L
          }

        lock <- unlock_access(lock)

        if (!lock$confirmed_access) {
          if (verbose) {
            print(paste0("'dbWork_update_job': (", runID, "-", status,
              ") access confirmation failed"))
          }
          temp <- 0L
          DBI::dbBreak()

        } else if (verbose) {
          print(paste0("'dbWork_update_job': (", runID, "-", status,
            ") transaction confirmed"))
        }

        as.integer(temp)
      })

      RSQLite::dbDisconnect(con)
      success <- lock$confirmed_access
    } else if (verbose) {
      print(paste0("'dbWork_update_job': (", runID, "-", status, ") 'dbWork' is locked"))
    }

    if (success) break
  }

  !is.na(res) && res == 1L
})


#------ End of dbWork functions
########################
