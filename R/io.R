

# Header
# Filename:      io.R
# Description:   This library, is a part of package viser and is used for data import and export to/from other environments
# Author:        Nicolas Berta
# Email :        nicolas.berta@gmail.com
# Start Date:    28 October 2016
# Last Revision: 12 September 2018
# Version:       1.3.3

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     28 October 2016    Initial Issue: Started with function readODBC().
# 1.1.0     21 November 2016   Added filters to function readODBC().
# 1.2.0     01 December 2016   Function sqlFilter() generates script for filtering. Functions sqlFilterComponent.*() removed.
# 1.2.1     01 December 2016   Function sqlScript() generates complete sql query.
# 1.2.2     01 December 2016   Function readODBC() calls sqlScript() first to generate the query and then executes the query.
# 1.2.3     24 January 2017    Function readODBC() transfers additional arguments passed to function sqlQuery() in package RODBC.
# 1.3.0     17 May 2017        Function rScriptFilter() added, converts a filter into a R script of a condition
# 1.3.1     01 August 2017     Function rScriptFilter() exported.
# 1.3.2     15 May 2018        Function sqlScript() modified: changes column names with 'AS' if argument 'fields' is a named vector
# 1.3.3     12 September 2018  Function runSQL() added
#

# This function reads a table by ODBC and generates an object of type TIME.SERIES
# Reads a table using ODBC
# similar functions will be like this:
# readODBC.TIME.SERIES
# readODBC.nibeTree
#
# Examples:
#
# D = readODBC('ASET_VALU_CURR', fields = c('ASET_I', 'ASET_VALU_D', 'ASET_VALU_A'), dbName = 'PVDATA')

# script.sql.select = function(fields = '*'){}
# script.sql.from   = function(dbName,tableName){}
# script.sql.where  =

valid.filter.elements = c('domain', 'min', 'max', 'type', 'na.rm', 'equal', 'query')
valid.field.types     = c('nominal', 'numeric', 'date', 'time')

#' @export
sqlFilter = function(colName, filter, vf = T){
  if(vf){
    verify(filter, 'list', names_domain = valid.filter.elements, varname = 'filter')
    filter$type  <- verify(filter$type, 'character', domain = valid.field.types, varname = 'filter$type', default = 'nominal')
    filter$na.rm <- verify(filter$na.rm, 'logical', domain = c(T,F), varname = 'filter$na.rm', default = F)
    filter$equal <- verify(filter$equal, 'logical', domain = c(T,F), varname = 'filter$equal', default = F)
    filter$query <- verify(filter$query, 'character', varname = 'filter$query')
  }
  if(!is.null(filter$query)){
    return(paste0(colName, " IN (", filter$query, ")"))
  }
  if (filter$equal){
    minopr = " >= "
    maxopr = " <= "
  } else {
    minopr = " > "
    maxopr = " < "
  }
  scr   = ""
  joint = ""
  switch(filter$type,
         'nominal' = {
           if(!is.null(filter$domain)){
             if (!inherits(filter$domain, 'character')){filter$domain %<>% as.character}
             scr = paste0(colName, " IN ('",  paste(filter$domain, collapse = "','"), "')")
             joint = " AND "
           }
           if(filter$na.rm){
             scr = paste0(scr, joint, colName, " IS NOT NULL")
           }
         },
         'numeric' = {
           if (!is.null(filter$min)){
             scr = paste0(colName, minopr, filter$min)
             joint = " AND "
           }
           if (!is.null(filter$max)){
             scr = paste0(scr, joint, colName, maxopr, filter$max)
             joint = " AND "
           }
           if(filter$na.rm){scr = paste0(scr, joint, colName, " IS NOT NULL")}
         },
         'date'    = {
           if (!is.null(filter$min)){
             if(vf){verify(filter$min, valid.time.classes, varname = 'filter$min')}
             if (inherits(filter$min, 'character')){minDate = char2Date(filter$min)} else {minDate = as.Date(filter$min)}
             minDate = as.character(minDate)
             scr = paste0(colName, minopr, "'", minDate, "'")
             joint = " AND "
           }
           
           if (!is.null(filter$max)){
             verify(filter$max, valid.time.classes, varname = 'filter$max')
             if (inherits(filter$max, 'character')){maxDate = char2Date(filter$max)} else {maxDate = as.POSIXlt(filter$max)}
             maxDate = as.character(maxDate)
             scr = paste0(scr, joint, colName, maxopr, "'", maxDate, "'")
             joint = " AND "
           }
           if(filter$na.rm){scr = paste0(scr, joint, colName, " IS NOT NULL")}
         },
         'time'    = {
           if (!is.null(filter$min)){
             verify(filter$min, valid.time.classes, varname = 'filter$min')
             # if (inherits(filter$min, 'character')){minTime = char2Time(filter$min)} else {minTime = as.POSIXlt(filter$min)}
             minTime = as.time(filter$min, target_class = 'POSIXlt')
             minTime = as.character(minTime, format = '%Y-%m-%d %H:%M:%S')
             scr = paste0(colName, minopr, "'", minTime, "'")
             joint = " AND "
           }
           
           if (!is.null(filter$max)){
             verify(filter$max, valid.time.classes, varname = 'filter$max')
             if (inherits(filter$max, 'character')){maxTime = char2Time(filter$max)} else {maxTime = as.POSIXlt(filter$max)}
             maxTime = as.character(maxTime, format = '%Y-%m-%d %H:%M:%S')
             scr = paste0(scr, joint, colName, maxopr,"'", maxTime, "'")
             joint = " AND "
           }
           if(filter$na.rm){scr = paste0(scr, joint, colName, " IS NOT NULL")}
         })
  
  return(scr)
}

#' @export
rScriptFilter = function(colName, filter, vf = T){
  if(vf){
    verify(filter, 'list', names_domain = valid.filter.elements, varname = 'filter')
    filter$type  <- verify(filter$type, 'character', domain = valid.field.types, varname = 'filter$type', default = 'nominal')
    filter$na.rm <- verify(filter$na.rm, 'logical', domain = c(T,F), varname = 'filter$na.rm', default = F)
    filter$equal <- verify(filter$equal, 'logical', domain = c(T,F), varname = 'filter$equal', default = F)
    filter$query <- verify(filter$query, 'character', varname = 'filter$query')
  }
  
  if (filter$equal){
    minopr = " >= "
    maxopr = " <= "
  } else {
    minopr = " > "
    maxopr = " < "
  }
  scr   = ""
  joint = ""
  switch(filter$type,
         'nominal' = {
           if(!is.null(filter$domain)){
             if (!inherits(filter$domain, 'character')){filter$domain %<>% as.character}
             scr = paste0(colName, " %in% c('",  paste(filter$domain, collapse = "','"), "')")
             joint = " & "
           }
           if(filter$na.rm){scr = paste0(scr, joint, "!is.na(", colName, ")")}
         },
         'numeric' = {
           if (!is.null(filter$min)){
             scr = paste0(colName, minopr, filter$min)
             joint = " & "
           }
           if (!is.null(filter$max)){
             scr = paste0(scr, joint, colName, maxopr, filter$max)
             joint = " & "
           }
           if(filter$na.rm){scr = paste0(scr, joint, "!is.na(", colName, ")")}
         },
         'date'    = {
           if (!is.null(filter$min)){
             if(vf){verify(filter$min, valid.time.classes, varname = 'filter$min')}
             if (inherits(filter$min, 'character')){minDate = char2Date(filter$min)} else {minDate = as.Date(filter$min)}
             minDate = as.character(minDate)
             scr = paste0(colName, minopr, "'", minDate, "'")
             joint = " & "
           }
           
           if (!is.null(filter$max)){
             verify(filter$max, valid.time.classes, varname = 'filter$max')
             if (inherits(filter$max, 'character')){maxDate = char2Date(filter$max)} else {maxDate = as.POSIXlt(filter$max)}
             maxDate = as.character(maxDate)
             scr = paste0(scr, joint, colName, maxopr, "('", maxDate, "' %>% as.Date)")
             joint = " & "
           }
           if(filter$na.rm){scr = paste0(scr, joint, "!is.na(", colName, ")")}
         },
         'time'    = {
           if (!is.null(filter$min)){
             verify(filter$min, valid.time.classes, varname = 'filter$min')
             # if (inherits(filter$min, 'character')){minTime = char2Time(filter$min)} else {minTime = as.POSIXlt(filter$min)}
             minTime = as.time(filter$min, target_class = 'POSIXlt')
             minTime = as.character(minTime, format = '%Y-%m-%d %H:%M:%S')
             scr = paste0(colName, minopr, "('", minTime, "' %>% as.POSIXlt)")
             joint = " & "
           }
           
           if (!is.null(filter$max)){
             verify(filter$max, valid.time.classes, varname = 'filter$max')
             if (inherits(filter$max, 'character')){maxTime = char2Time(filter$max)} else {maxTime = as.POSIXlt(filter$max)}
             maxTime = as.character(maxTime, format = '%Y-%m-%d %H:%M:%S')
             scr = paste0(scr, joint, colName, maxopr,"('", maxTime, "' %>% as.POSIXlt)")
             joint = " AND "
           }
           if(filter$na.rm){scr = paste0(scr, joint, "!is.na(", colName, ")")}
         })
  
  return(scr)
}


#' @export
sqlScript = function(tableName, fields = NULL, dbName = 'UDRBSCMS', filter = NULL, vf = T){
  if(vf){
    tableName  = verify(tableName, 'character', lengths = 1, varname = 'tableName')
    fields     = verify(fields, 'character', varname = 'fields', default = '*')
    filter     = verify(filter, 'list', varname = 'filter')
  }
  
  fldnms = names(fields)
  if(is.null(fldnms)){fields.str = paste(fields, collapse = " , ")} else {
    assert(length(fldnms) == length(fields))
    fldnms[fldnms == ''] <- fields[fldnms == '']
    fields.str = paste(paste(fields, 'AS', fldnms), collapse = " , ")
  }
  
  query      = paste0("SELECT ", fields.str, " from ", dbName, ".", tableName)
  
  if (!is.null(filter)){
    first = T
    query = query %++% " WHERE "
    for (fn in names(filter)){
      if (!first){query = query %++% " AND "}
      qfn   = sqlFilter(fn, filter[[fn]], vf = vf)
      query = query %++% qfn
      if (qfn != ""){first = F}
    }
  }
  return(query)
}


#' @export
readODBC <- function(tableName, fields = NULL, dbName = 'UDRBSCMS', filter = NULL, dsn = 'Teradata_Prod', vf = T){
  # Verifications:
  if(vf){assert(require(RODBC), "Package 'RODBC' is not installed!", err_src = match.call()[[1]])}
  
  channel    = odbcConnect(dsn = dsn, ...)
  D          = sqlQuery(channel = channel, query = sqlScript(tableName = tableName, fields = fields, dbName = dbName, filter = filter, vf = vf), ...)
  close(channel)
  return(D)
}

#' @export
runSQL <- function(query, ...){
  channel    = odbcConnect(...)
  D          = sqlQuery(channel = channel, query = query)
  close(channel)
  return(D)
}


