#******** Module to export data

library(openxlsx)

append2xlServer <- function(id, data, dashDB, sheetname, unitname, startrow) {
  moduleServer(
    id,
    # TO Excel
    function(input,output,session) {
      wb=loadWorkbook(dashDB)
      sheets <- getSheetNames(dashDB)
      sheetnum <- which(sheets==sheetname)
      #removeWorksheet(wb,sheets[sheetnum])
      #saveWorkbook(wb,dashDB,overwrite = TRUE)
      #addWorksheet(wb,sheetname)
      writeData(
        wb,
        sheetnum,
        data,
        startCol = 1,
        startRow = startrow,
        colNames = FALSE,
        keepNA = FALSE)
      saveWorkbook(wb,dashDB,overwrite = TRUE)
    })
}