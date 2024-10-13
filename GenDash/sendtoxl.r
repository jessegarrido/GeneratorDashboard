#******** Module to export data

#library(openxlsx)

send2xlServer <- function(id, data, dashDB, sheetname, unitname) {
  moduleServer(
    id,
    # TO Excel
    function(input,output,session) {
      message(dashDB)
      wb=loadWorkbook(dashDB)
      #sheets <- getSheetNames(dashDB)
      #sheetnum <- which(sheets==sheetname)
      #removeWorksheet(wb,sheets[sheetnum])
      removeWorksheet(wb,sheetname)
      saveWorkbook(wb,dashDB,overwrite = TRUE)
      addWorksheet(wb,sheetname)
      writeData(wb,sheetname,data,keepNA=FALSE)
      saveWorkbook(wb,dashDB,overwrite = TRUE)
    })
}