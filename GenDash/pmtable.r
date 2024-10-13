

pmtableUI <- function(id) {
	ns <- NS(id)
		DT::DTOutput(ns("pmtableB"))
	}

pmtableServer <- function(id, allPM, unitname, userinf) {
  moduleServer(id, function(input,output,session) {
      # CREATE Routine PMS TABLE
      errord <- reactiveVal(FALSE)
      selBPMs <- filter(allPM,(allPM[unitname] != "NA"))
      unitnamecol <- which(colnames(selBPMs)==unitname)
      colnames(selBPMs)[unitnamecol] <- 'PMNUM'
      selBPMs$PMNUM <- str_pad(selBPMs$PMNUM,8,side="left", pad="0")
      charselBPMs <- selBPMs
      charselBPMs[] <-lapply(selBPMs, as.character)
      pmnumlist<-paste0(charselBPMs[unitnamecol],sep='')
      pmnumlist<-gsub('c','',pmnumlist)
      pmnumlist<-gsub('\"','\'',pmnumlist)
      qry.string<-paste0(
       
      " 
        SELECT  \"PMNUM\", \"LASTCOMPDATE\", \"NEXTDATE\",\"FREQUENCY\" as \"MAXFREQ\", \"FREQUNIT\" as \"MAXFREQUNIT\",\"PM3\"
        FROM [MaximoDW].[dbo].[PM]
        WHERE PMNUM IN ",pmnumlist
    )
      maxdb.con <- tryCatch(
        expr = {
           if (userinf$os == 'Windows') {
           odbcDriverConnect('driver={SQL Server};server=SQLBI100;database=Generation;trusted_connection=true')
           } else {
           odbcDriverConnect('driver={ODBC Driver 17 for SQL Server};server=SQLBI100;database=Generation;trusted_connection=yes') 
           }
        },
      error = function(e){
        message(e)
        errord(TRUE)
        return(NA)
      },
      warning = function(w){
        message(w)
        errord(TRUE)
        return(NA)
      })
      if (is.na(maxdb.con)) {
        col.names = c("PMNUM","LASTCOMPDATE","NEXTDATE","MAXFREQ","MAXFREQUNIT","PM3")
        maxdata <- read.table(text = "",col.names = col.names)
      } else {
      maxdata<-sqlQuery(maxdb.con,qry.string)
      odbcClose(maxdb.con)
      #maxdata <- read.xlsx(dashDB,sheetName="MaxData")
    #  maxdata       ## DEBUGGO
      }
      maxdata$PMNUM <- str_pad(maxdata$PMNUM,8,side="left", pad="0")
      out_selBPMs<-merge(selBPMs,maxdata,by = "PMNUM", all=TRUE)
      #selBPMs$lastcomp <- for (selBPMs$unitname in )
      pmnumcol <- which(colnames(out_selBPMs)=="PMNUM")
      pmcompcol <- which(colnames(out_selBPMs)=="LASTCOMPDATE")
      pmnextdatecol <- which(colnames(out_selBPMs)=="NEXTDATE")
      pmfreqcol <- which(colnames(out_selBPMs)=="MAXFREQ")
      pmfrequncol <- which(colnames(out_selBPMs)=="MAXFREQUNIT")
      pmlastwo <- which(colnames(out_selBPMs)=="PM3")
      #out_selBPMs$frequencydisp <- paste(out_selBPMs$FREQUENCY,out_selBPMs$FREQCOL)
      #pmfreqdispcol <- which(colnames(out_selBPMs)=="frequencydisp")
      #out_selBPMs[pmcompcol]<-as.Date(out_selBPMs[pmcompcol], format='%m-%d-%Y')
      setcolorder(out_selBPMs,c(2,3,4,pmnumcol,pmcompcol,pmlastwo,pmnextdatecol))#,pmfreqcol,pmfrequncol))
      #cnt <- ncol(out_selBPMs)
      
      
      out_selBPMs<-out_selBPMs[order(out_selBPMs[1]),]
      out_selBPMs <- out_selBPMs %>% mutate(Frequency=paste0(out_selBPMs$MAXFREQ," ",out_selBPMs$MAXFREQUNIT))
      out_selBPMs <- subset(out_selBPMs, select=-c(9:ncol(out_selBPMs))) 
      colnames(out_selBPMs) <- c("ID","Task","Task Description", "PM Number","Last Date","Last WO Number", "Next Date","Frequency")
      
      
      output$pmtableB = renderDataTable( {
        out_table <- datatable(out_selBPMs,
          escape=FALSE,
          options = list(
            columnDefs=list(
              list(visible=FALSE,targets=c(0,3)),
              #list(width = '50px',targets=c(1))
              #list(width='20%',targets=c(1)),
              list(width=1,targets=c(1,4,6,8)),
              list(width=4,targets=c(2)),
              list(width=2,targets=c(5,7)),
              list(className='dt-center',targets=c(1:8))
              ),
            pageLength = 100,
            rowCallback = JS(
              "function(row, data) {",
              "var full_text = data[3]",
              "$('td', row).attr('title', full_text);",
              "}")
          #  )
            #dom = 't'
          )) %>%
        #color row red if PM is not set (PMNUM=0)
        formatStyle(
          'PM Number',
          target='row',
          backgroundColor = styleEqual('00000000','yellow')) %>%
        formatDate(c(5,7),method='toDateString')		
      })
      return(errord())
    }
  )
}  