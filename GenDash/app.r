#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
#library(rClr)
library(DT)
library(data.table)
library(shinyjs)
library(plyr)
library(dplyr)
#library(digest)
#library(xlsx)
library(openxlsx)
library(rJava)
library(RODBC)
library(stringr)
library(formattable)
library(tidyverse)
library(lubridate)
library(shinyscreenshot)
library(httr)
#library(wrapr)
#library(shinybusy)
#library(readxl)
#library(janitor)
#library(readxl)
#library(writexl)

source("investigations.r")
source("sendtoxl.r")
source("appendtoxl.r")
source("pmtable.r")

machineuser <- Sys.getenv('AppId')
machineuserpw <- Sys.getenv('AppPwd')

loading_modal <- function(unit) {
  shiny::modalDialog(
    title = unit,
    footer = NULL,
    size = "s",
    easyClose = FALSE,
    fade = TRUE,
    loadingState()
    )
}


# sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
#   f <- function(...) order(...,decreasing=decreasing)
#   i <- do.call(f,x[by])
#   x[i,,drop=FALSE]
# }

extPath="https://teams.sp.*******.int/sites/GenCompliance/GenDash"
writeExtPath="\\\\teams.sp.*******.int@SSL\\sites\\GenCompliance\\GenDash"
#extPath="https://bi.sp.*******.int/sites/GenEngineering/Electrical/Documents/GenDash/www"

#Turn PIAF configstring into a PI Vision link
createLink <- function(val,name) {
    sprintf('<a href="https://*******/PIVision/#/Displays/Adhoc?DataItems=%s&Symbol=trend;MultipleScales=false;StartTime=*-1mo&EndTime=*" target="_blank" >%s</a>',val,name)
}

fieldsAll <- c("h2pur", "h2sodiff","scwfl", "statemp","scwcond","gastemp","brvib","thrdharm","scwh2diff","fldtemp","fldgnd","scwoxy", "hydry","liqdet","brrig","shftgrnd","fldgrnd","inspnotes","operator")

# epochTime <- function() {
#   as.Date(as.POSIXct(Sys.time(),orig="1970-01-01"))
# }


humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

#Organize data retrieved from PI
alarmtable <- function(my_table,unit) {

   # my_table <- filter(my_table,my_table[9]==sysfilter)
  if(nrow(my_table)!=0){
    #str_remove(my_table$cs,"|.*$)")
    #apply(my_table,2,str_remove(my_table[2],"[|]"))
    my_table <- filter(my_table,!is.na(my_table$Value))     #Remove unavailable monitors
    my_table <- filter(my_table,my_table$sta!=0)            #only show items in alarm
    my_table$cs <- str_remove(my_table$cs,"\\|.*$")         #regex to remove UOM from configstring
    #my_table$cs <- my_table$cs[is.na(my_table$cs) <- "NOPE"]
    #message(my_table$cs)
    #stopApp()
    #my_table$cs[my_table$cs == ""] <- paste0("\\\\*******\\Generation\\Generator FMEA\\",unit,"\\",my_table$System[my_table$cs == ""],"\\",my_table$Name[my_table$cs == ""],"|Average")     # Fix up links for PIAF objects instead of Pi Tags
    
    my_table$cs <- paste0("\\\\*******\\Generation\\Generator FMEA\\",unit,"\\",my_table$System,"\\",my_table$Name,"|Array|Hi Alarm;","\\\\*******\\Generation\\Generator FMEA\\",unit,"\\",my_table$System,"\\",my_table$Name,"|Array|Lo Alarm;","\\\\*******\\Generation\\Generator FMEA\\",unit,"\\",my_table$System,"\\",my_table$Name,"|Average;",my_table$cs) 
    
    my_table$Item <- createLink(my_table$cs,my_table$Names) #create PIVision links
    setcolorder(my_table,c(11,4,5,3,9,10,2,1,6,7,8)) 

      

    my_formattedtable<-as.datatable(formattable(my_table, list(
      'Status' = formatter("span",
                           style = ~ style(color=ifelse(sta=="3","red",ifelse(sta=="2","orange",ifelse(sta=="1","yellow",ifelse(sta=="-1","grey","black")))),
                                           #background=ifelse(sta=="0","white","black"),
                                           font.weight="bold")                                                                   
      ))),
      style='bootstrap',
      filter = "top",
      selection = 'single',
      options = list( 
        columnDefs=list(
          list(visible=FALSE,targets=c(0,6:11)),
          list(width='20%',targets=c(1)),
          #list(width='20%',targets=c(2)),
          list(width='5%',targets=c(2:6)),
          # list(width='5%',targets=c(4)),
          # list(width='5%',targets=c(5)),
          list(className='dt-center',targets=c(2:11)),
          list(className='dt-left',targets=c(1))
        ),
        pageLength = 100,
        rowCallback = JS(
          "function(row,data) {",
          "var full_text = 'No Alarms'",
          "if (data[7] == 3) {var full_text = 'All Sensors In Alarm'}",
          "if (data[7] == 2) {var full_text = 'Sensors Total In Alarm'}",
          "if (data[7] == 1) {var full_text = 'One Sensor In Alarm'}",
          "$('td', row).attr('title',full_text);",
          "}"),
        dom = 't')             
    )
  } else {
    NULL
  }
}

cmtable <- function(my_table,sysfilter,unit) {
 # my_table <- filter(my_table,my_table[4]==0)
   my_table <- filter(my_table,my_table[9]==sysfilter)
  
  if(nrow(my_table)!=0){
    my_table <- filter(my_table,!is.na(my_table$Value))     #Remove unavailable monitor
    my_table$cs <- str_remove(my_table$cs,"\\|.*$")
    #my_table$cs[my_table$cs == ""] <- paste0("\\\\*******\\Generation\\Generator FMEA\\",unit,"\\",my_table$System[my_table$cs == ""],"\\",my_table$Name[my_table$cs == ""],"|Average")     # Fix up links for PIAF objects instead of Pi Tags
    #my_table$cs <- paste0("\\\\*******\\Generation\\Generator FMEA\\",unit,"\\",my_table$System,"\\",my_table$Name,"|Array|Hi Alarm;","\\\\*******\\Generation\\Generator FMEA\\",unit,"\\",my_table$System,"\\",my_table$Name,"|Array|Lo Alarm;",my_table$cs) 
    my_table$cs <- paste0("\\\\*******\\Generation\\Generator FMEA\\",unit,"\\",my_table$System,"\\",my_table$Name,"|Array|Hi Alarm;","\\\\*******\\Generation\\Generator FMEA\\",unit,"\\",my_table$System,"\\",my_table$Name,"|Array|Lo Alarm;","\\\\*******\\Generation\\Generator FMEA\\",unit,"\\",my_table$System,"\\",my_table$Name,"|Average;",my_table$cs) 
    
    
    my_table$Item <- createLink(my_table$cs,my_table$Names)
    setcolorder(my_table,c(11,4,5,3,10,2,1,6,7,8,9))
    my_formattedtable<-as.datatable(formattable(my_table, list(
      'Status' = formatter("span",
                           style = ~ style(color=ifelse(sta=="3","red",ifelse(sta=="2","orange",ifelse(sta=="1","yellow",ifelse(sta=="-1","grey","black")))),
                                          # background=ifelse(sta=="0","white","black"),
                                           font.weight="bold")                                                                   
      ))),
      style='bootstrap',
      selection = 'single',
      options = list( 
        columnDefs=list(
          list(visible=FALSE,targets=c(0,5:11)),
          list(width='20%',targets=c(1)),
          list(width='5%',targets=c(2:5)),
          # list(width='5%',targets=c(3)),
          # list(width='5%',targets=c(4)),
          # list(width='5%',targets=c(5)),
          list(className='dt-center',targets=c(2:11)),
          list(className='dt-left',targets=c(1))
        ),
        pageLength = 100,
        rowCallback = JS(
          "function(row,data) {",
          "var full_text = 'No Alarms'",
          "if (data[6] == 3) {var full_text = 'All Sensors In Alarm'}",
          "if (data[6] == 2) {var full_text = 'Sensors Total In Alarm'}",
          "if (data[6] == 1) {var full_text = 'One Sensor In Alarm'}",
          "$('td', row).attr('title',full_text);",
          "}"),
        dom = 't')         
    )
  } else {
    NULL
  }
}

#)

#Hide/Unhide things
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

# getoutage <- function(x) {
#   
#   myurl <- read_html("https://teams.sp.*******.int/sites/GenPlan/Documents/Proposed_Maintenance_2020911.htm") # read our webpage as html
#   myurl <- html_table(myurl)  # convert to an html table for ease of use
# } 

#Round value x to k decimal places
round_dec <- function(x, k) {
  y = as.numeric(as.character(x))
  trimws(format(round(y, k), nsmall=k))
}


# Function to get fleet PM data at page load
loadPM <- function(dashDB, pmTable, appName) {
      allPM <-read.xlsx(xlsxFile=dashDB,sheet=pmTable)
      allPM$ID <- as.character(allPM$ID)
      names(allPM) <- gsub(".", " ", names(allPM),fixed=TRUE)    # Replace periods with space in column names
      cnt <- nrow(allPM)
      allPM <- allPM[sort(allPM$Order),]
      for (val in  1:cnt) { allPM[val,1]<-paste(appName," -",str_pad(val,2,pad="0")) }
      return(allPM)
}


server <- function(input, output, session) {
  
  errData <- reactiveValues(
    Max = FALSE,
    PI = FALSE
  )

    # session$allowReconnect(TRUE)
	sysinfo <- Sys.info()
  #  #if ( sysinfo[1] != 'Windows' ) { click("auth") }       #Auto-login via machine account on Linux
  #click("auth")    # ok to always click auth?
	kerb <- if ( sysinfo[1] != 'Windows' ) {
					system2('klist',args = '-s')
				} else { 0 }
  userinf <- reactiveValues(
	os = sysinfo[1],
	user =sysinfo[6],
	kerb = kerb
  )
  
  if ( sysinfo[1] != 'Windows' ) {
  kinargs <- paste0("'",machineuserpw,"' | kinit ", machineuser)     #Log in machine user
  reply <- system2(
    'echo',
    args = kinargs,
    wait=TRUE,
    stdout = TRUE,
    stderr = 'autherr.txt'
    #write.csv(userinf$user,file=paste0(extPath, "/lastuser.csv"),append=false,row.names=false,sep="\t")
  ) }

 # output$logmessage <- renderText({logmessage})

    output$logininputs <- renderUI({
    #req(input$sysinf$system != "Windows",cancelOutput=TRUE)
    if (userinf$kerb == 0) {
      logmessage <- paste0("Authenticated")  # as ", userinf$user)
      fluidPage(
      span(renderText({logmessage}), style="font-size:12px;text-align:right;") 
      )
    } else { 
      # Uncomment for Login Window
      # fluidPage(
      #   #width=12,
      #   #olidHeader=false,
      #   textInput("username","E-Number:"),
      #   passwordInput("password","Password:"),
      #   div(style = "display:inline-block; float:right;",actionButton("auth","Authenticate")),
      #   br(),
      #   br()
      logmessage <- paste0("Not Authenticated")  # as ", userinf$user)
      fluidPage(
        span(renderText({logmessage}), style="font-size:12px;text-align:right;") 
      )
    }
      #span(paste(textOutput("logmessage")))),# style="font-size:12px;text-align:center;vertical-align:top;"))),#font-weight:bold"),
    
    #textOutput("logmessage")
      #
   })
    
  
  dashDB <- paste0(extPath,"/DashData.xlsx") ### path to database
  dashDBwrite <- paste0(writeExtPath,"/DashData.xlsx") ### path to database
  fmeaXL <- paste0(extPath,"/MC4.xlsx")
  rowsgrouppath <- getwd() #path to rowsGroup helper js
  enableBookmarking(store = "url")
  
  # Uncomment this block if copying DashData from Sharepoint ever works
  # tmpdashDB <- GET(dashDB, timeout(60000),authenticate("machineuser,machineuserpw,type="any"), write_disk("tmpdashDB.xlsx",overwrite=TRUE))
  # tmpfmeaXL <- GET(fmeaXL, authenticate(machineuser,machineuserpw,"ntlm"),write_disk("tmpfmeaXL.xlsx",overwrite=TRUE))
  # RowsScript <- GET(paste0(extPath,"dataTables.rowsGroup.js"), authenticate(machineuser,machineuserpw,"ntlm"),write_disk("dataTables.rowsGroup.js",overwrite=TRUE))
  # dashdb <- "tmpdashDB.xlsx"
  # fmeaXl <- "tmpfmeaXL.xlsx"
  # rowsgrouppath <- getwd()
  
  #dashDB <- "dashdat.xlsx"
  
  fmeainspectdf <- read.xlsx(dashDB,sheet="Inspection")
  fmeainspectdf['Condition'] <- NA
  FMEARV <- reactiveValues(data=fmeainspectdf)
  
  piReady <- read.xlsx(dashDB,sheet="PiReady")

  observeEvent(input$grabngo, { 
    screenshot() 
   #browseURL("mailto:*******************", encodeIfNeeded = TRUE)
  })
  
  observeEvent(input$auth, {
    userinf$user <- isolate(input$username)
    # system2(
    #   'kdestroy',
    #   wait=TRUE,
    # )
    #kinargs <- paste0("'",input$password,"' | kinit ",input$username)   
    kinargs <- paste0("'",machineuserpw,"' | kinit ", machineuser)     
    loginresult <- system2(
      'echo',
      args = kinargs,
      wait=TRUE,
      stdout = TRUE,
      stderr = 'autherr.txt'
      #write.csv(userinf$user,file=paste0(extPath, "/lastuser.csv"),append=false,row.names=false,sep="\t")
        )
    #getcurruser()
    session$reload()
  })
  
  # observeEvent(input$unauth, {
  #   system2(
  #     'kdestroy',
  #     wait=TRUE,
  #     stdout = 'authout.txt',
  #     stderr = 'autherr.txt'
  #   )
  #   session$reload()
  #   #shinyjs::hide(id = "logout")
  # })
  

  
  unitSel = reactive({
    stasel = input$station
    if (stasel == "Mill Creek" || stasel == "Ghent") {
      c("","Unit 1","Unit 2","Unit 3", "Unit 4")
    } else if (stasel == "Cane Run") {
      c("","7 CT1","7 CT2","7 ST")
    } else if (stasel == "EW Brown") {
      c("","Unit 3")
    } else if (stasel == "Trimble County") {
      c("","Unit 1", "Unit 2")
    } else {
      c("")
    }
    
    
  })
  observe({
    updateSelectInput(session, "unit",
                      choices=unitSel()
    )})
  
  observeEvent(input$selPage, {
    shinyjs::hide(id = "intro")
  })
       
  observeEvent(input$sidebarCollapsed, {
    if (input$sidebarCollapsed == TRUE ) {
      shinyjs::hide(id="unitselect")
    } else {
      shinyjs::show(id="unitselect")
    }
  })
  
  
  
   #Create TIL's Table
  #til<-read.xlsx(file.path("//fs01/folderredirect$/e028072/Desktop/Dash/Spreadsheets/TIL.xlsx"),1)
  # output$til = DT::renderDataTable({til})
  
  #PMdb.con<-dashDB
  #PMs<-odbcConnectAccess2007(PMdb.con)
  #appBPMs<-sqlFetch(PMs,"Appendix B")
  #appCPMs<-sqlFetch(PMs,"Appendix C")
  #appIssues<-sqlFetch(PMs,"Investigations")

  #MaxPMs<-sqlFetch(PMs,"MaximoPMs")
  #odbcCloseAll()
  
  appBPMs <- loadPM(dashDB,"Appendix B","B")
  appCPMs <- loadPM(dashDB,"Appendix C","C")

  observeEvent(input$sidebarCollapsed, {
    if (input$sidebarCollapsed == TRUE ) {
      shinyjs::hide(id="unitselect")
    } else {
      shinyjs::show(id="unitselect")
    }
  })
  
 # shinyjs::hide(id = "loadSpinner")
  removeModal()
  
  # Everything that happens when user selects a generator
  
  observeEvent(input$unit,{

    if (input$unit == "") {
      if (input$station != "") {
        return()} else { 
          unitname <- toString(read.delim("www/last.txt", header=FALSE))
          message(unitname)
        }
    } else { 
      unitname <-paste(input$station,input$unit, sep=" ") 
      write.table(unitname,"www/last.txt", append = FALSE, sep = " ", dec = ".",     #save last unit looked at
                  row.names = FALSE, col.names = FALSE)
      }
      #shinyjs::show(id = "loadSpinner")
     
    showModal(loading_modal(unitname))  
    
    sendtopi <- unitname    #Swap the comment on these lines
      #sendtopi <- "Mill Creek Unit 4"    #to lock/unlock unit selection on PI server
      

      
      maxerr <- pmtableServer("appB",appBPMs,unitname,userinf)
      maxerr <- pmtableServer("appC",appCPMs,unitname,userinf)
 
      piafdb.con <- tryCatch(
        expr={
          if (any(unitname == piReady) ) {
           if (userinf$os != 'Windows') {
             odbcDriverConnect('driver={ODBC Driver 17 for SQL Server};server=SQLPIAFPROD;trusted_connection=yes')
           } else {
             odbcDriverConnect('driver={SQL Server};server=SQLPIAFPROD;trusted_connection=yes')
           }    #piafdb.con <- odbcConnect('SQLPIAFPROD')
          } else {odbcDriverConnect('driver={blahdeeblahdeeblah)}')}
        },
      error = function(e){
        message(e)
        return(NA)
      },
      warning = function(w){
        message(w)
        return(NA)
      })
      
      if (is.na(piafdb.con) ) {
        col.names = c("Name","Status","DirStat","Average","UOM","Lo","Hi","ConfigString","System","SensID")
        sql_out2 <- read.table(text = "", col.names = col.names)
      } else {
          		  qry.string1 <- paste0("
          SELECT ea.ElementID,e.Name,s.Value,ea.Name Attribute
          FROM LINKEDAF.Generation.Asset.ElementHierarchy eh
          INNER JOIN LINKEDAF.Generation.Asset.ElementAttribute ea ON ea.ElementID = eh.ElementID
          INNER JOIN LINKEDAF.Generation.Asset.Element e ON e.ID = eh.ElementID
          INNER JOIN LINKEDAF.Generation.Data.Snapshot s ON s.ElementAttributeID = ea.ID
          WHERE eh.Path LIKE '\\Generator FMEA\\",sendtopi,"\\%'
          AND ea.Name IN ('Average','Hi Alarm','Lo Alarm','DirStat', 'Sensor ID', 'Status', 'System')
          OPTION (FORCE ORDER)
          ")
                    qry.string2 <- paste0("
          SELECT ea.ElementID,ea.ConfigString
          FROM LINKEDAF.Generation.Asset.ElementHierarchy eh
          INNER JOIN LINKEDAF.Generation.Asset.ElementAttribute ea ON ea.ElementID = eh.ElementID
          INNER JOIN LINKEDAF.Generation.Asset.Element e ON e.ID = eh.ElementID
          WHERE eh.Path LIKE '\\Generator FMEA\\",sendtopi,"\\%'
          AND ea.Name IN ('Array')
          OPTION (FORCE ORDER)
          ")
                    qry.string3 <- paste0("
          SELECT DISTINCT ea.ElementID,e.Name
          FROM LINKEDAF.Generation.Asset.ElementHierarchy eh
          INNER JOIN LINKEDAF.Generation.Asset.ElementAttribute ea ON ea.ElementID = eh.ElementID
          INNER JOIN LINKEDAF.Generation.Asset.Element e ON e.ID = eh.ElementID
          --INNER JOIN LINKEDAF.Generation.Data.Snapshot s ON s.ElementAttributeID = ea.ID
          WHERE eh.Path LIKE '\\Generator FMEA\\",sendtopi,"\\%'
          --AND ea.Name IN ('Average','Hi Alarm','Lo Alarm','DirStat', 'Sensor ID', 'Status', 'System')
          OPTION (FORCE ORDER)
          ")
                   qry.string4 <- paste0("
          SELECT ea.ElementID,e.Name,ea.Name Attribute,s.Value
          FROM LINKEDAF.Generation.Asset.ElementHierarchy eh
          INNER JOIN LINKEDAF.Generation.Asset.ElementAttribute ea ON ea.ElementID = eh.ElementID
          INNER JOIN LINKEDAF.Generation.Asset.Element e ON e.ID = eh.ElementID
          INNER JOIN LINKEDAF.Generation.Data.Snapshot s ON s.ElementAttributeID = ea.ID
          WHERE eh.Path LIKE '\\Generator FMEA\\' 
          AND ea.Name IN ('Online')
          OPTION (FORCE ORDER)
          ")
          sqldf1 <- sqlQuery(piafdb.con,qry.string1)
          sqldf2 <- sqlQuery(piafdb.con,qry.string2)
          sqldf3 <- sqlQuery(piafdb.con,qry.string3)
          sqldf4 <- sqlQuery(piafdb.con,qry.string4)
          odbcCloseAll()
          #view(sqldf1)
          #view(sqldf2)
          #view(sqldf3)
          #stopApp()
          offon <- sqldf4[sqldf4$Name==unitname,"Value"]
          ifelse(offon==-1,offline<-"",offline<-"OFFLINE")
          if ( is.data.frame(sqldf1) ) {
             sql_piv <- pivot_wider(
               sqldf1,
               id_cols = ElementID,
               names_from = Attribute,
               values_from = Value
             ) 
             #}
             sqldf2 <- sqldf2 %>%
               separate(ConfigString,c("CS","UOM"),";")
             sqldf2$UOM <- lapply(sqldf2$UOM, function(x) ifelse(grepl("UOM",x,fixed=TRUE),str_replace(x,"UOM=",""),x<-''))
             sqldf2$CS <- str_replace(sqldf2$CS,"\\?e934aa65-ea73-4935-96da-2e4f36dadab9","")
             #sqldf2test <- sqldf2
             sqldf2$CS<-str_replace_all(sqldf2$CS,paste0("\\?.*?\\|"),";\\\\\\\\**************\\\\")
             sqldf2$CS<-str_replace_all(sqldf2$CS,paste0("\\?.*"),";")
             sqldf2$CS <- lapply(sqldf2$CS, function(x) ifelse(grepl("=",x,fixed=TRUE),x<-'',x))
             sql_out <- merge(sqldf3,sql_piv,by = 'ElementID')
             sql_out <- merge(sql_out,sqldf2,by = 'ElementID')
             sql_out <- subset(sql_out, select=-c(1))
             sql_out2 <- sql_out[,c("Name","Status","DirStat","Average","UOM","Lo Alarm","Hi Alarm","CS","System","Sensor ID")]
             #sql_out2 <- sql_out[,c(1,3,2,7,10,6,8,9,4,5)]
             colnames(sql_out2) <- c("Name","Status","DirStat","Average","UOM","Lo","Hi","ConfigString","System","SensID")
            }
      } 
      new_data_frame <- as.data.frame(sql_out2)
      
      RV <- reactiveValues(data=new_data_frame)
      my_table <- RV$data
      colnames(my_table)[8] <- 'cs'
      colnames(my_table)[1] <- 'Names'  
      colnames(my_table)[4] <- 'Value'
      colnames(my_table)[5] <- 'Units'
      colnames(my_table)[2] <- 'sta'
      colnames(my_table)[3] <- 'Status'
      colnames(my_table)[9] <- 'System'
      colnames(my_table)[10] <- 'sid'
      
      #Get/Show Status Values
      RealPow <- match("Real Power",my_table$Names)
      RealPow <- paste0(round_dec(my_table[RealPow,4],2)," MW")
      ReacPow <- match("Reactive Power",my_table$Names)
      ReacPow <- paste0(round_dec(my_table[ReacPow,4],2)," MVAR")
      Freq <- match("Frequency",my_table$Names)
      Freq <- paste0(round_dec(my_table[Freq,4],2)," Hz")
      Pfac <- match("Power Factor",my_table$Names)
      Pfac <- paste0(round_dec(my_table[Pfac,4],2)," ")
      Tvol <- match("Terminal Voltage (Pos Seq L-N)",my_table$Names)
      Tvol <- paste0(round_dec(as.numeric(my_table[Tvol,4])*1.732,2)," kV")
      
       output$unitLabel <- renderText({(paste(unitname,"              <font color=\"#FF0000\"><b>",offline,"</b></font>"))})
      # #output$unitmw <- renderText({RealPow})
      # output$unitmvar <- renderText({ReacPow})
      # output$unitfreq <- renderText({Freq})
      
        output$realpow <- renderValueBox({
        valueBox(
          value = RealPow,
          subtitle = "Real Power",
          icon=icon("industry")
        )
      })
      output$reacpow <- renderValueBox({
        valueBox(
          value = ReacPow,
          color = "red",
          subtitle = "Reactive Power",
          icon=icon("industry")
        )
      })
      output$pfac <- renderValueBox({
        valueBox(
          value = Pfac,
          color = "green",
          subtitle = "Power Factor",
          icon=icon("industry")
        )
      })
      output$freq <- renderValueBox({
        valueBox(
          value = Freq,
          color = "blue",
          subtitle = "Frequency",
          icon=icon("industry")
        )
      })
      output$termvol <- renderValueBox({
        valueBox(
          value = Tvol,
          color = "blue",
          subtitle = "Terminal Voltage",
          icon=icon("industry")
        )
      })
      
      #populate daily inspection data
      h2pur_disp<-reactiveVal(new_data_frame[which(new_data_frame[,10]=='HYD-007'),4])
      h2sodiff_disp<-reactiveVal(new_data_frame[which(new_data_frame[,10]=='SEA-011'),4])
      scwfl_disp<-reactiveVal(new_data_frame[which(new_data_frame[,10]=='SCW-005'),4])
      #statemp_disp<-reactiveVal(mean(as.double(new_data_frame[which(new_data_frame[,10]=='STA-005'),4]),as.double(new_data_frame[which(new_data_frame[,10]=='STA-006'),4])))
      statemp_disp<-reactiveVal((as.double(new_data_frame[which(new_data_frame[,10]=='STA-005'),4]) + as.double(new_data_frame[which(new_data_frame[,10]=='STA-006'),4]))/2)
      scwcond_disp<-reactiveVal(new_data_frame[which(new_data_frame[,10]=='SCW-009'),4])
      gastemp_disp<-reactiveVal(new_data_frame[which(new_data_frame[,10]=='HYD-002'),4])
      brvib_disp<-reactiveVal(new_data_frame[which(new_data_frame[,10]=='ROT-003'),4])
      thrdharm_disp<-reactiveVal(new_data_frame[which(new_data_frame[,10]=='INT-006'),4])
      scwh2diff_disp<-reactiveVal(new_data_frame[which(new_data_frame[,10]=='SCW-015'),4])
      fldtemp_disp<-reactiveVal(new_data_frame[which(new_data_frame[,10]=='ROT-005'),4])
      fldgnd_disp<-reactiveVal(new_data_frame[which(new_data_frame[,10]=='ROT-004'),4])
      scwoxy_disp<-reactiveVal(new_data_frame[which(new_data_frame[,10]=='SCW-011'),4])
      
      
      #output$headertxt<-renderText({paste(unitname)})
      #output$unitlabel<- renderText({paste(input$selectedunit, sep="\n")})
      #new_data_frame <- rclr2R_obj(clrCall(obj,"GetPIData", sendtopi))
      output$h2pur_disp <- renderText({paste0(h2pur_disp()," %")})
      output$h2sodiff_disp <- renderText({paste0(h2sodiff_disp()," psi")})
      output$scwfl_disp <- renderText({paste0(scwfl_disp()," gal/min")})
      output$statemp_disp <- renderText({paste0(statemp_disp()," deg C")})
      output$scwcond_disp <- renderText({paste0(scwcond_disp()," mhos")})
      output$gastemp_disp <- renderText({paste0(gastemp_disp()," deg C")})
      output$brvib_disp <- renderText({paste0(brvib_disp()," mil")})
      output$thrdharm_disp <- renderText({paste0(thrdharm_disp()," V")})
      output$scwh2diff_disp <- renderText({paste0(scwh2diff_disp()," psi")})
      output$fldtemp_disp <- renderText({paste0(fldtemp_disp()," deg C")})
      output$fldgnd_disp <- renderText({paste0(fldgnd_disp())})
      output$scwoxy_disp <- renderText({paste0(scwoxy_disp()," ppm")})
      
      if (nrow(my_table) > 0 ) {
        for (i in 1:nrow(my_table)) { if (my_table[i,2] == "-1" ) {my_table[i,4] <- NA}}
      }
      my_table <- my_table[order(my_table[2],my_table[9],my_table[1],decreasing=TRUE),]

      
      
      
      output$alarms = DT::renderDataTable( {
       return(alarmtable(my_table,unitname))
      })
      output$stator = renderDataTable( {
        return(cmtable(my_table,"Stator",unitname))
      })
      output$servwat = renderDataTable( {
        return(cmtable(my_table,"Service Water",unitname))
      })
      output$rotor = renderDataTable( {
        return(cmtable(my_table,"Rotor",unitname))
      })
      output$excsys = renderDataTable( {
        return(cmtable(my_table,"Excitation System",unitname))
      })
      output$hydsys = renderDataTable( {
        return(cmtable(my_table,"Hydrogen System",unitname))
      })
      output$sosys = renderDataTable( {
        return(cmtable(my_table,"Seal Oil System",unitname))
      })
      output$losys = renderDataTable( {
        return(cmtable(my_table,"Lube Oil System",unitname))
      })
      output$scwsys = renderDataTable( {
        return(cmtable(my_table,"SCW System",unitname))
      })
      
      output$mysidebar <- renderUI ({
        sidebarMenu(
          id="selPage",
          menuItem("Information", tabName="information", icon=icon("info-circle")),
          menuItem("Continuous Monitoring",tabName="cm",icon=icon("dashboard")),
          #menuItem("Information", icon=icon("info-circle")),
          menuItem("Maintenance", tabName="pms", icon=icon("tools"), #startExpanded=FALSE,
                   menuSubItem("Maximo PMs", tabName="pms"),
                   menuSubItem("Repair Log", tabName="issuetracker"),
                   menuSubItem("Known Condition", tabName="fmeainspect")
          ),

          #menuItem("Daily Inspection", tabName="inspection", icon=icon("calendar-alt")),
          menuItem("Analysis",tabName="fmea", icon=icon("chart-pie"))#,
                    #menuSubItem("Failure Mode Analysis", tabName="fmea"),
                    #menuSubItem("Update Inspection Data", tabName="fmeainspect")
        )
      })
      isolate({updateTabItems(session, "selPage", "cm")})
      
      # Create Daily Inspection Log Table
      dailyLogAll <- read.xlsx(dashDB,sheet="Daily")
      logLen = nrow(dailyLogAll) + 2
      dailyLog <- dplyr::filter(dailyLogAll,Unit==unitname)
      
      RVdailyLog <- reactiveValues(
        data = dailyLog
          )

      output$insplog = renderDT({ 
                          datatable(
                              RVdailyLog$data,
                              options = list(
                                scrollX = TRUE,
                                columnDefs=list(list(visible=FALSE,targets=21)))) %>% 
        formatStyle(
          names(RVdailyLog$data[2:18]),
          target='cell',
          backgroundColor = styleEqual(c('na','low','high','no'),c('yellow','red','red','red'))
        )
      })
      
      dailyData <- reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- t(data)
        data
      })
      
      # observe ({
      #   RVdailyLog$data <- dailyLog
      # })

      observeEvent(input$submitDaily, {
        newDailyLog <- cbind(Timestamp = humanTime(),isolate(dailyData()),Unit = unitname)
        append2xlServer("writedailydata", newDailyLog, dashDBwrite, "Daily", unitname, logLen)
        colnames(newDailyLog) <- colnames(dailyLog)
        RVdailyLog$data <<- rbind(isolate(RVdailyLog$data),newDailyLog)
        logLen <<- logLen + 1
        updateTextAreaInput(session,"inspnotes",value = '')
        updateTextInput(session,"operator",value='')
      })
      
      #FMEA
      #dashDB="c:/Docker/GenDash/www/DashData.xlsx"
      #fmeaXL ="c:/Docker/GenDash/www/MC4.xlsx"
      #unitname = 'Mill Creek Unit 4'
      

      # Load initial FMEA inspection data
      roundsdf <- read.xlsx(dashDB,sheet="InspectionResult")
      if ( is.null(roundsdf) ) {
        roundsdf <- data.frame(
          SensID=character(),
          Unit=character(),
          Condition=character()
        )
      }
      roundsdf<-data.frame(lapply(roundsdf, as.character), stringsAsFactors=FALSE)
      roundsdf_unitfilt <- dplyr::filter(roundsdf,Unit==unitname)
      rounds_else <- filter(roundsdf,Unit!=unitname)
      roundsdf_unitfilt <- filter(roundsdf_unitfilt,Condition!='N/A')
      
      for( i in 1:nrow(FMEARV$data) ) {
        sens<-as.character(FMEARV$data[i,'SensID'])
        rownum <- which(roundsdf_unitfilt[,'SensID']==sens) 
        if (length(rownum)==0) { 
          FMEARV$data[i,'Condition'] <- 'OK'
        } else {
          FMEARV$data[i,'Condition'] <- roundsdf_unitfilt[rownum,'Condition']
        }
      }
      
      # Update FMEA Analysis table when the inspection data changes 
      #observeEvent (FMEARV$data, ignoreNULL = FALSE, ignoreInit = FALSE, {
      observe ({FMEARV$data
      ifelse (stasel == "Cane Run", gentype <- "Air-Cooled", gentype <- "Water-Cooled")
      #gentype <- "Water-Cooled"  # eventually get this value from generator data
      FMEA <- read.xlsx(fmeaXL,sheet="FMEA")
      FMEAadd <- read.xlsx(dashDB,sheet="FMEAadd")
      FMEAadd <- subset(FMEAadd, select=-c(1,6))
      #FMEAadd <- FMEAadd[,c('FMEA ROW','Monitoring Technology Classification'):=NULL]
      FMEA <- cbind(FMEA,FMEAadd)
      names(FMEA) <- gsub(" ", ".", names(FMEA))
      FMEA['Eval'] <- NA
      #dt <- data.table(FMEA)
      dt <- filter(data.table(FMEA),Generator.Type==gentype|Generator.Type=="Common")
      dt[,FM := Failure.Mode[1], .(cumsum(!is.na(Failure.Mode)))]
      dt[,Eff := Effect[1], .(cumsum(!is.na(Effect)))]
      dt[,Sev := SEV[1], .(cumsum(!is.na(SEV)))]
      dt[,Cau := Cause[1], .(cumsum(!is.na(Cause)))]
      #dt[,Prob := SEV[1], .(cumsum(!is.na(SEV)))]
      #dt[,Cau := Cause[1], .(cumsum(!is.na(Cause)))]
      dt[,Prob := PROB[1], .(cumsum(!is.na(PROB)))]
      dt[,Rem := Remedy[1], .(cumsum(!is.na(Remedy)))]
      #dt <- filter(dt,Monitoring.Technology.Classification=="OLC")
      dt <- dt %>% group_by(FM)
      dt<-data.frame(lapply(dt, as.character), stringsAsFactors=FALSE)
      dt$Eval <- as.integer(dt$Eval)
      pidf<-data.frame(lapply(new_data_frame, as.character), stringsAsFactors=FALSE)
      #roundsdf <- read.xlsx(dashDB,sheet="InspectionResult")

     # roundsdf4inspect <<- roundsdf_unitfilt

      roundsdf_unitfilt <- filter(FMEARV$data,( Condition!='OK' & Condition != 'NA' ))
      for (i in 1:nrow(dt)) {
        sens<-dt[i,19]
        rownum<-which(roundsdf_unitfilt[,1]==sens) 
        if (length(rownum)==0) {next}
        dt[i,23] <- ( ( any(str_detect(dt[i,22],roundsdf_unitfilt[rownum,4]) ) )  |  (roundsdf_unitfilt[rownum,4]=='Bad') )
      }
      for (i in 1:nrow(dt)) {
        sens<-as.character(dt[i,19])
        rownum<-which(pidf[,10]==sens)
        if (length(rownum)==0) {next}
        dt[i,23] <- ( ( any(str_detect(dt[i,22],pidf[rownum,3]) ) )  & (pidf[rownum,2]>=dt[i,21]) )
        dt[i,30] <- pidf[rownum,8] 
      }
      if ( ncol(dt)!=30 ) { dt[,30]<-NA }
      colnames(dt)[30]<-"CS"
      FMEAtable <- dt %>% group_by(FM) %>% dplyr::summarise(across(23,sum,na.rm=TRUE), n())
      FMEAtable <- filter(FMEAtable,Eval>0)
      dt<-filter(dt, Eval>0)
      newdt <- data.table(dt[,19:30])
      newdt$SensType <- ifelse(as.integer(str_sub(newdt$LKE.Sensor,start=-3))>99,1,0)
      #newdt$CS[newdt$CS=="" && newdt$SensType==0] <- paste0("\\\\*******\\Generation\\Generator FMEA\\",unitname,"\\",newdt$Sys,"\\",newdt$Sensor.Name,"|Average") # Don't have system name here, can't build link :(
      newdt$Link <- ifelse(newdt$SensType==0,createLink(newdt$CS,newdt$Sensor.Name),newdt$Sensor.Name)
      newdt <- subset(newdt, select=-c(1,2,3,4,5))   # why is this the only way to drop columns that works?@!#
      colnames(FMEAtable)[3]<-"Number"
      FMEAtable <- FMEAtable %>% mutate(Odds=as.integer(100*Eval/Number))
      FMEAtable <- FMEAtable %>% mutate(OddsDisp=paste0(FMEAtable$Odds,"%<br><br>[",FMEAtable$Eval," of ",FMEAtable$Number,"]"))
      FMEAtable <-merge(newdt,FMEAtable,by="FM",all=TRUE)
      FMEAtable <- FMEAtable[,c(1,13,2,6,4,9,3,5,10:12,8)]
      FMEAtable <- FMEAtable[order(FMEAtable$Odds,FMEAtable$FM,FMEAtable$Number,decreasing=TRUE),]
      colnames(FMEAtable) <- c("Failure Mode","Confidence","Effect","Remedy","Cause","Sensing","Severity","Probability","# Indicated","# Indicators","Odds","Type")
      output[["fmeatable"]] <- renderDT({
        dtable <- datatable(FMEAtable, rownames = FALSE, escape=FALSE, 
                            options = list(
                              rowsGroup = list(0,1,2,3,4,5), # merge cells
                              pageLength = 100,
                              columnDefs=list(
                                list(visible=FALSE,targets=c(6:11)),
                                list(className='dt-center',targets=c(0:10))
                                )
                              #dom = 't'
                            )) %>% 
                        formatStyle(c(1:11), border = '1px solid #ddd') %>%
                         #color row red if PM is not set (PMNUM=0)
                        formatStyle(
                          'Type',
                          target='cell',
                          backgroundColor = styleEqual(1,'yellow')
                          )
        dep <- htmltools::htmlDependency(
          "RowsGroup", "2.0.0", 
          rowsgrouppath, script = "dataTables.rowsGroup.js")
        dtable$dependencies <- c(dtable$dependencies, list(dep))
        dtable
        })
      }) 
      

        #### Create & Show Reactive FMEA Inspections table
      #  FMEARV <- reactiveValues(data=fmeainspectdf)
        output[["fmeainspect"]] <- renderDT({
          #dtable <- datatable(fmeainspectdf, rownames = FALSE, escape=FALSE, 
          datatable(
            FMEARV$data,
            #fmeainspectdf, 
            rownames = FALSE, 
            escape=FALSE, 
            filter = "top",
            options = list(
              #rowsGroup = list(0,1,3,2,4,5), # merge cells
              pageLength = 100,
              columnDefs=list(
                list(visible=FALSE,targets=c(0)),
                list(className='dt-center',targets=c(0:3))
              )
              #dom = 't'
            )) %>% 
            formatStyle(c(1:11), border = '1px solid #ddd', fontWeight = 'bold') %>%
            #color row red if PM is not set (PMNUM=0)
            formatStyle(
              'Condition',
              target='row',
              backgroundColor = styleEqual(c('N/A','Bad','Hi','Lo','OK'),c('yellow','red','red','red','green'))
            )
          # dep <- htmltools::htmlDependency(
          #     "RowsGroup", "2.0.0", 
          #     rowsgrouppath, script = "dataTables.rowsGroup.js")
          #   dtable$dependencies <- c(dtable$dependencies, list(dep))
          #   dtable
          #})
        })
        #fmeadata <- filter(isolate(FMEARV$data),Condition != "OK")

        # column(2, offset=9,
        #        #box(
        #        #itle="Inspection Results",
        #        # width=12,
        #        actionButton("updateinsp","Set Condition of Selected Rows to")
        # )
        #),)
        
        # Update FMEA Inspection table when any statuses are changed
        observeEvent(input$inspcondition, ignoreNULL = TRUE, ignoreInit = TRUE, {
          req(input$inspcondition != '')
          # if (exists("fmeainspectdf")) {
          for ( i in 1:length(input$fmeainspect_rows_selected) ) {
            rownum <- input$fmeainspect_rows_selected[i]
            # msg(rownum)
            FMEARV$data[rownum,'Condition'] <- input$inspcondition
          }
          #  }
          fmeadata <- filter(isolate(FMEARV$data),Condition != "OK")
          if (nrow(fmeadata) > 0 ) {
            #n <- nrow(fmeadata)
            vec <- rep(unitname,nrow(fmeadata))
            fmeadata <- cbind(fmeadata,vec)
            names(fmeadata) <- names(rounds_else)
          } else {
            fmeadata <- rounds_else[,FALSE]
          }

          fleetinsp <- rbind(fmeadata,rounds_else)
          #fleetinsp <- subset(fleetinsp,select=-c(2,3))
          send2xlServer("writeinspdata", fleetinsp, dashDBwrite, "InspectionResult", unitname)
          updateSelectInput(session,"inspcondition", selected = '' ) 
          
          
       }) 
        
	     repairServer("repairs", dashDBwrite, "Repairs", unitname)
	     repairServer("investigations", dashDBwrite, "Investigations", unitname)
		  
	     #errs <- data.frame(NULL)
	     #col.names = "msg"
	     errs <- data.frame(NULL)
	     if (nrow(new_data_frame) == 0) {errs <- rbind(errs,"PI Data Unavailable")}
	     if (maxerr == TRUE) {errs <- rbind(errs,"Maximo Data Unavailable")}
	     
	     if ( nrow(errs) > 0 ) {
	       colnames(errs) <- "msg"
	       output$errMenu <- renderMenu({
	       msgs <- apply(errs,1,function(row) {
	         notificationItem(text = row[["msg"]], icon = icon("exclamation-triangle") )
	         })
	       dropdownMenu(type = "notifications", .list = msgs)
	     })
	     } else {
	       removeUI(
	         selector = "#errMenu"
	       )
	       #output$errMenu <- renderMenu({dropdownMenu(type = "notifications", .list = NULL)})
	     }
	     
     # shinyjs::hide(id = "loadSpinner")
      
	     output$genNameplate <- renderImage({
	       imgname <- paste('./',extPath,'/Nameplate/Generator/',unitname,'.jpg', sep='')
	       list(src = imgname)
	     }, deleteFile = FALSE)
	     
	     removeModal()
	     
  }, ignoreNULL = FALSE)
}

#### VARIABLES ####
ostype=""
unitname=""
logmessage=""
kerb=""
RealPow=""
ReacPow=""
Freq=""
Pfac=""
Tvol=""
stasel=""
sql_piv=""
offline=""
#if (!exists("usedname")){usedname<-""}
ui <- dashboardPage(skin='black',
  dashboardHeader(
    #title="DEV VERSION",  #icon=icon("industry"),
    leftUi = tagList(
    span(htmlOutput("unitLabel"), style="font-size:32px;font-weight:bold")
    ),
    #tags$li(class = "dropdown", tags$a(HTML(paste(textOutput("logmessage")))),# style="font-size:12px;text-align:center;vertical-align:top;"))),#font-weight:bold"),
    #tags$li(class = "dropdown", tas$a(passwordInput("password","Password:")),
    #   type = "notifications", 
    #   #icon = icon("question-circle"),
    #   badgeStatus = NULL,
    #   headerText = "Log In",
    #   ,
    #   
    dropdownMenuOutput("errMenu"),
    dropdownMenu(
      type = "tasks",
      icon = icon("user"),
      badgeStatus = NULL,
      headerText =
        #tags$li(class = "dropdown", tags$a(HTML(paste(textOutput("logmessage"),
        #notificationItem("Dashboard Update Tasks", icon = icon("file"),
        tags$li(
          uiOutput("logininputs")
        )
    ),
    dropdownMenu(
      type = "notifications", 
      icon = icon("question-circle"),
      badgeStatus = NULL,
      headerText = "",
      notificationItem("Take Screenshot", 
                       icon = icon("camera"),
                       inputId = "grabngo"
      )#,

  )

  ),
 dashboardSidebar(
   uiOutput("mysidebar"),
   br(),
   br(),
   box(
     id="unitselect",
     #collapsible=TRUE,
     title="Select a Generator:",
     width=12,
     background="black",
     headerBorder=FALSE,
     selectInput("station","Station",c("","Cane Run","EW Brown","Ghent","Mill Creek","Trimble County"),multiple=FALSE),
     selectInput("unit","Unit","")
     ),
   br(),
   br()
   # box(
   #   id="user",
   #   width=12,
   #   #background="black",
   #   span(textOutput("logmessage"), style="font-size:32px;font-weight:bold")
   # ),
   ),
  dashboardBody(
  tags$head(tags$style(".rightAlign{float:right;}")),
  setShadow(class = "dropdown-menu"),
    useShinyjs(),
    # box(
    #   title="Loading...",
    #   id="loadSpinner",
    #   width=12,
    #   headerBorder=FALSE,
    #   br(),
    #   br(),
    #   br(),
    #   loadingState()
    # ),
  box(
    title="Welcome to the ************* Generator Health Dashboard",
    id="intro",
    width=12,
    headerBorder=FALSE,
    h5("Select a generator from dropdown menu and a category from sidebar menu to find unit-specific information.")
    ),
    tabItems(
      tabItem("information",
              fluidPage(
               fluidRow(
                  br(),
                  #box(width=9,title="Action Items:",br(),br(),br(),br()),
                  #box(width=9,title="Nameplate Data",br(),br(),br(),br())
                  valueBoxOutput("realpow", width=4),
                  valueBoxOutput("reacpow", width=4),
                  valueBoxOutput("pfac", width=2),
                  valueBoxOutput("freq", width =2)
                ),
              fluidRow(
                br(),
                #box(width=9,title="Action Items:",br(),br(),br(),br()),
                #box(width=9,title="Nameplate Data",br(),br(),br(),br())
                valueBoxOutput("termvol", width=4)
              ),
              fluidRow(
                box(title="Generator Nameplate",
                    width=8,
                    imageOutput("genNameplate",
                      inline=TRUE
                     )
                    )
                )
      )),
      tabItem("inspection",
                  fluidPage(
                 # tags$style("
                  #.nav-tabs {background: #f4f4f4;}
                  #.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {background-color: #000;
                  #                               border-color: #000;                                                      
                  #                               }
                  #.nav-tabs-custom .nav-tabs li.active {border-top-color: 
                  #                                      #314a6d;
                  #}
                #   shiny-tab-inspection.tab-pane.active {background: #000000;}"
                #  ),
                  tabBox(
                    width = 12,
                    tabPanel(
                        width=12,
                        background='light-blue',
                      "Add Log Entry",
                       #div(style='padding:10px;width:1000px',
                      # id="inspform",
                       br(),
                       fluidRow(  
                         #column(width=12,align = "center", radioButtons("h2pur",textOutput("h2pur_disp"),c("Normal"="norm","Low"="low","N/A"="na"),inline=TRUE)),
                         column(width=4,align = "center", box(title="1. Hydrogen Purity",width=12,radioButtons("h2pur",textOutput("h2pur_disp"),c("Normal"="norm","Low"="low","N/A"="na"),inline=TRUE))),
                         column(width=4,align = "center", box(title="2. Stator Cooling Water Flow",width=12,radioButtons("scwfl",textOutput("scwfl_disp"),c("Normal"="norm","Low"="low", "High"="hi", "N/A"="na"),inline=TRUE))),
                         column(width=4,align = "center", box(title="3. Stator Temperature",width=12,radioButtons("statemp",textOutput("statemp_disp"),c("Normal"="norm","Low"="low", "High"="hi", "N/A"="na"),inline=TRUE)))
                       ),
                       br(),
                       fluidRow( 
                         column(width=4,align = "center", box(title="4. Hydrogen-Seal Oil Differential Pressure",width=12,radioButtons("h2sodiff",textOutput("h2sodiff_disp"),c("Normal"="norm","Low"="low","High"="high","N/A"="na"),inline=TRUE))),
                         column(width=4,align = "center", box(title="5. Stator Cooling Water Conductivity",width=12,radioButtons("scwcond",textOutput("scwcond_disp"),c("Normal"="norm","Low"="low", "High"="hi", "N/A"="na"),inline=TRUE))),
                         column(width=4,align = "center", box(title="6. Cold Gas Temperature",width=12,radioButtons("gastemp",textOutput("gastemp_disp"),c("Normal"="norm","Low"="low", "High"="hi", "N/A"="na"),inline=TRUE)))
                       ),
                       br(),
                       fluidRow( 
                         column(width=4,align = "center", box(title="7. Stator Cooling Water-Hydrogen Differential Pressure",width=12,radioButtons("scwh2diff",textOutput("scwh2diff_disp"),c("Normal"="norm","Low"="low", "High"="hi", "N/A"="na"),inline=TRUE))),
                         column(width=4,align = "center", box(title="8. Field Temperature",width=12,radioButtons("fldtemp",textOutput("fldtemp_disp"),c("Normal"="norm","Low"="low", "High"="hi", "N/A"="na"),inline=TRUE))),
                         column(width=4,align = "center", box(title="9. Field Ground Detected",width=12,radioButtons("fldgnd",textOutput("fldgnd_disp"),c("Normal"="norm","Detected"="detected","N/A" = "na"),inline=TRUE)))
                        ),
                        br(),
                        fluidRow( 
                          column(width=4,align = "center", box(title="10. Bearing Vibration",width=12,radioButtons("brvib",textOutput("brvib_disp"),c("Normal"="norm","High"="high","N/A"="na"),inline=TRUE))),
                          column(width=4,align = "center", box(title="11. 3rd Harmonic Voltage",width=12,radioButtons("thrdharm",textOutput("thrdharm_disp"),c("Normal"="norm","High"="hi","N/A"="na"),inline=TRUE))),
                          column(width=4,align = "center", box(title="12. SCW Oxygen",width=12,radioButtons("scwoxy",textOutput("scwoxy_disp"),c("Normal"="norm","Low"="low", "High"="hi", "N/A"="na"),inline=TRUE)))
                        ),
                       br(),
                       fluidRow(
                         column(
                            width=8,
                            box(
                                width=12,
                                radioButtons("hydry","13. Are hydrogen dryers in service and operating properly, with desiccant in good condition?",c("Yes"="yes","No"="no","N/A"="na"),inline=TRUE),
                                radioButtons("liqdet","14. Are liquid detectors free from water & oil?",c("Yes"="yes","No"="no","N/A"="na"),inline=TRUE),
                                radioButtons("brrig","15. Brush rigging is not broken, vibrating, or arcing?",c("Yes"="yes","No"="no","N/A"="na"),inline=TRUE),
                                radioButtons("shftgrnd","16. Are the shaft grounding brush(es) or braid(s) in good physical condition?",c("Yes"="yes","No"="no","N/A"="na"),inline=TRUE),
                                radioButtons("fldgrnd","17. Is the field winding ground fault detection system operational?",c("Yes"="yes","No"="no","N/A"="na"),inline=TRUE)
                                )
                        ),
                        column(
                           width=4, align = "left", 
                           textAreaInput("inspnotes","Inspection Notes", rows=10),
                      # fluidRow( 
                         #column(width=4, align = "center", box(title="Bearing Vibration",width=12,radioButtons("brvib",textOutput("brvib_disp"),c("Normal"="norm","Low"="low","High"="high","N/A"="na"),inline=TRUE))),
                         #column(width=4, align = "center", box(title="3rd Harmonic Voltage",width=12,radioButtons("thrdharm",textOutput("thrdharm_disp"),c("Normal"="norm","Low"="low","N/A"="na"),inline=TRUE))),
                         # column(width=4, radioButtons("fldtemp",verbatimTextOutput("fldtemp_disp"),c("Normal"="norm","Low"="low","N/A"="na"),inline=TRUE))
                      # ),
                           textInput("operator","Operator",""), actionButton("submitDaily", "Submit", class = "btn-primary")
                        )
                       )
                       #br(),
                       #fluidRow(column(width=12, align = "center", textInput("name","Operator",""), actionButton("submit", "Submit", class = "btn-primary")))
                       #fluidRow(column(width=1),actionButton("submit", "Submit", class = "btn-primary"))
                    #  )
                    ),
                  tabPanel(
                    "Inspection Log",
                    DTOutput("insplog")
                  )
                 )
          )
      ),

      tabItem("cm",
              fluidRow(
                tabBox(
                  width=12,
                  tabPanel("Alarms",
                      DTOutput("alarms")
                      ),
                  tabPanel("Stator",
                     DTOutput("stator")
                  ),
                  tabPanel("Rotor",
                           DTOutput("rotor")
                  ),
                  tabPanel("Excitation System",
                           DTOutput("excsys")
                  ),
                  tabPanel("Hydrogen System",
                           DTOutput("hydsys")
                  ),
                  tabPanel("Seal Oil System",
                           DTOutput("sosys")
                  ),
                  tabPanel("Stator Cooling Water System",
                           DTOutput("scwsys")
                  ),
                  tabPanel("Lube Oil System",
                           DTOutput("losys")
                ),
                tabPanel("Station Service Water",
                         DTOutput("servwat")
                )
              ),
              #tags$div(class="header",
              #box(width=12,title="Alarm Status",
              #tags$h3("one sensor in alarm",style="color:yellow;"),
              #tags$h3("sensor total in alarm",style="color:orange;"),
              #tags$h3("all sensors in alarm",style="color:red;")
              # # Orange = sensor total in alarm \n
              ##  Red = all sensors in alarm")
              #), class='rightAlign')
              )), 
            #  tags$a(href="shiny.rstudio.com/tutorial", "Click Here!")
      tabItem("pms",
              fluidPage(
                tabBox(
                  width=12, 
                  tabPanel("Routine Maintenance",# collapsible=TRUE,
                     pmtableUI("appB")
                     #DT::dataTableOutput("pmtableB") 
                    ),
                  tabPanel(
                    "Outage Maintenance",
                    pmtableUI("appC"),# collapsible=TRUE,
                    #DT::dataTableOutput("pmtableC") 
                  )
                )
              )
            ),
      # tabItem("outagepms",
      #         fluidPage(
      #         #box(width=12, title="Outage Maintenance", collapsible=TRUE,
      #               DT::dataTableOutput("pmtableC")
      #         #)
      #         )),
       tabItem("issuetracker",
               fluidPage(
                 tabBox(
                   width=12,
                   #type="tabs",
                   tabPanel("Repair Log",
			          		  repairUI("repairs")
			          		  ),
			             tabPanel("Investigations",
			          	    repairUI("investigations")
			          		  )
                 )
               )
               ),
      tabItem("fmea",
              fluidPage(
                DT::dataTableOutput("fmeatable") 
                #)
              )),
      tabItem("fmeainspect",
              fluidPage(
                fluidRow(
                  column(4,offset=7,
                    selectInput(
                      "inspcondition",
                      "Set Condition of Selected Rows to",
                      c('','OK','N/A','Bad','Hi','Lo')
                    )
                   )
                ),
                box(
                width=10,
                DT::dataTableOutput("fmeainspect") 
                )
                )
              )
  #    )
#  br(),
 # img(src='https://bi.sp.*******.int/sites/GenEngineering/Electrical/Documents/lke_logo.png',height=50,style="display: block; margin-left: auto; margin-right: 0;align:bottom;")
  )),
 dashboardControlbar(disable=TRUE)
)
#onStop(function() {
#  system2('kdestroy')
#})
app <- shinyApp(ui = ui, server = server)
runApp(app,host="0.0.0.0",port=3838,launch.browser=FALSE)
