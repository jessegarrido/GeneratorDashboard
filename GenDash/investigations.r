# create_btns <- function(x) {
#   x %>%
#     purrr::map_chr(~
#                      paste0(
#                        '<div class = "btn-group">
#                    <button class="btn btn-default action-button btn-info action_button" id="edit_',
#                        .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button></div>'
#                    #<button class="btn btn-default action-button btn-danger action_button" id="delete_',
#                     #   .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
#                      ))
# }


repairUI <- function(id) {
	ns <- NS(id)
	tagList(
		DT::DTOutput(ns("dt_table")),
		shiny::actionButton(
			inputId = ns("add"),
			label = "Add New",
			icon = shiny::icon("plus"),
			class = "btn-success"
			),
		shiny::actionButton(
		  inputId = ns("edit_btn"),
		  label = "Edit Selected",
		  icon = shiny::icon("edit"),
		  class = "btn-primary"
		),
		#shiny::includeScript("www/addinvestigation.js")
		)
	}


repairServer <- function(id, dashDB, table, unitname) {


	moduleServer(
		id,
			function(input,output,session) {
			
			
			modal_dialog <- function(ope,syst,desc,rpr,vend,iden,rprd,sts,selected_sts,edit) {
  ns <- session$ns
  if (edit) {
    x <- "Apply Edit"
  } else {
    x <- "Add Item"
  }
  shiny::modalDialog(
    title = "Edit Investigation",
      class = "text-center",
      div(
        div(
         # style = "display:block; float:right;padding-bottom-30px;",
          style = "display:block;",
          shiny::checkboxInput(
            inputId = ns("ope"),
            label = "Case Open",
            value = ope,
            width = "100%"
          )
        ),
        div(
          style = "display: block;",
          shiny::textInput(
            inputId = ns("syst"),
            label = "System",
            value = syst,
            width = "100%"
          )
        ),
        div(
          style = "display: block;",
          shiny::textAreaInput(
          inputId = ns("desc"),
          label = "Issue Description",
          value = desc,
          placeholder = "Describe the issue...",
          width = "100%",
          rows = 5
          )
        ),
        div(
          style = "display: block;",
          shiny::textAreaInput(
          inputId = ns("rpr"),
          label = "Description of Repair",
          value = rpr,
          placeholder = "Describe the repair work...",
          width = "100%",
          rows = 5
          )
      ),
      div(
        style = "display: block;",
        shiny::textInput(
          inputId = ns("vend"),
          label = "Vendor(s)",
          width = "100%",
          value = vend
          #selected = selected_vs,
          #choices = unique(vs)
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::dateInput(
          inputId = ns("iden"),
          label = "Identified Date",
          value = iden,
          #format = "yyyy-mm-dd",
          width = "100%"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::dateInput(
          inputId = ns("rprd"),
          label = "Repair Date",
          value = rprd,
          #format = "yyyy-mm-dd",
          width = "100%"
        )
      ),
      div(
        style = "display: block;",
        shiny::selectInput(
          inputId = ns("sts"),
          label = "Work Status",
          width = "100%",
          #value = sts
          choices = c("","Issue Identified","Under Investigation","Work Scheduled", "Under Repair","Work Complete","Work Declined"),
          selected = selected_sts
        )
      ),
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      #class = "container",
      shiny::actionButton(
        inputId = ns("final_edit"),
        label = x,
        icon = shiny::icon("edit"),
        class = "btn-info"
      ),
      shiny::actionButton(
        inputId = ns("dismiss_modal"),
        label = "Close",
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}
			# #Initialize modal dialogue for Investigation Tracker
			# rv <- shiny::reactiveValues(
			#   df = NULL,
			#   dt_row = NULL,
			#   add_or_edit = NULL,
			#   edit_button = NULL,
			#   keep_track_id = NULL
			# )
			
			appIssues<-read.xlsx(xlsxFile = dashDB, sheet=table,colNames = TRUE,skipEmptyRows = TRUE)
			#appIssues <- sqlFetch(PMs,"Investigations")
			#odbcCloseAll()
			if ( nrow(appIssues)<2) {
				items <- 1 #appIssues[1,] <- (c("1","1","1","1","1","1","1","1","1","1"))
			#appIssues <- appIssues[FALSE,]
			} else {
			  appIssues$Issue<-as.numeric(appIssues$Issue)
				items <- max(appIssues$Issue,na.rm=TRUE) + 1
			}
			charappIssues <- appIssues
			appIssues[] <-lapply(charappIssues, as.character)
			#setcolorder(appIssues,c(12,1,7,5,4,8,6,10:11,2:3,9))
      
			#issues <- appIssues[FALSE,]
			if (any(appIssues==unitname,na.rm=TRUE)) {
				issues <- filter(appIssues,appIssues$Generator==unitname)
				otherissues <- filter(appIssues,appIssues$Generator!=unitname)
			#	x <- create_btns(1:nrow(issues))
			} else {
				issues <- appIssues[FALSE,]
				otherissues <- appIssues
			#	x <- ""
			}
      
		#	issues <- issues %>%
		#	dplyr::bind_cols(tibble("Edit" = x)) # %>%
			#dplyr::mutate(Open = ifelse(Open == "FALSE", FALSE, TRUE))# %>%
			#dplyr::mutate(am = ifelse(am == 0, "automatic", "manual"))
      
			rv <- shiny::reactiveValues(
				df = issues,
				dt_row = NULL,
				add_or_edit = NULL,
				#edit_button = NULL,
				keep_track_id = items
			)
      
			output$dt_table <- DT::renderDT(
				{	
					datatable(shiny::isolate(rv$df),
					         
				escape = F,
				rownames = FALSE,
				selection = 'single',
				options = list(
					processing = FALSE,
					columnDefs = list(
						list(visible=FALSE,targets=c(0:1,9))
					)
				)	
					)  %>%
				    formatStyle(
				      'Open',
				      target='row',
				      backgroundColor = styleEqual('TRUE','yellow')
				    )
				}
			)  

		#ns <- NS(id)
		proxy <- DT::dataTableProxy("dt_table")
		
		shiny::observe({
		 	DT::replaceData(proxy, rv$df, resetPaging = FALSE, rownames = FALSE)
		  
		 })

		#shiny::observeEvent(input$current_id, {
		shiny::observeEvent(input$edit_btn, ignoreNULL = TRUE, ignoreInit = TRUE,{
			#shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit"))
		  shiny::req(!is.null(input$dt_table_rows_selected) )
		  #rv$dt_row <- which(stringr::str_detect(rv$df$Edit, pattern = paste0("\\b", input$current_id, "\\b")))
			rv$dt_row <- input$dt_table_rows_selected
		  df <- rv$df[rv$dt_row, ]
			modal_dialog(
				ope = ifelse(is.logical(df$Open),eval(parse(text=df$Open)),FALSE),syst = df$System,desc = df$Description,rpr = df$Repair,vend = df$Vendors,iden = df$Identified,rprd = df$Repaired,sts = issues$Status,selected_sts=df$Status,edit = TRUE
			)
			rv$add_or_edit <- NULL
		})
		
		# when final edit button is clicked, table will be changed
		shiny::observeEvent(input$final_edit, ignoreNULL = TRUE, {
			#shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit") & is.null(rv$add_or_edit))
		  shiny::req(is.null(rv$add_or_edit))
		  rv$edited_row <- dplyr::tibble(
				Issue = rv$df$Issue[rv$dt_row],
				#ID = rv$df$Issue[rv$dt_row],
				Open = as.character(input$ope),
				System = input$syst,
				Description = input$desc,
				Repair = input$rpr,
				Vendors = input$vend,
				Identified = ifelse(is.Date(input$iden),as.character(input$iden),NA),
				Repaired = ifelse(is.Date(input$rprd),as.character(input$rprd),NA),
				#Identifid = '12/12/2012',
				#Repaired = '12/12/2012',
				Status = input$sts,
				Generator = as.character(unitname),
				#Edit = rv$df$Edit[rv$dt_row]
			)
			rv$df[rv$dt_row, ] <- rv$edited_row
			issues_write <- isolate(rv$df)
			issues_write <- rbind(issues_write,otherissues)
			#setcolorder(issues_write,c(9,10,4,3,6,2,5,11,7,8,12,1))
			#PMs<-odbcConnectAccess2007(PMdb.con)
			#ColumnsOfTable       <- sqlColumns(PMs, "Investigations_insert")
			#varTypes             <- as.character(ColumnsOfTable$TYPE_NAME) 
			# names(varTypes)      <- as.character(ColumnsOfTable$COLUMN_NAME) 
			#colnames(issues_write) <- as.character(ColumnsOfTable$COLUMN_NAME)
			#sqlUpdate(PMs,issues_write,tablename="Investigations_insert",index="Issue",fast=TRUE,verbose=TRUE)#,append=TRUE,safer=TRUE)
			#odbcCloseAll()
			wb=loadWorkbook(dashDB)
			#sheets <- getSheetNames(dashDB)
			#sheetnum <- which(sheets==table)
			removeWorksheet(wb,table)
			saveWorkbook(wb,dashDB,overwrite = TRUE)
			#issues_write <- c("1","1","1","1","1","1","1","1","1")
			addWorksheet(wb,table)
			writeData(wb,table,issues_write,keepNA=FALSE)
			saveWorkbook(wb,dashDB,overwrite = TRUE,returnValue=TRUE)
			#message(savesucc)
			remove(issues_write)
		},ignoreInit=TRUE)
		
		shiny::observeEvent(input$add, ignoreNULL = TRUE, {
			modal_dialog(
				ope = TRUE,syst = "",desc = "",rpr = "",vend = "",iden = "",rprd = "",sts = "",selected_sts=issues$Status,edit = FALSE
			)
			rv$add_or_edit <- 1
		},ignoreInit=TRUE)
      
		shiny::observeEvent(input$final_edit, ignoreNULL = TRUE, {
			shiny::req(rv$add_or_edit == 1)
			add_row <- dplyr::tibble(
				Issue = as.character(rv$keep_track_id),
				#ID = as.character(rv$keep_track_id),
				Open = as.character(input$ope),
				System = input$syst,
				Description = input$desc,
				Repair = input$rpr,
				Vendors = input$vend,
				Identified = ifelse(is.Date(input$iden),as.character(input$iden),NA),
				Repaired = ifelse(is.Date(input$rprd),as.character(input$rprd),NA),
				#Identifid = '12/12/2012',
				#Repaired = '12/12/2012',
				Status = input$sts,
				Generator = as.character(unitname),
				#Edit = create_btns(rv$keep_track_id)
			)
			#ifelse(add_row$Open==TRUE,add_row$Open<-"TRUE",f$Open<-"FALSE")
			rv$df <- add_row %>%
			dplyr::bind_rows(rv$df)
			rv$keep_track_id <- rv$keep_track_id + 1
			#issues_write <- subset(as.data.frame(rv$df), select=-c(Edit))
			issues_write <- isolate(rv$df)
			issues_write <- rbind(issues_write,otherissues)
        #issues$ID <- issues$ID[FALSE]
        #setcolorder(issues_write,c(9,10,4,3,6,2,5,11,7,8,12,1))
        #PMs<-odbcConnectAccess2007(PMdb.con)
        #ColumnsOfTable       <- sqlColumns(PMs, "Investigations")
       # varTypes             <- as.character(ColumnsOfTable$TYPE_NAME) 
        #names(varTypes)      <- as.character(ColumnsOfTable$COLUMN_NAME) 
       #colnames(issues_write) <- as.character(ColumnsOfTable$COLUMN_NAME)
        #sqlSave(PMs,issues_write,tablename="Investigations_insert",fast=FALSE,append=TRUE,safer=TRUE,rownames=FALSE,verbose=TRUE)
        #odbcCloseAll()
			wb=loadWorkbook(dashDB)
			#sheets <- getSheetNames(dashDB)
			#sheetnum <- which(sheets==table)
			removeWorksheet(wb,table)
			saveWorkbook(wb,dashDB,overwrite = TRUE)
			#issues_write <- c("1","1","1","1","1","1","1","1","1")
			addWorksheet(wb,table)
			writeData(wb,table,issues_write,keepNA=FALSE)
			savesucc <- saveWorkbook(wb,dashDB,overwrite = TRUE,returnValue=TRUE)
			#message(savesucc)
			remove(issues_write)
      },ignoreInit=TRUE)
	  
# 	  ### remove edit modal when close button is clicked or submit button
#       shiny::observeEvent(input$dismiss_modal, {
#         shiny::removeModal()
#       },ignoreInit=TRUE)
#       
	  shiny::observeEvent(input$final_edit, {
        shiny::removeModal()
      },ignoreInit=TRUE)
	  }
	)
 }
	  