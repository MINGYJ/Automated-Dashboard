# Install necessary packages if not already installed
if (!require(httr)) install.packages("httr")
if (!require(readxl)) install.packages("readxl")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

# Load the packages
library(httr)
library(readxl)
library(RJSONIO)
library(shiny)
library(shinyjs)
library(shinyalert)
library(lubridate)

#import ui
source("BCM_ui.R")
source("calc_table.R")
source("calc_table_shop.R")
source("table_name_replace.R")
source("graph_render.R")



form_id<-'******'
req_query<-list(format='csv',include_labels_only='True',language="English (en)",show_choice_labels='False')
httr::set_config(httr::config(http_version = 1.1))

#get the url of file
get_export<-function(){
  exp_status_url<-paste("https://api.moda.wfp.org/api/v1/export?xform=",form_id,sep="")
  exp_status_bd<-VERB(verb = "GET", url = exp_status_url, httr::add_headers(Authorization = token))
  if(httr::status_code(exp_status_bd)>300){
    shinyalert("Oops!", "Something went wrong. Invalid Token, please refresh page.", type = "error")
    return("ERROR")
  }
  exp_status<-(httr::content(exp_status_bd))
  #print(exp_status)
  if (length(exp_status)==0){
    return ("")
  }
  exp_status<-unname(unlist(exp_status[[length(exp_status)]]$export_url))
  return(exp_status)
}

#parse the file url to be downloadable file
parse_file_url<-function(res_str){
  file_url<-gsub("export_url,job_status\r\n(.+),SUCCESS\r\n", "\\1", res_str)
  print(paste("parsed url is",file_url))
  return(file_url)
}

#get file url from uuid
parse_file_uuid<-function(res_str){
  uuid<-gsub("job_uuid\r\n(.+)\r\n", "\\1", res_str)
  uuid_status_url<-paste("https://api.moda.wfp.org/api/v1/forms/",form_id,"/export_async?job_uuid=",uuid,sep="")
  uuid_status_bd<-VERB(verb = "GET", url = uuid_status_url, httr::add_headers(Authorization = token))
  exp_status<-(httr::content(uuid_status_bd))
  print(exp_status)
  return (exp_status$export_url)
}

#wait for a while when requesting
testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)#sleep
  proc.time() - p1 # The cpu usage should be negligible
}

api_test<-function(){
  
}

#get data between start and end data by selection
date_subset<-function(table,start_date,end_date){
  if(missing(start_date)){
    start_date<-floor_date(today(), unit = "month") - months(1)
  }
  table$start_time<-substring(table[,1],0,23)
  table$start_time<-ymd_hms(table$start_time)
  table<-subset(table, start_time >= start_date)
  
  if(missing(end_date)){
    end_date<-today()
  }
  table$end_time<-substring(table[,1],0,23)
  table$end_time<-ymd_hms(table$end_time)
  table<-subset(table, end_time <= end_date)
  
  return(table)
}

#binary data to readable urls
hex_to_str<-function(s){
  s<- paste(unlist(s), collapse='')
  h <- sapply(seq(1, nchar(s), by=2), function(x) substr(s, x, x+1))
  return(rawToChar(as.raw(strtoi(h, 16L))))
}

#download data
get_data<-function(start_date,end_date){
  ori_status<-get_export()
  print(ori_status)
  print(ori_status)
  
  req_url<-paste("https://api.moda.wfp.org/api/v1/forms/",form_id,"/export_async",sep="")
  
  #export request
  exp_res_bd<-VERB(verb = "GET", url = req_url, httr::add_headers(Authorization = token),query=req_query)
  exp_res<-hex_to_str(exp_res_bd[["content"]])
  print(exp_res)
  #get file
  if(status_code(exp_res_bd)==202){
    if (grepl('export_url',exp_res,fixed=TRUE)){
      print("no new update,download old file")
      new_status<-parse_file_url(exp_res)
    }else{
      new_status<-parse_file_uuid(exp_res)
      while(length(new_status)==0){
        print("Request sent, wait for file.")
        testit(2)
        new_status<-parse_file_uuid(exp_res)
      }
    }
    #get the file csv url
    file_url<-new_status
    print(file_url)
  }
  
  
  
  # Send GET request with authentication
  response <- GET(file_url, add_headers(Authorization = token))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Create a temporary file
    temp_file <- tempfile(fileext = ".csv")
    
    # Write the content of the response to the temporary file
    writeBin(content(response, "raw"), temp_file)
    
    # Read the CSV file
    data <- read.csv(temp_file)
    
    # Print the data
    #print(data)
    
    # Optionally, remove the temporary file
    unlink(temp_file)
    
    #get this months data
    data<-date_subset(data,start_date,end_date)
    
    #change name
    data<-replace_col(data)
    
    return(data)
  } else {
    stop("Failed to download the file. Status code: ", status_code(response))
  }

}

update_select<-function(df,session){
  dfdt<-(df$x$data)
  dfdt<-unique(dfdt$name)
  updateSelectInput(session, "breadshop",
                    label = paste("Select from ", length(dfdt)," breadshops"),
                    choices = (dfdt)
  )
}


#server func
server<-function(input,output,session){
  
    run_table<-function(){
    #setup table for display
    data_raw<-(get_data(input$date,input$end_date))
    
    #print(names(data_raw))
    

    
    #process data for group bu enumerator 
    data_enu<-sub_group(data_raw)
    #group by breadshop:
    data_shop<-bread_sub_group(data_raw)
    
    #update select part
    update_select(data_shop,session)
    
    #plot bar chart
    observeEvent(input$breadshop,{
      graphs<-graph_render(input$breadshop,data_raw)
      output$queue_pie<-renderPlot({graphs()$queue_pie()})
      output$time_bar<-renderPlot({graphs()$time_bar()})
      output$type_pie<-renderPlot({graphs()$type_pie()})
      output$problem_pie<-renderPlot({graphs()$problem_pie()})
    })
    
    #render table
    print("rendering...")
    output$table <- DT::renderDataTable(data_enu)
    output$bread_table <- DT::renderDataTable(data_shop)
    
    #refresh table
    observeEvent(input$refresh, {
      #setup table for display
      data_raw<-(get_data(input$date,input$end_date))
      #process data for group bu enumerator 
      data_enu<-sub_group(data_raw)
      #group by breadshop:
      data_shop<-bread_sub_group(data_raw)
      
      #update select part
      update_select(data_shop,session)
      
      #plot bar chart
      observeEvent(input$breadshop,{
        graphs<-graph_render(input$breadshop,data_raw)
        output$queue_pie<-renderPlot({graphs()$queue_pie()})
        output$time_bar<-renderPlot({graphs()$time_bar()})
        output$type_pie<-renderPlot({graphs()$type_pie()})
        output$problem_pie<-renderPlot({graphs()$problem_pie()})
      }) 
      
      #render table
      output$table <- DT::renderDataTable(data_enu)
      output$bread_table <- DT::renderDataTable(data_shop)
    })
  }
  
  shinyjs::hide("table_page")
  #get api token
  observeEvent(input$enter_api,{
    token<<-paste("Token ", input$api,collapse='')
    shinyjs::hide("api_login")
    shinyjs::show("table_page")
    run_table()
  })
 
}



shinyApp(ui, server=server)