library(lubridate)
library(dplyr)
library(stats)
library(DT)
library(jsonlite)
#nest the data, dat is the parent row, children is the data under parent row
NestedData <- function(dat, children){
  stopifnot(length(children) == nrow(dat))
  g <- function(d){
    if(is.data.frame(d)){
      purrr::transpose(d)
    }else{
      purrr::transpose(NestedData(d[[1]], children = d$children))
    }
  }
  subdats <- lapply(children, g)
  oplus <- ifelse(lengths(subdats), "⊕", "") 
  cbind(" " = oplus, dat, "_details" = I(subdats), 
        stringsAsFactors = FALSE)
}


pre_process<-function(df){
  df<-df %>% relocate("Google Map Location",.after="If Other Enumerator")
  df<-df %>% relocate("X_uuid",.after='Household Size')
  df<-df %>% select(1:which(names(df) == 'X_uuid'))
  return(df)
}

#function to get the outerlier 
outlier_finder<-function(my_data){
  start_col_num<-1
  end_col_num<-2
  name_col_num<- which(colnames(my_data) == "Enumerator Name")
  hh_size_col_num<- which(colnames(my_data) == "Household Size")
  time_start<-substring(my_data$start,0,23)
  time_end<-substring(my_data$end,0,23)
  time_start<-ymd_hms(time_start)
  time_end<-ymd_hms(time_end)
  time_diff<-my_data[name_col_num]
  names(time_diff)[1]<-paste('names')
  time_diff$time<-as.numeric(time_end-time_start,unit="secs")
  #print(dput(time_diff))
  #get uuid of outlier
  time_outlier<-which(time_diff$time>900 | time_diff$time<300)
  id_outlier<-my_data[time_outlier,]$X_uuid
  #get uuid for not sameday submission
  same_day<-which(as.Date(time_end)!=as.Date(time_start))
  id_sameday<-my_data[same_day,]$X_uuid
  name_sameday<-my_data[same_day,]$`Enumerator Name`
  #get uuid for enumerator submit not in working hours
  hour_start<-as.numeric(substring(my_data$`Start Time`,0,2))
  hour_end<-as.numeric(substring(my_data$`End Time`,0,2))
  working_hour<-which(hour_start<8 | hour_start>16 | hour_end<8 | hour_start>16)
  id_hour<-my_data[working_hour,]$X_uuid

  
  
  name_sameday<-table(unlist(name_sameday))
  #print(my_data[time_outlier,name_col_num])
  hh_size<-my_data[hh_size_col_num]
  freq_count<-table(unlist(my_data[name_col_num]))
  
  diff_mean_count<-time_diff %>%
    group_by(names) %>%
    summarise(mean_value = mean(time))%>%
    dplyr::mutate_if(is.numeric, format, 2)%>%
    ungroup() %>%
    select(mean_value)
  
  #count each person's warning for long time
  out_names<-data.frame(list(my_data[time_outlier,name_col_num]))
  enu_names<-data.frame(my_data[name_col_num])
  enu_names<-unique(enu_names)
  
  names(enu_names)[1]<-paste("names1")
  names(out_names)[1]<-paste("names1")
  
  out_names<-data.frame(table(out_names))
  enu_names<-(merge(enu_names,out_names,by="names1",all=TRUE))
  

  enu_names$Freq[is.na(enu_names$Freq)]<-0
  enu_names$NotSameDay<-name_sameday[match(enu_names$names1,names(name_sameday))]
  enu_names$NotSameDay[is.na(enu_names$NotSameDay)]<-0
  enu_names$Total<-freq_count
  enu_names$AvgDura<-as.numeric(unlist(diff_mean_count))
  enu_names$AvgDura<-(enu_names$AvgDura %/% 60)
  
  enu_names$Freq<-enu_names$Freq/freq_count
  enu_names$NotSameDay<-enu_names$NotSameDay/freq_count
  
  
  names(enu_names)[2]<-paste("Outlier Duration Detected")
  names(enu_names)[4]<-paste("Total Survey Count")
  names(enu_names)[5]<-paste("Average Survey Duration (min)")
  
  
  return(list(enu_names,id_outlier,id_sameday,id_hour))
}



sub_group<-function(table){
  
  #change the order of variables(columns)
  table<-pre_process(table)
  
  #the label name might change, maybe `1.2 Enumerator name (WFP)`
  name_col<-'`1.2 Enumerator name (WFP)`'
  name_col_num<-which(colnames(table) == "Enumerator Name")
  
  enu_name_list <- split(table, table[name_col_num])
  out_list<<-outlier_finder(table)
  #get enu names
  name_list<-out_list[[1]]
  
  name_list$names1 <- gsub("_", " ", name_list$names1)
  
  #get all duration outliers
  out_id<-out_list[[2]]
  out_id<-paste("'",out_id,"'",sep="")
  #set as global value so it can be read inside JS()
  out_id<-as.list(out_id)
  
  #get the people who start and end in different days
  diff_id<-out_list[[3]]
  diff_id<-paste("'",diff_id,"'",sep="")
  diff_id<-as.list(diff_id)

  #get those submit outside working hour (0800 to 1600)
  hour_id<-out_list[[4]]
  hour_id<-paste("'",hour_id,"'",sep="")
  hour_id<-as.list(hour_id)
  
  #add this as a new column
  colnames(table)[which(names(table) == "Date")] <- "Submit in Same Day"
  table$`Submit in Same Day`<-(as.Date(table$start)==as.Date(table$end))
  

  #bring them into a nested datatable
  Dat<-NestedData(name_list,enu_name_list)
  names(Dat)[2]<-paste("Enumerator Name")
  ## whether to show row names
  rowNames = FALSE
  colIdx <- as.integer(rowNames)
  
  ## the callback
  parentRows <- which(Dat[,1] != "")
  
  callback <- JS(
    sprintf("var parentRows = [%s];", toString(parentRows-1)),
    sprintf("var j0 = %d;", colIdx),
    "var nrows = table.rows().count();",
    "for(let i = 0; i < nrows; ++i){",
    "  var $cell = table.cell(i,j0).nodes().to$();",
    "  if(parentRows.indexOf(i) > -1){",
    "    $cell.css({cursor: 'pointer'});",
    "  }else{",
    "    $cell.removeClass('details-control');",
    "  }",
    "}",
    "",
    "// --- make the table header of the nested table --- //",
    "var formatHeader = function(d, childId){",
    "  if(d !== null){",
    "    var html = ", 
    "      '<table class=\"display hover\" ' + ",
    "      'style=\"padding-left: 30px;\" id=\"' + childId + ", 
    "      '\"><thead><tr>';",
    "    var data = d[d.length-1] || d._details;",
    "    for(let key in data[0]){",
    "      html += '<th>' + key + '</th>';",
    "    }",
    "    html += '</tr></thead></table>'",
    "    return html;",
    "  } else {",
    "    return '';",
    "  }",
    "};",
    "",
    "// --- row callback to style rows of child tables --- //",
    "var rowCallback = function(row, dat, displayNum, index){",
    "   if($(row).hasClass('odd')){",
    "     $(row).css('background-color', 'white');",
    "     $(row).hover(function(){",
    "       $(this).css('background-color', '#DDFF75');",
    "    }, function(){",
    "      $(this).css('background-color', 'white');",
    "    });",
    "   } else {",
    "     $(row).css('background-color', 'white');",
    "     $(row).hover(function(){",
    "      $(this).css('background-color', '#DDFF75');",
    "     }, function(){",
    "      $(this).css('background-color', 'white');",
    "     });",
    "  }",
    "  column='Household Size';",
    "  if (dat[column] > 10) {",
    "     var colIndex =  Object.keys(dat).indexOf(column);",
    "     var probcell = $('td:eq(' + colIndex + ')', row);",
    "     probcell.css('background-color', '#ffe6cc');",
    "     probcell.css('color', 'red');",
    "   }",
    #mark those have abnormal durations
    sprintf("var outer_id = [%s];", toString(out_id)),
    #could also replace number with colIndex to avoid change of data ruin the code
    "  if(outer_id.includes(dat.X_uuid)){",
    "    var duracell = $('td:eq(5)', row);",
    "    duracell.css('background-color', '#ffe6cc');",
    "    duracell.css('color', 'red');",
    "   }",
    #mark those who does not start and end in the same day
    sprintf("var diff_id = [%s];", toString(diff_id)),
    "  if(diff_id.includes(dat.X_uuid)){",
    "    var diffcell = $('td:eq(4)', row);",
    "    diffcell.css('background-color', '#ffe6cc');",
    "    diffcell.css('color', 'red');",
    "   }",
    #mark thouse submit survey outside of working hours
    sprintf("var hour_id = [%s];", toString(hour_id)),
    "  if(hour_id.includes(dat.X_uuid)){",
    "    var hourcell = $('td:eq(3)', row);",
    "    hourcell.css('background-color', '#ffe6cc');",
    "    hourcell.css('color', 'red');",
    "   }",
    "$('td:eq(0)', row).text(function(index, text) {",
     " return text.substring(0, 10); // Adjust the substring length to make it only have date",
    "});",
    "$('td:eq(1)', row).text(function(index, text) {",
    " return text.substring(0, 10); // Adjust the substring length to make it only have date",
    "});",
    "};",
    "",
    "// --- header callback to style header of child tables --- //",
    "var headerCallback = function(thead, data, start, end, display){",
    "  $('th', thead).css({",
    "    'border-top': '3px solid indigo',", 
    "    'color': 'white',",
    "    'background-color': '#00802b',",
    #"    'font-size': '5px',",
    "  });",
    "};",
    "",
    "// --- make the datatable --- //",
    "var formatDatatable = function(d, childId){",
    "  var data = d[d.length-1] || d._details;",
    "  var colNames = Object.keys(data[0]);",
    "  var columns = colNames.map(function(x){",
    "    return {data: x.replace(/\\./g, '\\\\\\.'), title: x};",
    "  });",
    "  var id = 'table#' + childId;",
    "  if(colNames.indexOf('_details') === -1){",
    "    var subtable = $(id).DataTable({",
    "      'data': data,",
    "      'columns': columns,",
    "      'autoWidth': false,",
    "      'deferRender': true,",
    "      'info': false,",
    "      'lengthChange': false,",
    "      'ordering': data.length > 1,",
    "      'order': [],",
    "      'paging': false,",
    "      'scrollX': true,",
    "      'scrollY': 600,",
    "      'searching': false,",
    "      'sortClasses': false,",
    "      'fixedHeader': false,",
    "      'fixedColumns': true,",
    "      'rowCallback': rowCallback,",
    "      'headerCallback': headerCallback,",
    "      'columnDefs': [{targets: '_all', className: 'dt-center'},{targets: '_all', className: 'child_dt'}]",
    "    });",
    "  } else {",
    "    var subtable = $(id).DataTable({",
    "      'data': data,",
    "      'columns': columns,",
    "      'autoWidth': false,",
    "      'deferRender': true,",
    "      'info': false,",
    "      'lengthChange': false,",
    "      'ordering': data.length > 1,",
    "      'order': [],",
    "      'paging': false,",
    "      'scrollX': true,",
    "      'scrollY': 600,",
    "      'searching': false,",
    "      'sortClasses': false,",
    "      'fixedHeader':  false,",
    "      'fixedColumns': true,",
    "      'rowCallback': rowCallback,",
    "      'headerCallback': headerCallback,",
    "      'columnDefs': [", 
    "        {targets: -1, visible: false},", 
    "        {targets: 0, orderable: false, className: 'details-control'},", 
    "        {targets: '_all', className: 'dt-center'},",
    "        {targets: '_all', className: 'child_dt'}",
    "      ]",
    "    }).column(0).nodes().to$().css({cursor: 'pointer'});",
    "  }",
    "};",
    "",
    "// --- display the child table on click --- //",
    "// array to store id's of already created child tables",
    "var children = [];", 
    "table.on('click', 'td.details-control', function(){",
    "  var tbl = $(this).closest('table'),",
    "      tblId = tbl.attr('id'),",
    "      td = $(this),",
    "      row = $(tbl).DataTable().row(td.closest('tr')),",
    "      rowIdx = row.index();",
    "  if(row.child.isShown()){",
    "    row.child.hide();",
    "    td.html('&oplus;');",
    "  } else {",
    "    var childId = tblId + '-child-' + rowIdx;",
    "    if(children.indexOf(childId) === -1){", 
    "      // this child has not been created yet",
    "      children.push(childId);",
    "      row.child(formatHeader(row.data(), childId)).show();",
    "      td.html('&CircleMinus;');",
    "      formatDatatable(row.data(), childId, rowIdx);",
    "    }else{",
    "      // this child has already been created",
    "      row.child(true);",
    "      td.html('&CircleMinus;');",
    "    }",
    "  }",
    "});")
  
  #Dat[,2]<-c(name_list)
  #Dat<-subset(Dat,select=-c(`_details`))
  
  parent_dt<-datatable(
    Dat, 
    callback = callback, rownames = rowNames, escape = -colIdx-1,
    extensions = c("Buttons","FixedHeader"),
    selection='none',
    options = list(
      paging = FALSE,
      searching = TRUE,
      dom = "Bfrtip",
      #fixedHeader = TRUE,
      columnDefs = list(
        list(
          visible = FALSE, 
          targets = ncol(Dat)-1+colIdx
        ),
        list(
          orderable = FALSE, 
          className = "details-control", 
          targets = colIdx
        ),
        list(
          className = "dt-center", 
          targets = "_all"
        )
      ),
      buttons = list(
        list(
          extend = "excel",
          exportOptions = list(
            orthogonal = "export", 
            columns = 0:(ncol(Dat)-2)
          ),
          title = "exported_BCM_Data",
          orientation = "landscape"
        )
      )
    ),
    class='parent_dt display hover'
  )
  
  
  #format the warning color of outliers
  brks <- seq(0,1,by=0.01)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
  
  parent_dt<-parent_dt %>% formatStyle('Outlier Duration Detected', backgroundColor = styleInterval(brks, clrs))

  parent_dt<-parent_dt %>% formatStyle('NotSameDay', backgroundColor = styleInterval(brks, clrs))
  

  return(parent_dt%>%formatPercentage(c("Outlier Duration Detected","NotSameDay"), 2))
  
  
  
}