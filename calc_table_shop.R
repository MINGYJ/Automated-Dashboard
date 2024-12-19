
library(DT)
library(dplyr)
library(purrr)

source("graph_render.R")


#change breadshop name, remove dash, add space and uppercase
transform_column <- function(text) {
  # Replace "-" with space
  text <- gsub("-", " ", text)
  
  # Add a space before each digit
  text <- gsub("([0-9])", " \\1", text)
  
  # Split the text by spaces
  parts <- unlist(strsplit(text, " "))
  
  # Capitalize the first letter of each part after the space
  parts <- sapply(parts, function(x) {
    if (nchar(x) > 0 && grepl("^[a-z]", x)) {
      x <- paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
    }
    return(x)
  })
  
  # Recombine the parts into a single string
  text <- paste(parts, collapse = " ")
  
  return(text)
}

#nest the data, dat is the parent row, children is the data under parent row
bread_NestedData <- function(dat, children){

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

#get the most frequently reported problem
Modes_problem <- function(x) {
  if (length(x) == 0) return(c(""))  # Handle empty lists
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

#add new lines for a better layout
newline_vectors <-function(df) {
  df<-(df%>%
         mutate(list=lapply(list,Modes_problem)))
  df<-df %>%
    mutate(`list` = map(`list`, ~ {
      #unique_items <- unique(.x)
      modified_items <- paste(.x, collapse = ",\n")
      modified_items
    }))
  return(df)
}

#group by bread shops
bread_problem<-function(table){

    shop_list<-data.frame(table %>% group_by(`Bread shop`) 
                          %>% summarise(sum((`Why wait at bread shop`)!="")))
    names(shop_list)[1]<-paste("BreadShops")
    names(shop_list)[2]<-paste("% bene. reporting long queue")
    #shop_list$'Reasons of long queue reports'<-(table %>%
    #  group_by(`Bread shop`) %>%
    #  summarise(list=list(unique(unlist(strsplit(`Why wait at bread shop`, ","))))))[2]
    #shop_list$'Reasons of long queue reports'<-newline_vectors(shop_list$'Reasons of long queue reports')
    
    shop_list<-shop_list%>%mutate(BreadShops = (sapply(BreadShops, transform_column)))
    shop_list$'% bene. reporting unavailable bread'<-(table %>% group_by(`Bread shop`) 
                                                     %>% summarise(sum(`When is bread unavailable`!="")))[2]
    #timeslot                                                                                                                                                   ]
    shop_list$'Time when bread was unavailable'<-(table %>%
      group_by(`Bread shop`) %>%
      summarise(list=list(unlist(strsplit(`When is bread unavailable`, ","))))
      )[2]
    shop_list$'Time when bread was unavailable'<-newline_vectors(shop_list$'Time when bread was unavailable')
    
    
    #bread type
    shop_list$'Type of Unavailable Bread'<-(table %>%
      group_by(`Bread shop`) %>%
      summarise(list=list((unlist(strsplit(`What kind of bread is unavailable`, ","))))))[2]
    shop_list$'Type of Unavailable Bread'<-newline_vectors(shop_list$'Type of Unavailable Bread')
    
    
    shop_list$'% bene. reporting quality problems'<-(table %>% group_by(`Bread shop`) 
                                                   %>% summarise(sum((`Problem of Bread`)!="")))[2]
    
    shop_list$'Most commom quality problems reported'<-(table %>%
      group_by(`Bread shop`) %>%
      summarise(list=list((unlist(strsplit(`Problem of Bread`, ","))))))[2]
    shop_list$'Most commom quality problems reported'<-newline_vectors(shop_list$'Most commom quality problems reported')
    
    shop_list$'% bene. observing hygiene problems'<-(table %>% group_by(`Bread shop`) 
                                                 %>% summarise(sum((`Do you see staff at bread selling point wearing a harinet and gloves?`)=="No")))[2]
    
    shop_list$'Did not Prioritized People with Special Needs'<-(table %>% group_by(`Bread shop`) 
                                                 %>% summarise(sum((`Did you think people with special needs are properly prioritized in the bread selling point to help carry their items?`)=="No")))[2]
    
    shop_list$'% bene. facing security challenges'<-(table %>% group_by(`Bread shop`) 
                                                                     %>% summarise(sum((`Have you or your household memebers experienced security challenge related to WFP assistant?`)=="Yes")))[2]
    
    shop_list$'Enumerators'<-(table %>%
                            group_by(`Bread shop`) %>%
                            summarise(list=list(unique(`Enumerator Name`))))[2]
    
    shop_list$'Enumerators'<-newline_vectors(shop_list$'Enumerators')

    shop_list$'Total Survey Count'<-(table %>%
                                       group_by(`Bread shop`) %>%
                                       summarise(n()))[2]
    
    shop_list$'% bene. reporting unavailable bread'<-shop_list$'% bene. reporting unavailable bread'/shop_list$'Total Survey Count'
    shop_list$'% bene. reporting quality problems'<-shop_list$'% bene. reporting quality problems'/shop_list$'Total Survey Count'
    shop_list$"% bene. reporting long queue"<-shop_list$"% bene. reporting long queue"/shop_list$'Total Survey Count'
    shop_list$'% bene. observing hygiene problems'<-shop_list$'% bene. observing hygiene problems'/shop_list$'Total Survey Count'
    shop_list$"Did not Prioritized People with Special Needs"<-shop_list$"Did not Prioritized People with Special Needs"/shop_list$'Total Survey Count'
    shop_list$'% bene. facing security challenges'<-shop_list$'% bene. facing security challenges'/shop_list$'Total Survey Count'
    
    return(shop_list)
}

#group by each bread shop
bread_sub_group<-function(table){
  #the label name might change, maybe `1.2 Enumerator name (WFP)`
  
  
  #print(table[['Bread shop']])
  shop_name_list <- split(table, table[['Bread shop']])
  name_list<-data.frame(names(shop_name_list))
  names(name_list)[1]<-paste("BreadShops")
  name_list<-name_list%>%mutate(BreadShops = (sapply(BreadShops, transform_column)))

  
  shop_pro_list<-bread_problem(table)
  
  name_list<-merge(name_list,shop_pro_list)
  
  #name_list$name <- gsub("_", " ", name_list$name)
  
  #bring them into a nested datatable
  Dat<-bread_NestedData(name_list,shop_name_list)
  
  
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
    "      '<table class=\"display compact hover\" ' + ",
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
    "       if($(row).hasClass('odd')){",
    "          $(row).css('background-color', 'white');",
    "          $(row).hover(function(){",
    "          $(this).css('background-color', '#DDFF75');",
    "          }, function(){",
    "             $(this).css('background-color', 'white');",
    "         });",
    "        } else {",
    "          $(row).css('background-color', 'white');",
    "          $(row).hover(function(){",
    "          $(this).css('background-color', '#DDFF75');",
    "          }, function(){",
    "              $(this).css('background-color', 'white');",
    "         });",
    "        }",
    "    ",
    "var colnames = [",
    "    'Why wait at bread shop',",
    "    'When is bread unavailable',",
    "    'What kind of bread is unavailable',",
    "    'Problem of Bread',",
    "    'Where have you and your HH member faced security challenges?',",
    "    'What kind of security challenges?',",
    "    'Who treat you disrespectfully?'",
    "];",
    "",
    "colnames.forEach(function(column) {",
    "    if (dat[column] != '') {",
    "        var colIndex =  Object.keys(dat).indexOf(column);",
    "        var probcell = $('td:eq(' + colIndex + ')', row);",
    "        probcell.css('background-color', '#ffe6cc');",
    "        probcell.css('color', 'red');",
    "    }",
    "});",
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
    "      'fixedHeader':  false,",
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
      autoWidth=TRUE,
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
        ),
        list(
          targets = "_all", # Apply to all columns
          width = 'auto'
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
  
  percent_list<-c("Did not Prioritized People with Special Needs",'% bene. reporting long queue','% bene. reporting unavailable bread','% bene. observing hygiene problems','% bene. facing security challenges','% bene. reporting quality problems')

  #color those percentages
  brks <- seq(0,1,by=0.01)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
  
  #format style and color
  for (percent_variable in percent_list){
    parent_dt<-parent_dt %>% formatStyle(percent_variable, backgroundColor = styleInterval(brks, clrs))
    
  }
  
  parent_dt<-parent_dt%>%formatPercentage(percent_list, 2)
  
  return(parent_dt)
  
  
  
}