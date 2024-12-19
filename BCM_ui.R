# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(shiny)
library(shinyalert)
library(bslib)

ui<-fluidPage(
  
    #apply style to the whole page
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Open+Sans');
      *{
          font-family: 'Open Sans', sans-serif;
      }
      .dataTables_wrapper .dataTables_scrollHeadInner, 
      .dataTables_wrapper .dataTables_scrollBody {
        width: 100% !important;
      }
      .parent_dt {
        table-layout: fixed;
        width: 100% !important;
      }
      table.dataTable th, table.dataTable td {
      }
      #mainPanelDT{
        transition: width 0.5s;
      }
      .full-width {
        width: 100% !important;
      }

    ")),
    
    #apply hide/show for visualization plots
    tags$script(HTML("
      $(document).ready(function() {
      $('#sidebarPlots').toggle();
        $('#hideplot').click(function() {
          $('#sidebarPlots').toggle();
          $('#mainPanelDT').toggleClass('full-width');
        });
      });
    ")),
  
    #api login page
    
    fluidRow(
      id='api_login',
      style="width:110%;
      background-color:#007DBC;border-color:#007DBC;
      ",
      column(4,
             tags$div(
               style="color:#FFFFFF;",
             textInput("api","API Token", value = ""),
             actionButton("enter_api", "Enter Dataset"),
             dateInput("date", "Dataset Start Date:",value = Sys.Date()-30),
             dateInput("end_date", "Dataset End Date:",value = Sys.Date()),
             ),
      ),
      column(5,
             tags$div(
               style = "text-align: center; font-size: 12px;color:#FFFFFF;",
               titlePanel("WFP Jordan"),
               titlePanel("Beneficiary Contact Monitoring (BCM) - Bread Shops"),
               titlePanel("Tracking & Data Assurance Dashboard"),
               titlePanel("RAM Unit")
             ),
      )
    ),
    navset_tab(
      nav_panel(
        id='table_page_enu',
        titlePanel(tags$h5("Group by Enumerator")),
        #refresh button
        column(3,actionButton("refresh", "Refresh")),
        # Create a new row for the table.
        DT::dataTableOutput("table"),
        style="width:100%;",
      ),
      nav_panel(
        id='table_page_bread',
        titlePanel(tags$h5("Group by Breadshop")),
        sidebarLayout(
          sidebarPanel(id = "sidebarPlots",
            selectInput("breadshop", "BreadShop:", 
                        choices=colnames("bread_table")),
            plotOutput("queue_pie",height = "300px"),
            plotOutput("time_bar",height = "300px"),
            plotOutput("type_pie",height = "300px"),
            plotOutput("problem_pie",height = "300px")
          ),
          mainPanel(id = "mainPanelDT",
                    class="full-width",
            #refresh button and hide plot button
            column(1,actionButton("refresh", "Refresh")),
            #button to hide/show plots
            column(1,actionButton("hideplot", "Hide/Show Plot")),
            # Create a new row for the table.
            column(5,tags$h4("All problems of breadshops reported are showing the most frequent problems.")),
            DT::dataTableOutput("bread_table"),
            
          )
        )
      ),
    )
  )

