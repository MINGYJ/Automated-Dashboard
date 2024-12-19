
graph_render<-function(shop,df){
  graph_list<-reactive({
    # Define the possible colors
    possible_colors <- c("red", "blue", "grey")
    
      time_df<- time_bar(shop,df)
      queue_df<-queue_pie(shop,df)
      type_df<-type_pie(shop,df)
      problem_df<-problem_pie(shop,df)
      #colors <- ifelse(queue_df$label == "No Queue", "white")
      time_bar<-function(){
        barplot(
          time_df,
          col = "steelblue",
          main = paste("Unavailable Time Slot of BreadShop: ",shop,sep=""),
          xlab = "Time Period",
          ylab = "Count"
        )
      }
      queue_pie<-function(){
        pie(
          queue_df$value,
          labels=queue_df$label,
          main =paste("Reason of Long Queue at BreadShop: ",shop,sep="")
        )
      }
      type_pie<-function(){
        pie(
          type_df$value,
          labels=type_df$label,
          main =paste("Unavailable Type of Bread at BreadShop: ",shop,sep="")
        )
      }
      problem_pie<-function(){
        pie(
          problem_df$value,
          labels=problem_df$label,
          main =paste("Problems of Bread at BreadShop: ",shop,sep="")
        )
      }
      
      list(time_bar=time_bar,queue_pie=queue_pie,type_pie=type_pie,problem_pie=problem_pie)
  })
  return(graph_list)
}

find_colnames <- function(c1, c2, data) data %>% select(c1:c2) %>% colnames()

time_bar<-function(shop,df){
  df1<-df[df$`Bread shop`==shop,]
  df1<-subset(df1,select=`7am to 11am`:`2pm to 5pm`)
  # Count the number of occurrences in each column
  check_equal<-"True"
  df1<- sapply(df1, function(col) sum(col == check_equal))
  return(df1)
}

queue_pie<-function(shop,df){
  df2<-df[df$`Bread shop`==shop,]
  total_count<-nrow(df2)
  df2<-subset(df2,select=`No Bread Available`:`Overcrowded`)
  # Count the number of occurrences in each column
  check_equal<-"True"
  df2<- sapply(df2, function(col) sum(col == check_equal))
  if (all(df2 == 0)) {
    # Step 3: Create a new data frame with one column named "No Queue" and a value of 1
    df2 <- c("No Queue" = total_count)
  }
  df2<-df2[df2!=0]
  value<-as.numeric(df2)
  label<-paste(names(df2)," With ",value," Reports",sep="")
  return(list(value=value,label=label))
}

type_pie<-function(shop,df){
  df2<-df[df$`Bread shop`==shop,]
  total_count<-nrow(df2)
  df2<-subset(df2,select=`Small Loaves`:`No Bread`)
  # Count the number of occurrences in each column
  check_equal<-"True"
  df2<- sapply(df2, function(col) sum(col == check_equal))
  if (all(df2 == 0)) {
    # Step 3: Create a new data frame with one column named "No Queue" and a value of 1
    df2 <- c("All Bread Available" = total_count)
  }
  df2<-df2[df2!=0]
  value<-as.numeric(df2)
  label<-paste(names(df2)," With ",value," Reports",sep="")
  return(list(value=value,label=label))
}

problem_pie<-function(shop,df){
  df2<-df[df$`Bread shop`==shop,]
  total_count<-nrow(df2)
  df2<-subset(df2,select=`Bread Dry`:`Bread Burnt`)
  # Count the number of occurrences in each column
  check_equal<-"True"
  df2<- sapply(df2, function(col) sum(col == check_equal))
  if (all(df2 == 0)) {
    # Step 3: Create a new data frame with one column named "No Queue" and a value of 1
    df2 <- c("No Problems" = total_count)
  }
  df2<-df2[df2!=0]
  value<-as.numeric(df2)
  label<-paste(names(df2)," With ",value," Reports",sep="")
  return(list(value=value,label=label))
}