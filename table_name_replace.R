library(dplyr)
library(tidyr)

replace_col<-function(df){
  colnames(df)[which(names(df) == "X1.1.Date")] <- "Date"
  colnames(df)[which(names(df) == "X1.2.Enumerator.name..WFP.")] <- "Enumerator Name"
  colnames(df)[which(names(df) == "X1.2.a.If.other..please.specify")] <- "If Other Enumerator"
  colnames(df)[which(names(df) == "X1.3.a.Do.you.agree.to.participate.")] <- "Agree to participant"
  
  colnames(df)[which(names(df) == "X2.1.Please.select.the.camp.s.name")] <- "Camps Name"
  colnames(df)[which(names(df) == "X2.2.Bread.selling.point.")] <- "Bread Shop in Zaatari"
  colnames(df)[which(names(df) == "X2.2.a.Bread.selling.point.")] <- "Bread Shop in Azarq"
  colnames(df)[which(names(df) == "X2.3.What.is.the.UNHCR.Case.ID")] <- "UNHCR Case ID"
  colnames(df)[which(names(df) == "X2.4.What.is.the.sex.of.the.respondent.")] <- "Respondent Sex"
  colnames(df)[which(names(df) == "X2.5.What.is.the.household.size.of.the.respondent.")] <- "Household Size"
  
  colnames(df)[which(names(df) == "X3.1.What.is.the.modality.used.for.purchasing.the.bread.")] <- "Modality"
  colnames(df)[which(names(df) == "X3.2.How.much.did.you.spent.last.month.Iris.")] <- "Amount Spent with Iris last Month"
  colnames(df)[which(names(df) == "X3.3.How.much.did.you.spent.last.month.Cash.")] <- "Amount Spent with Cash last Month"
  
  colnames(df)[which(names(df) == "X3.4.How.long.does.it.take.you.to.queue.in.line.at.the.bread.selling.point...Approximately.in.minutes.")] <- "How long does it take you to queue in line at the bread selling point(minutes)"
  
  colnames(df)[which(names(df) == "Do.you.always.find.bread.available.when.you.go.to.the.bread.selling.point.")] <- "Is bread always available?"
  
  colnames(df)[which(names(df) == "BPB08.1...no.bread.available.at.the.moment.")] <- "No Bread Available"
  colnames(df)[which(names(df) == "BPB08.2..shortage.of.customer.service.staff")] <- "Shortage of Staff"
  colnames(df)[which(names(df) == "BPB08.3.Overcrowded")] <- "Overcrowded"
  
  colnames(df)[which(names(df) == "BAI02.1.7am...11.am")] <- "7am to 11am"
  colnames(df)[which(names(df) == "BAI02.2.11am...2.pm")] <- "11am to 2pm"
  colnames(df)[which(names(df) == "BAI02.3.2pm...5.pm")] <- "2pm to 5pm"
  
  colnames(df)[which(names(df) == "BAI03.1.Small.loaves")] <- "Small Loaves"
  colnames(df)[which(names(df) == "BAI03.2.Big.loaves")] <- "Big Loaves"
  colnames(df)[which(names(df) == "BAI03.3.Brown.bread")] <- "Brown Loaves"
  colnames(df)[which(names(df) == "BAI03.4.Syrian.bread")] <- "Syrian Bread"
  colnames(df)[which(names(df) == "BAI03.5.Restaurant.Bread")] <- "Restaurant Bread"
  colnames(df)[which(names(df) == "BAI03.6..No.bread.at.all")] <- "No Bread"
  
  colnames(df)[which(names(df) == "X5.1.During.the.last.week.have.you.experienced.any.issues.with.the.quality.of.bread.")] <- "During last week did you experienced any issues with the quality of bread?"
  
  colnames(df)[which(names(df) == "BQI03.1.Dry")] <- "Bread Dry"
  colnames(df)[which(names(df) == "BQI03.2.Unbaked")] <- "Unbaked"
  colnames(df)[which(names(df) == "BQI03.3.Bad.odor")] <- "Bad Odor"
  colnames(df)[which(names(df) == "BQI03.4.Burnt")] <- "Bread Burnt"
  
  colnames(df)[which(names(df) == "X5.2.During.the.past.week..did.you.have.to.buy.less.or.more.bread.than.you.need.due.to.the.seller.s.packaging.")] <- "During the past week, did you have to buy less or more bread than need due to sellers packaging?"
  
  colnames(df)[which(names(df) == "X6.1.Do.you.see.staff.at.the.bread.selling.point.wearing.a.hairnet.and.gloves.")] <- "Do you see staff at bread selling point wearing a harinet and gloves?"
  colnames(df)[which(names(df) == "X6.2.Do.you.think.that.people.with.special.needs..elderly..pregnant.women..people.with.disabilities..etc...are.properly.prioritized.in.the.bread.selling.points..waiting.in.line..help.to.carry.their.items..")] <- "Did you think people with special needs are properly prioritized in the bread selling point to help carry their items?"
  colnames(df)[which(names(df) == "X6.3.Have.you.or.any.of.your.household.members.experienced.any.security.challenge.related.to.WFP.assistance.")] <- "Have you or your household memebers experienced security challenge related to WFP assistant?"
  
  colnames(df)[which(names(df) == "PI04.1..Going.to.WFP.programme.sites")] <- "Going to WFP Site"
  colnames(df)[which(names(df) == "PI04.2.Coming.from.WFP.programme.sites")] <- "Coming from WFP Site"
  colnames(df)[which(names(df) == "PI04.3..At.WFP.programme.sites")] <- "AT WFP Site"
  colnames(df)[which(names(df) == "PI04.4..Elsewhere.but.related.to.WFP.programmes.assistance")] <- "Elsewhere but related to WFP Assistance"
  
  colnames(df)[which(names(df) == "PI05.1.Physical.violence..harassment.or.threats")] <- "Physical violence harassment or threats"
  colnames(df)[which(names(df) == "PI05.2..Assault.in.connection.with.theft.of.assistance")] <- "Assault in connection with theft of assistance"
  colnames(df)[which(names(df) == "PI05.3..Injuries.or.casualties.at.programme.sites")] <- "Injuries or casualties at programme sites"
  colnames(df)[which(names(df) == "PI05.4..Obstruction.or.restriction.of.access.to.assistance.groups")] <- "Obstruction or restriction of access to assistance groups"
  colnames(df)[which(names(df) == "PI05.5..Lack.of.crowd.control.measures")] <- "Lack of crowd control measures"
  colnames(df)[which(names(df) == "PI05.999...Other..specify.")] <- "Other problems"
  
  colnames(df)[which(names(df) == "X6.3.b.Please.explain")] <- "Explain the problem"
  colnames(df)[which(names(df) == "X6.3.c.Have.WFP.and.or.its.partners.already.taken.measures.to.make.it.safer.or.easier.for.you.or.other.members.of.your.household.to.access.WFP.programme.sites")] <- "Have WFP and its parteners make it safer to access WFP sites?"
  colnames(df)[which(names(df) == "X6.4.How.would.you.rate.the.level.of.safety.you.experienced.when.traveling.to.and.from.bread.selling.points.")] <- "Rate of safety when travel to and from breadshop"
  colnames(df)[which(names(df) == "X6.5.Do.you.think.WFP.and.or.partner.staff.have.treated.you.and.members.of.your.household.respectfully.")] <- "Have WFP or partner staff treat you and your HH member respectfully?"
  
  colnames(df)[which(names(df) == "PI10.1..Treatment.by.WFP.CP.personnel")] <- "WFP CP Personnel"
  colnames(df)[which(names(df) == "PI10.2..Treatment.by.shop.owners")] <- "Shop Owner"
  colnames(df)[which(names(df) == "PI10.999..Other..specify.")] <- "Other Staff"
  
  colnames(df)[which(names(df) == "X6.5.b.Please.explain")] <- "Explain"
  
  colnames(df)[which(names(df) == "X6.6.Who.in.your.household.decides.what.to.do.with.the.cash.voucher.given.by.WFP..such.as.when..where.and.what.to.buy.")] <- "Who in your HH decide spent of WFP assistance"
  colnames(df)[which(names(df) == "X6.7.In.general..who.in.your.household.makes.decisions.over.the.household.resources.that.are.not.related.to.WFP.entitlement..is.it.women..men.or.both.")] <- "Who in your HH decide spent of non-WFP money?"
  
  colnames(df)[which(names(df) == "X7.1.Are.you.satisfied.with.the.service.provided.at.the.bread.selling.point.")] <- "Are you satisfied with service provided at bread selling point?"
  colnames(df)[which(names(df) == "X7.1.a.Please.explain..if.not.or.are.somehow.satisfied")] <- "Please explain if not satisfied"
  colnames(df)[which(names(df) == "X7.2.Do.you.have.suggestions.or.recommendations.for.WFP.on.how.to.improve.the.bread.selling.points.")] <- "Suggestion or recommendations?"
  colnames(df)[which(names(df) == "X7.2.a.If.yes..please.explain")] <- "Suggestion or recommendations"
  colnames(df)[which(names(df) == "X7.3.Do.you.have.any.additional.comments.")] <- "Additional comments?"
  colnames(df)[which(names(df) == "X7.3.a.Please.explain.or.provide.any.additional.information")] <- "Additional comments"
  colnames(df)[which(names(df) == "X7.4.GPS.Coordinates")] <- "GPS latitude, longitude, altitude and precision"
  
  colnames(df)[which(names(df) == "instanceID")] <- "instance ID"
  colnames(df)[which(names(df) == "X_duration")] <- "Duration"
  #colnames(df)[which(names(df) == "  ")] <- "  "
  #colnames(df)[which(names(df) == "  ")] <- "  "
  
  #combine some columns
  df<-combine_col_BAI02(df)
  df<-combine_col_BAI03(df)
  df<-combine_col_BQI03(df)
  df<-combine_col_BPB08(df)
  df<-combine_col_PI04(df)
  df<-combine_col_PI05(df)
  df<-combine_col_PI10(df)
  
  df<-combine_col_shop(df)
  
  df<-combine_col_GPS(df)
  
  #relocate some
  df<-relocate_col_dur(df)
  
  df<-move_back(df)
  
  df<-format_space(df)
  
  df<-working_hour(df)
  
  df<-label_replace(df)
  
  df<-enu_names(df)
  
  return(df)
}


combine_col_shop<-function(df){
  name_col_1<-'2.2 Bread selling point?'
  name_col_2<-'2.2.a Bread selling point?'
  name_col_1_num<-8
  name_col_2_num<-9
  df[['Bread shop']]<-ifelse((df[,name_col_1_num]=='n/a')|(is.na(df[,name_col_1_num])),df[,name_col_2_num],df[,name_col_1_num])
  df<-df %>% relocate("Bread shop",.after="Bread Shop in Zaatari")
  df<-df %>% relocate("Bread Shop in Zaatari",.after=last_col())
  df<-df %>% relocate("Bread Shop in Azarq",.after=last_col())
  return(df)
}

combine_col_BAI02<-function(df){
  df<-df %>% 
         mutate(across("7am to 11am":"2pm to 5pm", ~case_when(. == 'True' ~ cur_column()), .names = 'new_{col}')) %>%
         unite("When is bread unavailable", starts_with('new'), na.rm = TRUE, sep = ',')
  df<-df %>% relocate("When is bread unavailable",.after="7am to 11am")
  df<-df %>% relocate("7am to 11am",.after=last_col())
  df<-df %>% relocate("11am to 2pm",.after=last_col())
  df<-df %>% relocate("2pm to 5pm",.after=last_col())
  return(df)
}

combine_col_BAI03<-function(df){
  df<-df %>% 
    mutate(across("Small Loaves":"No Bread", ~case_when(. == 'True' ~ cur_column()), .names = 'new_{col}')) %>%
    unite("What kind of bread is unavailable", starts_with('new'), na.rm = TRUE, sep = ',')
  df<-df %>% relocate("What kind of bread is unavailable",.after="Small Loaves")
  df <- df %>%
    relocate("Small Loaves", .after = last_col()) %>%
    relocate("Big Loaves", .after = last_col()) %>%
    relocate("Brown Loaves", .after = last_col()) %>%
    relocate("Syrian Bread", .after = last_col()) %>%
    relocate("Restaurant Bread", .after = last_col()) %>%
    relocate("No Bread", .after = last_col())
  return(df)
}

combine_col_BQI03<-function(df){
  df<-df %>% 
    mutate(across("Bread Dry":"Bread Burnt", ~case_when(. == 'True' ~ cur_column()), .names = 'new_{col}')) %>%
    unite("Problem of Bread", starts_with('new'), na.rm = TRUE, sep = ',')
  df<-df %>% relocate("Problem of Bread",.after="Bread Dry")
  df<-df %>% relocate("Bread Dry",.after=last_col())
  df<-df %>% relocate("Bad Odor",.after=last_col())
  df<-df %>% relocate("Bread Burnt",.after=last_col())
  df<-df %>% relocate("Unbaked",.after=last_col())
  return(df)
}

combine_col_BPB08<-function(df){
  df<-df %>% 
    mutate(across("No Bread Available":"Overcrowded", ~case_when(. == 'True' ~ cur_column()), .names = 'new_{col}')) %>%
    unite("Why wait at bread shop", starts_with('new'), na.rm = TRUE, sep = ',')
  df<-df %>% relocate("Why wait at bread shop",.after="No Bread Available")
  df <- df %>%
    relocate("No Bread Available", .after = last_col()) %>%
    relocate("Shortage of Staff", .after = last_col()) %>%
    relocate("Overcrowded", .after = last_col())
  return(df)
}

combine_col_PI04<-function(df){
  df<-df %>% 
    mutate(across("Going to WFP Site":"Elsewhere but related to WFP Assistance", ~case_when(. == 'True' ~ cur_column()), .names = 'new_{col}')) %>%
    unite("Where have you and your HH member face security challenges?", starts_with('new'), na.rm = TRUE, sep = ',')
  df<-df %>% relocate("Where have you and your HH member face security challenges?",.after="Going to WFP Site")
  df <- df %>%
    relocate("Going to WFP Site", .after = last_col()) %>%
    relocate("Coming from WFP Site", .after = last_col()) %>%
    relocate("AT WFP Site", .after = last_col()) %>%
    relocate("Elsewhere but related to WFP Assistance", .after = last_col())
  return(df)
}

combine_col_PI05<-function(df){
  df<-df %>% 
    mutate(across("Physical violence harassment or threats":"Other problems", ~case_when(. == 'True' ~ cur_column()), .names = 'new_{col}')) %>%
    unite("What kind of security challenges?", starts_with('new'), na.rm = TRUE, sep = ',')
  df<-df %>% relocate("What kind of security challenges?",.after="Physical violence harassment or threats")
  df <- df %>%
    relocate("Physical violence harassment or threats", .after = last_col()) %>%
    relocate("Assault in connection with theft of assistance", .after = last_col()) %>%
    relocate("Injuries or casualties at programme sites", .after = last_col()) %>%
    relocate("Obstruction or restriction of access to assistance groups", .after = last_col()) %>%
    relocate("Lack of crowd control measures", .after = last_col()) %>%
    relocate("Other problems", .after = last_col())
  return(df)
}

combine_col_PI10<-function(df){
  df<-df %>% 
    mutate(across("WFP CP Personnel":"Other Staff", ~case_when(. == 'True' ~ cur_column()), .names = 'new_{col}')) %>%
    unite("Who treat you disrespectfully?", starts_with('new'), na.rm = TRUE, sep = ',')
  df<-df %>% relocate("Who treat you disrespectfully?",.after="WFP CP Personnel")
  df <- df %>%
    relocate("WFP CP Personnel", .after = last_col()) %>%
    relocate("Shop Owner", .after = last_col()) %>%
    relocate("Other Staff", .after = last_col()) 
  return(df)
}

combine_col_GPS<-function(df){
  df$"Google Map Location"<-paste0('<a href="',
                                   "https://maps.google.com?q=",df$"general.Closing_Information._gps_latitude",",",df$"general.Closing_Information._gps_longitude",
                                   '" target="_blank">', "<b>Click for Location</b>" ,'</a>',sep="")
  df<-df %>% relocate("Google Map Location",.after="general.Closing_Information._gps_latitude")
  df <- df %>%
    relocate("general.Closing_Information._gps_latitude", .after = last_col()) %>%
    relocate("general.Closing_Information._gps_longitude", .after = last_col())%>%
    relocate("general.Closing_Information._gps_altitude", .after = last_col())
  return(df)
}



relocate_col_dur<-function(df){
  df$"Duration"<-as.numeric(df$"Duration")
  df$"Duration"<-paste(df$"Duration"%/%60,"min",df$"Duration"%%60,"sec",sep="")
  df<-df %>% relocate("Duration",.after="Date")
  
  #for check if start and end in the same day
  colnames(df)[which(names(df) == "Date")] <- "Submit in Same Day"
  df$`Submit in Same Day`<-(as.Date(df$start)==as.Date(df$end))
  return(df)
}



move_back<-function(df){
  df<-df %>% relocate("Is bread always available?",.after=last_col())
  df<-df %>% relocate("During last week did you experienced any issues with the quality of bread?",.after=last_col())
  df<-df %>% relocate("Suggestion or recommendations?",.after=last_col())
  df<-df %>% relocate("Additional comments?",.after=last_col())
  return(df)
}

format_space<-function(df){
  df <- df %>%
    mutate(`Enumerator Name` = gsub("_", " ", `Enumerator Name`))
  df <- df %>%
    mutate(`Bread shop` = gsub("_", "-", `Bread shop`))
  return(df)
}

label_replace<-function(df){
  hhh_gender<-c('1'="Men",'2'="Women",'3'="Both")
  df <- df %>%
    mutate(`Who in your HH decide spent of WFP assistance` = as.character(`Who in your HH decide spent of WFP assistance`)) %>%
    mutate(`Who in your HH decide spent of WFP assistance` = hhh_gender[`Who in your HH decide spent of WFP assistance`])
  
  df <- df %>%
    mutate(`Who in your HH decide spent of non-WFP money?` = as.character(`Who in your HH decide spent of non-WFP money?`)) %>%
    mutate(`Who in your HH decide spent of non-WFP money?` = hhh_gender[`Who in your HH decide spent of non-WFP money?`])
  
  choice<-c('0'='No','1'='Yes','2'="Don't know")
  df <- df %>%
    mutate(`Do you see staff at bread selling point wearing a harinet and gloves?` = as.character(`Do you see staff at bread selling point wearing a harinet and gloves?`)) %>%
    mutate(`Do you see staff at bread selling point wearing a harinet and gloves?` = choice[`Do you see staff at bread selling point wearing a harinet and gloves?`])
  
  df <- df %>%
    mutate(`Did you think people with special needs are properly prioritized in the bread selling point to help carry their items?` = as.character(`Did you think people with special needs are properly prioritized in the bread selling point to help carry their items?`)) %>%
    mutate(`Did you think people with special needs are properly prioritized in the bread selling point to help carry their items?` = choice[`Did you think people with special needs are properly prioritized in the bread selling point to help carry their items?`])
  
  df <- df %>%
    mutate(`During the past week, did you have to buy less or more bread than need due to sellers packaging?` = as.character(`During the past week, did you have to buy less or more bread than need due to sellers packaging?`)) %>%
    mutate(`During the past week, did you have to buy less or more bread than need due to sellers packaging?` = choice[`During the past week, did you have to buy less or more bread than need due to sellers packaging?`])
  
  df <- df %>%
    mutate(`Have you or your household memebers experienced security challenge related to WFP assistant?` = as.character(`Have you or your household memebers experienced security challenge related to WFP assistant?`)) %>%
    mutate(`Have you or your household memebers experienced security challenge related to WFP assistant?` = choice[`Have you or your household memebers experienced security challenge related to WFP assistant?`])
  
  df <- df %>%
    mutate(`Have WFP or partner staff treat you and your HH member respectfully?` = as.character(`Have WFP or partner staff treat you and your HH member respectfully?`)) %>%
    mutate(`Have WFP or partner staff treat you and your HH member respectfully?` = choice[`Have WFP or partner staff treat you and your HH member respectfully?`])
  
  df <- df %>%
    mutate(`Agree to participant` = as.character(`Agree to participant`)) %>%
    mutate(`Agree to participant` = choice[`Agree to participant`])
  
  
  return(df)
}

working_hour<-function(df){
  df$'Start Time'<-paste(substr(df$start,12,19))
  df$'End Time'<-paste(substr(df$end,12,19))
  df<-df %>% relocate("End Time",.after=end)
  df<-df %>% relocate("Start Time",.after=end)
}


#change numerator name to add upper case
transform_upper<- function(text) {
  
  # Capitalize the first letter of each part after the first one
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

enu_names<-function(df){
  df<-df%>%mutate(`Enumerator Name` = (sapply(`Enumerator Name`, transform_column)))
  return(df)
  
}


