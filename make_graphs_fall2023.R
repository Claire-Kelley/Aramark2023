library(ggplot2)
#library(magrittr)
library(ggforce)
library(dplyr)
library(tidyr)
library(grid)
library(circlize)
library(ggrepel)
library(gt)
#library(wordcloud)
#library(wordcloud2)
library(htmlwidgets) 
#install.packages("webshot")
#webshot::install_phantomjs()


#setwd("~/Documents/Consulting/Qualtrics/Aramark2022/Fall 2022")
#setwd("/Users/sarahkelley/Documents/Consulting/Kelley&Kelley/Aramark2023")

#colors <- c("#750015","#595959","#ff91a3","#bfbfbf","#c00000")
#colors <- c("#8C0019","#EA0022","#999999")
colors <- c("#FFC72C","#9ACAEB","#999999","black","#EA002A")
colors_stacked <- rev(c("#FFC72C","#9ACAEB","#999999","black","#EA002A"))
two_tone <- c("#EA0022","#ff91a3")
#three_tone <- c("#EA0022","#7f7f7f","black")
three_tone <- colors[c(5,3,4)]
three_tone_dot <- c("#EA0022","#7f7f7f","black")
#four_tone <- c("#EA0022","#7f7f7f","black", "#ff91a3")
four_tone <- colors[c(5,3,2,1)]
tab_col <- c("#faccd4","#f47f94")

roundQual <- function(x,n){floor(x+.5)}

## Read in Data
last_path <- "C:/Users/Claire/Documents/Consulting/Qualtrics/Aramark2023/AramarkFall2022/data_files"
#last_path <- 'data_files'
### 

#not using now per thier comment on not being comparable
# last_data <- read.csv(paste0(last_path,"/spring2023_final.csv"),stringsAsFactors=FALSE)
# last_data <- last_data[3:nrow(last_data),]

last_data_fall_22 <- read.csv(paste0(last_path,"/Fall2022_V2.csv"),stringsAsFactors=FALSE)
last_data_fall_22 <- last_data_fall_22[3:nrow(last_data_fall_22),]

last_data_spring_22 <- read.csv(paste0(last_path,'/Spring_2022_v4.csv'),stringsAsFactors=FALSE)
last_data_spring_22 <- last_data_spring_22[3:nrow(last_data_spring_22),]

#there are two header rows below the actual heade
last_data21 <- read.csv(paste0(last_path,'/fall2021_V2.csv'),stringsAsFactors=FALSE)
last_data21 <- last_data21[3:nrow(last_data21),]

# read in uk seperate survey
datak_last <- read.csv(paste0(last_path,"/ukfall2021.csv"),stringsAsFactors=FALSE)
datak_last <- datak_last[3:nrow(datak_last),]
datak_last$SCHOOL_NAME <- "University of Kentucky"
datak_last$MARKET_SEGMENT <- 'Large Public'
datak_last$REGION_NAME <- 'Mid-Atlantic Region'

## merge together 
overlap <- names(last_data21)[names(last_data21) %in% names(datak_last)]
last_data21 <- bind_rows(last_data21,datak_last)
last_data21 <- last_data21 %>% filter(MSM.Filter==2)
last_data21$SCHOOL_NAME <- ifelse(last_data21$SCHOOL_NAME =="Simmons College","Simmons University", last_data21$SCHOOL_NAME)

## now read in spring 2021
last_data_spring21 <- read.csv(paste0(last_path,'/spring2021.csv'),stringsAsFactors=FALSE)
last_data_spring21 <- last_data_spring21[3:nrow(last_data_spring21),]

last_data_fall20 <- read.csv(paste0(last_path,'/fall2020_V2.csv'),stringsAsFactors=FALSE)
last_data_fall20 <- last_data_fall20[3:nrow(last_data_fall20),]


data<- read.csv("C:/Users/Claire/Documents/Consulting/Qualtrics/Aramark2023/Aramark2023/Fall2023.csv",stringsAsFactors=FALSE)
#data<- read.csv("Aramark_Dining_Base (Qual3538-0510DiningStyles)_October 16, 2023_10.19.csv",stringsAsFactors=FALSE)
data <- data[3:nrow(data),]

## Fix names 
data$SCHOOL_NAME <- ifelse(data$Q0=="USA",data$Q1_USA, data$Q1_CAN)

data$SCHOOL_NAME <- ifelse(data$SCHOOL_NAME =="Simmons College","Simmons University", data$SCHOOL_NAME)
data$SCHOOL_USA <- data$SCHOOL_NAME

# Split nationall - needed every year canada exists (ie starting spring 2023)
data_ca <- data[data$SCHOOL_CAN!="",]
data_us <- data[data$COUNTRY=="USA",]

# cod the "last year", will need to manually change this 
all_reps <- data_us %>% group_by(SCHOOL_NAME) %>% summarize(n=n()) 
all_reps$last_year <- ""

all_reps_spring2023 <- last_data %>% group_by(SCHOOL_NAME) %>% summarize(n=n()) 
all_reps$last_year <- ifelse(all_reps$SCHOOL_NAME %in% all_reps_spring2023$SCHOOL_NAME,"Spring 2023",all_reps$last_year)


all_reps_fall2022 <- last_data_fall_22 %>% group_by(SCHOOL_NAME) %>% summarize(n=n()) 
all_reps$last_year <- ifelse(all_reps$SCHOOL_NAME %in% all_reps_fall2022$SCHOOL_NAME,"Fall 2022",all_reps$last_year)

all_reps_spring2022 <- last_data_spring_22 %>% group_by(SCHOOL_NAME) %>% summarize(n=n()) 
all_reps$last_year <- ifelse((all_reps$last_year=="") & (all_reps$SCHOOL_NAME %in% all_reps_spring2022$SCHOOL_NAME),"Spring 2022",all_reps$last_year)


all_reps_fall2021 <- last_data21 %>% group_by(SCHOOL_NAME) %>% summarize(n=n()) %>% filter(n>49)
all_reps$last_year <- ifelse((all_reps$last_year=="") & (all_reps$SCHOOL_NAME %in% all_reps_fall2021$SCHOOL_NAME),"Fall 2021",all_reps$last_year)

all_reps_spring2021 <- last_data_spring21 %>% group_by(SCHOOL_NAME) %>% summarize(n=n()) 
all_reps$last_year <- ifelse((all_reps$last_year=="") & (all_reps$SCHOOL_NAME %in% all_reps_spring2021$SCHOOL_NAME),"Spring 2021",all_reps$last_year)







data_ca <- data[data$Q0=="Canada",]
data_us <- data[data$Q0=="USA",]

get_graphs <- function(data,data_last,UNIVERSITY_NAME,slide17_leg=-.4,slide23_leg=-.5,slide32_leg = .19,last_year ="Spring 2023", is_ca=F,is_harvest=F){

  ## SET up parameters 
  UNIVERSITY_NAME_WRAP <- paste(strwrap(UNIVERSITY_NAME,13),collapse = "\n")
  #use this for small graphs_fall_2022/ that don't want wrapped name for shorted universities
  UNIVERSITY_NAME_SHORT <- ifelse(nchar(UNIVERSITY_NAME) >= 23, paste(strwrap(UNIVERSITY_NAME,23),collapse = "\n  "), UNIVERSITY_NAME)
  UNIVERSITY_NAME_MED <- ifelse(nchar(UNIVERSITY_NAME) >= 30, paste(strwrap(UNIVERSITY_NAME,30),collapse = "\n  "), UNIVERSITY_NAME)
  
  # set up years
  this_year <- "Fall 2023"
  
  if (is_ca & (!'Q4ca' %in% names(data))){
    # move over q4 inot in data
    data$Q4ca <- data$Q4
  }
  

  # make directory for graphs_fall_2023/
  loc <- "graphs_fall_2023/"
  dir.create(paste0(loc,UNIVERSITY_NAME))
  
  #Fix some wierdnesses in region names
  #som have 'EAST' as region instead of eastern and same for Central
  data$REGION_NAME <- ifelse(data$REGION_NAME=='EAST', 'East Region', data$REGION_NAME)
  data$REGION_NAME <- ifelse(data$REGION_NAME=='CENTRAL', 'Central Region', data$REGION_NAME)
  
  data_last$REGION_NAME <- ifelse(data_last$REGION_NAME=='EAST', 'East Region', data_last$REGION_NAME)
  data_last$REGION_NAME <- ifelse(data_last$REGION_NAME=='CENTRAL', 'Central Region', data_last$REGION_NAME)
  
  
  

  data_c <- data 
  
  data <- data %>% filter(grepl("student|Other",Q2))

  
  # do same for last year 
  data_c_last <- data_last 
  data_last <- data_last %>% filter(grepl("student|Other",Q1.1)) # note mismatch of question numbers in older data
  
  
  
  data_school_c <- data_c %>% filter(SCHOOL_NAME== UNIVERSITY_NAME)
  data_school <- data %>% filter(SCHOOL_NAME== UNIVERSITY_NAME) 
  
  data_school_c_last <- data_c_last %>% filter(SCHOOL_NAME== UNIVERSITY_NAME)
  data_school_last <- data_last %>% filter(SCHOOL_NAME== UNIVERSITY_NAME) 
  
  if (nrow(data_school_c_last)>50) {
    HAS_LAST = TRUE
  } else {
    HAS_LAST = FALSE
  }
  
  
  #get region

  region <- data %>% filter(SCHOOL_USA== UNIVERSITY_NAME) %>% 
    select(REGION_NAME) %>% slice(1)
  REGION <- region[[1]]
  
 

  data_region_c <- data_c[data_c$REGION_NAME==REGION,]
  data_region <- data_region_c %>% filter(grepl("student|Other",Q2))

  if (REGION == 'East Region'){
    data_region_c_last <- filter(data_c_last, REGION_NAME%in%c("East","East Region")) # in some years east, some east region
  } else{
    data_region_c_last <- filter(data_c_last, REGION_NAME==REGION)
    
    
  }
  data_region_last <- data_region_c_last %>% filter(grepl("student|Other",Q1.1))

  #get market segment
  market_seg <- data %>% filter(SCHOOL_NAME== UNIVERSITY_NAME) %>% 
    select(MARKET_SEGMENT) %>% slice(1)
  MARKET_SEG <- market_seg[[1]]
  data_market_c <- filter(data_c, MARKET_SEGMENT==MARKET_SEG)
  data_market <- data_market_c %>% filter(grepl("student|Other",Q2))
  
  
  
  ############## Data Prep Functions
  
  top_two <- function(x,dataset){
    roundQual(100*sum(grepl("excellent|good|^agree|somewhat agree|strongly agree|definitely will buy|probably will buy|^extremely comfortable|^very comfortable|^very important|^important|extremely satisfied|somewhat satisfied",tolower(dataset[,x])))/sum(!is.na(dataset[,x]) & (!dataset[,x]%in%c("","Not Applicable/Don't Know", "N/A or Don't Know"))),0)
  }
  
  
  data_grp_bar <- function(col, appendix=FALSE, stacked_labels=FALSE,need_fill=FALSE,
                           comp=F,filter_enr=F,uselast=F,filterV=NULL,filter=NULL){
    if (comp==T){
      data <- data_c
      data_school <- data_school_c
      data_region <- data_region_c
       data_market <- data_market_c
    }
    
    if (uselast==T){
      data <- data_c_last 
      data_school <- data_school_c_last
      data_region <- data_region_c_last
    }
    
    if (filter_enr==T & uselast==F){ # note this implies comp=T
      if (is_ca== F){
        data <- data_c %>% filter(Q4=="Yes")
        data_school <- data_school_c %>% filter(Q4=="Yes")
        data_region <- data_region_c %>% filter(Q4=="Yes")
      } else {
        data <- data_c %>% filter(Q4ca=="Yes")
        data_school <- data_school_c %>% filter(Q4ca=="Yes")
        data_region <- data_region_c %>% filter(Q4ca=="Yes")
      } 
      
    } else if (filter_enr==T & uselast==T){
      if (is_ca== F){
        data <- data_c %>% filter(Q4=="Yes")
        data_school <- data_school_c %>% filter(Q4=="Yes")
        data_region <- data_region_c %>% filter(Q4=="Yes")
      } else {
        data <- data_c %>% filter(Q1.8ca=="Yes")
        data_school <- data_school_c %>% filter(Q4ca=="Yes")
        data_region <- data_region_c %>% filter(Q4ca=="Yes")
      } 
    }
    
    if (!is.null(filterV)){ # note this implies comp=T
      
      data <- data_c[data_c[[filterV]]%in%filter,]
      data_school <- data_school_c[data_school_c[[filterV]]%in%filter,]
      data_region <- data_region_c[data_region_c[[filterV]]%in%filter,]
    }
    
    
    
    data_temp_nation <- data %>%
      group_by_(col) %>%
      summarise(n = n()) %>%
      subset(.[,1]!="") %>%
      subset(.[,1]!="N/A or Don't Know") %>%
      mutate(freq = n / sum(n))
    
    data_temp_school <- data_school %>%
      group_by_(col) %>%
      summarise(n = n()) %>%
      subset(.[,1]!="") %>%
      subset(.[,1]!="N/A or Don't Know") %>%
      mutate(freq = n / sum(n))
    
    data_temp_region <- data_region %>%
      group_by_(col) %>%
      summarise(n = n()) %>%
      subset(.[,1]!="") %>%
      subset(.[,1]!="N/A or Don't Know") %>%
      mutate(freq = n / sum(n))
    
    # now if a level is mising from region or school add it
    levels <- data_temp_nation[,1,drop=T]
    school_levels <- levels[!levels %in% data_temp_school[,1,drop=T]]
    region_levels <- levels[!levels %in% data_temp_region[,1,drop=T]]
    
    if(length(school_levels) >0 ){
      
      add_rows <- data.frame("lev"= school_levels,"n" = rep(0,length(school_levels)),"freq" = rep(0,length(school_levels)))
      names(add_rows) <- names(data_temp_school)
      data_temp_school <- rbind(data_temp_school,add_rows)
    }
    
    if(length(region_levels) >0 ){
      
      add_rows <- data.frame("lev"= region_levels,"n" = rep(0,length(region_levels)),"freq" = rep(0,length(region_levels)))
      names(add_rows) <- names(data_temp_region)
      data_temp_region <- rbind(data_temp_region,add_rows)
    }
    
    
    if (stacked_labels) {
      data_temp_school$Location <- UNIVERSITY_NAME_WRAP
      data_temp_region$Location <- "Region"
      data_temp_nation$Location <- "Nation"
    } else {
      data_temp_school$Location <- UNIVERSITY_NAME
      data_temp_region$Location <- "Region"
      data_temp_nation$Location <- "Nation"
    }
    
    if (appendix){
      data_temp_market <- data_market  %>%
        group_by_(col) %>%
        summarise(n = n()) %>%
        subset(.[,1]!="") %>%
        subset(.[,1]!="N/A or Don't Know") %>%
        mutate(freq = n / sum(n))
      
      market_levels <- levels[!levels %in% data_temp_market[,1,drop=T]]
      
      if(length(market_levels) >0 ){
        
        add_rows <- data.frame("lev"= market_levels,"n" = rep(0,length(market_levels)),"freq" = rep(0,length(market_levels)))
        names(add_rows) <- names(data_temp_market)
        data_temp_market <- rbind(data_temp_market,add_rows)
      }
      
      
      if (stacked_labels){
          data_temp_market$Location <- "Market Segment"
        } else{
          data_temp_market$Location <- "Market Segment"
        }
        data_temp <- rbind(data_temp_school, data_temp_region, data_temp_market, data_temp_nation)
        if (stacked_labels){
            data_temp$Location <- factor(data_temp$Location, levels = c('Market Segment','Region', 'Nation', UNIVERSITY_NAME_WRAP))
          }else {
            data_temp$Location <- factor(data_temp$Location, levels = c('Market Segment','Region', 'Nation', UNIVERSITY_NAME))
      }
      } else{
        data_temp <- rbind(data_temp_school, data_temp_region, data_temp_nation)
        if (stacked_labels){
          data_temp$Location <- factor(data_temp$Location, levels = c('Region', 'Nation', UNIVERSITY_NAME_WRAP))
        }else {
          data_temp$Location <- factor(data_temp$Location, levels = c('Region', 'Nation', UNIVERSITY_NAME))
        }
      
    }
    
    data_temp$Percent <- data_temp$freq*100
    data_temp$Value <- data_temp[,col]
    #for levels that don't appear for one place, add 0 so they show up properly in graphs_fall_2022/
    
   
    #this adds exta levels 
    
    data_temp$label <- paste0(roundQual(data_temp$freq*100,0),"%")
    #data_temp$Location <- as.factor(data_temp$Location)
    #this makes .4 to 0 into <1% because otherwise it looks wier
    data_temp$label <- ifelse(data_temp$n>0 & data_temp$label=='0%', '<1%', data_temp$label)
    
    # this filters out NA rows
    data_temp <- data_temp[data_temp[,1]!="",]
    return(data_temp)
  }
  
  
  question_list <- c("Q6.2_1", "Q6.2_2", "Q6.2_3", "Q6.2_4", "Q6.2_5", "Q6.2_6", 
                     "Q6.2_7", "Q6.2_8", "Q6.2_9", "Q6.2_10", "Q6.2_11", "Q6.2_12", 
                     "Q6.2_13", "Q6.2_14", "Q6.2_15", "Q6.2_16", "Q6.2_17", "Q6.2_18","Q6.2_19")
  #in this case we are looking for is not blank
  
  make_separate_bin <- function(question_list, categories, levels, in_cat=TRUE, appendix=FALSE,comp=F,dot_plot=F){
    #then recreate subsets usually just students , all for the complete graphs_fall_2022/ 
    if (comp==T){
      data <- data_c
    }
    
    binary_questions <- c()
    for (i in 1:length(question_list)){
      q_label <- paste0(question_list[[i]], '_binary')
      binary_questions <- c(binary_questions, q_label)
      if (in_cat){
        data[,q_label] <- ifelse(data[,question_list[[i]]] %in% categories, 1, 0)
        # recode missing to NA
        data[,q_label] <- ifelse(data[,question_list[[i]]]=="", NA, data[,q_label])
       
         # recode don't know to NA, only for dot plots
        if (dot_plot){
          data[,q_label] <- ifelse(data[,question_list[[i]]]%in%c("","Not Applicable/Don't Know"), NA, data[,q_label])
        }
       
        
      }else {
        data[,q_label] <- ifelse(!data[,question_list[[i]]] %in% categories, 1, 0)
      }
    }
    #then recreate subsets usually just students , all for the complete graphs_fall_2022/ 
      data_market <- filter(data, MARKET_SEGMENT==MARKET_SEG)
      data_region <- filter(data, REGION_NAME==REGION)
      data_school <- data %>% filter(SCHOOL_USA== UNIVERSITY_NAME)

    
    grouped_school <- data_school %>% 
      gather(Value, num_meals, all_of(binary_questions)) %>%
      group_by(Value) %>% 
      summarise(count = sum(num_meals,na.rm=T),n=sum(!is.na(num_meals))) %>%
      mutate(Percent = 100 *count / n )
    grouped_school$Location <- UNIVERSITY_NAME
    
    grouped_region <- data_region %>% 
      gather(Value, num_meals, all_of(binary_questions))%>% 
      group_by(Value) %>% 
      summarise(count = sum(num_meals,na.rm=T),n=sum(!is.na(num_meals))) %>%
      mutate(Percent = 100 *count / n )
    grouped_region$Location <- 'Region'
    
    grouped_nation <- data %>% 
      gather(Value, num_meals, all_of(binary_questions)) %>% 
      group_by(Value) %>% 
      summarise(count = sum(num_meals,na.rm=T),n=sum(!is.na(num_meals))) %>%
      mutate(Percent = 100 *count / n )
    grouped_nation$Location <- 'Nation'
    
    if (appendix){
      grouped_market <- data_market %>% 
        gather(Value, num_meals, binary_questions) %>% 
        group_by(Value) %>% 
        summarise(count = sum(num_meals,na.rm=T),n=sum(!is.na(num_meals))) %>%
        mutate(Percent = 100 *count / n )
      grouped_market$Location <- 'Market Segment'
      grouped <- rbind(grouped_school, grouped_region, grouped_nation, grouped_market)
      grouped$Location <- factor(grouped$Location,levels = c('Market Segment', UNIVERSITY_NAME,"Nation","Region"))
    } else{
      grouped <- rbind(grouped_school, grouped_region, grouped_nation)
      grouped$Location <- factor(grouped$Location,levels = c(UNIVERSITY_NAME, "Nation","Region"))
      
    }
    
    grouped$label <- paste0(roundQual(grouped$Percent ,0),"%")

    #added levels as the binary question list to control order

    grouped$Value <- factor(grouped$Value, levels = binary_questions)
    levels(grouped$Value)  <-  levels
    wrap <-ifelse(dot_plot,27,35)
    grouped$Value <- unlist(lapply(grouped$Value,FUN=function(x){paste(strwrap(x,wrap),collapse="\n")}))
    
    orderdf <- grouped %>% subset(Location==UNIVERSITY_NAME) %>% 
      arrange(desc(Percent)) %>% select(Value)
    order <- orderdf$Value
    
    grouped$Value <- factor(grouped$Value,levels=rev(order))
    return (grouped)
    
  }
  
  top_two <- function(x,dataset){

    roundQual(100*sum(grepl("excellent|good|^agree|strongly agree|definitely will buy|probably will buy|extremely satisfied|somewhat satisfied|^important|very important",tolower(dataset[,x])))/sum(!is.na(dataset[,x]) & (!dataset[,x]%in%c("","Not Applicable/Don't Know", "N/A or Don't Know"))),0)

  }
  
  
  ############ NEW GRAPHS 
  
  ################## Slide 2 ##############
  year<- data_grp_bar('Q2',comp=T)
  year <-year %>% filter(Location==UNIVERSITY_NAME)
  year$Q2 <- gsub(" (please specify)","",year$Q2,fixed=T)
  year$Q2 <- gsub("year student","Year",year$Q2)
  year$Q2 <- gsub("Graduate student","Graduate Student",year$Q2)
  year$Q2 <- gsub("Other:","Other",year$Q2)
  year$Value <- factor(year$Q2)
  
  levels <-  c("1st Year",
               "2nd Year",
               "3rd Year",
               "4th Year",
               "5th Year",
               "International student",
               "Employee",
               "Faculty",
               "Graduate Student","Staff",
               "Other")
  # levels(year$Value) <-levels
  
  year$Value <- factor(year$Value, levels = rev(levels))
  
  plot21b<-
    ggplot(year,aes(x=Value,y=Percent,label=label)) + 
    geom_bar(stat="identity",fill=two_tone[1],width = .8)+ 
    geom_text(aes(x=Value,y=Percent + max(year$Percent)/7,label=label),size = 4, position = position_dodge(width = .9)) +
    coord_flip() + 
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(year$Percent)+max(year$Percent)/7+10), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.4,-0.03),
          legend.margin=margin(t = 0, unit='cm')) 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","location_2a.png"),plot=plot21b,width = 4, height=3,units="in")
  
  

  
  # graph 2, US - location pie chart
  #CA, location bar graph
  
  if (!is_ca){
    g2 <- data_school %>% group_by(Q3) %>% summarize(count=n())
    
    # Compute percentages
    g2$fraction <- g2$count / sum(g2$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    g2$ymax <- cumsum(g2$fraction)
    
    # Compute the bottom of each rectangle
    g2$ymin <- c(0, head(g2$ymax, n=-1))
    
    # Compute label position
    g2$labelPosition <- (g2$ymax + g2$ymin) / 2
    
    # Compute a good label
    g2$label <- paste0(round(g2$count*100/sum(g2$count)))
    
    # wrap legend
    g2$Q3 <- unlist(lapply(  g2$Q3,FUN=function(x){paste(strwrap(x,20),collapse="\n")}))
    
    # Make the plot
    pie <- ggplot(g2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Q3)) +
      geom_rect() +
      geom_text( x=3.5, aes(y=labelPosition, label=paste0(label,"%"),color=Q3), size=6,show.legend = FALSE) +
      scale_fill_manual(values=three_tone)+
      scale_color_manual(values=c("white","black","white"))+
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "bottom" , legend.title = element_blank(),legend.spacing.y = unit(1.0, 'cm'), legend.key.height = unit(.5, 'cm')) 
    
    
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","commuting_2b.png"),plot=pie,width = 4, height=4,units="in")
    
  } else{
    
    oncamp<- data_grp_bar('Q3',comp=T)
    oncamp <- oncamp%>% filter(Location==UNIVERSITY_NAME)
    oncamp$Value <- gsub("walking distance","walking\ndistance",oncamp$Q3,fixed=T)
    oncamp$Value <- gsub("drive or take public transportation","drive\npublic transportation",oncamp$Value,fixed=T)
    oncamp$Value <- factor(oncamp$Value,levels = rev(c("On campus", "Off campus (walking\ndistance)", "Off campus (drive\npublic transportation)")))
    
    plot21c<-
      ggplot(oncamp,aes(x=Value,y=Percent,label=label)) + 
      geom_bar(stat="identity",fill=two_tone[1],width = .8)+ 
      geom_text(aes(x=Value,y=Percent + max(Percent)/7,label=label),size = 4, position = position_dodge(width = .9)) +
      coord_flip() + 
      theme_minimal() + ylab("") + xlab("") +
      scale_y_continuous(limits = c(0,max(oncamp$Percent)+max(oncamp$Percent)/7+10), expand=c(0,0)) +
      theme(panel.grid = element_blank(),
            axis.line.y =  element_line(color = "grey"),
            axis.text.x = element_blank(), legend.title = element_blank(),
            # legend.position = "bottom", 
            legend.position=c(0.4,-0.03),
            legend.margin=margin(t = 0, unit='cm')) 
    
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide21c.png"),plot=plot21c,width = 4, height=4,units="in")
    
    
    
    
  }
   
  
  ################## Slide 3 ##############
  
  ##################
 
  ##################### Slide 4 ########### #

 
  
  #canadian don't get the previous year
  if (HAS_LAST & !is_ca){
    sat <- data_grp_bar('Q5', stacked_labels=TRUE,comp=T)
    sat_this <- sat %>% filter(!Location%in%c("Region", "Nation"))
    sat_this$top2 <- paste0("\nTop Two Box: ",top_two("Q5",data_school_c),"%")
    sat_this$year <- this_year
    sat_this$year <- paste0(sat_this$year,sat_this$top2)
    
    # FLAG : will need to change this after 2023 when old data is not consisten
    sat_last <- data_grp_bar('Q2.1', stacked_labels=TRUE,uselast = T) %>% filter(!Location%in%c("Region", "Nation"))
    sat_last$year <- last_year
    sat_last$top2 <- paste0("\nTop Two Box: ",top_two("Q2.1",data_school_c_last),"%")
    sat_last$year <- paste0(sat_last$year,sat_last$top2)
    names(sat_last) <- gsub("Q2.1","Q5",names(sat_last),fixed=T)
    names(sat_last)[6] <- "Value"
    
    sat_last <- sat_last %>% filter(Q5 %in% c( "Excellent", "Good","Fair","Average", "Poor",  "Terrible"))
    
    sat <- bind_rows(sat_this,sat_last)
    sat$hjust_var <- ifelse(sat$Percent<=4,-1.5,.5)
    sat$Value <- sat$Q5
    
    years <- c(unique(sat_last$year),unique(sat_this$year))
    
  } else{
    sat <- data_grp_bar('Q5', stacked_labels=TRUE,comp=T)
    sat <- sat %>% filter(!Location%in%c("Region", "Nation"))
    sat$year <- this_year
    sat$top2 <- paste0("\nTop Two Box: ",top_two("Q5",data_school_c),"%")
    sat$year <- paste0(sat$year,sat$top2)
    sat$hjust_var <- ifelse(sat$Percent<=4,-2.3,.5) 
    years <- unique(sat$year)
  }
  
  sat$Value <- as.factor(sat$Q5)
  sat$label_col <- ifelse(sat$Value %in% c("Excellent","Good"),"black","white")
  
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  
  sat$Q5 <- gsub("Average","Fair",sat$Q5) # correct for language change
  
  sat$Value <- factor(sat$Q5, levels <- c( "Excellent", "Good","Fair", "Poor", 
                                              "Terrible"))
  
  
  sat$year <- factor(sat$year,levels=years)
  
  sat_plot<- ggplot(sat,aes(x=year,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text(aes(x=year,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 6, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Fair","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          axis.text.x=element_text(size=15),
          legend.text=element_text(size=15),
          # legend.position = "left", 
          plot.margin = margin(l=.9, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0, unit='cm')) + guides(color="none")
  
  if(!is_ca){
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","sat_4a.png"),plot=sat_plot,width = 5, height=5,units="in")
  } else {
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide3a.png"),plot=sat_plot,width = 4, height=4.5,units="in")
    
  }
    
  ################ satisfaction comparison
  
  sat <- data_grp_bar('Q5', appendix=TRUE, stacked_labels=TRUE,comp=T) %>% filter(Location!="Market Segment")
  sat$Value <- as.factor(sat$Q5)
  sat$label_col <- ifelse(sat$Value %in% c("Excellent","Good"),"black","white")
  sat$hjust_var <- ifelse(sat$Percent<=4,-2,.5)
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  sat$Value <- factor(sat$Value, levels <- c( "Excellent", "Good","Fair", "Poor", 
                                              "Terrible"))
  
  sat$Location <- factor(sat$Location,levels=c(UNIVERSITY_NAME_WRAP,"Region", "Nation"))
  
  
  
  plot10 <- ggplot(sat,aes(x=Location,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text(aes(x=Location,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,
                    size = 5, position = position_stack(vjust = 0.5),segment.alpha=0,segment.color= "white") +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Fair","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size=13), 
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "left", 
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = margin(l=1,r=.1, unit="cm"),
          legend.text=element_text(size=15),
          legend.position=c(0.02,0.5),
          legend.margin=margin(t = 0, r = .3, unit='cm')) + guides(color=F)
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","sat_4b.png"),plot=plot10,width = 7.5, height=4.5,units="in")
  
  
  ####### Slide 5, Q13 - Satisfaction with specific dining locations
  #Q13_1 to Q13_4
  
  sat <- data_grp_bar('Q13_1', stacked_labels=T,comp=T)
  sat_1 <- sat %>% filter(!Location%in%c("Region", "Nation"))
  sat_1$q <- "All you care to eat"
  names(sat_1)[1] <- 'Q13'
  
  sat <- data_grp_bar('Q13_2', stacked_labels=TRUE,comp=T)
  sat_2 <- sat %>% filter(!Location%in%c("Region", "Nation"))
  sat_2$q <- "Retail"
  names(sat_2)[1] <- 'Q13'
  
  sat <- data_grp_bar('Q13_3', stacked_labels=TRUE,comp=T)
  sat_3 <- sat %>% filter(!Location%in%c("Region", "Nation"))
  sat_3$q <- "Convenience\nStores"
  names(sat_3)[1] <- 'Q13'
  
  sat <- data_grp_bar('Q13_4', stacked_labels=TRUE,comp=T)
  sat_4 <- sat %>% filter(!Location%in%c("Region", "Nation"))
  sat_4$q <- "Coffee Shops"
  names(sat_4)[1] <- 'Q13'

  
  sat <- bind_rows(sat_1, sat_2, sat_3, sat_4)
  sat$hjust_var <- ifelse(sat$Percent<=4,-1.5,.5)
  sat$Value <- as.factor(sat$Q13)
  sat$label_col <- ifelse(sat$Value %in% c("Extremely Satisfied","Somewhat Satisfied", "Extremely Dissatisfied"),"black","white")

  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  sat$Value <- factor(sat$Value, levels <- c( "Extremely Satisfied","Somewhat Satisfied","Neither Satisfied nor Dissatisfied","Somewhat Dissatisfied",  "Extremely Dissatisfied"))
  
  
  sat <- sat %>% filter(Q13!="N/A or Don't Know")
  
  sat$q<- factor(sat$q, levels = c("All you care to eat", "Retail", "Convenience\nStores", "Coffee Shops"))
  
  #find_me
  
  plot_outlet_sat <- ggplot(sat,aes(x=q,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    #REplace repel ( geom_text_repel)
    geom_text(aes(x=q,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,
              size = 6, position = position_stack(vjust = 0.5),segment.alpha=0,segment.color= "white") +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Extremely Satisfied","Somewhat Satisfied","Neither", "Extremely Dissatisfied","Somewhat Dissatisfied"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), 
          axis.text.x=element_text(size=17),
          legend.title = element_blank(),
          legend.text=element_text(size=15),
          # legend.position = "left", 
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = margin(l=6,r=.1, unit="cm"),
          legend.position=c(-0.05,0.5),
          legend.margin=margin(t = 0, r = 2, unit='cm')) + guides(color=F)
  
  if (!is_ca){
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide5.png"),plot=plot_outlet_sat,width = 10, height=7,units="in",bg = "transparent")
  } else {
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide6b_ca.png"),plot=plot_outlet_sat,width = 10, height=7,units="in",bg = "transparent")
    
  }
  

  ################## Slide 6a Value Rating
  ## These are just students
  # CA never has the past results even if they exist
  if (HAS_LAST & !is_ca){
    sat <- data_grp_bar('Q7', stacked_labels=TRUE,comp=F)
    sat_this <- sat %>% filter(!Location%in%c("Region", "Nation"))
    sat_this$year <- this_year
    sat_this$Value <- sat_this$Q7
    #NB: note the different question number in prior year
    sat_last <- data_grp_bar('Q2.4', stacked_labels=TRUE,uselast = T) %>% filter(!Location%in%c("Region", "Nation"))
    sat_last$year <- last_year
    sat_last$Value <- sat_last$Q2.4
    #there are a couple of data error rows that need removed
    sat_last <- sat_last %>% filter(Q2.4 %in% c( "Excellent value", "Good value","Fair value","Average value", "Poor value", 
                                                 "Terrible value"))
    sat <- bind_rows(sat_this,sat_last)
    sat$hjust_var <- ifelse(sat$Percent<=4,-2.4,.5)
    years <- c(unique(sat_last$year),unique(sat_this$year))
  } else{
    sat <- data_grp_bar('Q7', stacked_labels=TRUE,comp=F)
    sat <- sat %>% filter(!Location%in%c("Region", "Nation"))
    sat$year <- this_year
    sat$hjust_var <- ifelse(sat$Percent<=4,-4.4,.5)
    years <- unique(sat$year)
    sat$Value <- sat$Q7
    
  }
  #match previous categories to current categories
  sat$Value <- ifelse(sat$Value=='Average value', 'Fair value', sat$Value)
  sat$Value <- as.factor(sat$Value)
  
  sat$label_col <- ifelse(sat$Value %in% c("Poor value",'Terrible value'),"white","black")
  
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  
  
  
  sat$Value <- factor(sat$Value, levels <- c( "Excellent value", "Good value","Fair value", "Poor value", 
                                              "Terrible value"))
  
  
  sat$year <- factor(sat$year,levels=years)
  plot6a <- ggplot(sat,aes(x=year,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    #replace with geom_text_repel
    geom_text(aes(x=year,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Fair","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),axis.text.x=element_text(size=13), legend.title = element_blank(),
          # legend.position = "left", 
          plot.margin = margin(l=.7, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.text = element_text(size = 13),
          legend.margin=margin(t = 0, unit='cm')) + guides(color="none")
  
  if(!is_ca){
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide6a.png"),plot=plot6a,width = 5, height=4.5,units="in")
  } else{
    #for canada this is slide 3b
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide3b.png"),plot=plot6a,width = 5, height=4.5,units="in")
    
  }
  
  
  
  ################## Slide 6b Meaning of Value, region/nation
  meaning <- data_grp_bar('Q9',comp=F)
  

  
  #clean up the text answers
  meaning$Value <- gsub('Value to me is about ', '', meaning$Q9)
  meaning$Value <- paste0(toupper(substr(meaning$Value, 1, 1)), substr(meaning$Value, 2, nchar(meaning$Value)))
  
  meaning$Value <- factor(meaning$Value,
                          levels = c("I do not care about value", 
                                     "How much food I receive",
                                     "How much I pay",
                                     "The quality of the food",
                                     "The overall experience"))
  
  
  
  meaning <- complete(meaning, Location, Value, fill = list(Percent=0, label = '0%'))
  
  orderdf <- meaning %>% subset(Location==UNIVERSITY_NAME) %>% 
    arrange(desc(Percent)) %>% 
    select(Value)
  
  
  meaning$Value <- factor(meaning$Value,levels=rev(orderdf$Value))
  
  #ca doesn't have nation and region
  if (!is_ca){
    meaning$Location <- factor(meaning$Location,levels=c("Nation","Region",UNIVERSITY_NAME))
    
    legend_pos = 'bottom'
    
  } else {
    meaning <- meaning %>% filter(!Location %in% c("Nation","Region"))
    legend_pos="None"
    
  }
  
  
  plot6b<- ggplot(meaning,aes(x=Value,y=Percent,fill=Location,label=label)) + 
    geom_bar(stat="identity",position="dodge")+ 
    #geom_text(aes(x=Value,y=Percent + max(meaning$Percent)/15,label=label),size = 4, position = position_dodge(width = .9)) +
    geom_text(aes(x=Value,y=Percent + max(meaning$Percent)/10,label=label),size = 4, position = position_dodge(width = .9)) +
    coord_flip() + 
    scale_fill_manual(breaks = c("Nation","Region",UNIVERSITY_NAME),values=rev(three_tone),labels=c(" Nation "," Region ",paste(" ",UNIVERSITY_NAME_SHORT," "))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(meaning$Percent)+12), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.y=element_text(size=15),
          axis.text.x = element_blank(), 
          legend.title = element_blank(),
          legend.position = legend_pos, 
          #legend.position=c(0.5,0.5),
          legend.margin=margin(l=-1.8, t = 0,r=1, unit='cm'))  + 
    guides(fill = guide_legend(reverse = TRUE)) 
  
  

 if(!is_ca){
   ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide6b.png"),plot=plot6b,width = 5, height=4.5,units="in")
   
   
 } else{
   ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide3c.png"),plot=plot6b,width = 5, height=4.5,units="in")
   
 }
  
  
  
  ################# Slide 7 - Individual performance metrics
  if (!is_ca){
    #find_me
    questions = c( "Q11A_USA_1", "Q11A_USA_2", "Q11A_USA_3", 
                  "Q11A_USA_4", "Q11A_USA_5", "Q11A_USA_6", "Q11A_USA_7", "Q11A_USA_8", "Q11A_USA_9")
    q_names = c( "Availability of special dietary options to fit my dietary needs",
                'Cleanliness',
                'Comfortable dining experience',
                'Convenience',
                'Food quality',
                'Food variety',
                'Knowledgeable/Helpful staff',
                'Price/Value',
                'Welcoming/Friendly staff')
    
    vals <- c()
    for (q in questions){
      vals = c(vals,top_two(q,data_school_c))
    }
    
    data_7 <- data.frame(cbind(q_names, vals))
    data_7$vals <- as.numeric(data_7$vals)
    data_7 <- data_7 %>% arrange(desc(vals))
    data_7$pct <- paste0(as.character(data_7$vals), '%')
    
    
    data_7$q_names <- unlist(lapply(data_7$q_names,FUN=function(x){paste(strwrap(x,35),collapse="\n")}))
    
    
    levels <-  data_7$q_names
    
    data_7$q_names<- factor(data_7$q_names, levels = rev(levels))
    
    
    
    
    plot7<-
      ggplot(data_7,aes(x=q_names,y=vals,label=pct),fill=three_tone[2]) + 
      geom_bar(stat="identity",position="dodge",fill=two_tone[1])+ 
      geom_text(aes(x=q_names,y=vals + max(data_7$vals)/12,label=pct),size = 5, position = position_dodge(width = .9)) +
      coord_flip() + 
      scale_fill_manual(values=three_tone[2]) +
      #scale_fill_manual(breaks = c("Region","Nation",UNIVERSITY_NAME),values=rev(three_tone),labels=c("   Region   ","   Nation  ",paste(" ",UNIVERSITY_NAME,"  "))) +
      theme_minimal() + ylab("") + xlab("") +
      scale_y_continuous(limits = c(0,max(data_7$vals)+15), expand=c(0,0)) +
      theme(panel.grid = element_blank(),
            axis.line.y =  element_line(color = "grey"),
            axis.text.y= element_text(size=13),
            axis.text.x = element_blank(), legend.title = element_blank(),
            # legend.position = "bottom", 
            legend.position=c(0.3,-0.03),
            legend.margin=margin(t = 0, unit='cm'))  + 
      guides(fill = "none")
    
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide7.png"),plot=plot7,width = 6, height=4.5,units="in")
    
    
  }
  
  ####### Slide 8a Current Enrollment Status
  data_8a <- data_grp_bar('Q15',comp=T)
  data_8a$Value <- as.factor(data_8a$Q15)
  data_8a$Value <- gsub('participate in a meal plan', 'participate',data_8a$Value)
  data_8a$Value <- gsub('participate in the meal plan', 'participate',data_8a$Value)
  
  data_8a$Value <- unlist(lapply(data_8a$Value,FUN=function(x){paste(strwrap(x,25),collapse="\n")}))
  
  
  orderdf <- data_8a %>% subset(Location==UNIVERSITY_NAME) %>% 
    arrange(desc(Percent)) %>% 
    select(Value)
  
  
  data_8a$Value <- factor(data_8a$Value,levels=rev(orderdf$Value))
  data_8a$Location<- factor(data_8a$Location,levels=c("Nation","Region",UNIVERSITY_NAME))
  
  plot8a<- ggplot(data_8a,aes(x=Value,y=Percent,fill=Location,label=label)) + 
    geom_bar(stat="identity",position="dodge")+ 
    geom_text(aes(x=Value,y=Percent + max(data_8a$Percent)/10,label=label),size = 3, position = position_dodge(width = .9)) +
    coord_flip() + 
    scale_fill_manual(breaks = c("Nation","Region",UNIVERSITY_NAME),values=rev(three_tone),
                      labels=c(" Nation  ","  Region   ",paste(" ",UNIVERSITY_NAME_SHORT,"  "))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(data_8a$Percent)+15), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.y=element_text(size=10),
          axis.text.x = element_blank(), legend.title = element_blank(),
          #legend.position = "bottom", 
          panel.border = element_blank(),
          plot.background = element_rect(fill = "#f2f2f2",linetype=0),
          panel.background = element_rect(fill = "#f2f2f2",linetype=0),
          legend.position=c(0.05,-.06),
          legend.margin=margin(t = .1,b=.25, r=1, unit='cm'),
          legend.key.size = unit(.4, 'cm'),
          legend.key.height = unit(.4, 'cm'),
          legend.text = element_text( size = 7.5))  + 
    guides(fill = guide_legend(reverse = TRUE,ncol=3))
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide8a.png"),plot=plot8a,width = 4, height=2.5,units="in")
  

  ####### Slide 8b- Meal Plan purchasing intentions
  # both the enrollment question (used in the data_grp_bar function)
  #and the likelyhood of purchasing question 
  #have changed (4.2 in previous year, 17 today)
  
  if (!is_ca){
    if ( HAS_LAST){
      sat <- data_grp_bar('Q17', stacked_labels = TRUE,filter_enr=T, comp=T)
      sat_this <- sat %>% filter(!Location%in%c("Region", "Nation"))
      sat_this$year <- this_year
      sat_this$Value <- sat_this$Q17
      sat_last <- data_grp_bar('Q4.2', stacked_labels=TRUE,uselast = T) %>% filter(!Location%in%c("Region", "Nation"))
      sat_last$year <- last_year
      sat_last$Value <- sat_last$Q4.2
      #correct some data error values
      sat_last <- sat_last %>% filter(Value %in% c( "Definitely will buy" , "Probably will buy",    "Might or might not buy" ,      
                                                    "Probably will not buy" ,"Definitely will not buy"))
      sat <- bind_rows(sat_this,sat_last)
      sat$hjust_var <- ifelse(sat$Percent<=4,-1.6,.5)
      years <- c(unique(sat_last$year),unique(sat_this$year))
      
    } else {
      sat <- data_grp_bar('Q4.2', stacked_labels=TRUE,comp=T)
      sat <- sat %>% filter(!Location%in%c("Region", "Nation"))
      sat$year <- this_year
      sat$hjust_var <- ifelse(sat$Percent<=4,-2.6,.5)
      years <- unique(sat$year)
      
    }        
    
    plan <- sat
    plan$Value <- as.factor(plan$Value)
    plan$Value <- unlist(lapply(plan$Value,FUN=function(x){paste(strwrap(x,35),collapse="\n")}))
    
    plan$label_col <- ifelse(plan$Value %in% c("Definitely will buy" , "Probably will buy", "Definitely will not buy"),"black","white")
    
    #adust label color if it is off the side to be black
    plan$label_col <- ifelse(plan$Percent<=4,'black', plan$label_col)
    
    
    plan$Value <- factor(plan$Value, levels <- c( "Definitely will buy" , "Probably will buy",    "Might or might not buy" ,      
                                                  "Probably will not buy" ,"Definitely will not buy"  ))
    plan$year <- factor(plan$year,levels=years)
    plot8b <- ggplot(plan,aes(x=year,y=Percent,fill=Value,label=label)) + 
      geom_bar(stat="identity",width=.5)+ 
      #replace with geom_text_repel
      geom_text(aes(x=year,y=Percent,fill=Value,label=label, size=5, color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = 0.5)) +
      scale_x_discrete(position = "top")  + 
      scale_fill_manual(values=colors,labels=paste("   ",c("Definitely","Probably","Maybe","Probably Not","Definitely Not"))) +
      scale_color_manual(values=c("black","white"))+
      theme_minimal() + ylab("") + xlab("") + 
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank(), legend.title = element_blank(),
            axis.text.x=element_text(size=14),
            #legend.position = "left", 
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.spacing = unit(2, "cm"),
            plot.margin=unit(c(0,0,0,1.5),"cm"),
            legend.text=element_text(size=12),
            legend.position=c(0.03,0.5),
            legend.margin=margin(t = 0,l=0, r=1, unit='cm')) + guides(color=F)
    
    
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide8b.png"),plot=plot8b,width = 5.5, height=4.5,units="in",bg="transparent")
    
  }
  
  ####### Slide 8c motivators to purchase
  
  if (is_ca==F){
    base_data <- data_school_c%>% filter(Q4 %in% c("Yes")) %>% filter(Q19!="Graduating")
  } else {
    base_data <- data_school_c%>% filter(Q4ca %in% c("Yes")) %>% filter(Q19!="Graduating")
  }
  
  if(!is_ca){
    base <- nrow(base_data) 
    
    res <- base_data %>%
      select(dplyr::starts_with("Q20")) %>% select(!dplyr::contains("Q20_9_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q20")) %>%
      filter(value!="") %>%
      mutate(value= gsub(" \\(please specify)","",value)) %>% 
      #mutate(value = gsub("meal plan","dining plan",value)) %>% 
      #mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
    
    
    res$Percent <- ifelse(is.na(res$n),0,res$n)
    
    
    res$value <- unlist(lapply(res$value,FUN=function(x){paste(strwrap(x,40),collapse="\n")}))
    
    
    res$label<- paste0(roundQual(res$n,0),"%")
    
    res <- res %>% arrange(desc(n)) %>% arrange(value=="Other:")%>% arrange(value=="None of these")
    levels <-  res$value
    
    res$value <- factor(res$value, levels = rev(levels))
    
    plotMotiv<-
      ggplot(res,aes(x=value,y=n,label=label)) + 
      geom_bar(stat="identity",fill=two_tone[1],width = .8)+ 
      geom_text(aes(x=value,y=n + max(res$n)/12,label=label),size = 4, position = position_dodge(width = .9)) +
      coord_flip() + 
      theme_minimal() + ylab("") + xlab("") +
      scale_y_continuous(limits = c(0,max(res$n+max(res$n)/12+10)), expand=c(0,0)) +
      theme(panel.grid = element_blank(),
            axis.line.y =  element_line(color = "grey"),
            axis.text.y=element_text(size=11),
            axis.text.x = element_blank(), legend.title = element_blank(),
            # legend.position = "bottom", 
            legend.position=c(0.4,-0.03),
            legend.margin=margin(t = 0, unit='cm')) 
    
    
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide8c.png"),plot=plotMotiv,width = 6, height=5,units="in")
  }
  
  
  ##############################################
  #### Slide 9a
  ## this is for all repsondents 
  
  eligible = c('Milk', 'Eggs', 'Peanuts', 'Tree nuts', 'Fish', 'Shellfish', 'Soy', 
               'Wheat/Gluten', 'Sesame',  'Other (please specify):')
  if (is_ca){
    eligible = c(eligible, 'Mustard')
  }
  
  
  res <- data_school_c %>% filter(Q23_1=="Food allergies or intolerances")%>% 
    select(dplyr::starts_with("Q24")) %>% select(!dplyr::contains("Q24_11_TEXT")) %>%
    #select(!dplyr::contains("Q70_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q24")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n)) %>% 
    filter(value!='None of the above') %>% 
    filter(value %in% eligible) %>%
    arrange(value %in% c("Other (please specify):"))
  
  res$value <- gsub(" \\(please specify\\):","",res$value)
  res$value <- gsub("Wheat/Gluten","Wheat/\nGluten",res$value)
  
  res$label<- paste0(roundQual(res$n,0),"%")
  
  
  levels <-  res$value
  
  res$value <- factor(res$value, levels = levels)
  res$Percent <- res$n
  
  plot9a<- ggplot(res,aes(x=value,y=Percent,label=label)) + 
    geom_bar(stat="identity",position="dodge",fill=two_tone[1],width=.5)+ 
    geom_text(aes(x=value,y=Percent + max(Percent)/10,label=label),size = 6, position = position_dodge(width = .9)) +
    #scale_fill_manual(breaks = rev(c("Region","Nation",UNIVERSITY_NAME)),values=three_tone,labels=rev(c("  Region  ","   Nation  ",paste(" ",UNIVERSITY_NAME,"  ")))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent)+max(res$Percent)/5), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.x =  element_line(color = "grey"),
          #axis.text.x = element_text(angle=45,hjust=1,size=18),
          axis.text.x = element_text(size=18),
          axis.text.y = element_blank(), legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          #legend.position = "bottom", 
          legend.position=c(0.5,-.4),
          legend.margin=margin(t = 0.1,b=0,l=.1, unit='cm'))  + 
    guides(fill = "none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide9a.png"),
         plot=plot9a,width =10.5, height=2.55,units="in",bg="transparent")
  

  #Slide 9b
  ## this is for all repsondents 
  
  eligible = c('Vegetarian', 'Vegan', 'Halal', 'Kosher', 'Paleo/Primal', 'Ketogenic', 'Mediterranean', 'Other (please specify):')
  if (!is_ca){
    eligible = c(eligible, c('Atkins', 'GMO-Free', 'Whole 30', 'Plant-based', 'Other',"Low-FODMAP","Pescatarian"))
  } else {
    eligible = c(eligible, c( 'Flexitarian', 'No Peanut/Tree Nut', 'No Dairy', 'No Gluten', 'Other'))
  }
  
  
  
  res <- data_school_c %>% filter(Q23_2=="Special dietary lifestyle for religious reasons"|Q23_3=="Special dietary lifestyle for medical reasons"|Q23_4=="Special dietary lifestyle for personal reasons")%>% 
    select(dplyr::starts_with("Q25")) %>% select(!dplyr::contains("Q25_18_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q25")) %>%
    filter(value!="") %>%
    filter(value %in% eligible) %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n)) %>% 
    filter(value!='None of the above') %>% 
    arrange(value %in% c("Other (please specify):"))
  
  
  res$value <- gsub(" \\(please specify\\):","",res$value)
  res$value <- gsub("/", "/\n", res$value)
  res$value <- gsub("-", "-\n", res$value)
  res$value <- gsub("Ketogenic", "Keto", res$value)
  res$value <- gsub("Mediterranean", "Mediterr-\nanean", res$value)

  res$label<- paste0(roundQual(res$n,0),"%")
  
  
  levels <-  res$value
  
  res$value <- factor(res$value, levels = levels)
  res$Percent <- res$n
  
  plot9b<- ggplot(res,aes(x=value,y=Percent,label=label)) + 
    geom_bar(stat="identity",position="dodge",fill=two_tone[1],width=.5)+ 
    geom_text(aes(x=value,y=Percent + max(Percent)/10,label=label),size = 6, position = position_dodge(width = .9)) +
    #scale_fill_manual(breaks = rev(c("Region","Nation",UNIVERSITY_NAME)),values=three_tone,labels=rev(c("  Region  ","   Nation  ",paste(" ",UNIVERSITY_NAME,"  ")))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent)+max(res$Percent)/5), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          plot.margin = margin(0, 0, 0, .5, "cm"),
          axis.line.x =  element_line(color = "grey"),
          axis.text.x = element_text(size =15),
          axis.text.y = element_blank(), legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          #legend.position = "bottom", 
          legend.position=c(0.5,-.4),
          
          legend.margin=margin(t = 0.1,b=0,l=.1, unit='cm'))  + 
    guides(fill = "none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide9b.png"),
         plot=plot9b,width =10.5, height=2.55,units="in",bg="transparent")
  
  
  
  
  ####### Slide 10:  Sustainability 
  
  if (!is_ca){
    base <- data_school_c %>% select(dplyr::starts_with("Q30"))  %>% select(!dplyr::contains("Q30ca"))
    res <- data_school_c %>% select(dplyr::starts_with("Q30")) %>% select(!dplyr::contains("Q30_17_TEXT")) %>%
      select(!dplyr::contains("Q30_SDS")) 
  } else {
    base <- data_school_c %>% select(dplyr::starts_with("Q30ca"))
    res <- data_school_c %>% select(dplyr::starts_with("Q30ca")) %>% select(!dplyr::contains("TEXT")) %>%
      select(!dplyr::contains("Q30ca_SDS"))
  }
  
  
  # how many respondents made some selection for this
  base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
  
  
  res <- res %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q30")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/base_n)  %>% arrange(desc(n))%>%
    arrange(value %in% c("Other (please specify):")) %>% 
    arrange(value %in% c('None of the above'))
  
  #find_me
  
  
  res$value <- gsub(" \\(please specify\\):","",res$value)
  res$value <- gsub("\\(e.g., straws, water bottles, bags\\)","",res$value)
  res$value <- unlist(lapply(res$value,FUN=function(x){paste(strwrap(x,35),collapse="\n")}))
  
  
  res$label<- paste0(roundQual(res$n,0),"%")
  
  
  levels <-  res$value
  
  res$value <- factor(res$value, levels = rev(levels))
  
  plotSus<-
    ggplot(res,aes(x=value,y=n,label=label)) + 
    geom_bar(stat="identity",fill=two_tone[1],width = .8)+ 
    geom_text(aes(x=value,y=n + max(res$n)/8,label=label),size = 6, position = position_dodge(width = .9)) +
    coord_flip() + 
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$n+max(res$n)/7+15)), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.y=element_text(size=17),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.4,-0.03),
          legend.margin=margin(t = 0, unit='cm')) 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide10.png"),plot=plotSus,width = 8, height=9,units="in")
  
  
  ####### Slide 11a Mobile App Kiosk
  
  
  res <- data.frame(table(data_school_c$Q39)) %>% subset(Var1!="") 
  base <- sum(res$Freq)
  res$per <- roundQual(res$Freq*100/base)
  res <- res %>% 
    arrange(Var1=="No") %>% 
    mutate(label=paste0(Var1,": " ,per,"%"))
  
  # create labels
  #res$label_col <- ifelse(res$per<=4,'black', "white")
  res$label_col <- "black"
  res$hjust_var <- ifelse(res$per<=4,-3.6,.5)
  res$Var1 <- factor(res$Var1,levels=c("Yes","No"))
  
  
  
  df <- res %>% 
    mutate(end = 2 * pi * cumsum(per)/sum(per),
           start = lag(end, default = 0),
           middle = 0.5 * (start + end),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  
  
  hasMobile<-  ggplot(df) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                     start = start, end = end, fill = Var1)) +
    geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = label,
                  hjust = hjust, vjust = vjust), size=6) +
    scale_fill_manual(values=colors[c(5,3)],breaks=c("Yes","No")) + 
    coord_fixed() +
    scale_x_continuous(limits = c(-1.6, 1.6),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.6, 1.6),    # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    theme_minimal() + guides(fill="none")
  
  if (!is_ca){
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide11a.png"),plot=hasMobile,width = 3.7, height=3.7,units="in")
  } else{
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide11c_ca.png"),plot=hasMobile,width = 3.7, height=3.7,units="in")
  }
  
  
  ####### Slide 11b How Often Mobile
  
  ##### How often order from mobile kiosk
  
  
  oft <- data_school_c %>% filter(Q41 !="") %>% group_by(Q41) %>% summarize(n=n()) %>% 
    mutate(per=paste0(roundQual(100*n/sum(n)),"%")) %>%  complete(Q41=c("0 times / week",  "1-2 times / week", "3-5 times / week",
                                                                        "6-9 times / week", "10+ times / week"),fill=list(n=0,per="0%")) %>% 
    mutate(Q41 = factor(Q41,levels=c("0 times / week",  "1-2 times / week", "3-5 times / week",
                                     "6-9 times / week", "10+ times / week"))) %>%
    arrange(Q41)
  
  oft_out <- oft %>% select(Q41,per) %>% gt() %>%cols_label("Q41"="","per"="") %>%
    cols_align(
      align = "left",
      columns= c(1)
    )%>%
    cols_align(
      align = "right",
      columns= c(2)
    )  %>%  
    cols_width(
      c(2) ~ px(120),
      c(1) ~ px(300)
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = tab_col[1])
      ),
      locations = cells_body(
        rows = c(1,3,5))
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = tab_col[2])
      ),
      locations = cells_body(
        rows = c(2,4))
    ) 
  
  if (!is_ca){
    gtsave(oft_out,filename = paste0(loc,UNIVERSITY_NAME,"/","slide11b.png"))
  } else {
    gtsave(oft_out,filename = paste0(loc,UNIVERSITY_NAME,"/","slide10d_ca.png"))
  }
  
  
  
  
  ############### Slide 11 c
  # why not order at kiosk
  qs <- c('Q43_1','Q43_4', 'Q43_5', 'Q43_6' )
  
  base <- data_school_c %>% select(qs) 
  
  
  res_b <- data_school_c %>% select(qs,ResponseId) %>% 
    mutate(id=row.names(.)) %>% 
    select(!dplyr::contains("_SDS")) %>%
    pivot_longer(cols=dplyr::starts_with("Q43")) %>%
    filter(value!="") %>%
    mutate(value = trimws(value)) %>%
    select(-id) %>%
    summarise(n=length(unique(ResponseId)))
  
  
  res <- data_school_c %>% select(qs,ResponseId) %>% 
    mutate(id=row.names(.)) %>% 
    select(!dplyr::contains("_SDS")) %>%
    pivot_longer(cols=dplyr::starts_with("Q43")) %>%
    filter(value!="") %>%
    mutate(value = trimws(value)) %>%
    select(-id) %>%
    group_by(value) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(sum_t = sum(count)) 
  
  # add potentially missing rows
  all_lev <- data %>% select(qs) %>% 
    mutate(id=row.names(.)) %>% 
    select(!dplyr::contains("_SDS")) %>%
    pivot_longer(cols=dplyr::starts_with("Q43")) %>% 
    filter(value!="") %>%
    mutate(value = trimws(value)) %>%
    select(-id) %>%
    group_by(value) %>% summarize(tot=n()) %>% ungroup()
  
  res <- res %>%  right_join(all_lev,by="value")%>%
    mutate(n= roundQual(100*replace_na(count,0)/res_b$n))%>% 
    mutate(value=gsub(" \\(please specify\\):","",value),per=paste0(n,"%")) 
  
  
  
  
  
  not_out <- res %>% select(value,per) %>% gt() %>%cols_label("value"="","per"="") %>%
    cols_align(
      align = "left",
      columns= c(1)
    )%>%
    cols_align(
      align = "right",
      columns= c(2)
    )%>%  
    cols_width(
      c(2) ~ px(120),
      c(1) ~ px(300)
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = colors[3])
      ),
      locations = cells_body(
        rows = c(2))
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#EEEEEE")
      ),
      locations = cells_body(
        rows = c(1,3))
    ) 
  
  if (!is_ca){
    gtsave(not_out,filename = paste0(loc,UNIVERSITY_NAME,"/","slide11c.png"))
  } else{
    gtsave(not_out,filename = paste0(loc,UNIVERSITY_NAME,"/","slide10e_ca.png"))
  }
  
  

  
  ##### Slide 11 d : Preferred technologies
  
  base <- data_school_c %>% select(dplyr::starts_with("Q45")) 
  # how many respondents made some selection for this
  base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
  
  res_s <- data_school_c %>%
    select(dplyr::starts_with("Q45")) %>% select(!dplyr::contains("_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q45")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" (please specify):","",value,fixed=T)) %>% 
    group_by(value) %>% summarize(n=100*n()/base_n)  %>% arrange(desc(n)) %>% 
    arrange(value %in% c("None of the above","Other"))
  
  res_all <- data %>%
    select(dplyr::starts_with("Q45")) %>% select(!dplyr::contains("_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q45")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" (please specify):","",value,fixed=T)) %>% 
    group_by(value) %>% summarize(n_all=100*n()/base_n) 
  
  res <- merge(res_s,res_all,by="value",all=T)
  res$Percent <- ifelse(is.na(res$n),0,res$n)
  res$n <- ifelse(is.na(res$n),0,res$n)
  
  
  res$value <- unlist(lapply(res$value,FUN=function(x){paste(strwrap(x,30),collapse="\n")}))
  
  
  res$label<- paste0(roundQual(res$Percent,0),"%")
  
  res <- res %>% arrange(desc(n)) %>% arrange(value=="Other")  %>% arrange(value=="None of the above") 
  levels <-  res$value
  
  res$value <- factor(res$value, levels = rev(levels))
  
  plotTech<-
    ggplot(res,aes(x=value,y=n,label=label)) + 
    geom_bar(stat="identity",fill=two_tone[1],width = .8)+ 
    geom_text(aes(x=value,y=n + max(res$Percent)/6,label=label),size = 5, position = position_dodge(width = .9)) +
    coord_flip() + 
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent+max(res$Percent)/3+10)), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.y=element_text(size=12),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.4,-0.03),
          legend.margin=margin(t = 0, unit='cm')) 
  
  if (!is_ca){
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide11d.png"),plot=plotTech,width = 5, height=3.5,units="in")
  } else{
    ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide11b_ca.png"),plot=plotTech,width = 5, height=3.5,units="in")
    
  }
  
  
  
  ######################################################################################
  # bank of canada only visualizations
 
  if (is_ca){
    
    #######################################################
    ### SLide 4, satisfaction by category
    #######################################################
    questions_10 = c("Q10_1", "Q10_2", "Q10_3", "Q10_4", "Q10_5", "Q10_6", 
                  "Q10_8","Q10_9", "Q10_10", "Q10_12")
    q_names = c("Food quality", " Food variety", "Availability of \nnutrition information", 
                "Availability of \n ingredient information", "Healthy options", 
                " Price/Value", "Special dietary options", 
                " Freshness of food", " Affordability","Sustainable\n sourcing")
  

    
    for (i in 1:length(questions_10)){
      q <- questions_10[i]
      q_name <- q_names[i]
  
      sat <- data_grp_bar(q, stacked_labels=TRUE,comp=T)
      sat <- sat %>% filter(!Location%in%c("Region", "Nation"))
      names(sat)[6] <- 'Value'
      sat$cat <- q_name
      sat <- sat %>% select(c("n", "freq", "Location", "Percent", "Value", "label", 
                              "cat"))
    
      if (i==1){
        sat$Value <-  sat$Value[,1,drop=TRUE]
        final <- sat
      } else{
        
        names(sat) <- names(final)
        sat$Value <-  sat$Value[,1,drop=TRUE]
        final <- rbind(final, sat)
      }
     
    }
    
   
    

   
    final$Value <- as.factor(final$Value)
    final$label_col <- ifelse(final$Value %in% c("Excellent","Good"),"black","white")
    final$hjust_var <- ifelse(final$Percent<=4,-1,.5)
    #adust label color if it is off the side to be black
    final$label_col <- ifelse(final$Percent<=4,'black', final$label_col)
  
   
    final$Value <- factor(final$Value, levels <- c( "Excellent", "Good","Fair", "Poor", 
                                           "Terrible"))
  
  
    final$cat <- factor(final$cat,levels=q_names)
    
    sat_plot<- ggplot(final,aes(x=cat,y=Percent,fill=Value,label=label)) + 
      geom_bar(stat="identity",width=.5)+ 
      geom_text(aes(x=cat,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
      scale_x_discrete(position = "top", guide = guide_axis(angle = 45))  + 
      scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Fair","Poor","Terrible"))) +
      scale_color_manual(values=c("black","white"))+
      theme_minimal() + ylab("") + xlab("") + 
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank(), legend.title = element_blank(),
            axis.text.x=element_text(size=11),
            legend.text=element_text(size=11),
            # legend.position = "left", 
            plot.margin = margin(l=1.8, r=.8, unit="cm"),
            legend.position=c(-0.04,0.5),
            legend.margin=margin(t = 0, unit='cm')) + guides(color="none")
    
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide4_q10.png"),plot=sat_plot,width = 10, height=5,units="in")

  questions_11 <- c("Q11_1", "Q11_2", "Q11_3", 
                    "Q11_4", "Q11_5", "Q11_6", "Q11_7", "Q11_8", "Q11_9")
  q_names_11 <- c( " Convenience", "Welcoming/Friendly\n staff", " Knowledgeable/helpful\n staff", 
                   "Speed of service", "Cleanliness", "Hours of operation", "Place to socialize", 
                   "Comfortable dining\n experience", "Technology")
  
  
  
  for (i in 1:length(questions_11)){
    q <- questions_11[i]
    q_name <- q_names_11[i]
    
    sat <- data_grp_bar(q, stacked_labels=TRUE,comp=T)
    sat <- sat %>% filter(!Location%in%c("Region", "Nation"))
    names(sat)[6] <- 'Value'
    sat$cat <- q_name
    sat <- sat %>% select(c("n", "freq", "Location", "Percent", "Value", "label", 
                            "cat"))
    if (i==1){
      sat$Value <-  sat$Value[,1,drop=TRUE]
      final <- sat
    } else{
      names(sat) <- names(final)
      sat$Value <-  sat$Value[,1,drop=TRUE]
      final <- rbind(final, sat)
    }
    
  }
  
  
  
  
  
  final$Value <- as.factor(final$Value)
  final$label_col <- ifelse(final$Value %in% c("Excellent","Good"),"black","white")
  final$hjust_var <- ifelse(final$Percent<=4,-1,.5)
  #adust label color if it is off the side to be black
  final$label_col <- ifelse(final$Percent<=4,'black', final$label_col)
  
  
  final$Value <- factor(final$Value, levels <- c( "Excellent", "Good","Fair", "Poor", 
                                                  "Terrible"))
  
  
  final$cat <- factor(final$cat,levels=q_names_11)
  
  sat_plot<- ggplot(final,aes(x=cat,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text(aes(x=cat,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top", guide = guide_axis(angle = 45))  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Fair","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          axis.text.x=element_text(size=11),
          legend.text=element_text(size=11),
          # legend.position = "left", 
          plot.margin = margin(l=1.8, r=.8, unit="cm"),
          legend.position=c(-0.04,0.5),
          legend.margin=margin(t = 0, unit='cm')) + guides(color="none")
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide4_q11.png"),plot=sat_plot,width = 10, height=5,units="in")
  
  
  #######################################################
  ### SLide 5, Importance of various metrics
  #######################################################
  #TODO: test
  
  questions = c( "Q11A_CAN_1", "Q11A_CAN_2", "Q11A_CAN_3", 
                 "Q11A_CAN_4", "Q11A_CAN_5", "Q11A_CAN_6", "Q11A_CAN_7", "Q11A_CAN_8", "Q11A_CAN_9","Q11A_CAN_10")
  q_names = c( 'Food quality',
               'Food variety',
               'Price/Value',
               'Menu options that meet my dietary needs',
               'Convenience',
               'Cleanliness',
               'Speed of service',
               'Comfortable dining experience',
               'Knowledgeable/Helpful staff',
               'Technology to support dining program')
  
  vals <- c()
  for (q in questions){
    vals = c(vals,top_two(q,data_school_c))
  }
  
  data_7 <- data.frame(cbind(q_names, vals))
  data_7$vals <- as.numeric(as.character(data_7$vals))
  data_7 <- data_7 %>% arrange(desc(vals))
  data_7$pct <- paste0(as.character(data_7$vals), '%')
  
  
  data_7$q_names <- unlist(lapply(data_7$q_names,FUN=function(x){paste(strwrap(x,45),collapse="\n")}))
  
  
  levels <-  data_7$q_names
  
  data_7$q_names<- factor(data_7$q_names, levels = rev(levels))
  
  
  
  plot7<-
    ggplot(data_7,aes(x=q_names,y=vals,label=pct),fill=three_tone[2]) + 
    geom_bar(stat="identity",position="dodge",fill=two_tone[1])+ 
    geom_text(aes(x=q_names,y=vals + max(data_7$vals)/15,label=pct),size = 3, position = position_dodge(width = .9)) +
    coord_flip() + 
    scale_fill_manual(values=three_tone[2]) +
    #scale_fill_manual(breaks = c("Region","Nation",UNIVERSITY_NAME),values=rev(three_tone),labels=c("   Region   ","   Nation  ",paste(" ",UNIVERSITY_NAME,"  "))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(data_7$vals)+8), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.3,-0.03),
          legend.margin=margin(t = 0, unit='cm'))  + 
    guides(fill = "none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide5_ca.png"),plot=plot7,width = 6, height=4.5,units="in")
  
  #######################################################
  ### Slide 6:  Outlets
  #######################################################
  #frequency
  res <- data_school_c %>% select(Q12)%>%
    group_by(Q12) %>% summarize(n=100*n()/nrow(data_school_c)) %>% arrange(desc(n)) 
  
  res <- data_grp_bar('Q12',comp=T)
  res <-res %>% filter(Location==UNIVERSITY_NAME) %>% arrange(desc(n)) 
  
  res$Q12<- gsub(" (please specify):","",res$Q12,fixed=T)
  levels = res$Q12
  res$Value <- factor(res$Q12, levels = rev(levels))
  
  
  plot<-
    ggplot(res,aes(x=Value,y=Percent,label=label)) + 
    geom_bar(stat="identity",fill=two_tone[1],width = .8)+ 
    geom_text(aes(x=Value,y=Percent + max(res$Percent)/7,label=label),size = 4, position = position_dodge(width = .9)) +
    coord_flip() + 
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent)+max(res$Percent)/7+10), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.4,-0.03),
          legend.margin=margin(t = 0, unit='cm')) 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide6a_ca.png"),plot=plot,width = 4, height=4,units="in")
  
  
  #######################################################
  ### Slide 7:  Dietary Pie
  #######################################################
  n_diet <-nrow(data_school_c[data_school_c$Q23_2=="Special dietary lifestyle for religious reasons"|data_school_c$Q23_3=="Special dietary lifestyle for medical reasons"|data_school_c$Q23_4=="Special dietary lifestyle for personal reasons",])
  p_diet <- roundQual(n_diet/nrow(data_school_c)*100,0)
  
  diet_df <- data.frame(c('Yes', 'No'), c(p_diet, 100-p_diet))
  names(diet_df) <- c('Val', 'per')
  
  diet_df <- diet_df %>% 
    mutate(label=paste0(Val,": " ,per,"%"))
  
  res <- data.frame(table(data_school_c$Q39)) %>% subset(Var1!="") 
  base <- sum(res$Freq)
  res$per <- roundQual(res$Freq*100/base)
  res <- res %>% 
    arrange(Var1=="No") %>% 
    mutate(label=paste0(Var1,": " ,per,"%"))
  
  # create labels
  diet_df$label_col <- "black"
  diet_df$hjust_var <- ifelse(diet_df$per<=4,-3.6,.5)
  diet_df$Val <- factor(diet_df$Val,levels=c("Yes","No"))
  
  
  
  df <- diet_df %>% 
    mutate(end = 2 * pi * cumsum(per)/sum(per),
           start = lag(end, default = 0),
           middle = 0.5 * (start + end),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  
  
  hasDiet<-  ggplot(df) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                     start = start, end = end, fill = Val)) +
    geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = label,
                  hjust = hjust, vjust = vjust)) +
    scale_fill_manual(values=colors[c(5,3)],breaks=c("No","Yes")) + 
    coord_fixed() +
    scale_x_continuous(limits = c(-1.6, 1.6),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.6, 1.6),    # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    theme_minimal() + guides(fill="none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide7a_ca.png"),plot=hasDiet,width = 3.7, height=3.7,units="in")
  
  #######################################################
  ### Slide 8:  More about diet
  #######################################################
  #Q27 Ease of meeting dietary restrictions
  sat <- data_grp_bar('Q27', stacked_labels=TRUE,comp=T)
  sat <- sat %>% filter(!Location%in%c("Region", "Nation"))
  #sat$year <- this_year
  #sat$top2 <- paste0("\nTop Two Box: ",top_two("Q5",data_school_c),"%")
  #sat$year <- paste0(sat$year,sat$top2)
  sat$hjust_var <- ifelse(sat$Percent<=4,-6.2,.5) 
  
  sat$Value <- as.factor(sat$Q27)
  sat$label_col <- ifelse(sat$Value %in% c("Extremely easy","Somewhat easy"),"black","white")
  
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  
  sat$Value <- factor(sat$Q27, levels <- c("Extremely easy", "Somewhat easy", "Neither easy nor difficult", 
                                           "Somewhat difficult","Extremely difficult"))
  
  sat$year <- this_year
  
  sat_plot<- ggplot(sat,aes(x=year,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text(aes(x=year,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Extremely easy", "Somewhat easy", "Neither", 
                                                       "Somewhat difficult","Extremely difficult"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          axis.text.x=element_blank(),
          legend.text=element_text(size=13),
          # legend.position = "left", 
          plot.margin = margin(l=2.7, unit="cm"),
          legend.position=c(-0.15,0.5),
          legend.margin=margin(t = 0, unit='cm')) + guides(color="none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide8a_ca.png"),plot=sat_plot,width = 3.7, height=3.7,units="in")
  
  #Q27b Ease of answering questions w/r/t dietary restrictions
  sat <- data_grp_bar('Q27a', stacked_labels=TRUE,comp=T)
  sat <- sat %>% filter(!Location%in%c("Region", "Nation"))
  #sat$year <- this_year
  #sat$top2 <- paste0("\nTop Two Box: ",top_two("Q5",data_school_c),"%")
  #sat$year <- paste0(sat$year,sat$top2)
  sat$hjust_var <- ifelse(sat$Percent<=4,-6.2,.5) 
  
  sat$Value <- as.factor(sat$Q27a)
  sat$label_col <- ifelse(sat$Value %in% c("Extremely easy","Somewhat easy"),"black","white")
  
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  
  sat$Value <- factor(sat$Q27a, levels <- c("Extremely easy", "Somewhat easy", "Neither easy nor difficult", 
                                           "Somewhat difficult","Extremely difficult"))
  
  sat$year <- this_year
  
  sat_plot<- ggplot(sat,aes(x=year,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text(aes(x=year,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Extremely easy", "Somewhat easy", "Neither", 
                                                       "Somewhat difficult","Extremely difficult"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          axis.text.x=element_blank(),
          legend.text=element_text(size=13),
          # legend.position = "left", 
          plot.margin = margin(l=2.7, unit="cm"),
          legend.position=c(-0.15,0.5),
          legend.margin=margin(t = 0, unit='cm')) + guides(color="none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide8b_ca.png"),plot=sat_plot,width = 3.7, height=3.7,units="in")
  
  #### Plans for next year
  
  #TODO: test with actually CA data
  res <- data_school_c %>%
    select(dplyr::starts_with("Q29_CAN")) %>% select(!dplyr::contains("Q29_CAN_6_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q29_CAN")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" \\(please specify)","",value)) %>% 
    group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n))
  
  res$Percent <- ifelse(is.na(res$n),0,res$n)
  
  
  res$value <- unlist(lapply(res$value,FUN=function(x){paste(strwrap(x,45),collapse="\n")}))
  
  
  res$label<- paste0(roundQual(res$n,0),"%")
  
  res <- res %>% arrange(desc(n)) %>% arrange(value=="Other:")%>% arrange(value=="None of these")
  levels <-  res$value
  
  res$value <- factor(res$value, levels = rev(levels))
  
  plotIntent<-
    ggplot(res,aes(x=value,y=n,label=label)) + 
    geom_bar(stat="identity",fill=two_tone[1],width = .8)+ 
    geom_text(aes(x=value,y=n + max(res$n)/12,label=label),size = 3, position = position_dodge(width = .9)) +
    coord_flip() + 
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$n+max(res$n)/12+10)), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.4,-0.03),
          legend.margin=margin(t = 0, unit='cm')) 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide8c_ca.png"),plot=plotIntent,width = 5, height=5,units="in")
  
  ##################################################
  ## Slide 9: Healthy Priorities
  ##################################################
  base <- data_school_c %>% select(dplyr::starts_with("Q31")) 
  
  # how many respondents made some selection for this
  base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
  
  res <- data_school_c %>% select(dplyr::starts_with("Q31")) %>% select(!dplyr::contains("TEXT")) %>%
    mutate(id=row.names(.)) %>% 
    pivot_longer(cols=dplyr::starts_with("Q31")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/base_n)  %>% arrange(desc(n))
  
  res$value <- gsub(" \\(please specify\\)","",res$value)
  
  res$value <- unlist(lapply(res$value,FUN=function(x){paste(strwrap(x,25),collapse="\n")}))
  
  
  res$label<- paste0(roundQual(res$n,0),"%")
  
  
  levels <-  res$value
  
  res$value <- factor(res$value, levels = levels)
  res$Percent <- res$n
  
  plotPrior<-ggplot(res,aes(x=value,y=Percent,label=label)) + 
    geom_bar(stat="identity",position="dodge",fill=two_tone[1],width=.5)+ 
    geom_text(aes(x=value,y=Percent + max(Percent)/15,label=label),size = 3, position = position_dodge(width = .9)) +
    #scale_fill_manual(breaks = rev(c("Region","Nation",UNIVERSITY_NAME)),values=three_tone,labels=rev(c("  Region  ","   Nation  ",paste(" ",UNIVERSITY_NAME,"  ")))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent)+max(res$Percent)/7), expand=c(0,0)) +
    scale_x_discrete(expand=c(.1,0)) +
    theme(panel.grid = element_blank(),
          axis.line.x =  element_line(color = "grey"),
          #plot.margin(l = 2, unit = "cms"),
          axis.text.x = element_text(angle=45,hjust=1),
          axis.text.y = element_blank(), legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          #legend.position = "bottom", 
          #  legend.position=c(0.5,slide17_leg),
          legend.margin=margin(t = 0.1,b=0,l=.1, unit='cm'))  + 
    guides(fill = "none")
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide9_ca.png"),plot=plotPrior,width = 8.5, height=4,units="in")
  
  ########################################################################
  # Slide 10: Additional Mobile-related graphs
  ########################################################################
  
  #campus has mobile kiosk
  
  res <- data.frame(table(data_school_c$Q39)) %>% subset(Var1!="") 
  base <- sum(res$Freq)
  res$per <- roundQual(res$Freq*100/base)
  res <- res %>% 
    arrange(Var1=="No") %>% 
    mutate(label=paste0(Var1,": " ,per,"%"))
  
  # create labels
  # res$label_col <- ifelse(res$per<=4,'black', "white")
  res$label_col <-"black"
  res$hjust_var <- ifelse(res$per<=4,-2.5,.5)
  res$Var1 <- factor(res$Var1,levels=c("Yes","No"))
  
  
  df <- res %>% 
    mutate(end = 2 * pi * cumsum(per)/sum(per),
           start = lag(end, default = 0),
           middle = 0.5 * (end+start),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  
  df$label_x <-  1.05 * sin(df$middle)
  df$label_y <- 1.05 * cos(df$middle)
  
  hasKiosk<-  ggplot(df) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                     start = start, end = end, fill = Var1)) +
    geom_text(aes(x =label_x, y = label_y, label = label,
                  hjust = hjust, vjust = vjust)) +
    scale_fill_manual(values=colors[c(5,3)],breaks=c("Yes","No")) + 
    coord_fixed() +
    scale_x_continuous(limits = c(-1.6, 1.6),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.5, 1.5),    # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    theme_minimal() + guides(fill="none") 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide10a_ca.png"),plot=hasKiosk,width = 3.5, height=3.5,units="in")
  
  #would order from mobile if existed
  
  no_mobile <- data_school_c %>% filter(data_school_c$Q39=='No')
  
  res <- data.frame(table(no_mobile$Q42)) %>% subset(Var1!="") 
  base <- sum(res$Freq)
  res$per <- roundQual(res$Freq*100/base)
  res <- res %>% 
    arrange(Var1=="No") %>% 
    mutate(label=paste0(Var1,": " ,per,"%"))
  
  # create labels
  # res$label_col <- ifelse(res$per<=4,'black', "white")
  res$label_col <-"black"
  res$hjust_var <- ifelse(res$per<=4,-2.5,.5)
  res$Var1 <- factor(res$Var1,levels=c("Yes","No"))
  
  
  df <- res %>% 
    mutate(end = 2 * pi * cumsum(per)/sum(per),
           start = lag(end, default = 0),
           middle = 0.5 * (end+start),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  
  df$label_x <-  1.05 * sin(df$middle)
  df$label_y <- 1.05 * cos(df$middle)
  
  wouldOrder<-  ggplot(df) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                     start = start, end = end, fill = Var1)) +
    geom_text(aes(x =label_x, y = label_y, label = label,
                  hjust = hjust, vjust = vjust)) +
    scale_fill_manual(values=colors[c(5,3)],breaks=c("Yes","No")) + 
    coord_fixed() +
    scale_x_continuous(limits = c(-1.6, 1.6),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.5, 1.5),    # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    theme_minimal() + guides(fill="none") 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide10b_ca.png"),plot=wouldOrder,width = 3.5, height=3.5,units="in")
  
  ########################################################################
  # Slide 11: Additional Tech Questions
  ########################################################################
  
  
  #Q36 Eas of ordering
  sat <- data_grp_bar('Q38', stacked_labels=TRUE,comp=T)
  sat <- sat %>% filter(!Location%in%c("Region", "Nation"))
  sat$hjust_var <- ifelse(sat$Percent<=4,-2.2,.5) 
  
  sat$Value <- as.factor(sat$Q38)
  sat$label_col <- ifelse(sat$Value %in% c("Extremely easy","Somewhat easy"),"black","white")
  
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  
  sat$Value <- factor(sat$Q38, levels <- c("Extremely easy", "Somewhat easy", "Neither easy nor difficult", 
                                           "Somewhat difficult","Extremely difficult"))
  
  sat$year <- this_year
  
  
  sat_plot<- ggplot(sat,aes(x=year,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text(aes(x=year,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Extremely easy", "Somewhat easy", "Neither", 
                                                       "Somewhat difficult","Extremely difficult"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          axis.text.x=element_blank(),
          legend.text=element_text(size=13),
          # legend.position = "left", 
          plot.margin = margin(l=2.7, unit="cm"),
          legend.position=c(-0.15,0.5),
          legend.margin=margin(t = 0, unit='cm')) + guides(color="none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide11a_ca.png"),plot=sat_plot,width = 3.7, height=3.7,units="in")
  
  
  #would like to order from mobile
  
  res <- data.frame(table(data_school_c$Q44)) %>% subset(Var1!="") 
  base <- sum(res$Freq)
  res$per <- roundQual(res$Freq*100/base)
  res <- res %>% 
    arrange(Var1=="No") %>% 
    mutate(label=paste0(Var1,": " ,per,"%"))
  
  # create labels
  # res$label_col <- ifelse(res$per<=4,'black', "white")
  res$label_col <-"black"
  res$hjust_var <- ifelse(res$per<=4,-2.5,.5)
  res$Var1 <- factor(res$Var1,levels=c("Yes","No"))
  
  
  df <- res %>% 
    mutate(end = 2 * pi * cumsum(per)/sum(per),
           start = lag(end, default = 0),
           middle = 0.5 * (end+start),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  
  df$label_x <-  1.05 * sin(df$middle)
  df$label_y <- 1.05 * cos(df$middle)
  
  wouldLike<-  ggplot(df) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                     start = start, end = end, fill = Var1)) +
    geom_text(aes(x =label_x, y = label_y, label = label,
                  hjust = hjust, vjust = vjust)) +
    scale_fill_manual(values=colors[c(5,3)],breaks=c("Yes","No")) + 
    coord_fixed() +
    scale_x_continuous(limits = c(-1.6, 1.6),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.5, 1.5),    # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    theme_minimal() + guides(fill="none") 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide11c_ca.png"),plot=wouldLike,width = 3.5, height=3.5,units="in")
  
  
  }
} 





#for working
UNIVERSITY_NAME <-  "University of Virginia"
#always Fall 2022 now
data_last <- last_data_fall_22
last_year ="Fall 2022"


get_graphs(data_us, data_last, 'University of Virginia', last_year ="Fall 2022")

#make sure when we apply this we are correctly supplying the last year's data only for the correct country

ca_data<- read.csv("~/Consulting/Qualtrics/Aramark2023/Aramark2023/Fall2023_ca.csv",stringsAsFactors = F)
ca_data<- ca_data[ca_data$Status=="IP Address",] # get rid of extra tester rows
ca_data$SCHOOL_USA <- ca_data$SCHOOL_NAME

# handle renaming ca values

ca_data[,gsub("Q30_","Q30ca",names(ca_data)[grep("Q30_",names(ca_data))])] <- ca_data[,grep("Q30_",names(ca_data))]

get_graphs(ca_data, data_last, 'University of Toronto Scarborough',is_ca=T)

####################################################
##################### Fall 2023 ####################
####################################################
colors <- c("#FFC72C","#9ACAEB","#999999","black","#EA002A")
colors_stacked <- rev(c("#FFC72C","#9ACAEB","#999999","black","#EA002A"))
two_tone <- c("#EA0022","#ff91a3")
three_tone <- colors[c(5,3,4)]
three_tone_dot <- c("#EA0022","#7f7f7f","black")
four_tone <- colors[c(5,3,2,1)]
tab_col <- c("#faccd4","#f47f94")

setwd("~/Consulting/Qualtrics/Aramark2023/Aramark2023")
#change is_ca flag to true for canadian schools
data_ca <- data[data$Q0!="",]
data_us <- data[data$Q0=="USA",]

get_graphs(data_us,last_data, "James Madison University",last_year="Spring 2023", is_ca=F)


get_graphs(data_ca,last_data, "Memorial University of Newfoundland",last_year="Spring 2023", is_ca=T)


####################################################
##################### Spring 2023 ####################
####################################################
colors <- c("#FFC72C","#9ACAEB","#999999","black","#EA002A")
colors_stacked <- rev(c("#FFC72C","#9ACAEB","#999999","black","#EA002A"))
two_tone <- c("#EA0022","#ff91a3")
three_tone <- colors[c(5,3,4)]
three_tone_dot <- c("#EA0022","#7f7f7f","black")
four_tone <- colors[c(5,3,2,1)]
tab_col <- c("#faccd4","#f47f94")

#change is_ca flag to true for canadian schools
data_ca <- data[data$SCHOOL_CAN=="Canada",]
data_us <- data[data$COUNTRY=="USA",]

# identify harvest
harvest <- unique(data_us$SCHOOL_USA[data_us$REGION_NAME=="Harvest Table Region"])

uni_ca <- unique(data_ca$SCHOOL_USA)
data_ca$REGION_NAME <- ifelse(data_ca$REGION_NAME=="CENTRAL","Central Region",ifelse(data_ca$REGION_NAME=="EAST","East Region",data_ca$REGION_NAME))

for (uni in uni_ca){
  print(uni)
  
  withCallingHandlers({
    get_graphs(data_ca,last_data,uni,last_year ="Fall 2021",is_ca=T)
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
}




## US graphs with fall 2022 as the last (most common) - 18 out of 26 
uni_fall22 <- all_reps[all_reps$last_year=="Fall 2022","SCHOOL_NAME",FALSE]


for (uni in uni_fall22$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    if (uni%in% harvest){
      colors <- c('#d1ddbf', '#b9cc98', '#9cba5f', '#8aa453', '#748b45')
      colors_stacked <- rev(colors)
      two_tone <- c('#7f9a48', '#9bbb59')
      three_tone <- c('#7f9a48', '#9bbb59', '#c6d6ac')
      three_tone_dot <- c('#7f9a48', '#9bbb59', '#b9cc98')
      four_tone <- c("#EA0022","#7f7f7f","black", "#ff91a3")
      
      tab_col <- c("#7f9a48","#9bbb59")
    } else{
      colors <- c("#FFC72C","#9ACAEB","#999999","black","#EA002A")
      colors_stacked <- rev(c("#FFC72C","#9ACAEB","#999999","black","#EA002A"))
      two_tone <- c("#EA0022","#ff91a3")
      three_tone <- colors[c(5,3,4)]
      three_tone_dot <- c("#EA0022","#7f7f7f","black")
      four_tone <- colors[c(5,3,2,1)]
      tab_col <- c("#faccd4","#f47f94")
    }
    get_graphs(data_us,last_data, uni,last_year="Fall 2022", is_ca=F)
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}

# Run other years 
uni_spring22 <- all_reps[all_reps$last_year=="Spring 2022","SCHOOL_NAME",FALSE]


for (uni in uni_spring22$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    if (uni%in% harvest){
      colors <- c('#d1ddbf', '#b9cc98', '#9cba5f', '#8aa453', '#748b45')
      colors_stacked <- rev(colors)
      two_tone <- c('#7f9a48', '#9bbb59')
      three_tone <- c('#7f9a48', '#9bbb59', '#c6d6ac')
      three_tone_dot <- c('#7f9a48', '#9bbb59', '#b9cc98')
      four_tone <- c("#EA0022","#7f7f7f","black", "#ff91a3")
      
      tab_col <- c("#7f9a48","#9bbb59")
    } else{
      colors <- c("#FFC72C","#9ACAEB","#999999","black","#EA002A")
      colors_stacked <- rev(c("#FFC72C","#9ACAEB","#999999","black","#EA002A"))
      two_tone <- c("#EA0022","#ff91a3")
      three_tone <- colors[c(5,3,4)]
      three_tone_dot <- c("#EA0022","#7f7f7f","black")
      four_tone <- colors[c(5,3,2,1)]
      tab_col <- c("#faccd4","#f47f94")
    }
    get_graphs(data_us,last_data_spring_22, uni,last_year="Spring 2022", is_ca=F)
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}

# Run other years fall 2021 or blank (which will then be handled internall)
uni_fall21 <- all_reps[all_reps$last_year%in%c("Fall 2021",""),"SCHOOL_NAME",FALSE]


for (uni in uni_fall21$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    if (uni%in% harvest){
      colors <- c('#d1ddbf', '#b9cc98', '#9cba5f', '#8aa453', '#748b45')
      colors_stacked <- rev(colors)
      two_tone <- c('#7f9a48', '#9bbb59')
      three_tone <- c('#7f9a48', '#9bbb59', '#c6d6ac')
      three_tone_dot <- c('#7f9a48', '#9bbb59', '#b9cc98')
      four_tone <- c("#EA0022","#7f7f7f","black", "#ff91a3")
      
      tab_col <- c("#7f9a48","#9bbb59")
    } else{
      colors <- c("#FFC72C","#9ACAEB","#999999","black","#EA002A")
      colors_stacked <- rev(c("#FFC72C","#9ACAEB","#999999","black","#EA002A"))
      two_tone <- c("#EA0022","#ff91a3")
      three_tone <- colors[c(5,3,4)]
      three_tone_dot <- c("#EA0022","#7f7f7f","black")
      four_tone <- colors[c(5,3,2,1)]
      tab_col <- c("#faccd4","#f47f94")
    }
    get_graphs(data_us,last_data21, uni,last_year="Fall 2021", is_ca=F)
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}


# Harvest

colors <- c('#d1ddbf', '#b9cc98', '#9cba5f', '#8aa453', '#748b45')
colors_stacked <- rev(colors)
two_tone <- c('#7f9a48', '#9bbb59')
three_tone <- c('#7f9a48', '#9bbb59', '#c6d6ac')
three_tone_dot <- c('#7f9a48', '#9bbb59', '#b9cc98')
four_tone <- c("#EA0022","#7f7f7f","black", "#ff91a3")

tab_col <- c("#7f9a48","#9bbb59")
# Identify last year 
get_graphs(data_us,last_data, "Wake Forest University",last_year="Fall 2022",is_ca=F,is_harvest=T)

####################################################
##################### Fall 2022 ####################
####################################################
# Tester
colors <- c("#FFC72C","#9ACAEB","#999999","black","#EA002A")
colors_stacked <- rev(c("#FFC72C","#9ACAEB","#999999","black","#EA002A"))
two_tone <- c("#EA0022","#ff91a3")
three_tone <- colors[c(5,3,4)]
three_tone_dot <- c("#EA0022","#7f7f7f","black")
four_tone <- colors[c(5,3,2,1)]
tab_col <- c("#faccd4","#f47f94")

#change is_ca flag to true for canadian schools
get_graphs(data,last_data, "University of Virginia",last_year="Fall 2022", is_ca=F)

# Harvest graphs
#h <- c("Springfield College","High Point University","Wake Forest University","Elon University")


# set harvest colors 
colors <- c('#d1ddbf', '#b9cc98', '#9cba5f', '#8aa453', '#748b45')
colors_stacked <- rev(colors)
two_tone <- c('#7f9a48', '#9bbb59')
three_tone <- c('#7f9a48', '#9bbb59', '#c6d6ac')
three_tone_dot <- c('#7f9a48', '#9bbb59', '#b9cc98')
four_tone <- c("#EA0022","#7f7f7f","black", "#ff91a3")

tab_col <- c("#7f9a48","#9bbb59")
get_graphs(data,last_data, "Elon University",last_year="Spring 2022",is_harvest=T)




#################################################################
############################### Old #############################
#################################################################

######## run reports spring 2022


uni_spring21 <- all_reps[all_reps$last_year=="Spring 2021","SCHOOL_NAME",FALSE]


for (uni in uni_spring21$SCHOOL_NAME){
  print(uni)

  withCallingHandlers({
    get_graphs(data,last_data_spring21,uni,last_year ="Spring 2021")
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}

## all other graphs have fall 2021 or nothing
uni_fall21 <- all_reps[all_reps$last_year=="Fall 2021","SCHOOL_NAME",FALSE]


for (uni in uni_fall21$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    get_graphs(data,last_data,uni,last_year ="Fall 2021")
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}

uni_fall20 <- all_reps[all_reps$last_year=="Fall 2020","SCHOOL_NAME",FALSE]
for (uni in uni_fall20$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    get_graphs(data,last_data_fall20,uni,last_year ="Fall 2020")
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}


# no match - feed last years data adn it will be handled internally
uni_none <- all_reps[all_reps$last_year=="","SCHOOL_NAME",FALSE]
for (uni in uni_none$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    get_graphs(data,last_data,uni,last_year ="")
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}

######## run reports sprinfield 



h <- c("Springfield College","High Point University","Wake Forest University","Elon University")
all_reps_h <- all_reps[all_reps$SCHOOL_NAME%in%h,]

# set harvest colors 
colors <- c('#d1ddbf', '#b9cc98', '#9cba5f', '#8aa453', '#748b45')

two_tone <- c('#7f9a48', '#9bbb59')
three_tone <- c('#7f9a48', '#9bbb59', '#c6d6ac')
three_tone_dot <- c('#7f9a48', '#9bbb59', '#b9cc98')
four_tone <- c("#EA0022","#7f7f7f","black", "#ff91a3")



uni_spring21_h <- all_reps_h[all_reps_h$last_year=="Spring 2021","SCHOOL_NAME",FALSE]


for (uni in uni_spring21_h$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    get_graphs(data,last_data_spring21,uni,last_year ="Spring 2021")
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}


## all other graphs have fall 2021 
uni_fall21_h <- all_reps_h[all_reps_h$last_year=="Fall 2021","SCHOOL_NAME",FALSE]


for (uni in uni_fall21_h$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    get_graphs(data,last_data,uni,last_year ="Fall 2021")
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}



