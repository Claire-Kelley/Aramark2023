library(ggplot2)
#library(magrittr)
library(ggforce)
library(dplyr)
library(tidyr)
library(grid)
library(circlize)
library(ggrepel)
library(gt)
library(wordcloud)
library(wordcloud2)
library(htmlwidgets) 
#install.packages("webshot")
#webshot::install_phantomjs()



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
#last_path <- '/Users/sarahkelley/Documents/Consulting/Kelley&Kelley/Aramark_Spring_2023/AramarkFall2022/data_files'
### 
last_data <- read.csv(paste0(last_path,"/spring2023_final.csv"),stringsAsFactors=FALSE)
last_data <- last_data[3:nrow(last_data),]

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

data<- read.csv("C:/Users/Claire/Documents/Consulting/Qualtrics/Aramark2023/Aramark2023/Fall2023.csv",stringsAsFactors=FALSE)
data <- data[3:nrow(data),]

# merge in names
outlets <- read.csv("C:/Users/Claire/Documents/Consulting/Qualtrics/Aramark2023/Aramark2023/Spring2023_outlets.csv")


# NOTE THIS ONLY WORKS for all USA 
data$SCHOOL_USA <- data$SCHOOL_NAME  


# NOTE THIS ONLY WORKS for all USA 
data$SCHOOL_USA <- data$SCHOOL_NAME  




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


#for working
UNIVERSITY_NAME <-  "James Madison University"
data_last <- last_data
last_year ="Spring 2023"

## Fix names 
data$SCHOOL_NAME <- ifelse(data$Q0=="USA",data$Q1_USA, data$Q1_CAN)

get_graphs <- function(data,data_last,UNIVERSITY_NAME,slide17_leg=-.4,slide23_leg=-.5,slide32_leg = .19,last_year ="Fall 2022", is_ca=F,is_harvest=F){

  ## SET up parameters 
  UNIVERSITY_NAME_WRAP <- paste(strwrap(UNIVERSITY_NAME,13),collapse = "\n")
  #use this for small graphs_fall_2022/ that don't want wrapped name for shorted universities
  UNIVERSITY_NAME_SHORT <- ifelse(nchar(UNIVERSITY_NAME) >= 23, paste(strwrap(UNIVERSITY_NAME,23),collapse = "\n  "), UNIVERSITY_NAME)
  UNIVERSITY_NAME_MED <- ifelse(nchar(UNIVERSITY_NAME) >= 30, paste(strwrap(UNIVERSITY_NAME,30),collapse = "\n  "), UNIVERSITY_NAME)
  
  # set up years
  this_year <- "Fall 2023"
  
  # make directory for graphs_spring_2023/
  loc <- "graphs_fall_2023/"
  dir.create(paste0(loc,UNIVERSITY_NAME))
  
  # make student only data set 
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
  region <- data %>% filter(SCHOOL_NAME== UNIVERSITY_NAME) %>% 
    select(REGION_NAME) %>% slice(1)
  REGION <- region[[1]]

  data_region_c <- data_c[data_c$REGION_NAME==REGION,]
  data_region <- data_region_c %>% filter(grepl("student|Other",Q2))
  
  #last time east region was just EAST
  if (REGION == 'East Region'){
    data_region_c_last <- filter(data_c_last, REGION_NAME%in%c("East","East Region")) # in some years east, some east region
    data_region_last <- data_region_c_last %>% filter(grepl("student|Other",Q1.1))
  } else{
    
    data_region_c_last <- filter(data_c_last, REGION_NAME==REGION)
    data_region_last <- data_region_c_last %>% filter(grepl("student|Other",Q1.1))
    
  }

  #get market segment
  market_seg <- data %>% filter(SCHOOL_NAME== UNIVERSITY_NAME) %>% 
    select(MARKET_SEGMENT) %>% slice(1)
  MARKET_SEG <- market_seg[[1]]
  data_market_c <- filter(data_c, MARKET_SEGMENT==MARKET_SEG)
  data_market <- data_market_c %>% filter(grepl("student|Other",Q2))
  
  
  
  ############## Data Prep Functions
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
    
    if (filter_enr==T){ # note this implies comp=T
      if (is_ca== F){
        data <- data_c %>% filter(Q1.8=="Yes")
        data_school <- data_school_c %>% filter(Q1.8=="Yes")
        data_region <- data_region_c %>% filter(Q1.8=="Yes")
      } else {
        data <- data_c %>% filter(Q1.8ca=="Yes")
        data_school <- data_school_c %>% filter(Q1.8ca=="Yes")
        data_region <- data_region_c %>% filter(Q1.8ca=="Yes")
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
      mutate(freq = n / sum(n))
    
    data_temp_school <- data_school %>%
      group_by_(col) %>%
      summarise(n = n()) %>%
      subset(.[,1]!="") %>%
      mutate(freq = n / sum(n))
    
    data_temp_region <- data_region %>%
      group_by_(col) %>%
      summarise(n = n()) %>%
      subset(.[,1]!="") %>%
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
  
  ############ NEW GRAPHS 
  
  ################## Slide 1 ##############
  
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
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","location_1a.png"),plot=plot21b,width = 4, height=4,units="in")
  
  # graph 2 - location pie chart
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
  
  # Make the plot
  pie <- ggplot(g2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Q3)) +
    geom_rect() +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
    scale_fill_manual(values=three_tone)+
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(legend.position = "none")
  
  ################## Slide 2 ##############
  
  ##################
  
} 




get_graphs_old <- function(){
  
  ####### Slide  OVERALL EXPERIENCE SLide 6 
  ## These are just students
  
  top_two <- function(x,dataset){
    roundQual(100*sum(grepl("excellent|good|^agree|strongly agree|definitely will buy|probably will buy",tolower(dataset[,x])))/sum(!is.na(dataset[,x]) & (dataset[,x]!="")),0)
  }


  if (HAS_LAST){
    sat <- data_grp_bar('Q2.1', stacked_labels=TRUE,comp=T)
    sat_this <- sat %>% filter(!Location%in%c("Region", "Nation"))
    sat_this$top2 <- paste0("\nTop Two Box: ",top_two("Q2.1",data_school_c),"%")
    sat_this$year <- this_year
    sat_this$year <- paste0(sat_this$year,sat_this$top2)
    
    sat_last <- data_grp_bar('Q2.1', stacked_labels=TRUE,uselast = T) %>% filter(!Location%in%c("Region", "Nation"))
    sat_last$year <- last_year
    sat_last$top2 <- paste0("\nTop Two Box: ",top_two("Q2.1",data_school_c_last),"%")
    sat_last$year <- paste0(sat_last$year,sat_last$top2)
    
    sat <- bind_rows(sat_this,sat_last)
    sat$hjust_var <- ifelse(sat$Percent<=4,-3.6,.5)
    
    
    years <- c(unique(sat_last$year),unique(sat_this$year))
    
  } else{
    sat <- data_grp_bar('Q2.1', stacked_labels=TRUE,comp=T)
    sat <- sat %>% filter(!Location%in%c("Region", "Nation"))
    sat$year <- this_year
    sat$top2 <- paste0("\nTop Two Box: ",top_two("Q2.1",data_school_c),"%")
    sat$year <- paste0(sat$year,sat$top2)
    sat$hjust_var <- ifelse(sat$Percent<=4,-6.2,.5) 
    years <- unique(sat$year)
  }
  
  sat$Value <- as.factor(sat$Q2.1)
  sat$label_col <- ifelse(sat$Value %in% c("Excellent","Good"),"black","white")

  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  
  
  
  sat$Value <- factor(sat$Value, levels <- c( "Excellent", "Good","Average", "Poor", 
                                              "Terrible"))
  
  
  sat$year <- factor(sat$year,levels=years)
  
  sat_plot<- ggplot(sat,aes(x=year,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=year,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Average","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          axis.text.x=element_text(size=13),
          legend.text=element_text(size=13),
          # legend.position = "left", 
          plot.margin = margin(l=.7, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0, unit='cm')) + guides(color="none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slideSat.png"),plot=sat_plot,width = 7, height=4.5,units="in")
  
  
  
  
  
  
  
  ####### Slide 8 - Satisfaction with value . Left
  ## These are just students
  if (HAS_LAST){
    sat <- data_grp_bar('Q2.4', stacked_labels=TRUE,comp=T)
    sat_this <- sat %>% filter(!Location%in%c("Region", "Nation"))
    sat_this$year <- this_year
    sat_last <- data_grp_bar('Q2.4', stacked_labels=TRUE,uselast = T) %>% filter(!Location%in%c("Region", "Nation"))
    sat_last$year <- last_year
    sat <- bind_rows(sat_this,sat_last)
    sat$hjust_var <- ifelse(sat$Percent<=4,-2.4,.5)
    years <- c(unique(sat_last$year),unique(sat_this$year))
  } else{
    sat <- data_grp_bar('Q2.4', stacked_labels=TRUE,comp=T)
    sat <- sat %>% filter(!Location%in%c("Region", "Nation"))
    sat$year <- this_year
    sat$hjust_var <- ifelse(sat$Percent<=4,-4.4,.5)
    years <- unique(sat$year)
    
  }
  
  sat$Value <- as.factor(sat$Q2.4)
  sat$label_col <- ifelse(sat$Value %in% c("Poor value",'Terrible value'),"white","black")
  
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  
  
  
  sat$Value <- factor(sat$Value, levels <- c( "Excellent value", "Good value","Average value", "Poor value", 
                                              "Terrible value"))
  
  
  sat$year <- factor(sat$year,levels=years)
  plot8a <- ggplot(sat,aes(x=year,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=year,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Average","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),axis.text.x=element_text(size=13), legend.title = element_blank(),
          # legend.position = "left", 
          plot.margin = margin(l=.7, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.text = element_text(size = 13),
          legend.margin=margin(t = 0, unit='cm')) + guides(color="none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide8a.png"),plot=plot8a,width = 5, height=4.5,units="in")
  
  
  ####### Slide 7 meaning of value . Right
  # just students
  
  
  meaning <- data_grp_bar('Q62',comp=T)
  
  #clean up the text answers
  meaning$Value <- gsub('Value to me is about ', '', meaning$Q62)
  meaning$Value <- paste0(toupper(substr(meaning$Value, 1, 1)), substr(meaning$Value, 2, nchar(meaning$Value)))
  
  meaning$Value <- factor(meaning$Value,
                          levels = c("I do not care about value", 
                                     "How much food I receive",
                                     "How much I pay",
                                     "The quality of the food",
                                     "The overall experience"))
  #meaning$Location <- factor(meaning$Location, levels = c('Region', 'Nation', UNIVERSITY_NAME))
  
  meaning <- complete(meaning, Location, Value, fill = list(Percent=0, label = '0%'))
  
  orderdf <- meaning %>% subset(Location==UNIVERSITY_NAME) %>% 
    arrange(desc(Percent)) %>% 
    select(Value)
  
  
  meaning$Value <- factor(meaning$Value,levels=rev(orderdf$Value))
  meaning$Location <- factor(meaning$Location,levels=c("Nation","Region",UNIVERSITY_NAME))
  plot8b<- ggplot(meaning,aes(x=Value,y=Percent,fill=Location,label=label)) + 
    geom_bar(stat="identity",position="dodge")+ 
    geom_text(aes(x=Value,y=Percent + max(meaning$Percent)/15,label=label),size = 4, position = position_dodge(width = .9)) +
    coord_flip() + 
    scale_fill_manual(breaks = c("Nation","Region",UNIVERSITY_NAME),values=rev(three_tone),labels=c(" Nation "," Region ",paste(" ",UNIVERSITY_NAME_SHORT," "))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(meaning$Percent)+10), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.x = element_blank(), legend.title = element_blank(),
          legend.position = "bottom", 
          #legend.position=c(0.5,0.5),
          legend.margin=margin(l=-1.8, t = 0,r=1, unit='cm'))  + 
    guides(fill = guide_legend(reverse = TRUE)) 
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide8b.png"),plot=plot8b,width = 5, height=4.5,units="in")
  
  
  
  ################### Replace dot charts with these satisfaction charts, for now just did the first one
  
  sat <- data_grp_bar('Q2.1', appendix=TRUE, stacked_labels=TRUE,comp=T)
  sat$Value <- as.factor(sat$Q2.1)
  sat$label_col <- ifelse(sat$Value %in% c("Excellent","Good"),"black","white")
  sat$hjust_var <- ifelse(sat$Percent<=4,-1.6,.5)
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  sat$Value <- factor(sat$Value, levels <- c( "Excellent", "Good","Average", "Poor", 
                                              "Terrible"))
  
  sat$Location <- factor(sat$Location,levels=c(UNIVERSITY_NAME_WRAP,"Region", "Nation","Market Segment"))
  
  
  
  plot10 <- ggplot(sat,aes(x=Location,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=Location,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,
              size = 4, position = position_stack(vjust = 0.5),segment.alpha=0,segment.color= "white") +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Average","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "left", 
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = margin(l=1,r=.1, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0, r = .7, unit='cm')) + guides(color=F)
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide10.png"),plot=plot10,width = 7, height=5,units="in",bg = "transparent")
  
  #satisfaction with quality
  if (is_ca){
    sat <- data_grp_bar('Q63ca_1', appendix=TRUE, stacked_labels=TRUE,comp=T)
    sat$Value <- as.factor(sat$Q63ca_1)
    sat <- sat[sat$Q63ca_1!="Not Applicable/Don't Know",]
  } else {
    sat <- data_grp_bar('Q63_1', appendix=TRUE, stacked_labels=TRUE,comp=T)
    sat$Value <- as.factor(sat$Q63_1)
    sat <- sat[sat$Q63_1!="Not Applicable/Don't Know",]
  }

  #drop NA/don't know
  
  sat$label_col <- ifelse(sat$Value %in% c("Excellent","Good"),"black","white")
  sat$hjust_var <- ifelse(sat$Percent<=4,-1.6,.5)
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  sat$Value <- factor(sat$Value, levels <- c( "Excellent", "Good","Average", "Poor", 
                                              "Terrible"))
  
  sat$Location <- factor(sat$Location,levels=c(UNIVERSITY_NAME_WRAP,"Region", "Nation","Market Segment"))
  
  
  
  plot10_quality <- ggplot(sat,aes(x=Location,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=Location,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,
              size = 4, position = position_stack(vjust = 0.5),segment.alpha=0,segment.color= "white") +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Average","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "left", 
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = margin(l=1,r=.1, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0, r = .7, unit='cm')) + guides(color=F)
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide10_quality.png"),plot=plot10_quality,width = 7, height=5,units="in",bg = "transparent")
  
  #satisfaction with speed of service
  sat <- data_grp_bar('Q64_4', appendix=TRUE, stacked_labels=TRUE,comp=T)
  sat$Value <- as.factor(sat$Q64_4)
  #drop NA/don't know
  sat <- sat[sat$Q64_4!="Not Applicable/Don't Know",]
  sat$label_col <- ifelse(sat$Value %in% c("Excellent","Good"),"black","white")
  sat$hjust_var <- ifelse(sat$Percent<=4,-1.6,.5)
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  sat$Value <- factor(sat$Value, levels <- c( "Excellent", "Good","Average", "Poor", 
                                              "Terrible"))
  
  sat$Location <- factor(sat$Location,levels=c(UNIVERSITY_NAME_WRAP,"Region", "Nation","Market Segment"))
  
  
  
  plot10_speed_of_service <- ggplot(sat,aes(x=Location,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=Location,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,
              size = 4, position = position_stack(vjust = 0.5),segment.alpha=0,segment.color= "white") +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Average","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "left", 
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = margin(l=1,r=.1, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0, r = .7, unit='cm')) + guides(color=F)
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide10_speed_of_service.png"),plot=plot10_speed_of_service,width = 7, height=5,units="in",bg = "transparent")
  
  
  #satisfaction with cleanliness
  sat <- data_grp_bar('Q64_5', appendix=TRUE, stacked_labels=TRUE,comp=T)
  sat$Value <- as.factor(sat$Q64_5)
  #drop NA/don't know
  sat <- sat[sat$Q64_5!="Not Applicable/Don't Know",]
  sat$label_col <- ifelse(sat$Value %in% c("Excellent","Good"),"black","white")
  sat$hjust_var <- ifelse(sat$Percent<=4,-1.6,.5)
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  sat$Value <- factor(sat$Value, levels <- c( "Excellent", "Good","Average", "Poor", 
                                              "Terrible"))
  
  sat$Location <- factor(sat$Location,levels=c(UNIVERSITY_NAME_WRAP,"Region", "Nation","Market Segment"))
  
  
  
  plot10_cleanliness <- ggplot(sat,aes(x=Location,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=Location,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,
              size = 4, position = position_stack(vjust = 0.5),segment.alpha=0,segment.color= "white") +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Average","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "left", 
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = margin(l=1,r=.1, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0, r = .7, unit='cm')) + guides(color=F)
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide10_cleanliness.png"),plot=plot10_cleanliness,width = 7, height=5,units="in",bg = "transparent")
  
  
  #satisfaction with convenience
  sat <- data_grp_bar('Q64_1', appendix=TRUE, stacked_labels=TRUE,comp=T)
  sat$Value <- as.factor(sat$Q64_1)
  #drop NA/don't know
  sat <- sat[sat$Q64_1!="Not Applicable/Don't Know",]
  sat$label_col <- ifelse(sat$Value %in% c("Excellent","Good"),"black","white")
  sat$hjust_var <- ifelse(sat$Percent<=4,-1.6,.5)
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  sat$Value <- factor(sat$Value, levels <- c( "Excellent", "Good","Average", "Poor", 
                                              "Terrible"))
  
  sat$Location <- factor(sat$Location,levels=c(UNIVERSITY_NAME_WRAP,"Region", "Nation","Market Segment"))
  
  
  
  plot10_convenience <- ggplot(sat,aes(x=Location,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=Location,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,
              size = 4, position = position_stack(vjust = 0.5),segment.alpha=0,segment.color= "white") +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Average","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "left", 
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = margin(l=1,r=.1, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0, r = .7, unit='cm')) + guides(color=F)
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide10_convenience.png"),plot=plot10_convenience,width = 7, height=5,units="in",bg = "transparent")


  
  ####### Slide 12b ideal frequency 
  d_factor<- function(x){factor(x,levels= c("0 Times/Week","1-2 Times/Week","3-5 Times/Week","6-9 Times/Week","10+ Times/Week"))}
  q65_1 <- data_grp_bar('Q65_1', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
         filter(!Location %in% c("Nation","Region")) %>% 
         mutate_at(vars(dplyr::starts_with('Q65')),d_factor) %>%
         mutate(q='Dining in at a restaurant') 
  names(q65_1)[1] <- "Freq"
  
  q65_2 <- data_grp_bar('Q65_2', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
    filter(!Location %in% c("Nation","Region")) %>% 
    mutate_at(vars(dplyr::starts_with('Q65')),d_factor) %>%
    mutate(q='Ordering take-out') 
  names(q65_2)[1] <- "Freq"
  
  q65_3 <- data_grp_bar('Q65_3', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
    filter(!Location %in% c("Nation","Region")) %>% 
    mutate_at(vars(dplyr::starts_with('Q65')),d_factor) %>%
    mutate(q='Ordering delivery') 
  names(q65_3)[1] <- "Freq"
  
  q65_4<- data_grp_bar('Q65_4', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
    filter(!Location %in% c("Nation","Region")) %>% 
    mutate_at(vars(dplyr::starts_with('Q65')),d_factor) %>%
    mutate(q='Cooking at home') 
  names(q65_4)[1] <- "Freq"
 
  
  tot <- bind_rows(q65_1,q65_2,q65_3,q65_4)
  tot$label_col <- ifelse(tot$Freq %in% c("0 Times/Week","1-2 Times/Week"),"black","white")
  tot$hjust_var <- ifelse(tot$Percent<=4,-1.9,.5)
  #adust label color if it is off the side to be black
  tot$label_col <- ifelse(tot$Percent<=4,'black', tot$label_col)
  
  slide12b <- ggplot(tot,aes(x=q,y=Percent,fill=Freq,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=q,y=Percent,fill=Freq,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",levels(tot$Freq))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "left", 
          plot.margin = margin(l=1.5, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0, r=1,unit='cm')) + guides(color="none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide12b.png"),bg = "transparent", plot=slide12b,width = 8, height=4,units="in")
  
  ####### Slide 13new - most common locations
  res <- data_school_c %>% select(dplyr::starts_with("Q3.2")) %>% 
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q3.2")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_school_c)) %>% arrange(desc(n)) %>%
    mutate(value=gsub("}","",gsub("${e://Field/","",value,fixed=T)))
  
  
  outlets$cleanname <-trimws(outlets$SCHOOL_NAME)
  os <- outlets %>% filter(cleanname==!!enquo(UNIVERSITY_NAME)) %>%
    pivot_longer(cols=dplyr::starts_with("OUTLET_NAME")) %>% 
    select(name,value)%>%
    rename(outlet=value)

  
  res2 <- merge(res,os,by.x="value",by.y="name",all.x=T) %>% 
           group_by(outlet) %>%  # This is needed for the rare case of duplicate outlet names
           summarize(n=sum(n)) %>% 
           arrange(desc(n))%>% top_n(5,n) 
  
   res2$label<- paste0(roundQual(res2$n,0),"%")

  res2$outlet<- unlist(lapply(res2$outlet,FUN=function(x){paste(strwrap(x,35),collapse="\n")}))

  levels <-  res2$outlet
  res2$outlet <- factor(res2$outlet, levels = rev(levels))
  
  plotLocations<-
    ggplot(res2,aes(x=outlet,y=n,label=label)) + 
    geom_bar(stat="identity",fill=two_tone[1],width = .8)+ 
    geom_text(aes(x=outlet,y=n + max(res2$n)/15,label=label),size = 3, position = position_dodge(width = .9)) +
    coord_flip() + 
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res2$n+max(res2$n)/15+10)), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.4,-0.03),
          legend.margin=margin(t = 0, unit='cm')) 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slideLocations.png"),plot=plotLocations,width = 4, height=4,units="in")
  
  
  ####### Slide location  b 
  satL <- data_grp_bar('Q3.3', stacked_labels=TRUE,comp = T)
  satL <- satL %>% filter(!Location%in%c("Region", "Nation"))


satL$Value <- as.factor(satL$Q3.3)
satL$label_col <- ifelse(satL$Value %in% c("Extremely satisfied","Somewhat satisfied"),"black","white")
satL$hjust_var <- ifelse(satL$Percent<=4,-1.6,.5)
#adust label color if it is off the side to be black
satL$label_col <- ifelse(satL$Percent<=4,'black', satL$label_col)



satL$Value <- factor(satL$Value, levels = c( "Extremely satisfied",   "Somewhat satisfied",
                                             "Neither satisfied nor dissatisfied", "Somewhat dissatisfied", "Extremely dissatisfied"
                                           ))
satL$year <- this_year


plotLb <- ggplot(satL,aes(x=year,y=Percent,fill=Value,label=label)) + 
  geom_bar(stat="identity",width=.25)+ 
  geom_text_repel(aes(x=year,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
  scale_x_discrete(position = "top")  + 
  scale_fill_manual(values=colors,labels=paste(" ",  c( "Extremely satisfied",   "Somewhat satisfied",
                                                        "Neither satisfied \n  nor dissatisfied", "Somewhat dissatisfied", "Extremely dissatisfied" ))) +
  scale_color_manual(values=c("black","white"))+
  theme_minimal() + ylab("") + xlab("") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), legend.title = element_blank(),
        # legend.position = "left", 
        plot.margin = margin(l=.7, unit="cm"),
        legend.position=c(0.1,0.5),
        legend.margin=margin(t = 0, unit='cm')) + guides(color="none")

ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slideLocationSat.png"),plot=plotLb,width = 4, height=4.5,units="in")

  
  
  
  ####### Slide 13a - Meal Plan purchasing
  # as of 2023, 1.8.1 has changed back to 1.8 for us adn 1.8ca for canada
  if (!is_ca){
  if ( HAS_LAST){
    sat <- data_grp_bar('Q4.2', stacked_labels = TRUE,filter_enr=T)
    sat_this <- sat %>% filter(!Location%in%c("Region", "Nation"))
    sat_this$year <- this_year
    sat_last <- data_grp_bar('Q4.2', stacked_labels=TRUE,uselast = T) %>% filter(!Location%in%c("Region", "Nation"))
    sat_last$year <- last_year
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
  plan$Value <- as.factor(plan$Q4.2)
  plan$Value <- unlist(lapply(plan$Value,FUN=function(x){paste(strwrap(x,35),collapse="\n")}))
  
  plan$label_col <- ifelse(plan$Value %in% c("Definitely will buy" , "Probably will buy"),"black","white")
 
  #adust label color if it is off the side to be black
  plan$label_col <- ifelse(plan$Percent<=4,'black', plan$label_col)
  
  
  plan$Value <- factor(plan$Value, levels <- c( "Definitely will buy" , "Probably will buy",    "Might or might not buy" ,      
                                                "Probably will not buy" ,"Definitely will not buy"  ))
  plan$year <- factor(plan$year,levels=years)
  plot13a <- ggplot(plan,aes(x=year,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=year,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = 0.5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste("   ",c("Definitely","Probably","Maybe","Probably Not","Definitely Not"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          #legend.position = "left", 
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.spacing = unit(2, "cm"),
          plot.margin=unit(c(0,0,0,1.5),"cm"),
          legend.position=c(-0.01,0.5),
          legend.margin=margin(t = 0,l=0, r=1, unit='cm')) + guides(color=F)
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide13a.png"),plot=plot13a,width = 5.5, height=4.5,units="in",bg="transparent")
  
  }
  ####### Slide 13b Purchasing requirements
  data_13b <- data_grp_bar('Q4.1',comp=T)
  data_13b$Value <- as.factor(data_13b$Q4.1)
  data_13b$Value <- unlist(lapply(data_13b$Value,FUN=function(x){paste(strwrap(x,35),collapse="\n")}))

  
  orderdf <- data_13b %>% subset(Location==UNIVERSITY_NAME) %>% 
    arrange(desc(Percent)) %>% 
     select(Value)

  
  data_13b$Value <- factor(data_13b$Value,levels=rev(orderdf$Value))
  data_13b$Location<- factor(data_13b$Location,levels=c("Nation","Region",UNIVERSITY_NAME))
  
  plot13b<- ggplot(data_13b,aes(x=Value,y=Percent,fill=Location,label=label)) + 
    geom_bar(stat="identity",position="dodge")+ 
    geom_text(aes(x=Value,y=Percent + max(data_13b$Percent)/10,label=label),size = 3, position = position_dodge(width = .9)) +
    coord_flip() + 
    scale_fill_manual(breaks = c("Nation","Region",UNIVERSITY_NAME),values=rev(three_tone),
                      labels=c(" Nation  ","  Region   ",paste(" ",UNIVERSITY_NAME_SHORT,"  "))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(data_13b$Percent)+15), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
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
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide13b.png"),plot=plot13b,width = 4, height=2.5,units="in")
  

  ####### Slide 14 barriers to participation
  if (!is_ca){
  base <- sum(data_school_c$Q4.2 %in% c("Probably will not buy", "Might or might not buy",  
                                        "Definitely will not buy"),na.rm=T)
  
  data_14  <- data_school_c %>% filter(Q4.2 %in%c("Probably will not buy", "Might or might not buy",  
                                             "Definitely will not buy"))%>% 
    rename(value=Q67) %>% select(value) %>%
    filter(value!="") %>% 
    mutate(value= gsub(" \\(please specify)","",value)) %>% 
    mutate(value = gsub(" \\(e\\.g.*)","",value)) %>% 
    #mutate(value = gsub("meal","dining",value)) %>% 
    group_by(value) %>% summarize(Percent=100*n()/base)  %>% arrange(desc(Percent)) %>%
    mutate(label=paste0(roundQual(Percent,0),"%"))
  
  
  data_14_all <- data_c %>%
    rename(value=Q67) %>% select(value) %>%
    filter(value!="") %>% 
    mutate(value= gsub(" \\(please specify)","",value)) %>% 
    mutate(value = gsub(" \\(e\\.g.*)","",value)) %>% 
    #mutate(value = gsub("meal","dining",value)) %>% 
    group_by(value) %>% summarize(PercentNational=n())
  
  data_14 <- merge(data_14,data_14_all,all=T)
  data_14$Percent <- ifelse(is.na(data_14$Percent),0,data_14$Percent)
  data_14$label <- ifelse(is.na(data_14$label),"0%",data_14$label)
  
  data_14$value <- unlist(lapply(data_14$value,FUN=function(x){paste(strwrap(x,45),collapse="\n")}))
  
  orderdf <- data_14 %>% 
    arrange(desc(Percent)) %>%
    filter(value != "Other") %>% select(value)
  
  other_values <- unique(data_14$value)[(!unique(data_14$value) %in% orderdf$value) & (unique(data_14$value) != "Other" )]
  
  order <- c(orderdf$value,other_values,"Other")
  
  data_14$Value <- factor(data_14$value,levels=rev(order))
  
  plot14<-
    ggplot(data_14,aes(x=Value,y=Percent,label=label),fill=three_tone[2]) + 
    geom_bar(stat="identity",position="dodge",fill=two_tone[1])+ 
    geom_text(aes(x=Value,y=Percent + max(data_14$Percent)/15,label=label),size = 3, position = position_dodge(width = .9)) +
    coord_flip() + 
    scale_fill_manual(values=three_tone[2]) +
    #scale_fill_manual(breaks = c("Region","Nation",UNIVERSITY_NAME),values=rev(three_tone),labels=c("   Region   ","   Nation  ",paste(" ",UNIVERSITY_NAME,"  "))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(data_14$Percent)+5), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.3,-0.03),
          legend.margin=margin(t = 0, unit='cm'))  + 
    guides(fill = "none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide14.png"),plot=plot14,width = 6, height=4.5,units="in")
  
  }
  ####### Slide 15 motivators
  
  if (is_ca==F){
    base_data <- data_school_c%>% filter(Q1.8 %in% c("Yes")) %>% filter(Q67!="Graduating")
  } else {
    base_data <- data_school_c%>% filter(Q1.8ca %in% c("Yes")) %>% filter(Q67!="Graduating")
  }
  
  if(!is_ca){
  base <- nrow(base_data) 
  
  res_s <- base_data %>%
    select(dplyr::starts_with("Q68")) %>% select(!dplyr::contains("Q68_9_TEXT")) %>%
    select(!dplyr::contains("Q68_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q68")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" \\(please specify)","",value)) %>% 
    #mutate(value = gsub("meal plan","dining plan",value)) %>% 
    #mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
    group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
  
  res_all <- data %>% 
    select(dplyr::starts_with("Q68")) %>% select(!dplyr::contains("Q68_9_TEXT")) %>%
    select(!dplyr::contains("Q68_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q68")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" \\(please specify)","",value)) %>% 
    #mutate(value = gsub("meal plan","dining plan",value)) %>% 
   # mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
    group_by(value) %>% summarize(n_nat=100*n()/base)
  
 res <- merge(res_s,res_all,by="value",all=T)
 res$Percent <- ifelse(is.na(res$n),0,res$n)
  
  
  res$value <- unlist(lapply(res$value,FUN=function(x){paste(strwrap(x,45),collapse="\n")}))

  
  res$label<- paste0(roundQual(res$n,0),"%")
  
  res <- res %>% arrange(desc(n)) %>% arrange(value=="Other")%>% arrange(value=="None of these")
  levels <-  res$value
  
  res$value <- factor(res$value, levels = rev(levels))
  
  plotMotiv<-
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
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","plotMotiv.png"),plot=plotMotiv,width = 6, height=5,units="in")
  }
  
  ###################### new slide 17 
  # those who want to use off campus
  if(is_ca==F){
  base <- nrow(data_school_c %>% filter(Q68_14=="Ability to use my meal plan off campus"))
  
  res_s <- data_school_c %>% filter(Q68_14=="Ability to use my meal plan off campus") %>% 
    select(dplyr::starts_with("Q92")) %>% select(!dplyr::contains("Q92_6_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q92")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" (please specify):","",value,fixed=T)) %>% 
    #mutate(value = gsub("meal plan","dining plan",value)) %>% 
    #mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
    group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
  
  res_all <- data %>%
    select(dplyr::starts_with("Q92")) %>% select(!dplyr::contains("Q92_6_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q92")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" (please specify):","",value,fixed=T)) %>% 
    #mutate(value = gsub("meal plan","dining plan",value)) %>% 
    # mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
    group_by(value) %>% summarize(n_nat=100*n()/base)
  
  res <- merge(res_s,res_all,by="value",all=T)
  res$Percent <- ifelse(is.na(res$n),0,res$n)
  res$n <- ifelse(is.na(res$n),0,res$n)
  
  
  res$value <- unlist(lapply(res$value,FUN=function(x){paste(strwrap(x,45),collapse="\n")}))
  
  
  res$label<- paste0(roundQual(res$Percent,0),"%")
  
  res <- res %>% arrange(desc(n)) %>% arrange(value=="Other")%>% arrange(value=="None of these")
  levels <-  res$value
  
  res$value <- factor(res$value, levels = rev(levels))
  
  plotPrefer<-
    ggplot(res,aes(x=value,y=n,label=label)) + 
    geom_bar(stat="identity",fill=two_tone[1],width = .8)+ 
    geom_text(aes(x=value,y=n + max(res$Percent)/15,label=label),size = 3, position = position_dodge(width = .9)) +
    coord_flip() + 
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent+max(res$Percent)/15+10)), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.4,-0.03),
          legend.margin=margin(t = 0, unit='cm')) 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","plotPrefer.png"),plot=plotPrefer,width = 6, height=5,units="in")
  }
  ###################### new slide 18
  # all 
  base <- nrow(data_school_c )
  
  res_s <- data_school_c %>% 
    select(dplyr::starts_with("Q93")) %>% select(!dplyr::contains("Q93_6_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q93")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" (please specify):","",value,fixed=T)) %>% 
    #mutate(value = gsub("meal plan","dining plan",value)) %>% 
    #mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
    group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
  
  res_all <- data %>%
    select(dplyr::starts_with("Q93")) %>% select(!dplyr::contains("Q93_6_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q93")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" (please specify):","",value,fixed=T)) %>% 
    #mutate(value = gsub("meal plan","dining plan",value)) %>% 
    # mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
    group_by(value) %>% summarize(n_nat=100*n()/base)
  
  res <- merge(res_s,res_all,by="value",all=T)
  res$Percent <- ifelse(is.na(res$n),0,res$n)
  res$n <- ifelse(is.na(res$n),0,res$n)
  
  

  res$label<- paste0(roundQual(res$Percent,0),"%")
  res$value <- gsub("In-app push notifications","In-app push\n notifications",res$value,fixed=T)
  
   levels <- c( "Email","Text message", "Social media","Campus dining website","In-app push\n notifications", 
               "Other"  )
  
  res$value <- factor(res$value, levels = levels)
  
  plotCommunicate<-
    ggplot(res,aes(x=value,y=n,label=label)) + 
    geom_bar(stat="identity",fill=two_tone[1],width = .5)+ 
    geom_text(aes(x=value,y=n + max(res$Percent)/20,label=label),size = 3, position = position_dodge(width = .9)) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent+max(res$Percent)/20+10)), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.x =  element_line(color = "grey"),
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.4,-0.03),
          legend.margin=margin(t = 0, unit='cm')) 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","plotCommunicate.png"),plot=plotCommunicate,width = 9.5, height=4,units="in")
  
  
  
    ####### Slide 17 
  ## this is for all repsondents 
  
  eligible = c('Milk', 'Eggs', 'Peanuts', 'Tree nuts', 'Fish', 'Shellfish', 'Soy', 
               'Wheat/Gluten', 'Sesame',  'Other')
  if (is_ca){
    eligible = c(eligible, 'Mustard')
  }
  
  
  
  res <- data_school_c %>% filter(Q69_1=="Food allergies or intolerances")%>% 
    select(dplyr::starts_with("Q70")) %>% select(!dplyr::contains("Q70_10_TEXT")) %>%
    select(!dplyr::contains("Q70_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q70")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n)) %>% 
    filter(value!='None of the above') %>% 
    filter(value %in% eligible) %>%
    arrange(value %in% c("Other (please specify)"))
  
  res$value <- gsub(" \\(please specify\\)","",res$value)
  
 
  
  
  
  res$label<- paste0(roundQual(res$n,0),"%")
  
  
  levels <-  res$value
  
  res$value <- factor(res$value, levels = levels)
  res$Percent <- res$n
  
  plot17<- ggplot(res,aes(x=value,y=Percent,label=label)) + 
    geom_bar(stat="identity",position="dodge",fill=two_tone[1],width=.5)+ 
    geom_text(aes(x=value,y=Percent + max(Percent)/10,label=label),size = 6, position = position_dodge(width = .9)) +
    #scale_fill_manual(breaks = rev(c("Region","Nation",UNIVERSITY_NAME)),values=three_tone,labels=rev(c("  Region  ","   Nation  ",paste(" ",UNIVERSITY_NAME,"  ")))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent)+max(res$Percent)/5), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.x =  element_line(color = "grey"),
          axis.text.x = element_text(angle=45,hjust=1,size=18),
          axis.text.y = element_blank(), legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          #legend.position = "bottom", 
          legend.position=c(0.5,slide17_leg),
          legend.margin=margin(t = 0.1,b=0,l=.1, unit='cm'))  + 
    guides(fill = "none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide17a.png"),
         plot=plot17,width =11, height=3,units="in",bg="transparent")

  
  ####### Slide 17 b 
  ## this is for all repsondents 
  
  eligible = c('Vegetarian', 'Vegan', 'Halal', 'Kosher', 'Paleo/Primal', 'Ketogenic', 'Mediterranean')
  if (!is_ca){
    eligible = c(eligible, c('Atkins', 'GMO-Free', 'Whole 30', 'Plant-based', 'Other',"Low-FODMAP","Pescatarian"))
  } else {
    eligible = c(eligible, c( 'Flexitarian', 'No Peanut/Tree Nut', 'No Dairy', 'No Gluten', 'Other'))
  }
  
               

  
  
  res <- data_school_c %>% filter(Q69_2=="Special dietary lifestyle for personal preference or religious reasons")%>% 
    select(dplyr::starts_with("Q71")) %>% select(!dplyr::contains("Q71_14_TEXT")) %>%
    select(!dplyr::contains("Q71_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q71")) %>%
    filter(value!="") %>%
    filter(value %in% eligible) %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n)) %>% 
  filter(value!='None of the above') %>% 
    arrange(value %in% c("Other (please specify)"))
  
  
  res$value <- gsub(" \\(please specify\\)","",res$value)
  
  
  res$label<- paste0(roundQual(res$n,0),"%")
  
  
  levels <-  res$value
  
  res$value <- factor(res$value, levels = levels)
  res$Percent <- res$n
  
  plot17b<- ggplot(res,aes(x=value,y=Percent,label=label)) + 
    geom_bar(stat="identity",position="dodge",fill=two_tone[1],width=.5)+ 
    geom_text(aes(x=value,y=Percent + max(Percent)/10,label=label),size = 6, position = position_dodge(width = .9)) +
    #scale_fill_manual(breaks = rev(c("Region","Nation",UNIVERSITY_NAME)),values=three_tone,labels=rev(c("  Region  ","   Nation  ",paste(" ",UNIVERSITY_NAME,"  ")))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent)+max(res$Percent)/5), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          plot.margin = margin(0, 0, 0, .5, "cm"),
          axis.line.x =  element_line(color = "grey"),
          axis.text.x = element_text(angle=45,hjust=1,size =18),
          axis.text.y = element_blank(), legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          #legend.position = "bottom", 
          legend.position=c(0.5,slide17_leg),
          
          legend.margin=margin(t = 0.1,b=0,l=.1, unit='cm'))  + 
    guides(fill = "none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide17b.png"),
         plot=plot17b,width =11.1, height=3,units="in",bg="transparent")
  
  ########################## this slide is for cuisine 
  # those who want to use off campus
  base <- nrow(data_school_c)
  
  if (!is_ca){
    res_s <- data_school_c %>% 
      select(dplyr::starts_with("Q94")) %>% select(!dplyr::contains("Q94_18")) %>% select(!dplyr::contains("Q94ca"))
  } else {
    res_s <- data_school_c %>% 
      select(dplyr::starts_with("Q94ca")) %>% select(!dplyr::contains("Q94ca_20")) 
  }

  res_s <- res_s %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q94")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" (please specify):","",value,fixed=T)) %>% 
    #mutate(value = gsub("meal plan","dining plan",value)) %>% 
    #mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
    group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
  
  if (!is_ca){
    res_all <- data %>% 
      select(dplyr::starts_with("Q94")) %>% select(!dplyr::contains("Q94_18")) %>% select(!dplyr::contains("Q94ca"))
  } else {
    res_all <- data %>% 
      select(dplyr::starts_with("Q94ca")) %>% select(!dplyr::contains("Q94ca_20")) 
  }
  
  res_all <- res_all %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q94")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" (please specify):","",value,fixed=T)) %>% 
    #mutate(value = gsub("meal plan","dining plan",value)) %>% 
    # mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
    group_by(value) %>% summarize(n_nat=100*n()/base)
  
  res <- merge(res_s,res_all,by="value",all=T)
  res$Percent <- ifelse(is.na(res$n),0,res$n)
  res$n <- ifelse(is.na(res$n),0,res$n)
  
  
  res$value <- unlist(lapply(res$value,FUN=function(x){paste(strwrap(x,45),collapse="\n")}))
  
  
  res$label<- paste0(roundQual(res$Percent,0),"%")
  
  res <- res %>% arrange(desc(n)) %>% arrange(value=="Other")%>% arrange(value=="None of these")
  levels <-  res$value
  
  res$value <- factor(res$value, levels = rev(levels))
  
  plotCuis<-
    ggplot(res,aes(x=value,y=n,label=label)) + 
    geom_bar(stat="identity",fill=two_tone[1],width = .8)+ 
    geom_text(aes(x=value,y=n + max(res$Percent)/10,label=label),size = 4, position = position_dodge(width = .9)) +
    coord_flip() + 
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent+max(res$Percent)/10+7)), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.y=element_text(size=12),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.4,-0.03),
          legend.margin=margin(t = 0, unit='cm')) 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","plotCuis.png"),plot=plotCuis,width = 7, height=6,units="in")
  
 #########################################
   ### ######NEw slide 21 
  base <- nrow(data_school_c)
  
  res <- data_school_c %>% 
    select(dplyr::starts_with("Q95")) %>% select(!dplyr::contains("Q95_10_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q95")) %>%
    filter(value!="") %>%
    #  filter(!grepl("Other",value)) %>%
    #  mutate(value= gsub(" (please specify) :","",value, fixed=T)) %>% 
    group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
  
  VAL <- 100-res[res$value=="None of the above",'n']
  
  VAL$val <- "Yes"
  VAL2 <- VAL
  VAL2$n <- 100-VAL$n
  VAL2$val <- "No"
  
  
  val_all <- rbind(VAL2,VAL)
  val_all$label <- paste0(val_all$val,": ",round(val_all$n,0),"%")
  
  # create labels
  val_all$label_col <- ifelse(val_all$n<=4,'black', "white")
  val_all$hjust_var <- ifelse(val_all$n<=4,-3.6,.5)
  val_all$per <- val_all$n
  val_all <- val_all %>% arrange(desc(val))
  
  
  df <- val_all %>% 
    mutate(end = 2 * pi * cumsum(per)/sum(per),
           start = lag(end, default = 0),
           middle = 0.5 * (start + end),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  
  plot_veg <-  ggplot(df) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                     start = start, end = end, fill = val)) +
    geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = label,
                  hjust = hjust, vjust = vjust)) +
    scale_fill_manual(values=colors[c(5,3)],breaks=c("Yes","No")) + 
    coord_fixed() +
    scale_x_continuous(limits = c(-1.6, 1.6),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.6, 1.6),    # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    theme_minimal() + guides(fill="none")
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","plot_veg.png"),plot=plot_veg,width = 3.5, height=3.5,units="in")
  
  ### ######CA slide 9 
  if(is_ca){
  
  base <- nrow(data_school_c)
  
  res <- data_school_c %>% 
    select(dplyr::starts_with("Q63ca_2_"))  %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q63ca_2_")) %>%
    filter(value!="") %>%
    filter(value!="None of the above") %>%
    #  filter(!grepl("Other",value)) %>%
    #  mutate(value= gsub(" (please specify) :","",value, fixed=T)) %>% 
    group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n)) %>% 
    mutate(lab=paste0("#",row_number()),n=paste0(value,"-",round(n,0),"%")) %>% head(5)
    
  
  
  tablefav <- res %>% select(lab,n) %>% gt() %>%cols_label("lab"="Rank","n"="Feature") %>%
    cols_align(
      align = "center",
      columns= c(2)
    ) %>%  
    cols_width(
      c(2) ~ px(220) 
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
  
  
  gtsave(tablefav,filename = paste0(loc,UNIVERSITY_NAME,"/","tablefav.png"))
}
  ############## second graph in slide 21
  ### ######NEw slide 21 
  base <- nrow(data_school_c)
  
  res <- data_school_c %>% 
    select(dplyr::starts_with("Q95")) %>% select(!dplyr::contains("Q95_10_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q95")) %>%
    filter(value!="") %>%
    filter(value!="None of the above") %>%
    #  filter(!grepl("Other",value)) %>%
    #  mutate(value= gsub(" (please specify) :","",value, fixed=T)) %>% 
    group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n)) %>% 
    mutate(n=paste0(round(n,0),"%"))
  
  
  tableVeg <- res %>% gt() %>%cols_label("value"="","n"="") %>%
    cols_align(
      align = "center",
      columns= c(2)
    ) %>%  
    cols_width(
      c(2) ~ px(120) 
    )
  
  gtsave(tableVeg,filename = paste0(loc,UNIVERSITY_NAME,"/","tableVeg.png"))
 
  ############## veggie alternative frequency
  d_factor<- function(x){factor(x,levels= rev(c( "Never","Rarely","Sometimes","Often", "Always")))}
  if (!is_ca){
  q96_1 <- data_grp_bar('Q96_1', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
    filter(!Location %in% c("Nation","Region")) %>% 
    mutate_at(vars(dplyr::starts_with('Q96')),d_factor) %>%
    mutate(q='Meat alternatives (Impossible, Beyond, etc.)') 
  names(q96_1)[1] <- "Freq"
  
  q96_2 <- data_grp_bar('Q96_2', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
    filter(!Location %in% c("Nation","Region")) %>% 
    mutate_at(vars(dplyr::starts_with('Q96')),d_factor) %>%
    mutate(q='Veggie forward alternatives (Black bean burger, quinoa burger, falafel, etc.)') 
  names(q96_2)[1] <- "Freq"
  
  q96_3 <- data_grp_bar('Q96_3', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
    filter(!Location %in% c("Nation","Region")) %>% 
    mutate_at(vars(dplyr::starts_with('Q96')),d_factor) %>%
    mutate(q='Vegetable-based proteins (tofu, tempeh, seitan, etc.)') 
  names(q96_3)[1] <- "Freq"
  tot <- bind_rows(q96_1,q96_2,q96_3)
  } else {
    q115_1 <- data_grp_bar('Q115_1', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
      filter(!Location %in% c("Nation","Region")) %>% 
      mutate_at(vars(dplyr::starts_with('Q115')),d_factor) %>%
      mutate(q='Meat alternatives (Impossible, Beyond, Yves Chick!n etc.)') 
    names(q115_1)[1] <- "Freq"
    
    q115_2 <- data_grp_bar('Q115_2', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
      filter(!Location %in% c("Nation","Region")) %>% 
      mutate_at(vars(dplyr::starts_with('Q115')),d_factor) %>%
      mutate(q='Vegetable based alternatives (Black bean burger, quinoa burger, etc.)') 
    names(q115_2)[1] <- "Freq"
    
    q115_3 <- data_grp_bar('Q115_3', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
      filter(!Location %in% c("Nation","Region")) %>% 
      mutate_at(vars(dplyr::starts_with('Q115')),d_factor) %>%
      mutate(q='Vegetable-based proteins (tofu, tempeh, seitan, falafel etc.)') 
    names(q115_3)[1] <- "Freq"
    
    q115_4 <- data_grp_bar('Q115_4', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
      filter(!Location %in% c("Nation","Region")) %>% 
      mutate_at(vars(dplyr::starts_with('Q115')),d_factor) %>%
      mutate(q='Plant-Based Milk') 
    names(q115_4)[1] <- "Freq"
    
    q115_5 <- data_grp_bar('Q115_5', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
      filter(!Location %in% c("Nation","Region")) %>% 
      mutate_at(vars(dplyr::starts_with('Q115')),d_factor) %>%
      mutate(q='Plant-Based Cheese or Other Dairy') 
    names(q115_5)[1] <- "Freq"
    
    tot <- bind_rows(q115_1,q115_2,q115_3,q115_4,q115_5)
    
  }
  
  #tot$label_col <- ifelse(tot$Freq %in% c( "Never","Rarely"),"black","white")
  tot$label_col <- ifelse(tot$Freq %in% c( "Never","Rarely"),"white","black")
  
  tot$hjust_var <- ifelse(tot$Percent<=4,-1.9,.5)
  #adust label color if it is off the side to be black
  tot$label_col <- ifelse(tot$Percent<=4,'black', tot$label_col)
  
  tot$q <- unlist(lapply(tot$q,FUN=function(x){paste(strwrap(x,25),collapse="\n")}))
  

  
  slideVegType <- ggplot(tot,aes(x=q,y=Percent,fill=Freq,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=q,y=Percent,fill=Freq,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=rev(colors_stacked),labels=paste(" ",levels(tot$Freq))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "left", 
          plot.margin = margin(l=1.5, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0, r=1,unit='cm')) + guides(color="none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slideVegType.png"),bg = "transparent", plot=slideVegType,width = 8, height=4,units="in")
  
  
  
   ####### Slide Sustainability 
  
  if (!is_ca){
    base <- data_school_c %>% select(dplyr::starts_with("Q73"))  %>% select(!dplyr::contains("Q73ca"))
    res <- data_school_c %>% select(dplyr::starts_with("Q73")) %>% select(!dplyr::contains("Q73_16_TEXT")) %>%
      select(!dplyr::contains("Q73_SDS")) %>% select(!dplyr::contains("Q73ca"))
  } else {
    base <- data_school_c %>% select(dplyr::starts_with("Q73ca"))
    res <- data_school_c %>% select(dplyr::starts_with("Q73ca")) %>% select(!dplyr::contains("TEXT")) %>%
      select(!dplyr::contains("Q73ca_SDS"))
  }
  
  
  # how many respondents made some selection for this
  base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
  
  
  res <- res %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q73")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/base_n)  %>% arrange(desc(n))%>%
    arrange(value %in% c("Other (please specify)")) %>% 
    arrange(value %in% c('None of the above'))
    
    
    
  
  res$value <- gsub(" \\(please specify\\)","",res$value)
  
 # res$value <- unlist(lapply(res$value,FUN=function(x){paste(strwrap(x,45),collapse="\n")}))
  
  
  res$label<- paste0(roundQual(res$n,0),"%")
  
  
  levels <-  res$value
  
  res$value <- factor(res$value, levels = rev(levels))
  
  plotSus<-
    ggplot(res,aes(x=value,y=n,label=label)) + 
    geom_bar(stat="identity",fill=two_tone[1],width = .8)+ 
    geom_text(aes(x=value,y=n + max(res$n)/12,label=label),size = 4, position = position_dodge(width = .9)) +
    coord_flip() + 
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$n+max(res$n)/7+10)), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.4,-0.03),
          legend.margin=margin(t = 0, unit='cm')) 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","plotSustain.png"),plot=plotSus,width = 8, height=5,units="in")
  
  
  
  ####### Slide Top priorities
  base <- data_school_c %>% select(dplyr::starts_with("Q74")) 
  
  # how many respondents made some selection for this
  base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
  
  res <- data_school_c %>% select(dplyr::starts_with("Q74")) %>% select(!dplyr::contains("TEXT")) %>%
    mutate(id=row.names(.)) %>% 
    select(!dplyr::contains("Q74_SDS")) %>%
    pivot_longer(cols=dplyr::starts_with("Q74")) %>%
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
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","plotPriority.png"),plot=plotPrior,width = 8.5, height=4,units="in")
  
  
  
  
  ####### Slide Top priorities
  base <- data_school_c %>% select(dplyr::starts_with("Q75")) 
  
  # how many respondents made some selection for this
  base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
  
  res <- data_school_c %>% select(dplyr::starts_with("Q75")) %>% select(!dplyr::contains("Q75_9_TEXT")) %>%
    select(!dplyr::contains("Q75_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q75")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/base_n)  %>%
    arrange(desc(n)) %>% 
    arrange(value=="I do not apply these types of practices") %>% 
    arrange(value=="I prefer not to answer")
  
  res$value <- gsub(" \\(please specify\\)","",res$value)
  
  res$value <- unlist(lapply(res$value,FUN=function(x){paste(strwrap(x,25),collapse="\n")}))
  
  
  res$label<- paste0(roundQual(res$n,0),"%")
  
  
  levels <-  res$value
  
  res$value <- factor(res$value, levels = levels)
  res$Percent <- res$n
  
  plotWell<-ggplot(res,aes(x=value,y=Percent,label=label)) + 
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
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","plotWell.png"),plot=plotWell,width = 8.5, height=4,units="in")
  
 
  ########################
  #slide 26
  d_factor<- function(x){factor(x,levels= c( "Extremely connected", 
"Very connected",  "Moderately connected","Slightly connected", "Not connected at all"))}
  tot <- data_grp_bar('Q97', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
    filter(!Location %in% c("Nation","Region")) %>% 
    mutate_at(vars(dplyr::starts_with('Q97')),d_factor) %>%
    mutate(q='') 
  names(tot)[1] <- "Freq"
  

  tot$label_col <- ifelse(tot$Freq %in% c("Moderately connected", "Slightly connected", "Not connected at all"),"white","black")
  tot$hjust_var <- ifelse(tot$Percent<=4,-3.9,.5)
  #adust label color if it is off the side to be black
  tot$label_col <- ifelse(tot$Percent<=4,'black', tot$label_col)
  
  tot$q <-  paste0("Top Two Box: ",roundQual(sum(tot$Percent[tot$Freq%in%c("Extremely connected", 
                                                                   "Very connected")])),"%")
  
  slideComm <- ggplot(tot,aes(x=q,y=Percent,fill=Freq,label=label)) + 
    geom_bar(stat="identity",width=.45)+ 
    geom_text_repel(aes(x=q,y=Percent,fill=Freq,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=rev(colors_stacked)
                      #,labels=paste(" ",levels(tot$Freq))
                      ) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.x=element_text(size=13),
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "left", 
          plot.margin = margin(l=1.5, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0,l=1, r=1,unit='cm')) + guides(color="none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slideComm.png"),bg = "transparent", plot=slideComm,width = 4.5, height=4,units="in")
  
  ########### slide 27
 
  # These are wierd numbers but they are the right quesitons 
  if (!is_ca){
    rests <- c(data_school_c$Q98_4,data_school_c$Q98_5,data_school_c$Q98_6,data_school_c$Q98_7,data_school_c$Q98_9)
  } else{
    rests <- c(data_school_c$Q98ca_4,data_school_c$Q98ca_5,data_school_c$Q98ca_6,data_school_c$Q98ca_7,data_school_c$Q98ca_9)
    
  }
  rests <- gsub("????T","T",rests)
  rests <- gsub("â€™", "", rests)
  rests <- str_to_title(rests)
  rests <- str_trim(rests)
  
  rests <- gsub('Chick-Fil-A', 'Chick-fil-A', rests)
  rests <- gsub('Chick Fil A', 'Chick-fil-A', rests)
  rests <- gsub('Chic Fil A', 'Chick-fil-A', rests)
  rests <- gsub('Chickfila', 'Chick-fil-A', rests)
  rests <- gsub('Chic-Fil-A', 'Chick-fil-A', rests)
  rests <- gsub('Chick Fila', 'Chick-fil-A', rests)
  rests <- gsub('Chik Fil A', 'Chick-fil-A', rests)
  rests <- gsub('Mcdonalds', "Mcdonald's", rests)
  rests <- gsub("Mcdonald's", "McDonald's", rests)
  rests <- gsub('Wendys', "Wendy's", rests)
  rests <- gsub('Panda', "Panda Express", rests)
  rests <- gsub('Express Express', "Express", rests)
  rests[grep("Horton",rests)] <- "Tim Hortons"
  
  
  freq<-table(rests)
  freq1<-data.frame(sort(freq, decreasing=TRUE))
  names(freq1) <- c('word', 'freq')
  
  
  
  
  #remove filler words
  freq1 <- freq1 %>% filter(!word %in% c('')) %>% head(10 )
 
  # #WORKING
  # word2 <- wordcloud2(data = freq1, color = rep(colors[3:5],4), rotateRatio=0,backgroundColor = "white")
  # 
  # 
  # saveWidget(word2, paste0(loc,UNIVERSITY_NAME,"/","temp.html"),selfcontained = F)
  # #can set height and width if desired
  # #webshot::webshot("temp.html","word_cloud.png",vwidth = 1992, vheight = 1744, delay =10)
  # webshot::webshot( paste0(loc,UNIVERSITY_NAME,"/","temp.html"), paste0(loc,UNIVERSITY_NAME,"/","word_cloud.png"), delay =10)
  # 
  png(file=paste0(loc,UNIVERSITY_NAME,"/word_cloud.png"),
      width=600, height=350)
  wordcloud(freq1$word,freq1$freq,scale=c(4,.75),min.freq=3,max.words=Inf,
            random.order=TRUE, random.color = T, rot.per=0,
            colors=colors[3:5])
  dev.off()
  
  ######################## Slide 28 - price and quality 

  if (is_ca){
    res <- data.frame(table(data_school_c$Q99ca)) %>% subset(Var1!="") 
  } else{
    res <- data.frame(table(data_school_c$Q99)) %>% subset(Var1!="") 
  }
  
  base <- sum(res$Freq)
  res$per <- roundQual(res$Freq*100/base)
  res <- res %>% arrange(desc(per)) %>% 
    mutate(word=gsub("\ .*","",Var1),label=paste0(word,":\n", per,"%"))
  
  # create labels
  res$label_col <- ifelse(res$per<=4,'black', "white")
  res$hjust_var <- ifelse(res$per<=4,-3.6,.5)
  

  
  
  df <- res %>% 
    mutate(end = 2 * pi * cumsum(per)/sum(per),
           start = lag(end, default = 0),
           middle = 0.5 * (start + end),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  
  pq_plot<-  ggplot(df) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                     start = start, end = end, fill = word)) +
    geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = label,
                  hjust = hjust, vjust = vjust)) +
    scale_fill_manual(values=colors[c(5,3)],breaks=c("Quality","Price")) + 
    coord_fixed() +
    scale_x_continuous(limits = c(-1.6, 1.6),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.6, 1.6),    # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    theme_minimal() + guides(fill="none")
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","pq_plot.png"),plot=pq_plot,width = 3.5, height=3.5,units="in")
  

  ############################### Slide 29 - more fast food reasons
  base <- data_school_c %>% select(dplyr::starts_with("Q100")) 
  
  # how many respondents made some selection for this
  base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
  
  res <- data_school_c %>% select(dplyr::starts_with("Q100")) %>% select(!dplyr::contains("_TEXT")) %>%
    mutate(id=row.names(.)) %>% 
    select(!dplyr::contains("_SDS")) %>%
    pivot_longer(cols=dplyr::starts_with("Q100")) %>%
    filter(value!="") %>%
    group_by(value) %>%
    summarize(n=100*n()/base_n)  %>% 
    mutate(value=gsub(" \\(please specify:\\)","",value)) %>%
    mutate(value = gsub(" \\(e\\.g.*)","",value)) %>% 
    mutate(value = gsub("????T","",value)) %>% 
    arrange(desc(n)) %>%
    arrange(value=="Other") %>% 
    arrange(value=="None of the above")
  

  
  res$value <- unlist(lapply(res$value,FUN=function(x){paste(strwrap(x,35),collapse="\n")}))
  
  
  res$label<- paste0(roundQual(res$n,0),"%")
  
  
  levels <-  res$value
  
  res$value <- factor(res$value, levels = rev(levels))
  res$Percent <- res$n
  
  plot_pq2<-ggplot(res,aes(x=value,y=Percent,label=label)) + 
    geom_bar(stat="identity",position="dodge",fill=two_tone[1],width=.5)+ 
    geom_text(aes(x=value,y=Percent + max(Percent)/15,label=label),size = 3, position = position_dodge(width = .9)) +
    #scale_fill_manual(breaks = rev(c("Region","Nation",UNIVERSITY_NAME)),values=three_tone,labels=rev(c("  Region  ","   Nation  ",paste(" ",UNIVERSITY_NAME,"  ")))) +
    theme_minimal() + ylab("") + xlab("") +
    coord_flip()+
    scale_y_continuous(limits = c(0,max(res$Percent)+max(res$Percent)/7), expand=c(0,0)) +
    scale_x_discrete(expand=c(.1,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y  =  element_line(color = "grey"),
          #plot.margin(l = 2, unit = "cms"),
         # axis.text.y = element_text(angle=45,hjust=1),
          axis.text.x = element_blank(), legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          #legend.position = "bottom", 
          #  legend.position=c(0.5,slide17_leg),
          legend.margin=margin(t = 0.1,b=0,l=.1, unit='cm'))  + 
    guides(fill = "none")
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","plot_pq2.png"),plot=plot_pq2,width = 5, height=4.5,units="in")
  
 ############################ slide 29 eas of pick up
  d_factor<- function(x){factor(x,levels= c( "Extremely easy", "Somewhat easy", "Neither easy nor difficult",  "Somewhat difficult", 
                                              "Extremely difficult"))}
  tot <- data_grp_bar('Q102', stacked_labels = FALSE,filter_enr=F,comp=T) %>% 
    filter(!Location %in% c("Nation","Region")) %>% 
    mutate_at(vars(dplyr::starts_with('Q102')),d_factor) %>%
    mutate(q='') 
  names(tot)[1] <- "Freq"
  
  
  tot$label_col <- ifelse(tot$Freq %in% c("Neither easy nor difficult",  "Somewhat difficult", 
                                          "Extremely difficult"),"white","black")
  tot$hjust_var <- ifelse(tot$Percent<=4,-3.9,.5)
  #adust label color if it is off the side to be black
  tot$label_col <- ifelse(tot$Percent<=4,'black', tot$label_col)
  
  
  slidePickup <- ggplot(tot,aes(x=q,y=Percent,fill=Freq,label=label)) + 
    geom_bar(stat="identity",width=.45)+ 
    geom_text_repel(aes(x=q,y=Percent,fill=Freq,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = .5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=rev(colors_stacked),labels=paste(" ",levels(tot$Freq))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "left", 
          plot.margin = margin(l=1.5, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0,l=1, r=1,unit='cm')) + guides(color="none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slidePickup.png"),bg = "transparent", plot=slidePickup,width = 4.5, height=4,units="in")
  
  
  ##################### Slide 31- many graphs and tables
  
  
  res <- data.frame(table(data_school_c$Q103)) %>% subset(Var1!="") 
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
                  hjust = hjust, vjust = vjust)) +
    scale_fill_manual(values=colors[c(5,3)],breaks=c("Yes","No")) + 
    coord_fixed() +
    scale_x_continuous(limits = c(-1.6, 1.6),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.6, 1.6),    # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    theme_minimal() + guides(fill="none")
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","hasMobile.png"),plot=hasMobile,width = 3.7, height=3.7,units="in")
  
  
  
  
  res <- data.frame(table(data_school_c$Q104)) %>% subset(Var1!="") 
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

  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","hasKiosk.png"),plot=hasKiosk,width = 3.5, height=3.5,units="in")
  

  
  ##### Howoften order 
  
  
  oft <- data_school_c %>% filter(Q105 !="") %>% group_by(Q105) %>% summarize(n=n()) %>% 
    mutate(per=paste0(roundQual(100*n/sum(n)),"%")) %>%  complete(Q105=c("0 times / week",  "1-2 times / week", "3-5 times / week",
                      "6-9 times / week", "10+ times / week"),fill=list(n=0,per="0%")) %>% 
    mutate(Q105 = factor(Q105,levels=c("0 times / week",  "1-2 times / week", "3-5 times / week",
                                       "6-9 times / week", "10+ times / week"))) %>%
    arrange(Q105)
  
 oft_out <- oft %>% select(Q105,per) %>% gt() %>%cols_label("Q105"="","per"="") %>%
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

  
  gtsave(oft_out,filename = paste0(loc,UNIVERSITY_NAME,"/","oft_out.png"))
  
  # why not
  ############################ slide 29 eas of pick up
  qs <- c('Q107_1','Q107_4', 'Q107_5', 'Q107_6' )
  
  base <- data_school_c %>% select(qs) 
  
  
  res_b <- data_school_c %>% select(qs,ResponseId) %>% 
    mutate(id=row.names(.)) %>% 
    select(!dplyr::contains("_SDS")) %>%
    pivot_longer(cols=dplyr::starts_with("Q107")) %>%
    filter(value!="") %>%
    mutate(value = trimws(value)) %>%
    select(-id) %>%
    summarise(n=length(unique(ResponseId)))
  
  
  res <- data_school_c %>% select(qs,ResponseId) %>% 
    mutate(id=row.names(.)) %>% 
    select(!dplyr::contains("_SDS")) %>%
    pivot_longer(cols=dplyr::starts_with("Q107")) %>%
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
    pivot_longer(cols=dplyr::starts_with("Q107")) %>% 
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
  
  gtsave(not_out,filename = paste0(loc,UNIVERSITY_NAME,"/","not_out.png"))

  ###################  slide 31 - technology 
  
  
  res <- data.frame(table(data_school_c$Q108)) %>% subset(Var1!="") 
  base <- sum(res$Freq)
  res$per <- roundQual(res$Freq*100/base)
  res <- res %>% 
    arrange(Var1=="No") %>% 
    mutate(label=paste0(Var1,": " ,per,"%"))
  

  
  # create labels
  res$label_col <- ifelse(res$per<=4,'black', "white")
  res$hjust_var <- ifelse(res$per<=4,-2.5,.5)
  res$Var1 <- factor(res$Var1,levels=c("Yes","No"))
  
  
  df <- res %>% 
    mutate(end = 2 * pi * cumsum(per)/sum(per),
           start = lag(end, default = 0),
           middle = 0.5 * (start + end),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  
  tech_plot <-  ggplot(df) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                     start = start, end = end, fill = Var1)) +
    geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = label,
                  hjust = hjust, vjust = vjust)) +
    scale_fill_manual(values=colors[c(5,3)],breaks=c("Yes","No")) + 
    coord_fixed() +
    scale_x_continuous(limits = c(-1.6, 1.6),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1, 1.1),    # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    theme_minimal() + guides(fill="none")
  

  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","tech_plot.png"),plot=tech_plot,width = 3.7, height=3.7,units="in")
  
  
  ##technology 
  base <- data_school_c %>% select(dplyr::starts_with("Q109")) 
  # how many respondents made some selection for this
  base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
  
  res_s <- data_school_c %>%
    select(dplyr::starts_with("Q109")) %>% select(!dplyr::contains("_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q109")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" (please specify):","",value,fixed=T)) %>% 
    group_by(value) %>% summarize(n=100*n()/base_n)  %>% arrange(desc(n)) %>% 
    arrange(value %in% c("None of the above","Other"))
  
  res_all <- data %>%
    select(dplyr::starts_with("Q109")) %>% select(!dplyr::contains("_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q109")) %>%
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
    geom_text(aes(x=value,y=n + max(res$Percent)/7,label=label),size = 4, position = position_dodge(width = .9)) +
    coord_flip() + 
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent+max(res$Percent)/7+10)), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          #axis.text.y=element_text(size=13),
          axis.text.x = element_blank(), legend.title = element_blank(),
          # legend.position = "bottom", 
          legend.position=c(0.4,-0.03),
          legend.margin=margin(t = 0, unit='cm')) 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","plotTech.png"),plot=plotTech,width = 4, height=3.5,units="in")
  
  
  
  ####### Slide 21b enrollment status

  year<- data_grp_bar('Q1.1',comp=T)
  year <-year %>% filter(Location==UNIVERSITY_NAME)
  year$Q1.1 <- gsub(" (please specify)","",year$Q1.1,fixed=T)
  year$Q1.1 <- gsub("year student","Year",year$Q1.1)
  year$Q1.1 <- gsub("Graduate student","Graduate Student",year$Q1.1)
  year$Value <- factor(year$Q1.1)
  
  levels <-  c("1st Year",
               "2nd Year",
               "3rd Year",
               "4th Year",
               "5th Year",
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
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide21b.png"),plot=plot21b,width = 4, height=4,units="in")
  
   ## slide 21c the table
  oncamp<- data_grp_bar('Q1.5',comp=T)
  oncamp <- oncamp%>% filter(Location==UNIVERSITY_NAME)
  oncamp$Value <- gsub("walking distance","walking\ndistance",oncamp$Q1.5,fixed=T)
  oncamp$Value <- gsub("drive or take public transportation","drive\npublic transportation",oncamp$Value,fixed=T)
  oncamp$Value <- factor(oncamp$Value,levels = rev(c("On-Campus", "Off-Campus (walking\ndistance)", "Off-Campus (drive\npublic transportation)")))
   
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
  
  
  ####### Slide 22 communiting
 
  ## Slide 23
  top_two_dot <- function(x,dataset){
    roundQual(100*sum(grepl("excellent|good|^agree|strongly agree|definitely will buy|probably will buy",tolower(dataset[,x])))/sum(!is.na(dataset[,x]) & (dataset[,x]!="") & (dataset[,x]!="Not Applicable/Don't Know")),0)
  }
  
  SAT <- top_two("Q2.1",data_school_c) # satifaction for first grapjh
  VAL <- top_two("Q2.4",data_school_c)
  MEAL <- top_two("Q4.2",data_school_c)
  
  # CA does not 
  if (is_ca){
    QUAL <- top_two_dot("Q63ca_1",data_school_c)
    VAR  <- top_two_dot("Q63ca_2",data_school_c)
    NUT <- top_two_dot("Q63ca_3",data_school_c)
    ALL <- top_two_dot("Q63ca_4",data_school_c)
    HEALTH <- top_two_dot("Q63ca_5",data_school_c)
    SPEC <- top_two_dot("Q63ca_7",data_school_c)
    ORG <- top_two_dot("Q63ca_10",data_school_c)
    FRESH <- top_two_dot("Q63ca_8",data_school_c)
    
    PRICE <- top_two_dot("Q63ca_6",data_school_c)
    AFFORD <- top_two_dot("Q63ca_9",data_school_c)
  }else {
  QUAL <- top_two_dot("Q63_1",data_school_c)
  VAR  <- top_two_dot("Q63_2",data_school_c)
  NUT <- top_two_dot("Q63_3",data_school_c)
  ALL <- top_two_dot("Q63_4",data_school_c)
  
  HEALTH <- top_two_dot("Q63_5",data_school_c)
  SPEC <- top_two_dot("Q63_7",data_school_c)
  ORG <- top_two_dot("Q63_10",data_school_c)
  FRESH <- top_two_dot("Q63_8",data_school_c)
  
  PRICE <- top_two_dot("Q63_6",data_school_c)
  AFFORD <- top_two_dot("Q63_9",data_school_c)
  }
  

  
  CONV <- top_two_dot("Q64_1",data_school_c)
  WELCOME <- top_two_dot("Q64_2",data_school_c)
  KNOW <- top_two_dot("Q64_3",data_school_c)
  SPEED <- top_two_dot("Q64_4",data_school_c)
  
  CLEAN <- top_two_dot("Q64_5",data_school_c)
  HOURS <- top_two_dot("Q64_6",data_school_c)
  SOCIAL <- top_two_dot("Q64_7",data_school_c)
  COMFORT <- top_two_dot("Q64_8",data_school_c)
  
  graphs <- list(SAT,VAL,MEAL,QUAL,VAR,NUT,ALL,HEALTH,SPEC,ORG,FRESH,PRICE,AFFORD,
                 CONV,WELCOME,KNOW,SPEED,CLEAN,HOURS,SOCIAL,COMFORT)
  names(graphs) <- c("sat","val","meal","quality","variety","nutrition","allergy",
                     "healthy","specialdiet","organic","fresh","price","afford","conv","welcome","know","speed",
                     "clean","hours","social","comfort")
  
  if (is_ca){ # CA doesnt have meal purchasing
    graphs <- list(SAT,VAL,QUAL,VAR,NUT,ALL,HEALTH,SPEC,ORG,FRESH,PRICE,AFFORD,
                   CONV,WELCOME,KNOW,SPEED,CLEAN,HOURS,SOCIAL,COMFORT)
    names(graphs) <- c("sat","val","quality","variety","nutrition","allergy",
                       "healthy","specialdiet","organic","fresh","price","afford","conv","welcome","know","speed",
                       "clean","hours","social","comfort")
    
  }
  
  get_x <- function(x){
    if (x<50){
      return(-.6)
    } else if( x < 80){
      return(-.3)
    } else {
      return(.5)
    }
  }
  
  get_y <- function(x){
    if (x<50){
      return(.25)
    } else if (x<80){
      return(.6)
    } else {
      return(.4)
    }
  }
  for (i in 1:length(graphs)){
    
  png(filename = paste0(loc,UNIVERSITY_NAME,"/",names(graphs)[i],"_donut.png"))
  par(mar = rep(0, 4), bg="transparent")
  plot(c(-1, 1), c(0, 1), bg="transparent",type = "n", axes = FALSE, ann = FALSE, asp = 1,mar=c(0, 0, 0, 0))
  draw.sector(0, 180,clock.wise = F , rou1 = 1, rou2 = 0.3,  col = "#f2f2f2",border="#f2f2f2")
  draw.sector(180*(1-graphs[i][[1]]/100),180, clock.wise=F,rou1 = 1, rou2 = 0.3,  col = two_tone[1],border=two_tone[1])
  # Calculate position for label 
  text(x = get_x(graphs[i][[1]]),y=get_y(graphs[i][[1]]),paste0(graphs[i][[1]],"%"),cex=5,col="white")
  dev.off()
  
  }

  
  #####################################
  #### APPENDIX graphs_fall_2022/
  ####################################
  
  ########################
  
  # Slide 28, left
  sat <- data_grp_bar('Q2.4', appendix=TRUE, stacked_labels=TRUE,comp=T)
  sat$Value <- as.factor(sat$Q2.4)
  sat$label_col <- ifelse(sat$Value %in% c("Excellent value","Good value"),"black","white")
  sat$hjust_var <- ifelse(sat$Percent<=4,-1.6,.5)
  #adust label color if it is off the side to be black
  sat$label_col <- ifelse(sat$Percent<=4,'black', sat$label_col)
  sat$Value <- factor(sat$Value, levels <- c( "Excellent value", "Good value","Average value", "Poor value", 
                                              "Terrible value"))
  
  sat$Location <- factor(sat$Location,levels=c(UNIVERSITY_NAME_WRAP,"Region", "Nation","Market Segment"))
  
  plot28a <- ggplot(sat,aes(x=Location,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=Location,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,
              size = 4, position = position_stack(vjust = 0.5),segment.alpha=0,segment.color= "white") +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(" ",c("Excellent","Good","Average","Poor","Terrible"))) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          # legend.position = "left", 
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = margin(l=1,r=.1, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0, r = .7, unit='cm')) + guides(color=F)
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide28a.png"),plot=plot28a,width = 5, height=4,units="in",bg = "transparent")
  
  # Slide 28, right
  # Like Slide 8 but with market comparion
  meaning_mar <- data_grp_bar('Q62', appendix=TRUE,comp=T)
  
  #clean up the text answers
  meaning_mar$Value <- gsub('Value to me is about ', '', meaning_mar$Q62)
  meaning_mar$Value <- paste0(toupper(substr(meaning_mar$Value, 1, 1)), substr(meaning_mar$Value, 2, nchar(meaning_mar$Value)))
  
  meaning_mar$Value <- factor(meaning_mar$Value,
                              levels = c("I do not care about value", 
                                         "How much food I receive",
                                         "How much I pay",
                                         "The quality of the food",
                                         "The overall experience"))
  meaning_mar$Location <- factor(meaning_mar$Location,levels= c(UNIVERSITY_NAME, "Region","Nation",'Market Segment'))
  
  orderdf <- meaning_mar %>% subset(Location==UNIVERSITY_NAME) %>% 
    arrange(desc(Percent)) %>% 
    select(Value)
  
  
  meaning_mar$Value <- factor(meaning_mar$Value,levels=rev(orderdf$Value))
  meaning_mar$Location <- factor(meaning_mar$Location,levels=rev(c(UNIVERSITY_NAME, "Region","Nation","Market Segment")))
  
  plot25b<- ggplot(meaning_mar,aes(x=Value,y=Percent,fill=Location,label=label)) + 
    geom_bar(stat="identity",position="dodge")+ 
    geom_text(aes(x=Value,y=Percent + max(meaning_mar$Percent)/15,label=label),size = 4, position = position_dodge(width = .9)) +
    coord_flip() + 
    scale_fill_manual(breaks = rev(c(UNIVERSITY_NAME, "Region","Nation","Market Segment")),values=rev(four_tone),
                      labels=rev(c(paste(" ",UNIVERSITY_NAME_MED,"  "),"   Region   ","   Nation  ","  Market Segment  "))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(meaning_mar$Percent)+10), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.x = element_blank(), legend.title = element_blank(),
          plot.margin = margin(b=.7, unit="cm"),
          #legend.position = "bottom", 
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position=c(0.35,-.07),
          legend.margin=margin(t=0,b=.05, unit='cm'))  + 
    guides(fill = guide_legend(reverse = TRUE, nrow=2))
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide28b.png"),plot=plot25b,width = 5, height=4,units="in",bg = "transparent")
  
  # 
  # 
  ## Slide 27 
  # like slide 13
  if(!is_ca){
  plan_mar <- data_grp_bar('Q4.2', stacked_labels = TRUE, appendix = TRUE,filter_enr=T)
  plan_mar$Value <- as.factor(plan_mar$Q4.2)
  plan_mar$Value <- unlist(lapply(plan_mar$Value,FUN=function(x){paste(strwrap(x,35),collapse="\n")}))
  
  plan_mar$label_col <- ifelse(plan_mar$Value %in% c("Definitely will buy","Probably will buy"),"black","white")
  plan_mar$hjust_var <- ifelse(plan_mar$Percent<=4,-1.6,.5)
  #adust label color if it is off the side to be black
  plan_mar$label_col <- ifelse(plan_mar$Percent<=4,'black', plan_mar$label_col)
  
  
  
  plan_mar$Value <- factor(plan_mar$Value, levels <- c("Definitely will buy", "Probably will buy", "Might or might not buy", 
                                                       "Probably will not buy","Definitely will not buy"))
  
  plan_mar$Location <- factor(plan_mar$Location,levels=c(UNIVERSITY_NAME_WRAP,"Region", "Nation","Market Segment"))
  
  plot27a <- ggplot(plan_mar,aes(x=Location,y=Percent,fill=Value,label=label)) + 
    geom_bar(stat="identity",width=.5)+ 
    geom_text_repel(aes(x=Location,y=Percent,fill=Value,label=label,color=label_col,hjust=hjust_var),force=.25,direction="y",point.padding = NA,segment.alpha=0,segment.color= "white",size = 4, position = position_stack(vjust = 0.5)) +
    scale_x_discrete(position = "top")  + 
    scale_fill_manual(values=colors,labels=paste(c("Definitely","Probably","Maybe","Probably Not","Definitely Not")," ")) +
    scale_color_manual(values=c("black","white"))+
    theme_minimal() + ylab("") + xlab("") + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), legend.title = element_blank(),
          #legend.position = "left", 
          panel.spacing = unit(1.3, "cm"),
          plot.margin = margin(l=1.3, unit="cm"),
          legend.position=c(0.01,0.5),
          legend.margin=margin(t = 0, r = 1.3, unit='cm')) + guides(color=F)
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide29a.png"),plot=plot27a,width = 5, height=4,units="in")
  
  }
  ####### Slide 29b Purchasing requirements
  data_27b <- data_grp_bar('Q4.1', appendix=TRUE,comp=T)
  data_27b$Value <- as.factor(data_27b$Q4.1)
  data_27b$Value <- unlist(lapply(data_27b$Value,FUN=function(x){paste(strwrap(x,35),collapse="\n")}))
  data_27b$Location <- factor(data_27b$Location,levels= c('Market Segment',"Nation","Region",UNIVERSITY_NAME))
  orderdf <- data_27b %>% subset(Location==UNIVERSITY_NAME) %>% 
    arrange(desc(Percent)) %>% 
    select(Value)
  
  
  data_27b$Value <- factor(data_27b$Value,levels=rev(orderdf$Value))
  plot27b<- ggplot(data_27b,aes(x=Value,y=Percent,fill=Location,label=label)) + 
    geom_bar(stat="identity",position="dodge")+ 
    geom_text(aes(x=Value,y=Percent + max(data_27b$Percent)/10,label=label),size = 3, position = position_dodge(width = .9)) +
    coord_flip() + 
    scale_fill_manual(breaks = c("Market Segment", "Nation","Region",UNIVERSITY_NAME),values=rev(four_tone),labels=c("  Market Segment  ","   Nation  ","   Region   ",paste(" ",UNIVERSITY_NAME_SHORT,"  "))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(data_27b$Percent)+15), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.y =  element_line(color = "grey"),
          axis.text.x = element_blank(), legend.title = element_blank(),
          #legend.position = "bottom", 
          panel.border = element_blank(),
          plot.background = element_rect(fill = "#f2f2f2",linetype=0),
          panel.background = element_rect(fill = "#f2f2f2",linetype=0),
          plot.margin = margin(b=.7, unit="cm"),
          #legend.position = "bottom", 
          legend.position=c(0.05,-.1),
          legend.margin=margin(t=.04,b=.02, unit='cm'))  + 
    guides(fill = guide_legend(reverse = TRUE,ncol=2))
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide29b.png"),plot=plot27b,width = 4, height=2.65,units="in")
  
  
  
  
  ### 30 diet
  eligible = c('Milk', 'Eggs', 'Peanuts', 'Tree nuts', 'Fish', 'Shellfish', 'Soy', 
               'Wheat/Gluten', 'Sesame',  'Other')
  if (is_ca){
    eligible = c(eligible, 'Mustard')
  }
  
  
  res_sch <- data_school_c %>% filter(Q69_1=="Food allergies or intolerances")%>% 
    select(dplyr::starts_with("Q70")) %>% select(!dplyr::contains("Q70_10_TEXT")) %>%
    select(!dplyr::contains("Q70_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q70")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n)) %>% 
    mutate(Location=UNIVERSITY_NAME_SHORT,value=gsub(" \\(please specify\\)","",value))%>% 
    arrange(value%in%c("Other")) %>%
    filter(value %in% eligible) %>%
    filter(!value %in%c( "None of the above")) 
 
  
  res_reg <- data_region_c %>% filter(Q69_1=="Food allergies or intolerances")%>% 
    select(dplyr::starts_with("Q70")) %>% select(!dplyr::contains("Q70_10_TEXT")) %>%
    select(!dplyr::contains("Q70_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q70")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_region_c))  %>% arrange(desc(n)) %>% 
    mutate(Location="Region",value=gsub(" \\(please specify\\)","",value))  %>% 
    filter(!value %in%c( "None of the above")) %>%
    filter(value %in% eligible) %>%
    filter(value %in% res_sch$value) #only compare to ones the school has, otherwise oto many
    
 
  res_nat <- data_c %>% filter(Q69_1=="Food allergies or intolerances")%>% 
    select(dplyr::starts_with("Q70")) %>% select(!dplyr::contains("Q70_10_TEXT")) %>%
    select(!dplyr::contains("Q70_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q70")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_c))  %>% arrange(desc(n)) %>% 
    mutate(Location="Nation",value=gsub(" \\(please specify\\)","",value)) %>% 
    filter(!value %in%c( "None of the above")) %>%
    filter(value %in% eligible) %>%
    filter(value %in% res_sch$value) #only compare to ones the school has, otherwise oto many
  
  #in order to limit, now national can only include ones
  
  
  
  res_mark <- data_market_c %>% filter(Q69_1=="Food allergies or intolerances")%>% 
    select(dplyr::starts_with("Q70")) %>% select(!dplyr::contains("Q70_10_TEXT")) %>%
    select(!dplyr::contains("Q70_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q70")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_market_c))  %>% arrange(desc(n)) %>% 
    mutate(Location= "Market Segment",value=gsub(" \\(please specify\\)","",value)) %>% 
    filter(value %in% eligible) %>%
    filter(!value %in%c( "None of the above")) %>%
    filter(value %in% res_sch$value) #only compare to ones the school has, otherwise oto many
  
  
  res <- bind_rows(res_sch,res_reg,res_nat,res_mark)

  cleaned <- c()
  
  for (value in res$value){
    value_2 <- ifelse(nchar(value)>30, 
                      ifelse(grepl(',', value), 
                             gsub(",.*", "", value), 
                             ifelse(grepl('!', value), gsub("!.*", "", value), paste0(substr( value, 1, 27), '...')))
                      , value)
    cleaned <- c(cleaned, value_2)
  }
  
  res$value <- cleaned
  
  res$label<- paste0(roundQual(res$n,0),"%")
  

  
  res <- res %>% filter(!value %in%c( "None of the above")) 
  levels <-  c(res_sch$value,unique(res$value[which (!res$value %in% res_sch$value)]))
  levels <- c(levels[levels!="Other"],"Other")
  
  res$value <- factor(res$value, levels = levels)
  res$Percent <- res$n
  res$Location <- factor(res$Location,levels=rev(c("Market Segment","Nation","Region",UNIVERSITY_NAME_SHORT)))
  
  plot30<- ggplot(res,aes(x=value,y=Percent,label=label,fill=Location)) + 
    geom_bar(stat="identity",position="dodge")+ 
    geom_text(aes(x=value,y=Percent + max(Percent)/15,label=label),size = 3, position = position_dodge(width = .9)) +
    scale_fill_manual(breaks = rev(c("Market Segment","Nation","Region",UNIVERSITY_NAME_SHORT)),values=four_tone,
                      labels= rev(c(" Market Segment"," Nation"," Region",paste(" ",UNIVERSITY_NAME_SHORT,"  ")))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent)+max(res$Percent)/7), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.x =  element_line(color = "grey"),
          axis.text.x = element_text(angle=45,hjust=1),
          axis.text.y = element_blank(), legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = "bottom", 
          #legend.position=c(0.5,slide17_leg),
          legend.margin=margin(t =- 0.5,b=0,l=.1, unit='cm')) 

  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide30.png"),
         plot=plot30,width =9, height=3,units="in",bg="transparent")
  
  ### 31 restrictions
  eligible = c('Vegetarian', 'Vegan', 'Halal', 'Kosher', 'Paleo/Primal', 'Ketogenic', 'Mediterranean')
  if (!is_ca){
    eligible = c(eligible, c('Atkins', 'GMO-Free', 'Whole 30', 'Plant-based', 'Other',"Low-FODMAP","Pescatarian"))
  } else {
    eligible = c(eligible, c( 'Flexitarian', 'No Peanut/Tree Nut', 'No Dairy', 'No Gluten', 'Other'))
  }
  
  
  
  
  res_sch <- data_school_c %>% filter(Q69_2=="Special dietary lifestyle for personal preference or religious reasons")%>% 
    select(dplyr::starts_with("Q71")) %>% select(!dplyr::contains("Q71_14_TEXT")) %>%
    select(!dplyr::contains("Q71_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q71")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n)) %>% 
    mutate(Location=UNIVERSITY_NAME_SHORT,value=gsub(" \\(please specify\\)","",value))%>% 
    arrange(value%in%c("Other")) %>% filter(!value %in%c( "None of the above"))%>% filter(value %in% eligible) 
  

  
  res_reg <- data_region_c %>% filter(Q69_2=="Special dietary lifestyle for personal preference or religious reasons")%>% 
    select(dplyr::starts_with("Q71")) %>% select(!dplyr::contains("Q71_14_TEXT")) %>%
    select(!dplyr::contains("Q71_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q71")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_region_c))  %>% arrange(desc(n)) %>% 
    mutate(Location="Region",value=gsub(" \\(please specify\\)","",value))%>% filter(!value %in%c( "None of the above")) %>% filter(value %in% eligible) 
  
  
  res_nat <- data_c %>% filter(Q69_2=="Special dietary lifestyle for personal preference or religious reasons")%>% 
    select(dplyr::starts_with("Q71")) %>% select(!dplyr::contains("Q71_14_TEXT")) %>%
    select(!dplyr::contains("Q71_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q71")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_c))  %>% arrange(desc(n)) %>% 
    mutate(Location="Nation",value=gsub(" \\(please specify\\)","",value))%>% filter(!value %in%c( "None of the above")) %>% filter(value %in% eligible) 
  

  
  res_mark <- data_market_c %>% filter(Q69_2=="Special dietary lifestyle for personal preference or religious reasons")%>% 
    select(dplyr::starts_with("Q71")) %>% select(!dplyr::contains("Q71_14_TEXT")) %>%
    select(!dplyr::contains("Q71_SDS")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q71")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_market_c))  %>% arrange(desc(n)) %>% 
    mutate(Location= "Market Segment",value=gsub(" \\(please specify\\)","",value))%>% filter(!value %in%c( "None of the above")) %>% filter(value %in% eligible) 
  
  
  res <- bind_rows(res_sch,res_reg,res_nat,res_mark)
  
  res$label<- paste0(roundQual(res$n,0),"%")
  
  res <- res %>% filter(!value %in%c( "None of the above"))  
  levels <-  c(res_sch$value,unique(res$value[which(!res$value %in% res_sch$value)]))
  levels <- c(levels[levels!="Other"],"Other")
  
  res$value <- factor(res$value, levels = levels)
  res$Percent <- res$n
  res$Location <- factor(res$Location,levels=rev(c("Market Segment","Nation","Region",UNIVERSITY_NAME_SHORT)))
  
  plot31<- ggplot(res,aes(x=value,y=Percent,label=label,fill=Location)) + 
    geom_bar(stat="identity",position="dodge")+ 
    geom_text(aes(x=value,y=Percent + max(Percent)/15,label=label),size = 2, position = position_dodge(width = .9)) +
    scale_fill_manual(breaks = rev(c("Market Segment","Nation","Region",UNIVERSITY_NAME_SHORT)),values=four_tone,
                      labels= rev(c(" Market Segment"," Nation"," Region",paste(" ",UNIVERSITY_NAME_SHORT,"  ")))) +
    theme_minimal() + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0,max(res$Percent)+max(res$Percent)/7), expand=c(0,0)) +
    theme(panel.grid = element_blank(),
          axis.line.x =  element_line(color = "grey"),
          axis.text.x = element_text(angle=45,hjust=1),
          axis.text.y = element_blank(), legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = "bottom", 
          #legend.position=c(0.5,slide17_leg),
          legend.margin=margin(t = -0.5,b=0,l=.1, unit='cm')) 
  
  
  ggsave(filename = paste0(loc,UNIVERSITY_NAME,"/","slide31.png"),
         plot=plot31,width =9, height=3,units="in",bg="transparent")
  ## Table 1
  # barriers
  get_dat_table <- function(data,name_) {
    t <- data %>% 
      select("Q67") %>% 
      mutate(id=row.names(.)) %>% pivot_longer(cols="Q67") %>%
      filter(value!="") %>%
      group_by(value) %>% summarize(n=100*n()/nrow(data))  %>% arrange(desc(n)) %>%
      mutate(PCT=paste0(roundQual(n,0),"%"),value = gsub(" \\(e\\.g.*)","",value)) %>%
      mutate(value= gsub(" \\(please specify)","",value)) %>%
     # mutate(value = gsub("meal","dining",value)) %>% 
      select(value,PCT) %>%
      rename(!!enquo(name_):=PCT) %>% 
      arrange(value%in%c("Other")) # move other to end
  
    return(t)
  } 
  
  if(!is_ca){
  # only fo rthose who wont purchase
  t1 <- get_dat_table(data_school_c  %>% filter(Q4.2 %in%c("Probably will not buy","Definitely will not buy","Might or might not buy") ),!!enquo(UNIVERSITY_NAME))
  t2 <- get_dat_table(data_region_c %>% filter(Q4.2 %in%c("Probably will not buy","Definitely will not buy","Might or might not buy") ),"Regional")
  t3 <- get_dat_table(data_c %>% filter(Q4.2 %in%c("Probably will not buy","Definitely will not buy","Might or might not buy") ),"National")
  t4 <- get_dat_table(data_market_c %>% filter(Q4.2 %in%c("Probably will not buy","Definitely will not buy","Might or might not buy") ),"Market Segment")
  
  merge_all <- function(x,y){merge(x,y,all=T)}
  
  table1 <- Reduce(merge_all,list(t1,t2,t3,t4))
  table1[is.na(table1)] <- "0%"
  table1 <- table1 %>% 
    arrange(desc(as.numeric(gsub("%","",.[[2]])))) %>%
    arrange(value%in%c("Other")) 

  #names(table1) <- paste("Average for",names(table1))
  
  table1_out <- table1 %>% gt() %>%cols_label("value"="") %>%
    cols_align(
    align = "center",
    columns= c(2:5)
    ) %>%  
    cols_width(
      c(2:5) ~ px(120) 
    )
  
  gtsave(table1_out,filename = paste0(loc,UNIVERSITY_NAME,"/","appendix1.png"))
  }
 
  ## Table 2-3
  # barriers
  
  get_dat_table2 <- function(data,name_) {
    if (is_ca==F){
      base <- nrow(data%>% filter(Q1.8=="Yes" ))
    } else {
      base <- nrow(data%>% filter(Q1.8ca=="Yes" ))
    }
    
    t<- data %>% select(dplyr::starts_with("Q68")) %>% select(!dplyr::contains("Q68_9_TEXT")) %>%
      select(!dplyr::contains("Q68_SDS")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q68")) %>%
      filter(value!="") %>%
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n)) %>%
      mutate(PCT=paste0(roundQual(n,0),"%"),value = gsub(" \\(e\\.g.*)","",value)) %>%
      mutate(value= gsub(" \\(please specify)","",value)) %>%
      #mutate(value = gsub("meal plan","dining plan",value)) %>% 
      #mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
      select(value,PCT) %>%
      rename(!!enquo(name_):=PCT) 
    
    # re order other and none to the bottom
    t$value <- factor(t$value,levels=c(t$value[!t$value %in% c("Other","None of these")],"Other","None of these"))
    t <- t %>% arrange(value)
    t$value <- as.character(t$value)
    
    return(t)
  } 
  
  # only fo rthose who wont purchase
  if (is_ca ==F){
    t1 <- get_dat_table2(data_school_c %>% filter(Q1.8=="Yes" )%>% filter(Q67!="Graduating"),!!enquo(UNIVERSITY_NAME))
    t2 <- get_dat_table2(data_region_c %>% filter(Q1.8=="Yes" )%>% filter(Q67!="Graduating"),"Regional")
    t3 <- get_dat_table2(data_c %>% filter(Q1.8=="Yes" )%>% filter(Q67!="Graduating"),"National")
    t4 <- get_dat_table2(data_market_c %>% filter(Q1.8=="Yes" )%>% filter(Q67!="Graduating"),"Market Segment")
    
   
    # t1 <- get_dat_table2(data_school_c %>% filter(Q1.8ca=="Yes" )%>% filter(Q67!="Graduating"),!!enquo(UNIVERSITY_NAME))
    # t2 <- get_dat_table2(data_region_c %>% filter(Q1.8ca=="Yes" )%>% filter(Q67!="Graduating"),"Regional")
    # t3 <- get_dat_table2(data_c %>% filter(Q1.8ca=="Yes" )%>% filter(Q67!="Graduating"),"National")
    # t4 <- get_dat_table2(data_market_c %>% filter(Q1.8ca=="Yes" )%>% filter(Q67!="Graduating"),"Market Segment")
    
  

  table2 <- Reduce(merge_all,list(t1,t2,t3,t4))
  table2[is.na(table2)] <- "0%"
  
  table2 <- table2 %>% arrange(desc(as.numeric(gsub("%","",.[[2]]))))%>%
    arrange(value%in%c("Other")) %>%
    arrange(value%in%c("None of these"))
  
  #names(table2) <- paste("Average for",names(table2))
  
  table2_out <- table2 %>% head(10) %>% gt() %>%cols_label("value"="")%>%
    cols_align(
      align = "center",
      columns=c(2:5)
    )%>%  
    cols_width(
      c(2:5) ~ px(120)
      
    )
  table3_out <- table2 %>% tail(7) %>% gt() %>%cols_label("value"="")%>%
    cols_align(
      align = "center",
      columns=c(2:5)
    )%>%  
    cols_width(
     c(2:5) ~ px(120)
      
    )
  
  gtsave(table2_out,filename = paste0(loc,UNIVERSITY_NAME,"/","appendix2.png"))
  gtsave(table3_out,filename = paste0(loc,UNIVERSITY_NAME,"/","appendix3.png"))
  }
  
  # table 4
  res <- data_school_c %>% select(dplyr::starts_with("Q3.2")) %>% 
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q3.2")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_school_c)) %>% 
    top_n(10,n) %>% arrange(desc(n)) %>% 
    mutate(n=paste0(roundQual(n,0),"%"),value=gsub("}","",gsub("${e://Field/","",value,fixed=T)))
  
  # sat by venue
  sat <- data_school_c %>%
    group_by(SELECTED_OUTLET) %>%
    summarize(sat = paste0(roundQual(sum(Q3.3 %in% c("Extremely satisfied", 
                          "Somewhat satisfied"))*100/n(),0),"%"))
  sat$SELECTED_OUTLET <- gsub("Campion DIning Hall","Campion Dining Hall", sat$SELECTED_OUTLET )
  
  # merge in names
  outlets$cleanname <-trimws(outlets$SCHOOL_NAME)
  os <- outlets %>% filter(cleanname==!!enquo(UNIVERSITY_NAME)) %>%
    pivot_longer(cols=dplyr::starts_with("OUTLET_NAME")) %>% 
    select(name,value)%>%
    mutate(value=trimws(value))%>%
    rename(outlet=value)
  
  res2 <- merge(res,os,by.x = "value",by.y="name")
  res2 <- merge(res2,sat,by.x="outlet",by.y="SELECTED_OUTLET",all.x=T)
  
  table4 <- res2 %>% 
    arrange(desc(as.numeric(gsub("%","",.[[3]])))) %>%
    select(outlet,n,sat)
  names(table4) <-c("value","% Who Have Frequented Location This Semester","% Extremely/ Somewhat Satisfied with Specific 
Food Ordered")
  
  table4_out <- table4 %>% gt() %>%cols_label("value"="") %>%
    cols_align(
      align = "center",
      columns=  c("% Who Have Frequented Location This Semester","% Extremely/ Somewhat Satisfied with Specific 
Food Ordered")

    ) %>%  cols_align(
      align = "left",
      columns=  "value"
    ) %>%
    cols_width(
      c("% Who Have Frequented Location This Semester","% Extremely/ Somewhat Satisfied with Specific 
Food Ordered") ~ px(150)
      
    )
  
  gtsave(table4_out,filename = paste0(loc,UNIVERSITY_NAME,"/","appendix4.png"))

  
  
  ############# new appendix slides 47-50
  
  
  get_dat_table_92 <- function(data,name_) {
    base <- nrow(data %>% filter(Q68_14=="Ability to use my meal plan off campus"))
    
    t <- data %>% filter(Q68_14=="Ability to use my meal plan off campus") %>% 
      select(dplyr::starts_with("Q92")) %>% select(!dplyr::contains("Q92_6_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q92")) %>%
      filter(value!="") %>%
      mutate(value= gsub(" (please specify):","",value,fixed=T))%>% 
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n)) %>% 
      mutate(PCT=paste0(roundQual(n,0),"%"),value = gsub(" \\(e\\.g.*)","",value)) %>%
      mutate(value= gsub(" \\(please specify)","",value)) %>%
      # mutate(value = gsub("meal","dining",value)) %>% 
      select(value,PCT) %>%
      rename(!!enquo(name_):=PCT) %>% 
      arrange(value%in%c("Other")) # move other to end
    
    return(t)
  } 
  
  if(!is_ca){
  # only fo rthose who wont purchase
  t1 <- get_dat_table_92(data_school_c,!!enquo(UNIVERSITY_NAME))
  t2 <- get_dat_table_92(data_region_c ,"Regional")
  t3 <- get_dat_table_92(data_c,"National")
  t4 <- get_dat_table_92(data_market_c,"Market Segment")
  
  merge_all <- function(x,y){merge(x,y,all=T)}
  
  table1 <- Reduce(merge_all,list(t1,t2,t3,t4))
  table1[is.na(table1)] <- "0%"
  table1 <- table1 %>% 
    arrange(desc(as.numeric(gsub("%","",.[[2]])))) %>%
    arrange(value%in%c("Other")) 
  
  #names(table1) <- paste("Average for",names(table1))
  
  table47_out <- table1 %>% gt() %>%cols_label("value"="") %>%
    cols_align(
      align = "center",
      columns= c(2:5)
    ) %>%  
    cols_width(
      c(2:5) ~ px(120) 
    )
  
  gtsave(table47_out,filename = paste0(loc,UNIVERSITY_NAME,"/","appendix47.png"))
  
  }
  ########################### slide 48
  get_dat_table_93 <- function(data,name_) {
    base <- nrow(data)
    
    t <- data%>% 
      select(dplyr::starts_with("Q93")) %>% select(!dplyr::contains("_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q93")) %>%
      filter(value!="") %>%
      mutate(value= gsub(" (please specify):","",value,fixed=T))%>% 
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n)) %>% 
      mutate(PCT=paste0(roundQual(n,0),"%"),value = gsub(" \\(e\\.g.*)","",value)) %>%
      mutate(value= gsub(" \\(please specify)","",value)) %>%
      # mutate(value = gsub("meal","dining",value)) %>% 
      select(value,PCT) %>%
      rename(!!enquo(name_):=PCT) %>% 
      arrange(value%in%c("Other")) # move other to end
    
    return(t)
  } 
  
  # only fo rthose who wont purchase
  t1 <- get_dat_table_93(data_school_c,!!enquo(UNIVERSITY_NAME))
  t2 <- get_dat_table_93(data_region_c ,"Regional")
  t3 <- get_dat_table_93(data_c,"National")
  t4 <- get_dat_table_93(data_market_c,"Market Segment")
  
  merge_all <- function(x,y){merge(x,y,all=T)}
  
  table1 <- Reduce(merge_all,list(t1,t2,t3,t4))
  table1[is.na(table1)] <- "0%"
  table1 <- table1 %>% 
    arrange(desc(as.numeric(gsub("%","",.[[2]])))) %>%
    arrange(value%in%c("Other")) 
  
  #names(table1) <- paste("Average for",names(table1))
  
  table48_out <- table1 %>% gt() %>%cols_label("value"="") %>%
    cols_align(
      align = "center",
      columns= c(2:5)
    ) %>%  
    cols_width(
      c(2:5) ~ px(120) 
    )
  
  gtsave(table48_out,filename = paste0(loc,UNIVERSITY_NAME,"/","appendix48.png"))
  
  
  ################ and table of cuisine
  get_dat_table_94 <- function(data,name_) {
    base <- nrow(data)
    
    t <- data%>% 
      select(dplyr::starts_with("Q94")) %>% select(!dplyr::contains("_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q94")) %>%
      filter(value!="") %>%
      mutate(value= gsub(" (please specify):","",value,fixed=T))%>% 
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n)) %>% 
      mutate(PCT=paste0(roundQual(n,0),"%"),value = gsub(" \\(e\\.g.*)","",value)) %>%
      mutate(value= gsub(" \\(please specify)","",value)) %>%
      # mutate(value = gsub("meal","dining",value)) %>% 
      select(value,PCT) %>%
      rename(!!enquo(name_):=PCT) %>% 
      arrange(value%in%c("Other")) # move other to end
    
    return(t)
  } 
  
  # only fo rthose who wont purchase
  t1 <- get_dat_table_94(data_school_c,!!enquo(UNIVERSITY_NAME))
  t2 <- get_dat_table_94(data_region_c ,"Regional")
  t3 <- get_dat_table_94(data_c,"National")
  t4 <- get_dat_table_94(data_market_c,"Market Segment")
  
  merge_all <- function(x,y){merge(x,y,all=T)}
  
  table1 <- Reduce(merge_all,list(t1,t2,t3,t4))
  table1[is.na(table1)] <- "0%"
  table1 <- table1 %>% 
    arrange(desc(as.numeric(gsub("%","",.[[2]])))) %>%
    arrange(value%in%c("Other")) 
  
  #names(table1) <- paste("Average for",names(table1))
  
  table49_out1 <- table1[1:9,] %>% gt() %>%cols_label("value"="") %>%
    cols_align(
      align = "center",
      columns= c(2:5)
    ) %>%  
    cols_width(
      c(2:5) ~ px(120) 
    )
  
  table49_out2 <- table1[10:nrow(table1),] %>% gt() %>%cols_label("value"="") %>%
    cols_align(
      align = "center",
      columns= c(2:5)
    ) %>%  
    cols_width(
      c(2:5) ~ px(120) 
    )
  
  gtsave(table49_out1,filename = paste0(loc,UNIVERSITY_NAME,"/","appendix49_1.png"))
  gtsave(table49_out2,filename = paste0(loc,UNIVERSITY_NAME,"/","appendix49_2.png"))
  
  
  
  ############## slide 51 plant base food - two tables
  get_dat_table_95 <- function(data,name_) {
    base <- nrow(data)
    
    t <- data%>% 
      select(dplyr::starts_with("Q95")) %>% select(!dplyr::contains("_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q95")) %>%
      filter(value!="") %>%
      mutate(value= gsub(" (please specify):","",value,fixed=T))%>% 
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n)) %>% 
      mutate(PCT=paste0(roundQual(n,0),"%"),value = gsub(" \\(e\\.g.*)","",value)) %>%
      mutate(value= gsub(" \\(please specify)","",value)) %>%
      # mutate(value = gsub("meal","dining",value)) %>% 
      select(value,PCT) %>%
      rename(!!enquo(name_):=PCT) %>% 
      arrange(value%in%c("Other")) # move other to end
    
    return(t)
  } 
  
  # only fo rthose who wont purchase
  t1 <- get_dat_table_95(data_school_c,!!enquo(UNIVERSITY_NAME))
  t2 <- get_dat_table_95(data_region_c ,"Regional")
  t3 <- get_dat_table_95(data_c,"National")
  t4 <- get_dat_table_95(data_market_c,"Market Segment")
  
  merge_all <- function(x,y){merge(x,y,all=T)}
  
  table1 <- Reduce(merge_all,list(t1,t2,t3,t4))
  table1[is.na(table1)] <- "0%"
  table1 <- table1 %>% 
    arrange(desc(as.numeric(gsub("%","",.[[2]])))) %>%
    arrange(value%in%c("Other")) 
  
  #names(table1) <- paste("Average for",names(table1))
  
  yes <- table1[table1$value=="None of the above",] %>% 
    mutate(across(!contains("value"),function(x){paste0(100-as.numeric(gsub("%","",x)),"%")})) %>% 
    mutate(value="% Yes")
  
  table51_out1 <- yes %>% gt() %>%cols_label("value"="") %>%
    cols_align(
      align = "center",
      columns= c(2:5)
    ) %>%  
    cols_width(
      1 ~ px(240),
      c(2:5) ~ px(120) 
    )
  
  names(table1)[2] <- "University"
  
  table51_out2 <- table1[table1$value!="None of the above",] %>% gt() %>%
    cols_label("value"="","University" ="","Regional"="","National"="","Market Segment"="") %>%
    cols_align(
      align = "center",
      columns= c(2:5)
    ) %>%  
    cols_width(
      1 ~ px(240),
      c(2:5) ~ px(120) 
    )
  

  gtsave(table51_out1,filename = paste0(loc,UNIVERSITY_NAME,"/","appendix51_1.png"))
  gtsave(table51_out2,filename = paste0(loc,UNIVERSITY_NAME,"/","appendix51_2.png"))
  
  
}


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



