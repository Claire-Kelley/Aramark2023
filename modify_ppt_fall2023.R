library(officer)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
################################################################################################
########### ################################ Notes #############################################
#  variables that will come from the data are in all CAPS
#  There is a section at th ebottom with notes on helpful functions 
#  You can only write out to file if the file is not currently open 
#   use slide_summary(my_pres,index=1) to see the current contents of slide 1 and thier location
################################################################################################

################################################################################################
# Helper Functions
################################################################################################
# This function creates text that splits data into subsantially above, above, equal too, below, 
# substantially below
get_quartiles <- function(this_percent,national_average,all_percents){
  this_percent <- roundQual(this_percent,0)
  national_average <- roundQual(national_average,0)
  cut_points <- quantile(all_percents) 
    
  if(this_percent==national_average){
    return(paste0("\u2022   Equal to the national average of ",national_average,"%"))
  } else if(this_percent < national_average){
    if (this_percent < cut_points[2]){
      return(paste0("\u2022   Substantially below the national average of ",national_average,"%"))
    } else {
      return(paste0("\u2022   Below the national average of ",national_average,"%"))
    }
  } else {
    if (this_percent > cut_points[4]){
      return(paste0("\u2022   Substantially above the national average of ",national_average,"%"))
    } else {
      return(paste0("\u2022   Above the national average of ",national_average,"%"))
    }
  }
  
}


top_two <- function(x,dataset,dot_plot=F){
  if (dot_plot){
    roundQual(100*sum(grepl("excellent|good|^agree|somewhat agree|strongly agree|definitely will buy|probably will buy|^extremely comfortable|^very comfortable",tolower(dataset[,x])))/sum(!is.na(dataset[,x]) & (!dataset[,x]%in%c("","Not Applicable/Don't Know"))),0)
    
  } else {
  roundQual(100*sum(grepl("excellent|good|^agree|somewhat agree|strongly agree|definitely will buy|probably will buy|^extremely comfortable|^very comfortable",tolower(dataset[,x])))/sum(!is.na(dataset[,x]) & (dataset[,x]!="")),0)
  }
}

top_two_group <- function(x){

  100*sum(unlist(lapply(x[,1],FUN=function(x){grepl("excellent|good|^agree|somewhat agree|strongly agree|definitely will buy|probably will buy|^extremely comfortable|^very comfortable",tolower(x))})))/sum(!is.na(x[,1]) & x[,1]!="")
}

get_vector <- function(data,var){
  v <- data %>% select(var,SCHOOL_USA) %>%
    group_by(SCHOOL_USA) %>% 
    group_map(~ top_two_group(.x)) %>% 
    unlist()
  return(v[!is.na(v)])
}

n_format <- function(x){
  if (x<50){
    return("<50")
  } else {
    return(format(x,big.mark=","))
}
}

change <- function(this,last,year){
  if (this==last){
    return(paste0("\u2022   The same as ",year," scores."))
  } else if (this>last){
    return(paste0("\u2022   An increase of ",this-last,"% compared to ",year," scores."))
  } else {
    return(paste0("\u2022   A decrease of ",last-this,"% compared to ",year," scores."))
  }
}


roundQual <- function(x,n){floor(x+.5)}

################################################################################################
# read in data 

## Read in Data
#last_path <- "C:/Users/Claire/Documents/Consulting/Qualtrics/Aramark2023/AramarkFall2022/data_files"
last_path <- 'data_files'
### 

#last_data <- read.csv(paste0(last_path,"/spring2023_final.csv"),stringsAsFactors=FALSE)
#TODO: sarah to get file
last_data <- read.csv(paste0(last_path,"/spring2023_temp.csv"),stringsAsFactors=FALSE)

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

last_data_fall20 <- read.csv(paste0(last_path,'/fall2020_V2.csv'),stringsAsFactors=FALSE)
last_data_fall20 <- last_data_fall20[3:nrow(last_data_fall20),]


#data<- read.csv("C:/Users/Claire/Documents/Consulting/Qualtrics/Aramark2023/Aramark2023/Fall2023.csv",stringsAsFactors=FALSE)
data<- read.csv("Aramark_Dining_Base (Qual3538-0510DiningStyles)_October 16, 2023_10.19.csv",stringsAsFactors=FALSE)
data <- data[3:nrow(data),]


# merge in names
#TODO sarah to get outlest file
#outlets <- read.csv("C:/Users/Claire/Documents/Consulting/Qualtrics/Aramark2023/Aramark2023/Spring2023_outlets.csv")
outlets <- read.csv("Fall2022_outlets.csv")


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





#for working
UNIVERSITY_NAME <-  "University of Virginia"
data_last <- last_data
last_year ="Spring 2023"

run_report <- function(data,last_data,UNIVERSITY_NAME,ca=F,last_year) {
  
   graph_loc = "graphs_fall_2023/"

    # make student only data set 
    data_c <- data 
    data <- data %>% filter(grepl("student|Other",Q2))
    
    data_c_last <- last_data 
    data_last <- last_data %>% filter(grepl("student|Other",Q1.1))

    # School data 
    data_school_c <- data_c %>% filter(SCHOOL_USA== UNIVERSITY_NAME)
    data_school <- data %>% filter(SCHOOL_USA== UNIVERSITY_NAME) 
    
    data_school_c_last <- data_c_last %>% filter(SCHOOL_USA== UNIVERSITY_NAME)
    data_school_last <- data_last %>% filter(SCHOOL_USA== UNIVERSITY_NAME) 
    
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
    
    #TODO: Claire are we sure about this including other?
    data_region_c_last <- filter(data_c_last, REGION_NAME==REGION)
    data_region_last <- data_region_c_last %>% filter(grepl("student|Other",Q1.1))
    
    # add region to those without it
    REGION <- ifelse(!grepl("Region",REGION),paste0(REGION," Region"),REGION)
    
   
    #get market segment
    market_seg <- data_c %>% filter(SCHOOL_USA== UNIVERSITY_NAME) %>% 
      select(MARKET_SEGMENT) %>% slice(1)
    MARKET_SEG <- market_seg[[1]]
    data_market_c <- filter(data_c, MARKET_SEGMENT==MARKET_SEG)
    data_market <- data_market_c %>% filter(grepl("student|Other",Q2))
    
    data_market_c_last <- filter(data_c_last, MARKET_SEGMENT==MARKET_SEG)
    data_market_last <- data_market_c_last %>% filter(grepl("student|Other",Q1.1))

    
    
    ################################################################################################
    # Read in template 
    if (!ca){
      my_pres <- read_pptx("DSS F2023 USA report template.pptx") 
    } else {
      my_pres <- read_pptx("Template Spring 2023 CA.pptx") 
    }
    
    
    VERYLONG <- nchar(UNIVERSITY_NAME) >30
   
    UNIVERSITY_NAME_SHORT <- ifelse(nchar(UNIVERSITY_NAME) >= 23, paste(strwrap(UNIVERSITY_NAME,23),collapse = "\n  "), UNIVERSITY_NAME)
  
    
    add_sample <- function(p1,p2,p3,p4,p5=NULL) {
      sample_size1 <- fpar(
        ftext(p1,
              fp_text( font.size = 7)))
      sample_size2 <- fpar(
        ftext(p2,
              fp_text( font.size = 7)))
      sample_size3 <- fpar(
        ftext(p3,
              fp_text( font.size = 7)))
      sample_size4 <- fpar(
        ftext(p4,
              fp_text( font.size = 7)))
      if(!is.null(p5)){
        sample_size5 <- fpar(
          ftext(p5,
                fp_text( font.size = 7)))
        return(list(sample_size1,sample_size2,sample_size3,sample_size4,sample_size5))
      } else{
        return(list(sample_size1,sample_size2,sample_size3,sample_size4))
      }
      
    }

    ################################################################################################
    # Slide One - Change Title To university name 
    # TODO: move line down if the title breaks over two lines
    # first title is smaller if name is very long 
    print("at slide 1")
    needs_wrap <- (length(strwrap(UNIVERSITY_NAME,44))>2)
    
    if (needs_wrap){
      bold_face <- update(shortcuts$fp_bold(font.size = 16),color="white")
    } else {
      bold_face <- update(shortcuts$fp_bold(font.size = 18),color="white")
    }
    

    
    
    university_par <- fpar(
      ftext(UNIVERSITY_NAME , prop = bold_face),fp_p = fp_par(text.align="right") )
    
    my_pres <-  my_pres %>% on_slide(index=1) %>%
      ph_with_fpars_at(fpars=list(university_par), 
                       left=1.75,top=-.29,height=1,width=8
      ) 
    
    ####### SLide 2
    ################################################################################################
    
    # Add Sample and Nsize with formatting for numbers 
    UNIVERSITY_N <- n_format(nrow(data_school_c)) # sample size for universty 
    REGION_N <- n_format(nrow(data_region_c)) # sample size for this region
    NATIONAL_N <- n_format(nrow(data_c)) # sample size for entire study
    
    university_name_par <- fpar(
      ftext(UNIVERSITY_NAME,prop =  fp_text(font.size = 16)),fp_p = fp_par(text.align="center"))
    
    university_par <- fpar(
      ftext(paste0("n=",UNIVERSITY_N ),prop = fp_text(font.size=16)),fp_p = fp_par(text.align="center") )
    

    region_name_par <- fpar(
      ftext(REGION,prop =  fp_text(font.size = 12)),fp_p = fp_par(text.align="center"))
    
    region_par <- fpar(
      ftext(paste0("n=",REGION_N ),prop = fp_text(font.size=12)),fp_p = fp_par(text.align="center") )
    
    national_name_par <- fpar(
      ftext("National",prop =  fp_text(font.size = 12)),fp_p = fp_par(text.align="center"))
    
    national_par <- fpar(
      ftext(paste0("n=",NATIONAL_N ),prop = fp_text(font.size=12)),fp_p = fp_par(text.align="center") )
  
    s1 <- paste0('Among Total Respondents (n=', UNIVERSITY_N, '), Among Students/Other (n=', n_format(nrow(data_school)), ')')
    s2 <- "Q2. Which of the following best describes you?"
    s3 <- " Q4. Where do you live?"
      
    
    
    
    my_pres <- my_pres %>% on_slide(index=2) %>%
      ph_with_fpars_at(fpars=list(university_name_par,university_par), 
                       left=1.5,top=2.5,height=1,width=1.5 ) %>% # this segement adds university n
      ph_with_fpars_at(fpars=list(region_name_par,region_par), 
                       left=-.1,top=3.75,height=1,width=1.5 ) %>%  # this segement adds region
      ph_with_fpars_at(fpars=list(national_name_par,national_par), 
                       left=-.1,top=2.15,height=1,width=1.5 ) %>%
      ph_with_fpars_at(fpars=add_sample('', s1, s2, s3), 
                       left=.25,top=4.85,height=.75,width=9) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","location_1a.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=2.5,width=4,height=3.5)) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide1b.png"), 100/72, 76/72),
              location = ph_location(top=1.5,left=5.5,width=4,height=3.5))

    #find_me
    
    ####### SLide 3
    ################################################################################################
    
    
    
    UNIVERSITY_SAT <- top_two("Q5",data_school)
    sat_text1 = paste0(UNIVERSITY_SAT, "%")
    sat_text= " of students are satisfied with their dining program, a"
    decrease = 11
    #add decrease or increase function
    sat_text2 = paste0(" decrease of ", decrease, '% ')
    sat_text3 = paste0("from ", last_year, '.')
    #add decrease
    
    sat_par = fpar(ftext(sat_text1,prop =  fp_text(font.size = 20)), 
      ftext(sat_text,prop =  fp_text(font.size = 12)),
      ftext(sat_text2, prop=fp_text(font.size=12, bold=TRUE)),
      ftext(sat_text3, prop=fp_text(font.size=12))
      )
    
    my_pres <- my_pres %>% on_slide(index=3) %>%
      ph_with_fpars_at(fpars=list(sat_par), 
                       left=.4,top=1.3,height=1,width=2.8 )
    
    #66% of students are satisfied with their dining program, a decrease of 7% from Fall 2022.
    
    
    
    
  
     
     
     
    print(my_pres, target = paste0("fallPPTS_2023/",UNIVERSITY_NAME,".pptx"))
    
}

run_report(data, data_last, 'University of Virginia', last_year='Spring 2023')

old_run_report <- function(data,last_data,UNIVERSITY_NAME,ca=F,last_year) {
  
  
  # make student only data set 
  data_c <- data 
  data <- data %>% filter(grepl("student|Other",Q1.1))
  
  data_c_last <- last_data 
  data_last <- last_data %>% filter(grepl("student|Other",Q1.1))
  
  # School data 
  data_school_c <- data_c %>% filter(SCHOOL_USA== UNIVERSITY_NAME)
  data_school <- data %>% filter(SCHOOL_USA== UNIVERSITY_NAME) 
  
  data_school_c_last <- data_c_last %>% filter(SCHOOL_USA== UNIVERSITY_NAME)
  data_school_last <- data_last %>% filter(SCHOOL_USA== UNIVERSITY_NAME) 
  
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
  
  #TODO: Claire are we sure about this including other?
  data_region_c_last <- filter(data_c_last, REGION_NAME==REGION)
  data_region_last <- data_region_c_last %>% filter(grepl("student|Other",Q1.1))
  
  # add region to those without it
  REGION <- ifelse(!grepl("Region",REGION),paste0(REGION," Region"),REGION)
  
  
  #get market segment
  market_seg <- data_c %>% filter(SCHOOL_USA== UNIVERSITY_NAME) %>% 
    select(MARKET_SEGMENT) %>% slice(1)
  MARKET_SEG <- market_seg[[1]]
  data_market_c <- filter(data_c, MARKET_SEGMENT==MARKET_SEG)
  data_market <- data_market_c %>% filter(grepl("student|Other",Q2))
  
  data_market_c_last <- filter(data_c_last, MARKET_SEGMENT==MARKET_SEG)
  data_market_last <- data_market_c_last %>% filter(grepl("student|Other",Q1.1))
  
  
  
  ################################################################################################
  # Read in template 
  if (!ca){
    my_pres <- read_pptx("DSS F2023 USA report template.pptx") 
  } else {
    my_pres <- read_pptx("Template Spring 2023 CA.pptx") 
  }
  
  
  VERYLONG <- nchar(UNIVERSITY_NAME) >30
  
  UNIVERSITY_NAME_SHORT <- ifelse(nchar(UNIVERSITY_NAME) >= 23, paste(strwrap(UNIVERSITY_NAME,23),collapse = "\n  "), UNIVERSITY_NAME)
  
  
  ################################################################################################
  # Slide One - Change Title To university name 
  # TODO: move line down if the title breaks over two lines
  # first title is smaller if name is very long 
  print("at slide 1")
  needs_wrap <- (length(strwrap(UNIVERSITY_NAME,44))>2)
  
  if (needs_wrap){
    bold_face <- update(shortcuts$fp_bold(font.size = 24),color="white")
  } else {
    bold_face <- update(shortcuts$fp_bold(font.size = 30),color="white")
  }
  
  
  
  
  university_par <- fpar(
    ftext(UNIVERSITY_NAME , prop = bold_face),fp_p = fp_par(text.align="right") )
  
  my_pres <-  my_pres %>% on_slide(index=1) %>%
    ph_with_fpars_at(fpars=list(university_par), 
                     left=.75,top=2.25,height=1,width=8
    ) 
  
    
    if (needs_wrap){
      slide_2_height <- 2.4
      slide_6_height <- 2.7
      
    } else {
      slide_2_height <- 3.35
      slide_6_height <- 2.6
    }
    
    ################################################################################################
    # Slide Two 
    # about all respondents
    
    # Add Sample and Nsize with formatting for numbers 
    UNIVERSITY_N <- n_format(nrow(data_school_c)) # sample size for universty 
    SCHOOL_SEGEMENT_N <- n_format(nrow(data_market_c)) # sample size for this kind of school
    REGION_N <- n_format(nrow(data_region_c)) # sample size for this region
    NATIONAL_N <- n_format(nrow(data_c)) # sample size for entire study
    
    university_name_par <- fpar(
      ftext(UNIVERSITY_NAME,prop =  fp_text(font.size = 12)),fp_p = fp_par(text.align="center"))
    
    university_par <- fpar(
      ftext(paste0("n=",UNIVERSITY_N ),prop = fp_text(font.size=12)),fp_p = fp_par(text.align="center") )
    
    school_segment_name_par <- fpar(
      ftext(MARKET_SEG,prop =  fp_text(font.size = 12)),fp_p = fp_par(text.align="center"))
    
    school_segment_par <- fpar(
      ftext(paste0("n=",SCHOOL_SEGEMENT_N ),prop = fp_text(font.size=12)),fp_p = fp_par(text.align="center") )
    
    region_name_par <- fpar(
      ftext(REGION,prop =  fp_text(font.size = 12)),fp_p = fp_par(text.align="center"))
    
    region_par <- fpar(
      ftext(paste0("n=",REGION_N ),prop = fp_text(font.size=12)),fp_p = fp_par(text.align="center") )
    
    national_name_par <- fpar(
      ftext("National",prop =  fp_text(font.size = 12)),fp_p = fp_par(text.align="center"))
    
    national_par <- fpar(
      ftext(paste0("n=",NATIONAL_N ),prop = fp_text(font.size=12)),fp_p = fp_par(text.align="center") )
    
    width <- ifelse(needs_wrap ,2,1.5)
    lshift <- ifelse(needs_wrap ,5.2,5.47)
    
    my_pres <- my_pres %>% on_slide(index=2) %>%
      ph_with_fpars_at(fpars=list(university_name_par,university_par), 
                       left=5.47,top=2.35-.25,height=1,width=1.5 ) %>% # this segement adds university n
      ph_with_fpars_at(fpars=list(school_segment_name_par,school_segment_par), 
                       left=7.63,top=2.35-.25,height=1,width=1.5 ) %>%  # this segement adds segment
      ph_with_fpars_at(fpars=list(region_name_par,region_par), 
                       left=5.47,top=3.76 +.5,height=1,width=1.5 ) %>%  # this segement adds region
      ph_with_fpars_at(fpars=list(national_name_par,national_par), 
                       left=7.63,top=3.76 +.5,height=1,width=1.5 )   # this segement adds national n
    #
   
    ################################################################################################
    # Slide Three
    # ONLY STUDENTS 
    SATISFACTION <- top_two("Q2.1",data_school_c)
    NATIONAL_AVERAGE <- top_two("Q2.1",data_c)
    if(HAS_LAST){
      SATISFACTION_LAST <- top_two("Q2.1",data_school_c_last)
      highlight1_text <- change(SATISFACTION,SATISFACTION_LAST,last_year)
      
      SATISFACTION_VALUE<- top_two("Q2.4",data_school_c)
      SATISFACTION_VALUE_LAST <- top_two("Q2.4",data_school_c_last)
      highlight2_text <- change(SATISFACTION_VALUE,SATISFACTION_VALUE_LAST,last_year)#highlight2_text <- get_quartiles(SATISFACTION_VALUE,NATIONAL_AVERAGE_VALUE,VECTOR_OF_SATISFACTION_VALUE)
      
    } else {
      highlight1_text <- ""
      highlight2_text <- ""
    }
    
    VECTOR_OF_SATISFACTION <-  get_vector(data_c,"Q2.1")
    

    
    highlight1 <- paste0("\u2022   Overall ",SATISFACTION,"% of students were satisfied with their dining program") 
    # highlight1_text <- get_quartiles(SATISFACTION,NATIONAL_AVERAGE,VECTOR_OF_SATISFACTION)
    
    
    SATISFACTION_VALUE <- top_two("Q2.4",data_school_c)
    NATIONAL_AVERAGE_VALUE <- top_two("Q2.4",data_c)
    VECTOR_OF_SATISFACTION_VALUE <- get_vector(data_c,"Q2.4")
    
    highlight2 <- paste0("\u2022   Overall ",SATISFACTION_VALUE,"% of students were satisfied with the value of the dining plan") 

    if (ca ==F){
      data_school_next_year <- data_school_c %>% filter(Q1.8=="Yes")
    } else {
      data_school_next_year <- data_school_c %>% filter(Q1.8ca=="Yes")
    }
     
    if (ca==F){
    PLAN_TO_PURCHASE <- top_two("Q4.2",data_school_next_year)
    highlight3 <- paste0("\u2022   ",PLAN_TO_PURCHASE,"% of students are likely to purchase a dining plan next year.")
          base_data <- data_school_c%>% filter(Q1.8 %in% c("Yes")) %>% filter(Q67!="Graduating")
          base <- nrow(base_data)
          
          res <- base_data %>%
            select(dplyr::starts_with("Q68")) %>% select(!dplyr::contains("Q68_9_TEXT")) %>%
            select(!dplyr::contains("Q68_SDS")) %>%
            mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q68")) %>%
            filter(value!="") %>%
            mutate(value= gsub(" \\(please specify)","",value)) %>% 
            group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n)) %>% head(1)
          
          highlight3_text <- paste0("\u2022   The largest incentive to improve dining plan participation is ",tolower(res$value)," (",roundQual(res$n,0),"%)")
          
          
          } else {
          base_data <- data_school_c%>% filter(Q1.8ca %in% c("Yes")) %>% filter(Q67!="Graduating")
        }
        
        base <- nrow(base_data) 

    
    if(!ca){
      question_list <- c("Q63_1", "Q63_2", "Q63_3", "Q63_4","Q63_5", "Q63_6","Q63_7","Q63_8","Q63_9", "Q63_10",
                         'Q64_1','Q64_2','Q64_3','Q64_4','Q64_5','Q64_6','Q64_7','Q64_8') 
      
    } else {
      question_list <- c("Q63ca_1", "Q63ca_2", "Q63ca_3", "Q63ca_4","Q63ca_5", "Q63ca_6","Q63ca_7","Q63ca_8","Q63ca_9", "Q63ca_10",
                         'Q64_1','Q64_2','Q64_3','Q64_4','Q64_5','Q64_6','Q64_7','Q64_8') 
      
      
    }
      #in this case we are looking for is not blank
    categories <- c('Good', 'Excellent')
    in_cat <- TRUE
    
    levels <- c("Food quality","Food variety","Availability of nutrition information","Availability of ingredient and/or allergen information","Availability of healthy options","Price/value","Availability of special dietary options to fit my dietary needs","Freshness of food","Affordability","Made from organically or sustainably sourced products", 
                'Convenience','Welcoming/friendly staff','Knowledgeable/helpful staff','Speed of service','Cleanliness','Hours of operation','Place to socialize','Comfortable dining experience')
    
    

     atts <- apply(data_school_c[,question_list],MARGIN=2,FUN=function(x){sum(x %in% categories )/sum(!x%in%c("","Not Applicable/Don't Know"),na.rm=T)})
  
     names(atts) <- levels

#    
     max_agree <- which(atts==max(atts))[1]
     per_agree <- roundQual(atts[max_agree]*100,0)
  
     max_agree_l <- which(atts==min(atts))[1]
     per_agree_l <- roundQual(atts[max_agree_l]*100,0)
     
    highlight4 <- paste0("\u2022  Your university's top rated metric is ",tolower(names(per_agree)),", with a ",per_agree,"% favorability.")
 
    highlight4_text <- paste0("\u2022  Your university's bottom rated metric is ",tolower(names(per_agree_l)),", with a ",per_agree_l,"% favorability.")
    
    
    
    h1 <- block_list(highlight1,"c ",highlight1_text)
    
    my_pres <-  my_pres %>% on_slide(index=3) %>%
      ph_with_ul(type = "body", index = 1,
                 str_list = c(highlight1,"",highlight1_text),
                 level_list = c(2,2,2),
                 style = fp_text( font.size = 12),
                 location = ph_location(left=.45,width=2.2,top=2.4, height=2)) %>%
      ph_with_ul(type = "body", index = 1,
                 str_list = c(highlight2,"",highlight2_text),
                 level_list = c(2,2,2),
                 style = fp_text( font.size = 12),
                 location = ph_location(left=2.85,width=2.2,top=2.45, height=2))  %>%
      ph_with_ul(type = "body", index = 1,
                 str_list = c(highlight4,"",highlight4_text),
                 level_list = c(2,2,2),
                 style = fp_text( font.size = 12),
                 location = ph_location(left=7.55,width=2.4,
                                        top = 2.15,
                                       # top=!!enquo(height_att), 
                                        height=2.4))
    
   if (ca==F){
   if (nchar(res$value)<35){
my_pres <- my_pres %>%     ph_with_ul(type = "body", index = 1,
                 str_list = c(highlight3,"",highlight3_text),
                 level_list = c(2,2,2),
                 style = fp_text( font.size = 12),
                 location = ph_location(left=5.15,width=2.2,top=2.55, height=2)) 
} else {
my_pres <- my_pres %>%     ph_with_ul(type = "body", index = 1,
                 str_list = c(highlight3,"",highlight3_text),
                 level_list = c(2,2,2),
                 style = fp_text( font.size = 12),
                 location = ph_location(left=5.15,width=2.2,top=2.75, height=2)) 
} 
     }# CA has no answer to 68 so nopart of highlihgt 3

  

    
    # add sample size text 
    add_sample <- function(p1,p2,p3,p4,p5=NULL) {
      sample_size1 <- fpar(
        ftext(p1,
              fp_text( font.size = 7)))
      sample_size2 <- fpar(
        ftext(p2,
              fp_text( font.size = 7)))
      sample_size3 <- fpar(
        ftext(p3,
              fp_text( font.size = 7)))
      sample_size4 <- fpar(
        ftext(p4,
              fp_text( font.size = 7)))
      if(!is.null(p5)){
        sample_size5 <- fpar(
          ftext(p5,
                fp_text( font.size = 7)))
        return(list(sample_size1,sample_size2,sample_size3,sample_size4,sample_size5))
      } else{
        return(list(sample_size1,sample_size2,sample_size3,sample_size4))
      }
      
    }
    if(HAS_LAST) {
      response_vol <- paste0("Response volumes differ between fall and spring. There were ", nrow(data_school_c_last), " total responses to last survey (", last_year, ") and ", nrow(data_school_c), " total responses to the current survey.")
      
    } else{
      response_vol <- paste0(" There were ", nrow(data_school_c), " total responses to the survey.")
      
    }
       
    my_pres <- my_pres %>% on_slide(index=3) %>% 
      ph_with_fpars_at(fpars=add_sample("","", "", response_vol), 
                       left=.25,top=4.9,height=.75,width=9 )
    
      # my_pres <- my_pres %>% on_slide(index=5) %>% 
      #   ph_with_fpars_at(fpars=list(paste('Sample in Spring 2023 is ', nrow(data_school_c))), 
      #                  left=.25,top=4.8,height=.75,width=9 )
  
    ################################################################################################
    # Slide five 
    # this is only students 
    # new covid slides 
    
    
    COMMUTE <- names(which(table(data_school$Q1.5)==max(table(data_school$Q1.5))))[1]  # which is most common
    ## get rid of extra text
    COMMUTE <- gsub("\\(in-person and online\\) ","",COMMUTE)
    
    type <- data_school %>% group_by(Q1.1) %>% summarize(n=n()) %>% arrange(desc(n))
    type$Q1.1[1]
    commute_text <- paste0("Most respondents are living ",tolower(COMMUTE)," and most are ",type$Q1.1[1],"s")

    title_p23 <- fpar(
      ftext(commute_text,
            fp_text( font.size = 21)))
    
    # add title 
    my_pres <- my_pres %>% on_slide(index=5) %>% 
      ph_with_fpars_at(fpars=list(title_p23), 
                       left=.25,top=.25,height=.75,width=9 )
    # Add graphs_fall_2022
    
    my_pres <- my_pres %>% on_slide(index=5) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide21b.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=.5,width=4,height=3.5), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide21c.png"), 100/72, 76/72),
               location = ph_location(top=1.6,left=5.9,width=4,height=4), 
               use_loc_size = TRUE )
    


   
   
    # add title 
    my_pres <- my_pres %>% on_slide(index=5) %>% 
      ph_with_fpars_at(fpars=add_sample("",paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),"), Among Students/Other (n=",format(nrow(data_school),big.mark = ","),")"),
                                        "Q2. Which of the following best describes you?","Q4. Where do you live?"), 
                       left=.25,top=4.8,height=.75,width=9 )
    ################################################################################################
    # Slide Six - New 
    # this is all 
    if(HAS_LAST){

      # top_two("Q2.1",data_school_c)
      UNIVERSITY_SAT <-  top_two("Q2.1",data_school_c)
      UNIVERSITY_LAST <-   top_two("Q2.1",data_school_c_last)
      
      sat_text <- ifelse(UNIVERSITY_SAT <UNIVERSITY_LAST,paste0("decreased by ",abs(UNIVERSITY_SAT-UNIVERSITY_LAST),"%"),paste0("increased by ",abs(UNIVERSITY_SAT-UNIVERSITY_LAST),"%"))
      sat_text <- ifelse(UNIVERSITY_SAT==UNIVERSITY_LAST,"stayed the same",sat_text)
      title_p6 <- fpar(
        ftext("Overall Experience at ", fp_text( font.size = 22)),
        ftext( UNIVERSITY_NAME, fp_text(bold = TRUE, font.size = 22)),
        ftext(paste0(" has ",sat_text," (",UNIVERSITY_SAT,"% Top 2 Box) since ",last_year,"."), prop = fp_text( font.size = 22) )
      )
      
 
      my_pres <- my_pres %>% on_slide(index=6) %>%
        ph_with_fpars_at(fpars=list(title_p6), 
                         left=.15,top=.25,height=.75,width=9 ) %>%
        ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slideSat.png"), 100/72, 76/72),
                 location = ph_location(top=1.3,left=2,width=5.5,height=3.5), 
                 use_loc_size = TRUE )
      
      # add foot note 
      my_pres <- my_pres %>% on_slide(index=6) %>% 
        ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n last year=",format(nrow(data_school_c_last),big.mark = ","),", n =",format(nrow(data_school_c),big.mark = ","),")"),
                                          "Q5. Overall, how would you rate your campus dining program?","",""), 
                         left=.25,top=4.9,height=.75,width=9 )
    } else {
      
      UNIVERSITY_SAT <- top_two("Q2.1",data_school_c)
      title_p6 <- fpar(
        ftext("Overall Experience at ", fp_text( font.size = 22)),
        ftext( UNIVERSITY_NAME, fp_text(bold = TRUE, font.size = 22)),
        ftext(paste0(" was ",UNIVERSITY_SAT,"% (Top 2 Box)."), prop = fp_text( font.size = 22) )
      )
      
      my_pres <- my_pres %>% on_slide(index=6) %>%
        ph_with_fpars_at(fpars=list(title_p6), 
                         left=.15,top=.25,height=.75,width=9 )  %>%
        ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slideSat.png"), 100/72, 76/72),
                 location = ph_location(top=1.5,left=2,width=5.5,height=3.5), 
                 use_loc_size = TRUE )
      
      # add foot note 
      my_pres <- my_pres %>% on_slide(index=6) %>% 
        ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),")"),
                                          "Q2.1. Overall, how would you rate your campus dining program?","",""), 
                         left=.25,top=4.9,height=.75,width=9 )
    }
    
    
    
    

    ################################################################################################
    # Slide Seven
    
    # remains food quality.
    
    SAT_PERCENTAGE <- top_two("Q2.4",data_school)
    
    if(HAS_LAST){
      SAT_PERCENTAGE_LAST <- top_two("Q2.4",data_school_last)
      title7 <- ifelse(SAT_PERCENTAGE_LAST<SAT_PERCENTAGE,paste0("Students' value satisfaction has increased since ",last_year,", and the main value driver is"),paste0("Students' value satisfaction has decreased since ",last_year,", and the main value driver is"))
      title7 <- ifelse(SAT_PERCENTAGE_LAST==SAT_PERCENTAGE,paste0("Students' value satisfaction has remained the same since ",last_year,", and the main value driver is"),title7)
 
    } else {
      title7 <- "The main driver of students' value satisfaction is"
    }
    
    driver <- data_school%>% group_by(Q62) %>% summarize(n=n()) %>% arrange(desc(n)) %>%head(1)
    
    title7 <- paste0(title7,gsub("Value to me is about","",driver$Q62),".")

    title_p7 <- fpar(
      ftext(title7, fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=7) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide8a.png"), 100/72, 76/72),
                   location = ph_location(top=1.3,left=.4,width=4.5,height=3.9), 
                   use_loc_size = TRUE ) 
    my_pres <- my_pres %>% on_slide(index=7) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide8b.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=4.8,width=4.5,height=4), 
               use_loc_size = TRUE )  
    
    my_pres <- my_pres %>% on_slide(index=7) %>% 
      ph_with_fpars_at(fpars=list(title_p7), 
                       left=.25,top=.25,height=.75,width=9 )
    
    if (HAS_LAST){
      
      my_pres <- my_pres %>% on_slide(index=7) %>% 
        ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n last year=",format(nrow(data_school_c_last),big.mark = ","),", n =",format(nrow(data_school_c),big.mark = ","),")"),
                                          "Q7. How would you rate the value you receive when dining on campus?","Q9. Which statement below best describes how you value a meal?",""), 
                         left=.25,top=4.9,height=.75,width=9 )
    } else {
      my_pres <- my_pres %>% on_slide(index=7) %>% 
        ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),")"),
                                          "Q7. How would you rate the value you receive when dining on campus?","Q9. Which statement below best describes how you value a meal?",""), 
                         left=.25,top=4.9,height=.75,width=9 )
    }

    
    ################################################################################################
    # Slide eight - 12 various satisfaction metrics
    # Now the market segment comparison of all groups 
    
    UNIVERSITY_SAT <- top_two("Q2.1",data_school_c)
    title_p8 <- fpar(
      ftext(paste0(UNIVERSITY_SAT,"% (Top 2 Box) rate the campus dining program as good or excellent overall."), fp_text( font.size = 22)))


    
    if(!ca){
    my_pres <- my_pres %>% on_slide(index=8) %>% 
      ph_with_fpars_at(fpars=list(title_p8), 
                       left=.15,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide10.png"), 100/72, 76/72),
               location = ph_location(top=1.2,left=1.7,width=5.5,height=3.9 ), 
               use_loc_size = TRUE ) 
    my_pres <- my_pres %>% on_slide(index=8) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),")"),
                                        "Q2.1. Overall, how would you rate your campus dining program?","",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    } else { #add food sat table for ca 
      my_pres <- my_pres %>% on_slide(index=8) %>% 
        ph_with_fpars_at(fpars=list(title_p8), 
                         left=.15,top=.25,height=.75,width=9 ) %>% 
        ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide10.png"), 100/72, 76/72),
                 location = ph_location(top=1.2,left=0,width=4.5,height=3.5 ), 
                 use_loc_size = TRUE )  %>% 
        ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","tablefav.png"), 100/72, 76/72),
                 location = ph_location(top=1.2,left=6.4,width=4,height=3.8 ), 
                 use_loc_size = TRUE ) 
      my_pres <- my_pres %>% on_slide(index=8) %>% 
        ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),")"),
                                          "Q2.1. Overall, how would you rate your campus dining program?","Q63. Thinking about your campus dining program, which of the following are most important to you? Please select up to 5.",""), 
                         left=.25,top=4.9,height=.75,width=9 )
  
}
    
    if (!ca){
      UNIVERSITY_SAT <- top_two("Q63_1",data_school_c)
    } else {
      UNIVERSITY_SAT <- top_two("Q63ca_1",data_school_c)
    }
    
    title_p9 <- fpar(
      ftext(paste0(UNIVERSITY_SAT,"% (Top 2 Box) rate the campus dining program's quality as good or excellent."), fp_text( font.size = 22)))
  
    
    my_pres <- my_pres %>% on_slide(index=9) %>% 
      ph_with_fpars_at(fpars=list(title_p9), 
                       left=.15,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide10_quality.png"), 100/72, 76/72),
               location = ph_location(top=1.2,left=1.7,width=5.5,height=3.9 ), 
               use_loc_size = TRUE ) 
    
    my_pres <- my_pres %>% on_slide(index=9) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),")"),
                                        "Q63_1. Thinking about your campus dining program, how would you rate each of the following? - Food quality","",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    
    
    UNIVERSITY_SAT <- top_two("Q64_4",data_school_c)
    title_p10 <- fpar(
      ftext(paste0(UNIVERSITY_SAT,"% (Top 2 Box) rate the campus dining program's speed of service as good or excellent."), fp_text( font.size = 22)))
    
print("at slide 10")
    my_pres <- my_pres %>% on_slide(index=10) %>% 
      ph_with_fpars_at(fpars=list(title_p10), 
                       left=.15,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide10_speed_of_service.png"), 100/72, 76/72),
               location = ph_location(top=1.2,left=1.7,width=5.5,height=3.9 ), 
               use_loc_size = TRUE ) 
    
    my_pres <- my_pres %>% on_slide(index=10) %>% 
      ph_with_fpars_at(fpars=add_sample("", paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),")"),
                                        "Q64_4. Still thinking about your campus dining program, how would you rate each of the following? - Speed of service",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    
    UNIVERSITY_SAT <- top_two("Q64_5",data_school_c)
    title_p11 <- fpar(
      ftext(paste0(UNIVERSITY_SAT,"% (Top 2 Box) rate the campus dining program's cleanliness as good or excellent."), fp_text( font.size = 22)))
    
    
    my_pres <- my_pres %>% on_slide(index=11) %>% 
      ph_with_fpars_at(fpars=list(title_p11), 
                       left=.15,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide10_cleanliness.png"), 100/72, 76/72),
               location = ph_location(top=1.2,left=1.7,width=5.5,height=3.9 ), 
               use_loc_size = TRUE ) 
    my_pres <- my_pres %>% on_slide(index=11) %>% 
      ph_with_fpars_at(fpars=add_sample("", paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),")"),
                                        "Q64_5. Still thinking about your campus dining program, how would you rate each of the following? - cleanliness",""), 
                       left=.25,top=4.9,height=.75,width=9 )

    
    
    UNIVERSITY_SAT <- top_two("Q64_1",data_school_c)
    title_p12 <- fpar(
      ftext(paste0(UNIVERSITY_SAT,"% (Top 2 Box) rate the campus dining program's convenience as good or excellent."), fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=12) %>% 
      ph_with_fpars_at(fpars=list(title_p12), 
                       left=.15,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide10_convenience.png"), 100/72, 76/72),
               location = ph_location(top=1.2,left=1.7,width=5.5,height=3.9 ), 
               use_loc_size = TRUE ) 
    
    my_pres <- my_pres %>% on_slide(index=12) %>% 
      ph_with_fpars_at(fpars=add_sample("", paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),")"),
                                        "Q64_1. Still thinking about your campus dining program, how would you rate each of the following? - Convenience",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    
    
    
    ###############################################################################################
    # Slide twelve
    # This is all repsondents 
    question_list <- c('Q65_1','Q65_2','Q65_3','Q65_4')
    levels <- c('Dining in at a restaurant','Ordering take-out','Ordering delivery','Cooking at home') 
    names(levels) <- question_list
 
    
    res <- data_school_c %>% select(dplyr::starts_with("Q65")) %>% 
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q65")) %>%
      filter(value!="") %>%
      group_by(value,name) %>% summarize(n=n()) 
    
    min_val_df <- res %>% filter(value%in%c("0 Times/Week","1-2 Times/Week")) %>% 
      group_by(name) %>% 
      summarize(n=sum(n)) %>%
      arrange(desc(n)) %>%
      slice(1) 
    min_val <- levels[min_val_df$name]
    max_val_df <- res %>% filter(value%in%c("6-9 Times/Week","10+ Times/Week"))     %>% 
      group_by(name) %>% 
      summarize(n=sum(n)) %>%
      arrange(desc(n)) 
    max_val <- levels[max_val_df$name[1]]
    
    #most liked calculates 
    
    title_p11 <- fpar(
      ftext(paste0(max_val," is the most popular dining option and ",tolower(min_val)," is the least popular."), fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=13) %>% 
      ph_with_fpars_at(fpars=list(title_p11), 
                       left=.05,top=.25,height=.75,width=9 )
    
    my_pres <- my_pres %>% on_slide(index=13) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide12b.png"), 100/72, 76/72),
               location = ph_location(top=1.4,left=.25,width=9,height=3.95 ), 
               use_loc_size = TRUE ) 
    
    my_pres <- my_pres %>% on_slide(index=13) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),")"),
                                        "Q12.  How many meals would you like to have from each of the following options per week?","",""), 
                       left=.25,top=5.1,height=.75,width=9 )
    
    
    ################################################################################################
    # Slide thirteen
     res <- data_school_c %>% select(dplyr::starts_with("Q3.2")) %>% 
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q3.2")) %>%
    filter(value!="") %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_school_c)) %>% arrange(desc(n)) %>%
    mutate(value=gsub("}","",gsub("${e://Field/","",value,fixed=T)))
  

  
  outlets$cleanname <-trimws(outlets$SCHOOL_NAME)
  os <- outlets %>% filter(cleanname==!!enquo(UNIVERSITY_NAME)) %>%
    pivot_longer(cols=dplyr::starts_with("OUTLET_NAME")) %>% 
    select(name,value)%>%
    mutate(value=trimws(value))%>%
    rename(outlet=value)
  
  res2 <- merge(res,os,by.x="value",by.y="name",all.x=T) %>% 
           group_by(outlet) %>%  # This is needed for the rare case of duplicate outlet names
           summarize(n=sum(n)) %>% 
           arrange(desc(n))
  
    
    title_p11 <- fpar(
      ftext(paste0("The most frequented dining location on campus is ",as.character(res2$outlet[1]),", and the least frequented is ",as.character(res2$outlet[nrow(res2)]),"."), fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=14) %>% 
      ph_with_fpars_at(fpars=list(title_p11), 
                       left=.05,top=.25,height=.75,width=9 )
    
    my_pres <- my_pres %>% on_slide(index=14) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slideLocations.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=1.25,width=3,height=3.95 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slideLocationSat.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=6.25,width=3,height=3.95 ), 
               use_loc_size = TRUE ) 
    
    my_pres <- my_pres %>% on_slide(index=14) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),")"),
                                        "Q13. Which of the following dining locations on-campus have you visited frequently this semester/quarter?","Q14. How satisfied were you with the specific food you ordered at ... ?",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    

    
    ################################################################################################
    # Slide Fourteen
    # oncampus next year 
    # entire data not jsut students 
    #data_school_c$Q1.8.1
     # participate even though not riquered
    #table(data_school_c$Q4.1)
    # most 

    # val <- ifelse(sum(data_school_c$Q4.1 =="I am required to participate in the meal plan") >sum(data_school_c$Q4.1 =="I participate in the meal plan although it is not required"),
    #                     "Most students who have a meal plan are required to purchase one","Most students who have a meal plan are not required to purchase one")
    # val <- ifelse(sum(data_school_c$Q4.1 =="I am required to participate in the meal plan")==sum(data_school_c$Q4.1 =="I participate in the meal plan although it is not required"),"Half of students who have a meal plan are required to purchase one",val)
    
    pct <- round(sum(data_school_c$Q4.1 =="I am required to participate in the meal plan")/nrow(data_school_c) *100, 0)
    val<- paste0(pct, "% of students are required to participate in the meal plan")
    
    if (!ca){
    
    if(HAS_LAST){
        # Set up title 
        changed <- ifelse(top_two("Q4.2",data_school_c) <top_two("Q4.2",data_school_c_last),'decreased',"increased")
        changed <- ifelse(top_two("Q4.2",data_school_c)==top_two("Q4.2",data_school_c_last),'stayed the same',changed)
        title <- paste0(val, ", and future meal plan purchase likelihood has ",changed, " compared to ",last_year,".")
        
    } else{
      # Set up title 
        title <- paste0(val,".")
      
    }
      title <- paste0(val,".")
      title_p11 <- fpar(
        ftext(title, fp_text( font.size = 18)))
    } else { # CA does not have last in this round
      title <- paste0(val,".")
      title_p11 <- fpar(
        ftext(title, fp_text( font.size = 18)))
    } 
    
    
    
    my_pres <- my_pres %>% on_slide(index=15) %>% 
      ph_with_fpars_at(fpars=list(title_p11), 
                       left=.05,top=.05,height=.95,width=9 )
    
    my_pres <- my_pres %>% on_slide(index=15) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide13b.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=.8,width=3.5,height=2.6 ), 
               use_loc_size = TRUE ) 
    if(!ca){
      my_pres <- my_pres %>% 
        ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide13a.png"), 100/72, 76/72),
                 location = ph_location(top=1.5,left=5.5,width=3.8,height=3.6 ), 
                 use_loc_size = TRUE ) 
    }
   
    
    # find top resons
    res <- data_school_c %>% 
      select(dplyr::starts_with("Q66")) %>% select(!dplyr::contains("Q66_9_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q66")) %>%
      filter(value!="") %>%
      group_by(value) %>% summarize(n=n()) %>%arrange(desc(n)) %>% 
      mutate(value=tolower(value))%>% 
      head(3)
  
      motiv_text <- paste0("Among students who participate in a meal plan although it is not required, they most commonly state that they made their decision to purchase because: ",
                           res$value[1],", ",res$value[2]," and ",res$value[3])
    
      motiv_textpa <- fpar(
        ftext(as.character(motiv_text), fp_text( font.size = 9)))
      
      
    if (ca==F){
      base_data <- data_school_c%>% filter(Q1.8 %in% c("Yes"))
    } else {
      base_data <- data_school_c%>% filter(Q1.8ca %in% c("Yes"))
    }
    
    base <- nrow(base_data) 
      
    my_pres <- my_pres %>% on_slide(index=15) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents/ Who Participate Even Though It Is Not Required (n=",
                                                format(nrow(data_school_c),big.mark = ","),",",format(sum(data_school_c$Q4.1=="I participate in the meal plan although it is not required",na.rm=T),big.mark = ","),"), Who Plan to be On Campus Next Year (n=",format(base,big.mark = ","),")"),
                                        "Q16. Which of the following best describes your current participation in your school's meal plan?",
                                        "Q17. What research or information did you receive that led you to purchase a meal plan?","Q18. How likely are you to purchase a meal plan next year?"), 
                       left=.25,top=4.9,height=.75,width=9 ) %>% 
      ph_with_fpars_at( fpars=list(motiv_textpa),
                             fp_pars = list(fp_par(text.align = "center")),
                            left=.4,top=4.2,height=.85,width=4 )
  
    

    ################################################################################################
    # Slide fifteen
    # entire data not jsut students 
    if(!ca){
      base <- sum(data_school_c$Q4.2 %in% c("Probably will not buy", "Might or might not buy",  
                                        "Definitely will not buy"),na.rm=T)

    res <- data_school_c %>% filter(Q4.2 %in%c("Probably will not buy", "Might or might not buy",  
                                             "Definitely will not buy"))%>% 
      rename(value=Q67) %>% select(value) %>%
      filter(value!="") %>%
      mutate(value= gsub(" \\(please specify)","",value)) %>% 
     # mutate(value = gsub("meal","dining",value)) %>% 
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
    
    title <- paste0("Students state '",res$value[1],"' as the biggest barrier to purchasing a dining plan for next year.")
    
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 20)))
    
    my_pres <- my_pres %>% on_slide(index=16) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       left=.05,top=.25,height=.75,width=9 ) 

    my_pres <- my_pres %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide14.png"), 100/72, 76/72),
               location = ph_location(top=1.4,left=1.5,width=5.5,height=4 ), 
               use_loc_size = TRUE ) 
    my_pres <- my_pres %>% on_slide(index=16) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Respondents Who May Not Buy a Dining Plan Next Year (n=",format(sum(data_school_c$Q4.2%in% c("Might or might not buy","Probably will not buy","Definitely will not buy"),na.rm=T),big.mark = ","),")"),
                                        "Q20. Which of the following is the biggest barrier to you purchasing a meal plan next year?",
                                        "",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    }


  
   
      
  

    ################################################################################################
    # Slide sixteen
    # entire data not jsut students
    if (ca==F){
    if (ca==F){
      base_data <- data_school_c%>% filter(Q1.8 %in% c("Yes")) %>% filter(Q67!="Graduating")
    } else {
      base_data <- data_school_c%>% filter(Q1.8ca %in% c("Yes")) %>% filter(Q67!="Graduating")
    }
    
    base <- nrow(base_data) 

  	res <- base_data %>%
    select(dplyr::starts_with("Q68")) %>% select(!dplyr::contains("Q68_9_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q68")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" \\(please specify)","",value)) %>% 
    group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))

    
    title <- paste0("Students state ",tolower(res$value[1])," as the biggest motivator to purchasing a meal plan for next year.")
    
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=17) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","plotMotiv.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=2.5,width=5,height=4 ), 
               use_loc_size = TRUE ) 
    

    my_pres <- my_pres %>% on_slide(index=17) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Respondents Who Plan to be On Campus Next Year (n=",format( base,big.mark = ","),")"),
                                        "Q21. Which of these, if any, would make you more likely to purchase a meal plan in the future?",
                                        "",""), 
                       left=.25,top=5.1,height=.75,width=9 )
  
    }
    ################################################################################################
    ### new slide 17 - where they would like to purchase 
    if (ca==F){
    # those who want to use off campus
    base <- nrow(data_school_c %>% filter(Q68_14=="Ability to use my meal plan off campus"))
    
    res <- data_school_c %>% filter(Q68_14=="Ability to use my meal plan off campus") %>% 
      select(dplyr::starts_with("Q92")) %>% select(!dplyr::contains("Q92_6_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q92")) %>%
      filter(value!="") %>%
      filter(!grepl("Other",value)) %>%
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
    
    
    title <- paste0(res$value[1],"s are where students would most prefer to use their meal plans off-campus.")
    
    title_p17 <- fpar(
      ftext(title, fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=18) %>% 
      ph_with_fpars_at(fpars=list(title_p17), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","plotPrefer.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=2.5,width=5,height=4 ), 
               use_loc_size = TRUE ) 
    
    
    my_pres <- my_pres %>% on_slide(index=18) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Respondents who want the ability to use thier meal plan off-campus (n=",format( base,big.mark = ","),")"),
                                        "Q22. If given the option, what types of off-campus locations would you most prefer to be able to use your meal plan?",
                                        "",""), 
                       left=.25,top=5.1,height=.75,width=9 )
    
    }
    ################################################################################################
    ### new slide 18 - digital chanels
    
    # school 
    base <- nrow(data_school_c)
    
    res <- data_school_c %>% 
      select(dplyr::starts_with("Q93")) %>% select(!dplyr::contains("Q93_6_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q93")) %>%
      filter(value!="") %>%
      filter(!grepl("Other",value)) %>%
      mutate(value= gsub(" (please specify) :","",value, fixed=T)) %>% 
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
    
    
    title <- paste0(res$value[1]," is the most preferred digital channel to receive meal plan communications.")
    
    title_p18 <- fpar(
      ftext(title, fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=19) %>% 
      ph_with_fpars_at(fpars=list(title_p18), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","plotCommunicate.png"), 100/72, 76/72),
               location = ph_location(top=.9,left=.25,width=9.5,height=3.9 ), 
               use_loc_size = TRUE ) 
    
    
    my_pres <- my_pres %>% on_slide(index=19) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format( base,big.mark = ","),")"),
                                        "Q23. Which of the following digital channels would you most prefer to receive meal plan communications from Dining Services?",
                                        "",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    ################################################################################################
    # Slide seventeen - now 20
    # restrictions
    # head line number having any restiiction
    eligible = c('Milk', 'Eggs', 'Peanuts', 'Tree nuts', 'Fish', 'Shellfish', 'Soy', 
                 'Wheat/Gluten', 'Sesame',  'Other')
    if (ca){
      eligible = c(eligible, 'Mustard')
    }
    
  
    
    res_sch <- data_school_c %>% filter(Q69_1=="Food allergies or intolerances")%>% 
      select(dplyr::starts_with("Q70")) %>% select(!dplyr::contains("Q70_10_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q70")) %>%
      filter(value!="") %>%
      group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n)) %>% 
      mutate(Location=UNIVERSITY_NAME_SHORT,value=gsub(" \\(please specify\\)","",value)) %>% 
      filter(!value%in%c('None of the above',"Other")) %>% 
      filter(value %in% eligible) 

    allergy <- res_sch$value[1]
    
    
    eligible_f = c('Vegetarian', 'Vegan', 'Halal', 'Kosher', 'Paleo/Primal', 'Ketogenic', 'Mediterranean')
    if (!ca){
      eligible_f = c(eligible_f, c('Atkins', 'GMO-Free', 'Whole 30', 'Plant-based', 'Other',"Low-FODMAP","Pescatarian"))
    } else {
      eligible_f = c(eligible_f, c( 'Flexitarian', 'No Peanut/Tree Nut', 'No Dairy', 'No Gluten', 'Other'))
    }
    
    
    res_schres <- data_school_c %>% filter(Q69_2=="Special dietary lifestyle for personal preference or religious reasons")%>% 
      select(dplyr::starts_with("Q71")) %>% select(!dplyr::contains("Q71_14_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q71")) %>%
      filter(value!="") %>%
      group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n)) %>% 
      mutate(Location=UNIVERSITY_NAME_SHORT,value=gsub(" \\(please specify\\)","",value))%>% 
      filter(!value%in%c('None of the above',"Other")) %>% 
      filter(value %in% eligible_f) 
    
    diet <- res_schres$value[1]
    
    title <- paste0(roundQual(sum(data_school_c$Q69_1!=""|data_school_c$Q69_2!="")/nrow(data_school_c)*100,0),"% of respondents have a food allergy or dietary requirement, with ",tolower(allergy)," being the most common allergy and ",tolower(diet)," being the most common diet.")
    p_allergy <- paste0(roundQual(sum(data_school_c$Q69_1!="")/nrow(data_school_c)*100,0),"%")
    p_diet <- paste0(roundQual(sum(data_school_c$Q69_2!="")/nrow(data_school_c)*100,0),"%")
    
    sat <- roundQual(100*sum(data_school_c[data_school_c$Q69_1!=""|data_school_c$Q69_2!="","Q72"] %in%c("Extremely satisfied","Somewhat satisfied"))/nrow(data_school_c[data_school_c$Q69_1!=""|data_school_c$Q69_2!="",]))
    
    p_desc <- paste0("Among students with food allergies or special diets, ",sat,
                    "% are satisfied with the ease of communicating their special dietary needs to staff.")
    
properties1 <- fp_text( font.size = 10)
properties2 <- fp_text(bold = TRUE, font.size = 10)
ftext1 <- ftext("Among students with food allergies or special diets, ", properties1)
ftext2 <- ftext(paste0(sat,"%"), properties2)
ftext3 <- ftext(" are satisfied with the ease of communicating their special dietary needs to staff.", properties1)
par_desc <- fpar(ftext1, ftext2,ftext3)

    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 20)))
    
    par_allergy <- fpar(
      ftext(p_allergy, fp_text(bold = TRUE, font.size = 18)))
    par_diet <- fpar(
      ftext(p_diet, fp_text(bold = TRUE, font.size = 18)))
    
    print("at slide 20")
    
    my_pres <- my_pres %>% on_slide(index=20) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       left=.05,top=.05,height=.95,width=9 )   %>% 
       ph_with_fpars_at(fpars=list(par_allergy), 
                        left=1.2,top=1.7,height=.25,width=1 ) %>% 
       ph_with_fpars_at(fpars=list(par_diet), 
                        left=1.2,top=3.2,height=.25,width=1 ) %>% 
       ph_with_fpars_at(fpars=list(par_desc), 
                    fp_pars = list(fp_par(text.align = "center")),
                        left=5.8,top=4.5,height=.75,width=3.5 ) %>%
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide17a.png"), 100/72, 76/72),
               location = ph_location(top=1.25,left=3.7,width=5.5,height=1.6 ), 
               use_loc_size = TRUE )  %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide17b.png"), 100/72, 76/72),
               location = ph_location(top=3 ,left=3.7,width=5.5,height=1.6), 
               use_loc_size = TRUE ) 

    my_pres <- my_pres %>% on_slide(index=20) %>%
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",
                                                format(nrow(data_school_c),big.mark = ","),")"),
                                        "Q24. Do you adhere to any dietary plans or restrictions?",
                                        "Q25. Which of the following food allergies or intolerances do you have?","Q26. Which of the following special dietary requirements, for personal preference or religious reasons do you follow?",
                                        "Q27. How satisfied are you with the ease of communicating your dietary needs or questions to dining staff members?"),
                       left=.25,top=4.7,height=.75,width=9 )

    #######################################################
    # Slide 20 favorite cuisine 
    base <- nrow(data_school_c)
    
    res <- data_school_c %>% 
      select(dplyr::starts_with("Q94")) %>% select(!dplyr::contains("Q94_18_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q94")) %>%
      filter(value!="") %>%
      filter(!grepl("Other",value)) %>%
      mutate(value= gsub(" (please specify) :","",value, fixed=T)) %>% 
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
    
    
    title <- paste0(res$value[1],", ",res$value[2],", and ",res$value[3]," are the top favorite cuisines students like to eat.")
    
    title_p18 <- fpar(
      ftext(title, fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=21) %>% 
      ph_with_fpars_at(fpars=list(title_p18), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","plotCuis.png"), 100/72, 76/72),
               location = ph_location(top=1.4,left=2,width=5,height=4 ), 
               use_loc_size = TRUE ) 
    
    
    my_pres <- my_pres %>% on_slide(index=21) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format( base,big.mark = ","),")"),
                                        "Q28. What are your favorite types of cuisine to eat?",
                                        "",""), 
                       left=.25,top=4.9,height=.75,width=9 )
   
    
    ################################################################################################
    # New slide 21 
    # Title 
    base <- nrow(data_school_c)
    
    res <- data_school_c %>% 
      select(dplyr::starts_with("Q95")) %>% select(!dplyr::contains("Q95_10_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q95")) %>%
      filter(value!="") %>%
    #  filter(!grepl("Other",value)) %>%
    #  mutate(value= gsub(" (please specify) :","",value, fixed=T)) %>% 
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
    
    VAL <- 100-res[res$value=="None of the above",'n']
    top <- res[res$value!="None of the above",]$value[1]
    second <- res[res$value!="None of the above",]$value[2]
    
    val_title <- paste0("About ", round(VAL,0),"% of students eat plant-based items; ",tolower(top)," and ",tolower(second)," are most popular.")
    
    title_p21 <- fpar(
      ftext(val_title, fp_text( font.size = 18)))
    
    my_pres <- my_pres %>% on_slide(index=22) %>% 
      ph_with_fpars_at(fpars=list(title_p21), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","plot_veg.png"), 100/72, 76/72),
               location = ph_location(top=1.7,left=.75,width=2.5,height=2.5 ), 
               use_loc_size = TRUE )  %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","tableVeg.png"), 100/72, 76/72),
               location = ph_location(top=1.8,left=5.3,width=3,height=3.2 ), 
               use_loc_size = TRUE ) 
    
    
    my_pres <- my_pres %>% on_slide(index=22) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format( base,big.mark = ","),")"),
                                        "Q29. Which of the following plant-based foods do you eat?",
                                        "",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    
    ################new slide 21 vegetypes
    base <- nrow(data_school_c)
    if (!ca){
      data_school_c$VegType <- paste(data_school_c$Q96_1,data_school_c$Q96_2,data_school_c$Q96_3)
    } else {
      data_school_c$VegType <- paste(data_school_c$Q115_1,data_school_c$Q115_2,data_school_c$Q115_3,data_school_c$Q115_4,data_school_c$Q115_5)
      
    }
 
    data_school_c$VegTypeTr <-  grepl("Sometimes|Often|Always",data_school_c$VegType)
    
    val_t <- round(sum(data_school_c$VegTypeTr==TRUE)*100/nrow(data_school_c),0)
    
    if (!ca){
    val_title <- paste0(val_t,"% of students are eating plant-based proteins at least sometimes.")
    } else {
      val_title <- paste0(val_t,"% of students are eating plant-based products at least sometimes.")
      
    }
    title_p21 <- fpar(
      ftext(val_title, fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=23) %>% 
      ph_with_fpars_at(fpars=list(title_p21), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slideVegType.png"), 100/72, 76/72),
               location = ph_location(top=1.2,left=1.25,width=7,height=4 ), 
               use_loc_size = TRUE )  
    
    if (!ca){ # us has protien
    my_pres <- my_pres %>% on_slide(index=23) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format( base,big.mark = ","),")"),
                                        "Q30. How often, if at all, are you eating the following plant-based proteins?",
                                        "",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    }else{
      my_pres <- my_pres %>% on_slide(index=23) %>% 
        ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format( base,big.mark = ","),")"),
                                          "Q30. How often, if at all, are you eating the following plant-based products?",
                                          "",""), 
                         left=.25,top=4.9,height=.75,width=9 )
    }
    
    
    
    ################################################################################################
    # Slide 23- sustainability 

    res <- data_school %>% select(dplyr::starts_with("Q73")) %>% select(!dplyr::contains("Q73_16_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q73")) %>%
      filter(value!="") %>%
      group_by(value) %>% summarize(n=100*n()/nrow(data_school))  %>% arrange(desc(n))
    
    res$value <- gsub(" \\(please specify\\)","",res$value)
    
    title <- paste0(res$value[1],", ",tolower(res$value[2])," and ",tolower(res$value[3])," are the most important sustainability and social initiatives to students.")
    
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 18)))
    
    my_pres <- my_pres %>% on_slide(index=24) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       left=.05,top=.05,height=.95,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","plotSustain.png"), 100/72, 76/72),
               location = ph_location(top=1.4,left=2.5,width=5,height=4.3 ), 
               use_loc_size = TRUE ) 
    
    my_pres <- my_pres %>% on_slide(index=24) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents (n=",format( nrow(data_school_c),big.mark = ","),")"),
                                        "Q31. Which of the following sustainability and social initiatives, if any, are most important to you? Select up to 2 initiatives.",
                                        "",""), 
                       left=.25,top=5.1,height=.75,width=9 )
  
    
    ################################################################################################
    # Slide twenty 4 heatlhy eating
  
    
    base <- data_school_c %>% select(dplyr::starts_with("Q74")) 
    
    # how many respondents made some selection for this
    base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
  
    
    res <- data_school_c %>% select(dplyr::starts_with("Q74")) %>% select(!dplyr::contains("Q74_12_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q74")) %>%
      filter(value!="") %>%
      group_by(value) %>% summarize(n=100*n()/base_n)  %>% arrange(desc(n))
    
    res$value <- gsub(" \\(please specify\\)","",res$value)
    
    title <- paste0(res$value[1]," and ",tolower(res$value[2])," are top priority when it comes to looking for a healthy meal option.")
    
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 20)))
    
    my_pres <- my_pres %>% on_slide(index=25) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","plotPriority.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=1,width=7.5,height=4 ), 
               use_loc_size = TRUE ) 
    
    my_pres <- my_pres %>% on_slide(index=25) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Answering Respondents (n=",format( base_n,big.mark = ","),")"),
                                        "Q32. When looking for a healthy meal option, what are your top priorities?",
                                        "",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    
    

    ################################################################################################
    # Slide twenty five  mental well being
    
    base <- data_school_c %>% select(dplyr::starts_with("Q75")) 
    
    # how many respondents made some selection for this
    base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
    
    
    res <- data_school_c %>% select(dplyr::starts_with("Q75")) %>% select(!dplyr::contains("Q75_9_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q75")) %>%
      filter(value!="") %>%
      group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n)) %>% 
      arrange(value=="I do not apply these types of practices") %>% 
      arrange(value=="I prefer not to answer")
    
    res$value <- gsub(" \\(please specify\\)","",res$value)
    
    #  adequate sleep and relaxation are most important to respondents.

    title <- paste0("When it comes to mental wellbeing, ",tolower(res$value[1])," and ",tolower(res$value[2]),
                    " are most important to respondents.")
    
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=26) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","plotWell.png"), 100/72, 76/72),
               location = ph_location(top=1.4,left=1,width=7.5,height=4 ), 
               use_loc_size = TRUE ) 
    
    my_pres <- my_pres %>% on_slide(index=26) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Answering Respondents (n=",format( base_n,big.mark = ","),")"),
                                        "Q33. When thinking of mental wellbeing, which of the following is most important to you?",
                                        "",""), 
                       left=.25,top=4.9,height=.75,width=9 )


    ######################### new slide 26
    base <- data_school_c %>% select(dplyr::starts_with("Q97")) 
    
    # how many respondents made some selection for this
    base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
    
    
    res <- roundQual(sum(data_school_c$Q97 %in% c("Extremely connected", 
                                        "Very connected")) * 100/base_n,0)
    
      
    #  adequate sleep and relaxation are most important to respondents.
    
    title <- paste0("About ",res,"% feel the dining program is highly connected to campus and local community.")
    
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=27) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slideComm.png"), 100/72, 76/72),
               location = ph_location(top=1.2,left=2.5,width=4.5,height=4 ), 
               use_loc_size = TRUE ) 
    
    my_pres <- my_pres %>% on_slide(index=27) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Answering Respondents (n=",format( base_n,big.mark = ","),")"),
                                        "Q34. How connected do you feel the dining program is to your campus and the local community?",
                                        "",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    
    ####################################################
    ##  NEw slide 27, word cloud 
    base <- data_school_c %>% select(dplyr::starts_with("Q98")) 
    
    # how many respondents made some selection for this
    base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
    
    

    if (!ca){
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
    
    freq<-table(rests)
    freq1<-data.frame(sort(freq, decreasing=TRUE))
    names(freq1) <- c('word', 'freq')
    
    
    
    
    #remove filler words
    freq1 <- freq1 %>% filter(!word %in% c('')) %>% head(10)

    #  adequate sleep and relaxation are most important to respondents.
    
    title <- paste0(freq1$word[1],", ",freq1$word[2],", and ",freq1$word[3]," are students' favorite and most frequented fast food/fast casual restaurants.")
    
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=28) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                        left=.05,top=.25,height=.75,width=9 ) %>% 
       ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","word_cloud.png"), 100/72, 76/72),
                location = ph_location(top=1.5,left=2.5,width=4.5,height=4 ),
               use_loc_size = TRUE )

    my_pres <- my_pres %>% on_slide(index=28) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Answering Respondents (n=",format( base_n,big.mark = ","),")"),
                                        "Q37. Please list your favorite three to five fast food or fast casual dining restaurants that you most frequent.  (Some examples include Chick-fil-A, Starbucks, Panera, Panda Express, Subway.)",
                                        "Note: Size of mentions in word cloud indicate how often it was mentioned by respondents. Top mentions shown.",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    
    
    ################################################################################################
    # Slide 28
    
    if (ca){
      res <- data.frame(table(data_school_c$Q99ca)) %>% subset(Var1!="")
    } else{
      res <- data.frame(table(data_school_c$Q99)) %>% subset(Var1!="")
    }
    
    base <- sum(res$Freq)
    res$per <- roundQual(res$Freq*100/base)
    res <- res %>% arrange(desc(per)) %>% mutate(word=gsub("\ .*","",Var1))
    
    if (ca){
      title <- paste0("When it comes to motivation to visit a favourite fast food or fast casual restaurant, ",
                      res$per[1],"% of students are most motivated by ",tolower(res$word[1])," over ",tolower(res$word[2]),".")
      
    } else{
      title <- paste0("When it comes to motivation to visit a favorite fast food or fast casual restaurant, ",
                      res$per[1],"% of students are most motivated by ",tolower(res$word[1])," over ",tolower(res$word[2]),".")
      
    }
    

    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 18)))
    
    my_pres <- my_pres %>% on_slide(index=29) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       left=.05,top=.25,height=.75,width=9 )  %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","pq_plot.png"), 100/72, 76/72),
               location = ph_location(top=2,left=.75,width=2.5,height=2.5 ), 
               use_loc_size = TRUE )  %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","plot_pq2.png"), 100/72, 76/72),
               location = ph_location(top=1.45,left=5.5,width=3.5,height=4 ), 
               use_loc_size = TRUE ) 
    
    sample2 = " Q38. When you decide to visit one of your favorite fast food or fast casual dining restaurants (e.g. Chick-fil-A, Starbucks, Panera, Panda Express, Subway), are you more motivated to go there because of the price or the quality of the meal?"
    if (ca){
      sample2 = gsub('favorite', 'favourite', sample2)
    }
    
    my_pres <- my_pres %>% on_slide(index=29) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Answering Respondents (n=",format( base,big.mark = ","),")"),
                                        sample2,
                                        "Q39. Besides price or quality, what else may motivate you to visit your favorite fast food or fast casual dining restaurants?",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    
    ############################## Slide 29 
    base <- data_school_c %>% select(dplyr::starts_with("Q102")) 
    
    # how many respondents made some selection for this
    base_n <- sum(apply(base,MARGIN=1,FUN=function(x){sum(x!="")})!=0)
    
    
    res <- roundQual(sum(data_school_c$Q102 %in% c("Extremely easy", 
                                                  "Somewhat easy")) * 100/base_n,0)
    
    
    #  adequate sleep and relaxation are most important to respondents.
    
    title <- paste0("About ",res,"% of students feel it is easy to order and to pick up food on campus.")
    
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 22)))
    print("at slide 30")
    my_pres <- my_pres %>% on_slide(index=30) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slidePickup.png"), 100/72, 76/72),
               location = ph_location(top=1.1,left=2.5,width=4.5,height=4 ), 
               use_loc_size = TRUE ) 
    
    my_pres <- my_pres %>% on_slide(index=30) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Answering Respondents (n=",format( base_n,big.mark = ","),")"),
                                        "Q40. To what extent would you say food on campus is easy to order and to pick up?",
                                        "",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    
    ################################### 
   ### Slide 30 
 
    res <- data.frame(table(data_school_c$Q103)) %>% subset(Var1!="") 
    base <- sum(res$Freq)
    res$per <- roundQual(res$Freq*100/base)
    res <- res %>% 
      arrange(Var1=="No") %>% 
      mutate(label=paste0(per,"%"))
    

    res2 <- data.frame(table(data_school_c$Q104)) %>% subset(Var1!="") 
    base <- sum(res2$Freq)
    res2$per <- roundQual(res2$Freq*100/base)
    res2 <- res2 %>% 
      arrange(Var1=="No") %>% 
      mutate(label=paste0(per,"%"))
    
    
    
    title <- paste0("About ",res$label[res$Var1=="Yes"]," of students state their campus offers mobile app/self-ordering kiosk ordering, of which ",res2$label[res2$Var1=="Yes"]," have used it.")
    
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 22)))
    
    my_pres <- my_pres %>% on_slide(index=31) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","hasMobile.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=.3,width=2.5,height=2.5 ), 
               use_loc_size = TRUE )  %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","hasKiosk.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=3.1,width=2.5,height=2.5 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","oft_out.png"), 100/72, 76/72),
               location = ph_location(top=1.9,left=6.5,width=2.5,height=1.5 ), 
               use_loc_size = TRUE )  %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","not_out.png"), 100/72, 76/72),
               location = ph_location(top=4,left=6.5,width=2.5,height=1.1 ), 
               use_loc_size = TRUE ) 
    
    
    #count of reasons for not ordering, weirdl logic because can have multiple reasons
    qs <- c('Q107_1','Q107_4', 'Q107_5', 'Q107_6' )
    
    
    res_no <- data_school_c %>% select(qs,ResponseId) %>% 
      mutate(id=row.names(.)) %>% 
      select(!dplyr::contains("_SDS")) %>%
      pivot_longer(cols=dplyr::starts_with("Q107")) %>%
      filter(value!="") %>%
      mutate(value = trimws(value)) %>%
      select(-id) %>%
      summarize(l=length(unique(ResponseId))) %>%
      ungroup() 
    
    my_pres <- my_pres %>% on_slide(index=31) %>% 
      ph_with_fpars_at(fpars=add_sample(
                                        paste0("Whose Campuses Offer Mobile App/ Self-Ordering Kiosk Ordering (n=",format( sum(data_school_c$Q103=="Yes",na.rm=T),big.mark = ","),")/ Whose Campuses Do Not Offer Mobile App/ Self-Ordering Kiosk Ordering (n=",sum(data_school_c$Q103=="No",na.rm=T),")/ Have Ever Used Mobile App/ Self-Ordering Kiosk Ordering (n=",sum(data_school_c$Q104=="Yes",na.rm=T),")/ Reasons for Not Ever Used Mobile App/ Self-Ordering Kiosk Ordering (num reasons=",res_no$l,")"),
                                        "See question text in notes section","",""), 
                       left=.25,top=5.0,height=.75,width=9 )
    
    ########### slide 31
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
    
    
    title <- paste0(res_s$value[1]," and ",tolower(res_s$value[2])," are technologies students would most like to see on campus.")
    
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 20)))
    
    my_pres <- my_pres %>% on_slide(index=32) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       left=.05,top=.25,height=.75,width=9 ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","plotTech.png"), 100/72, 76/72),
               location = ph_location(top=1.8,left=1,width=3.3,height=3.5 ), 
               use_loc_size = TRUE )  %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","tech_plot.png"), 100/72, 76/72),
               location = ph_location(top=1.7,left=6.5,width=3,height=3.3 ), 
               use_loc_size = TRUE ) 
    
    
    
    my_pres <- my_pres %>% on_slide(index=32) %>% 
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Answering Respondents (n=",format( base_n,big.mark = ","),")"),
                                       "Q47. Please select which technology you would like to see at your campus, if it doesn't already exist." ,
                                        "Q46.  Would you buy Ramen Noodles or Pizza from a Robotic Vending machine?",""), 
                       left=.25,top=4.9,height=.75,width=9 )
    
      

    
    ################################################################################################
    # Slide32 -26

    
    my_pres <- my_pres %>% on_slide(index=33) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","sat_donut.png"), 100/72, 76/72),
               location = ph_location(top=1.6,left=1.65,width=1.5,height=1.5 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","val_donut.png"), 100/72, 76/72),
               location = ph_location(top=2.8,left=1.65,width=1.5,height=1.5 ), 
               use_loc_size = TRUE )  
      if(!ca){
        my_pres <- my_pres%>% 
          ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","meal_donut.png"), 100/72, 76/72),
                   location = ph_location(top=4,left=1.65,width=1.5,height=1.5 ), 
                   use_loc_size = TRUE )
      }
      
    my_pres <- my_pres%>% 
      on_slide(index=34) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","quality_donut.png"), 100/72, 76/72),
               location = ph_location(top=1.6,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","variety_donut.png"), 100/72, 76/72),
               location = ph_location(top=2.5,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","nutrition_donut.png"), 100/72, 76/72),
               location = ph_location(top=3.4,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","allergy_donut.png"), 100/72, 76/72),
               location = ph_location(top=4.3,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>%
      on_slide(index=35) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","healthy_donut.png"), 100/72, 76/72),
               location = ph_location(top=1.6,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","specialdiet_donut.png"), 100/72, 76/72),
               location = ph_location(top=2.5,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","organic_donut.png"), 100/72, 76/72),
               location = ph_location(top=3.4,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","fresh_donut.png"), 100/72, 76/72),
               location = ph_location(top=4.3,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE )%>%
      on_slide(index=36) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","price_donut.png"), 100/72, 76/72),
               location = ph_location(top=1.6,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","afford_donut.png"), 100/72, 76/72),
               location = ph_location(top=2.5,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE )%>%
      on_slide(index=37) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","conv_donut.png"), 100/72, 76/72),
               location = ph_location(top=1.6,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","welcome_donut.png"), 100/72, 76/72),
               location = ph_location(top=2.5,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","know_donut.png"), 100/72, 76/72),
               location = ph_location(top=3.4,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","speed_donut.png"), 100/72, 76/72),
               location = ph_location(top=4.3,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE )%>%
      on_slide(index=38        ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","clean_donut.png"), 100/72, 76/72),
               location = ph_location(top=1.6,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","hours_donut.png"), 100/72, 76/72),
               location = ph_location(top=2.5,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","social_donut.png"), 100/72, 76/72),
               location = ph_location(top=3.4,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","comfort_donut.png"), 100/72, 76/72),
               location = ph_location(top=4.3,left=1.85,width=1.2,height=1.2 ), 
               use_loc_size = TRUE )


 
    
    

  

    ############# Slide 28 - appendix 
  
    
    my_pres <- my_pres %>% on_slide(index=40) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide28a.png"), 100/72, 76/72),
               location = ph_location(top=1.3,left=.4,width=4.5,height=3.9), 
               use_loc_size = TRUE )  %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide28b.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=4.8,width=4.5,height=4), 
               use_loc_size = TRUE )  
    

      
    
    ################################################################################################
    # Slide 41 - appendix 

    title <- paste0( roundQual(100*sum(data_school_c$Q4.1=="I am required to participate in the meal plan")/nrow(data_school_c),0),"% ARE REQUIRED TO PURCHASE A DINING PLAN")
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 13,bold=T)))
    
    my_pres <- my_pres %>% on_slide(index=41) %>% 
      ph_with_fpars_at(fpars=list(title_p15), 
                       fp_pars = list(fp_par(text.align = "center")),
                       left=.75,top=1.15,height=.75,width=3.8 )
    
    my_pres <- my_pres %>% on_slide(index=41) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide29b.png"), 100/72, 76/72),
               location = ph_location(top=2,left=.6,width=3.8,height=3), 
               use_loc_size = TRUE )  
    if(!ca){
      my_pres <- my_pres %>% 
        ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide29a.png"), 100/72, 76/72),
                 location = ph_location(top=1.3,left=4.95,width=4.5,height=4), 
                 use_loc_size = TRUE ) 
    }
  
  
      
    ################################################################################################
    # Slide 30
 
    #66% 
    title <- paste0( roundQual(100*sum(data_school_c$Q69_1!="")/nrow(data_school_c),0),"%")
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 16)))
    
    titlemid <- fpar(
      ftext("of respondents have a food allergy or intolerance", fp_text( font.size = 12)))
    
    title2 <- paste0( "(",roundQual(100*sum(data_region_c$Q69_1!="")/nrow(data_region_c),0),"% - Regional, ",
                      roundQual(100*sum(data_c$Q69_1!="")/nrow(data_c),0),"% - National, ",
                      roundQual(100*sum(data_market_c$Q69_1!="")/nrow(data_market_c),0),"% - Market Segment)")
    title_p30 <- fpar(
      ftext(title2, fp_text( font.size = 12)))
    
    my_pres <- my_pres %>% on_slide(index=42) %>% 
      ph_with_fpars_at(fpars=list(title_p15,titlemid,title_p30), 
                       fp_pars = list(fp_par(text.align = "center"),fp_par(text.align = "center"), fp_par(text.align = "center")),
                       left=2.5,top=1.2,height=1,width=5 )
    
    my_pres <- my_pres %>% on_slide(index=42)%>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide30.png"), 100/72, 76/72),
               location = ph_location(top=2.25,left=.5,width=8.5,height=3), 
               use_loc_size = TRUE )  
    
  
  
    ################################################################################################
    # Slide 31
    
    #66% 
    title <- paste0( roundQual(100*sum(data_school_c$Q69_2!="")/nrow(data_school_c),0),"%")
    title_p15 <- fpar(
      ftext(title, fp_text( font.size = 16)))
    
    titlemid <- fpar(
      ftext("of respondents have specialty dietary requirements", fp_text( font.size = 12)))
    
    title2 <- paste0( "(",roundQual(100*sum(data_region_c$Q69_2!="")/nrow(data_region_c),0),"% - Regional, ",
                      roundQual(100*sum(data_c$Q69_2!="")/nrow(data_c),0),"% - National, ",
                      roundQual(100*sum(data_market_c$Q69_2!="")/nrow(data_market_c),0),"% - Market Segment)")
    title_p30 <- fpar(
      ftext(title2, fp_text( font.size = 12)))
    
    my_pres <- my_pres %>% on_slide(index=43) %>% 
      ph_with_fpars_at(fpars=list(title_p15,titlemid,title_p30), 
                       fp_pars = list(fp_par(text.align = "center"),fp_par(text.align = "center"), fp_par(text.align = "center")),
                       left=2.5,top=1.25,height=1,width=5 )
    
    my_pres <- my_pres %>% on_slide(index=43) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","slide31.png"), 100/72, 76/72),
               location =  ph_location(top=2.25,left=.5,width=8.5,height=3), 
               use_loc_size = TRUE )  
    
   
    
    ################################################################################################
    # Slide 32-6

    if(!ca){
    my_pres <- my_pres %>% on_slide(index=44) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","appendix1.png"), 100/72, 76/72),
               location = ph_location(top=1.4,left=1.5,width=6.5,height=3.9), 
               use_loc_size = TRUE )  %>% on_slide(index=45) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","appendix2.png"), 100/72, 76/72),
                                        location = ph_location(top=1.4,left=1.5,width=6.5,height=3.9), 
                                        use_loc_size = TRUE )  %>% on_slide(index=46) %>% 
          ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","appendix3.png"), 100/72, 76/72),
                   location = ph_location(top=1.4,left=1.5,width=6.5,height=3.9), 
                   use_loc_size = TRUE )  
      }
     
      
    my_pres <- my_pres %>% on_slide(index=52) %>% ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","appendix4.png"), 100/72, 76/72),
               location = ph_location(top=1.2,left=2.2,width=5,height=3.9), 
               use_loc_size = TRUE )  %>%
      ph_with_fpars_at(fpars=add_sample(paste0( "Among Total Respondents/ Who Answered for Specific Location (n=",format( nrow(data_school_c),big.mark = ","),"; n size will differ by outlet)"),
                                        "Q13.  Which of the following dining locations on-campus have you visited frequently this semester/quarter?",
                                        "Q14. How satisfied were you with the specific food you ordered at...?",""), 
                       left=.25,top=4.9,height=.75,width=9 )
   
    
    ######## new appendix slides 
    
    ## slide 47 and 49
    if(!ca){
    base <- nrow(data_school_c %>% filter(Q68_14=="Ability to use my meal plan off campus"))
    

    my_pres <- my_pres %>% on_slide(index=47) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","appendix47.png"), 100/72, 76/72),
               location = ph_location(top=1.4,left=1.5,width=6.5,height=3.9), 
               use_loc_size = TRUE ) 
    }
    my_pres <- my_pres %>% 
      on_slide(index=48) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","appendix48.png"), 100/72, 76/72),
               location = ph_location(top=1.4,left=1.5,width=6.5,height=3.9), 
               use_loc_size = TRUE )  %>% 
      on_slide(index=49) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","appendix49_1.png"), 100/72, 76/72),
               location = ph_location(top=1.4,left=1.5,width=6.5,height=3.9), 
               use_loc_size = TRUE )  %>%
      on_slide(index=50) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","appendix49_2.png"), 100/72, 76/72),
               location = ph_location(top=1.4,left=1.5,width=6.5,height=3.9), 
               use_loc_size = TRUE )  %>% 
      on_slide(index=51) %>% 
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","appendix51_1.png"), 100/72, 76/72),
               location = ph_location(top=1.35,left=2.5,width=6.5,height=1), 
               use_loc_size = TRUE )  %>%
      ph_with( external_img(paste0("graphs_spring_2023/",UNIVERSITY_NAME,"/","appendix51_2.png"), 100/72, 76/72),
               location = ph_location(top=2.25,left=2.5,width=6.5,height=3.25), 
               use_loc_size = TRUE )
  
      
    
    # move two appendix slides
    my_pres <- move_slide(my_pres, index = 40, to = 8)
    my_pres <- move_slide(my_pres, index = 41, to = 17)
    
   
  
   
    # Print Target 
    print(my_pres, target = paste0("fallPPTS_2022/",UNIVERSITY_NAME,".pptx"))

}



###############################################
################## Run reports Spring 2023 ######
###############################################

data_ca <- data[data$SCHOOL_CAN!="",]
data_us <- data[data$COUNTRY=="USA",]

# canada schools first 


uni_ca <- unique(data_ca$SCHOOL_USA)
data_ca$REGION_NAME <- ifelse(data_ca$REGION_NAME=="CENTRAL","Central Region",ifelse(data_ca$REGION_NAME=="EAST","East Region",data_ca$REGION_NAME))

for (uni in uni_ca){
  print(uni)
  
  withCallingHandlers({
    run_report(data_ca,last_data, uni,last_year="Fall 2022", ca=T)
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
}




# run harvest in with the rest because graphs will be manually moved

uni_fall22 <- all_reps[all_reps$last_year=="Fall 2022","SCHOOL_NAME",FALSE]


for (uni in uni_fall22$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    run_report(data_us,last_data,uni,last_year="Fall 2022", ca=F)
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}

# Run other years 
uni_spring22 <- all_reps[all_reps$last_year=="Spring 2022","SCHOOL_NAME",FALSE]


for (uni in uni_spring22$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    run_report(data_us,last_data_spring_22,uni,last_year="Spring 2022", ca=F)
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}

# Run other years fall 2021 or blank (which will then be handled internall)
uni_fall21 <- all_reps[all_reps$last_year%in%c("Fall 2021",""),"SCHOOL_NAME",FALSE]


for (uni in uni_fall21$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    run_report(data_us,last_data21,uni,last_year="Fall 2021", ca=F)
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}




###############################################
################## Run reports fall 2022 ######
###############################################
#run_report(data,last_data,UNIVERSITY_NAME,last_year="Fall 2021")
run_report(data,last_data, "University of Virginia",last_year="Fall 2022", ca=F)


# Run harvest reports- be sure to delete after moving graphs ove 
run_report(data,last_data, "Elon University",last_year="Spring 2022")

# Test case is Cal state LA, last seen fall 2021

#####################################################
#################### OLD #############################
#########################################################

######## run reports spring 2022

uni_spring21 <- all_reps[all_reps$last_year=="Spring 2021","SCHOOL_NAME",FALSE]

for (uni in uni_spring21$SCHOOL_NAME){
  run_report(data,last_data_spring21,uni,last_year="Spring 2021")
}

uni_fall21 <- all_reps[all_reps$last_year=="Fall 2021","SCHOOL_NAME",FALSE]
for (uni in uni_fall21$SCHOOL_NAME){
  run_report(data,last_data,uni,last_year="Fall 2021")
}

uni_fall20 <- all_reps[all_reps$last_year=="Fall 2020","SCHOOL_NAME",FALSE]
for (uni in uni_fall20$SCHOOL_NAME){
  run_report(data,last_data_fall20,uni,last_year="Fall 2020")
}

uni_none <- all_reps[all_reps$last_year=="","SCHOOL_NAME",FALSE]
for (uni in uni_none$SCHOOL_NAME){
  run_report(data,last_data,uni,last_year="")
}



### harvest if needed

uni_spring21_h <- all_reps_h[all_reps_h$last_year=="Spring 2021","SCHOOL_NAME",FALSE]


for (uni in uni_spring21_h$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    run_report(data,last_data_spring21,uni,last_year ="Spring 2021")
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}


## all other graphs have fall 2021 
uni_fall21_h <- all_reps_h[all_reps_h$last_year=="Fall 2021","SCHOOL_NAME",FALSE]


for (uni in uni_fall21_h$SCHOOL_NAME){
  print(uni)
  
  withCallingHandlers({
    run_report (data,last_data,uni,last_year ="Fall 2021")
  }, warning=function(w) {
    invokeRestart("muffleWarning")
  })
  
}



