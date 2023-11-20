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
    roundQual(100*sum(grepl("excellent|good|^agree|somewhat agree|strongly agree|definitely will buy|probably will buy|^extremely comfortable|^very comfortable|^very important|important",tolower(dataset[,x])))/sum(!is.na(dataset[,x]) & (!dataset[,x]%in%c("","Not Applicable/Don't Know"))),0)
    
  } else {
  roundQual(100*sum(grepl("excellent|good|^agree|somewhat agree|strongly agree|definitely will buy|probably will buy|^extremely comfortable|^very comfortable|^very important|important",tolower(dataset[,x])))/sum(!is.na(dataset[,x]) & (dataset[,x]!="")),0)
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
last_path <- "C:/Users/Claire/Documents/Consulting/Qualtrics/Aramark2023/AramarkFall2022/data_files"
#last_path <- 'data_files'
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





#for working
UNIVERSITY_NAME <-  "University of Virginia"
data_last <- last_data
last_year ="Spring 2023"


ca_data<- read.csv("Fall2023_ca.csv")
ca_data<- ca_data[3:nrow(ca_data),]
ca_data$SCHOOL_USA <- ca_data$SCHOOL_NAME

# handle renaming ca values

ca_data[,gsub("Q30_","Q30ca",names(ca_data)[grep("Q30_",names(ca_data))])] <- ca_data[,grep("Q30_",names(ca_data))]

data <- ca_data
last_data <- data_last
UNIVERSITY_NAME <- 'University of Toronto Scarborough'
ca=T

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
      my_pres <- read_pptx("template_canada.pptx") 
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
    
    # update size for CA
    
    if (ca){
      bold_face <- update(shortcuts$fp_bold(font.size = 32),color="white")
      
      university_par <- fpar(
        ftext(UNIVERSITY_NAME , prop = bold_face),fp_p = fp_par(text.align="center") )
      
      my_pres <-  my_pres %>% on_slide(index=1) %>%
        ph_with_fpars_at(fpars=list(university_par), 
                         left=.75,top=0,height=1,width=8
        ) 
 
    } else {
      university_par <- fpar(
        ftext(UNIVERSITY_NAME , prop = bold_face),fp_p = fp_par(text.align="right") )
      
      my_pres <-  my_pres %>% on_slide(index=1) %>%
        ph_with_fpars_at(fpars=list(university_par), 
                         left=1.75,top=-.29,height=1,width=8
        ) 
    }
    
  
    
    
    
  
    ######################################################################
    ####### SLide 2 ######################################################
 
    # Add Sample and Nsize with formatting for numbers 
    UNIVERSITY_N <- n_format(nrow(data_school_c)) # sample size for universty 
    STUDENT_N <- n_format(nrow(data_school)) 
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
    s3 <- " Q3. Where do you live?"
      

    
    my_pres <- my_pres %>% on_slide(index=2) %>%
      ph_with_fpars_at(fpars=list(university_name_par,university_par), 
                       left=1.5,top=2.5,height=1,width=1.5 ) %>% # this segement adds university n
      ph_with_fpars_at(fpars=list(region_name_par,region_par), 
                       left=-.1,top=3.75,height=1,width=1.5 ) %>%  # this segement adds region
      ph_with_fpars_at(fpars=list(national_name_par,national_par), 
                       left=-.1,top=2.15,height=1,width=1.5 ) %>%
      ph_with_fpars_at(fpars=add_sample('', s1, s2, s3), 
                       left=.25,top=4.85,height=.75,width=9) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","location_2a.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=2.5,width=4,height=3.5)) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","commuting_2b.png"), 100/72, 76/72),
              location = ph_location(top=1.35,left=6.2,width=3.5,height=3.5))


    
    ####### SLide 3
    ################################################################################################
    
    
    ##### Box 1
    
    UNIVERSITY_SAT <- top_two("Q5",data_school)
    sat_text1 = paste0(UNIVERSITY_SAT, "%")
    sat_text2= " of students are satisfied with their dining program"
    
    if(HAS_LAST){
      current <- top_two("Q5",data_school)
      last <- top_two("Q2.1",data_school_last)
        
      changed <- ifelse(current < last,'decrease',"increase")
      changed <- ifelse(current==last,'no change from',changed)
      
      if (changed != 'no change from'){
        sat_text2 <- paste0(sat_text2, ifelse(changed=='increase', ", an ", ', a '))
        sat_text3 = paste0(changed," of ", roundQual(abs(current-last), 0),  '% ')
        sat_text4 = paste0("from ", last_year, '.')

      } else{
        sat_text3 = "the same as"
        sat_text4 = paste0(last_year, '.')
      }

     
      
    } else{
      # Set up title 
      sat_text2 <- paste0(sat_text2, '.')
      sat_text3 = ''
      sat_text3 = ''
      
    }
    
    
    #top rated metrics
    questions = c("Q10_1", "Q10_2", "Q10_3", "Q10_4", "Q10_5", "Q10_6", 
                  "Q10_7", "Q10_9", "Q10_10", "Q10_11", "Q11_1", "Q11_2", "Q11_3", 
                  "Q11_4", "Q11_5", "Q11_6", "Q11_7", "Q11_8", "Q11_9")
    q_names = c(" Food quality", " Food variety", " Availability of nutrition information", 
                " Availability of ingredient/allergen information", " Availability of healthy options", 
                " Price/Value", " Availability of special dietary options", 
                " Freshness of food", " Affordability", " Made from sustainably sourced products", 
                " Convenience", " Welcoming/Friendly staff", " Knowledgeable/Helpful staff", 
                " Speed of service", " Cleanliness", " Hours of operation", " Place to socialize", 
                " Comfortable dining experience", " Technology to support dining program")
    
    vals <- c()
    for (q in questions){
      vals = c(vals,top_two(q,data_school))
    }
    
    data_7 <- data.frame(cbind(q_names, vals))
    data_7$vals <- as.numeric(data_7$vals)
    data_7 <- data_7 %>% arrange(desc(vals))
    data_7$pct <- paste0(as.character(data_7$vals), '%')
    
    
    max_agree <- trimws(data_7[ c(1), c('q_names')])
    per_max_agree <- data_7[ c(1), c('pct')]
    
    min_agree <- trimws(data_7[ c(nrow(data_7)), c('q_names')])
    per_min_agree <- data_7[ c(nrow(data_7)), c('pct')]
    
    thumbs_up <- paste0(" is the top rated attribute with a ", per_max_agree ," favorability.")
    thumbs_down <- paste0(" is the lowest rated attribute with a ", per_min_agree ," favorability.")
    

    
    sat_par = fpar(ftext(sat_text1,prop =  fp_text(font.size = 20, bold=TRUE)), 
      ftext(sat_text2,prop =  fp_text(font.size = 12)),
      ftext(sat_text3, prop=fp_text(font.size=12, bold=TRUE)),
      ftext(sat_text4, prop=fp_text(font.size=12))
      )
    
    thumbs_up_par = fpar(ftext(max_agree, prop=fp_text(font.size=12, bold=TRUE)), 
                         ftext(thumbs_up, prop=fp_text(font.size=12)))
    thumbs_down_par = fpar(ftext(min_agree, prop=fp_text(font.size=12, bold=TRUE)), 
                         ftext(thumbs_down, prop=fp_text(font.size=12)))
    
    ##### Box 2 - Satisfaction with Value
    #satisfied with the value of the meal plan, 
    VAL_UNIVERSITY_SAT <- top_two("Q7",data_school)
    val_sat_text1 = paste0(VAL_UNIVERSITY_SAT, "%")
    val_sat_text2= " of students are satisfied with the value of the meal plan"
    
    if(HAS_LAST){
      current <- top_two("Q7",data_school)
      last <- top_two("Q2.4",data_school_last)
      
      changed <- ifelse(current < last,'decrease',"increase")
      changed <- ifelse(current==last,'no change from',changed)
      
      if (changed != 'no change from'){
        val_sat_text2 <- paste0(val_sat_text2, ifelse(changed=='increase', ", an ", ', a '))
        val_sat_text3 = paste0(changed," of ", roundQual(abs(current-last), 0),  '% ')
        val_sat_text4 = paste0("from ", last_year, '.')
        
      } else{
        val_sat_text3 = "the same as"
        val_sat_text4 = paste0(last_year, '.')
      }
      
    } else{
      val_sat_text2 <- paste0(sat_text2, '.')
      val_sat_text3 = ''
      val_sat_text3 = ''
      
    }
    
    val_sat_par = fpar(ftext(val_sat_text1,prop =  fp_text(font.size = 20, color = "white", bold=TRUE)), 
                   ftext(val_sat_text2,prop =  fp_text(font.size = 12, color = "white")),
                   ftext(val_sat_text3, prop=fp_text(font.size=12, bold=TRUE, color = "white")),
                   ftext(val_sat_text4, prop=fp_text(font.size=12, color = "white")),
                   fp_p = fp_par(text.align="center"))
    
    ##### Box 3 Likelyhood to purchase
    
    #only for non-graduating
    if (ca==F){
      base_data <- data_school%>% filter(Q4 %in% c("Yes")) %>% filter(Q19!="Graduating")
    } else {
      base_data <- data_school%>% filter(Q4ca %in% c("Yes")) %>% filter(Q19!="Graduating")
    }
    
    pct_by <- top_two("Q17",base_data)
    buy_text1 = paste0(pct_by, "%")
    buy_text2= " of students are likely to purchase a meal plan next year."
    buy_par = fpar(ftext(buy_text1,prop =  fp_text(font.size = 20, bold=T)), 
                       ftext(buy_text2,prop =  fp_text(font.size = 12)),
                       fp_p = fp_par(text.align="center"))
    
    base <- nrow(base_data) 
    res_s <- base_data %>%
      select(dplyr::starts_with("Q20")) %>% select(!dplyr::contains("Q20_9_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q20")) %>%
      filter(value!="") %>%
      mutate(value= gsub(" \\(please specify)","",value)) %>% 
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
    
    top_driver <- tolower(res_s[ c(1), c('value')][[1]])
    per_top <- paste0('At ', roundQual(res_s[ c(1), c('n')][[1]], 1), "%, ")
    
    driver_text=' is the largest incentive for improving dining plan participation followed closely by '
    
    next_driver <- tolower(res_s[ c(2), c('value')][[1]])
    third_driver <- tolower(res_s[ c(3), c('value')][[1]])
    
    other_drivers= paste0(next_driver, ' and ', third_driver, '.')
    
    driver_par = fpar(ftext(per_top,prop =  fp_text(font.size = 12)), 
                   ftext(top_driver,prop =  fp_text(font.size = 12, bold=T)),
                   ftext(driver_text,prop =  fp_text(font.size = 12)), 
                   ftext(other_drivers,prop =  fp_text(font.size = 12, bold=T)),
                   fp_p = fp_par(text.align="center"))
    
    ####### Box 4 : Allergies and intolerances
    
    eligible = c('Milk', 'Eggs', 'Peanuts', 'Tree nuts', 'Fish', 'Shellfish', 'Soy', 
                 'Wheat/Gluten', 'Sesame',  'Other (please specify):')
    if (ca){
      eligible = c(eligible, 'Mustard')
    }
    
    
    res <- data_school %>% filter(Q23_1=="Food allergies or intolerances")%>% 
      select(dplyr::starts_with("Q24")) %>% select(!dplyr::contains("Q24_11_TEXT")) %>%
      #select(!dplyr::contains("Q70_SDS")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q24")) %>%
      filter(value!="") %>%
      group_by(value) %>% summarize(n=100*n()/nrow(data_school))  %>% arrange(desc(n)) %>% 
      filter(value!='None of the above') %>% 
      filter(value %in% eligible) %>%
      arrange(value %in% c("Other (please specify):"))
    
    top_allergy <- paste0(tolower(res[ c(1), c('value')][[1]]), '.')
    
    eligible = c('Vegetarian', 'Vegan', 'Halal', 'Kosher', 'Paleo/Primal', 'Ketogenic', 'Mediterranean', 'Other (please specify):')
    if (!ca){
      eligible = c(eligible, c('Atkins', 'GMO-Free', 'Whole 30', 'Plant-based', 'Other',"Low-FODMAP","Pescatarian"))
    } else {
      eligible = c(eligible, c( 'Flexitarian', 'No Peanut/Tree Nut', 'No Dairy', 'No Gluten', 'Other'))
    }
    
    res2 <- data_school %>% filter(Q23_2=="Special dietary lifestyle for religious reasons"|Q23_3=="Special dietary lifestyle for medical reasons"|Q23_4=="Special dietary lifestyle for personal reasons")%>% 
      select(dplyr::starts_with("Q25")) %>% select(!dplyr::contains("Q25_18_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q25")) %>%
      filter(value!="") %>%
      filter(value %in% eligible) %>%
      group_by(value) %>% summarize(n=100*n()/nrow(data_school))  %>% arrange(desc(n)) %>% 
      filter(value!='None of the above') %>% 
      arrange(value %in% c("Other (please specify):"))
    
    
    top_diet <- res2[ c(1), c('value')][[1]]
    
    #allergy or intolerance
    n_allergy <- nrow(data_school[data_school$Q23_1=="Food allergies or intolerances"|data_school$Q23_2=="Special dietary lifestyle for religious reasons"|data_school$Q23_3=="Special dietary lifestyle for medical reasons"|data_school$Q23_4=="Special dietary lifestyle for personal reasons",])
    
    p_allergy <- paste0(roundQual(n_allergy/nrow(data_school)*100,0),"%")
    
    allergy_text <- ' of respondents have a food allergy or dietary requirement'
    
    
    allergy_par = fpar(ftext(p_allergy,prop =  fp_text(font.size = 20, bold=T)), 
                   ftext(allergy_text,prop =  fp_text(font.size = 12)),
                   fp_p = fp_par(text.align="center"))
    
    allergy_text <-"- The most common allergy is "
    diet_text <-  ' is the most common diet.'
    
    
    
    diet_block <- block_list(fpar(ftext(allergy_text,prop =fp_text(font.size = 12)), ftext(top_allergy,prop =fp_text(font.size = 12, bold=T))),
                             fpar( ftext(" ",prop =fp_text(font.size = 12))),
                             fpar(ftext("- ",prop =fp_text(font.size = 12)), ftext(top_diet,prop =fp_text(font.size = 12, bold=T)), ftext(diet_text,prop =fp_text(font.size = 12)))
                             )

    
    ######## Box 5 - Sustainability
    
    if (!ca){
      base <- data_school %>% select(dplyr::starts_with("Q30"))  %>% select(!dplyr::contains("Q30ca"))
      res <- data_school %>% select(dplyr::starts_with("Q30")) %>% select(!dplyr::contains("Q30_17_TEXT")) %>%
        select(!dplyr::contains("Q30_SDS")) 
    } else {
      base <- data_school %>% select(dplyr::starts_with("Q30ca"))
      res <- data_school %>% select(dplyr::starts_with("Q30ca")) %>% select(!dplyr::contains("TEXT")) %>%
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
    
    top_priority <- res[ c(1), c('value')][[1]]
    priority_text <- " is the #1 most important initiative to students."
    
    priority_par = fpar(ftext(top_priority,prop =  fp_text(font.size = 20, bold=T, color='white')), 
                       ftext(priority_text,prop =  fp_text(font.size = 12, color='white')),
                       fp_p = fp_par(text.align="center"))
    
    ###### Box 6 -- Technology
    
    res <- data.frame(table(data_school$Q39)) %>% subset(Var1!="") 
    base <- sum(res$Freq)
    res$per <- roundQual(res$Freq*100/base)
    
    pct_use <- paste0(res[res$Var1=='Yes',]$per, '%')
    use_text <- ' of students use the mobile app or self-ordering kiosk.'
    
    
    tech_par1 <- fpar(ftext(pct_use,prop =  fp_text(font.size = 20, bold=T)), 
         ftext(use_text,prop =  fp_text(font.size = 12)),
         fp_p = fp_par(text.align="center"))
    
    oft <- data_school %>% filter(Q41 !="") %>% group_by(Q41) %>% summarize(n=n()) %>% 
      mutate(per=paste0(roundQual(100*n/sum(n)),"%")) %>%  complete(Q41=c("0 times / week",  "1-2 times / week", "3-5 times / week",
                                                                          "6-9 times / week", "10+ times / week"),fill=list(n=0,per="0%")) %>% 
      mutate(Q41 = factor(Q41,levels=c("0 times / week",  "1-2 times / week", "3-5 times / week",
                                       "6-9 times / week", "10+ times / week"))) %>%
      arrange(n)
    
    order_val <-  gsub('/', 'per', oft[ c(nrow(oft)), c('Q41')][[1]])
    order_pct <-oft[ c(nrow(oft)), c('per')][[1]]
    
    order_text <- paste0(order_pct, ' order via technology ', order_val, '.')
    
    tech_par2 <- fpar(ftext(order_text, prop=fp_text(font.size=12)), fp_p = fp_par(text.align="center"))
    
    qs <- c('Q43_1','Q43_4', 'Q43_5', 'Q43_6' )
    
    res <- data_school %>% select(qs,ResponseId) %>% 
      mutate(id=row.names(.)) %>% 
      select(!dplyr::contains("_SDS")) %>%
      pivot_longer(cols=dplyr::starts_with("Q43")) %>%
      filter(value!="") %>%
      mutate(value = trimws(value)) %>%
      select(-id) %>%
      group_by(value) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      mutate(sum_t = sum(count)) %>%
      arrange(desc(count))
    
    reason <- res[ c(1), c('value')][[1]]
    #tidy reason to sentence form
    reason <- ifelse(reason=='I prefer to order in person', 'Preferring to order in person', reason)
    reason <- ifelse(reason=='I do not want to pay a transaction fee in the mobile app', 'The transaction fee', reason)
    reason <- ifelse(reason=='I have no interest in ordering via a mobile app or self-ordering kiosk', 'Lack of interest', reason)
  
    reason_text <- " is the biggest barrier for those not ordering via technology."
    
    tech_par3 <- fpar(ftext(reason, prop=fp_text(font.size=12, bold=T)),
                      ftext(reason_text, prop=fp_text(font.size=12)), fp_p = fp_par(text.align="center"))
    
    
    
    
    my_pres <- my_pres %>% on_slide(index=3) %>%
      ph_with_fpars_at(fpars=list(sat_par), 
                       left=.4,top=1.3,height=1,width=2.8 ) %>%
      ph_with_fpars_at(fpars= list(thumbs_up_par),
                       left=.7,top=2.2,height=.5,width=2.6 ) %>%
      ph_with_fpars_at(fpars= list(thumbs_down_par),
                       left=.7,top=2.7,height=.5,width=2.6 ) %>%
      ph_with_fpars_at(fpars= list(val_sat_par),
                       left=3.7,top=1.9,height=.5,width=2.6 ) %>%
      ph_with_fpars_at(fpars= list(buy_par),
                       left=6.85,top=1.3,height=.75,width=2.9 ) %>%
      ph_with_fpars_at(fpars= list(driver_par),
                       left=6.85,top=2,height=1,width=2.9 ) %>%
      ph_with_fpars_at(fpars=list(allergy_par), 
                       left=.4,top=3.7,height=1,width=2.8 ) %>%
      ph_with(type = "body",
                 diet_block,
                 level_list = c(1L,1L),
                 style = fp_text( font.size = 12),
                 location = ph_location(left=.45,width=2.6,top=4.5, height=2))%>%
      ph_with_fpars_at(fpars= list(priority_par),
                       left=3.7,top=4.1,height=1,width=2.6 ) %>%
      ph_with_fpars_at(fpars=list(tech_par1), 
                       left=6.85,top=3.7,height=1,width=2.8 ) %>%
      ph_with_fpars_at(fpars= list(tech_par2),
                       left=6.85,top=4.3,height=.5,width=2.6 ) %>%
      ph_with_fpars_at(fpars= list(tech_par3),
                       left=6.85,top=4.8,height=.5,width=2.6 )
      
    ####### SLide 4
    ################################################################################################
    current <- top_two("Q5",data_school)
    market <- top_two("Q5",data_market)
    
    comparator <- ifelse(current-market>0, 'higher than', 'lower than')
    comparator <- ifelse(abs(current-market)<5, 'about the same as', comparator)
    
    
    
    if (HAS_LAST){
      last <- top_two("Q2.1",data_school_last)
          
      decline <- (last - current)>5
      increase <- (current- last)>5
      
      prefix <- ifelse(decline & comparator != 'lower than', 'Despite decline,', 'Remaining about the same as last year,')
      prefix <- ifelse(decline & comparator == 'lower than', 'Declining from last year,', prefix)
      prefix <- ifelse(increase & comparator != 'lower than', 'Increasing from last year,,', prefix)
      prefix <- ifelse(increase & comparator == 'lower than', 'Despite increase,', prefix)
  
      title <- paste0(prefix, " students rate overall experience ", comparator, " comparable segments." )
      s1 <- paste0('Among Total Respondents (n=', UNIVERSITY_N, ',', nrow(data_school_c_last), ')')
    } else {
      
      title <- paste0( "Students rate overall experience ", comparator, " comparable segments." )
      s1 <- paste0('Among Total Respondents (n=', UNIVERSITY_N, ')')
    }
    
    
    title_par <- fpar(
                 ftext(title,
                fp_text( font.size = 21)))
  
    s2 <- "Q5. Overall, how would you rate your campus dining program?"
   
    
    my_pres <- my_pres %>% on_slide(index=4) %>%
      ph_with_fpars_at(fpars=list(title_par), 
                       left=.25,top=.25,height=.75,width=9.5 )%>%
      ph_with_fpars_at(fpars=add_sample('', "", s1, s2), 
                       left=.25,top=4.75,height=.75,width=9) %>%       
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","sat_4a.png"), 100/72, 76/72),
           location = ph_location(top=1.5,left=.5,width=3,height=3)) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","sat_4b.png"), 100/72, 76/72),
              location = ph_location(top=1.5,left=4.5,width=5,height=3))
    

    ########################## Slide 5 #
    s1 <-  paste0('Among Total Respondents (n=', UNIVERSITY_N, ')')
    s2 <- "Q13 Please rate your satisfaction with the following dining locations."
    
    
    my_pres <- my_pres %>% on_slide(index=5) %>%
      ph_with_fpars_at(fpars=add_sample('', "", s1, s2), 
                       left=.25,top=4.6,height=.75,width=9) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide5.png"), 100/72, 76/72),
              location = ph_location(top=.8,left=1.5,width=6,height=4))
    
    ####################################################
    ########################## Slide 6 #
    s1 <-  paste0('Among Student Respondents (n=', STUDENT_N,',', nrow(data_school_last) , ')')
    s2 <- "Q7. How would you rate the value you receive when dining on campus?"
    s3 <- "Q9. Which statement below best describes how you value a meal?"
    
    driver_df  <- data_school%>% group_by(Q9) %>% summarize(n=n()) %>% arrange(desc(n))
    
    
    clean_value <- function(value){
      clean <- ifelse(value=='Value to me is about the quality of the food', 'quality', '')
      clean <- ifelse(value=='Value to me is about the overall experience', 'overall experience', clean)
      clean <- ifelse(value=='Value to me is about how much I pay', 'cost', clean)
      clean <- ifelse(value=='	Value to me is about how much food I receive', 'quantity', clean)
    return(clean)
     }
    

    title6 <- paste0("Main drivers of value rating are ", clean_value(driver_df[c(1), c('Q9')][[1]]), ' and ',clean_value(driver_df[c(2), c('Q9')][[1]]) , '.')

    

    
    title_6_par <- fpar(
      ftext(title6, fp_text( font.size = 22)))
  
    
    my_pres <- my_pres %>% on_slide(index=6) %>%
      ph_with_fpars_at(fpars=list(title_6_par), 
                       left=.25,top=.25,height=.75,width=9.5 ) %>%
      ph_with_fpars_at(fpars=add_sample('', s1, s2, s3), 
                       left=.25,top=4.75,height=.75,width=9) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide6a.png"), 100/72, 76/72),
              location = ph_location(top=1.5,left=.5,width=3.5,height=3.5)) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide6b.png"), 100/72, 76/72),
              location = ph_location(top=1.5,left=5,width=3.5,height=3.5))
    
    ####################################################
    ########################## Slide 7 #
    questions = c("Q10_1", "Q10_2", "Q10_3", "Q10_4", "Q10_5", "Q10_6", 
                  "Q10_7", "Q10_9", "Q10_10", "Q10_11", "Q11_1", "Q11_2", "Q11_3", 
                  "Q11_4", "Q11_5", "Q11_6", "Q11_7", "Q11_8", "Q11_9")
    q_names = c("Food quality", "Food variety", "Availability of nutrition information", 
                "Availability of ingredient/allergen information", " Availability of healthy options", 
                "Price/Value", "Availability of special dietary options", 
                "Freshness of food", "Affordability", "Made from sustainably sourced products", 
                "Convenience", "Welcoming/Friendly staff", "Knowledgeable/Helpful staff", 
                "Speed of service", "Cleanliness", "Hours of operation", "Place to socialize", 
                "Comfortable dining experience", "Technology to support dining program")
    
    vals <- c()
    for (q in questions){
      vals = c(vals,top_two(q,data_school_c))
    }
    
    data_7 <- data.frame(cbind(q_names, vals))
    data_7$vals <- as.numeric(data_7$vals)
    data_7 <- data_7 %>% arrange(desc(vals))
    data_7$pct <- paste0(as.character(data_7$vals), '%')
    
    
  
    
    title7 <- paste0("The highest rated performance metrics are ", tolower(data_7[c(1), c('q_names')][[1]]),', ', tolower(data_7[c(2), c('q_names')][[1]]),  ' and ',tolower(data_7[c(3), c('q_names')][[1]]), '.')
    

    if (ca==F){
      questions2 <- c('Q11A_USA_1', 'Q11A_USA_2','Q11A_USA_3','Q11A_USA_4','Q11A_USA_5','Q11A_USA_6','Q11A_USA_7','Q11A_USA_8','Q11A_USA_9')
    } else{
      #TODO check CA questionnames
      questions2 <- c('Q11A_CA_1', 'Q11A_CA_2','Q11A_CA_3','Q11A_CA_4','Q11A_CA_5','Q11A_CA_6','Q11A_CA_7','Q11A_CA_8','Q11A_CA_9')
      
    }
    
    q_names_2 <- c('Availability of special dietary options', 'Cleanliness','Comfortable dining experience','Convenience','Food quality', 'Food variety', 'Knowledgeable/Helpful staff', 'Price/Value','Welcoming/Friendly staff')
    
    vals <- c()
    for (q in questions2){
      vals = c(vals,top_two(q,data_school_c))
    }
    
    data_7_2 <- data.frame(cbind(q_names_2, vals))
    data_7_2$vals <- as.numeric(data_7_2$vals)
    data_7_2 <- data_7_2 %>% arrange(desc(vals))
    data_7_2$pct <- paste0(as.character(data_7_2$vals), '%')
    
    top1 <- fpar(ftext(data_7_2[c(1), c('q_names_2')][[1]], fp_text( font.size = 12)))
    top2 <- fpar(ftext(data_7_2[c(2), c('q_names_2')][[1]], fp_text( font.size = 12)))
    top3 <- fpar(ftext(data_7_2[c(3), c('q_names_2')][[1]], fp_text( font.size = 12)))

    #cross reference for opportunity
    #keep only the ones in Q11a
    merged_7 = merge(data_7, data_7_2, by.x='q_names', by.y='q_names_2',all=T)
    #important - quality
    merged_7 <- merged_7 %>% mutate(dif = vals.y - vals.x )  %>% arrange(desc(dif))
    
    opp1 <- fpar(ftext(merged_7[c(1), c('q_names')][[1]], fp_text( font.size = 12)))
    opp2 <- fpar(ftext(merged_7[c(2), c('q_names')][[1]], fp_text( font.size = 12)))
    opp3 <- fpar(ftext(merged_7[c(3), c('q_names')][[1]], fp_text( font.size = 12)))
    

    title_7_par <- fpar(
      ftext(title7, fp_text( font.size = 22)))
    
    s1 <- paste0('Among Total Respondents (n=', UNIVERSITY_N, ')')
    s2 <- "Q10. Thinking about your campus dining program, how would you rate each of the following?"
    
    
    
    
    my_pres <- my_pres %>% on_slide(index=7) %>%
      ph_with_fpars_at(fpars=list(title_7_par), 
                       left=.25,top=.25,height=.75,width=9.5 ) %>%
      ph_with_fpars_at(fpars=add_sample('', "", s1, s2), 
                       left=.25,top=4.6,height=.75,width=9) %>%
      ph_with_fpars_at(fpars=list(top1), 
                       left=.5,top=1.83,height=.75,width=2.5 )%>%
      ph_with_fpars_at(fpars=list(top2), 
                       left=.5,top=2.14,height=.75,width=2.5 )%>%
      ph_with_fpars_at(fpars=list(top3), 
                       left=.5,top=2.45,height=.75,width=2.5 )%>%
      ph_with_fpars_at(fpars=list(opp1), 
                       left=.6,top=3.75,height=.75,width=2.5 )%>%
      ph_with_fpars_at(fpars=list(opp2), 
                       left=.6,top=3.95,height=.75,width=2.5 ) %>%
      ph_with_fpars_at(fpars=list(opp3), 
                       left=.6,top=4.15,height=.75,width=2.5 ) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide7.png"), 100/72, 76/72),
              location = ph_location(top=1.3,left=4.5,width=4.5,height=4)) 
    
    #################################################### 
    ########################## Slide 8 : Meal plan participation
    
    n_part <- nrow(data_school_c[data_school_c$Q15=="I participate in the meal plan although it is not required",])
    pct <- roundQual(n_part/nrow(data_school_c)*100)
    pct_nat <- roundQual(nrow(data_c[data_c$Q15=="I participate in the meal plan although it is not required",])/nrow(data_c)*100)
    
    
    qualifier <- ifelse(abs(pct - pct_nat) < 5, 'about the same as', ' ')
    qualifier <- ifelse(pct-pct_nat >= 5, 'higher than', qualifier)
    qualifier <- ifelse(pct-pct_nat > 10, 'much higher than', qualifier)
    qualifier <- ifelse(pct-pct_nat <= -5, 'lower than', qualifier)
    qualifier <- ifelse(pct-pct_nat < -10, 'much lower than', qualifier)
    
    
    title8 <- paste0(pct, '% of students take part in the meal plan even though participation is not required; ',  qualifier, ' the national average.')
    title_8_par <- fpar(
      ftext(title8, fp_text( font.size = 22)))
    
    
    if (ca==F){
      base_data <- data_school_c%>% filter(Q4 %in% c("Yes")) %>% filter(Q19!="Graduating")
    } else {
      base_data <- data_school_c%>% filter(Q4ca %in% c("Yes")) %>% filter(Q19!="Graduating")
    }
    
   
    base <- nrow(base_data) 
    
    res_s <- base_data %>%
      select(dplyr::starts_with("Q20")) %>% select(!dplyr::contains("Q20_9_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q20")) %>%
      filter(value!="") %>%
      mutate(value= gsub(" \\(please specify)","",value)) %>% 
      #mutate(value = gsub("meal plan","dining plan",value)) %>% 
      #mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
      group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))

    subtitle <-toupper(paste0(res_s[c(1),]$value, ', ',res_s[c(2),]$value, ', and ', res_s[c(3),]$value," could increase dining program participation."))
    sub_title_8_par <- fpar(ftext(subtitle, fp_text(font.size=10, color='red')))
    
    eligible  <- data_school_c%>% filter(Q4 %in% c("Yes")) %>% filter(Q19!="Graduating")
    
    
    s1 <-  paste0('Among Total Respondents/ Who Participate Even Though It Is Not Required (n=', UNIVERSITY_N, ',',n_part,'), Who Plan to be On Campus Next Year (n=', nrow(eligible), ')')
    s2 <- "Q15. Which of the following best describes your current participation in your school's meal plan?"
    s3 <- "Q16. What research or information did you receive that led you to purchase a meal plan?"
    s4 <- "Q20. Which of these, if any, would make you more likely to purchase a meal plan in the future?"

    
    my_pres <- my_pres %>% on_slide(index=8) %>%
      ph_with_fpars_at(fpars=list(title_8_par), 
                       left=.25,top=.2,height=.75,width=9.5 ) %>%
      ph_with_fpars_at(fpars=list(sub_title_8_par), 
                       left=.35,top=.95,height=.75,width=9.5 ) %>%
      ph_with_fpars_at(fpars=add_sample(s1, s2, s3, s4), 
                       left=.25,top=5,height=.75,width=9) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide8a.png"), 100/72, 76/72),
              location = ph_location(top=1.5,left=.4,width=3,height=3)) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide8b.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=3.6,width=3,height=3))  %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide8c.png"), 100/72, 76/72),
              location = ph_location(top=1.5,left=6.6,width=3,height=3.85))
    
    ####################################################
    ########################## Slide 9 #
    #Totals different than slide 3 because this is ALL RESPONDENTS and slide 3 is students

    
    eligible = c('Milk', 'Eggs', 'Peanuts', 'Tree nuts', 'Fish', 'Shellfish', 'Soy', 
                 'Wheat/Gluten', 'Sesame',  'Other (please specify):')
    if (ca){
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
    
    top_allergy <- res[ c(1), c('value')][[1]]
    top_allergy <- ifelse(substr(top_allergy, nchar(top_allergy), nchar(top_allergy))=='s', paste(top_allergy, 'are'), paste(top_allergy, 'is'))
    
    eligible = c('Vegetarian', 'Vegan', 'Halal', 'Kosher', 'Paleo/Primal', 'Ketogenic', 'Mediterranean', 'Other (please specify):')
    if (!ca){
      eligible = c(eligible, c('Atkins', 'GMO-Free', 'Whole 30', 'Plant-based', 'Other',"Low-FODMAP","Pescatarian"))
    } else {
      eligible = c(eligible, c( 'Flexitarian', 'No Peanut/Tree Nut', 'No Dairy', 'No Gluten', 'Other'))
    }
    
    res2 <- data_school_c %>% filter(Q23_2=="Special dietary lifestyle for religious reasons"|Q23_3=="Special dietary lifestyle for medical reasons"|Q23_4=="Special dietary lifestyle for personal reasons")%>% 
      select(dplyr::starts_with("Q25")) %>% select(!dplyr::contains("Q25_18_TEXT")) %>%
      mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q25")) %>%
      filter(value!="") %>%
      filter(value %in% eligible) %>%
      group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n)) %>% 
      filter(value!='None of the above') %>% 
      arrange(value %in% c("Other (please specify):"))
    
    
    top_diet <- res2[ c(1), c('value')][[1]]
    
    #allergy or intolerance
    n_all <- nrow(data_school_c[data_school_c$Q23_1=="Food allergies or intolerances"|data_school_c$Q23_2=="Special dietary lifestyle for religious reasons"|data_school_c$Q23_3=="Special dietary lifestyle for medical reasons"|data_school_c$Q23_4=="Special dietary lifestyle for personal reasons",])
    p_all <- paste0(roundQual(n_all/nrow(data_school_c)*100,0),"%")
    #allergy
    n_allergy <- nrow(data_school_c[data_school_c$Q23_1=="Food allergies or intolerances",])
    p_allergy <- paste0(roundQual(n_allergy/nrow(data_school_c)*100,0),"%")
    #special diet
    n_diet <-nrow(data_school_c[data_school_c$Q23_2=="Special dietary lifestyle for religious reasons"|data_school_c$Q23_3=="Special dietary lifestyle for medical reasons"|data_school_c$Q23_4=="Special dietary lifestyle for personal reasons",])
    p_diet <- paste0(roundQual(n_diet/nrow(data_school_c)*100,0),"%")
    
    


    title9 <- paste0(p_all, ' of respondents have a food allergy or dietary requirement')
    title_9_par <- fpar(
      ftext(title9, fp_text( font.size = 22)))
    
    
    subtitle <-toupper(paste0(top_allergy, ' the most common allergy and ',tolower(top_diet)," is the most common diet."))
    sub_title_9_par <- fpar(ftext(subtitle, fp_text(font.size=10, color='red')))
    
    
    allergy_par = fpar(ftext(p_allergy,prop =  fp_text(font.size = 20, bold=T)))
    diet_par = fpar(ftext(p_diet,prop =  fp_text(font.size = 20, bold=T)))
    
    has_all <- data_school_c[data_school_c$Q23_1=="Food allergies or intolerances"|data_school_c$Q23_2=="Special dietary lifestyle for religious reasons"|data_school_c$Q23_3=="Special dietary lifestyle for medical reasons"|data_school_c$Q23_4=="Special dietary lifestyle for personal reasons",]
    #Among students with food allergies or special diets, 67% are satisfied with the ease of meeting their special dietary needs.
    
    pct_easy<- paste0(roundQual(nrow(has_all %>% subset(Q27 %in% c('Extremely easy', 'Somewhat easy')))/nrow(has_all)*100), '%')
    easy_par <- fpar(ftext("Among students with food allergies or special diets, " ,prop =  fp_text(font.size = 12)), 
                                       ftext(pct_easy,prop =  fp_text(font.size = 12, bold=T)),
                                       ftext(" are satisfied with the ease of meeting their special dietary needs.", prop =  fp_text(font.size = 12)),
                                       fp_p = fp_par(text.align="center"))

    
    s1 <-  paste0('Among Total Respondents (n=', UNIVERSITY_N, ')')
    s2 <- "Q23. Do you adhere to any dietary plans or restrictions?"
    s3 <- "Q24. Which of the following food allergies or intolerances do you have?"
    s4 <- "Q25. Which of the following special dietary requirements, for personal preference or religious reasons do you follow?"
    s5 <- "Q27. How easy is it to meet dietary needs?"

    
    my_pres <- my_pres %>% on_slide(index=9) %>%
      ph_with_fpars_at(fpars=list(title_9_par), 
                       left=.25,top=.2,height=.75,width=9.5 ) %>%
      ph_with_fpars_at(fpars=list(sub_title_9_par), 
                       left=.35,top=.55,height=.75,width=9.5 ) %>%
      ph_with_fpars_at(fpars=add_sample(s1, s2, s3, s4, s5), 
                       left=.25,top=5,height=.75,width=9)%>%
      ph_with_fpars_at(fpars=list(allergy_par), 
                       left=1.25,top=1.53,height=.75,width=1 ) %>%
      ph_with_fpars_at(fpars=list(diet_par), 
                       left=1.25,top=2.5,height=.75,width=1 ) %>%
      ph_with_fpars_at(fpars=list(easy_par), 
                       left=6.6,top=4.4,height=1,width=3 ) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide9a.png"), 100/72, 76/72),
              location = ph_location(top=1,left=2.9,width=7,height=1.7))%>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide9b.png"), 100/72, 76/72),
              location = ph_location(top=2.5,left=2.9,width=7,height=1.7))
    
    ##############################################################################
    ########################## Slide 10 #
    if (!ca){
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
    

    title10 <- paste0(res[c(1),]$value,', ', tolower(res[c(2),]$value),' and ', tolower(res[c(3),]$value),  ' are the most important sustainability and social initiatives to students.')
    title_10_par <- fpar(ftext(title10, fp_text( font.size = 22)))
    
    s1 <-  paste0('Among Total Respondents (n=', UNIVERSITY_N, ')')
    s2 <- "Q30. Which of the following sustainability and social initiatives, if any, are most important to you? Select up to 3 initiatives."
    
    
    my_pres <- my_pres %>% on_slide(index=10) %>%
      ph_with_fpars_at(fpars=list(title_10_par), 
                       left=.25,top=.2,height=.75,width=9.5 ) %>%
      ph_with_fpars_at(fpars=add_sample('', "", s1, s2), 
                       left=.25,top=4.7,height=.75,width=9) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide10.png"), 100/72, 76/72),
              location = ph_location(top=1.2,left=2.5,width=4,height=4.5))
    

    
    ##############################################################################
    ########################## Slide 11 # Mobile ordering
    res <- data.frame(table(data_school_c$Q39)) %>% subset(Var1!="")
    pct <- res[res$Var1=='Yes',]$Freq/sum(res$Freq)

    qual <- ifelse(pct>.5, 'Most ', '')
    qual <- ifelse(pct<.5 & pct>.3, 'A substantial minority of ', qual)
    qual <- ifelse(pct<.3 & pct>0, 'A few ', qual)
    qual <- ifelse(pct==0, 'No ', qual)
    
    oft <- data_school %>% filter(Q41 !="") %>% group_by(Q41) %>% summarize(n=n()) %>% 
      mutate(per=paste0(roundQual(100*n/sum(n)),"%")) %>%  complete(Q41=c("0 times / week",  "1-2 times / week", "3-5 times / week",
                                                                          "6-9 times / week", "10+ times / week"),fill=list(n=0,per="0%")) %>% 
      mutate(Q41 = factor(Q41,levels=c("0 times / week",  "1-2 times / week", "3-5 times / week",
                                       "6-9 times / week", "10+ times / week"))) %>%
      arrange(n)
    
    order_val <-  gsub('/', 'per', oft[ c(nrow(oft)), c('Q41')][[1]])
    order_pct <-oft[ c(nrow(oft)), c('per')][[1]]
    
    #Most students order using Mobile App or Kiosk, with 51% ordering 3-10+ times per week.
    
    title11 <- paste0(qual, "students order using Mobile App or Kiosk, with ", order_pct, ' ordering ', order_val, '.')
    title_11_par <- fpar(ftext(title11, fp_text( font.size = 22)))
    
    s1 <-  paste0('Among Answering Respondents (n=', sum(res$Freq), ')')
    s2 <- 'Q40. Do you ever order via the mobile app or self ordering kiosk?'
    s3 <- "Q45. Please select which technology you would like to see at your campus, if it doesn't already exist."
    s4 <- "See question text in notes section"

    
    
   
    my_pres <- my_pres %>% on_slide(index=11) %>%
      ph_with_fpars_at(fpars=list(title_11_par), 
                       left=.25,top=.2,height=.75,width=9.5 ) %>%
      ph_with_fpars_at(fpars=add_sample(s1, s2, s3, s4), 
                       left=.25,top=5.1,height=.75,width=9) %>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide11a.png"), 100/72, 76/72),
              location = ph_location(top=1.7,left=0,width=2.5,height=2.5))%>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide11b.png"), 100/72, 76/72),
              location = ph_location(top=1.65,left=3.15,width=2.5,height=1.7))%>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide11c.png"), 100/72, 76/72),
              location = ph_location(top=3.75,left=3.15,width=2.5,height=1.2))%>%
      ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide11d.png"), 100/72, 76/72),
              location = ph_location(top=1.6,left=6.5,width=3,height=3.5))
    
    
    
    
    
    print(my_pres, target = paste0("fallPPTS_2023/",UNIVERSITY_NAME,".pptx"))
    #find_me
    
    
    #
}

run_report_ca <- function(data,last_data,UNIVERSITY_NAME,last_year) {
  
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
  
  UNIVERSITY_N <- n_format(nrow(data_school_c)) # sample size for universty 
  STUDENT_N <- n_format(nrow(data_school)) 
  REGION_N <- n_format(nrow(data_region_c)) # sample size for this region
  NATIONAL_N <- n_format(nrow(data_c)) # sample size for entire study
  
  
  ################################################################################################
  # Read in template 
 
  my_pres <- read_pptx("template_canada.pptx") 
  
  
  
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
  
    bold_face <- update(shortcuts$fp_bold(font.size = 32),color="white")
    
    university_par <- fpar(
      ftext(UNIVERSITY_NAME , prop = bold_face),fp_p = fp_par(text.align="center") )
    
    my_pres <-  my_pres %>% on_slide(index=1) %>%
      ph_with_fpars_at(fpars=list(university_par), 
                       left=.6,top=0,height=1,width=8.5
      ) 
    

 
  
  ####### SLide 2
  ################################################################################################
    COMMUTE <- names(which(table(data_school$Q3)==max(table(data_school$Q3))))[1]  # which is most common
    ## get rid of extra text
    COMMUTE <- gsub("\\(in-person and online\\) ","",COMMUTE)
    
    type <- data_school %>% group_by(Q2) %>% summarize(n=n()) %>% arrange(desc(n))

    commute_text <- paste0("Most respondents are living ",tolower(COMMUTE)," and most are ",type$Q2[1],"s")
    
    title_p23 <- fpar(
      ftext(commute_text,
            fp_text( font.size = 21)))
    
    # add title 
    my_pres <- my_pres %>% on_slide(index=2) %>% 
      ph_with_fpars_at(fpars=list(title_p23), 
                       left=.25,top=.25,height=.75,width=9 )
    # Add graphs_fall_2022
    
    my_pres <- my_pres %>% on_slide(index=2) %>% 
      ph_with( external_img(paste0(graph_loc ,UNIVERSITY_NAME,"/","location_2a.png"), 100/72, 76/72),
               location = ph_location(top=1.5,left=.5,width=4,height=3.5), 
               use_loc_size = TRUE ) %>% 
      ph_with( external_img(paste0(graph_loc ,UNIVERSITY_NAME,"/","slide21c.png"), 100/72, 76/72),
               location = ph_location(top=1.6,left=5.9,width=4,height=4), 
               use_loc_size = TRUE )
    

    # add title 
    my_pres <- my_pres %>% on_slide(index=2) %>% 
      ph_with_fpars_at(fpars=add_sample("",paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),"), Among Students/Other (n=",format(nrow(data_school),big.mark = ","),")"),
                                        "Q2. Which of the following best describes you?","Q4. Where do you live?"), 
                       left=.25,top=4.8,height=.75,width=9 )
  
 
  ####### SLide 4
  ################################################################################################
  current <- top_two("Q5",data_school)

    driver_df  <- data_school%>% group_by(Q9) %>% summarize(n=n()) %>% arrange(desc(n))
    
    
    clean_value <- function(value){
      clean <- ifelse(value=='Value to me is about the quality of the food', 'quality', '')
      clean <- ifelse(value=='Value to me is about the overall experience', 'overall experience', clean)
      clean <- ifelse(value=='Value to me is about how much I pay', 'cost', clean)
      clean <- ifelse(value=='	Value to me is about how much food I receive', 'quantity', clean)
      return(clean)
    }
    
    

    title <- paste0("Overall Experience at ",UNIVERSITY_NAME, " was ",current,"% (Top 2 Box). The main driver of students' value satisfaction is ",clean_value(driver_df[c(1), c('Q9')][[1]]),".")

    s1 <- paste0('Among Total Respondents (n=', UNIVERSITY_N, ')')

  
  
  title_par <- fpar(
    ftext(title,
          fp_text( font.size = 18)))
  

  
  my_pres <- my_pres %>% on_slide(index=3) %>%
    ph_with_fpars_at(fpars=list(title_par), 
                     left=.25,top=.25,height=.75,width=9 )%>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide3a.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=.75,width=2.5,height=3)) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide3b.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=4.75,width=2.5,height=3))%>% 
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide3c.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=7.25,width=2.5,height=3))%>% 
    ph_with_fpars_at(fpars=add_sample("",paste0( "Among Total Respondents (n=",format(nrow(data_school_c),big.mark = ","),")"),
                                      "Q5. Overall, how would you rate your campus dining program?","Q7. How would you rate the value you receive when dining on campus?",
                                      "Q9. Which statement below best describes how you value a meal?"), 
                     left=.25,top=4.8,height=.75,width=9 )

  
    
    
    
  
  ########################## Slide 4 and 5 #
  s1 <-  paste0('Among Total Respondents (n=', UNIVERSITY_N, ')')
  s2 <- "Q10. Thinking about your overall campus dining program, how would you rate each of the following?"
  s3 <- "Q11. Still thinking about your overall campus dining program, how would you rate each of the following?"
  
  
  
  # Write title
  questions = c("Q10_1", "Q10_2", "Q10_3", "Q10_4", "Q10_5", "Q10_6", 
                "Q10_8", "Q10_9", "Q10_10", "Q10_11","Q11_1", "Q11_2", "Q11_3", 
                "Q11_4", "Q11_5", "Q11_6", "Q11_7", "Q11_8", "Q11_9")
  q_names = c("Food quality", "Food variety", "Availability of nutrition information", 
              "Availability of ingredient/allergen information", " Availability of healthy options", 
              "Price/Value", "Availability of special dietary options", 
              "Freshness of food", "Affordability", "Made from sustainably sourced products", 
              "Convenience", "Welcoming/Friendly staff", "Knowledgeable/Helpful staff", 
              "Speed of service", "Cleanliness", "Hours of operation", "Place to socialize", 
              "Comfortable dining experience", "Technology to support dining program")
  
  vals <- c()
  for (q in questions){
    vals = c(vals,top_two(q,data_school_c))
  }
  
  data_7 <- data.frame(cbind(q_names, vals,questions))
  data_7$vals <- as.numeric(as.character(data_7$vals))
  data_7$pct <- paste0(as.character(data_7$vals), '%')
  data_7 <- data_7 %>% arrange(desc(vals))
  
  data_7a <- data_7 %>% filter(grepl("Q10",questions))
  data_7b <- data_7 %>% filter(grepl("Q11",questions))



  title4 <- paste0("The highest rated performance metrics are ", tolower(data_7a[c(1), c('q_names')][[1]]),', ', tolower(data_7a[c(2), c('q_names')][[1]]),  ' and ',tolower(data_7a[c(3), c('q_names')][[1]]), '.')
  
  title5 <- paste0("The highest rated performance metrics are ", tolower(data_7b[c(1), c('q_names')][[1]]),', ', tolower(data_7b[c(2), c('q_names')][[1]]),  ' and ',tolower(data_7b[c(3), c('q_names')][[1]]), '.')
  
  title_par <- fpar(
    ftext(title4,
          fp_text( font.size = 21)))
  title_par5 <- fpar(
    ftext(title5,
          fp_text( font.size = 21)))
  
  

  my_pres <- my_pres %>% on_slide(index=4) %>%
    ph_with_fpars_at(fpars=list(title_par), 
                     left=.25,top=.25,height=.75,width=9 ) %>% 
    ph_with_fpars_at(fpars=add_sample('', "",s1,s2), 
                     left=.25,top=4.6,height=.75,width=9) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide4_q10.png"), 100/72, 76/72),
            location = ph_location(top=.8,left=.75,width=9,height=4))
  
  my_pres <- my_pres %>% on_slide(index=5) %>%
    ph_with_fpars_at(fpars=list(title_par5), 
                     left=.25,top=.25,height=.75,width=9 ) %>% 
    ph_with_fpars_at(fpars=add_sample('', "",s1,s3), 
                     left=.25,top=4.6,height=.75,width=9) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide4_q11.png"), 100/72, 76/72),
            location = ph_location(top=.8,left=.75,width=9,height=4))

  ########################## Slide 6
  
  title_par6 <- fpar(
    ftext("Relative importance of performance metrics.",
          fp_text( font.size = 21)))
  
  s2 <- "Q11A_CAN. How important is it for your ideal campus dining program to perform well at..."
  
  my_pres <- my_pres %>% on_slide(index=6) %>%
    ph_with_fpars_at(fpars=list(title_par6), 
                     left=.25,top=.2,height=.75,width=9.5 ) %>% 
    ph_with_fpars_at(fpars=add_sample('', '',s1,s2), 
                     left=.25,top=4.8,height=.75,width=9) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide5_ca.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=2.5,width=4.3,height=3.9))
  
  
  
  

  ########################## Slide 7
 
  s2 <- "Q12. Which of the following dining locations on-campus have you visited most frequently this semester?"
  s3<- "Q13. Please rate your satisfaction with the following dining locations"
  
  title_par7 <- fpar(
    ftext("Satisfaction with dining location by location type.",
          fp_text( font.size = 21)))
  
  my_pres <- my_pres %>% on_slide(index=7) %>%
    ph_with_fpars_at(fpars=list(title_par7), 
                     left=.25,top=.25,height=.75,width=9.5 ) %>% 
    ph_with_fpars_at(fpars=add_sample('', s1,s2,s3), 
                     left=.25,top=4.8,height=.75,width=9) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide6a_ca.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=.35,width=4,height=3.5))%>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide6b_ca.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=4.25,width=5,height=3.5))
 
  ## Slide 8 
  
  eligible = c('Milk', 'Eggs', 'Peanuts', 'Tree nuts', 'Fish', 'Shellfish', 'Soy', 
               'Wheat/Gluten', 'Sesame',  'Other (please specify):')
  if (ca){
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
  
  top_allergy <- res[ c(1), c('value')][[1]]
  top_allergy <- ifelse(substr(top_allergy, nchar(top_allergy), nchar(top_allergy))=='s', paste(top_allergy, 'are'), paste(top_allergy, 'is'))
  
  eligible = c('Vegetarian', 'Vegan', 'Halal', 'Kosher', 'Paleo/Primal', 'Ketogenic', 'Mediterranean', 'Other (please specify):')
  if (!ca){
    eligible = c(eligible, c('Atkins', 'GMO-Free', 'Whole 30', 'Plant-based', 'Other',"Low-FODMAP","Pescatarian"))
  } else {
    eligible = c(eligible, c( 'Flexitarian', 'No Peanut/Tree Nut', 'No Dairy', 'No Gluten', 'Other'))
  }
  
  res2 <- data_school_c %>% filter(Q23_2=="Special dietary lifestyle for religious reasons"|Q23_3=="Special dietary lifestyle for medical reasons"|Q23_4=="Special dietary lifestyle for personal reasons")%>% 
    select(dplyr::starts_with("Q25")) %>% select(!dplyr::contains("Q25_18_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q25")) %>%
    filter(value!="") %>%
    filter(value %in% eligible) %>%
    group_by(value) %>% summarize(n=100*n()/nrow(data_school_c))  %>% arrange(desc(n)) %>% 
    filter(value!='None of the above') %>% 
    arrange(value %in% c("Other (please specify):"))
  
  
  top_diet <- res2[ c(1), c('value')][[1]]
  
  #allergy or intolerance
  n_all <- nrow(data_school_c[data_school_c$Q23_1=="Food allergies or intolerances"|data_school_c$Q23_2=="Special dietary lifestyle for religious reasons"|data_school_c$Q23_3=="Special dietary lifestyle for medical reasons"|data_school_c$Q23_4=="Special dietary lifestyle for personal reasons",])
  p_all <- paste0(roundQual(n_all/nrow(data_school_c)*100,0),"%")
  #allergy
  n_allergy <- nrow(data_school_c[data_school_c$Q23_1=="Food allergies or intolerances",])
  p_allergy <- paste0(roundQual(n_allergy/nrow(data_school_c)*100,0),"%")
  #special diet
  n_diet <-nrow(data_school_c[data_school_c$Q23_2=="Special dietary lifestyle for religious reasons"|data_school_c$Q23_3=="Special dietary lifestyle for medical reasons"|data_school_c$Q23_4=="Special dietary lifestyle for personal reasons",])
  p_diet <- paste0(roundQual(n_diet/nrow(data_school_c)*100,0),"%")
  
  
  
  
  title9 <- paste0(p_all, ' of respondents have a food allergy or dietary requirement')
  title_9_par <- fpar(
    ftext(title9, fp_text( font.size = 22)))
  
  
  subtitle <-toupper(paste0(top_allergy, ' the most common allergy and ',tolower(top_diet)," is the most common diet."))
  sub_title_9_par <- fpar(ftext(subtitle, fp_text(font.size=10, color='red')))
  
  
  allergy_par = fpar(ftext(p_allergy,prop =  fp_text(font.size = 20, bold=T)))
  diet_par = fpar(ftext(p_diet,prop =  fp_text(font.size = 20, bold=T)))
  
  has_all <- data_school_c[data_school_c$Q23_1=="Food allergies or intolerances"|data_school_c$Q23_2=="Special dietary lifestyle for religious reasons"|data_school_c$Q23_3=="Special dietary lifestyle for medical reasons"|data_school_c$Q23_4=="Special dietary lifestyle for personal reasons",]
  #Among students with food allergies or special diets, 67% are satisfied with the ease of meeting their special dietary needs.
  
  pct_easy<- paste0(roundQual(nrow(has_all %>% subset(Q27 %in% c('Extremely easy', 'Somewhat easy')))/nrow(has_all)*100), '%')
  easy_par <- fpar(ftext("Among students with food allergies or special diets, " ,prop =  fp_text(font.size = 12)), 
                   ftext(pct_easy,prop =  fp_text(font.size = 12, bold=T)),
                   ftext(" are satisfied with the ease of meeting their special dietary needs.", prop =  fp_text(font.size = 12)),
                   fp_p = fp_par(text.align="center"))
  
  
  s1 <-  paste0('Among Total Respondents (n=', UNIVERSITY_N, ')')
  s2 <- "Q23. Do you adhere to any dietary plans or restrictions?"
  s3 <- "Q24. Which of the following food allergies or intolerances do you have?"
  s4 <- "Q25. Which of the following special dietary requirements, for personal preference or religious reasons do you follow?"
  s5 <- "Q27. How easy is it to meet dietary needs?"
  
  
  my_pres <- my_pres %>% on_slide(index=9) %>%
    ph_with_fpars_at(fpars=list(title_9_par), 
                     left=.25,top=.2,height=.75,width=9.5 ) %>%
    ph_with_fpars_at(fpars=list(sub_title_9_par), 
                     left=.35,top=.55,height=.75,width=9.5 ) %>%
    ph_with_fpars_at(fpars=add_sample(s1, s2, s3, s4, s5), 
                     left=.25,top=5,height=.75,width=9)%>%
    ph_with_fpars_at(fpars=list(allergy_par), 
                     left=1.25,top=1.53,height=.75,width=1 ) %>%
    ph_with_fpars_at(fpars=list(diet_par), 
                     left=1.25,top=2.5,height=.75,width=1 ) %>%
    ph_with_fpars_at(fpars=list(easy_par), 
                     left=6.6,top=4.4,height=1,width=3 ) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide9a.png"), 100/72, 76/72),
            location = ph_location(top=1,left=2.9,width=7,height=1.7))%>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide9b.png"), 100/72, 76/72),
            location = ph_location(top=2.5,left=2.9,width=7,height=1.7))
  
  
  
  
  
  
   ####################################################
  ########################## Slide 6 old #
  s1 <-  paste0('Among Student Respondents (n=', STUDENT_N,',', nrow(data_school_last) , ')')
  s2 <- "Q7. How would you rate the value you receive when dining on campus?"
  s3 <- "Q9. Which statement below best describes how you value a meal?"
  
  driver_df  <- data_school%>% group_by(Q9) %>% summarize(n=n()) %>% arrange(desc(n))
  
  
  clean_value <- function(value){
    clean <- ifelse(value=='Value to me is about the quality of the food', 'quality', '')
    clean <- ifelse(value=='Value to me is about the overall experience', 'overall experience', clean)
    clean <- ifelse(value=='Value to me is about how much I pay', 'cost', clean)
    clean <- ifelse(value=='	Value to me is about how much food I receive', 'quantity', clean)
    return(clean)
  }
  
  
  title6 <- paste0("Main drivers of value rating are ", clean_value(driver_df[c(1), c('Q9')][[1]]), ' and ',clean_value(driver_df[c(2), c('Q9')][[1]]) , '.')
  
  
  
  
  title_6_par <- fpar(
    ftext(title6, fp_text( font.size = 22)))
  
  
  my_pres <- my_pres %>% on_slide(index=6) %>%
    ph_with_fpars_at(fpars=list(title_6_par), 
                     left=.25,top=.25,height=.75,width=9.5 ) %>%
    ph_with_fpars_at(fpars=add_sample('', s1, s2, s3), 
                     left=.25,top=4.75,height=.75,width=9) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide6a.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=.5,width=3.5,height=3.5)) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide6b.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=5,width=3.5,height=3.5))
  
  ####################################################
  ########################## Slide 7 #
  questions = c("Q10_1", "Q10_2", "Q10_3", "Q10_4", "Q10_5", "Q10_6", 
                "Q10_7", "Q10_9", "Q10_10", "Q10_11", "Q11_1", "Q11_2", "Q11_3", 
                "Q11_4", "Q11_5", "Q11_6", "Q11_7", "Q11_8", "Q11_9")
  q_names = c("Food quality", "Food variety", "Availability of nutrition information", 
              "Availability of ingredient/allergen information", " Availability of healthy options", 
              "Price/Value", "Availability of special dietary options", 
              "Freshness of food", "Affordability", "Made from sustainably sourced products", 
              "Convenience", "Welcoming/Friendly staff", "Knowledgeable/Helpful staff", 
              "Speed of service", "Cleanliness", "Hours of operation", "Place to socialize", 
              "Comfortable dining experience", "Technology to support dining program")
  
  vals <- c()
  for (q in questions){
    vals = c(vals,top_two(q,data_school_c))
  }
  
  data_7 <- data.frame(cbind(q_names, vals))
  data_7$vals <- as.numeric(data_7$vals)
  data_7 <- data_7 %>% arrange(desc(vals))
  data_7$pct <- paste0(as.character(data_7$vals), '%')
  
  
  
  
  title7 <- paste0("The highest rated performance metrics are ", tolower(data_7[c(1), c('q_names')][[1]]),', ', tolower(data_7[c(2), c('q_names')][[1]]),  ' and ',tolower(data_7[c(3), c('q_names')][[1]]), '.')
  
  
  if (ca==F){
    questions2 <- c('Q11A_USA_1', 'Q11A_USA_2','Q11A_USA_3','Q11A_USA_4','Q11A_USA_5','Q11A_USA_6','Q11A_USA_7','Q11A_USA_8','Q11A_USA_9')
  } else{
    #TODO check CA questionnames
    questions2 <- c('Q11A_CA_1', 'Q11A_CA_2','Q11A_CA_3','Q11A_CA_4','Q11A_CA_5','Q11A_CA_6','Q11A_CA_7','Q11A_CA_8','Q11A_CA_9')
    
  }
  
  q_names_2 <- c('Availability of special dietary options', 'Cleanliness','Comfortable dining experience','Convenience','Food quality', 'Food variety', 'Knowledgeable/Helpful staff', 'Price/Value','Welcoming/Friendly staff')
  
  vals <- c()
  for (q in questions2){
    vals = c(vals,top_two(q,data_school_c))
  }
  
  data_7_2 <- data.frame(cbind(q_names_2, vals))
  data_7_2$vals <- as.numeric(data_7_2$vals)
  data_7_2 <- data_7_2 %>% arrange(desc(vals))
  data_7_2$pct <- paste0(as.character(data_7_2$vals), '%')
  
  top1 <- fpar(ftext(data_7_2[c(1), c('q_names_2')][[1]], fp_text( font.size = 12)))
  top2 <- fpar(ftext(data_7_2[c(2), c('q_names_2')][[1]], fp_text( font.size = 12)))
  top3 <- fpar(ftext(data_7_2[c(3), c('q_names_2')][[1]], fp_text( font.size = 12)))
  
  #cross reference for opportunity
  #keep only the ones in Q11a
  merged_7 = merge(data_7, data_7_2, by.x='q_names', by.y='q_names_2',all=T)
  #important - quality
  merged_7 <- merged_7 %>% mutate(dif = vals.y - vals.x )  %>% arrange(desc(dif))
  
  opp1 <- fpar(ftext(merged_7[c(1), c('q_names')][[1]], fp_text( font.size = 12)))
  opp2 <- fpar(ftext(merged_7[c(2), c('q_names')][[1]], fp_text( font.size = 12)))
  opp3 <- fpar(ftext(merged_7[c(3), c('q_names')][[1]], fp_text( font.size = 12)))
  
  
  title_7_par <- fpar(
    ftext(title7, fp_text( font.size = 22)))
  
  s1 <- paste0('Among Total Respondents (n=', UNIVERSITY_N, ')')
  s2 <- "Q10. Thinking about your campus dining program, how would you rate each of the following?"
  
  
  
  
  my_pres <- my_pres %>% on_slide(index=7) %>%
    ph_with_fpars_at(fpars=list(title_7_par), 
                     left=.25,top=.25,height=.75,width=9.5 ) %>%
    ph_with_fpars_at(fpars=add_sample('', "", s1, s2), 
                     left=.25,top=4.6,height=.75,width=9) %>%
    ph_with_fpars_at(fpars=list(top1), 
                     left=.5,top=1.83,height=.75,width=2.5 )%>%
    ph_with_fpars_at(fpars=list(top2), 
                     left=.5,top=2.14,height=.75,width=2.5 )%>%
    ph_with_fpars_at(fpars=list(top3), 
                     left=.5,top=2.45,height=.75,width=2.5 )%>%
    ph_with_fpars_at(fpars=list(opp1), 
                     left=.6,top=3.75,height=.75,width=2.5 )%>%
    ph_with_fpars_at(fpars=list(opp2), 
                     left=.6,top=3.95,height=.75,width=2.5 ) %>%
    ph_with_fpars_at(fpars=list(opp3), 
                     left=.6,top=4.15,height=.75,width=2.5 ) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide7.png"), 100/72, 76/72),
            location = ph_location(top=1.3,left=4.5,width=4.5,height=4)) 
  
  #################################################### 
  ########################## Slide 8 : Meal plan participation
  
  n_part <- nrow(data_school_c[data_school_c$Q15=="I participate in the meal plan although it is not required",])
  pct <- roundQual(n_part/nrow(data_school_c)*100)
  pct_nat <- roundQual(nrow(data_c[data_c$Q15=="I participate in the meal plan although it is not required",])/nrow(data_c)*100)
  
  
  qualifier <- ifelse(abs(pct - pct_nat) < 5, 'about the same as', ' ')
  qualifier <- ifelse(pct-pct_nat >= 5, 'higher than', qualifier)
  qualifier <- ifelse(pct-pct_nat > 10, 'much higher than', qualifier)
  qualifier <- ifelse(pct-pct_nat <= -5, 'lower than', qualifier)
  qualifier <- ifelse(pct-pct_nat < -10, 'much lower than', qualifier)
  
  
  title8 <- paste0(pct, '% of students take part in the meal plan even though participation is not required; ',  qualifier, ' the national average.')
  title_8_par <- fpar(
    ftext(title8, fp_text( font.size = 22)))
  
  
  if (ca==F){
    base_data <- data_school_c%>% filter(Q4 %in% c("Yes")) %>% filter(Q19!="Graduating")
  } else {
    base_data <- data_school_c%>% filter(Q4ca %in% c("Yes")) %>% filter(Q19!="Graduating")
  }
  
  
  base <- nrow(base_data) 
  
  res_s <- base_data %>%
    select(dplyr::starts_with("Q20")) %>% select(!dplyr::contains("Q20_9_TEXT")) %>%
    mutate(id=row.names(.)) %>% pivot_longer(cols=dplyr::starts_with("Q20")) %>%
    filter(value!="") %>%
    mutate(value= gsub(" \\(please specify)","",value)) %>% 
    #mutate(value = gsub("meal plan","dining plan",value)) %>% 
    #mutate(value = gsub("Meal plan","Dining plan",value)) %>% 
    group_by(value) %>% summarize(n=100*n()/base)  %>% arrange(desc(n))
  
  subtitle <-toupper(paste0(res_s[c(1),]$value, ', ',res_s[c(2),]$value, ', and ', res_s[c(3),]$value," could increase dining program participation."))
  sub_title_8_par <- fpar(ftext(subtitle, fp_text(font.size=10, color='red')))
  
  eligible  <- data_school_c%>% filter(Q4 %in% c("Yes")) %>% filter(Q19!="Graduating")
  
  #find_me
  
  s1 <-  paste0('Among Total Respondents/ Who Participate Even Though It Is Not Required (n=', UNIVERSITY_N, ',',n_part,'), Who Plan to be On Campus Next Year (n=', nrow(eligible), ')')
  s2 <- "Q15. Which of the following best describes your current participation in your school's meal plan?"
  s3 <- "Q16. What research or information did you receive that led you to purchase a meal plan?"
  s4 <- "Q20. Which of these, if any, would make you more likely to purchase a meal plan in the future?"
  
  
  my_pres <- my_pres %>% on_slide(index=8) %>%
    ph_with_fpars_at(fpars=list(title_8_par), 
                     left=.25,top=.2,height=.75,width=9.5 ) %>%
    ph_with_fpars_at(fpars=list(sub_title_8_par), 
                     left=.35,top=.95,height=.75,width=9.5 ) %>%
    ph_with_fpars_at(fpars=add_sample(s1, s2, s3, s4), 
                     left=.25,top=5,height=.75,width=9) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide8a.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=.4,width=3,height=3)) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide8b.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=3.6,width=3,height=3))  %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide8c.png"), 100/72, 76/72),
            location = ph_location(top=1.5,left=6.6,width=3,height=3.85))
  
  ####################################################
  ########################## Slide 9 #
  #Totals different than slide 3 because this is ALL RESPONDENTS and slide 3 is students
  
   
  ##############################################################################
  ########################## Slide 10 #
  if (!ca){
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
  
  
  title10 <- paste0(res[c(1),]$value,', ', tolower(res[c(2),]$value),' and ', tolower(res[c(3),]$value),  ' are the most important sustainability and social initiatives to students.')
  title_10_par <- fpar(ftext(title10, fp_text( font.size = 22)))
  
  s1 <-  paste0('Among Total Respondents (n=', UNIVERSITY_N, ')')
  s2 <- "Q30. Which of the following sustainability and social initiatives, if any, are most important to you? Select up to 3 initiatives."
  
  
  my_pres <- my_pres %>% on_slide(index=10) %>%
    ph_with_fpars_at(fpars=list(title_10_par), 
                     left=.25,top=.2,height=.75,width=9.5 ) %>%
    ph_with_fpars_at(fpars=add_sample('', "", s1, s2), 
                     left=.25,top=4.7,height=.75,width=9) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide10.png"), 100/72, 76/72),
            location = ph_location(top=1.2,left=2.5,width=4,height=4.5))
  
  
  
  ##############################################################################
  ########################## Slide 11 # Mobile ordering
  res <- data.frame(table(data_school_c$Q39)) %>% subset(Var1!="")
  pct <- res[res$Var1=='Yes',]$Freq/sum(res$Freq)
  
  qual <- ifelse(pct>.5, 'Most ', '')
  qual <- ifelse(pct<.5 & pct>.3, 'A substantial minority of ', qual)
  qual <- ifelse(pct<.3 & pct>0, 'A few ', qual)
  qual <- ifelse(pct==0, 'No ', qual)
  
  oft <- data_school %>% filter(Q41 !="") %>% group_by(Q41) %>% summarize(n=n()) %>% 
    mutate(per=paste0(roundQual(100*n/sum(n)),"%")) %>%  complete(Q41=c("0 times / week",  "1-2 times / week", "3-5 times / week",
                                                                        "6-9 times / week", "10+ times / week"),fill=list(n=0,per="0%")) %>% 
    mutate(Q41 = factor(Q41,levels=c("0 times / week",  "1-2 times / week", "3-5 times / week",
                                     "6-9 times / week", "10+ times / week"))) %>%
    arrange(n)
  
  order_val <-  gsub('/', 'per', oft[ c(nrow(oft)), c('Q41')][[1]])
  order_pct <-oft[ c(nrow(oft)), c('per')][[1]]
  
  #Most students order using Mobile App or Kiosk, with 51% ordering 3-10+ times per week.
  
  title11 <- paste0(qual, "students order using Mobile App or Kiosk, with ", order_pct, ' ordering ', order_val, '.')
  title_11_par <- fpar(ftext(title11, fp_text( font.size = 22)))
  
  s1 <-  paste0('Among Answering Respondents (n=', sum(res$Freq), ')')
  s2 <- 'Q40. Do you ever order via the mobile app or self ordering kiosk?'
  s3 <- "Q45. Please select which technology you would like to see at your campus, if it doesn't already exist."
  s4 <- "See question text in notes section"
  
  
  
  
  my_pres <- my_pres %>% on_slide(index=11) %>%
    ph_with_fpars_at(fpars=list(title_11_par), 
                     left=.25,top=.2,height=.75,width=9.5 ) %>%
    ph_with_fpars_at(fpars=add_sample(s1, s2, s3, s4), 
                     left=.25,top=5.1,height=.75,width=9) %>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide11a.png"), 100/72, 76/72),
            location = ph_location(top=1.7,left=0,width=2.5,height=2.5))%>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide11b.png"), 100/72, 76/72),
            location = ph_location(top=1.65,left=3.15,width=2.5,height=1.7))%>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide11c.png"), 100/72, 76/72),
            location = ph_location(top=3.75,left=3.15,width=2.5,height=1.2))%>%
    ph_with(external_img(paste0(graph_loc,UNIVERSITY_NAME,"/","slide11d.png"), 100/72, 76/72),
            location = ph_location(top=1.6,left=6.5,width=3,height=3.5))
  
  
  
  
  print(my_pres, target = paste0("fallPPTS_2023/",UNIVERSITY_NAME,".pptx"))
  #find_me
  
  
  #
}

run_report(data, data_last, 'University of Virginia', last_year='Spring 2023')



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



