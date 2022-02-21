########################################
#---sbs enrolments surveysurveys--------------#
#---Matt Navarro 19/02/2022------------#
########################################


#SETUP---------------

#LIBRARIES
library(tidyverse)
library(dplyr)
library(ggplot2)
library(magrittr)
library(cowplot)
library(RColorBrewer)
library(sf)
library(ggsflabel) #installed from source on github - not in CRAN
library(ggsn)
library(gtable)
library(gridExtra)
library(eoffice)

#DEFAULT FIGURE SIZES
ax.title<-14
ax.text<-9
ax.legend<-12
strip<-9

a4_width<- 160

my_theme<-theme_classic()+ theme( axis.text=element_text(size=ax.text),
                                  text = element_text(size=ax.text),
                                  axis.title=element_text(size=ax.title),
                                  line = element_line(size = 0.3),
                                  strip.background = element_rect(colour = "white", fill = "gray80"),
                                  strip.text = element_text(size=strip))

#DATA + DIRECTORIES
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own
data_dir <- paste(working.dir, "data", sep="/")
fig_dir <- paste(working.dir, "figures", sep="/")

setwd(data_dir)
d <- read.csv('Enrolment - Masters of Marine Science_February 18, 2022_23.36.csv', na.strings=c("","NA")) #, col.names = headers) #skipping the first row

d<-d[-c(1,2), ]

source(paste0(working.dir, "/Plotting Functions.R"))

#DATA CLEANING AND FORMATTING
d %<>% filter(Status=="IP Address") %>%
  filter(Finished=="True")

#Plots
setwd(fig_dir)
#Which University
propSummariesCategories(d$Q5, ytitle = "Perc", xtitle = "Where did you do your undergraduate studies?")
topptx(file = "plots.pptx", width = 6, height = 5)

#Which undergraduate course?

propSummariesCategories(d$Q11, ytitle = "Perc", xtitle = "Which of the following best matches your undergraduate course?")
topptx(file = "plots.pptx", width = 6, height = 5, append = T)

#Where live?
propSummariesCategories(d$Q23, ytitle = "Perc", xtitle = "When you were selecting a university for your masters studies did you")
topptx(file = "plots.pptx", width = 6, height = 5, append = T)

#Consider other Unis?
propSummariesCatsMultipleSelections(data = d[,c("Q16_1","Q16_2","Q16_3", "Q16_4")],
                                    xlabels = c("Investigate other\n unis in Perth","Investigate other\n unis interstate",
                                                "Investigate other\n unis internationally","Only UWA"),
                                    xtitle = "When you were selecting a university for your masters studies did you?", 
                                    ci=F, ytitle="Perc")
topptx(file = "plots.pptx", width = 6, height = 5, append = T)

#Family in Perth?
propSummariesCategories(d$Q12, ytitle = "Perc", xtitle = "Do you have immediate family members that live in Perth?")
topptx(file = "plots.pptx", width = 6, height = 5, append = T)

#Work after undergraduate?
propSummariesCategories(d$Q17, ytitle = "Perc", xtitle = "After completing your undergraduate studies did you")
topptx(file = "plots.pptx", width = 6, height = 5, append = T)

#Interest in PhD
propSummariesCategories(d$Q19, ytitle = "Perc", xtitle = "At this stage how interested are you in pursuing a PhD in marine biology or marine science?")
topptx(file = "plots.pptx", width = 6, height = 5, append = T)

#Type of masters
propSummariesCategories(d$Q22, ytitle = "Perc", xtitle = "Which of the following matches your situation")
topptx(file = "plots.pptx", width = 6, height = 5, append = T)

#Attracted to Masters of Marine Biology
scale<-c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")

d %<>% mutate_at(vars(contains("Q1_")), funs(factor(.,levels = scale)))

  propSummariesLikert(d$Q1_1, xtitle = "The masters provides a pathway to a PhD", LegendName = "Agreeance", title = "I was attracted to studying a Masters of Marine Biology\n(not necessarily at UWA) because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q1_2, xtitle = "The masters will increase my job\nprospects in the marine\nbiology/science field", LegendName = "Agreeance", title = "I was attracted to studying a Masters of Marine Biology\n(not necessarily at UWA) because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q1_3, xtitle = "I am passionate about the marine\nenvironment and simply want to learn more about it", LegendName = "Agreeance", title = "I was attracted to studying a Masters of Marine Biology\n(not necessarily at UWA) because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q1_4, xtitle = "There weren't jobs available at\nthe undergraduate level, so I\ndecided to continue my studies", LegendName = "Agreeance", title = "I was attracted to studying a Masters of Marine Biology\n(not necessarily at UWA) because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q1_5, xtitle = "There were jobs available at the\nundergraduate level, but I prefer\nto increase my job prospects for\nhigher-level employment", LegendName = "Agreeance", title = "I was attracted to studying a Masters of Marine Biology\n(not necessarily at UWA) because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q1_6, xtitle = "I was excited to learn more skills\nin marine biology", LegendName = "Agreeance", title = "I was attracted to studying a Masters of Marine Biology\n(not necessarily at UWA) because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q1_7, xtitle = "I was excited by the prospects of\ndoing more fieldwork", LegendName = "Agreeance", title = "I was attracted to studying a Masters of Marine Biology\n(not necessarily at UWA) because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  
#De-tracted to Masters of Marine Biology
  d %<>% mutate_at(vars(contains("Q20_")), funs(factor(.,levels = scale)))
  propSummariesLikert(d$Q20_1, xtitle = "I was unsure if I would be able to\nget a job after completing\nthe Masters of Marine Biology", LegendName = "Agreeance", title = "I was deterred from studying a Masters of Marine\nBiology (not necessarily at UWA) because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q20_2, xtitle = "The cost of the courses were high", LegendName = "Agreeance", title = "I was deterred from studying a Masters of Marine\nBiology (not necessarily at UWA) because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q20_3, xtitle = "I wasn't sure how I would meet\nmy living costs whilst studying", LegendName = "Agreeance", title = "I was deterred from studying a Masters of Marine\nBiology (not necessarily at UWA) because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q20_4, xtitle = "There were other fields that\ninterested me", LegendName = "Agreeance", title = "I was deterred from studying a Masters of\nMarine Biology (not necessarily at UWA) because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  
#Attracted to UWA
  
  d %<>% mutate_at(vars(contains("Q8_")), funs(factor(.,levels = scale)))

  propSummariesLikert(d$Q8_1, xtitle = "I was an undergraduate at UWA and\nit was convenient", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_2, xtitle = "It was easy for me to find research\nprojects and supervisors", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_3, xtitle = "UWA overall has a good reputation", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_4, xtitle = "The Masters of Marine Biology at\nUWA has a good reputation", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_5, xtitle = "The lecturers in marine biology at\nUWA have a good reputation", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_6, xtitle = "The course descriptions included\nfieldwork which was exciting", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_7, xtitle = "The course costs weren't too high", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_8, xtitle = "I wanted to study in WA as it is\na marine biodiversity hotspot", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_9, xtitle = "I wanted to study in WA for\nother reasons", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_10, xtitle = "The course description was exciting\nas it included a lot of skills that\nmight help me get a job", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_11, xtitle = "I lived close to campus when choosing\nmy university, and UWA was convenient", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_12, xtitle = "The UWA campus seemed like a\nnice place to study", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q8_13, xtitle = "I have family/friends who have studied\nat UWA in the past and\nrecommended it", LegendName = "Agreeance", title = "I was attracted to UWA\n(as opposed to another university) for\nmy Masters of Marine Biology because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  
  
#Detracted from UWA
  d %<>% mutate_at(vars(contains("Q21_")), funs(factor(.,levels = scale)))
  
  propSummariesLikert(d$Q21_1, xtitle = "The course costs were high compared to other universities", LegendName = "Agreeance", title = "I was deterred from selecting\nUWA for my Masters of Marine\nBiology studies because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q21_2, xtitle = "The course description didn't seem to include a lot of fieldwork", LegendName = "Agreeance", title = "I was deterred from selecting\nUWA for my Masters of Marine\nBiology studies because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q21_3, xtitle = "The course description didn't seem to include job ready skills", LegendName = "Agreeance", title = "I was deterred from selecting\nUWA for my Masters of Marine\nBiology studies because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q21_4, xtitle = "Applying for the course was complicated", LegendName = "Agreeance", title = "I was deterred from selecting\nUWA for my Masters of Marine\nBiology studies because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q21_5, xtitle = "The course description was confusing", LegendName = "Agreeance", title = "I was deterred from selecting\nUWA for my Masters of Marine\nBiology studies because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q21_6, xtitle = "I had to move to Perth to study the Masters of Marine Biology", LegendName = "Agreeance", title = "I was deterred from selecting\nUWA for my Masters of Marine\nBiology studies because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)
  propSummariesLikert(d$Q21_7, xtitle = "There was no online study option for the course", LegendName = "Agreeance", title = "I was deterred from selecting\nUWA for my Masters of Marine\nBiology studies because")
  topptx(file = "plots.pptx", width = 6, height = 5, append = T)




