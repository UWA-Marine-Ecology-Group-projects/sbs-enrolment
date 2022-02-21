########################################
#---functions for General public analysis---#
#---Matt Navarro 05/02/2020------------#
########################################




getPalette2<-function (n) 
{
  x <- ramp(seq.int(0, 1, length.out = n))
  if (ncol(x) == 4L) 
    rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
  else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
}


Plot_Multi<-function(colsOfInterest,xlabels,respInterested, xtitle, legTitle){
  #Function to plot select response levels for multiple questions for a series of sub-topics. Fit one per major topic. 
  
  #ARGUMETNS:
  #colsOfInterest is the columns from the dataframe you want to include - i.e., the subtopics
  #xlabels is how you want to label the subtopics on the x axis
  #xtitle: X axis title - make to match the major topic e.g., marine tourism
  
  correspondence = tibble(colsOfInterest = colsOfInterest, xlabels = xlabels) # we use this to match sub-topic labels to the data
  
  
  d %>% select(c("ResponseId", colsOfInterest)) %>% pivot_longer(!ResponseId, names_to = "subTopic", values_to = "concern") %>% 
    group_by(subTopic, concern) %>% summarise(n = n()) %>% 
    filter(!concern %in% c(NA, "-99")) %>% #Don't count NA's (people who didn't see question) or -99 (people who didn't respond) in % calcs.
    mutate(perc = 100*(n / sum(n))) %>% 
    filter(concern %in% respInterested) %>% 
    mutate(concern = factor(concern, levels = respInterested)) %>%
    group_by(subTopic ) %>% mutate(perc_order = sum(perc)) %>%
    left_join(.,correspondence, by = c("subTopic" = "colsOfInterest")) %>%
    ggplot(.,aes(x= reorder(xlabels, -perc), y = perc, fill = concern)) + geom_bar(stat="identity",  colour = "black", size = 0.3 , position = "stack") +
    ylim(0,100) +
    xlab(xtitle) + ylab("Percentage (%)") + 
    scale_fill_manual(values = c("#74CAEB", "#051C31"), name = legTitle) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    my_theme +
    theme(plot.margin = unit(c(1,0,0,0), "cm")) +
    theme(plot.title=element_text(face="bold", size=10))
  
}

variableOfInterest=d$Q1_1
propSummariesLikert<-function(variableOfInterest, xtitle, LegendName, title){
    temp<-tibble(
      variableOfInterest = variableOfInterest)
    
    temp %<>% filter(!is.na(variableOfInterest)) %>% 
      group_by(variableOfInterest, .drop = F) %>%  
      summarise(n = n()) %>% 
      mutate(tot = sum(n)) 
    
    temp %<>% mutate(perc = 100*(n/tot))
    
    res <- Map(prop.test,temp$n,temp$tot)
    
    temp[c("lower","upper")] <- t(sapply(res,"[[", "conf.int"))*100
    
    plot_byNetwork<-temp  %>%
      ggplot(.,aes(x= variableOfInterest, y = perc)) + geom_bar(aes(fill = variableOfInterest),stat="identity",  colour = "black", size = 0.3 , position = position_dodge(width=0.2)) +
      #geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
      scale_fill_brewer(palette="RdYlGn",direction=1, drop=FALSE, name = LegendName,
                        labels = function(variableOfInterest) str_wrap(variableOfInterest, width = 8)) +
      scale_x_discrete( labels = function(x) str_wrap(x, width = 10)) +
      ylim(0,100) +
      ggtitle(title) +
      xlab(xtitle) +
      ylab("Percentage (%)") + 
      my_theme +
      geom_text(aes( label=paste0(round(perc), "%")), vjust=-0.2, hjust = 1, size = 3)+
      theme(plot.margin = unit(c(1,0,0,0), "cm")) +
      theme(plot.title=element_text(face="bold", size=10))+
      theme(legend.position = "none")
    
    output<-list(plot_byNetwork, temp)
    
  return(output)
}


propSummariesCatsMultipleSelections<-function(data, group, fill, xlabels, xtitle,ytitle, ci){
  
  #Use this for variables with multiple selection options per respondent (e.g. a charter operator who is an owner and captain)
  #ci = T if you want CI's otherwise ci = F
  # data should be in 0,1 format with multiple data columns selected
  
    
    #gather up your data
    temp<-cbind(data)
    temp <- as.data.frame(temp)
    colNames<-colnames(data)

    
    #calculate percentages
    temp <- temp %>%  
      mutate(across(Q16_1:Q16_4, ~ifelse(is.na(.)==F, 1, 0))) %>%
      tidyr::gather(activity, occurance, colNames ) %>% #this just stacks them all (0s and 1s)
      group_by(activity) %>%  
      summarise(tot = n(), n = sum(occurance)) %>%
      ungroup() %>%
      mutate(perc = 100*(n/tot))
    
    #if you want CI's estimate them
    if(ci==T) {
      
      res <- Map(prop.test,temp$n,temp$tot)
      
      temp[c("lower","upper")] <- t(sapply(res,"[[", "conf.int"))*100
    }
    temp <- temp %>% 
      mutate(activity = fct_relevel(activity,colNames))
    
    #make plot
    plot_byNetwork<-temp %>% ungroup() %>% 
      ggplot(.,aes(x= activity, y = perc)) + geom_bar(stat="identity",  colour = "black", fill = "grey", size = 0.3, position = position_dodge()) +
      scale_x_discrete(drop=FALSE, labels = xlabels) +
      scale_fill_manual(name = "Network", values = cols, drop = FALSE)  +
      ylim(0,100) +
      xlab(xtitle) + ylab(ytitle) + my_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      theme(legend.position="none")+
      geom_text(aes( label=paste0(round(perc), "%")), vjust=-0.2, hjust = 1, size = 3)
    #Add CI's if you want them
    if(ci==T) {plot_byNetwork<-plot_byNetwork+geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)}
    if(max(temp$perc) >90) {plot_byNetwork<- plot_byNetwork+ scale_y_continuous(breaks = seq(0,125,25), limits = c(0,120))}
    
    output<-list(plot_byNetwork, temp)
  
  
  return(output)
}


# 
# propSummariesBin<-function(variableOfInterest, group,fill,  xtitle, ytitle, ci, rem = c()){
#   
#   if(is.null(group)){#NOT GROUPING VERSION
#     
#     data<-tibble(variableOfInterest = variableOfInterest)
#     data%<>% filter(!is.na(variableOfInterest))
#     temp<- data %>% 
#     mutate(variableOfInterest = factor(variableOfInterest, levels=c(0,1))) %>% 
#     group_by(variableOfInterest, .drop = F) %>% 
#     summarise(num = n()) %>%
#     mutate(tot =  sum(num)) %>% 
#     filter(variableOfInterest==1) %>%
#     mutate(perc = 100*(num/tot))%>%
#     ungroup()
#   
#   res <- Map(prop.test,temp$num,temp$tot)
#   
#   temp[c("lower","upper")] <- t(sapply(res,"[[", "conf.int"))*100
#   
#   plot<-temp %>% 
#     # ungroup(group) %>%
#     ggplot(.) + 
#     geom_bar(aes(x =variableOfInterest, y = perc),  stat="identity",  colour = "black", fill = "grey",size = 0.3) +
#     scale_fill_manual(name = "Network", values = cols, drop = FALSE)  +
#     scale_x_discrete(labels = "National") +
#     ylim(0,100) +
#     xlab(xtitle) + ylab(ytitle) + my_theme +
#     geom_text(aes(x =variableOfInterest, y = perc, label=paste0(round(perc), "%")), vjust=-0.2, hjust = 1, size = 2.5)
#   
#   if(ci==T){ plot<-plot+ geom_errorbar(aes(ymin = lower, ymax = upper, x = variableOfInterest), width = .2) }
#   if(max(temp$perc) >85) {plot<- plot+ scale_y_continuous(breaks = seq(0,125,25), limits = c(0,110))}
#   if(max(temp$perc) >90) {plot<- plot+ scale_y_continuous(breaks = seq(0,125,25), limits = c(0,117))}
#   
#   output<-list(plot, temp)
#   }
#   
#   else{ #GROUPING VERSION
#   data<-tibble(
#     variableOfInterest = variableOfInterest,
#     group = group,
#     fill = fill)
#   data %<>% filter(!is.na(variableOfInterest))
#   Chi<-chisq.test(data$variableOfInterest, data$group )
# 
#   temp<-data %>%
#     mutate(variableOfInterest = factor(variableOfInterest, levels=c(0,1))) %>% 
#     group_by(group, fill, variableOfInterest, .drop = F) %>%  
#     summarise(num = n()) %>%
#     mutate(tot =  sum(num)) %>% 
#     filter((variableOfInterest==1 | str_detect(variableOfInterest, "Yes")) & tot > 0) %>%
#     filter((!group %in% rem)) %>% 
#     mutate(perc = 100*(num/tot))%>%
#     ungroup()
#   
#   col <- loc%>%
#     filter(!Location %in% rem)
#   col <- as.character(unique(col$Colour))
# 
#   res <- Map(prop.test,temp$num,temp$tot)
#   
#   temp[c("lower","upper")] <- t(sapply(res,"[[", "conf.int"))*100
#   
#   
# plot<-temp %>% 
#       ggplot(.) + 
#       geom_bar(aes(x= group, y = perc, fill=fill),  stat="identity",  colour = "black", size = 0.3) +
#     scale_x_discrete(drop=FALSE) +
#     scale_fill_manual(name = "Network", values = col, drop = TRUE)  +
#     ylim(0,100) +
#     xlab(xtitle) + ylab(ytitle) + my_theme +
#     geom_text(aes(x= group, y = perc, label=paste0(round(perc), "%")), vjust=-0.2, hjust = 1, size = 2)
#   
#   national<-temp %>% summarise(num_avg = sum(num), perc_avg = 100*sum(num)/sum(tot), perc_grouped_avg = mean(perc))
#   
#   output<-list(plot, temp, Chi, national)
#   
#   }
#   
#   
#   return(output)
# }


propSummariesCategories<-function(variableOfInterest,  ytitle,  xtitle){
  #variableOfInterest<-d$Q5
  
 # if(is.null(group)){#NOT GROUPING VERSION
    
    temp<-tibble(variableOfInterest = variableOfInterest)
    
    temp %<>% filter(!is.na(variableOfInterest)) %>%
      mutate(variableOfInterest = factor(variableOfInterest)) %>% 
      group_by(variableOfInterest, .drop = F) %>%  summarise(n = n()) %>%
     mutate(tot= sum(n)) %>% mutate(perc = 100*(n/tot))
    
    res <- Map(prop.test,temp$n,temp$tot)
    
    temp[c("lower","upper")] <- t(sapply(res,"[[", "conf.int"))*100
    
    plot_byNetwork<-temp %>% ungroup() %>% 
      ggplot(.,aes(x= variableOfInterest, y = perc)) + geom_bar(stat="identity",  colour = "black", fill = "grey", size = 0.3, position = position_dodge()) +
      #geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
      #scale_x_discrete(drop = F) +
      ylim(0,100) +
      xlab(xtitle) + ylab(ytitle) + my_theme +
      scale_x_discrete(drop = F, labels = function(x) str_wrap(x, width = 10)) +
      #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(aes( label=paste0(round(perc), "%")), vjust=-0.2, hjust = 1, size = 2.5)+
      theme(legend.position="none")
    
    output<-list(plot_byNetwork, temp)
  
  
  return(output)
}

