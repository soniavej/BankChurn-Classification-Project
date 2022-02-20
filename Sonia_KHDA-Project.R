str(Main)
str(Fees2020)
school=inner_join(Main,Fees2020,by="School Name")
str(school)
summary(school)
#View(school)
School1=school %>%
  select(School="School Name",Location,Type="Type of School",Year_Est="Year Established in Dubai",
         Rating="2019/20\r\nDSIB Rating",Enrol20_21="2020/21 Enrollments",Enrol19_20="2019/20 Enrollments",
         Enrol18_19="2018/19 Enrollments",FS1="Pre primary/ FS1...4",KG1="KG1/FS2...5",KG2="KG2/Y1...6",
         Grade1="G1/Y2...7",Grade2="G2/Y3...8",Grade3="G3/Y4...9",Grade4="G4/Y5...10",Grade5="G5/Y6...11",
         Grade6="G6/Y7...12",Grade7="G7/Y8...13",Grade8="G8/Y9...14",Grade9="G9/Y10...15",Grade10="G10/Y11...16",
         Grade11="G11/Y12...17",Grade12="G12/Y13...18")%>%
  #View(School1)
  School1%>%
   mutate(Rating=ifelse(Rating=="Unsatisfactory"|Rating=="Weak",0,
                     ifelse(Rating=="Acceptable",1,
                              ifelse(Rating=="Good",2,
                                    ifelse(Rating=="Very Good"|Rating=="Very good",3, 
                                           ifelse(Rating=="Outstanding",4,"No Rating Available"))))))%>%
  
  #School1%>%
  #summarise(unique(Type))%>%
  School1%>%
  mutate(Type=recode(Type,"Iranian"="Other Asian","Ministry of Education"="MOE",
                     "UK/IB"="IB","Chinese"="Other Asian","UK/BTEC"="UK",
                     "American/ IB"="IB","International Baccalaureate"="IB",
                     "UK - UK/IB"="IB","American - Ministry of Education"="MOE",
                     "Indian - IB"="IB","German"="Other Western","US/IB"="IB",
                     "Pakistani"="Other Asian","French - IB"="IB",
                     "Iranian - IB"="IB","Japanese"="Other Western",
                     "French"="Other Western","Ministry of Education/ IB"="MOE",
                     "Russian"="Other Asian","SABIS (UK/US)"="SABIS","Philippine"="Other Asian"))%>%

   ###########   1."Percentage of Schools as per Ratings"################
###########   1."Percentage of Schools as per Ratings"################
School1%>%
  select(School="School Name",Location,Type="Type of School",Year_Est="Year Established in Dubai",
         Year2020="2019/20\r\nDSIB Rating",Year2019="2018/19\r\nDSIB Rating",
         Enrol20_21="2020/21 Enrollments",Enrol19_20="2019/20 Enrollments",
         Enrol18_19="2018/19 Enrollments",FS1="Pre primary/ FS1...4",KG1="KG1/FS2...5",KG2="KG2/Y1...6",
         Grade1="G1/Y2...7",Grade2="G2/Y3...8",Grade3="G3/Y4...9",Grade4="G4/Y5...10",Grade5="G5/Y6...11",
         Grade6="G6/Y7...12",Grade7="G7/Y8...13",Grade8="G8/Y9...14",Grade9="G9/Y10...15",Grade10="G10/Y11...16",
         Grade11="G11/Y12...17",Grade12="G12/Y13...18")%>%
  mutate(Rating=ifelse(Year2020=="Not inspected due to Covid 19",Year2019,Year2020))%>%
  #summarise(unique(Year2020))
  mutate(Rating=ifelse(Rating=="Unsatisfactory"|Rating=="Weak",0,
                       ifelse(Rating=="Acceptable",1,
                              ifelse(Rating=="Good",2,
                                     ifelse(Rating=="Very Good"|Rating=="Very good",3, 
                                            ifelse(Rating=="Outstanding",4,"No Rating Available"))))))%>%
  
  filter(Rating>=0,Rating<=4,na.rm=TRUE) %>%
  group_by(Rating)%>%
  summarise(NSchools=n(),Percentage=(NSchools/nrow(School1)))%>%
  ggplot(aes(x=Rating,y=Percentage))+geom_col(color="Black", fill="Blue",width=0.4)+
  geom_text(aes(label = scales::percent(Percentage)),vjust=-0.2,size=5,check_overlap = FALSE)+
  scale_y_continuous(label = scales::percent)%>%
  labs(title="Percentage of Schools as per Ratings")+xlab("Rating")+
  ylab("Percentage of Schools")+ 
  theme(panel.background = element_rect(fill = "skyblue", colour = "Blue",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))

     
           
 #####   2.Percentage of Enrolments per curiculum for each rating################

School1%>%
  select(Enrol20_21,Type,Rating)%>%
  mutate(Rating=ifelse(Rating=="Unsatisfactory"|Rating=="Weak",0,
                       ifelse(Rating=="Acceptable",1,
                              ifelse(Rating=="Good",2,
                                     ifelse(Rating=="Very Good"|Rating=="Very good",3, 
                                            ifelse(Rating=="Outstanding",4,"No Rating Available"))))))%>%
  
  mutate(Type=recode(Type,"Iranian"="Other Asian","Ministry of Education"="MOE",
                     "UK/IB"="IB","Chinese"="Other Asian","UK/BTEC"="UK",
                     "American/ IB"="IB","International Baccalaureate"="IB",
                     "UK - UK/IB"="IB","American - Ministry of Education"="MOE",
                     "Indian - IB"="IB","German"="Other Western","US/IB"="IB",
                     "Pakistani"="Other Asian","French - IB"="IB",
                     "Iranian - IB"="IB","Japanese"="Other Western",
                     "French"="Other Western","Ministry of Education/ IB"="MOE",
                     "Russian"="Other Asian","SABIS (UK/US)"="SABIS","Philippine"="Other Asian"))%>%
  #mutate(TotalEnrol=sum(Enrol20_21))%>%
  filter(Rating>=0,Rating<=4,na.rm=TRUE) %>%
  group_by(Rating,Type)%>%
  summarise(Enrol=sum(Enrol20_21),Percentage=round(Enrol/sum(School1$Enrol20_21),3))%>%
  ggplot(aes(as.factor(Rating),Percentage,fill=Type))+geom_col(color="Blue",width=0.6)+
  geom_text(aes(label =scales::percent(Percentage)),position = position_stack(vjust=0.25),size=3.5,check_overlap =TRUE)+
 scale_y_continuous(label = scales::percent)%>%
    labs(title="Percentage of Enrollments in each Curriculum per Ratings")+xlab("Rating")+ylab("Percentage Enrolments")+
theme(panel.background = element_rect(fill = "grey", colour = "Blue",
                                      size = 2, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"))



 #### 3. Calculating Avg fees per student per school  ############
    
    
    School1 %>%
      #select(.,row(9:23))%>%
      mutate_all( ~replace(., is.na(.), 0))%>%
      mutate(Avg = rowMeans(.[9:23]))%>%
      #View()
      mutate(Rating=ifelse(Rating=="Unsatisfactory"|Rating=="Weak",0,
                           ifelse(Rating=="Acceptable",1,
                                  ifelse(Rating=="Good",2,
                                         ifelse(Rating=="Very Good"|Rating=="Very good",3, 
                                                ifelse(Rating=="Outstanding",4,"No Rating Available"))))))%>%
      mutate(Type=recode(Type,"UK/BTEC"="UK","UK/IB"="IB","American/ IB"="IB","French - IB"="IB",
                         "International Baccalaureate"="IB","UK - UK/IB"="IB","Indian - IB"="IB",
                         "Iranian - IB"="IB","US/IB"="IB","Ministry of Education/ IB"="MOE","Ministry of Education"="MOE","American - Ministry of Education"="MOE",
                         "Philippine"="Other Asian","Russian"="Other Asian","Iranian"="Other Asian","Pakistani"="Other Asian",
                         "French"="Other Western","Japanese"="Other Western","German"="Other Western",
                         "SABIS (UK/US)"="SABIS")) %>%
      
      filter(Rating>=2,Rating<=4)%>%
      ggplot(aes(x=Avg,y=Enrol20_21,col=Type,shape=Rating))+geom_point(size=2.5)+
      scale_x_continuous(breaks = seq(0, 90000, by = 10000))+
      labs(title=" Private School Fees Distribution by size,Curiculum and Ratings in Dubai")+xlab("Average Fees in AED")+ylab("Enrollments")
    
#6.
School1 %>%
  #View(School1)
  #select(.,row(9:23))%>%
  mutate_all(~replace(.,is.na(.),0))%>%
  mutate(Avg=rowMeans(.[9:23]))%>%
  mutate(Range=ifelse(Avg<=40000,"Below 40000",ifelse(Avg>40000|Avg<80000,"40000-80000",ifelse(Avg>=80000,"Above 80000"))))%>%
  mutate(Rating=ifelse(Rating=="Unsatisfactory"|Rating=="Weak",0,
                       ifelse(Rating=="Acceptable",1,
                              ifelse(Rating=="Good",2,
                                     ifelse(Rating=="Very Good"|Rating=="Very good",3, 
                                            ifelse(Rating=="Outstanding",4,"No Rating Available"))))))%>%
  mutate(Type=recode(Type,"UK/BTEC"="UK","UK/IB"="IB","American/ IB"="IB","French - IB"="IB",
                     "International Baccalaureate"="IB","UK - UK/IB"="IB","Indian - IB"="IB",
                     "Iranian - IB"="IB","US/IB"="IB","Ministry of Education/ IB"="MOE","Ministry of Education"="MOE","American - Ministry of Education"="MOE",
                     "Philippine"="Other Asian","Russian"="Other Asian","Iranian"="Other Asian","Pakistani"="Other Asian",
                     "French"="Other Western","Japanese"="Other Western","German"="Other Western",
                     "SABIS (UK/US)"="SABIS")) %>%
  group_by(Range,Rating)%>%
  filter(Rating>=1,Rating<=4)%>%
  summarise(NSchools=n(),Schools=(NSchools/nrow(School1)*100))%>%
  ggplot(aes(x=Range,y=Schools,fill=Rating))+geom_col()+
 #scale_x_continuous(breaks = seq(0, 30000, by = 10000))+
  #facet_grid(.~Rating)
    
    ######## 4.Quality Education by location ##########
    
  School1%>%
    select(Location,Rating,School)%>%
    #summarise(unique(Location))%>%
     #View()
    mutate(Location=recode(Location,"Al Qusais"="Deira", "Al Barsha 1"="Al Barsha","Al Khawaneej 1"="Others",
                           "Hatta"="Outside Dubai City",
                           "Al Nahda 2"="Deira",
                           "Al Rashidiya"="Deira",
                           "Jabel Ali"="Jebel Ali",
                           "Al Mamzar"="Deira",
                           "Al Safa 1"="Jumeirah",
                           "Al Garhoud"="Deira",
                           "Al Barsha 2"="Al Barsha",
                           "Al Mizhar 1"="Outside Dubai City",
                           "Dubai Investment Park"="New Dubai",
                           "Oud Metha"="Bur Dubai",
                           "Al Twar 2"="Deira",
                           "Jumeirah 1"="Jumeirah",
                           "Al Quoz 2"="Bur Dubai",
                           "Al Mankhool"="Bur Dubai",
                           "Nad Al Sheba"="Bur Dubai",
                           "Al Quoz 4"="Bur Dubai",
                           "Jumeirah Village Triangle"="Nakheel",
                           "Al Muhaisnah 1"="Deira",
                           "Al Twar 1"="Deira",
                           "Mirdif"="Deira",
                           "Al Quoz 1"="Bur Dubai",
                           "Umm Suqueim 3"="Jumeirah",
                           "Dubai Fesitval City"="Deira",
                           "Al Hudaiba"="Deira",
                           "Emirates Hills"="Emaar Emirates Hills",
                           "Jumeirah Islands"="Nakheel",
                           "Al Sufouh 1"="Al Sufouh",
                           "Academic City"="Others",
                           "Zabeel 1"="Bur Dubai",
                           "Wadi Al Safa 3"="Deira",
                           "Deira"="Deira",
                           "Umm Suqueim 1"="Jumeirah",
                           "Meadows"="Emaar Emirates Hills",
                           "Umm Hurair"="Bur Dubai",
                           "Dubai Land"="New Dubai",
                           "Barsha South"="Al Barsha",
                           "Al Khail"="Nakheel",
                           "Jumeirah 3"="Jumeirah",
                           "Motor City"="New Dubai",
                           "Nad Al Sheba 1"="Bur Dubai",
                           "Al Warqa'a 1"="Deira",
                           "Dubai Silicon Oasis"="Others",
                           "Al Satwa"="Bur Dubai",
                           "Meydan City"="Bur Dubai",
                           "Al Karama"="Bur Dubai",
                           "Abu Hail"="Deira",
                           "Umm Al Sheif"="Jumeirah",
                           "Al Wasl"="Jumeirah",
                           "Dubai Knowledge Village"="Al Sufouh",
                           "Al Warqa'a 3"="Deira",
                           "Nad Al Sheba 3"="Bur Dubai",
                           "Jumeirah Village Circle (South)"="Nakheel",
                           "Arabian Ranches"="New Dubai",  
                           "Nad Al Sheba 2"="Bur Dubai",
                           "Hor Al Anz"="Deira",
                           "Al Rowaiyah"="Bur Dubai",
                           "Al Muhaisnah 4"="Deira",
                           "Al Raffa"="Bur Dubai",
                           "The Greens"="Emaar Emirates Hills",
                           "Dubai Sports City"="Nakheel",
                           "Al Warqa'a 4"="Deira",
                           "Bur Dubai"="Bur Dubai",
                           "Dubai Healthcare City"="Bur Dubai",
                           "Al Furjan"="Jebel Ali", 
                           "Nad Al Hammar"="Others",
                           "Al Muhaisnah 2"="Deira",
                           "Ras Al Khor"="Others"))%>%
    #summarise(unique(Location))
    mutate(Rating=ifelse(Rating=="Unsatisfactory"|Rating=="Weak",0,
                         ifelse(Rating=="Acceptable",1,
                                ifelse(Rating=="Good",2,
                                       ifelse(Rating=="Very Good"|Rating=="Very good",3, 
                                              ifelse(Rating=="Outstanding",4,"No Rating Available"))))))%>%

  mutate(Location_f=factor(Location, levels=c('Deira','Bur Dubai',
                                                'Jumeirah','Al Sufouh',"Al Barsha",
                                                "Emaar Emirates Hills","Nakheel",
                                                "New Dubai","Jabel Ali","Offshore",
                                                "Outside Dubai City","Others")))%>%
                                                
  group_by(Location_f, Rating) %>%
    summarise(NSchool=n())%>%
    filter(Rating==2|Rating==3|Rating==4,na.rm=TRUE)%>%
    summarise(NSchool=n())%>%
    ggplot(aes(Rating,NSchool))+geom_point() +facet_wrap(.~Location_f)+
    labs(title="Quality of Education per Region in Dubai")+xlab("Rating")+ylab("Number of Schools")+
   theme(strip.background=element_rect(fill="SkyBlue"))+
  theme(panel.background = element_rect(fill = "grey", colour = "SkyBlue",size = 2, linetype = "solid"))+
  
  
    
                    
    ###### 5.Schools Ratings in Dubai in last 3 years.############
    
    School1 %>%
      #summary(School1)
      select(School="School Name",Location,Type="Type of School",Year_Est="Year Established in Dubai",
             Year2020="2019/20\r\nDSIB Rating",Year2019="2018/19\r\nDSIB Rating",Year2018="2017/18\r\nDSIB Rating",
             Enrol20_21="2020/21 Enrollments",Enrol19_20="2019/20 Enrollments",
             Enrol18_19="2018/19 Enrollments",FS1="Pre primary/ FS1...4",KG1="KG1/FS2...5",KG2="KG2/Y1...6",
             Grade1="G1/Y2...7",Grade2="G2/Y3...8",Grade3="G3/Y4...9",Grade4="G4/Y5...10",Grade5="G5/Y6...11",
             Grade6="G6/Y7...12",Grade7="G7/Y8...13",Grade8="G8/Y9...14",Grade9="G9/Y10...15",Grade10="G10/Y11...16",
             Grade11="G11/Y12...17",Grade12="G12/Y13...18")%>%
      mutate(Year2020=ifelse(Year2020=="Not inspected due to Covid 19",Year2019,Year2020))
      #View(School1)
    
    Long=select(School1,School,Year2020,Year2019,Year2018)
    RankingDB=gather(Long,Year,Rating,-1)
    Ratings=mutate(RankingDB,Rating=ifelse(Rating=="Unsatisfactory"|Rating=="Weak",0,
                                           ifelse(Rating=="Acceptable",1,
                                                  ifelse(Rating=="Good",2,
                                                         ifelse(Rating=="Very Good"|Rating=="Very good",3, 
                                                                ifelse(Rating=="Outstanding",4,"No Rating Available"))))))
    
    HRatings=filter(Ratings,Rating>=1,Rating<=4,na.rm=TRUE)
    View(HRatings)
    
    HRatings%>%
      select(Year,Rating)%>%
      group_by(Year,Rating)%>%
      summarise(NSchool=n())%>%
      ggplot(HRatings)+geom_bar(aes(x=Year,fill=Rating),position=position_dodge(preserve='single'))+
      labs(title=" Private School Ratings in Dubai (2018-2020)")+xlab("Year")+ylab("Number of Schools")
    
       
    
    
                     
                     
                     
                     
                     
                     
                   

  
  
  
  
  
  
  
  
  