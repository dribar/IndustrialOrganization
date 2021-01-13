### CODE FOR EMPIRICS SECTION

setwd("")

#TOC:
#1) Ahrar & HTS/Nusra Info
#2) Local v Non-Local Groups
#3) Mapmaking

#1) Ahrar Info
#Loading in the info:
ahrar<-read.csv("All_Ahrar.csv")
#Who are they attacking?
table(ahrar$Other_Group[ahrar$Ahrar_Status=="Attacker"])
#Average casualties of the attacks?
mean(ahrar$Total.Killed[ahrar$Ahrar_Status=="Attacker" &
                          ahrar$Other_Group=="Government" & is.na(ahrar$Total.Killed)==F])
mean(ahrar$Total.Killed[ahrar$Ahrar_Status=="Attacker" &
                          ahrar$Other_Group=="Other Rebels" & is.na(ahrar$Total.Killed)==F])
#Average Scale of the attacks?
ahrar$Attack.Scale.Num<-NA
ahrar$Attack.Scale<-as.factor(ahrar$Attack.Scale)
ahrar$Attack.Scale.Num<-ifelse(grepl("Nano",ahrar$Attack.Scale)==T,1,ahrar$Attack.Scale.Num)
ahrar$Attack.Scale.Num<-ifelse(grepl("Micro",ahrar$Attack.Scale)==T,2,ahrar$Attack.Scale.Num)
ahrar$Attack.Scale.Num<-ifelse(grepl("Macro",ahrar$Attack.Scale)==T,3,ahrar$Attack.Scale.Num)
ahrar$Attack.Scale.Num<-ifelse(grepl("Extreme",ahrar$Attack.Scale)==T,4,ahrar$Attack.Scale.Num)
mean(ahrar$Attack.Scale.Num[ahrar$Ahrar_Status=="Attacker" & ahrar$Other_Group=="Government" &
                              is.na(ahrar$Attack.Scale.Num)==F])
mean(ahrar$Attack.Scale.Num[ahrar$Ahrar_Status=="Attacker" & ahrar$Other_Group=="Other Rebels" &
                              is.na(ahrar$Attack.Scale.Num)==F])
#Loading in HTS/Nusra Info:



#2) Local v Non-Local Groups
#Creating a combined dataset of all events:
fsa<-read.csv("FSA.csv")
local1<-read.csv("Local1.csv")
local2<-read.csv("Local2.csv")
local3<-read.csv("Local3.csv")
nonlocal1<-read.csv("NonLocal1.csv")
nonlocal2<-read.csv("NonLocal2.csv")
nonlocal3<-read.csv("NonLocal3.csv")
nonlocal4<-read.csv("NonLocal4.csv")
nonlocal5<-read.csv("NonLocal5.csv")
nonlocal6<-read.csv("NonLocal6.csv")
nonlocal7<-read.csv("NonLocal7.csv")
nonlocal8<-read.csv("NonLocal8.csv")
nonlocal9<-read.csv("NonLocal9.csv")
nonlocal10<-read.csv("NonLocal10.csv")
nonlocal11<-read.csv("NonLocal11.csv")
nonlocal12<-read.csv("NonLocal12.csv")
nonlocal13<-read.csv("NonLocal13.csv")
nonlocal14<-read.csv("NonLocal14.csv")
nonlocal15<-read.csv("NonLocal15.csv")
nonlocal16<-read.csv("NonLocal16.csv")
full<-rbind(local1,local2)
full<-rbind(full,local3)
full<-rbind(full,fsa)
full<-rbind(full,nonlocal1)
full<-rbind(full,nonlocal2)
full<-rbind(full,nonlocal3)
full<-rbind(full,nonlocal4)
full<-rbind(full,nonlocal5)
full<-rbind(full,nonlocal6)
full<-rbind(full,nonlocal7)
full<-rbind(full,nonlocal8)
full<-rbind(full,nonlocal9)
full<-rbind(full,nonlocal10)
full<-rbind(full,nonlocal11)
full<-rbind(full,nonlocal12)
full<-rbind(full,nonlocal13)
full<-rbind(full,nonlocal14)
full<-rbind(full,nonlocal15)
full<-rbind(full,nonlocal16)
rm(local1,local2,local3,fsa,nonlocal1,nonlocal2,nonlocal3,nonlocal4,nonlocal5,nonlocal6,nonlocal7,nonlocal8,
   nonlocal9,nonlocal10,nonlocal11,nonlocal12,nonlocal13,nonlocal14,nonlocal15,nonlocal16)
#Subsetting out the Macro and Extreme Events:
full.big<-subset.data.frame(full,
                            grepl("Extreme",full$Attack.Scale)==T|
                              grepl("Macro",full$Attack.Scale)==T)
#write.csv(full.big,file="newfullbig.csv")
#Loading the now-handcoded data back in:
fb.coded<-read.csv("fullbig.csv")
#Coding each attack for local or non-local initiation:
fb.coded$Date<-as.Date(fb.coded$Event.Date,"%d-%b-%y")
fb.coded$init.local<-ifelse(fb.coded$Instigator=="All Three"|
                              fb.coded$Instigator=="Both"|
                              (fb.coded$Instigator=="FSA" & 
                                 fb.coded$Date<=as.Date("31-Mar-12","%d-%b-%y"))|
                              fb.coded$Instigator=="Islamic State, Local"|
                              fb.coded$Instigator=="Local"|
                              fb.coded$Instigator=="Local, FSA",1,0)
fb.coded$init.nonlocal<-ifelse(fb.coded$Instigator=="All Three"|
                                 fb.coded$Instigator=="Both"|
                                 (fb.coded$Instigator=="FSA" & 
                                    fb.coded$Date>as.Date("31-Mar-12","%d-%b-%y"))|
                                 fb.coded$Instigator=="Islamic State"|
                                 fb.coded$Instigator=="Islamic State, Local"|
                                 fb.coded$Instigator=="Nonlocal"|
                                 fb.coded$Instigator=="NonLocal",1,0)
#Getting the Local target proportions:
table(fb.coded$Target[fb.coded$init.local==1])
#Govt proportion:
(65)/97
#Other rebel proportion:
(18+11)/97
#Civilian proportion:
2/97
#Getting the NonLocal target proportions:
table(fb.coded$Target[fb.coded$init.nonlocal==1])
#Govt proportion:
(189+1)/454
#Other rebel proportion:
(1+66+2+1+115)/454
#Civilian Proportion:
(78)/454
#Coding for propensity to target the government, rebels, or civ's:
fb.coded$targ.gov<-ifelse(fb.coded$Target=="Government"|
                            fb.coded$Target=="International",1,0)
fb.coded$targ.reb<-ifelse(fb.coded$Target=="All Three"|
                            fb.coded$Target=="Islamic State"|
                            fb.coded$Target=="ISlamic State"|
                            fb.coded$Target=="Kurds"|
                            fb.coded$Target=="Other Rebels",1,0)
fb.coded$targ.civ<-ifelse(fb.coded$Target=="Civilians",1,0)
#T-tests for local v non-local propensity to target the government, rebels, or civ's:
t.test(fb.coded$targ.gov[fb.coded$init.local==1],fb.coded$targ.gov[fb.coded$init.nonlocal==1],
       alternative="greater")
t.test(fb.coded$targ.reb[fb.coded$init.local==1],fb.coded$targ.reb[fb.coded$init.nonlocal==1],
       alternative="less")
t.test(fb.coded$targ.civ[fb.coded$init.local==1],fb.coded$targ.civ[fb.coded$init.nonlocal==1],
       alternative="less")


#Getting target fatalities:
#avg local gov fatalities:
mean(fb.coded$Total.Killed[fb.coded$init.local==1 & fb.coded$targ.gov==1&
                             is.na(fb.coded$Total.Killed)==F])
#avg local reb fatalities:
mean(fb.coded$Total.Killed[fb.coded$init.local==1 & fb.coded$targ.reb==1&
                             is.na(fb.coded$Total.Killed)==F])
#avg local civ fatalities:
mean(fb.coded$Total.Killed[fb.coded$init.local==1 & fb.coded$targ.civ==1&
                             is.na(fb.coded$Total.Killed)==F])
#avg nonlocal gov fatalities:
mean(fb.coded$Total.Killed[fb.coded$init.nonlocal==1 & fb.coded$targ.gov==1&
                             is.na(fb.coded$Total.Killed)==F])
#avg nonlocal reb fatalities:
mean(fb.coded$Total.Killed[fb.coded$init.nonlocal==1 & fb.coded$targ.reb==1&
                             is.na(fb.coded$Total.Killed)==F])
#avg nonlocal civ fatalities:
mean(fb.coded$Total.Killed[fb.coded$init.nonlocal==1 & fb.coded$targ.civ==1&
                             is.na(fb.coded$Total.Killed)==F])
#T-tests to examine the significance of these fatality differences:
t.test(fb.coded$Total.Killed[fb.coded$init.local==1 & fb.coded$targ.reb==1&
                               is.na(fb.coded$Total.Killed)==F],
       fb.coded$Total.Killed[fb.coded$init.nonlocal==1 & fb.coded$targ.reb==1&
                               is.na(fb.coded$Total.Killed)==F],
       alternative="less")
t.test(fb.coded$targ.civ[fb.coded$init.local==1],fb.coded$targ.civ[fb.coded$init.nonlocal==1],
       alternative="less")


#And fatality totals for the early part of the paper:
sum(fb.coded$Total.Killed[fb.coded$targ.reb==1 & is.na(fb.coded$Total.Killed)==F &
                            (fb.coded$init.local==1 | fb.coded$init.nonlocal==1)])/
  sum(fb.coded$Total.Killed[is.na(fb.coded$Total.Killed)==F])
sum(fb.coded$Total.Killed[fb.coded$targ.reb==1 & is.na(fb.coded$Total.Killed)==F &
                            (fb.coded$init.local==1 | fb.coded$init.nonlocal==1) &
                            grepl("Islamic State",fb.coded$Active.Group.Name)==F])/
  sum(fb.coded$Total.Killed[is.na(fb.coded$Total.Killed)==F])
#Comparing brutality:
#first by comparing proportion of suicide attacks:
table(fb.coded$Suicide.[fb.coded$init.local==1])/
  sum(is.na(fb.coded$Suicide.[fb.coded$init.local==1])==F)
table(fb.coded$Suicide.[fb.coded$init.nonlocal==1])/
  sum(is.na(fb.coded$Suicide.[fb.coded$init.nonlocal==1])==F)
#creating a binary variable and running t-tests:
fb.coded<-fb.coded %>% mutate(killer=as.numeric(Suicide. != " No"))
t.test(fb.coded$killer[fb.coded$init.local==1],
       fb.coded$killer[fb.coded$init.nonlocal==1],
       alternative="less")


#second by random or indiscriminate targeting:
mean(grepl("Random or Indiscriminate",fb.coded$Target.Sector[fb.coded$init.local==1]))
mean(grepl("Random or Indiscriminate",fb.coded$Target.Sector[fb.coded$init.nonlocal==1]))
#creating a binary variable and running t-tests:
fb.coded<-fb.coded %>% mutate(randos=as.numeric(grepl("Random or Indiscriminate",fb.coded$Target.Sector)))
t.test(fb.coded$randos[fb.coded$init.local==1],
       fb.coded$randos[fb.coded$init.nonlocal==1],
       alternative="less")

#Now confirming these findings w/ the GTD data.
#Pull in the gtd data and subset to just attacks in Syria w/ named assailants
gtdf<-read.csv("EntireGTD.csv")
gtd<-subset.data.frame(gtdf,country_txt=="Syria" & iyear>=2011)
gtd<-subset.data.frame(gtd,gname!="Unknown")
#Bust ghosts
gtd<-droplevels(gtd)
#Classify local, nonlocal instigators:
localnames<-c("Abu Amarah Battalion","Ahrar al-Sham","Ajnad al-Sham",
              "Aleppo Fatah Operations Room","Al-Furqan Brigades","Al-Muthana Islamic Movement",
              "Ansar al-Din Front","Fajr al-Umma Brigade","Greater Damascus Operations Room",
              "Islamic Front (Syria)","Jaish al-Mujahideen (Syria)","Jaish al-Sunnah",
              "Jund al-Aqsa","Khaled Ibn al-Walid Army","Liwa al-Haqq","Liwa al-Tawhid",
              "People's Protection Units (YPG)","Revolutionaries Army (Jaysh al-Thowwar)",
              "Yarmouk Martyrs Brigade")
nonlocalnames<-c("Abdul Qader Husseini Battalions of the Free Palestine movement",
                 "Al-Nasir Army (Syria)","Al-Nusrah Front","Al-Qaida in Iraq", "Al-Sham Legion",
                 "Ansar al-Islam",
                 "Ansar al-Sharia Operations Room (Syria)","Authenticity and Development Front",
                 "Free Idlib Army","Hezbollah","Hay'at Tahrir al-Sham",
                 "Islamic State of Iraq and the Levant (ISIL)","Jaish al-Fatah (Syria)",
                 "Jaish al-Muhajireen wal-Ansar (Muhajireen Army)","Jaysh al-Islam (Syria)",
                 "Kurdistan Workers' Party (PKK)","Liwa al-Islam",
                 "Northern Homs Countryside Operations Room","Nur-al-Din al-Zinki Movement",
                 "Popular Front for the Liberation of Palestine, Gen Cmd (PFLP-GC)",
                 "Shamiya Front","Southern Front","Syrian Democratic Forces (SDF)",
                 "Syrian Turkmen Brigades","Tajamo Ansar al-Islam")
unknownnames<-c("Ayesha bint al-Sadiq Brigade","Diraa al-Shahbaa Rebel Brigade",
                "Gathering to Aid the Oppressed","League of Damascus for Special Tasks",
                "Liwa al-Sham","The Mujahadeen Room in Latakia Countryside",
                "Turkestan Islamic Party")
gtd$init.local<-ifelse(gtd$gname %in% localnames|
                         gtd$gname2 %in% localnames|
                         gtd$gname3 %in% localnames|
                         (gtd$gname=="Free Syrian Army" & gtd$eventid<107188)|
                         (gtd$gname2=="Free Syrian Army" & gtd$eventid<107188)|
                         (gtd$gname3=="Free Syrian Army" & gtd$eventid<107188),
                       1,0)
gtd$init.nonlocal<-ifelse(gtd$gname %in% nonlocalnames|
                            gtd$gname2 %in% nonlocalnames|
                            gtd$gname3 %in% nonlocalnames|
                            (gtd$gname=="Free Syrian Army" & gtd$eventid>=107188)|
                            (gtd$gname2=="Free Syrian Army" & gtd$eventid>=107188)|
                            (gtd$gname3=="Free Syrian Army" & gtd$eventid>=107188),
                          1,0)
#Classify Government, Rebel, Civilian targeting:
gtd$targ.gov<-ifelse(gtd$targtype1 %in% c(2,3,4,7)|
                       gtd$targtype2 %in% c(2,3,4,7)|
                       gtd$targtype3 %in% c(2,3,4,7),1,0)
gtd$targ.reb<-ifelse(gtd$targtype1 %in% c(17)|
                       gtd$targtype2 %in% c(17)|
                       gtd$targtype3 %in% c(17),1,0)
gtd$targ.civ<-ifelse(gtd$targtype1 %in% c(1,8,9,10,12,13,14,15,18,19)|
                       gtd$targtype2 %in% c(1,8,9,10,12,13,14,15,18,19)|
                       gtd$targtype3 %in% c(1,8,9,10,12,13,14,15,18,19),1,0)
#Numbers, proportions:
#Local v gov:
sum(gtd$init.local==1 & gtd$targ.gov==1)/sum(gtd$init.local==1)
#Local v reb:
sum(gtd$init.local==1 & gtd$targ.reb==1)/sum(gtd$init.local==1)
#NonLocal v gov:
sum(gtd$init.nonlocal==1 & gtd$targ.gov==1)/sum(gtd$init.nonlocal==1)
#NonLocal v reb:
sum(gtd$init.nonlocal==1 & gtd$targ.reb==1)/sum(gtd$init.nonlocal==1)
#Local v civ:
sum(gtd$init.local==1 & gtd$targ.civ==1)/sum(gtd$init.local==1)

sum(gtd$init.nonlocal==1 & gtd$targ.civ==1)/sum(gtd$init.nonlocal==1)
#T-test for statistical significance in their different propensities to attack other rebs:
t.test(gtd$targ.reb[gtd$init.local==1],
       gtd$targ.reb[gtd$init.nonlocal==1],
       alternative="less")

#Distinguishing rebel orgs that are geographically localized (see paper)
stayedlocalprov<-c("Abu Amarah Battalion","Ajnad al-Sham","Al-Qaida in Iraq",
                   "Aleppo Fatah Operations Room","Al-Furqan Brigades",
                   "Al-Nasir Army","Ansar al-Din Front","Ansar al-Islam",
                   "Ansar al-Sharia Operations Room","Authenticity and Development Front",
                   "Free Idlib Army","Greater Damascus Operations Room",
                   "Hezbollah","Jaish al-Fatah","Jaish al-Mujahideen",
                   "Jaish al-Sunnah","Jund al-Aqsa","Kurdistan Workers’ Party",
                   "Liwa al-Islam","Nur-al-Din al-Zinki Movement",
                   "People’s Protection Units (YPG","Shamiya Front",
                   "Syrian Democratic Forces","Syrian Turkmen Brigades",
                   "Turkestan Islamic Party","Yarmouk Martyrs Brigade")
gtd$init.cluster.prov<-ifelse(gtd$gname %in% stayedlocalprov|
                                gtd$gname2 %in% stayedlocalprov|
                                gtd$gname3 %in% stayedlocalprov,1,0)
sum(gtd$init.cluster.prov==1 &gtd$targ.reb==1)/sum(gtd$init.cluster.prov)
sum(gtd$init.cluster.prov==1 &gtd$targ.gov==1)/sum(gtd$init.cluster.prov)
stayedlocaltime<-c("Abu Amarah Battalion","Al-Furqan Brigades","Ansar al-Din Front",
                   "Jaish al-Fatah","Liwa al-Islam","People’s Protection Units (YPG)",
                   "Shamiya Front")
gtd$init.cluster.time<-ifelse(gtd$gname %in% stayedlocaltime|
                                gtd$gname2 %in% stayedlocaltime|
                                gtd$gname3 %in% stayedlocaltime,1,0)
sum(gtd$init.cluster.time==1 &gtd$targ.reb==1)/sum(gtd$init.cluster.time)
sum(gtd$init.cluster.time==1 &gtd$targ.gov==1)/sum(gtd$init.cluster.time)


###3) Mapmaking
#Creating a base map of Syria:
#devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
Syria <- get_stamenmap(bbox = c(left = 35.5, bottom = 32.2, 
                                  right = 42.4, top = 37.3), 
                         zoom = 7)
SyriaMap<-ggmap(Syria,extent = "device")
#Prepping the interaction data:
ahrar.v.is<-read.csv("jointattacks.csv")
dupes<-!duplicated(ahrar.v.is$Event.ID)
ahrar.v.is<-ahrar.v.is[dupes==T,]
ahrar.v.is$Date<-as.Date(ahrar.v.is$Event.Date.Date,"%m/%d/%y")
ahrar.v.is$IslamicFront<-ifelse(ahrar.v.is$Date<as.Date("22-Nov-13","%d-%b-%y"),0,1)
ahrar.v.is$DynColour<-ifelse(ahrar.v.is$Dynamic=="Clashing","gray0","gray60")
#And creating the maps:
#First a map of Pre-IF interactions:
ahrarpre<-subset.data.frame(ahrar.v.is,IslamicFront==0 & (ISvAhrar=="N/A" | ISvAhrar=="Yes"))
SyriaMap+
  geom_count(data=ahrarpre,
             aes(x=Longitude,y=Latitude,colour=Dynamic))+
  scale_size_continuous(range = c(5, 15))+
  scale_colour_manual(breaks=ahrar.v.is$Dynamic, 
                      values=unique(as.character(ahrar.v.is$DynColour)))+
  theme(legend.title=element_blank())
#Saved as a 4x6 pdf titled SyriaMapPreIF
#Second a map of Post-IF interactions:
ahrarpost<-subset.data.frame(ahrar.v.is,IslamicFront==1 & (ISvAhrar=="N/A" | ISvAhrar=="Yes"))
SyriaMap+
  geom_count(data=ahrarpost,
             aes(x=Longitude,y=Latitude,colour=Dynamic))+
  scale_size_continuous(range = c(5, 15))+
  scale_colour_manual(breaks=ahrar.v.is$Dynamic, 
                      values=unique(as.character(ahrar.v.is$DynColour)))+
  theme(legend.title=element_blank())
#Saved as a 4x6 pdf titled SyriaMapPostIF
#And third a map of who initiated clashes:
ahrarclashes<-subset.data.frame(ahrar.v.is,
                                ISvAhrar=="Yes" & (StartedClash=="Ahrar" | StartedClash=="IS"))
ahrarclashes$ClaColour<-ifelse(ahrarclashes$StartedClash=="Ahrar","gray0","gray60")
SyriaMap+
  geom_count(data=ahrarclashes,
             aes(x=Longitude,y=Latitude,colour=StartedClash))+
  scale_size_continuous(range = c(5, 15))+
  scale_colour_manual(breaks=ahrarclashes$StartedClash, 
                      values=unique(as.character(ahrarclashes$ClaColour)))+
  theme(legend.title=element_blank())
#Saved as a 4x6 pdf titled SyriaMapInstigators





