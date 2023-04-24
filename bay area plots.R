library(tidyverse)
library(sf)

mydf<-read_csv("Data/DataCompile.csv")

mydf_2<-mydf %>%
  select(StationCode, SampleDate, Replicate, CSCI=`CSCI Score`,ASCI_D=ASCIdiatom,ASCI_H=ASCIhybrid,CRAM) %>%
  pivot_longer(cols=c(CSCI,ASCI_D,ASCI_H,CRAM), names_to = "Index",values_to = "IndexScore", values_drop_na = T)

#Table 3 in the report
stacked_bar_dat<-
  read.table(file="clipboard", header = T, sep="\t")

stacked_bar_dat2<-stacked_bar_dat %>%
  pivot_longer(cols=c(Likely.intact, Possibly.altered, Likely.altered,Very.likely.altered)) %>%
  mutate(name=factor(name, levels=c("Likely.intact", "Possibly.altered", "Likely.altered","Very.likely.altered")),
         Index=factor(Index, levels=c("CSCI","ASCI-D","ASCI-H","CRAM"))) %>%
  group_by(Index, Subpopulation) %>%
  mutate(TotalValue=sum(value)) %>%
  ungroup() %>%
  mutate(value2=value/TotalValue)
  

regional_condition_stacked_bar<-ggplot(data=stacked_bar_dat2 %>%
         filter(Subpopulation=="Water Board Region 2") %>%
           filter(Index!="IPI"),
       aes(y=value2, fill=name, x=Index))+
  geom_bar(position=position_stack(), stat="identity")+
  scale_fill_brewer(palette="RdYlBu", direction=-1, 
                    name="Condition",
                    labels=c("Likely intact","Possibly altered","Likely altered","Very likely altered"))+
  theme_bw()+
  ylab("Proportion of streams")+xlab("")+
  theme(panel.grid = element_blank())
ggsave(regional_condition_stacked_bar, filename="Figures/regional_condition_stacked_bar.png", height=4, width=6.5)


#GIS
mygis<-read_csv("Data/Stations_GIS_Index.csv")
master_df<-read_csv("Data/Stations_Master.csv")

mygis2<-mydf_2 %>%
  inner_join(mygis %>%
                select(StationCode,New_Lat, New_Long)) %>%
  mutate(Condition = case_when(Index==CSCI & IndexScore<0.63~"Very likely altered",
                               Index==CSCI & IndexScore<0.79~"Likely altered",
                               Index==CSCI & IndexScore<0.92~"Possibly altered",
                               Index==CSCI > ))
  inner_join(master_df %>%
                select(StationCode, FlowStatus_Final, SiteStatus_Final))

ggplot(data=mygis2 %>%
         filter(SiteStatus_Final == "Reference"), aes(x=Index, y=IndexScore))+
  geom_boxplot()
