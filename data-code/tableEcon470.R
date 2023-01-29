library(dplyr)


table1<-table(full.ma.data$plan_type,full.ma.data$year)
table1

knitr::kable(table1, type="html",caption = "Plan Count by Year", booktabs= TRUE)

modifiedFullData<-subset(full.ma.data, snp !="Yes" | eghp != "Yes" | planid !=800:899)
modifiedFullData <- full.ma.data[!(full.ma.data$snp == "Yes" | full.ma.data$eghp == "Yes" | full.ma.data$planid == 800:899), ]

table2<-table(modifiedFullData$plan_type,modifiedFullData$year)


knitr::kable(table2, type="html",caption = "Plan Count by Year", booktabs= TRUE)

avgMediGraph<-drop_na(final.data, avg_enrollment)

avgMediGraph<-inner_join(avgMediGraph,contract.service.area%>%select(contractid,fips,year),by=c("contractid","fips","year"))

enrollmentGraph<- graph1Data%>%
  group_by(fips,year)%>%
  select(fips,year,avg_enrollment)%>%
  summarize(all_enroll=sum(avg_enrollment))%>%
                          ggplot(aes(x = as.factor(year), y = all_enroll))+
  stat_summary(fun.y="mean",geom="bar")+
  xlab("Year") +
  ylab("Average Enrollment") +
  ggtitle("Average Enrollment per County by Year")+
  scale_y_continuous(labels=comma)
enrollmentGraph
figure1<-ggsave(filename="enrollmentGraph.png",enrollmentGraph,height=8,width=10)

mergedData<-inner_join(plan.premiums,avgMediGraph)

avgPremium<-drop_na(plan.premiums, premium)
avgPremiumGraph <- final.data %>% summarize(avg_variable = mean(premium))
premiumAvgGraph<- ggplot(final.data, aes(x = year , y = mean(premium))) + geom_line()+xlab("Year") +
  ylab("Average Premium") +
  ggtitle("Average Premium per Year")
premiumAvgGraph
write_rds(avgPremium,)

avgPremiumGraph<- ggplot(data=final.data,aes(x=year,y=premium))+
  stat_summary(fun.y="mean",geom='bar')+
  labs(
    x="Year",
    y="Average Premium",
    title="Average Premium Over Years"
  )
avgPremiumGraph
figure2<-ggsave(filename="averagePremiums.png",avgPremiumGraph,height=8,width=10)

final.data<-filter(final.data,!is.na(premium))
final.data$percent <- ifelse(final.data$premium == 0,1,0)
final.data$percent <- ave(final.data$percent, final.data$year, FUN = mean) * 100

premium0graph<-ggplot(final.data, aes(x =year, y = percent)) +
  geom_line() +
  ylab("Percentage of $0 Premium Plans") +
  xlab("Year") +
  ggtitle("Percentage of $0 Premium Plans From 2007 to 2015") 
premium0graph
figure3<-ggsave(filename="premium0graph.png",premium0graph,height=8,width=10)
