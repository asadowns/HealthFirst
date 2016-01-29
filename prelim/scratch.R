library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(knitr)
library(kfigr)

setwd("~/RProg/HealthFirst/prelim")

if (!file.exists('Hospital_Revised_Flatfiles.zip')) {
  download.file('https://data.medicare.gov/views/bg9k-emty/files/Ma46xU4I05xsIKuEqLLi-N-s7GoO2ZefzJ7SYyTIKjA?content_type=application%2Fzip%3B%20charset%3Dbinary&filename=Hospital_Revised_Flatfiles.zip','Hospital_Revised_Flatfiles.zip',method='curl')
  downloadDate <- date()  
}

if (!file.exists('raw')) {
  unzip('Hospital_Revised_Flatfiles.zip', exdir="./raw")
}

if (!exists('merged')) {
  readmissionsMortality <- tbl_df(read.csv('raw/Readmissions and Deaths - Hospital.csv', stringsAsFactors = FALSE, header = TRUE, na.strings=c("Not Available","Too Few To Report")))
  readmissionsMortality <- readmissionsMortality %>% select(one_of(c("Provider.ID","Measure.ID","Score","Denominator","Compared.to.National"))) %>% 
    mutate(Provider.ID = as.character(Provider.ID)) %>%
    na.omit()
  
  readmissionHipKnee <- subset(readmissionsMortality, Measure.ID=='READM_30_HIP_KNEE')
  readmissionTotal <- subset(readmissionsMortality, Measure.ID=='READM_30_HOSP_WIDE')
  colnames(readmissionTotal) <- c("Provider.ID", "Measure.ID", "READM_SCORE", "Denominator", "Compared.to.National")

  effectiveCare <- tbl_df(read.csv('raw/Timely and Effective Care - Hospital.csv', stringsAsFactors = FALSE, header = TRUE, na.strings=c("Not Available","Too Few To Report")))
  

  totalPerformanceScore <- tbl_df(read.csv('raw/hvbp_tps_08_06_2015.csv', stringsAsFactors = FALSE, header = TRUE, na.strings=c("Not Available","Too Few To Report")))
  totalPerformanceScore <- totalPerformanceScore %>% 
    select(one_of(c("Provider.Number","Weighted.Patient.Experience.of.Care.Domain.Score", "Weighted.Clinical.Process.of.Care.Domain.Score","Weighted.Outcome.Domain.Score","Weighted.Efficiency.Domain.Score","Hospital.Name","Total.Performance.Score","State"))) %>% 
    na.omit() %>% 
    mutate(Provider.Number=as.character(Provider.Number)) %>% 
    group_by(State) %>% 
    filter(State=='FL') %>%
    gather(domain.type,domain.score, Weighted.Patient.Experience.of.Care.Domain.Score, Weighted.Clinical.Process.of.Care.Domain.Score, Weighted.Outcome.Domain.Score, Weighted.Efficiency.Domain.Score) %>%
    transform(Hospital.Name = reorder(Hospital.Name, domain.score))


  valueOfCare <- tbl_df(read.csv('raw/Payment and Value of Care - Hospital.csv', stringsAsFactors = FALSE, header = TRUE, na.strings=c("Not Available","Too Few To Report")))
  valueOfCare <- valueOfCare %>% select(one_of(c("Provider.ID","Value.of.care.display.name","Value.of.care.category","Payment.category"))) %>% 
    na.omit() %>% mutate(Provider.ID=as.character(Provider.ID))
  
  
  
  spendingPerPatient <- tbl_df(read.csv('raw/Medicare Hospital Spending per Patient - Hospital.csv', stringsAsFactors = FALSE, header = TRUE, na.strings=c("Not Available","Too Few To Report")))
  spendingPerPatient <- spendingPerPatient %>% select(one_of(c("Provider.ID","Hospital.Name","City","State","ZIP.Code","County.Name","Score"))) %>%
    mutate(Provider.ID = as.character(Provider.ID)) %>%
    na.omit()
  
  valueCareSpending <- inner_join(spendingPerPatient, valueOfCare, by="Provider.ID")
  
  readmissionSpendingScore <- inner_join(spendingPerPatient, readmissionTotal, by="Provider.ID")
  totalPerformanceReadmissionSpendingScore <- inner_join(readmissionSpendingScore, totalPerformanceScore, by=c("Provider.ID" = "Provider.Number"))
  
  
}
#Readmission Score vs. Medicare Spending Per Beneficiary score
ggplot(readmissionSpendingScore, aes(x=READM_SCORE, y=Score)) + 
  geom_point() + 
  geom_smooth(method="lm")
  


hist(spendingPerPatient$Score)

#Spending Per Patient
ggplot(spendingPerPatient, aes(Score)) + 
  geom_histogram(binwidth=0.025,alpha=0.5,aes(y = ..density.., fill= ..count..)) +
  geom_density() +
  stat_function(fun=dnorm, colour="red", args=list(mean=mean(spendingPerPatient$Score),sd=sd(spendingPerPatient$Score)))

#Mortality Not Linked to Efficiency score
ggplot(valueCareSpending, aes(x=Score,fill= Value.of.care.category)) + 
  geom_freqpoly(aes(colour= Value.of.care.category),size=2)
ggplot(valueCareSpending, aes(x=Score,fill= Value.of.care.category)) + 
  geom_bar(binwidth=0.025)

ggplot(valueCareSpending, aes(x=Score,fill= Payment.category)) + 
  geom_freqpoly(aes(colour= Payment.category),size=2)


#ggplot(totalPerformanceScore, aes(x=Hospital.Name, y=Total.Performance.Score))+geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Total Performance by Domain Score
ggplot(totalPerformanceScore, aes(x=Hospital.Name, y=domain.score)) + 
  geom_bar(aes(fill=domain.type),stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Domain", labels = c("Experience of Care", "Clinical Process of Care", "Outcome", "Efficiency")) + 
  xlab("Hospital Name") + ylab("Domain Score") + 
  ggtitle("Florida Hospitals Total Performance Score (Weighted)")

#ggplot(valueCareSpending, aes(Score,..density..,colour=Value.of.care.category))+geom_freqpoly()

#Good
ggplot(valueCareSpending, aes(Score,..density..,fill=Payment.category))+geom_density(alpha = 0.2)
ggplot(valueCareSpending, aes(Score,..density..,fill=Value.of.care.category))+geom_density(alpha = 0.2)
#/good

ggplot(readmissionHipKnee,aes(x=Denominator, y=Score))+geom_point(aes(colour=Compared.to.National))
ggplot(readmissionTotal,aes(x=Denominator, y=READM_SCORE))+geom_point(aes(colour=Compared.to.National))

with(totalPerformanceScore,dotchart(Total.Performance.Score[State=="FL"],Hospital.Name[State=="FL"]))

readmissionModel <- lm(Score~READM_SCORE,data=readmissionSpendingScore)
summary(readmissionModel)


var(totalPerformanceScore$Weighted.Patient.Experience.of.Care.Domain.Score)
var(totalPerformanceScore$Weighted.Clinical.Process.of.Care.Domain.Score)
var(totalPerformanceScore$Weighted.Efficiency.Domain.Score)
var(totalPerformanceScore$Weighted.Outcome.Domain.Score)
cor(totalPerformanceScore$Weighted.Patient.Experience.of.Care.Domain.Score,totalPerformanceScore$Weighted.Clinical.Process.of.Care.Domain.Score)
cor(totalPerformanceScore$Weighted.Patient.Experience.of.Care.Domain.Score,totalPerformanceScore$Weighted.Efficiency.Domain.Score)
cor(totalPerformanceScore$Weighted.Clinical.Process.of.Care.Domain.Score, totalPerformanceScore$Weighted.Efficiency.Domain.Score)

fit1 <- lm(Score~Efficiency,totalPerformanceReadmissionSpendingScore)
fit2 <- lm(Score~Efficiency+READM_SCORE,totalPerformanceReadmissionSpendingScore)
fit3 <- lm(Score~Efficiency+READM_SCORE+Patient,totalPerformanceReadmissionSpendingScore)
fit4 <- lm(Score~Efficiency+READM_SCORE+Patient+Process+Outcome,totalPerformanceReadmissionSpendingScore)

