library(tidyverse)
library(janitor)
library(broom)

dat <- read.csv('./FacultySalaries_1995.csv')

salary <- dat %>% select(FedID,UnivName,State,Tier,
                         AvgFullProfSalary,AvgAssocProfSalary,AvgAssistProfSalary,AvgProfSalaryAll) %>%
  pivot_longer(cols=-c(FedID,UnivName,Tier,State),
               names_to = 'position',
               values_to = 'salary') %>% 
  mutate(position=str_remove(position,'Prof'),
         position =str_remove(position,'Avg'),
         position=str_remove(position,'Salary'))


comp <- dat %>% select(FedID,UnivName,State,Tier,
                       AvgFullProfComp,AvgAssocProfComp,AvgAssistProfComp,AvgProfCompAll) %>%
  pivot_longer(cols=-c(FedID,UnivName,Tier,State),
               names_to = 'position',
               values_to = 'comp')%>% 
  mutate(position=str_remove(position,'Prof'),
         position =str_remove(position,'Avg'),
         position=str_remove(position,'Comp'))


num <- dat %>% select(FedID,UnivName,State,Tier,
                      NumFullProfs,NumAssocProfs,NumFacultyAll,NumInstructors,NumAssistProfs) %>%
  mutate(ProfAll=NumFullProfs+NumAssocProfs+NumAssistProfs) %>% 
  pivot_longer(cols=-c(FedID,UnivName,Tier,State),
               names_to = 'position',
               values_to = 'num')%>% 
  mutate(position=str_remove(position,'Prof'),
         position=str_remove(position,'Num'),
         position=if_else(str_ends(position,'s'),str_sub(position,end = -2L),position))
  


tidy <-salary %>% 
  full_join(num) %>% 
  full_join(comp)
  
tidy%>%
  filter(position=='Assist'|position=='Assoc'|position=='Full') %>%
  filter(Tier!='VIIB') %>% 
  
  ggplot(aes(y=salary,x=position,fill=position))+
  facet_wrap(vars(Tier))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90,vjust = .5))
  names(tidy)

  aov <- aov(data = tidy,
    salary~State+Tier+position)
aov
  anova(data = tidy,
      salary~State+Tier+position)
sasLM::aov1(data = tidy,
            salary~State+Tier+position) 
summary(aov)
 tidy(aov) 

 
 chemname<- c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene",
             "alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene",
             "beta-chamigrene","cuparene","compound.1","alpha-chamigrene","widdrol",
             "cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol",
             "alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol","compound.2",
             "thujopsenal")
chemname <- str_replace_all(chemname,'-','.')

oils <- read.csv('./Juniper_Oils.csv') 
 names(oils) 

t_oil <- oils %>% pivot_longer(cols = chemname,
                      names_to = 'chemical_id',
                      values_to ='concentration' )
######################################
t_oil %>% 
  ggplot(aes(y=concentration,x=YearsSinceBurn))+
  geom_smooth()+
  facet_wrap(vars(chemical_id),scales = 'free')

oilm<- glm(data = t_oil,concentration~YearsSinceBurn*chemical_id)


significant <- tidy(oilm) %>% 
  filter(p.value<=0.05)






