#make data

library(tidyverse)

library(ggplot2)

ggplot(opioid_data, aes(y=pill_rate, x=republican_vote))+
  geom_point()+
  geom_smooth(method="lm", formula=y~x)

opioid_data$pill_rate<-opioid_data$num_pills/opioid_data$population

out<-lm(pill_rate~
   republican_vote+
   age+
   income+
   education+ 
   white,
   data=opioid_data)

# install.packages("stargazer")
library(stargazer)
stargazer(out, type="text")

?stargazer

#outliers

out<-formodels[is.na(formodels),]



names(formodels)

summary(lm(num_pills~
    obamacare+
      population,
   data=formodels))

#assumptions

#inspect missing data

#different outcomes


