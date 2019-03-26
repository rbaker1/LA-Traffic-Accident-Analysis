### Insights into LA Collisions dataset

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(data.table)) install.packages("data.table") 
if(!require(ggmap)) install.packages("ggmap")


## TODO: Basic insights of data


dat.sample <- dat %>% sample_n(100, replace = F)
data.insights <- dat %>% group_by(mo_codes) %>% summarise(count = n())

