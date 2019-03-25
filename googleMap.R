<<<<<<< HEAD
## Google Map Visualization

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(data.table)) install.packages("data.table") 
if(!require(ggmap)) install.packages("ggmap")


source('gmapapi.R') #contains gmapkey

register_google(key = gmapkey, write = T) #register Google map API key 

LA_ggmap <- get_googlemap("waco, texas", zoom = 12, maptype = "roadmap", source = "google") %>% ggmap()
=======
## Google Map Visualization

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(data.table)) install.packages("data.table") 
if(!require(ggmap)) install.packages("ggmap")


source('gmapapi.R') #contains gmapkey

register_google(key = gmapkey, write = T) #register Google map API key 

LA_ggmap <- get_googlemap("waco, texas", zoom = 12, maptype = "roadmap", source = "google") %>% ggmap()
>>>>>>> efc6cb8c05cdb07d193f9091188bbb8c4b24eb25
