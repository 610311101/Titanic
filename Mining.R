##### Mining data
library("slam");
library("tm");
library("dplyr");
##### ------------
Full <- merge(read.csv("train.csv"),read.csv("test.csv"), all = T); data.table(Full);
table(Full$Pclass, Full$Survived)


Full_class <- Full %>% filter(Pclass==1);
table(Full_class$ ,Full_class$Survived)















