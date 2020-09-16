
last_test <- read.csv('/srv/shiny-server/internetspeed/last_test.csv')
all_test <- read.csv('/srv/shiny-server/internetspeed/all_test.csv')
if (last_test$time != all_test[nrow(all_test),]$time) {
  write.table(last_test,'/srv/shiny-server/internetspeed/all_test.csv', 
              sep=',', 
              append=TRUE, 
              col.names = FALSE, 
              row.names=FALSE)
}

