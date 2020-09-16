last_test <- read_csv('last_test.csv')
all_test <- read_csv('all_test.csv')
if (last_test$time != all_test[nrow(all_test),]$time) {
  write.table(last_test,'/srv/shiny-server/internetspeed/all_test.csv', 
              sep=',', 
              append=TRUE, 
              col.names = FALSE, 
              row.names=FALSE)
}

