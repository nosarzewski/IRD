list.of.packages <- c('arules',
                      'arulesViz',
                      'caret',
                      'datasets',
                      'data.table',
                      'dplyr',
                      'FSAdata',
                      'ggplot2',
                      'party',
                      'randomForest',
                      'ROCR',
                      'rpart',
                      'rpart.plot')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)