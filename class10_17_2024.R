(list1 <-  list(1:10, 2:20, 3:30))
lapply(list1, sum)

?df

library(DBI)
"hello"




library(tidyverse)
dragons_db <- dbConnect(RSQLite::SQLite(), "dragons.db")
morphometrics <-  dbGetQuery(dragons_db, "SELECT * FROM morphometrics;")
head(morphometrics)
#word is from tidyverse
morphometrics$dragon_id <- word(morphometrics$capture_event,
                                start = 1, end = 1, sep = "_")

#counts by groups
table(morphometrics$dragon_id)

#modify dragon ids to make it looks more 
#than one measurement for some of the 
#dragon_ids

morphometrics[morphometrics$dragon_id %in% c("D86",
                                             "D6", "D485"),]$dragon_id <- "D80"

morphometrics[morphometrics$dragon_id %in% c("D43",
                                             "D1", "D270"),]$dragon_id <- "D49"
table(morphometrics$dragon_id)
?unique
#trouble shooting a loop ####
min_wingspan <-  rep(NA, length(unique(morphometrics$dragon_id)))
for (i in 1:length(unique(morphometrics$dragon_id))){
  sub <-  morphometrics[morphometrics$dragon_id ==
                          unique(morphometrics$dragon_id)[i], ]
  min_wingspan[i] <-  min(sub$wingspan_cm)
}

length(morphometrics$dragon_id)
length(unique(morphometrics$dragon_id))
length(min_wingspan)

#returns the 4 rows where dragon id is 80
morphometrics[morphometrics$dragon_id == "D80",]


morphometrics[morphometrics$dragon_id == "D9",]$wingspan_cm <- NA
min_wingspan
length(min_wingspan)

i <-  3
#when trouble shooting a loop look at global environment to
#see the value of i where the loop broke.


# trouble shooting apply ####
morph_list <-  split(morphometrics, f = morphometrics$dragon_id)
?split
morph_list[[1]]

morph_list[[2]]

morph_list[[3]]

min_wingspan_list <-lapply(morph_list, function(x) {
  min(x$wingspan_cm)
})

length(min_wingspan_list)
# list of data frames ####
x <-  morph_list[["D80"]]

unlist(min_wingwpan_list)#if you want to go from list to vector

do.call("rbind", morph_list)# if you want to 
lapply(morph_list, nrow)




#lapply returns a list
#sapply return a vector of matrix, multiple arguments
#mapply is multivariate version of lapply, multiple arguements

#apply, used for matrices/arrays, can be applied across columns
#(margin = 2) or rows (margin = 1)