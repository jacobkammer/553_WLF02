R.version
#repeat actions ####

vec3 * 3
deploy <-  DBI::dbGetQuery(dragons_db,
                            "SELECT * FROM deployments;")


deploy$start_deployment <-  lubridate::ymd(deploy$start_deployment)

deploy$start_deployment[1:(nrow(deploy) - 1)] - 
  deploy$start_deployment[2:nrow(deploy)]


for (i in 1:nrow(deploy)){
  print(deploy$start_deployment[i] - 
          deploy$start_deployment[i + 1])
}

system.time({
  for (i in 1:nrow(deploy)){
  print(deploy$start_deployment[i] - 
          deploy$start_deployment[i + 1])
}})

time_difference <-  c()
for (i in 1:nrow(deploy)){
  time_difference[i] <- deploy$start_deployment[i] - 
          deploy$start_deployment[i + 1]
}

time_difference


deploy$time_difference <-  NA

for (i in 1:nrow(deploy)){
  deploy$time_difference[i] <- deploy$start_deployment[i] - 
    deploy$start_deployment[i + 1]
}

head(deploy)


#apply family ####
?apply
mat1<- matrix(1:25, nrow = 5, ncol = 5)
mat1
apply(mat1, 2, FUN = sum)

list.2 <-  lapply(X = 1:length(deploy$start_deployment),
       FUN = function(x){
         deploy$start_deployment[x] - 
           deploy$start_deployment[x + 1]
       })

unlist(list.2)
unlist(lapply(vec1, FUN = function(x) {x * 2}))


?lapply