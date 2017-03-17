
id <- readLines("A:/R/Log/Log.txt")
test_id <- Sys.getenv("USERNAME")


res <- NULL

if(length(test_id) != 0) {
  for(i in 1:nchar(id[test_id])){
    let <- substr(id[test_id],i,i)
    n_let <- (which(let == letters) + 20) %% 26
    res <- paste0(res,letters[n_let])
  }
}

if(length(test_id) != 0) {
  for(i in 1:nchar(id[test_id])){
    let <- substr(id[test_id],i,i)
    n_let <- (which(let == letters) - 20) %% 26
    res <- paste0(res,letters[n_let])
  }
}

id <- readLines("A:/R/Log/Log.txt")
test_id <- Sys.getenv("USERNAME")

j <- 0
for (i in 1:length(id)){
  if(verifyPassword(hash = id[i],passwd = test_id)) j <- j + 1
}

if (j > 0)
  
  writeLines(id,"A:/R/Log/Log.txt")

id <- readLines()