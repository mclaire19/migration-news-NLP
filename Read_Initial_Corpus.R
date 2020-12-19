library(stringr)
library(tidyverse)
library(quanteda)

# number of documents in test directory
N <- 498

for (i in 2:22){
  assign(paste0('df', i), data.frame(Date=character(N),
                                text=character(N), 
                                Author=character(N), 
                                DocID=character(N),
                                Publisher=character(N),
                                Location=character(N),
                                stringsAsFactors=FALSE))
}

# loop2
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data2//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df2$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df2$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df2$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df2$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df2$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df2$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df2$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df2$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df2$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df2$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df2$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}

# loop3
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data3//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df3$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df3$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df3$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df3$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df3$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df3$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df3$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df3$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df3$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df3$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df3$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}


# loop4
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data4//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df4$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df4$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df4$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df4$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df4$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df4$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df4$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df4$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df4$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df4$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df4$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}

# loop5
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data5//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df5$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df5$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df5$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df5$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df5$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df5$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df5$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df5$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df5$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df5$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df5$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}

# loop6
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data6//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df6$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df6$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df6$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df6$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df6$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df6$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df6$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df6$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df6$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df6$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df6$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}

# loop7
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data7//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df7$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df7$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df7$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df7$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df7$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df7$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df7$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df7$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df7$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df7$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df7$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}

# loop8
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data8//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df8$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df8$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df8$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df8$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df8$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df8$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df8$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df8$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df8$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df8$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df8$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}

# loop9
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data9//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df9$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df9$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df9$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df9$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df9$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df9$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df9$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df9$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df9$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df9$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df9$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}

# loop10
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data10//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df10$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df10$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df10$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df10$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df10$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df10$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df10$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df10$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df10$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df10$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df10$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}

# loop11
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data11//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df11$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df11$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df11$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df11$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df11$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df11$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df11$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df11$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df11$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df11$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df11$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}

# loop12
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data12//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df12$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df12$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df12$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df12$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df12$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df12$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df12$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df12$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df12$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df12$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df12$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}

# loop13
for (i in 1:N) {
  
  if (i == 60){
    next()
  }
  
  #read in document
  doc <- readLines(paste0("data13//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df13$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df13$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df13$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df13$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df13$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df13$Publisher[i] <- NA
  }
  
  if (length(ind_pub) != 0){
    lendoc <- length(doc)
    ind_fin <- lendoc
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df13$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df13$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df13$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df13$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if ((length(ind_end) != 1) & length(ind_pub) != 0){
    ind_end <- ind_pub
  } else {
    ind_end <- ind_fin
  }
  
  #append fulltext to the dataframe
  df13$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}


# loop14
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data14//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df14$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df14$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df14$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df14$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df14$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df14$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df14$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df14$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df14$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df14$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df14$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}


# loop15
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data15//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df15$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df15$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df15$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df15$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df15$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df15$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df15$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df15$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df15$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df15$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df15$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}


# loop16
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data16//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df16$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df16$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df16$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df16$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df16$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df16$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df16$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df16$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df16$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df16$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df16$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}



# loop17
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data17//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df17$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df17$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df17$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df17$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df17$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df17$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df17$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df17$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df17$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df17$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df17$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}



# loop18
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data18//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df18$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df18$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df18$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df18$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df18$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df18$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df18$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df18$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df18$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df18$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df18$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}


# loop2
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data19//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df19$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df19$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df19$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df19$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df19$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df19$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df19$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df19$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df19$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df19$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df19$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}


# loop20
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data20//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df20$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df20$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df20$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df20$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df20$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df20$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df20$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df20$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df20$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df20$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df20$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}


# loop21
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data21//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df21$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df21$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df21$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df21$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df21$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df21$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df21$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df21$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df21$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df21$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df21$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}


# loop22
for (i in 1:N) {
  
  #read in document
  doc <- readLines(paste0("data22//", i, '.txt'))
  
  # search for author index and replace in dataframe if it exists
  ind_auth <- grep("^Author: ", doc)
  if (length(ind_auth) == 1) {
    df22$Author[i] <- stringr::str_replace_all(doc[ind_auth],"Author: ", "")
  } else {
    df22$Author[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_date <- grep("^Publication date: ", doc)
  if (length(ind_date) == 1) {
    df22$Date[i] <- stringr::str_replace_all(doc[ind_date],"Publication date: ", "")
  } else {
    df22$Date[i] <- NA
  }
  
  # search for publication date index and replace in dataframe if it exists
  ind_pub <- grep("^Publication title: ", doc)
  if (length(ind_pub) == 1) {
    df22$Publisher[i] <- stringr::str_replace_all(doc[ind_pub],"Publication title: ", "")
  } else {
    df22$Publisher[i] <- NA
  }
  
  # search for docid index and replace in dataframe if it exists
  ind_id <- grep("^ProQuest document ID: ", doc)
  if (length(ind_id) == 1) {
    df22$DocID[i] <- stringr::str_replace_all(doc[ind_id],"ProQuest document ID: ", "")
  } else {
    df22$DocID[i] <- NA
  }
  
  # search for location index and replace in dataframe if it exists
  ind_loc <- grep("^Location: ", doc)
  if (length(ind_loc) == 1) {
    df22$Location[i] <- stringr::str_replace_all(doc[ind_loc],"Location: ", "")
  } else {
    df22$Location[i] <- NA
  }
  
  # search for start index and end index of the fulltext
  ind_start <- grep("^Full text: ", doc)
  ind_end <- (grep("^Subject: ", doc) - 1)
  
  # one of the documents is missing this, so reassign to the publication index
  if (length(ind_end) != 1) {
    ind_end <- ind_pub
  }
  
  #append fulltext to the dataframe
  df22$text[i] <- paste(doc[ind_start:ind_end], collapse = ' ')
}

dflist <- list(df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17, df18, df19, df20, df21, df22)

final <- bind_rows(dflist)

final$text[final$text==""] <- NA

final <- final %>%
  filter(!is.na(text)) %>%
  filter(!is.na(DocID)) %>%
  separate(text, c(NA, 'Text'), sep = 11) %>%
  separate(Date, c('FullDate', 'Year'), sep = ', ', remove = FALSE) %>%
  mutate(PostTrump = ifelse(Year >= 2016, 1, 0))


sapply(final, function(x) sum(is.na(x)))


corp <- corpus(final, text_field = 'Text', docid_field = 'DocID')

docvars(corp, "Date") <- final$Date
docvars(corp, "FullDate") <- final$FullDate
docvars(corp, "Year") <- final$Year
docvars(corp, "Author") <- final$Author
docvars(corp, "Publisher") <- final$Publisher
docvars(corp, "Location") <- final$Location
docvars(corp, "PostTrump") <- final$PostTrump

summary(corp)

rm(dflist, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17, df18, df19, df20, df21, df22)
