## ----
##  Name中哪些字是重要的？
Name.tdm <- 
  Corpus(VectorSource(D[,"Name"])) %>% ##  Name建立書冊
  TermDocumentMatrix(
    control = list(
      wordLengths = c(1,Inf)  ##  單字長度範圍
    )
  )
tail(sort(row_sums(Name.tdm)),10)  ##  出現次數相當高的單字 => 可能是重要的。










