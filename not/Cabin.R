## ----
##  Cabin中哪些字是重要的？
Cabin <- substr(D[,"Cabin"],start = 1,stop = 1)
Cabin.tdm <- 
  Corpus(VectorSource(Cabin)) %>% ##  Cabin建立書冊
  TermDocumentMatrix(
    control = list(
      wordLengths = c(1,Inf)  ##  單字長度範圍
    )
  )
tail(sort(row_sums(Cabin.tdm)),10)  ##  出現次數相當高的單字 => 可能是重要的。
