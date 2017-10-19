## ----
##  Ticket中哪些字是重要的？
Ticket <- substr(D[,"Ticket"],start=1,stop=1);
Ticket.tdm <- 
  Corpus(VectorSource(Ticket)) %>% ##  Name建立書冊
  TermDocumentMatrix(
    control = list(
      wordLengths = c(1,Inf)  ##  單字長度範圍
    )
  )
tail(sort(row_sums(Ticket.tdm)),10)  ##  出現次數相當高的單字 => 可能是重要的。
