name <- function(x){
  require(tm)
  x <- gsub("[[:punct:]]","",x)
  x <- tolower(x)
  strsplit(x," ")
  tdm <- 
    VCorpus(VectorSource(x)) %>% ##  Name建立書冊
    TermDocumentMatrix(
      control = list(
        removePunctuation = TRUE,
        wordLengths = c(1,Inf)  ##  單字長度範圍
      )
    )
  tdm <- as.matrix(tdm)
  return(t(tdm))
}
#  select_Name(d,80)

select_Ticket <- function(data, stop = 1, frequency){
  Ticket <- substr(data[,"Ticket"],start = 1,stop = stop);
  Ticket.tdm <- 
    Corpus(VectorSource(Ticket)) %>% ##  Ticket建立書冊
    TermDocumentMatrix(
      control = list(
        wordLengths = c(1,Inf)  ##  單字長度範圍
      )
    )
  highfreq <- names(
    row_sums(Ticket.tdm)[
      row_sums(Ticket.tdm)>frequency
      ]
  )
  Ticket.tdm <- as.matrix(Ticket.tdm)
  Ticket.tdm <- Ticket.tdm[rownames(Ticket.tdm) %in% highfreq,]
  Ticket.data.frame <- as.data.frame(t(Ticket.tdm))
  return(Ticket.data.frame)
}


select_Cabin <- function(data, stop = 1, frequency){
  Cabin <- substr(data[,"Cabin"],start = 1,stop = stop);
  Cabin.tdm <- 
    Corpus(VectorSource(Cabin)) %>% ##  Cabin建立書冊
    TermDocumentMatrix(
      control = list(
        wordLengths = c(1,Inf)  ##  單字長度範圍
      )
    )
  highfreq <- names(
    row_sums(Cabin.tdm)[
      row_sums(Cabin.tdm)>frequency
      ]
  )
  Cabin.tdm <- as.matrix(Cabin.tdm)
  Cabin.tdm <- Cabin.tdm[rownames(Cabin.tdm) %in% highfreq,]
  Cabin.data.frame <- as.data.frame(t(Cabin.tdm))
  return(Cabin.data.frame)
}


variable.dot <- function(data, main, ignore){
  data.ignore <- select(data,ignore)
  data.main   <- select(data,main)
  data <- 
    select(
      data,
      setdiff(colnames(data),c(main,ignore))
    )
  for( i in 1:ncol(data) ){
    v <- data.main * data[,i]
      if( i == 1 ) V <- v
      if( i != 1 ) V <- cbind(V,v)
  }
  colnames(V) <- paste0(main,".V",seq(ncol(V)))
  data.return <- 
    cbind(
      data.ignore,
      data.main,
      data,
      V
    )
  return(data.return)
}
















