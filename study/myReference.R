loadlibrary <- function(x, repos='http://cran.fiocruz.br') 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos=repos, dep=TRUE)
    require(x)
  }
}

loadlibrary("bibtex")
loadlibrary("network")
loadlibrary("RColorBrewer")
loadlibrary("igraph")
loadlibrary("ggplot2")
loadlibrary("readr")
loadlibrary("dplyr")
loadlibrary("reshape")
loadlibrary("tm")
loadlibrary("SnowballC")
loadlibrary("wordcloud")
loadlibrary("xlsx")

bibtext.dataset <- function(citations) {
  cit <- lapply(citations, function(x) x$title)
  citcode <- names(unlist(cit))
  authors.l <- lapply(citations, function(x) x$author)
  authors <- character(length(authors.l))
  for(i in 1:length(authors.l)){
    x <- authors.l[[i]]
    main <- x[1]$family
    if (length(x) > 1)
      main <- paste(c(main, 'et al.'), sep=" ")
    authors[i] <- main
  }
  title <- as.character(unlist(lapply(citations, function(x) x$title)))
  years <- as.integer(unlist(lapply(citations, function(x) x$year)))
  cites <- as.integer(substr(unlist(lapply(citations, function(x) x$note)), 1, 5))
  abstract <- as.character(lapply(citations, function(x) x$abstract))
  
  dataset <- data.frame(citcode, authors, years, cites, title, abstract)
  return(dataset)
}

bibtext.cloudword <- function(mydata, filename) {
  #mytext <- c(as.character(mydata$title), as.character(mydata$abstract))
  mytext <- as.character(mydata$abstract)
  mytext <- paste(mytext, collapse = ' ')
  
  myCorpus <- Corpus(VectorSource(mytext))
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, PlainTextDocument)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, removeWords, stopwords('english'))
  myCorpus <- tm_map(myCorpus, stemDocument)
  myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
  pal2 <- brewer.pal(8,"Dark2")
  m = as.matrix(myDTM)
  v = sort(rowSums(m), decreasing = TRUE)
  set.seed(1)
  pdf(filename, width=16,height=12)
  wordcloud(names(v), v,max.words =Inf,min.freq=3,scale=c(8,0.2), random.order = FALSE,rot.per=.15,vfont=c("sans serif","plain"),colors=brewer.pal(21, 'Blues')[7:14])
  dev.off()  
}

bibtext.rankbycites <- function(data, pivot_value, filter=3) {
  if(nrow(data)==0) {
    mydata <- data.frame(pivot = pivot_value, citcode="")
  }    
  else {
    mydata <- head(data[order(data$cites, decreasing=TRUE), ], filter)
    mydata <- aggregate(citcode ~ pivot, data = mydata, paste, collapse = ",")
  }
  return(mydata)
}

bibtext.savecsv <- function(mydata, csvfilename) {
  write.table(mydata, file=csvfilename, row.names=FALSE, quote = FALSE, sep="\t")
}

bibtext.saveexcel <- function(mydata, xlsfilename) {
  write.xlsx(mydata, xlsfilename, sheetName="database", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)  
}


bibtext.mergecsv <- function(mydata, csvfilename) {
  references <- read_csv(csvfilename)
  references$title <- NULL
  references <- unique(references)
  
  mydata <- merge(x = mydata, y = references, by = "citcode", all.x=TRUE)
  return(mydata)
}


bibtext.diffbib <- function(mydata, bibtext) {
  citations <- read.bib(bibtext)
  citations <- lapply(citations, function(x) x$title)
  citations <- names(unlist(citations))
  
  mydiff <- setdiff(mydata$citcode, citations)
  return (mydiff)
}

# leitura de bib text com a contabilização
citations <- read.bib('scopus-count.bib')
mydataset <- bibtext.dataset(citations)

# motagem de nuvem de palavras para análise
myword <- bibtext.cloudword(mydataset, "publication-cloud.pdf")

# motagem de nuvem de palavras para análise
bibtext.saveexcel(mydataset, "references.xlsx")

#analise (leitura manual dos abstracts) e carregamento dos atributos adicionais
mydataset <- bibtext.mergecsv(mydataset, "references-filled.csv")

# filtragem de artigos e filtro adicionais
mydataset$rate <-  mydataset$cites - (2015-mydataset$years) > 0
mydataset$delay <- (regexpr('delay', mydataset$abstract) + regexpr('delay', mydataset$title)) != -2
mydataset <- mydataset[mydataset$rate & mydataset$delay,]

# identifica artigos não citados ainda
mydiff <- bibtext.diffbib(mydataset, 'references.bib')

databases.aggreg <- function(mydata, country) {
  myEnsemble <- bibtext.rankbycites(filter(mydata, Ensemble == 1), country)
  names(myEnsemble) = c("country", "ensemble")
  myAirport <- bibtext.rankbycites(filter(mydata, airport == 1), country)
  names(myAirport) = c("country", "airport")
  myAirline <- bibtext.rankbycites(filter(mydata, airline == 1), country)
  names(myAirline) = c("country", "airline")
  mydata <- data.frame(myEnsemble$country, myEnsemble$ensemble, myAirline$airline, myAirport$airport)
  return(mydata)
}

databases <- function() {
  mydataset$pivot <-  mydataset$Country
  mydatabase <- databases.aggreg(filter(mydataset, pivot == "Asia"), "Asia")
  mydatabase <- rbind(mydatabase, databases.aggreg(filter(mydataset, pivot == "Brazil"), "Brazil"))
  mydatabase <- rbind(mydatabase, databases.aggreg(filter(mydataset, pivot == "Europe"), "Europe"))
  mydatabase <- rbind(mydatabase, databases.aggreg(filter(mydataset, pivot == "US"), "US"))

  bibtext.savecsv(mydatabase, "databases.csv")
  bibtext.saveexcel(mydatabase, "databases.xlsx")
}

databases()

timeline.aggreg <- function(mydataset) {
  mydata <- merge(x = aggregate(cites ~ years, data = filter(mydataset, ML == 1), max), y = filter(mydataset, ML == 1), by=c("years","cites"), all.x=TRUE)
  myML <- aggregate(citcode ~ years, data = mydata, paste, collapse = ",")
  mydata <- merge(x = aggregate(cites ~ years, data = filter(mydataset, OR == 1), max), y = filter(mydataset, OR == 1), by=c("years","cites"), all.x=TRUE)
  myOR <- aggregate(citcode ~ years, data = mydata, paste, collapse = ",")
  mydata <- merge(x = aggregate(cites ~ years, data = filter(mydataset, NET == 1), max), y = filter(mydataset, NET == 1), by=c("years","cites"), all.x=TRUE)
  myNET <- aggregate(citcode ~ years, data = mydata, paste, collapse = ",")
  mydata <- merge(x = aggregate(cites ~ years, data = filter(mydataset, PS == 1), max), y = filter(mydataset, PS == 1), by=c("years","cites"), all.x=TRUE)
  myPS <- aggregate(citcode ~ years, data = mydata, paste, collapse = ",")
  mydata <- merge(x = aggregate(cites ~ years, data = filter(mydataset, STAT == 1), max), y = filter(mydataset, STAT == 1), by=c("years","cites"), all.x=TRUE)
  mySTAT <- aggregate(citcode ~ years, data = mydata, paste, collapse = ",")
  
  myMLOR <- merge(x = myML, y = myOR, by="years", all=TRUE)
  myNETPS <- merge(x = myNET, y = myPS, by="years", all=TRUE)
  myMLORNETPS <- merge(x = myMLOR, y = myNETPS, by="years", all=TRUE)
  myMLORNETPSSTAT <- merge(x = myMLORNETPS, y = mySTAT, by="years", all=TRUE)
  names(myMLORNETPSSTAT) <- c("Years", "ML", "OR", "NET", "PS", "STAT")
  return(myMLORNETPSSTAT)
}

timeline <- function(mydataset) {
  new <- timeline.aggreg(filter(mydataset, new == 1 | cancel == 1))
  bibtext.savecsv(new, "timeline-new-cancel.csv")
  bibtext.saveexcel(new, "timeline-new-cancel.xlsx")

  propag <- timeline.aggreg(filter(mydataset, propag == 1))
  bibtext.savecsv(propag, "timeline-propag.csv")
  bibtext.saveexcel(propag, "timeline-propag.xlsx")
}

timeline(mydataset)

plot_publications <- function(mydataset, col.set) {
  myMLOR <- merge(x = aggregate(ML ~ years, data = mydataset, sum), y = aggregate(OR ~ years, data = mydataset, sum), by="years", all=TRUE)
  myNETPS <- merge(x = aggregate(NET ~ years, data = mydataset, sum), y = aggregate(PS ~ years, data = mydataset, sum), by="years", all=TRUE)
  myMLORNETPS <- merge(x = myMLOR, y = myNETPS, by="years", all=TRUE)
  myMLORNETPSSTAT <- merge(x = myMLORNETPS, y = aggregate(STAT ~ years, data = mydataset, sum), by="years", all=TRUE)
  names(myMLORNETPSSTAT) <- c("Years", "ML", "OR", "NET", "PS", "STAT")
  series <- melt(myMLORNETPSSTAT[,c("Years", "ML", "OR", "NET", "PS", "STAT")],id.vars = 1)
  names(series) <- c("x", "variable", "value")

  grf <- plot.stackedbar(series, colors=rev(col.set),label_x = "", label_y = "")
  return (grf)
}

bibtext.journals <- function(citations) {
  journals <- unlist(lapply(citations, function(x) x$journal))
  journals.summary <- table(journals)
  journals.summary <- sort(journals.summary, decreasing=TRUE)
  myjnames <- names(journals.summary)
  myjnames <- gsub("(?<=[A-Z])[^A-Z]+", "", myjnames, perl = TRUE)
  names(journals.summary) <- myjnames
  journals.summary <- journals.summary[journals.summary>1]
  
  journals.tbl <- data.frame(journals.summary)
  names(journals.tbl) <- c("variable", "value")
  return(journals.tbl)
}

plots <-function(mydataset, citations) {
  col.set <- brewer.pal(5, 'Blues')
  
  grf <- plot_publications(mydataset, col.set)
  grf <- grf + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plot(grf)
  
  journals.tbl <- bibtext.journals(citations)
  grf <- plot.bar(journals.tbl, label_x="journal", label_y="qtd", color=col.set[5])
  grf <- grf + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plot(grf)
}

plots(mydataset, citations)

collaboration <- function(citations) {
  col.set <- brewer.pal(5, 'Blues')
  authors <- lapply(citations, function(x) x$author)
  idx <- grep(".family",names(unlist(authors)))
  unique.authors <- unique((unlist(authors))[idx])
  coauth.table <- matrix(nrow = length(unique.authors), ncol=length(unique.authors), dimnames = list(unique.authors, unique.authors), 0)
  
  for(i in 1:length(citations)){
    paper.auth <- unlist(authors[[i]])[names(unlist(authors[[i]])) == 'family']
    coauth.table[paper.auth,paper.auth] <- coauth.table[paper.auth,paper.auth] + 1
  }
  
  linhas <- apply(coauth.table, 1, max, na.rm=TRUE)
  series <- data.frame(value=linhas)
  grf <- plot.hist(series, bin=1, label_x="publications of authors", colors=col.set[5])
  grf <- grf + scale_x_continuous(breaks = round(seq(1, max(series$value), by = 2),1))
  plot(grf)
  
  coauth.table <- subset(coauth.table, linhas > 3)   
  coauth.table <- t(coauth.table)
  coauth.table <- subset(coauth.table, linhas > 3)   
  
  author.net <- network(coauth.table)
  network.vertex.names(author.net) <- rownames(coauth.table)
  mat <- coauth.table
  val <- rep(0,nrow(mat))
  for(i in 1:nrow(mat)){  # need an expression that retruns a sequence
    val[i] <- mat[i,i];
    mat[i,i] <- 0;  # need to index the matrix with two element if using i,j
  }
  
  rep <- graph.adjacency(mat, mode="undirected", weighted=TRUE)
  
  plot( rep, layout = layout.reingold.tilford,
        #edge.width = 1,
        #edge.arrow.width = 0.3,
        vertex.size = 5+val/min(val),
        #edge.arrow.size = 0.5,
        vertex.size2 = 3,
        vertex.label.cex = 1,
        asp = 0.5,
        edge.width=E(rep)$weight,
        vertex.color=col.set[1],
        margin = -0.1)
}

collaboration(citations)



