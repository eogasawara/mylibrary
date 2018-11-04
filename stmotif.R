STSADatasetAdjust  <- function(D, tb, sb) {
  c = ncol(D)
  r = nrow(D)
  ec = c %% sb
  er = r %% tb
  D = D[1:(r-er), 1:(c-ec)]
  return (D)
}

CSAMiningProcess <- function (D,DS,w,a,sb,tb,si,ka){
  DS <- NormSAX(D,a)
  stmotifs <- SearchSTMotifs(D,DS,w,a,sb,tb,si,ka)
  rstmotifs <- RankSTMotifs(stmotifs)
  return(rstmotifs)
}

NormSAX <- function (D,a){
  vector <- as.matrix(D)
  vector <- as.vector(vector)
  vectorNorm <- (vector-mean(vector, na.rm = T))/sd(vector, na.rm = T)
  DS <- STSSaxEncode(D, vectorNorm, a)
  return (DS)
}

SearchSTMotifs <- function (D,DS,w,a,sb,tb,si=3,ka=3){

  saxblocks <- STSComputeBlocks(DS, tb, sb)
  saxblocks$rectangles <- NULL

  blocks <- STSComputeBlocks(D, tb, sb)
  nrows = blocks$nrows
  ncols = blocks$ncols
  rectangles = blocks$rectangles
  blocks$rectangles <- NULL

  motifs<-list()
  size=length(blocks$datasets)
  for (i in 1:size) {
    block = blocks$datasets[[i]]
    saxblock = saxblocks$datasets[[i]]
    block = as.vector(as.matrix(block))
    saxblock = as.vector(as.matrix(saxblock))
    motifs[[i]] <- identifyMotifsInBlock(ts = block, tss = saxblock, tb = tb ,w = w, a = a)
  }

  stmotifs <- list()
  for (i in 1:length(motifs)) {
    stmotifs <- STSIdentifySTMotif(stmotifs, motifs[[i]], nrows, ncols, rectangles[[i]], ka = ka, si = si)
  }

  sttightmotifs <- list()

  if (length(stmotifs)>0){
    for (i in 1:length(stmotifs)) {
      stmotif = stmotifs[[i]]
      s = stmotif$vecs
      t = stmotif$vect
      stmotif$vecst = data.frame(s, t)
      stmotif$vecs <- NULL
      stmotif$vect <- NULL
      stmotifs[[i]] = stmotif
    }

    for(stmotif in (stmotifs)) {
      sttightmotifsSplit <- STSIdentifyTightSTMotif(stmotif, rectangles)
      for (item in (sttightmotifsSplit)) {
        pos = length(sttightmotifs)+1
        sttightmotifs[[pos]] <- item
        names(sttightmotifs)[pos] = item$isaxcod
      }
    }
  }
  return (sttightmotifs)
}


RankSTMotifs <- function(stmotifs) {
  rstmotifs<-list()
  if(length(stmotifs)>0){
    dataRank <- NULL
    for (i in 1:length(stmotifs)) {
      s <- stmotifs[[i]][["vecst"]][["s"]]
      t <- stmotifs[[i]][["vecst"]][["t"]]
      word <- stmotifs[[i]]$isaxcod
      occurrences<- data.frame(space = s, time = t)
      distance_rank <- comp_distance(occurrences)
      word_rank <- comp_word(stmotifs[[i]]$isaxcod)
      qtd_rank <- log(nrow(occurrences), base=2)
      dataRank <- rbind(dataRank, data.frame(dist = distance_rank, word = word_rank, qtd=qtd_rank))
    }
    rownames(dataRank) <- c(1:length(stmotifs))
    rstmotifs <- rank(dataRank,stmotifs)
  }
  return(rstmotifs)
}



# binning the dataset
# Build an encode for the values
binning <- function(v, a) {
  p <- seq(from = 0, to = 1, by = 1/a)
  q <- quantile(v, p)
  qf <- matrix(c(q[1:(length(q)-1)],q[2:(length(q))]), ncol=2)
  vp <- cut(v, unique(q), FALSE, include.lowest=TRUE)
  m <- tapply(v, vp, mean)
  vm <- m[vp]
  mse <- mean( (v - vm)^2, na.rm = TRUE)
  return (list(binning=m, bins_factor=vp, q=q, qf=qf, bins=vm, mse=mse))
}

# Normalize the data
# Normalize the data using z-score
STSNormalization <- function (vector){
  return ((vector-mean(vector, na.rm = T))/sd(vector, na.rm = T))
}



# Encode values
# Encode numeric values from a vector
STSSaxEncode <- function(dataset, vector, a) {
  mybin <- binning(vector, a)
  myletters <- letters[1:a]
  saxvector <- myletters[mybin$bins_factor]
  saxvector = matrix(saxvector, nrow = nrow(dataset), ncol = ncol(dataset))
  saxvector = data.frame(saxvector)
  colnames(saxvector) =  colnames(dataset)
  return(saxvector)
}

# Compute blocks
# Create blocks from the original dataset
STSComputeBlocks <- function(dataset, tb, sb) {
  datasets <- list()
  rectangles <- list()

  c = ncol(dataset)
  r = nrow(dataset)
  nc = c / sb
  nr = r / tb
  i = 1
  j = 1
  n = 1
  for (i in 1:nc) {
    sc = (i-1)*sb + 1
    ec = (i)*sb
    for (j in 1:nr) {
      sr = (j-1)*tb + 1
      er = (j)*tb
      ds = dataset[sr:er, sc:ec]
      datasets[[n]] = ds
      rect = c(sS = sc, eS = ec, sT = sr, eT = er, nr = j, nc = i)
      rectangles[[n]] = rect
      n = n + 1
    }
  }
  blocks = list(datasets = datasets, nrows = nr, ncols = nc, rectangles = rectangles)
  return (blocks)
}

identifyMotifsInBlock <- function(ts, tss, w, tb , a) {
  #Generation all the possible subsequences
  #ts.sax: a matrix with all the SAX subsequences
  ts.sax <- NULL
  for (i in 1:length(tss)){
    if(floor((i-1)/tb)==floor((i-1+w-1)/tb)){ #Check if it's a fake motif
      ts.sax  <- rbind(ts.sax ,c(i,tss[i:(i+w-1)]) )
    }
  }

  ts.sax <- na.omit(ts.sax)
  ts.sax <- as.data.frame(ts.sax, stringsAsFactors = FALSE)

  colnames(ts.sax) <- c("StartPosition", 1:w)
  ts.sax$StartPosition <- as.numeric(ts.sax$StartPosition)

  #Creating a list with a list of starpPosition of the same motifs
  i = j <- 1
  indices <- list()
  for (i in 1:nrow(ts.sax)){
    saxMotif <- paste(ts.sax[i,-1], collapse = "")
    indices[[saxMotif]] <- c(indices[[saxMotif]],ts.sax[i,1])
  }
  while (j <= length(indices)){ #removing the motif with just 1 or less occurences
    if(length(indices[[j]])<=1){indices[[j]]<-NULL}else{j<-j+1}
  }


  #Each identical sequence is grouping to create a sub matrix of ts.sax
  motif.sax <- NULL
  if (length(indices)>0){
    for (i in 1:length(indices)){
        motif.sax[[i]] <- ts.sax[which(ts.sax[,1] %in% indices[[i]]),]
    }
  }

  return(list(Subs.SAX=ts.sax, Motif.SAX=motif.sax, Indices=indices))
}


STSIdentifySTMotif <- function(stmotifs, motif, nrows, ncols, rectangle, ka, si) {
  k <- length(stmotifs)

  #Get propreties of the block handled
  sS = rectangle["sS"] #startSpatial
  eS = rectangle["eS"] #endSpatial
  sT = rectangle["sT"] #startTime
  eT = rectangle["eT"] #endTime
  nr = rectangle["nr"] #number of the row
  nc = rectangle["nc"] #number of the column

  recMatrix = matrix(rep(0, nrows*ncols), nrow = nrows, ncol = ncols)
  tb <- eT - sT + 1
  sb <- eS - sS + 1
  #for motif discoverd inside the block
  if(length(motif$Indices)>0){ #Check if there is repeated motif found into the block
    for(a in 1:length(motif$Indices)){
      #vectorize the indices of the motif
      vec <- motif$Indices[[a]]

      #BO - Block Occurrences validation
      #check if the number of occurrences into the block is greater or equal to sigma
      if(length(vec) >= si) {
        #scount: vector of 0, with sb columns
        scount <- rep(0, sb)

        #for each occurence of the motif
        for(z in 1: length(vec)) {
          #mark each column wich contains the motif
          i <- as.integer(vec[z] / tb) + 1
          scount[i] <- 1
        }

        #BSO - Block Spatial Occurrences Validation
        #check if the number of columns, into the block, which contains the motif is greater or equal to kappa
        if(sum(scount) >= ka) {
          #take the SAX of the motif
          isaxcod <- paste(motif$Motif.SAX[[a]][1,2:(length(motif$Subs.SAX))], collapse = "")

          vect <- as.integer(vec %% tb) + sT - 1

          vecs <- as.integer(vec / tb) + sS
          i <- match(isaxcod, names(stmotifs))
          if (is.na(i)) {
            k = k + 1
            stmotifs[[k]] <- list(isaxcod=isaxcod, vecs=vecs, vect=vect, recmatrix=recMatrix)
            stmotifs[[k]]$recmatrix[nr, nc] = 1
            names(stmotifs)[k] = isaxcod
          }
          else {
            list <- stmotifs[[i]]
            list$recmatrix[nr, nc] = max(list$recmatrix)+1
            list$vect <- c(list$vect, vect)
            list$vecs <- c(list$vecs, vecs)
            stmotifs[[i]] <- list
          }
        }#Final Block Spatial Occurrences validation
      }#Final Block Occurrences validation
    }
  }
  return (stmotifs)
}

STSIdentifyTightSTMotif <- function(stmotif, rectangles) {
  #We selected one motif with its information
  tight <- list()
  mat <- stmotif$recmatrix #Get the recmatrix of one motif
  vecst <- stmotif$vecst #Get start position of the motif
  #For each block
  for (i in 1:nrow(mat)) {
    for (j in 1:(ncol(mat)-1)) {
      #Checking blocks neighbor if there is a presence of this motif
      if (mat[i,j] != 0) {
        iP <- i + 1
        jP <- j + 1
        if ((iP <= nrow(mat)) && (mat[iP,j] != 0)) {
          k <- min(mat[iP,j], mat[i,j])
          mat[mat == mat[iP,j] | mat == mat[i,j]] = k
        }
        if ((jP <= ncol(mat)) && (mat[i,jP] != 0)) {
          k <- min(mat[i,jP], mat[i,j])
          mat[mat == mat[i,jP] | mat == mat[i,j]] = k
        }
        if ((iP <= nrow(mat)) && (mat[iP,j] != 0) && (jP <= ncol(mat)) && (mat[i,jP] != 0)) {
          k <- min(mat[iP,jP], mat[i,j])
          mat[mat == mat[iP,jP] | mat == mat[i,j]] = k
        }
      }
    }
  }
  vec <- as.vector(mat)
  vec <- vec[vec > 0]
  vec <- unique(vec)
  k <- 1
  stmotif_org <- stmotif
  for (i in (vec)) {
    stmotif <- stmotif_org
    stmotif$recmatrix[mat != i] <- 0
    stmotif$recmatrix[mat == i] <- k
    vecrects <- as.vector(stmotif$recmatrix)
    #Get position of each blocks whitch contains this motif
    rects <- rectangles[vecrects>0]
    stmotif$vecst <- vecst
    conds = rep(FALSE, nrow(stmotif$vecst))
    for (rect in (rects)) {
      sS = rect["sS"]
      eS = rect["eS"]
      sT = rect["sT"]
      eT = rect["eT"]
      conds = conds | (stmotif$vecst$s >= sS & stmotif$vecst$s <= eS & stmotif$vecst$t >= sT & stmotif$vecst$t <= eT)
    }
    stmotif$vecst <- stmotif$vecst[conds,]
    tight[[k]] <- stmotif
    k <- k + 1
  }
  return(tight)
}


plot.series <- function(series, label_series = "", label_x = "", label_y = "") {
  grf <- ggplot(data=series, ggplot2::aes(x = series$x, y = series$value, colour = series$color, group = 1))
  grf <- grf + scale_colour_identity(series$color) + geom_line() + geom_point(data=series, aes(x = series$x, y = series$value), size=0.5) + facet_grid(variable ~ .)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
  return(grf)
}


comp_distance <- function(data) {
  nv <- nrow(data)
  na <- nrow(data)*(nrow(data)-1)/2

  ver <- rep(FALSE, nv)
  adj_mat <- matrix(0, nrow = na, ncol=3)
  k <- 0
  for (i in (1:(nv-1))) {
    for (j in ((i+1):nv)) {
      k <- k + 1
      adj_mat[k, 1] <- i
      adj_mat[k, 2] <- j
      adj_mat[k, 3] <- sqrt((data$space[i]-data$space[j])^2+(data$time[i]-data$time[j])^2)
    }
  }
  adj_mat <- data.frame(s = adj_mat[,1],d = adj_mat[,2], w = adj_mat[,3])
  o <- order(adj_mat$w)
  adj_mat <- adj_mat[o,]

  edges <- NULL
  for (k in 1:na) {
    i <- adj_mat$s[k]
    j <- adj_mat$d[k]
    if (!ver[i] | !ver[j]) {
      ver[i] <- TRUE
      ver[j] <- TRUE
      edges <- rbind(edges, adj_mat[k,])
    }
  }
  return(1/mean(edges$w))
}

comp_word <- function(str) {
  x <- strsplit(str, "^")
  x <- x[[1]]
  n <- length(x)
  x <- table(x)
  x <- x / n
  y <- 0
  for (i in 1:length(x)) {
    y <- y - x[i]*log(x[i],2)

  }
  return(y)
}


normalize.minmax <- function(data, norm.set=NULL)
{
  data = data.frame(data)
  nums = unlist(lapply(data, is.numeric))
  data = data[ , nums]
  
  if(is.null(norm.set))
  {
    minmax = data.frame(t(sapply(data, max, na.rm=TRUE)))
    minmax = rbind(minmax, t(sapply(data, min, na.rm=TRUE)))
    colnames(minmax) = colnames(data)    
    rownames(minmax) = c("max", "min")
  }
  else {
    minmax = norm.set
  }
  for (i in 1:ncol(data))
    data[,i] = (data[,i] - minmax["min", i]) / (minmax["max", i] - minmax["min", i])
  return (list(data=data, norm.set=minmax))
}


rank <- function(dataRank,stmotifs)
{
  dataRankOrg <- dataRank
  
  dataRank <- normalize.minmax(dataRank)$data

  dataRank = as.matrix(dataRank)

  transf<- rep(sqrt(0.5),ncol(dataRank))

  dataRankOrg$proj = dataRank %*% transf

  #order
  o <- order(dataRankOrg$proj, decreasing=TRUE)
  stmotifsRank <- list()
  for (i in 1:length(stmotifs)) {
    indice <- o[i]
    stmotifs[[indice]][["rank"]] <- c(dataRankOrg[indice,]['dist'], dataRankOrg[indice,]['word'], dataRankOrg[indice,]['qtd'], dataRankOrg[indice,]['proj'])
    stmotifsRank[[i]] <- stmotifs[[indice]]
  }
  return (stmotifsRank)
}




