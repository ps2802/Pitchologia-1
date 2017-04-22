# returns all combinations of chars in the string 'itemsets'
# if size provided, returns only the combinations with that size

allCombinations <- function (itemsets, size=NULL) {
  for (v in itemsets) {
    charArr <- strsplit(v, NULL)[[1]]
    len <- length(charArr)    
    if (len > 1) {
      for (combcounter in c(1:len - 1)) {
        m_combs <- combn(charArr, combcounter)
        v_combs <- apply(m_combs, 2, paste, collapse="")
        itemsets <- c(itemsets, v_combs)
      }
    }
    itemsets <- unique(itemsets)
  }
  if (!is.null(size)) {
    lenSize <- function (x) {
      nchar(x) == size
    }
    lenSize <- Vectorize(lenSize)
    itemsets <- itemsets[lenSize(itemsets)]
  }
  itemsets
}


#returns the statistic value used for comparing the
# similarity and diversity of sample sets (Jaccard index).

jaccard <- function(M, col1, col2) {
  if (is.data.frame(M)) {
    M <- data.matrix(M)
  }
  sums = rowSums(M[, c(col1, col2)])
  
  similarity = length(sums[sums==2])
  total = length(sums[sums==1]) + similarity
  
  similarity/total
}

#example#
a <- c(1,1,1,1,0,0,0,0,0,0)
b <- c(0,1,0,0,1,0,0,1,0,1)
c <- c(0,0,0,0,0,1,1,1,1,0)
d <- c(0,1,1,1,1,1,1,1,1,1)
e <- c(1,0,1,1,1,1,1,1,1,1)

M <- matrix(numeric(0), 0, length(a))
M <- rbind(M, a)
M <- rbind(M, b)
M <- rbind(M, c)
M <- rbind(M, d)
M <- rbind(M, e)
M <- t(M)

# combinations of pairs
combs <- expand.grid(1:dim(M)[2], 1:dim(M)[2])
combs <- combs[combs[,2]>combs[,1] , ]
combs <- matrix(unlist(combs, use.names=FALSE),ncol=length(combs))

jlist <- rep(0, dim(combs)[1])

for (i in 1:dim(combs)[1]) {
  jlist[i] <- (1 - jaccard(M, combs[i,1], combs[i,2]))
}

