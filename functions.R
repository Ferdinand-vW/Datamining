bestsplit <- function(x,y,minleaf) {
  sorted <- sort(unique(x))
  splitpoints <- (sorted[1:length(sorted) - 1] + sorted[2:length(sorted)]) / 2
  
  split <- foldSplits(x,y,splitpoints,minleaf)
  c(split,computesplit(x,y,split,minleaf))
}

foldSplits <- function(x,y,splits,minleaf) {
  Reduce(function(a,b) {
    asplit <- computesplit(x,y,a,minleaf)
    bsplit <- computesplit(x,y,b,minleaf)
    
    if(!is.na(asplit) && !is.na(bsplit)) {
      if(asplit > bsplit) {
        a
      }
      else {
        b
      }
    }
    else if(!is.na(asplit)){
      a
    }
    else if(!is.na(bsplit)){
      b
    }
    else {
      NA
    }
  }, splits, NA)
}

computesplit <- function(x, y, val,minleaf) {
  
  if(!is.na(val)) {
    lchild <- y [x <= val]
    rchild <- y [x > val]
    
    if(!canbeSplit(lchild, rchild,minleaf)) {
      NA
    }
    else {
      valOfChild(y,length(y)) - (valOfChild(lchild, length(x)) + valOfChild(rchild, length(x)))
    }
  }
  else {
    NA
  }
}

canbeSplit <- function(l, r,minleaf) {
  length(l) >= minleaf && length(r) >= minleaf
}

valOfChild <- function(x, n) {
  num <- length(x) / n
  pGood <- sum(x) / length(x)
  pBad <- length(x[x==0]) / length(x)
  
  num * pGood * pBad
}