plot.2dsn <- function(testing.value.out.working,Par1,Par2,cexticklabel=0.8,DoDiff=F,DoValue=F,fraction.notesting.winner=0.5) {
  
  
  tbl.tmp = table(testing.value.out.working$BestStrategy,testing.value.out.working[,Par1],testing.value.out.working[,Par2],dnn=c('Strategy',Par1,Par2))
  
  #value.central.tmp = as.data.frame(testing.value.out.working %>% dplyr::filter(BestStrategy=='central') %>% dplyr::group_by((!!as.symbol(Par1)),(!!as.symbol(Par2))) %>% summarise(value=sum(central.strategy.value)) %>% ungroup())
  #value.local.tmp = as.data.frame(testing.value.out.working %>% dplyr::filter(BestStrategy=='local') %>% dplyr::group_by((!!as.symbol(Par1)),(!!as.symbol(Par2))) %>% summarise(value=sum(local.strategy.value)) %>% ungroup())

  value.central.all.tmp = as.data.frame(testing.value.out.working %>% dplyr::group_by((!!as.symbol(Par1)),(!!as.symbol(Par2))) %>% dplyr::summarise(value=mean(central.strategy.value)) %>% ungroup())
  value.local.all.tmp = as.data.frame(testing.value.out.working %>% dplyr::group_by((!!as.symbol(Par1)),(!!as.symbol(Par2))) %>% dplyr::summarise(value=mean(local.strategy.value)) %>% ungroup())

  #value.tmp = as.data.frame(testing.value.out.working %>% dplyr::group_by(BestStrategy,(!!as.symbol(Par1)),(!!as.symbol(Par2))) %>% summarise(value=mean(central.strategy.value)) %>% ungroup())
  
  mymap = matrix(NA, dim(tbl.tmp)[2], dim(tbl.tmp)[3])
  d1.ticklabel = dimnames(tbl.tmp)[2]
  rownames(mymap) = d1.ticklabel[[1]]
  d2.ticklabel = dimnames(tbl.tmp)[3]
  colnames(mymap) = d2.ticklabel[[1]]
  d1.axislabel = names(d1.ticklabel)[[1]]
  d2.axislabel = names(d2.ticklabel)[[1]]
  mymap.num = mymap
  mymap.diff = mymap
  mymap.value = mymap
  mymap.value.winner = mymap
  for (i in c(1:dim(mymap)[1])) {
    for (j in c(1:dim(mymap)[2])) {
      #mymap[i, j] = names(which(tbl.tmp[, i, j] == max(tbl.tmp[, i, j])))[1]  # if there is a tie, choosing first
      mymap[i, j] = rownames(tbl.tmp)[which(tbl.tmp[, i, j] == max(tbl.tmp[, i, j]))[1]]  # if there is a tie, choosing first
      mymap.num[i, j] = tbl.tmp[, i, j][which(tbl.tmp[, i, j] == max(tbl.tmp[, i, j]))][1]  # if there is a tie, choosing first
      best = sort(tbl.tmp[, i, j], decreasing = T)[1]
      nextbest = sort(tbl.tmp[, i, j], decreasing = T)[2]
      mymap.diff[i, j] = best - nextbest
      
      indices.tmp = value.local.all.tmp[,Par1]==unlist(dimnames(tbl.tmp)[Par1])[i] & value.local.all.tmp[,Par2]==unlist(dimnames(tbl.tmp)[Par2])[j]
      if (sum(indices.tmp)>0) {
        local.value = value.local.all.tmp$value[indices.tmp]
      } else {
        local.value = 0
      }
      indices.tmp = value.central.all.tmp[,Par1]==unlist(dimnames(tbl.tmp)[Par1])[i] & value.central.all.tmp[,Par2]==unlist(dimnames(tbl.tmp)[Par2])[j]
      if (sum(indices.tmp)>0) {
        central.value = value.central.all.tmp$value[indices.tmp]
      } else {
        central.value = 0
      }
      if (local.value>central.value) mymap.value.winner[i,j] = 'local'
      if (local.value<=central.value) mymap.value.winner[i,j] = 'central'
      
      if (mymap[i,j]=='no testing' & mymap.num[i,j]/sum(tbl.tmp[, i, j])>fraction.notesting.winner) mymap.value.winner[i,j] = 'no testing'
      
      if (mymap.value.winner[i,j] == 'local') mymap.value[i,j] = round(local.value-central.value,2)
      if (mymap.value.winner[i,j] == 'central') mymap.value[i,j] = round(central.value-local.value,2)
      if (mymap.value.winner[i,j] == 'no testing') mymap.value[i,j] = 0
      
    }
  }
  
  col.local = get.laurels.colors(2)[2]
  col.central = get.laurels.colors(3)[2]
  col.notesting = get.laurels.colors(4)[2]
  
  if (DoDiff == T) {
    mymap.tmp = mymap.diff 
    mymap.label.tmp = mymap
  } else if (DoValue == T) {
    mymap.tmp = mymap.value
    mymap.label.tmp = mymap.value.winner
  } else {
    mymap.tmp = mymap.num
    mymap.label.tmp = mymap
  }
  
  
  mymap.central = mymap.tmp
  mymap.central[mymap.label.tmp != 'central'] = NA
  mymap.local = mymap.tmp
  mymap.local[mymap.label.tmp != 'local'] = NA
  mymap.notesting = mymap.tmp
  mymap.notesting[mymap.label.tmp != 'no testing'] = NA
  
  
  coulfun <-
    colorRampPalette(c(brewer.pal(8, "Blues")[2], brewer.pal(8, "Blues")[8]))
  coul.central <- coulfun(10)
  coulfun <-
    colorRampPalette(c(brewer.pal(8, "Greens")[2], brewer.pal(8, "Greens")[8]))
  coul.local <- coulfun(10)
  coulfun <-
    colorRampPalette(c(brewer.pal(8, "Greys")[2], brewer.pal(8, "Greys")[8]))
  coul.notesting <- coulfun(10)
  
  dims = dim(mymap.central)
  if (dims[1]>1) {
    mymap.central <- as.matrix(apply(mymap.central, 2, rev))
    mymap.local <- as.matrix(apply(mymap.local, 2, rev))
    mymap.notesting <- as.matrix(apply(mymap.notesting, 2, rev))
  }
  mymap.init <- mymap.central
  mymap.init[,] <- 0
  par(mar=c(5.1,4.1,4.1,2.1))
  #par(mar=c(7.1,6.1,4.1,2.1))
  if (is.null(dim(mymap.init))) {
    dim1 = 1
    dim2 = length(mymap.init)
  } else {
    dim1 = dim(mymap.init)[1]
    dim2 = dim(mymap.init)[2]
  }
  if (DoDiff) {
    maintitle = 'Incremental trials preferred: Blue=central; Green=local; Grey=no testing'
  } else if (DoValue) {
    maintitle = 'Incremental value of preferred: Blue=central; Green=local; Grey=no testing'
  } else {
    maintitle = 'Absolute trials preferred: Blue=central; Green=local; Grey=no testing'
  }
  image(
    1:dim2,
    1:dim1,
    t(mymap.init),
    col = 'white',
    axes = F,
    xlab = d2.axislabel,
    ylab = d1.axislabel,
    main = maintitle,
    cex.main = 0.8
  )
  # if ((sum(!is.na(mymap.local))>0))
  # image(
  #   1:dim(mymap.central)[2],
  #   1:dim(mymap.central)[1],
  #   t(mymap.central),
  #   col = coul.central,
  #   axes = F,
  #   xlab = d2.axislabel,
  #   ylab = d1.axislabel,
  #   main = 'Blue=central; Green=local; Grey=no testing',
  #   cex.main = 0.8
  # )
  if (sum(!is.na(mymap.central))>0) {
    image(
      1:dim(mymap.central)[2],
      1:dim(mymap.central)[1],
      t(mymap.central),
      add = T,
      col = coul.central
    )
  }
  if (sum(!is.na(mymap.local))>0) {
    image(
      1:dim(mymap.central)[2],
      1:dim(mymap.central)[1],
      t(mymap.local),
      add = T,
      col = coul.local
    )
  }
  if (sum(!is.na(mymap.notesting))>0) {
    image(
      1:dim(mymap.central)[2],
      1:dim(mymap.central)[1],
      t(mymap.notesting),
      add = T,
      col = coul.notesting
    )
  }
  
  if (dim(mymap.central)[1]==1) {
    pos1 = 0.6
  } else { 
  pos1 = 0.5
  }
  if (dim(mymap.central)[2]==1) {
    pos2 = 0.6
  } else { 
    pos2 = 0.5
    }
  
  
  axis(
    1,
    at = c(1:dim(mymap.central)[2]),
    labels = d2.ticklabel[[1]],
    lwd = 0,
    pos = pos1,
    cex.axis = cexticklabel,
    outer=F
  )
  axis(
    2,
    at = c(dim(mymap.central)[1]:1),
    labels = d1.ticklabel[[1]],
    lwd = 0,
    pos = pos2,
    cex.axis = cexticklabel
  )
  e <-
    expand.grid(c(dim(mymap.central)[1]:1), 1:c(dim(mymap.central)[2]))
  e = e[, c(2, 1)]
  text(e, labels = mymap.tmp, col = 'black')
}
