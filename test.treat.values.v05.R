test.treat.values  <- function(NetBenefitTx,CostTx,sn,sp,priors,cTest=0,lostValue=0,value.lost.altmethod=F) {

  ## Get the utilities of treat and no-treat strategies
  tt.lut = data.frame(Pretest=priors$pretestprob) #Create a df with 1 column with the range of pretest probabilities
  tt.lut$TxUtility = tt.lut$Pretest*NetBenefitTx #values of the NO TREATMENT disutility curve #the probability of having the disease * the Net Benefit of Treatment (expected value of treatment - expected costs of treatment) #if you don't treat someone with 0% probability of disease, then you get no disutility from withholding treatment; if you don't treatsome with 100% probability of disease, you get disutility = net benefit of treatment
  tt.lut$NoTxUtility = (1-tt.lut$Pretest)*CostTx #values of the treatment disutility curve #the probability of not having the disease * the net cost of treatment ()
  tt.lut$MyPretestDist = 0 #create blank df variable
  tt.lut$MyPretestDist[which(tt.lut$Pretest %in% priors$pretestprob)] = priors$num #add in the number of patients for each pretest probability # in case the priors are just a subset of the tt.lut$Pretest
  tt.lut$BestStrategyNoTest = rowMaxs(as.matrix(tt.lut[,c('NoTxUtility','TxUtility')])) #take the best strategy, not considering testing
  tt.lut$BestStrategyNoTestUtility = tt.lut$BestStrategyNoTest*tt.lut$MyPretestDist #calculate the sum of the disutility, not considering testing

  ## Get the testing utility  
  if (value.lost.altmethod) {
    NetBenefitTxAdjusted = (1-lostValue)*NetBenefitTx
    tt.lut$TestUtility = tt.lut$Pretest*sn*(NetBenefitTxAdjusted-cTest) + (1-tt.lut$Pretest)*(1-sp)*(-cTest) + tt.lut$Pretest*(1-sn)*(-cTest) + (1-tt.lut$Pretest)*sp*(CostTx - cTest)
    tt.lut$BestStrategy = rowMaxs(as.matrix(tt.lut[,c('NoTxUtility','TxUtility','TestUtility')])) #take the value of the utility for the strategy with the lowest disutility at each pretest probability
    tt.lut$BestStrategyUtility = tt.lut$BestStrategy*tt.lut$MyPretestDist #calculate the sum of the disutility by multiplying disutility for the best strategy by the number of patients at that strategy
    BestStrategyUtility = sum(tt.lut$BestStrategyUtility) #sum of the utility for all patients when testing is a strategy
    BestStrategyNoTestUtility = sum(tt.lut$BestStrategyNoTestUtility) #sum of the utility for all patients when testing is not a strategy
    BestStrategyIncrementalUtilityTesting = BestStrategyUtility-BestStrategyNoTestUtility #find the difference when testing is an option
  } else {
    tt.lut$TestUtility = tt.lut$Pretest*sn*(NetBenefitTx-cTest) + (1-tt.lut$Pretest)*(1-sp)*(-cTest) + tt.lut$Pretest*(1-sn)*(-cTest) + (1-tt.lut$Pretest)*sp*(CostTx - cTest)
    tt.lut$BestStrategy = rowMaxs(as.matrix(tt.lut[,c('NoTxUtility','TxUtility','TestUtility')])) #take the value of the utility for the strategy with the lowest disutility at each pretest probability
    tt.lut$BestStrategyUtility = tt.lut$BestStrategy*tt.lut$MyPretestDist #calculate the sum of the disutility by multiplying disutility for the best strategy by the number of patients at that strategy
    BestStrategyUtility.tmp = sum(tt.lut$BestStrategyUtility) #sum of the utility for all patients when testing is a strategy
    BestStrategyNoTestUtility = sum(tt.lut$BestStrategyNoTestUtility) #sum of the utility for all patients when testing is not a strategy
    BestStrategyIncrementalUtilityTesting.adj.tmp = (1-lostValue)*(BestStrategyUtility.tmp-BestStrategyNoTestUtility) #find the difference when testing is an option
    BestStrategyUtility = BestStrategyIncrementalUtilityTesting.adj.tmp + BestStrategyNoTestUtility
    BestStrategyIncrementalUtilityTesting = BestStrategyUtility-BestStrategyNoTestUtility #find the difference when testing is an option
  }
  
  NumTestStrategy = sum(tt.lut$MyPretestDist[tt.lut$BestStrategy==tt.lut$TestUtility]) #Num initially tested when testing an option
  NumTxStrategy = sum(tt.lut$MyPretestDist[tt.lut$BestStrategy==tt.lut$TxUtility]) #Num initially treated when when testing an option
  NumNoTxStrategy = sum(tt.lut$MyPretestDist[tt.lut$BestStrategy==tt.lut$NoTxUtility]) #num initially not treated when testing an option
  indices.tested = which(tt.lut$BestStrategy==tt.lut$TestUtility) #Pre test probabilities where the best strategy is to test
  indices.tx = which(tt.lut$BestStrategy==tt.lut$TxUtility) #Pre test probabilities where the best strategy is to treat
  indices.notx = which(tt.lut$BestStrategy==tt.lut$NoTxUtility) #Pre test probabilities where the best strategy is to not treat
  numTestedWithDz = sum(tt.lut$MyPretestDist[indices.tested]*tt.lut$Pretest[indices.tested]) #num of people tested who had the disease (pretest prop * number of people)
  numTestedWithoutDz = sum(tt.lut$MyPretestDist[indices.tested]*(1-tt.lut$Pretest[indices.tested])) #num of people tested who did not have the disease (1 - pretest prop * number of people)
  numTxWithDz = sum(tt.lut$MyPretestDist[indices.tx]*tt.lut$Pretest[indices.tx]) #num of people treated who actually had the disease (pretest prop * number of people)
  numTxWithoutDz = sum(tt.lut$MyPretestDist[indices.tx]*(1-tt.lut$Pretest[indices.tx])) #num of people treated who did not have the disease (1 - pretest prop * number of people)
  numNoTxWithoutDz = sum(tt.lut$MyPretestDist[indices.notx]*(1-tt.lut$Pretest[indices.notx])) #num of people treated who did not have the disease (1 - pretest prop * number of people)
  numNoTxWithDz = sum(tt.lut$MyPretestDist[indices.notx]*(tt.lut$Pretest[indices.notx])) #num of people not treated who had the disease (pretest prop * number of people)
  
  TxTP = numTestedWithDz*sn + numTxWithDz 
  TxFP = numTestedWithoutDz*(1-sp) + numTxWithoutDz
  NoTxTN = numTestedWithoutDz*sp + numNoTxWithoutDz
  NoTxFN = numTestedWithDz*(1-sn) + numNoTxWithDz
  NumTx = TxTP + TxFP
  NumNoTx = NoTxTN + NoTxFN
  NumEngaged = TxTP + TxFP + NoTxTN + NoTxFN
  out = data.frame(NumEngaged=NumEngaged,NumTests=NumTestStrategy,NumTxStrategy=NumTxStrategy,NumNoTxStrategy=NumNoTxStrategy,NumTx=NumTx,NumNoTx=NumNoTx,TxTP=TxTP,TxFP=TxFP,NoTxFN=NoTxFN,NoTxTN=NoTxTN,BestStrategyCost=BestStrategyUtility,BestStrategyNoTestCost=BestStrategyNoTestUtility,BestStrategyValueTesting=BestStrategyIncrementalUtilityTesting)  
  return(out)
}








