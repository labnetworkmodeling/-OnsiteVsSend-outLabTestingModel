#clear workspace
rm(list=ls())
 
############ Load Packages ##############
needed_packages <- c("tidyverse", "matrixStats", "readxl", "ggplot2", "RColorBrewer", "rpart", "rpart.plot", "rattle", "caret", "uniformly",
                     "triangle", "hrbrthemes", "corrplot", "progress", "janitor", "table1", "flextable", "tinytex", "knitr", "kableExtra", "ddpcr", "rlang",
                     "ceplot","magrittr","reshape2","purrr","stringr", "remotes")
missing_packages <- needed_packages[!(needed_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages((missing_packages))
lapply(needed_packages, require, character.only=TRUE)
#remotes::install_github("n8thangreen/plotCostEffectiveness")

############# Install Latex ################
if(!tinytex::is_tinytex()) tinytex::install_tinytex()

############# Import Source Files ####################
source('get.many.colors.R', encoding = 'UTF-8')
source('add.alpha.R', encoding = 'UTF-8')
source('get.laurels.colors.R', encoding = 'UTF-8')
source('get.pastel.R', encoding = 'UTF-8')
source('plot.2dsn.R', encoding = 'UTF-8')
source('test.treat.values.V05.R')


################## SET PARAMETER VALUES ####################
psa = 2  # 1: PSA by interval; 2: PSA by sampling distributions
numRuns = 10000
value.lost.altmethod = T  # the alt method decreases net benefit of treatment by a fraction

#disutility.curve.selection <- "i == 100000000"

## sensitivity analysis
set.seed(38)
parameter.inputs = read.table('parametersinput.csv',sep=',',header=T,stringsAsFactors = F)
whichmodel = 'gen' ## to select the appropriate column name in parameter.inputs
whichmodel = 'tb' ## to select the appropriate column name in parameter.inputs

low_col <- paste0('low_', whichmodel)
high_col <- paste0('high_', whichmodel)

total.suspected.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='total.suspected'),all_of(low_col),all_of(high_col)))
fraction.suspected.with.disease.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='fraction.suspected.with.disease'),all_of(low_col),all_of(high_col)))
test.cost.central.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='test.cost.central'),all_of(low_col),all_of(high_col)))
test.cost.local.capital.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='test.cost.local.capital'),all_of(low_col),all_of(high_col)))
specimen.transport.cost.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='specimen.transport.cost'),all_of(low_col),all_of(high_col)))
CostTx.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='CostTx'),all_of(low_col),all_of(high_col)))
NetBenefitTx.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='NetBenefitTx'),all_of(low_col),all_of(high_col)))
central.sn.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='central.sn'),all_of(low_col),all_of(high_col)))
central.sp.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='central.sp'),all_of(low_col),all_of(high_col)))
local.sn.dec.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='local.sn.dec'),all_of(low_col),all_of(high_col)))
local.sp.dec.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='local.sp.dec'),all_of(low_col),all_of(high_col)))
valueLostCentral.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='valueLostCentral'),all_of(low_col),all_of(high_col)))
valueLostLocal.space = as.numeric(dplyr::select(dplyr::filter(parameter.inputs,parameter=='valueLostLocal'),all_of(low_col),all_of(high_col)))

## Originally, we included a model for sendout testing that led to a delay, and we compared that to a 'time until the test was worthless', assuming linear descent
#TimeUntilWorthless.space = round(seq(0.5,60,length.out=4),1)  #days  7   3  60    -16.99  12.42  (29.41)
#travel.time.to.central.space = round(seq(0.5/24,12/24,length.out=4),2)  # days 5/24  1/24  8/24    -8.2  2.7  (10.9)
#batch.shipment.cost.space = round(seq(3,50,length.out=4),2)
#CentralTAT.space = round(seq(1,10,length.out=4),2)  # days  2 1 10

start.time = Sys.time()
myseed = 40
set.seed(myseed)
for(j in 1:4000) {

  ###### paramater df for classification/regression
  parameter.space = data.frame(runNum = 1:numRuns)
  parameter.space$total.suspected = runif(numRuns,total.suspected.space[1],max=total.suspected.space[length(total.suspected.space)])
  parameter.space$fraction.suspected.with.disease = runif(numRuns,fraction.suspected.with.disease.space[1],fraction.suspected.with.disease.space[length(fraction.suspected.with.disease.space)])
  parameter.space$test.cost.central = runif(numRuns,test.cost.central.space[1],test.cost.central.space[length(test.cost.central.space)])
  parameter.space$test.cost.local.capital = runif(numRuns,test.cost.local.capital.space[1],test.cost.local.capital.space[length(test.cost.local.capital.space)])
  parameter.space$specimen.transport.cost = runif(numRuns,specimen.transport.cost.space[1],specimen.transport.cost.space[length(specimen.transport.cost.space)])
  parameter.space$CostTx = sample(CostTx.space[1] + (CostTx.space[length(CostTx.space)]-CostTx.space[1])* seq(0,1,0.01)^2,numRuns,replace=T)
  parameter.space$NetBenefitTx = sample(NetBenefitTx.space[1] + (NetBenefitTx.space[length(NetBenefitTx.space)]-NetBenefitTx.space[1])* seq(0,1,0.01)^2,numRuns,replace=T)
  parameter.space$CentralSn = runif(numRuns,central.sn.space[1],central.sn.space[length(central.sn.space)])
  parameter.space$CentralSp = runif(numRuns,central.sp.space[1],central.sp.space[length(central.sp.space)])
  parameter.space$LocalSnDec = runif(numRuns,local.sn.dec.space[1],local.sn.dec.space[length(local.sn.dec.space)])
  parameter.space$LocalSpDec = runif(numRuns,local.sp.dec.space[1],local.sp.dec.space[length(local.sp.dec.space)])
  parameter.space$valueLostCentral = runif(numRuns,valueLostCentral.space[1],valueLostCentral.space[length(valueLostCentral.space)])
  parameter.space$valueLostLocal = runif(numRuns,valueLostLocal.space[1],valueLostLocal.space[length(valueLostLocal.space)])
  #parameter.space$TimeUntilWorthless = runif(numRuns,TimeUntilWorthless.space[1],TimeUntilWorthless.space[length(TimeUntilWorthless.space)])
  #parameter.space$travel.time.to.central = runif(numRuns,travel.time.to.central.space[1],travel.time.to.central.space[length(travel.time.to.central.space)])
  #parameter.space$batch.shipment.cost = runif(numRuns,batch.shipment.cost.space[1],batch.shipment.cost.space[length(batch.shipment.cost.space)])
  #parameter.space$CentralTAT = runif(numRuns,CentralTAT.space[1],CentralTAT.space[length(CentralTAT.space)])
  

  ################### Build Blank Outcome DF ########################
  outcome.variables.list = c(
    'Index','local.strategy.value','central.strategy.value','local.less.central.value',
    "NumEngaged_local","NumTests_local","NumTxStrategy_local","NumNoTxStrategy_local","NumTx_local","NumNoTx_local","TxTP_local",
    "TxFP_local","NoTxFN_local","NoTxTN_local","BestStrategyCost_local","BestStrategyNoTestCost_local",
    "BestStrategyValueTesting_local","PerTestCostLocal",
    "NumEngaged_central","NumTests_central","NumTxStrategy_central","NumNoTxStrategy_central","NumTx_central","NumNoTx_central","TxTP_central",
    "TxFP_central","NoTxFN_central","NoTxTN_central","BestStrategyCost_central","BestStrategyNoTestCost_central",
    "BestStrategyValueTesting_central","PerTestCostCentral","SpecimenTransportCost",
    "fraction.suspected.with.disease","test.cost.central","test.cost.local.capital",
    "CostTx","NetBenefitTx","CentralSn","CentralSp", "LocalSnDec", "LocalSpDec",
    "LocalSn", "LocalSp", "ValueLostLocal", "ValueLostCentral", "TotalSuspected"
  )
  

  run_model <- function(parameter.df, numRuns, heatmap = FALSE) {
    pb <- progress_bar$new(total = 100)
    modelruntime <- format(Sys.time(), '%Y%m%d %H%M')
    testing.value.out = data.frame(matrix(NA,numRuns,length(outcome.variables.list)))
    colnames(testing.value.out) = outcome.variables.list
    for (i in 1:numRuns) {
      total.suspected = parameter.df$total.suspected[i]
      fraction.suspected.with.disease = parameter.df$fraction.suspected.with.disease[i]
      travel.time.to.central = parameter.df$travel.time.to.central[i]  # in days
      test.cost.central = parameter.df$test.cost.central[i]
      CostTx = parameter.df$CostTx[i]
      NetBenefitTx = parameter.df$NetBenefitTx[i]
      diag.sn.central = parameter.df$CentralSn[i]
      diag.sp.central = parameter.df$CentralSp[i]
      local.sn.dec = parameter.df$LocalSnDec[i]
      local.sp.dec = parameter.df$LocalSpDec[i]
      specimen.transport.cost = parameter.df$specimen.transport.cost[i]
      test.cost.local.capital = parameter.df$test.cost.local.capital[i]
      valueLostCentral = parameter.df$valueLostCentral[i]
      valueLostLocal = parameter.df$valueLostLocal[i]
      #TimeUntilWorthless = parameter.df$TimeUntilWorthless[i]
      #central.lab.tat = parameter.df$CentralTAT[i]  #receive until reporting results
      #batch.shipment.cost = parameter.df$batch.shipment.cost[i]
      
      ### calculate the priors (this could be a distribution but is currently a single value for all patients)
      prior.dist = rep(fraction.suspected.with.disease,total.suspected)
      prior.hist.out=hist(prior.dist,breaks=seq(0,1,.01),plot=F) #create a historgram for the pretest probability in 1% buckets, don't graph it
      #sum(prior.dist)
      priors = data.frame(num=prior.hist.out$counts,pretestprob=prior.hist.out$mids)
      
      diag.sn.local = diag.sn.central - local.sn.dec
      diag.sp.local = diag.sp.central - local.sp.dec
      
      ## Details of the prior model to calculate turn-around time and thus value lost due to delays
      #local strategy turnaround time; set as a constant
      #tat.local.strategy = 2/24  # days (2 hours)
      #valueLostLocal = tat.local.strategy/TimeUntilWorthless #sets the value lost as a linear function
      
      # Specimen transport parameters
      ## Specimen transport time and costs
      # 1. Batched shipment at some frequency (daily, weekly) that depends on time until useless: 
      #       0 to 1 week (daily), 1 to 3 weeks (twice week), > 3 weeks (once per week); if within 50 miles, then 0.5 day courier service
      #    Cost of shipment not dependent on distance ($5 per delivery in all cases)
      #    Delay between 0.5 and 3 days depending on drive time (<1 hour,1-5, 5-10, >10 )
      
      ## determine without any specimen transport cost first, then iteratively calculate specimen transport cost (it depends on how many tests are sent)

      #  pertest.cost.central = test.cost.central  
      
      ## determine how much extra time will be added by specimen transport and batching  
      # travel.time.to.central.zones = c(1/24,5/25,10/24)  # days; this determines how slow the actual shipping is (4 zones, 3 breakpoints)
      # specimen.transport.time.added.zones = c(2/24,1,2,3)  # days;  delivery times based on how far away (travel.time.to.central.zones)
      # TimeUntilWorthless.zones = c(1,7,30)  # days; these characterize importance of urgency for different tests
      # #specimen.transport.time.added = travel.time.to.central*4  # same units as TimeUntilWorthless
      # if (TimeUntilWorthless <= TimeUntilWorthless.zones[1]) {
      #   batched.per.week = 14 # batch twice per day
      # } else if (TimeUntilWorthless > TimeUntilWorthless.zones[1] & TimeUntilWorthless <= TimeUntilWorthless.zones[2]) {
      #   batched.per.week = 7 # batch once per day
      # } else if (TimeUntilWorthless > TimeUntilWorthless.zones[2] & TimeUntilWorthless <= TimeUntilWorthless.zones[3]) {
      #   batched.per.week = 2 # batch twice per week
      # } else if (TimeUntilWorthless > TimeUntilWorthless.zones[3]) {
      #   batched.per.week = 1 # batch once per week
      # }
      # if (travel.time.to.central <= travel.time.to.central.zones[1]) {
      #   specimen.transport.time.added = specimen.transport.time.added.zones[1] + 7/(2*batched.per.week)  # days
      # } else if (travel.time.to.central <= travel.time.to.central.zones[2]) {
      #   specimen.transport.time.added = specimen.transport.time.added.zones[2] + 7/(2*batched.per.week)  # days
      # } else if (travel.time.to.central <= travel.time.to.central.zones[3]) {
      #   specimen.transport.time.added = specimen.transport.time.added.zones[3] + 7/(2*batched.per.week)  # days
      # } else {
      #   specimen.transport.time.added = specimen.transport.time.added.zones[4] + 7/(2*batched.per.week)  # days
      # }
      # 
      # tat.central.strategy = specimen.transport.time.added + central.lab.tat
      #valueLostCentral = tat.central.strategy/TimeUntilWorthless
      
      #### Run for Central strategy 
      ## run twice, to figure out the num tests, which impacts specimen transport costs per test
      # out.central.pre=test.treat.values(NetBenefitTx,CostTx,diag.sn.central,diag.sp.central,priors,cTest=pertest.cost.central,lostValue=valueLostCentral)
      # 
      # 
      # specimen.transport.batchdelivery = batch.shipment.cost  # cost per delivery
      # if (out.central.pre$NumTests > 0) {
      #   num.tests.per.delivery = out.central.pre$NumTests/(batched.per.week*52)   
      #   if (num.tests.per.delivery<1) {
      #     specimen.transport.cost = specimen.transport.batchdelivery  # assume 1 specimen sent per delivery
      #   } else {
      #     specimen.transport.cost = specimen.transport.batchdelivery/num.tests.per.delivery
      #   }
      #   pertest.cost.central = test.cost.central+specimen.transport.cost  
      # } else {
      #   specimen.transport.cost = specimen.transport.batchdelivery
      #   pertest.cost.central = test.cost.central+specimen.transport.batchdelivery
      #   num.tests.per.delivery = 0
      # }

      pertest.cost.central = test.cost.central+specimen.transport.cost 
      
      
      out.central=test.treat.values(NetBenefitTx,CostTx,diag.sn.central,diag.sp.central,priors,cTest=pertest.cost.central,lostValue=valueLostCentral,value.lost.altmethod=value.lost.altmethod)
      if (out.central$BestStrategyValueTesting > 0) {
        if (value.lost.altmethod) {
          central.strategy.value = out.central$BestStrategyCost/out.central$NumEngaged
        } else {
          central.strategy.value = out.central$BestStrategyValueTesting/out.central$NumEngaged  ## currently this can be negative, but since it's never the best, then it gets set to zero by the last if
        }
        # not sure how to penalize the testing strategy when it doesn't make sense to test 
      } else {
        central.strategy.value = 0  # this should be the case that out.local$BestStrategyValueTesting = 0
      }
      
      #### Run for local strategy 
      # run twice, determine cost of tests, given the volume
      out.local.pre=test.treat.values(NetBenefitTx,CostTx,diag.sn.local,diag.sp.local,priors,cTest=test.cost.central,lostValue=valueLostLocal,value.lost.altmethod=value.lost.altmethod)  # run to get initial estimate of num tests to determine price using operational case only
      if (out.local.pre$NumTests>0) {
        pertest.cost.local = (test.cost.local.capital + out.local.pre$NumTests*test.cost.central)/out.local.pre$NumTests
      } else {
        pertest.cost.local = test.cost.local.capital + test.cost.central # basically, cost of doing 1 test
      }  
      
      out.local=test.treat.values(NetBenefitTx,CostTx,diag.sn.local,diag.sp.local,priors,cTest=pertest.cost.local,lostValue=valueLostLocal,value.lost.altmethod=value.lost.altmethod)

      if (out.local$BestStrategyValueTesting > 0) {
        if (value.lost.altmethod) {
          local.strategy.value = out.local$BestStrategyCost/out.local$NumEngaged
        } else {
          local.strategy.value = out.local$BestStrategyValueTesting/out.local$NumEngaged  ## currently this can be negative, but since it's never the best, then it gets set to zero by the last if
        }
        # not sure how to penalize the testing strategy when it doesn't make sense to test 
      } else {
        local.strategy.value = 0  # this should be the case that out.local$BestStrategyValueTesting = 0
      }
      
      testing.value.out[i,] = c(i,local.strategy.value,central.strategy.value,local.strategy.value-central.strategy.value,out.local,
                                pertest.cost.local,out.central,pertest.cost.central,
                                specimen.transport.cost, fraction.suspected.with.disease,test.cost.central,test.cost.local.capital,
                                CostTx,NetBenefitTx,diag.sn.central,diag.sp.central, local.sn.dec, local.sp.dec,
                                diag.sn.local, diag.sp.local, valueLostLocal, valueLostCentral, round(total.suspected))
      
      if (i %% (numRuns/100) == 0) pb$tick()
      
    }
    ###add a value to your outcome df that identifies which strategy is best
    ### This is now based on total utility (not incremental from next best). this allows for implementation of value lost based on clinical utility
    ### and that is because the incremental utility may not change much even though the overall absolute clinical utility drops by 50%
    testing.value.out <- testing.value.out %>% dplyr::mutate(BestStrategy = case_when(
                      (NumTests_local > 0 & BestStrategyCost_local >= BestStrategyCost_central) ~ 'local',
                      (NumTests_central > 0 & BestStrategyCost_central > BestStrategyCost_local) ~ 'central',
                      (NumTests_local == 0 &  NumTests_central == 0) ~ 'no testing'))
                    
    #add a column for the additional value of best strategy
    testing.value.out$BestStrategyIncrementalValue = NA
    indices.tmp = which(testing.value.out$BestStrategy=='local' & testing.value.out$central.strategy.value>0)
    testing.value.out$BestStrategyIncrementalValue[indices.tmp] = testing.value.out$local.strategy.value[indices.tmp] - testing.value.out$central.strategy.value[indices.tmp]
    indices.tmp = which(testing.value.out$BestStrategy=='local' & testing.value.out$central.strategy.value<=0)
    testing.value.out$BestStrategyIncrementalValue[indices.tmp] = testing.value.out$local.strategy.value[indices.tmp]
    
    indices.tmp = which(testing.value.out$BestStrategy=='central' & testing.value.out$local.strategy.value>0)
    testing.value.out$BestStrategyIncrementalValue[indices.tmp] = testing.value.out$central.strategy.value[indices.tmp] - testing.value.out$local.strategy.value[indices.tmp]
    indices.tmp = which(testing.value.out$BestStrategy=='central' & testing.value.out$local.strategy.value<=0)
    testing.value.out$BestStrategyIncrementalValue[indices.tmp] = testing.value.out$central.strategy.value[indices.tmp]
    
    indices.tmp = which(testing.value.out$BestStrategy=='no testing' & testing.value.out$local.strategy.value>testing.value.out$central.strategy.value)
    testing.value.out$BestStrategyIncrementalValue[indices.tmp] = -1*testing.value.out$local.strategy.value[indices.tmp]
    indices.tmp = which(testing.value.out$BestStrategy=='no testing' & testing.value.out$local.strategy.value<=testing.value.out$central.strategy.value)
    testing.value.out$BestStrategyIncrementalValue[indices.tmp] = -1*testing.value.out$central.strategy.value[indices.tmp]
    
    return(testing.value.out)
  }
  
  
  ###### CALL RUN MODEL FUNCTION ####
  #testing.value.out <- run_model(parameter.space, disutility.curve.selection, numRuns)
  testing.value.out <- run_model(parameter.space, numRuns)
  
  ##### Variables
  
  vout = testing.value.out
  vout$NetBenefitCostTxRatio = vout$NetBenefitTx/vout$CostTx
  vout$test.cost.central.costTx.ratio = vout$test.cost.central/vout$CostTx
  vout$PerTestCostLocal.CostTx.ratio = vout$PerTestCostLocal/vout$CostTx
  vout$PerTestCostCentral.CostTx.ratio = vout$PerTestCostCentral/vout$CostTx
  vout$BestStrategyLabel = vout$BestStrategy
  vout$BestStrategyLabel[vout$BestStrategyLabel=='local'] = 'onsite'
  vout$BestStrategyLabel[vout$BestStrategyLabel=='central'] = 'sendout'
  vout$BestStrategyLabel[vout$BestStrategyLabel=='no testing'] = 'test none'
  vout$BestStrategyLabel = as.factor(vout$BestStrategyLabel)
  vout$BestStrategy_dichot <- ifelse(vout$BestStrategyLabel == 'onsite', 1, 0)
  vout$BestStrategy_dichot_central <- ifelse(vout$BestStrategyLabel == 'sendout', 1, 0)
  vout$BestStrategy_dichot_no_test <- ifelse(vout$BestStrategyLabel == 'test none', 1, 0)
  vout$PerTestCostCentral.test.cost.central.ratio = vout$PerTestCostCentral/vout$test.cost.central
  vout$PerTestCostLocal.test.cost.central.ratio = vout$PerTestCostLocal/vout$test.cost.central
  vout$PerTestCostLocal.PerTestCostCentral.ratio = vout$PerTestCostLocal/vout$PerTestCostCentral
  vout$Treat.NoTreat.Cutpoint = vout$CostTx / (vout$CostTx + vout$NetBenefitTx)
  #vout$tat.central.strategy.timeuntilworthless.ratio = vout$tat.central.strategy/vout$TimeUntilWorthless
  #vout$ValueLostDuetoTATlocal = vout$tat.local.strategy/vout$TimeUntilWorthless
  #vout$ValueLostDuetoTATcentral = vout$tat.central.strategy/vout$TimeUntilWorthless
  #vout$tat.local.central.ratio = vout$tat.local.strategy/vout$tat.central.strategy
  
  vout$per.test.cost.central.local.avg = (vout$PerTestCostCentral+vout$PerTestCostLocal)/2
  vout$CostTxNetBenefitTxAvg = (vout$CostTx+vout$NetBenefitTx)/2
  vout$AvgPerTestCost.AvgCostTxNetBenefitTx.ratio = vout$per.test.cost.central.local.avg/vout$CostTxNetBenefitTxAvg
  vout$cost.central.capital.cost.ratio = vout$test.cost.central/vout$test.cost.local.capital

  write.csv(vout, file = paste0("Output/MillionRuns_valuelostbyUtilityTxFixed.TB/Million Runs Output -", j, " seed ",myseed,".csv"), row.names = FALSE)
}
end.time = Sys.time()
end.time - start.time






