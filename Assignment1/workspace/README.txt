Some information about the values that are currently in .RData

Heartbin.dat: Dataframe, which was directly read from Heartbin.txt
sample_data: Dataframe of size 97. This is the sample data that we used to test our final model with
data:List of two containing the above sample_data as its first value and the training_data as its second value
minleaf: The minleaf that we found was best for the given training_data
nmin:The nmin that we found was best for the given training_data
sample_err:Calculated classification error using sample_data against the final model
sample_result:List of two containing sample_err as its first value and sample_tree as its second value
sample_tree: Constructed tree from the sample, which was used against the final model. A human readable version can be found in tree.png.
training_data: List of 10 dataframes, which were used for 10-Fold cross validation


HOWTO Heartbin analysis

Do the following steps in this exact order (underscores are important):
1. Open HeartbinAnalysis.R
2. Save to source
3. First we need to read the data. 
    heartbin.dat_ <- read.csv("datafiles/heartbin.txt")
4. Then we wish to create sample data and training data. 
    data_ <- prepareData(heartbin.dat_)
5. Now we want to extract the data that we want to use and assign them to variables.
    sample_data_ <- data_[[1]]
    training_data_ <- data_[[2]]
6. Next we want to perform the crossvalidation using the training_data_ given an nmin,minleaf
    evaluateModel(training_data_,nmin,minleaf)
7. Repeat that step until you have found an appropriate nmin and minleaf
8. Now that we have our final model let's apply it to the sample data.
    sample_result_ <- evaluateSample(sample_data_,nmin,minleaf)
9. Lets unpack it to get the results we want
    sample_err_ <- sample_result[[1]]
    sample_tree_ <- sample_result[[2]]
10. Now we are done with the heartbin analysis. If you want to compare the results to ours
    it is possible to use the variables that are defined above for this analysis. Understand
    that prepareData will give different data each time it is run.