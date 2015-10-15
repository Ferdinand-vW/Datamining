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