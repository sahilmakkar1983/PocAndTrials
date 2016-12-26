# Databricks notebook source exported at Mon, 7 Nov 2016 07:31:16 UTC
from pyspark.ml.feature import Tokenizer, RegexTokenizer
from pyspark.ml.feature import StopWordsRemover
from pyspark.ml.feature import OneHotEncoder, StringIndexer, IndexToString

tfModelSave="tfmodelsave"
idfModelSave="idfmodelsave"
encodedMapTable="spamClassencodedMapTable"
class tmutils():
  
  def __init__(self):
    pass
  
  # Treat documents with pre-feature extraction
  def preProcessDocuments(self,iDF,iCol,oCol):
    tokenizer = Tokenizer(inputCol=iCol, outputCol="tmp")
    wordsDataFrame = tokenizer.transform(iDF)

    remover = StopWordsRemover(inputCol="tmp", outputCol=oCol)

    #TBD: Stemming
    # Spark unlike nltk doesn't have in-built stemming API - it would take some time to develop that, (beyond shorter scope)
    # TBD later, though it limits accuracy.
    return remover.transform(wordsDataFrame)

  # Let's transform output features as well
  def encodeClasses(self,iDF,iCol,oCol):
    stringIndexer = StringIndexer(inputCol=iCol, outputCol="temp")
    model = stringIndexer.fit(iDF)
    indexed = model.transform(iDF)

    self.encodedKey = "temp"
    self.encodedValue = "categoryValue"
    idx_to_string = IndexToString(inputCol=self.encodedKey, 
                                  outputCol=self.encodedValue)
    self.encodedMap = idx_to_string.transform(indexed).select(self.encodedKey,
                                                              self.encodedValue).distinct()

    '''
    self.encodedMap \
      .write.format("com.databricks.spark.redshift") \
      .option("tempdir", '/temp/tables') \
      .option("dbtable", "encodedMapTable") \
      .mode(SaveMode.Overwrite) \
      .save()
    '''
    encoder = OneHotEncoder(dropLast=True, inputCol="temp", outputCol=oCol)
    return (encoder.transform(indexed))
  
  def getDecodedValue(self,encodedVal):
    return self.encodedMap.filter(col(self.encodedValue) == encodedVal).select(self.encodedKey)

  def tfidf(self,sc,iDF,iCol,oCol):
    
    #Feature extraction using tf-idf
    ## Tf part 
    self.hashingTF = HashingTF(inputCol=iCol, outputCol="rawFeatures", numFeatures=20)
    params = {self.hashingTF.numFeatures: 5, self.hashingTF.outputCol: "vector"}
    self.hashingTF.transform(iDF, params).head().vector
    featurizedData = self.hashingTF.transform(iDF)
    
    ## idf part 
    self.idf = IDF(inputCol="rawFeatures", outputCol=oCol)
    idfDF = self.idf.fit(featurizedData).transform(featurizedData)
    
    #TBD: SAVING models for test and online predictions:
    # --> Not spending much time on this because of other pre-limitations of data-bricks platform in community edition
    #hashingTF.saveAsObjectFile(tfModelSave)
    #hashingTF.save(tfModelSave)
    #sc.parallelize(Seq(idf), 1).saveAsObjectFile(idfModelSave)
    #idf.save(idfModelSave)
    return idfDF

# COMMAND ----------

## train_classifier.py 
from pyspark import SparkContext, SparkConf
from pyspark.ml.feature import HashingTF, IDF, Tokenizer
from pyspark.mllib.regression import LabeledPoint
from pyspark.mllib.classification import NaiveBayes 
from pyspark.mllib.classification import SVMWithSGD
from pyspark.mllib.evaluation import BinaryClassificationMetrics
import numpy as np

import csv

#UDF: user-defined functions
#from tmutils import *

# PLEASE NOTE THAT : on databricks notebooks - these conf,sc,sqlContext are default created, recreation is not allowed
#conf = SparkConf().setAppName('ClassficationCloudApp')
#sc = SparkContext(conf=conf)

spamDF = sqlContext \
    .read.format("com.databricks.spark.csv") \
    .option("header", "false") \
    .option("inferschema", "true") \
    .option("mode", "DROPMALFORMED") \
    .load("/FileStore/tables/59sya0ao1478160935643/SMSSpamCollection.csv")

tm = tmutils()

# Let's pre-process the smses; preProcessDocuments takes care of tokenization and stop-words, stemming etc removed
inputCol="C1"
outputCol="filtered"
processedDF = tm.preProcessDocuments(spamDF, inputCol,outputCol) \
                .select("C0",inputCol,outputCol)
## PLEASE NOTE : combining .select api in single line has lot of optimization, it avoids creating tmp DF(hence avoids extra serialization,deserialization step)
# and improves performance; in our case too time saved by combining is 300%)

# Let's encode classes with OneHotEncoder; encodeClasses function would transform 'spam'->1; 'ham'->0
inputCol="C0"
outputCol="categoryVec"
encodedDF = tm.encodeClasses(processedDF,inputCol,outputCol) \
              .select("filtered",inputCol,outputCol).cache()
## Please NOTE: .cache() is another performance optimization on trade-off for memory 

#Feature extraction using tf-idf
rescaledData = tm.tfidf(sc,encodedDF,"filtered","features")
encodedDFLabels = encodedDF.map(lambda doc: doc['categoryVec']).map(lambda x: x[0])

## Let's train a simple model 
training_test = encodedDFLabels.zip(rescaledData.select('features').rdd).map(lambda x: LabeledPoint(x[0],x[1][0]))

#split data
training, test = training_test.randomSplit([0.9, 0.1], seed = 0)

# Train and check
#Let's put our own validation code, cross-validation standard tool from spark is showing some unknown issues
# Not spending more on fixing SPARK native cross-validation (it may be just limitation on data-bricks(community edition) + python)
minWrongPrediction = -1
bestTuningParam = -1
training, validation = training.randomSplit([0.9, 0.1], seed = 0)
for r in np.logspace(-3,3,14):
  model = SVMWithSGD.train(training,iterations=1000,regType="l2",regParam=r) # step=20.0
  labels_and_preds = validation.zip(model.predict(validation.map(lambda x: x.features))) \
                  .map(lambda x: {"actual": x[0].label, "predicted": float(x[1])})
  wrongPredictions = labels_and_preds.filter(lambda x : x['actual'] != x['predicted']).count()
  correctPredictions = labels_and_preds.filter(lambda x : x['actual'] == x['predicted']).count()
  
  #print(r,wrongPredictions)
  #Simplest way, there are better ways
  if (minWrongPrediction == -1 or wrongPredictions < minWrongPrediction ):
    minWrongPrediction = wrongPredictions
    bestTuningParam = r
    bestModel = model
  #print("Accuracy is {}".format((correctPredictions*100)/labels_and_preds.count()))
  
#print("Best tuning param:{}".format(bestTuningParam))


labels_and_preds = test.zip(bestModel.predict(test.map(lambda x: x.features))) \
                  .map(lambda x: {"actual": x[0].label, "predicted": float(x[1])})

## Let's check accuracy#
wrongPredictions = labels_and_preds.filter(lambda x : x['actual'] != x['predicted']).count()
correctPredictions = labels_and_preds.filter(lambda x : x['actual'] == x['predicted']).count()

# Another example of optimization using .cache(); saves around 35% time
nonSPAM = labels_and_preds.filter(lambda x : x['actual'] == 0.0 ).cache()
SPAM = labels_and_preds.filter(lambda x : x['actual'] == 1.0 ).cache()

print ("Total non-SPAM(-ve): {}".format(nonSPAM.count()))
print ("Total SPAM(+ve)    : {}".format(SPAM.count()))
print ("Total              : {}".format(labels_and_preds.count()))
print ("Wrong-predictions  : {}".format(wrongPredictions))
print ("Accuracy           : {}".format((correctPredictions*100)/labels_and_preds.count()))
print ("False-positive     : %f" % ((nonSPAM.filter(lambda x : x['actual'] != x['predicted']).count()*100)/nonSPAM.count()))
print ("False-negative     : %f" % ((SPAM.filter(lambda x : x['actual'] != x['predicted']).count()*100)/SPAM.count()))
# Please note that model needs data look-up more as false-positives are very high
nonSPAM.unpersist()
SPAM.unpersist()

## Gives unknown issues, probably because of some data-bricks issue
## TBD: to be resolved later
'''
#Get evaluation metrics.
predictionAndLabels = labels_and_preds.map(lambda lp: (lp[0], lp[1]))
metrics = BinaryClassificationMetrics(predictionAndLabels)
# Area under ROC curve
print("Area under ROC = %s" % metrics.areaUnderROC)
'''

# COMMAND ----------

##test_classifier.py 
from pyspark.sql.types import *
from pyspark.sql.functions import col

# Local function to get predictions
def getPredictions(testString):
  df = sqlContext.createDataFrame(sc.parallelize([(testString,)]), ('text',))

  inputCol="text"
  outputCol="filtered"
  testDF = tm.preProcessDocuments(df, inputCol,outputCol) \
                .select(inputCol,outputCol)


  tFeaturizedData = tm.hashingTF.transform(testDF)
  orescaledData = tm.idf.fit(tFeaturizedData).transform(tFeaturizedData)
  #predictions
  opredicted = model.predict(orescaledData.select('features').rdd.map(lambda x: x[0])).collect()
  
  return tm.encodedMap.filter(col('temp') == opredicted[0]).select('categoryValue').toPandas().categoryValue 
  
testString = "Free entry in 2 a wkly comp to win FA Cup final tkts 21st May 2005. Text FA to 87121 to receive entry great deal T&C's apply 08452810075over18's"
print ("It's a [{}]".format(str.upper(str(getPredictions(testString)[0]))))

testString = "I will be late today from office"
print ("It's a [{}]".format(str.upper(str(getPredictions(testString)[0]))))



# COMMAND ----------


