# -*- coding: utf-8 -*-
"""
Created on Sat Jul 16 23:34:58 2016

@author: welcome
"""

import pandas as pd

UnstructuredData = pd.read_csv("H:/Case_Study/CaseStudy_Final/Unstructured_Assigment/Unstructured Data English.csv")
Tweets = UnstructuredData.iloc[:,1]


def CleanTweets(x):
    splitRow = x.split(" ")
    iteratorTxt = ""
    CleanedTxt = ""
    for i in splitRow:        
        CleanedTxt = re.sub("http.*","",i)
        CleanedTxt = re.sub('[^a-zA-Z0-9 \n\.]', '', CleanedTxt)
        iteratorTxt = iteratorTxt +" "+ CleanedTxt        
    return(iteratorTxt)
    
import re

TweetsRes = Tweets.apply(CleanTweets)
UnstructuredData[['Katakana text Translated']] = TweetsRes

UnstructuredData.to_csv("Sentiment.csv",sep=",")


NegativeTweets = UnstructuredData[UnstructuredData.Sentiment=="N"]['Katakana text Translated']
PositiveTweets = UnstructuredData[UnstructuredData.Sentiment=="P"]['Katakana text Translated']

NegativeTweets = NegativeTweets.reset_index(drop=True).tolist()
PositiveTweets = PositiveTweets.reset_index(drop=True).tolist()

import gensim
import numpy as np
LabeledSentence = gensim.models.doc2vec.LabeledSentence
from sklearn.cross_validation import train_test_split
import numpy as np

pos_reviews = PositiveTweets
neg_reviews = NegativeTweets

    
y = np.concatenate((np.ones(len(pos_reviews)), np.zeros(len(neg_reviews))))

x_train, x_test, y_train, y_test = train_test_split(np.concatenate((pos_reviews, neg_reviews)), y, test_size=0.2)


def cleanText(corpus):
    punctuation = """.,?!:;(){}[]"""
    corpus = [z.lower().replace('\n','') for z in corpus]
    corpus = [z.replace('<br />', ' ') for z in corpus]

    #treat punctuation as individual words
    for c in punctuation:
        corpus = [z.replace(c, ' %s '%c) for z in corpus]
    corpus = [z.split() for z in corpus]
    return corpus

x_train = cleanText(x_train)
x_test = cleanText(x_test)


def labelizeReviews(reviews, label_type):
    labelized = []
    for i,v in enumerate(reviews):
        label = '%s_%s'%(label_type,i)
        labelized.append(LabeledSentence(v, [label]))
    return labelized
    
x_train = labelizeReviews(x_train, 'TRAIN')
x_test = labelizeReviews(x_test, 'TEST')    

########################################################

import random

    


size = 400

#instantiate our DM and DBOW models
model_dm = gensim.models.Doc2Vec(min_count=1, window=10, size=size, sample=1e-3, negative=5, workers=3)
model_dbow = gensim.models.Doc2Vec(min_count=1, window=10, size=size, sample=1e-3, negative=5, dm=0, workers=3)

#build vocab over all reviews

a1 = np.concatenate((x_train, x_test))
labelized1 = []
for i in range(a1.shape[0]):
    labelized1.append(LabeledSentence(a1[i][0], a1[i][1]))

model_dm.build_vocab(labelized1)
model_dbow.build_vocab(labelized1)

#We pass through the data set multiple times, shuffling the training reviews each time to improve accuracy.
all_train_reviews = x_train
import pandas as pd
#all_train_reviews = pd.DataFrame(all_train_reviews)
for epoch in range(10):
    perm = np.random.permutation(len(all_train_reviews))
    model_dm.train([all_train_reviews[i] for i in perm])
    model_dbow.train([all_train_reviews[i] for i in perm])

#Get training set vectors from our models
def getVecs(model, corpus, size):
    vecs = [np.array(model.docvecs[z[1]]).reshape((1, size)) for z in corpus]
    return np.concatenate(vecs)

#for z in x_train:
#    print(z[1])
#    break



train_vecs_dm = getVecs(model_dm, x_train, size)
train_vecs_dbow = getVecs(model_dbow, x_train, size)

train_vecs = np.hstack((train_vecs_dm, train_vecs_dbow))

#train over test set
#x_test = np.array(x_test)

for epoch in range(10):
    perm = np.random.permutation(len(x_test))
    model_dm.train([x_test[i] for i in perm])
    model_dbow.train([x_test[i] for i in perm])
    
#    perm = np.random.permutation(x_test.shape[0])
#    model_dm.train(x_test[perm])
#    model_dbow.train(x_test[perm])

#Construct vectors for test reviews
test_vecs_dm = getVecs(model_dm, x_test, size)
test_vecs_dbow = getVecs(model_dbow, x_test, size)

test_vecs = np.hstack((test_vecs_dm, test_vecs_dbow))


train_vecs = pd.DataFrame(train_vecs)
test_vecs = pd.DataFrame(test_vecs)

y_test = pd.Series(y_test)
y_train = pd.Series(y_train)

from sklearn.linear_model import LogisticRegression
from sklearn import metrics
#x = list(train.columns)
#x.remove('Class')
data = train_vecs
#data = data[list(data.columns)[1:]]
#list(data.columns)
model = LogisticRegression()
#y = train.Class
#x,y= train[:]
model.fit(data,y_train)
print(model)
expected = y_test
expected
predicted = model.predict(test_vecs)
print(metrics.classification_report(expected,predicted))
print(metrics.confusion_matrix(expected,predicted))
#                 precision    recall  f1-score   support
#
#        0.0       0.89      0.80      0.84        20
#        1.0       0.56      0.71      0.63         7

#Accuracy on Test
#77%
predicted = model.predict(train_vecs)
expected = y_train
print(metrics.classification_report(expected,predicted))
print(metrics.confusion_matrix(expected,predicted))
#94 %
#                   precision    recall  f1-score   support
#        0.0       0.96      0.96      0.96        71
#        1.0       0.92      0.92      0.92        36
##model = LogisticRegression(data,train.Class)


from sklearn.neighbors import KNeighborsClassifier
modelKNN = KNeighborsClassifier(n_neighbors = 3)
modelKNN.fit(data,y_train)
list(data.columns)
predictedKNN = modelKNN.predict(test_vecs)
print(metrics.confusion_matrix(y_test,predictedKNN))
print(metrics.classification_report(y_test,predictedKNN))
# 85 % accuracy on Test using KNN
#
#                precision    recall  f1-score   support
#
#        0.0       0.89      0.80      0.84        20