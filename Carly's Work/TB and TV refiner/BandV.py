# Program to refine stars based on B and V
# Extracted from VizieR database, only including TB and TV

# Carly Hessler
# 9/15/2014

import csv
import numpy as np

starList = []
Teff = []
finalStarList = []


def reader(fileName):
    with open(fileName, 'rU') as csvfile:
        starReader = csv.reader(csvfile, delimiter='|')
        starReader.next()
        i = 0   # necessary as I am a bad programmer
        for row in starReader:
            if row and i > 53 and row[:-10]:    # this isn't good coding, I'm aware, but it works
                starList.append([row[1], row[2], row[3], row[4]])
            i += 1
        # note that at the end, i will be OVER NINE THOUSAAAAND. VizieR spits out blank rows which I removed

def BminusV():
    for i in range(len(starList)):
        x = float(starList[i][2]) - float(starList[i][3])
        if 0.7 < x < 0.775:
            finalStarList.append((starList[i][0], starList[i][1]))

    print(len(finalStarList))
    print(finalStarList)

def writeToFile(fileName):
    writer = csv.writer(open(fileName, "wb"))
    writer.writerows(finalStarList)

np.set_printoptions(threshold='nan')
reader('/home/carly/Documents/asu.tsv')
#reader('E:/BandVrefiner/asu.tsv')
print(len(starList))
# not entirely sure how they got in but there were empty lines
del(starList[2460])
del(starList[2836])
del(starList[3070])
del(starList[3080])
del(starList[3684])
del(starList[3929])
#TODO: Re-code such that the empty lines are thrown out

BminusV()
writeToFile('tempermentalStars.csv')
