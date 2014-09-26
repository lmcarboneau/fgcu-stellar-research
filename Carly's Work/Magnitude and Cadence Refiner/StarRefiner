# This is a script to take a list of targets and find the ones we want
# We want stars of magnitude < 14 and cadence = 30
# (which are indices 4 and 3, respectively)

# Carly Hessler
# 9/8/2014

import csv

starList = []
refinedList = []
finalList = []

def refiner(fileName):
    with open(fileName, 'rU') as csvfile:
        starReader = csv.reader(csvfile, delimiter=',', quotechar='|')
        starReader.next()
        for row in starReader:
            starList.append(row)

        print(len(starList))

        # boot out the stars magnitude < 14
        for i in range(len(starList)):
            if float(starList[i][3]) < 14:
                refinedList.append(starList[i])

        print(len(refinedList))

        # now to refine it to stars with cadence 30 min
        for i in range(len(refinedList)):
            if int(refinedList[i][4]) == 30:
                finalList.append(refinedList[i])

        print(len(finalList))


refiner('/home/carly/Documents/K2Campaign0targets.csv')