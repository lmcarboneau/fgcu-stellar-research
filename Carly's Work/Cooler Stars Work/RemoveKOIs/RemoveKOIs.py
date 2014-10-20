#A ridiculously simple KOI list remover
#Mostly uploading it in case I need the code later

#Carly Hessler
#10/14/2014

import csv
import numpy as np

KOIArr = []
FullArr = []
nonKOIArr = []

def reader(fileName):
    newArr = []
    with open(fileName, 'rU') as csvfile:
        starReader = csv.reader(csvfile, delimiter='|')
        starReader.next()
        for row in starReader:
            if row:
                newArr.append(row)

        return newArr

def removeOneListFromAnother():
    for row in KOIArr:
        try:
            FullArr.remove(row)
        #TODO: Figure out WHY some KOIs aren't in the list that I put through MAST to find the KOIs...
        #in the meantime, exception handling
        except ValueError:
            pass
        except AttributeError:
            pass

def writeToFile(fileName):
    writer = csv.writer(open(fileName, "wb"))
    for row in FullArr:
        writer.writerow(row)

FullArr = reader('C:/Users/cahessler3098/Downloads/fgcu-stellar-research/Carly\'s Work/Cooler Stars Work/RemoveKOIs/InitialList.csv')
KOIArr = reader('C:/Users/cahessler3098/Downloads/fgcu-stellar-research/Carly\'s Work/Cooler Stars Work/RemoveKOIs/Complete List of KOIs.csv')

temp = len(FullArr)
print(len(FullArr))

removeOneListFromAnother()
print(temp - len(FullArr))

writeToFile('nonKOITargets.csv')
