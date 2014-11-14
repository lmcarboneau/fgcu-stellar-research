#!/usr/bin/env python
# coding: utf-8

#This code properly imports all '.fits' files in a specified directory and 
#outputs them into a .txt format that allows several headers and their contained
#data to be read.  
#Author: Andy Lezcano
#Version: 1.04
#Version Date: 11/25/2013

import sys
import pyfits
import glob
import os.path
from operator import itemgetter

print 'If working Directory is the current one, please input "Current".'
DIR = raw_input("Please input a valid directory : ") #-----> This prompts for input from the user to find the '.fits' files

if DIR == 'Current':
	DIR = os.getcwd()
else:
	DIR = DIR

def Checkinitial(TD):
	#This counts the number of '.fits' files in your directory
    		check = len(glob.glob1(TD,"*.fits"))
		if not check:
			print 'There are no .FITS files in this directory! Try Again...'
			sys.exit()
		return check
def Sorter(TD):
	#This function will sort through the fits files for the defined queries, then output them to a .txt file
		os.chdir(TD)
		for allfiles in glob.iglob(os.path.join(TD,'*.fits')):
			print allfiles #This prints out the filenames the porgram is currently processing
			with pyfits.open(allfiles) as HDU:
				#This block outlines all of the search terms in their respective headers, you will need to set the indices 					#below to search in the correct header for the specified term you are looking for, however no alterations to 					#the header definitions should be made.
				HDU_HD_0 = HDU[0].header
				HDU_HD_1 = HDU[1].header
				#HDU_HD_2 = HDU[2].header  -----> Not usually needed, can be activated if data from this header is required
				HDU_DATA = HDU[1].data
				KeplerIDIndex = HDU_HD_0.index('KEPLERID')
				ChannelIndex = HDU_HD_0.index('SKYGROUP')
				TTYPE1Index = HDU_HD_1.index('TTYPE1')
				TTYPE8Index = HDU_HD_1.index('TTYPE8')
				TTYPE4Index = HDU_HD_1.index('TTYPE4')
				TUNIT1Index = HDU_HD_1.index('TUNIT1')
				TUNIT8Index = HDU_HD_1.index('TUNIT8')
    				TUNIT4Index = HDU_HD_1.index('TUNIT4')
				CROWDSAPIndex = HDU_HD_1.index('CROWDSAP')
				#The below variables are an index search for the data found in the specified indices above, allowing the data 					#to be found in teh numpy array that '.fits' files use  					
				File_Data_KID = list( HDU_HD_0[i] for i in [KeplerIDIndex])
				File_Data_CHAN = list( HDU_HD_0[i] for i in [ChannelIndex])
				Astro_Data_1 = list( HDU_HD_1[i] for i in [TTYPE1Index])
				Astro_Data_8 = list( HDU_HD_1[i] for i in [TTYPE8Index])
				Astro_Data_4 = list( HDU_HD_1[i] for i in [TTYPE4Index])
				Astro_Unit_1 = list( HDU_HD_1[i] for i in [TUNIT1Index])
				Astro_Unit_8 = list( HDU_HD_1[i] for i in [TUNIT8Index])
				Astro_Unit_4 = list( HDU_HD_1[i] for i in [TUNIT4Index])
				CROWDSAP_Data = list(HDU_HD_1[i] for i in [CROWDSAPIndex])
				TData_Pre_Out = map(itemgetter(0, 7, 3), HDU_DATA) #Maps the indices (Data) that you would like to actually 					#pull from the data file
				Nan_Filter = ['nan']
				TData_Pre_Filter = [Data for Data in TData_Pre_Out if not any(nan in Data for nan in Nan_Filter)]
				#'Filters' out the 'nan' occurences in the list to identify lines with missing numbers (you may remove 	 			 		#this without consequence, just rename the next line accordingly
				TData_Output = '\n'.join([',  '.join(map(str, i)) for i in TData_Pre_Filter]) #Orders the output into a 				#convenient row by row format, edit the 'in TData...' to match whatever you have named the mapped list you 					#want to have formatted
				HDU.close()
				out_file_name = 'Processed_' + os.path.basename(allfiles) + '.txt'
    				with open(os.path.join(TD, out_file_name), "w") as copy:
					#Opens up a file and writes the relevant data while also formatting it to have a header and display 						#data below
					Title1_Format = '{0}-----{1}'.format('Kepler I.D.','Channel')
					Title2_Format = '-{0}--------{1}------------{2}'.format('TTYPE1','TTYPE8','TTYPE4')
					File_Format = '{0}--------{1}'.format(File_Data_KID, File_Data_CHAN)
					Astro_Format = '{0}---{1}---{2}'.format(Astro_Data_1, Astro_Data_8, Astro_Data_4)
					Astro_Format_Units = '{0}--{1}--------{2}'.format(Astro_Unit_1, Astro_Unit_8, Astro_Unit_4)
					Astro_Format_Data = '{0}'.format(TData_Output)
					copy.writelines('%s\n' % Title1_Format + '\n')
        				copy.writelines('%s\n' % File_Format + '\n')
					copy.writelines('%s\n' % Title2_Format + '\n')
        				copy.writelines('%s\n' % Astro_Format + '\n')
        				copy.writelines('%s\n' % Astro_Format_Units + '\n')
        				copy.writelines("CROWDSAP --- " + "".join( repr(i) for i in CROWDSAP_Data) + '\n' + '\n')
					copy.writelines('%s\n' % "".join( repr(i) for i in File_Data_KID) + '\n')
					copy.writelines('%s\n' % Astro_Format_Data)
					Results = copy			
		return Results
Checkinitial(DIR)
Sorter(DIR)
