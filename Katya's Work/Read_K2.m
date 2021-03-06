%%Author: Ekaterina Vydra
%%File for reading through all files in the k2 folder and making EB triggers 
%%Reads K2 pipeline output file format only

function [Output] = Read_K2(FileNamesFile)
%If folder containing k2 processed files is not named k2, change this line to the folder name
%Make sure file list has been created (in terminal -> ls * > 'fileList.txt')
addpath('k2')
mkdir 'Matlab_Processed'

%Imports the filenames file:
fileNames = importdata(FileNamesFile);

fileOutHeader = fopen('Matlab_Processed/Star_Data.txt','a');
fprintf(fileOutHeader, '%s\n\n', 'Filename:         max_amp,    lower_amp,     max_amp2,   lower_amp2,    max_freq,     lower_freq,    Variable_Star,  Potential_EB,   Visual_Followup');
fileOutHeader = fclose(fileOutHeader);

%Existence check for the files in fileName variable and further processing:
for i = 1:numel(fileNames)
    exist_check = exist(fileNames{i}, 'file');
    disp((i/numel(fileNames))*100)
    disp('% complete')
    %Operations made if file exists:
    if exist_check ~= 0
              
        %Prepare the fits file matrix for manipulation:
        fileNames_open = fopen(fileNames{i});
        
        for k = 1
            fgetl(fileNames_open);
        end

        %!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       
        fitsFile = textscan(fileNames_open, '%f,%f,%f,%f,%f,%f,%f,%f');
        %!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 


        fclose(fileNames_open);
        
        [max_amp, amp, max_amp2, amp2, max_freq, max_freq2, Variable, EB, SP] = EBDetection(fitsFile);

              %Opens the new file and starts appending information to it:
            fileOutHeader = fopen('Matlab_Processed/Star_Data.txt','a');
            fprintf(fileOutHeader, '%s  %f  %f  %f  %f  %f  %f  %i  %i  %i\n\n', fileNames{i},max_amp, amp, max_amp2, amp2, max_freq, max_freq2, Variable, EB, SP);
            fileOutHeader = fclose(fileOutHeader);
            
            %End of operations if files exist.
            %end
            %If file does not exist:
            elseif exist_check == 0
                disp(fileName{i}) %Shows me the file that doesnt exist.
    end
end
