function [Output] = read_test(FileNamesFile)
addpath('002302092')
mkdir 'Matlab_Processed'

%Imports the filenames file:
fileNames = importdata(FileNamesFile);

fileOutHeader = fopen('Matlab_Processed/Star_Data.txt','a');
fprintf(fileOutHeader, '%s\n\n', 'Filename: max_freq, max_val, binary');
fileOutHeader = fclose(fileOutHeader);

%INITIALIZE min_SNR %try 10 to start with
min_SNR = 10; 
%INITIALIZE relative_amplitude %try 0.25 to start with
relative_amplitude = 0.25;


%Existence check for the files in fileName variable and further processing:
for i = 1:numel(fileNames)
    exist_check = exist(fileNames{i}, 'file');
    
    %Operations made if file exists:
    if exist_check ~= 0
              
        %Prepare the fits file matrix for manipulation:
        fileNames_open = fopen(fileNames{i});
        
        for k = 1:14
            fgetl(fileNames_open);
        end

        fitsFile = textscan(fileNames_open, '%f,%f,%f,%f,%f,%f');
        fitsFile = cell2mat(fitsFile);
        
        fclose(fileNames_open);
            
            %Pick out the columns we want from file
            count = fitsFile(:,2);          
            time = fitsFile(:,1);
            ttime = time;        
            counts = count;
       
	    %INITIALIZE frequency array %from 0 to 24 in steps of 0.005 (or even 0.01) should work
            frequency = (0:0.005:24);
            mean_counts = mean(counts);
            counts = counts-mean_counts;
            counts = detrend(counts);
            DFT=dft(ttime,counts,frequency);
            
            %calculate amplitude spectrum from DFT by taking absolute value of complex array from DFT_calc
            amplitude = abs(DFT);
                      
            %average high-frequency noise level
            noise=[amplitude;frequency];
            noise1=noise';
            min_threshold = 20;
            max_threshold = 23;
            idx = any(noise1(:,2)>=min_threshold & noise1(:,2)<=max_threshold,2);
            HF_noise=mean(noise1(idx));
          
            
            %find highest peak in amplitude spectrum and its frequency
            [a,b] = max(amplitude);
                      
                     
            
            det_stat=sqrt((a-mean(abs(DFT)))/std(abs(DFT(numel(DFT)-100:numel(DFT)))));
            
            dataFile = 1/frequency(b);
            
            %Now calculate the statistics we will use to find log g and characterize
            %variability & sort them:
            
            counts = counts-mean(counts);
            counts = detrend(counts);
            
            X = counts;
            Y = fitsFile(:,1);
            [X, Index] = sort(X);
            Y = Y(Index);
                        
            %Cumulative frequency:         
            CumFreq = cumsum(Y);
            sigma = std(counts)/mean_counts;
            
            %Opens the new file and starts appending information to it:
            fileOutHeader = fopen('Matlab_Processed/Star_Data.txt','a');
            fprintf(fileOutHeader, '%s  %f  %f  %f  %f  %f\n\n', fileNames{i},a, b, sigma);
            fileOutHeader = fclose(fileOutHeader);
            
            %End of operations if files exist.
            %end
            %If file does not exist:
            elseif exist_check == 0
                disp(fileName{i}) %Shows me the file that doesnt exist.
    end
end
