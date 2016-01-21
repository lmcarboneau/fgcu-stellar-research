function [Output] = read_test(FileNamesFile)
addpath('AllEBFiles')
mkdir 'Matlab_Processed'

%Imports the filenames file:
fileNames = importdata(FileNamesFile);

fileOutHeader = fopen('Matlab_Processed/Star_Data.txt','a');
fprintf(fileOutHeader, '%s\n\n', 'Filename:         max_freq,   max_val,    Variable_Star,  Potential_EB,   Visual_Followup');
fileOutHeader = fclose(fileOutHeader);

%INITIALIZE min_SNR %try 10 to start with
min_SNR = 10; 
%INITIALIZE relative_amplitude %try 0.25 to start with
relative_amplitude = 0.25;

%Existence check for the files in fileName variable and further processing:
for i = 1:numel(fileNames)
    exist_check = exist(fileNames{i}, 'file');
    disp((i/numel(fileNames))*100)
    disp('% complete')
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
                   
            
            %find highest peak in amplitude spectrum and its index
            [max_amp,max_index] = max(amplitude);
            max_freq = frequency(max_index);

            Peak_SNR = max_amp/HF_noise;     
            
            
            if(Peak_SNR > min_SNR) 
                %Flag as potential variable star
                Variable = 1;
                
                %checks frequency only if variable star
                if(max_freq>.33 & max_freq < 2)
                    %if in range,flag as potential EB
                    EB = 1;
                else
                    EB = 0;
                end
                
            else 
                %Flag as uninteresting
                Variable = 0;
                EB=0;
            end
   
            
            %define range to search for second peak
            Lower_range = 0.5*max_freq-0.022;
            
            %0.022 c/d comes from anticipated freq resolution of time series
            Upper_range = 0.5*max_freq+0.022;
            
            %[max_freq2, max_val2] = max(Amplitude(freq>lower_range & freq<upper_range)
            %gets amplitude and frequency in the correct freq range (between high and low)
            range = noise1(any(noise1(:,2)>=Lower_range & noise1(:,2)<=Upper_range,2),:);
            %finds max amplitude in that correct range
            max_amp2=max(range(:,1));
            %index for the row with the max amplitude 
            idx2=range(:,1)==max_amp2;
            %gets freq value associated with the max amplitude
            max_freq2=range(idx2,2);


            
            %IF ((max_val2/max_val) > relative amplitude) THEN
            % Flag for visual followup
            %ENDIF        
            if (max_amp2/max_amp)  > relative_amplitude
                %mark for visual follow up
                VF=1;
            else 
                VF=0;
            end
            
            
            %Opens the new file and starts appending information to it:
            fileOutHeader = fopen('Matlab_Processed/Star_Data.txt','a');
            fprintf(fileOutHeader, '%s  %f  %f  %i  %i  %i\n\n', fileNames{i},max_amp, max_freq, Variable, EB, VF);
            fileOutHeader = fclose(fileOutHeader);
            
            %End of operations if files exist.
            %end
            %If file does not exist:
            elseif exist_check == 0
                disp(fileName{i}) %Shows me the file that doesnt exist.
    end
end
