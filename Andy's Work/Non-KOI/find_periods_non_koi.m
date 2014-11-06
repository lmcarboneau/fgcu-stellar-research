function [Range,zc,flicker,dataFile,det_stat,sigma] = find_periods_non_koi(FileNamesFile)
 
%**********************Read Me!*************************************
%Text in this format is questionable or not completely explained yet.
%which means....I have little to noclue yet what it does :D
%*******************************************************************

mkdir 'Matlab_Processed'

%Imports the filenames:
fileNames = importdata(FileNamesFile);
%

%Imports the data from the KOI data file we manually retrieve:
%fitsData = fopen(KeplerDataFile, 'r');
%delimiter = ' ';
%formatSpec = '%f%f%f%f%[^\n\r]';
%dataArray = textscan(fitsData, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
%fclose(fitsData);
%KOI_Data = [dataArray{1:end-1}];
%clearvars filename delimiter formatSpec fileID dataArray ans;

%Prints the first line of the new 'Star_Data' file in the new directory,
%just a header:
fileOutHeader = fopen('Matlab_Processed/Star_Data.txt','a');
fprintf(fileOutHeader, '%s\n\n', 'Format: Range, ZC, Flicker, Period, Det_Stat, Sigma');
fileOutHeader = fclose(fileOutHeader);

%Existence check for the files in fileName variable and further processing:
for i = 1:numel(fileNames)
    exist_check = exist(fileNames{i}, 'file');
    
    %Operations made if file exists:
    if exist_check ~= 0
        
        %Pull ID from fits filename:
        %fileNameId = regexprep(fileNames{i}, '[a-zA-Z_0]+(\d+).*', '$1');
        
        %Prepare the fits file matrix for manipulation:
        fileNames_open = fopen(fileNames{i});
        
        for k = 1:12
            fgetl(fileNames_open);
        end

        fitsFile = textscan(fileNames_open, '%f,%f,%f');
        fitsFile = cell2mat(fitsFile);

        %fileNameId = str2num(fileNameId);
        
        
        %for ii = 1:size(fileNameId)
        %Find all matching indexes in the KOI_Data File:
        %koiIndex = find(KOI_Data(:,1) == fileNameId(ii));
        %end
        
        fclose(fileNames_open);

            %for xx = 1:numel(koiIndex)
            %jj = koiIndex(xx);
            
            %Find phase values for all points:
            %phase = (fitsFile(:,1)-KOI_Data(jj, 3)/KOI_Data(jj, 1));
            %tr_phase_length = KOI_Data(jj, 2)/(24*KOI_Data(jj, 1));
            %phase = phase - fix(phase);
            %pphase = phase;
            ccount = fitsFile(:,2);
            rawcount = fitsFile(:,3);
            time = fitsFile(:,1);
            ttime = time;
            
            %For those "close" to transit, remove points:
            %index = find(abs(phase)<1.1*tr_phase_length | abs(phase-1)<1.1*tr_phase_length);
            %pphase(index) = [];
            %ccount(index) = [];
            %rawcount(index) = [];
            %ttime(index) = [];
            
            %Now interpolate across the gaps in the time domain:
            %in_count = interp1(ttime,ccount,time,'pchip');
            %counts = in_count;
            
            %in_count = interp1(ttime,rawcount,time,'pchip');
            %rcounts = in_count;
            
	        counts = ccount;
	        rcounts = rawcount;
        

            ff = [0.0225:1e-4:1.0];
            mean_counts = mean(rcounts);
            rcounts = rcounts-mean_counts;
            rcounts = detrend(rcounts);
            XF=dft(time,rcounts,ff);
            
            [a,b] = max(abs(XF));
            
            det_stat=sqrt((a-mean(abs(XF)))/std(abs(XF(numel(XF)-100:numel(XF)))));
            
            dataFile = 1/ff(b);
            
            %Now calculate the statistics we will use to find log g and characterize
            %variability & sort them:
            
            counts = counts-mean(counts);
            counts = detrend(counts);
            
            X = counts;
            Y = fitsFile(:,1);
            [X, Index] = sort(X);
            Y = Y(Index);
            
            %Find the desired indices:
            
            NumSamples = sum(Y);
            MedianInd = (NumSamples+1)/2;
            Per5Ind = .05 * (NumSamples + 1);
            Per95Ind = .95 * (NumSamples + 1);
            
            %Cumulative frequency:
            
            CumFreq = cumsum(Y);
            
            %Get the frequency bins of interest:
            
            MedBinFloor = find(CumFreq >= floor(MedianInd), 1);
            MedBinCeil = find(CumFreq >= ceil(MedianInd), 1);
            
            Per5BinFloor = find(CumFreq >= floor(Per5Ind), 1);
            Per5BinCeil = find(CumFreq >= ceil(Per5Ind), 1);
            
            Per95BinFloor = find(CumFreq >= floor(Per95Ind), 1);
            Per95BinCeil = find(CumFreq >= ceil(Per95Ind), 1);
            
            Median = (X(MedBinFloor) + X(MedBinCeil))/2;
            
            %Average is used instead of interpolation to match PRCTILE:
            
            Per5 =   (X(Per5BinFloor) + X(Per5BinCeil))/2;
            Per95 =  (X(Per95BinFloor) + X(Per95BinCeil))/2;
            Range = (Per95-Per5)/mean_counts;
            
            %Now calculate the 8-hour flicker & calculate 16-point (8 hour) boxcar smooth:
            
            smooth_counts = bsmooth(rcounts,16);
            
            %Subtract smoothed curve from original and calculate rms
            %then remove endpoints which have detritus from boxcar smoothing:
            
            flicker = std(rcounts(10:end-10)-smooth_counts(10:end-10))/mean_counts;
            
            %Finally calculate the number of zero crossings then
            %smooth by 10 hour (20 point) bins and pass to zerocross:
            
            zc_in = rcounts-bsmooth(rcounts,20);
            zc_in = zc_in(10:end-10); %Removing endpoints which have detritus from boxcar smoothing
            zc_in = zc_in-mean(zc_in);
            
            z = zerocross(zc_in);
            zc = numel(z);
            
            %Calulate the standard deviation of the counts:
            
            sigma = std(counts)/mean_counts;
            
            %Opens the new file and starts appending information to it:
            fileOutHeader = fopen('Matlab_Processed/Star_Data.txt','a');
            fprintf(fileOutHeader, '%s  %s  %f  %f  %f  %f  %f\n\n', fileNames{i}, Range*1e3, zc, flicker*1e3, dataFile, det_stat, sigma);
            fileOutHeader = fclose(fileOutHeader);
            
            %End of operations if files exist.
            %end
            %If file does not exist:
            elseif exist_check == 0
                disp(fileName{i}) %Shows me the file that doesnt exist.
    end
end
end
