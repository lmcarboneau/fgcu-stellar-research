%%Author: Ekaterina Vydra
%Function calculates the second peak that is within the right range of the main peak 
%Looks before and after the main peak, returns both amplitudes
function [max_ampL,max_ampU] = Peak2Calc(max_freq,noise1)

    %define range to search for second peak in lower frequency
    %fist trial .022
    LLower_range = 0.5*max_freq-0.04;         
    %0.022 c/d comes from anticipated freq resolution of time series
    LUpper_range = 0.5*max_freq+0.04;  
    %makes sure that the range does not include the main peak itself
    if LUpper_range>max_freq 
        LUpper_range=max_freq-0.01;
    end
    %[max_freq2, max_val2] = max(Amplitude(freq>lower_range & freq<upper_range)
    %gets amplitude and frequency in the correct freq lower range (between high and low)
    Lrange = noise1(any(noise1(:,2)>LLower_range & noise1(:,2)<LUpper_range,2),:);
    %finds max amplitude in that correct range
    max_ampL=max(Lrange(:,1));
    %index for the row with the max amplitude 
    %idxL=Lrange(:,1)==max_ampL;
    %gets freq value associated with the max amplitude
    %max_freqL=Lrange(idxL,2);

   
    
    %define range to search for second peak in higher frequency
    ULower_range = 2*max_freq-0.04;     
    if ULower_range<max_freq 
        ULower_range=max_freq+0.01;
    end
    %0.022 c/d comes from anticipated freq resolution of time series
    UUpper_range = 2*max_freq+0.04;
               %[max_freq2, max_val2] = max(Amplitude(freq>lower_range & freq<upper_range)
    %gets amplitude and frequency in the correct freq upper range (between high and low)
    Urange = noise1(any(noise1(:,2)>ULower_range & noise1(:,2)<UUpper_range,2),:);
    %finds max amplitude in that correct range
    max_ampU=max(Urange(:,1));
    %index for the row with the max amplitude 
    %idxU=Urange(:,1)==max_ampU;
    %gets freq value associated with the max amplitude
    %max_freqU=Urange(idxU,2);
    if isempty(max_ampU)
                max_ampU=0;
    end 
                         
    if isempty(max_ampL)
        max_ampL=0;
    end 
