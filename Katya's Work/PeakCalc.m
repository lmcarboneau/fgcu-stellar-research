%Author: Ekaterina Vydra
%function for max amplitude and peak snr calculations
%also outputs the frequency associated with the max amp
function [Peak_SNR,max_amp,max_freq] = PeakCalc(noise1) 

            min_threshold = 20;
            max_threshold = 23;
            idx = any(noise1(:,2)>=min_threshold & noise1(:,2)<=max_threshold,2);
            HF_noise=mean(noise1(idx));
               
            
            amplitude = noise1(:,1)';
            frequency = noise1(:,2)';
             %find highest peak in amplitude spectrum and its index
            [max_amp,max_index] = max(amplitude);
            max_freq = frequency(max_index);
            Peak_SNR = max_amp/HF_noise; 
