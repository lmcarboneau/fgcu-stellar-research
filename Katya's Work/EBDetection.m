%Author: Ekaterina Vydra
%Processes each fits file table given, calculates max amplitudes and frequencies, 
%returns triggers from further calculation  
function [max_amp, amp, max_amp2, amp2, max_freq, max_freq2, Variable, EB, SP] = EBDetection(fitsFile)
        
        %INITIALIZE min_SNR %try 10 to start with
        min_SNR = 10; 
        %INITIALIZE relative_amplitude %try 0.25 to start with
        relative_amplitude = 0.25;

        %fitsFile = textscan(fileNames_open, '%f,%f,%f');
        fitsFile = cell2mat(fitsFile);  
          [split,b]=size(fitsFile);

          %Pick out the columns we want from file
            flux = fitsFile(1:round(split/2),2);          
            time = fitsFile(1:round((split/2)),1); 
            %%%%%%splitting by 2 to throw out gb's and other false positives  
            flux2 = fitsFile(round(split/2)+1:end,2);          
            time2 = fitsFile(round(split/2)+1:end,1);
                
	    %INITIALIZE frequency array %from 0 to 24 in steps of 0.005 (or even 0.01) should work
            frequency = (0:0.005:24);
                      
            amplitude=AmpCalc(time,flux,frequency);
            %average high-frequency noise level
            noise=[amplitude;frequency];
            noise1=noise';
            
            %amplitude for second half            
            amplitude2=AmpCalc(time2,flux2,frequency);
            noise=[amplitude2;frequency];
            noise2=noise';
                     
            [Peak_SNR,max_amp,max_freq] = PeakCalc(noise1);
            [Peak_SNR2,max_amp2,max_freq2] = PeakCalc(noise2);
                   
            if(Peak_SNR > min_SNR && Peak_SNR2 > min_SNR) 
                %Flag as potential variable star
                Variable = 1;
                %checks frequency only if variable star
                if(max_freq >=.25 && max_freq <= 2.5 && max_freq2 >=.25 && max_freq2 <= 2.5)
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
    
            [max_ampL,max_ampU] = Peak2Calc(max_freq,noise1);
            [max_ampL2,max_ampU2] = Peak2Calc(max_freq2,noise2);
                
                    
             %IF ((max_val2/max_val) > relative amplitude) THEN
            % Flag for visual followup, second peak detection        
            if (((max_ampL/max_amp)  > relative_amplitude) && ((max_ampL2/max_amp2)  > relative_amplitude)) || (((max_ampL/max_amp)  > relative_amplitude) && ((max_ampU2/max_amp2) > relative_amplitude)) 
                %mark for visual follow up
                SP=1;
            else                      
                if (((max_ampU/max_amp) > relative_amplitude) && ((max_ampL2/max_amp2)  > relative_amplitude)) || (((max_ampU/max_amp)  > relative_amplitude) && ((max_ampU2/max_amp2)  > relative_amplitude)) 
                    SP=1;                
                else
                    SP=0;
                end
            end
                       
            %quick statement to print the higher second peak instead of both
            if max_ampL>max_ampU
                amp=max_ampL;
                %freq=max_freqL;
            else
                amp=max_ampU;
                %freq=max_freqU;
            end
            
            %quick statement to print the higher second peak instead of both
            if max_ampL2>max_ampU2
                amp2=max_ampL2;
            else
                amp2=max_ampU2;
            end
            
