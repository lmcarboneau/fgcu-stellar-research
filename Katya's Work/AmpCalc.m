%Author: Ekaterina Vydra
%amplitude calcultion using absolute value of dft

function [amplitude] = AmpCalc(time,flux,frequency) 

            mean_flux = mean(flux);
            flux = flux-mean_flux;
            flux = detrend(flux);
            DFT=Dft(time,flux,frequency);
            
            %calculate amplitude spectrum from DFT by taking absolute value of complex array from DFT_calc
            amplitude = abs(DFT);      
             %!!!!!!!!!!!!!!!fitting
            amplitudeFit=sum(amplitude.^2);
            fluxFit=sum(flux.^2);
            amplitude= amplitude./sqrt(amplitudeFit/fluxFit);
            
           
