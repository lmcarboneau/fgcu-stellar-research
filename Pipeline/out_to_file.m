function out_to_file(time,aperture,bkg,xcentroid,ycentroid,file,masksize,targetNum)
    
    output_data = [time'; aperture(masksize,:);bkg'; xcentroid'; ycentroid';];
    % places all output data into same matrix to print
    
    ms_str = int2str(masksize);
    % mask size as a string
    
    filename = strrep(file,'_lpd-targ.fits','');
    filename = strrep(filename,'ktwo','outputs/pipeout_ktwo');
    filename = strcat(filename,'_',ms_str);
    filename = strcat(filename,'_target', num2str(targetNum));
    %filename = strcat(filename,'.fits');
    % creates output filename based on input filename, preserving target ID

    filename
    ID = fopen(filename,'w')
    fprintf(ID,'%s %s %s %s %s\n','Time','Flux-Mask','Background','XCent','YCent');
    fprintf(ID,'%12.8f %f %f %f %f\n',output_data);
    fclose(ID);
    % will overwrite any existing target output file
    
end
