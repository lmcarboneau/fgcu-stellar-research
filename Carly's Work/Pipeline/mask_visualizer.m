function [mask_arr] = mask_visualizer(mask_grow,masksize,dim,file,targetNum)

mask_arr = zeros(dim(1),dim(2));

for i=1:masksize
    x = mask_grow(i,2);
    y = mask_grow(i,3);
    mask_arr(x,y) = 1;
end

    ms_str = int2str(masksize);
    % mask size as a string
    
    filename = strrep(file,'_lpd-targ.fits','');
    filename = strrep(filename,'ktwo','outputs\pipeout_ktwo');
    filename = strcat(filename,'_',ms_str);
    filename = strcat(filename,'_target', num2str(targetNum));
    filename = strcat(filename,'_maskFile');
    
dlmwrite(filename,mask_arr)