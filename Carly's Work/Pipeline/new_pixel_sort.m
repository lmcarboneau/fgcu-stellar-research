format long

% folder = 0; %input('Default folder is "December 2014/new_pixel_sort". Is this correct? (y/n) ','s');
% uncomment for folder selection
%{
    if (folder == 'n') | (folder == 'N')
        folder = input('Please specify correct folder name/path: ','s');
    else
        %folder = '/home/lindsey/kplrfits/Nov_K2OI';
        folder = '/home/derek/K2 test/Lindsey/December 2014/TestData3';
    end
%}

folder = input('Assuming you are Carly on Colossus. Is this correct? (Y/N) ','s');
if (folder == 'n') || (folder == 'N')
    folder = input('Please specify correct folder name/path: ','s');
else
    folder = 'C:\Users\dmajo_000\Documents\K2Data';
end

    % uncommented for loop allows multiple files to be run at once:
  %  file = input('full file name:  ','s');
  %  file = strcat(folder,'/',file); %

kplrfiles = dir(folder);
   for fileNum = 3:(length(kplrfiles))
         
         file = kplrfiles(fileNum).name;
         file = strcat(folder,'\',file);

        [time,apdim,n,dim,series,data] = get_k2_data(file);
        
        [mean_image,count_sort,num_max,peak_locs] = mean_image_sorting2(series,dim,n);
        num_max;
        [row col] = find(peak_locs);
        temp_loc = [row col]';
        bkg = remove_background(apdim,n,count_sort,series,time);

        for count = 1:apdim
            series(:,:,count) = series(:,:,count) - bkg(count,1);
        end
       
        %Find the local peaks in the peak_locs array and send the x, y to
        %find_masksize.
        for i=1:num_max
            [x,y] = find(peak_locs,1);
            peak_locs(x,y) = 0;
            [mask_grow,masksize,aperture] = find_masksize(mean_image,count_sort,series,time,apdim,dim,n,x,y);
            [mask_arr] = mask_visualizer(mask_grow,masksize,dim,file,i);
            %find the centroid of each target
            [xcentroid,ycentroid] = calc_centriod(aperture,apdim,masksize,series,mask_grow);
        
            %put raw lightcurves out to file
            out_to_file(time,aperture,bkg,xcentroid,ycentroid,file,masksize,i);
        end
         %plot(bkg,'color',rand(1,3));
         %hold on
  end
