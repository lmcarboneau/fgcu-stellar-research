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
    folder = 'C:\Users\dmajo_000\Documents\K2Data'; %I hate the _000 thing
end

    % uncommented for loop allows multiple files to be run at once:
  %  file = input('full file name:  ','s');
  %  file = strcat(folder,'/',file); %

     kplrfiles = dir(folder)
   for fileNum = 3:(length(kplrfiles))
         
         file = kplrfiles(fileNum).name
         file = strcat(folder,'\',file);
         

        [time,apdim,n,dim,series,data] = get_k2_data(file);
        
        %[mean_image,count_sort] = mean_image_sorting(series,dim,n);
        [mean_image,count_sort,num_max,peak_locs] = mean_image_sorting2(series,dim,n);
        num_max
        [row col] = find(peak_locs);
        temp_loc = [row col]'
        bkg = remove_background(apdim,n,count_sort,series,time);
        
        %bkg = remove_background_mode(data,apdim,n,data);
        for count = 1:apdim
            series(:,:,count) = series(:,:,count) - bkg(count,1);
        end
       
        %fancy data structures later, make it work NAO
        
        [x,y] = find(peak_locs,1);
        peak_locs(x,y) = 0;
        [mask_grow1,masksize1,aperture1] = find_masksize(mean_image,count_sort,series,time,apdim,dim,n,x,y);
        [x,y] = find(peak_locs,1);
        peak_locs(x,y) = 0;
        [mask_grow2,masksize2,aperture2] = find_masksize(mean_image,count_sort,series,time,apdim,dim,n,x,y);
        [x,y] = find(peak_locs,1);
        peak_locs(x,y) = 0;
        [mask_grow3,masksize3,aperture3] = find_masksize(mean_image,count_sort,series,time,apdim,dim,n,x,y);
        
        %[mask_grow,masksize,aperture] = find_masksize(mean_image,count_sort,series,time,apdim,dim,n);
        
        [xcentroid1,ycentroid1] = calc_centriod(aperture1,apdim,masksize1,series,mask_grow1);
        [xcentroid2,ycentroid2] = calc_centriod(aperture2,apdim,masksize2,series,mask_grow2);
        [xcentroid3,ycentroid3] = calc_centriod(aperture3,apdim,masksize3,series,mask_grow3);
        
        out_to_file(time,aperture1,bkg,xcentroid1,ycentroid1,file,masksize1,1);
        out_to_file(time,aperture2,bkg,xcentroid2,ycentroid2,file,masksize2,2);
        out_to_file(time,aperture3,bkg,xcentroid3,ycentroid3,file,masksize3,3);
        
         %plot(bkg,'color',rand(1,3));
         %hold on
  end
