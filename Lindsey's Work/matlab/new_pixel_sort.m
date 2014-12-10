format long

folder = 0; %input('Default folder is Nov_K2OI. Is this correct? (y/n) ','s');
% uncomment for folder selection
    if (folder == 'n') | (folder == 'N')
        folder = input('Please specify correct folder name/path: ','s');
    else
        folder = '/home/lindsey/kplrfits/Nov_K2OI';
        %folder = '/home/derek/K2 test/Lindsey/Aarhus meeting/Code';
    end

    % uncommented for loop allows multiple files to be run at once:
  %  file = input('full file name:  ','s');
  %  file = strcat(folder,'/',file); %

     kplrfiles = dir(folder);
   for fileNum = 4:(length(kplrfiles) - 3)
         
         file = kplrfiles(fileNum).name;
         file = strcat(folder,'/',file);
         

        [time,apdim,n,dim,series,data] = get_k2_data(file);
        
        [mean_image,count_sort] = mean_image_sorting(series,dim,n);
        
        bkg = remove_background(apdim,n,count_sort,series,time);
        
        %bkg = remove_background_mode(data,apdim,n,data);
        
        
        [mask_grow,masksize,aperture] = find_masksize(mean_image,count_sort,series,time,apdim,dim,n);
        
        [xcentroid,ycentroid] = calc_centriod(aperture,apdim,masksize,series,mask_grow);
        
        out_to_file(time,aperture,bkg,xcentroid,ycentroid,file,masksize);
        
%         plot(bkg,'color',rand(1,3));
%         hold on
  end