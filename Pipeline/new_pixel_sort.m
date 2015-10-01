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
   % folder = 'C:\Users\dmajo_000\Documents\K2Data';
   % folder = '/home/derek/Carly_Test/K2Data_Solar';
   % folder = '/home/derek/K2 test/Lindsey/December 2014/TestData2';
   % folder = '/home/derek/Carly_Test/28July2015/inputs';
   folder = '/home/derek/K2 test/Exoplanet';
end

    % uncommented for loop allows multiple files to be run at once:
  %  file = input('full file name:  ','s');
  %  file = strcat(folder,'/',file); %

kplrfiles = dir(strcat(folder,'/ktwo*'))
   for fileNum = 1:(length(kplrfiles))
     %kplrfiles = dir(folder)
   %for fileNum = 3:(length(kplrfiles))
         
         file = kplrfiles(fileNum).name;
         file = strcat(folder,'/',file);
        disp('getting data')
        [time,apdim,n,dim,series,data] = get_k2_data(file);
        disp('calling mean_image_sorting2')
        [mean_image,count_sort,num_max,peak_locs] = mean_image_sorting2(series,dim,n);
        disp('back from mean_image_sorting2')
        filename = strrep(file,'_lpd-targ.fits','');
        filename = strrep(filename,'ktwo','outputs/pipeout_ktwo');
        filename = strcat(filename,'_','mean_image');
        dlmwrite(filename, mean_image);
        %print the peak locations to array
        %as x, y, value
        filename2 = strrep(file,'_lpd-targ.fits','');
        filename2 = strrep(filename2,'ktwo','outputs/pipeout_ktwo');
        filename2 = strcat(filename2,'_','peak_locs');
        
        %dirty trick #1, if we have more than 10 we probably have a pancake
        %rather than 10 targets in a field. just a guess.
        %TODO: Detecting more than 1 target in a field of aberrants
%         if num_max > 5
%             num_max = 1;
%             flag = true;
%         end
        [row col] = find(peak_locs);
        temp_loc = [row col]';
        bkg = remove_background(apdim,n,count_sort,series,time);

        disp('Background removed')
        disp(num_max)
        
        for count = 1:apdim
            series(:,:,count) = series(:,:,count) - bkg(count,1);
        end
        
        mean_bkg = mean(bkg(:,1));
        
        %Need to move this to later in the file, and have it dump/label
        %peaks in decreasing order of value, just as they are now processed
        %below
        
         ID = fopen(filename2,'w');
         fprintf(ID,'%s %s %s\n','x','y','value');
         temp_out = zeros(1,3);
         for i = 1:size(peak_locs,1)
             for j = 1:size(peak_locs,2)
                 if peak_locs(i,j) == 1
                     temp_out = vertcat(temp_out,[i,j,mean_image(i,j)-mean_bkg]);
                     %fprintf(ID,'%f %f %8.4f\n',i,j,mean_image(i,j));
                 end
             end
         end
         temp_out = sortrows(temp_out,-3);
         for i=1:size(temp_out,1)-1
            fprintf(ID, '%d\t %d\t %f ', temp_out(i,:));
            fprintf(ID, '\n');
         end
         
         fclose(ID);
       
        %Find the local peaks in the peak_locs array and send the x, y to
        %find_masksize.
        

      % [x,y] = find(peak_locs);
      %  aa = zeros(size(peak_locs,1),size(peak_locs,2));
      %  aa = sortrows([x y mean_image(find(peak_locs))],-3)
      %  peak_locs2 = [aa(:,1),aa(:,2)];
      
        
        %resorts order of peaks to highest value to lowest
    
        for i=1:num_max
            %if flag == true
            %    x = count_sort(1,2);
            %    y = count_sort(1,3);
            %else
                %[x,y] = find(peak_locs,1)
                
                [x0 y0] = find(peak_locs); % find non-zero values, which are peaks
                locs = sub2ind(size(mean_image),x0,y0);
                v0 = mean_image(locs);
                aa = sortrows([x0,y0,v0],-3); % sort by peak value, decreasing
                x = aa(1,1); %assign locations to brightest remaining peaks
                y = aa(1,2);
                peak_locs(x,y) = 0; %sets peak location information to zero so that it doesn't get used twice
                
                
                
            %end
           %[mask_grow,masksize,aperture,apdim,time,bkg] = find_masksize(mean_image,count_sort,series,time,apdim,dim,n,x,y,bkg);
       [mask_grow,masksize,aperture] = old_find_masksize(mean_image,count_sort,series,time,apdim,dim,n,x,y);
       

       
            [mask_arr] = mask_visualizer(mask_grow,masksize,dim,file,i);
            %find the centroid of each target
            [xcentroid,ycentroid] = calc_centriod(aperture,apdim,masksize,series,mask_grow);
        
            %put raw lightcurves out to file
            out_to_file(time,aperture,bkg,xcentroid,ycentroid,file,masksize,i);
        end
         %plot(bkg,'color',rand(1,3));
         %hold on
   end

   %Need to add something which notes the number of overlapping mask pixels
   %in each mask, as a proxy for crowding
   
   %Need cleaner output format, talk with Lindsey about what's needed here
   
   %A GUI input would really be nice, along with the ability to
   %modify/choose parameters to use instead of defaults; need to make a
   %list of what those might be!
   
   %Code snippet for resorting order of peaks...
%aa = sortrows([x y mean_image(find(peak_locs))],-3)
%x = aa(:,1)
%y = aa(:,2)
%peak_locs2 = [aa(:,1),aa(:,2)] %resorts order of peaks to highest value to lowest
