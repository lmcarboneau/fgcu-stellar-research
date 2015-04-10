function bkg = remove_background(apdim,n,count_sort,series,time)
    bkg_msize = 1;      % background 'mask' size, for using more than one pixel
    bkg = zeros(bkg_msize,apdim);
    i = n;
    while (count_sort(i) == 0) | isnan(count_sort(i,1))
        i = i - 1;
    end
    
    for j = 1:bkg_msize
        for count = 1:apdim
            %bkg(j,count) = series(count_sort(i,2),count_sort(i,3),count);
            temp = series(:,:,count);
            temp = temp(:);
            temp = temp(temp>0);
            %temp = temp(:);
            %temp = mode(temp);
            %ttp = sort(temp(:))
            %bkg(j,count) = mean(ttp(1:40));
           bkg(j,count) = median(temp(:));
           temp = sort(temp);
           bkg(j,count) = median(temp(1:ceil(numel(temp)/5)));
        end
        i = i - 1;
    end
    
    %median(~isnan(temp(:)))
    %grid = [min(time(:,1)):1:max(time(:,1))]; %define a one-day grid on which to bin
    %binned = bindata(time(:,1),bkg(1,:),grid); %bin the data onto the new grid
    
    
    temp_time = time(:,1);
    temp_bkg = bkg(1,:);
    old_bkg = bkg(1,:);
    
    
    figure(223)
    plot(temp_time,temp_bkg,'.')
    %temp_time = temp_time(temp_bkg>0);
    %temp_bkg = temp_bkg(temp_bkg>0);
    
    numel(temp_time)
    numel(temp_bkg)
    
    pp = splinefit(temp_time,temp_bkg,'r',0.5);
    numel(bkg)
    bkg = ppval(pp,time(:,1));
    bkg = old_bkg';
    hold on
    plot(time(:,1),bkg,'r')
    hold off
    
    %f2 = fit(temp_time,temp_bkg','poly5','Normalize','on','Robust','on');
    %bkg = feval(f2,time(:,1));
    
    
 %   grid = [min(time(:,1)):1:max(time(:,1))]; %define a one-day grid on which to bin
 %   binned = bindata(time(:,1),bkg,grid); %bin the data onto the new grid
   % figure(2000)
   % plot(grid,binned)
    
 %   bkg = spline(grid,binned,time(:,1)); 
 %evaluate a spline based on the binned data but evaluated at the original points
    
    %bkg = bkg'; 
    
    %for count = 1:apdim
    %    % bkg(1,count) = mean(bkg(:,count));
    %    series(:,:,count) = series(:,:,count) - bkg(count,1);
    %end
    
end