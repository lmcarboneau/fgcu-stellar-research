function bkg = remove_background(apdim,n,count_sort,series,time)
    bkg_msize = 4;      % background 'mask' size, for using more than one pixel
    bkg = zeros(bkg_msize,apdim);
    i = n;
    while (count_sort(i) == 0) | isnan(count_sort(i,1))
        i = i - 1;
    end
    
    for j = 1:bkg_msize
        for count = 1:apdim
            bkg(j,count) = series(count_sort(i,2),count_sort(i,3),count);
        end
        i = i - 1;
    end
    
    grid = [min(time(:,1)):1:max(time(:,1))]; %define a one-day grid on which to bin
    binned = bindata(time(:,1),bkg(1,:),grid); %bin the data onto the new grid
    bkg = spline(grid,binned,time(:,1)); %evaluate a spline based on the binned data but evaluated at the original points
    
    for count = 1:apdim
        % bkg(1,count) = mean(bkg(:,count));
        series(:,:,count) = series(:,:,count) - bkg(count,1);
    end
end