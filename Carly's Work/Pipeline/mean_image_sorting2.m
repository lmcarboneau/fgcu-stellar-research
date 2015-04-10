function [mean_image,count_sort,num_max,peak_locs] = mean_image_sorting2(series,dim,n)

    mean_image = zeros(dim(2),dim(1));
    % create mean image from average of 'series'
    for j = 1:dim(2)
        for i = 1:dim(1)
            mean_image(i,j) = mean(series(i,j,:));
            if isnan(mean_image(i,j))
                mean_image(i,j) = 0;
            else if (mean_image(i,j) < 0)
                    mean_image(i,j) = 0;
            end
        end
    end
    figure(222)
    imagesc(mean_image)
    colorbar
    
    %find local maxima using brute-force approach, which is OK since the
    %images are so small
    num_max = 0;
    delt = std(mean_image(:));
    peak_locs = zeros(dim(2),dim(1));
    for ii = 2+4:dim(2)-4
        for jj = 2+4:dim(1)-4
            if (mean_image(ii+1,jj)+delt <= mean_image(ii,jj))
                if (mean_image(ii-1,jj)+delt <= mean_image(ii,jj))
                    if (mean_image(ii,jj+1)+delt <= mean_image(ii,jj))
                        if (mean_image(ii,jj-1)+delt <= mean_image(ii,jj))
                            peak_locs(ii,jj) = 1;
                            num_max = num_max+1;
                        end
                    end
                end
            end
        end
    end
  
    
    % sort pixels of mean image by size
    count_sort = zeros(n,3);
    count = 1;
    for i = 1:dim(2)
        for j = 1:dim(1)
            count_sort(count,1) = mean_image(i,j);
            count_sort(count,2) = i;
            count_sort(count,3) = j;
            count = count + 1;
        end
    end
    
    count_sort = sortrows(count_sort,1);
    count_sort = flipdim(count_sort,1);
    
end