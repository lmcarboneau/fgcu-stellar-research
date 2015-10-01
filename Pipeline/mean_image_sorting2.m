function [mean_image,count_sort,num_max,peak_locs] = mean_image_sorting2(series,dim,n)

    mean_image = zeros(dim(1),dim(2));
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
    end
    figure(222)
    %imagesc(log10(mean_image))
    imagesc(mean_image)
    colorbar
    
    %find local maxima using brute-force approach, which is OK since the
    %images are so small
    num_max = 0;
    temp1 = mean_image(:);
    temp1 = sort(temp1);
    %delt = 10*std(temp1(1:ceil(numel(temp1)/2)))
    %if campaign 1
    %delt = std(mean_image(:))
    %if campaign 0 
    %delt = 10*std(temp1(1:ceil(3*numel(temp1)/4)))
    delt = 0.5*std(temp1(temp1<3*std(temp1)))
    
    peak_locs = zeros(dim(1),dim(2));
    for ii = 2:dim(1)-2
        for jj = 2:dim(2)-2
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
    for j = 1:dim(2)
        for i = 1:dim(1)
            count_sort(count,1) = mean_image(i,j);
            count_sort(count,2) = i;
            count_sort(count,3) = j;
            count = count + 1;
        end
    end
    
    count_sort = sortrows(count_sort,1);
    count_sort = flipdim(count_sort,1);
    end