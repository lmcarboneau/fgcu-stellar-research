function [mean_image,count_sort] = mean_image_sorting(series,dim,n)

    mean_image = zeros(dim(1),dim(2));
    % create mean image from average of 'series'
    for i = 1:dim(1)
        for j = 1:dim(2)
            mean_image(i,j) = mean(series(i,j,:));
            if isnan(mean_image(i,j))
                mean_image(i,j) = 0;
            end
        end
    end
    
    % sort pixels of mean image by size
    count_sort = zeros(n,3);
    count = 1;
    for i = 1:dim(1)
        for j = 1:dim(2)
            count_sort(count,1) = mean_image(i,j);
            count_sort(count,2) = i;
            count_sort(count,3) = j;
            count = count + 1;
        end
    end
    
    count_sort = sortrows(count_sort,1);
    count_sort = flipdim(count_sort,1);

end