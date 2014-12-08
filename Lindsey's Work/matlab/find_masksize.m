function [mask_grow,masksize,aperture] = find_masksize(mean_image,count_sort,series,time,apdim,dim,n)

    % create mean image from average of 'series'

    % grow by adjacent
    mask_grow = zeros(n,3);  % to store pixels by brightest adj
    adj_sort = zeros(n,3);   % to store and sort the adj pixels

    temp = mean_image; % temp image to be 'blacked out' while growing adj

    mask_count = 1;
    loop_count = 1;
    count = 1;
    mask_grow(1,:,:) = count_sort(1,:,:);
    % TODO: check that these are all needed counters,
    % and if the following should be a for loop instead

    while (loop_count < n)
        % every pixel in the mean image in order of greatest adjacent flux
        xy = mask_grow(mask_count,:,:);
        mask_count = mask_count + 1;
        x = xy(2);
        y = xy(3);

        if ((x > 0) && (x <= dim(1)) && (y > 0) && (y <= dim(2)))
            temp(x,y) = -1;
        end

        if ((x-1 > 0) && (y ~= 0) && (temp(x-1,y) ~= -1))
            adj_sort(count,:,:) = [temp(x-1,y),x-1,y];
            count = count + 1;
            temp(x-1,y) = -1;
        end

        if ((x+1 <= dim(1)) && (y ~= 0) && (temp(x+1,y)~= -1))
            adj_sort(count,:,:) = [temp(x+1,y),x+1,y];
            count = count + 1;
            temp(x+1,y) = -1;
        end

        if ((y-1 > 0) && (x ~= 0) && (temp(x,y-1)~= -1))
            adj_sort(count,:,:) = [temp(x,y-1),x,y-1];
            count = count + 1;
            temp(x,y-1) = -1;
        end

        if ((y+1 <= dim(2)) && (x ~= 0) && (temp(x,y+1)~= -1))
            adj_sort(count,:,:) = [temp(x,y+1),x,y+1];
            count = count + 1;
            temp(x,y+1) = -1;
        end

        adj_sort = sortrows(adj_sort,1);
        adj_sort = flipdim(adj_sort,1);
        mask_grow(mask_count,:,:) = adj_sort(1,:,:);
        adj_sort(1,:,:) = [];

        loop_count = loop_count + 1; % why is there no ++ operator??

    end
    
    % mask_grow is the results matrix after the loop
    % used to calculate the appropriate masksize:
    
    masksize = 200; % just a baseline to start with
    aperture = zeros(masksize,apdim);
    % holds time series for every possible mask sized 1 to 200 pixels

    for i = 1:apdim
        % mask size of 1 = flux of pixel at posistion in image
        % from greatest in average image, stored in mask_grow
        aperture(1,i) = series(mask_grow(1,2),mask_grow(1,3),i);
    end

    for a = 2:masksize
        % for each new size, add value of new pixel
        % in mask to previously calculated mask
        for b = 1:apdim
            aperture(a,b) = aperture((a-1),b) + series(mask_grow(a,2),mask_grow(a,3),b);
        end
    end


    std_dev = zeros(1,masksize);
    % find the standard deviation for each aperture
    for i = 1:masksize
        std_dev(1,i) = std(aperture(i,:));
        std_dev(1,i) = std_dev(1,i) / aperture(i,1);
    end

    fom = ones(100,3); % figure of merit
   
    % TODO: fix variables for readability/convention?
    for u = 1:3
        qu = [3,10,50];
        f = [qu(u):0.01:(qu(u)+2)];
        for i = 200:-1:1
            X = dft(time,1-aperture(i,:)./mean(aperture(i,:)),f);
            % dft(Discrete Fourier Transform) is a separate function
            fom(i,u) = mean(numel(X)*abs(X).^2);
        end
    end

    for i = 1:3
        [masksize(i,1),masksize(i,2)] = min(fom(:,i));
        %create list of candidate mask sizes
    end
    
    masksize = max(masksize(:,2));
    % choose the largest mask size from list of candidates

end