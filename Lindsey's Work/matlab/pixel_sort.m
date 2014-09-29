% file = '/home/lindsey/kplrfits/ktwo202071443-c002_lpd-targ.fits' 
% TODO: change to be run with 'file' as a command arg input or with a .csv

test3 = fitsread(file,'binarytable');
time = test3{1}; % data time stamp
data = test3{5}; % calibrated pixel flux
qual = test3{10}; % qualitly flag - some data is known to be questionable!

[apdim,n] = size(data); % apdim - number of images
dim = sqrt(n); % dim x dim - size of total image
% TODO: add an error handler for non-square images (check dim mod?)


temp = reshape(data(1,:,:),dim,dim); % reshape first data set to image grid
bkg(1) = min(temp(:)); % the background flux for first image
temp = temp - bkg(1); % roughly corrected: pixel flux - (some) noise
series = temp; % store image for later


% remove images where the quality is flagged non-zero
% TODO: may want to make separate array to hold removed/changed data
for i = apdim:-1:1
    if (qual(i) ~= 0)
        data(i,:) = [];
    end
end
[apdim,n] = size(data); % because the matrix had 'bad' data removed
% apdim may change, but n shouldn't. If there's a difference, that's bad


% TODO: Change series cat to pre-allocated
for i = 2:apdim % for the rest of the images in the file, do the same
    temp = reshape(data(i,:,:),dim,dim);
    bkg(i) = min(temp(:));
    temp = temp - bkg(i);
    series = cat(3,series,temp); % then stack the images back up
end


% removes images that aren't marked as 'bad' quality, but still corrupted
temp = apdim;
x = 1;
while x < temp
    if (series(fix(dim/2),fix(dim/2),x) < 1000)
        series(:,:,x) = [];
        temp = temp - 1;
    end
    x = x + 1;
end


mean_image = zeros(dim);
% create mean image from average of 'series'
for i = 1:dim
    for j = 1:dim
        mean_image(i,j) = mean(series(i,j,:));
        if isnan(mean_image(i,j))
            mean_image(i,j) = 0;
        end
    end
end



count_sort = zeros(n,3);
count = 1;
for i = 1:dim
    for j = 1:dim
        count_sort(count,1) = mean_image(i,j);
        count_sort(count,2) = i;
        count_sort(count,3) = j;
        count = count + 1;
    end
end

count_sort = sortrows(count_sort,1);
count_sort = flipdim(count_sort,1);

% figure;
% semilogx(count_sort(:,1));
% figure;
% imagesc(mean_image);
% figure;
% imagesc(series(:,:,1));


% grow by adjacent 
mask_grow = zeros(n,3);  % space for storing pixels by brightest adj
adj_sort = zeros(n,3);   % space for storing and sorting the adj pixels
temp_image = mean_image; % temp image to be 'blacked out' while growing adj

mask_count = 1; % might need this for inside a loop
loop_count = 1;
count = 1; % so many counters >.<  TODO: Check these!
mask_grow(1,:,:) = count_sort(1,:,:);

% loop might start here
while (loop_count < n)
    xy = mask_grow(mask_count,:,:);
    mask_count = mask_count + 1;
    x = xy(2);
    y = xy(3);

    if ((x > 0) && (x < dim) && (y > 0) && (y < dim))
        temp_image(x,y) = -1;
    end

    if ((x-1 > 0) && (y ~= 0) && (temp_image(x-1,y) ~= -1))
        adj_sort(count,:,:) = [temp_image(x-1,y),x-1,y];
        count = count + 1;
        temp_image(x-1,y) = -1;
    end

    if ((x+1 < dim) && (y ~= 0) && (temp_image(x+1,y)~= -1))
        adj_sort(count,:,:) = [temp_image(x+1,y),x+1,y];
        count = count + 1;
        temp_image(x+1,y) = -1;
    end

    if ((y-1 > 0) && (x ~= 0) && (temp_image(x,y-1)~= -1))
        adj_sort(count,:,:) = [temp_image(x,y-1),x,y-1];
        count = count + 1;
        temp_image(x,y-1) = -1;
    end

    if ((y+1 < dim) && (x ~= 0) && (temp_image(x,y+1)~= -1))
        adj_sort(count,:,:) = [temp_image(x,y+1),x,y+1];
        count = count + 1;
        temp_image(x,y+1) = -1;
    end
    
    adj_sort = sortrows(adj_sort,1);
    adj_sort = flipdim(adj_sort,1);
    mask_grow(mask_count,:,:) = adj_sort(1,:,:);
    adj_sort(1,:,:) = [];
    
    loop_count = loop_count + 1; % why is there no ++ operator??
    
end
