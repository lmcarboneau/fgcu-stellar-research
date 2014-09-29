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
    end
end

% % using a map to make sorting and such possibly efficient, in terms of
% % memory usage and hopefully runttime
% 
% mapObj = containers.Map('KeyType','double','ValueType','any');
% 
% for i = 1:dim
%     for j = 1:dim
%         if (isnan(mean_image(i,j)))
%             mean_image(i,j) = 0;
%         end
%         mapObj(mean_image(i,j)) = [i,j];
%     end
% end

count_sort = mean_image;
count_sort = reshape(count_sort,1,n);
count_sort = sort(count_sort,'descend');

figure;
semilogx(count_sort);
figure;
image(mean_image);
figure;
image(series(:,:,1));
