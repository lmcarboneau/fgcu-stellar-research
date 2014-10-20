% file = '/home/lindsey/kplrfits/ktwo202071443-c002_lpd-targ.fits' 
% TODO: change to be run with 'file' as a command arg input or with a csv

fitsdata = fitsread(file,'binarytable');
time = fitsdata{1}; % data time stamp - is this actually useful?
data = fitsdata{5}; % calibrated pixel flux
qual = fitsdata{10}; % qualitly flag
cnum = fitsdata{3}; % cadence number, for dealing w/ campaign 0's safe mode

[apdim,n] = size(data); %#ok<NASGU> % apdim -> number of images
 dim = fitsinfo(file);
 dim = dim.Image.Size;  % dim(1) x dim(2) -> size of total image


% remove images where the quality is flagged non-zero or is bad in general
% TODO: Change to remove all data from before safe mode! C#0 sp.
ind = find(cnum==89347); % get the index of the first good cadence
rem_rows = [1:ind];

for i = apdim:-1:ind
    if (qual(i) ~= 0)
        rem_rows = [rem_rows,i];
        
    else if (max(data(i,:)) < 1000)
            rem_rows = [rem_rows,i];
        end
    end
end

[data] = removerows(data,'ind',rem_rows);
[time] = removerows(time,'ind',rem_rows);
[cnum] = removerows(cnum,'ind',rem_rows);
[apdim,n] = size(data); 
% because the matrix had 'bad' data removed
% apdim will change, but n shouldn't. If there's a difference, that's bad
% TODO: Deal with that case with error handler?



temp = data;
bkg = zeros(1,apdim);
for i = 1:apdim
    bkg(1,i) = min(temp(i,:));
    temp(i,:) = temp(i,:) - bkg(1,i);
end

series = zeros(dim(1),dim(2),apdim);
for i = 1:apdim
    series(:,:,i) = reshape(temp(i,:),dim(1),dim(2));
end


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

% figure;
% semilogx(count_sort(:,1));
% figure;
% imagesc(mean_image);
% figure;
% imagesc(series(:,:,1));


% grow by adjacent 
mask_grow = zeros(n,3);  % space for storing pixels by brightest adj
adj_sort = zeros(n,3);   % space for storing and sorting the adj pixels

temp = mean_image; % temp image to be 'blacked out' while growing adj
% temp changes definitions here

mask_count = 1; % might need this for inside a loop
loop_count = 1;
count = 1; % so many counters >.<  TODO: Check these!
mask_grow(1,:,:) = count_sort(1,:,:);

% TODO: Can this be a for loop here?
while (loop_count < n)
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

masksize = 20; %input('Enter the maximum mask size to check: ') <- auto now
% temp = zeros(1,n);
aperture = zeros(masksize,apdim);
% Possibly misguided atempt to do things: 
% for x = 1:apdim
%     for a = 1:200
%         for i = 1:a
%             if (mask_grow(i,1) == 0)
%                 break
%             end
%             temp(i) = series(mask_grow(i,2),mask_grow(i,3),x);
%         end
%         aperture(a,x) = sum(temp(1,:));
%     end
% end
for i = 1:apdim
    aperture(1,i) = series(mask_grow(1,2),mask_grow(1,3),i);
end

for a = 2:masksize
    for b = 1:apdim
        aperture(a,b) = aperture((a-1),b) + series(mask_grow(a,2),mask_grow(a,3),b);
    end
end


std_dev = zeros(1,masksize);
for i = 1:masksize
    std_dev(1,i) = std(aperture(i,:));
    std_dev(1,i) = std_dev(1,i) / aperture(i,1);
end

% semilogx(std_dev);  % don't really need to see it every time


% 
% count = 1;
% while count < masksize 
%     if std_dev(count) < std_dev(count + 1)
%         masksize = count;
%     else
%         count = count + 1;
%     end
% end
fix = 1:1:masksize;
temp = polyval(std_dev,fix);
temp = polyfit(fix,temp,2);
fix = polyval(temp,fix);
[temp,masksize] = min(fix);

xcentroid = zeros(apdim,1);
ycentroid = zeros(apdim,1);
xifi = zeros(masksize,1);
yifi = zeros(masksize,1);

for i = 1:apdim
    %put stuff here
    for j = 1:masksize
        pixdata = mask_grow(j,1); % the value of that pixel
        xifi(j) = mask_grow(j,2) * pixdata;
        yifi(j) = mask_grow(j,3) * pixdata;
    end
    tempx = sum(xifi);
    tempy = sum(yifi);
    xcentroid(i) = tempx / aperture(masksize,i);
    ycentroid(i) = tempy / aperture(masksize,i);
end

output_data = [cnum,time,aperture(masksize,:)',xcentroid,ycentroid];
filename = strrep(file,'_lpd-targ.fits','');
filename = strrep(filename,'ktwo','pipeout_ktwo');
dlmwrite(filename,['cadence  time    flux   xcent   ycent'],'delimiter','');
dlmwrite(filename,output_data,'-append','delimiter','\t','newline','unix');

figure(1)
plot(xcentroid,ycentroid,'r');
figure(2)
plot(time,xcentroid,'b');
hold on;
plot(time,ycentroid,'g');