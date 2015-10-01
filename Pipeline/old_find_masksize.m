function [mask_grow,masksize,aperture] = old_find_masksize(mean_image,count_sort,series,time,apdim,dim,n,x1, y1)
x1
y1
% create mean image from average of 'series'
% grow by adjacent
%mask_grow = zeros(n,3); % to store pixels by brightest adj
adj_sort = zeros(n,3); % to store and sort the adj pixels
temp = mean_image; % temp image to be 'blacked out' while growing adj
mask_count = 1;
loop_count = 1;
count = 1;
superCount = 1;
%for multiple targets, find where the target is and skip
%everything brighter than it so we get a good mask
for i = 1:length(count_sort)
if(count_sort(i,2) == x1 && count_sort(i,3) == y1)
mask_count = i;
loop_count = i;
superCount = i;
break
end
end


superCount = superCount - 1;
mask_grow = zeros((n-mask_count), 3);
%cut out the bigger pixels from mask_grow...
mask_grow(1,:,:) = count_sort(mask_count,:,:);
% TODO: check that these are all needed counters,
% and if the following should be a for loop instead
%then set it back to one because Carly's dumb and just wants this done
mask_count = 1;
while (loop_count < n)
% every pixel (except all brighter than our target) in the mean image in order of greatest adjacent flux
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
if ((y+1 <= dim(2)) && (x-1 > 0) && (temp(x-1,y+1)~= -1))
adj_sort(count,:,:) = [temp(x-1,y+1),x-1,y+1];
count = count + 1;
temp(x-1,y+1) = -1;
end
if ((y-1 > 0) && (x-1 > 0) && (temp(x-1,y-1)~= -1))
adj_sort(count,:,:) = [temp(x-1,y-1),x-1,y-1];
count = count + 1;
temp(x-1,y-1) = -1;
end
if ((y+1 <= dim(2)) && (x+1 <= dim(1)) && (temp(x+1,y+1)~= -1))
adj_sort(count,:,:) = [temp(x+1,y+1),x+1,y+1];
count = count + 1;
temp(x+1,y+1) = -1;
end
if ((y-1 >0) && (x+1 <= dim(1)) && (temp(x+1,y-1)~= -1))
adj_sort(count,:,:) = [temp(x+1,y-1),x+1,y-1];
count = count + 1;
temp(x+1,y-1) = -1;
end

%% Need to fix this so that it looks at the remaining four diagonal pixels as well!

%if ((y+1 <= dim(2)) && (x ~= 0) && (temp(x,y+1)~= -1))
%adj_sort(count,:,:) = [temp(x,y+1),x,y+1];
%count = count + 1;
%temp(x+1,y+1) = -1;
%end
%if ((y+1 <= dim(2)) && (x ~= 0) && (temp(x,y+1)~= -1))
%adj_sort(count,:,:) = [temp(x,y+1),x,y+1];
%count = count + 1;
%temp(x+1,y-1) = -1;
%end

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

%std_dev = zeros(1,masksize);
%% find the standard deviation for each aperture
%for i = 1:masksize
%std_dev(1,i) = std(aperture(i,:));
%std_dev(1,i) = std_dev(1,i) / aperture(i,1);
%end
%fom = ones(20,3); % figure of merit
%% TODO: fix variables for readability/convention?
%for u = 1:3
%qu = [3,10,50];
%f = [qu(u):0.01:(qu(u)+2)];
%for i = 20:-1:1
%X = dft(time,1-aperture(i,:)./mean(aperture(i,:)),f);
%% dft(Discrete Fourier Transform) is a separate function
%fom(i,u) = mean(numel(X)*abs(X).^2);
%end
%end
%
%for i = 1:3
%[masksize(i,1),masksize(i,2)] = min(fom(:,i));
%%create list of candidate mask sizes
%end
%masksize = max(masksize(:,2));
%% choose the largest mask size from list of candidates

fom = ones(masksize,1); % figure of merit
fom2 = zeros(masksize,1); %second fom
for i = 1:masksize
    %fom(i) = std(diff(aperture(i,:)))/mean(aperture(i,:)); 
    fom(i) = std(diff(diff(aperture(i,:))))/mean(aperture(i,:));
    temp1 = aperture(i,:);
    temp1 = temp1(~isnan(temp1));
    temp2 = mean(temp1);
    temp3 = diff(diff(temp1));
    fom(i) = std(temp3)/temp2;
	%using diff gets rid of drifts and focuses on high frequencies
    
    %now see if we can set up a figure of merit to see when the mask values
    %start increasing...
    if (i>1 & i<masksize)
        temp_zero = aperture(i,:);
        temp_zero = temp_zero(~isnan(temp_zero));
        temp_p_one = aperture(i+1,:);
        temp_p_one = temp_p_one(~isnan(temp_p_one));
        temp_m_one = aperture(i-1,:);
        temp_m_one = temp_m_one(~isnan(temp_m_one));
        temp1 = sum(temp_zero)-sum(temp_m_one);
        temp2 = sum(temp_p_one)-sum(temp_zero);
        %temp1 = sum(aperture(i,:))-sum(aperture(i-1,:));
        %temp2 = sum(aperture(i+1,:))-sum(aperture(i,:));
        fom2(i) = temp2-temp1;
    end
    
end
%NEED TO MODIFY to stop adding when the aperture value starts
%increasing....looks ok, but might need to go back to check that there are
%more than one pixel in both x, y directions. OR...fix detrending so that
%it doesn't happen in a direction where there's no change. I think the
%latter is better, but we'll see...
    [minval, masksize] = min(fom(fom>0))%smallest non-zero value
    masksize2 = find(fom2>0,1) % find first positive fom2 value
    masksize = min(masksize,masksize2) %unblock
    %if (masksize < 15)
    %    masksize = 15; %minimum mask size to try to ensure that centroids are meaningful
    %end  
%masksize = 15;
masksize

end
