format long

 file = input('EPIC number: ','s');
folder = input('Default folder is Nov_K2OI. Is this correct? (y/n) ','s');
if (folder == 'n') | (folder == 'N')
    folder = input('Please specify correct folder name/path: ','s');
else
    folder = '/home/lindsey/kplrfits/Nov_K2OI';
end

% uncommenting allows multiple files to be run at once, but 
% file = strcat(folder,file); 

% kplrfiles = dir(folder);
%for fileNum = 3:(length(kplrfiles) - 1)
    
    %file = kplrfiles(fileNum).name;
    file = strcat(folder,'/ktwo',file,'-c00_lpd-targ.fits');
    
    
    fitsdata = fitsread(file,'binarytable');
    time = fitsdata{1}; % data time stamp - is this actually useful?
    data = fitsdata{5}; % calibrated pixel flux
    qual = fitsdata{10}; % qualitly flag
    cnum = fitsdata{3}; % cadence number, for dealing w/ campaign 0's safe mode
    
    [apdim,n] = size(data); %#ok<NASGU> % apdim -> number of images
    dim = fitsinfo(file);
    dim = dim.Image.Size;  % dim(1) x dim(2) -> size of total image
    
    
    % remove images where the quality is flagged non-zero or is bad in general
    % Remember: Changed to remove all data from before safe mode! C#0 sp.
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

    series = zeros(dim(1),dim(2),apdim);
    for i = 1:apdim
        series(:,:,i) = reshape(data(i,:),dim(1),dim(2));
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
    
    for count = 1:apdim
        bkg(1,count) = mean(bkg(:,count));
        series(:,:,count) = series(:,:,count) - bkg(1,count);
    end
    
    
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
    
    masksize = 200; % just a baseline to start with
    
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
    
    % Find masksize
    % build a matrix full of the ratios for every mask 1 to 100
    maskfind = zeros(100,1);
    for i = 100:-1:1
        maskfind(i) = std(aperture(i,:))/mean(aperture(i,:));
    end
    % build a table to hold minimums to check - possible good masks
    % initialized to ones to search for minimums later
    temp = ones(20,2);
    count = 1;
    for i = 99:-1:2
        if (maskfind(i) < maskfind(i+1)) && (maskfind(i) < maskfind(i-1))
            temp(count,:) = [maskfind(i),i];
            count = count + 1;
            % keep this from breaking in a messy way with strange targets
            if count > 20
                break;
            end
        end
    end
    % find the minimum, that's the mask size
    [val,masksize] = min(temp(:,1));
    masksize = temp(masksize,2);
    % Stuff we did today, 5/6/14 for a better mask
    % X = dft(time,aperture(1,:)./mean(aperture(1,:)),f);
    % fom = mean(abs(X))
    
    
    
    xcentroid = zeros(apdim,1);
    ycentroid = zeros(apdim,1);
    xifi = zeros(masksize,1);
    yifi = zeros(masksize,1);
    
    for i = 1:apdim
        %put stuff here
        for j = 1:masksize
            pixdata = series(mask_grow(j,2),mask_grow(j,3),i); % the value of that pixel
            xifi(j) = mask_grow(j,2) * pixdata;
            yifi(j) = mask_grow(j,3) * pixdata;
        end
        tempx = sum(xifi);
        tempy = sum(yifi);
        xcentroid(i) = tempx / aperture(masksize,i);
        ycentroid(i) = tempy / aperture(masksize,i);
    end
    
    % all sorts of things, now in .txt form!
%     
%     output_data = [time'; aperture(masksize,:);bkg(1,:); xcentroid'; ycentroid';];
%     filename = strrep(file,'_lpd-targ.fits','');
%     filename = strrep(filename,'ktwo','/outputs/pipeout_ktwo');
%     
%     ID = fopen(filename,'w');
%     fprintf(ID,'%s %s:%d %s:%d %s %s\n','Time','Flux - Mask',masksize,'Background',bkg_msize,'X Cent','Y Cent');
%     fprintf(ID,'%8.4f %f %f %f %f\n',output_data);
%     fclose(ID);
    
    figure(1)
    plot(time,aperture(masksize,:))
    % hold on;
    % plot(time,ycentroid,'g');
    % hold off;
%end