function [xcentroid,ycentroid] = calc_centriod(aperture,apdim,masksize,series,mask_grow)
    
    xcentroid = zeros(apdim,1);
    ycentroid = zeros(apdim,1);
    xifi = zeros(masksize,1);
    yifi = zeros(masksize,1);
    
    for i = 1:apdim % for each image
        for j = 1:masksize % for each pixel in the mask
            pixdata = series(mask_grow(j,2),mask_grow(j,3),i); % the value of that pixel
            xifi(j) = mask_grow(j,2) * pixdata; % location in image, x
            yifi(j) = mask_grow(j,3) * pixdata; % location in image, y
        end
        tempx = sum(xifi);
        tempy = sum(yifi);
        testing = aperture(masksize, i);
        xcentroid(i) = tempx / testing;
        ycentroid(i) = tempy / testing;
        % calculates centroid based on formula sum(pos*flux)/sum(flux)
    end
    
end