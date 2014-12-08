function bkgmode = remove_background_mode(series,apdim,n,data)

    bkgmode = [];
    temp = [];
    count = 1;
    
    for i = 1:apdim             % for every image
        for j = 1:n             % for all the pixels
            if ~(data(i,j) == 0 | isnan(data(i,j)))
                % if data actually exists
                temp(1,count) = data(i,j);
                count = count + 1;
                % save it
            end
        end
        % store the mode as background
        bkgmode(1,i) = mode(temp);
        % reset variables for next sweep
        temp = [];
        count = 1;
    end
    
    for count = apdim:-1;1
        % remove background from series 
        series(:,:,count) = series(:,:,count) - bkgmode(1,count);
    end
    
end