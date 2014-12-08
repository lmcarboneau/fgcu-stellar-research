mask = zeros(dim(1),dim(2));
for i = 1:masksize
    mask(mask_grow(i,2),mask_grow(i,3)) = 1;
end

maskimage = mean_image .* mask;

figure(1)
ai = imagesc(mean_image);  % ai = avg image
figure(2)
mi = imagesc(maskimage);   % mi = masked image
% 
%  ainame = strrep(filename,folder,'');
%  ainame = strrep(ainame,'/outputs/pipeout_','meanimg');
% %  ainame = strcat(folder,'meanimg',pname);
%  miname = strcat(folder,'maskimg',pname);
%  
%  print(ai, '-djpeg',ainame);
% % print(mi,'-djpeg',miname);