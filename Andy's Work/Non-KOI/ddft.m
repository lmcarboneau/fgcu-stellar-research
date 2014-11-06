function X=ddft(t,x,f)
% function X=dft(t,x,f)
% Compute DFT (Discrete Fourier Transform) at frequencies given
%   in f, given samples x taken at times t:
%     X(f) = sum { x(k) * e**(2*pi*j*t(k)*f) }
%             k

%28-29 December 2010: modify code to break inputs into 1000-point chunks
%for time series longer than 1500 points

if (numel(f) > 100)
    num_segs = int16(numel(f)/100); %note int8 rounds rather than truncates
    num_segs = max(1,num_segs); %ensure at least one segment is used!
else
    num_segs = 1;
end

%now set up each segment, one at a time
for i=1:num_segs-1
    start_point = (int32(i-1)*100 + 1);
    end_point = (start_point+99);
    ff(i,:) = f(start_point:end_point);
end
last_seg=f(end_point+1:numel(f));

%Now call dft code on each ff segment

for i=1:num_segs-1
    XX(i,:) = dft(t,x,ff(i,:));
end
last_XX = dft(t,x,last_seg);

%Now concatenate all of the segments to make the completed dft
X = XX(1,:);
for i = 2:num_segs-1
    X = horzcat(X,XX(i,:));
end
X = horzcat(X,last_XX);
    
%shape = size(f);
%t = t(:); % Format 't' into a column vector
%x = x(:); % Format 'x' into a column vector
%f = f(:); % Format 'f' into a column vector


% It's just this simple:
%W = exp(-2*pi*j * f*t');
%X = W * x;
%X = reshape(X,shape);



