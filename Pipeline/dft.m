function X=dft(t,x,f)
% function X=dft(t,x,f)
% Compute DFT (Discrete Fourier Transform) at frequencies given
%   in f, given samples x taken at times t:
%     X(f) = sum { x(k) * e**(2*pi*j*t(k)*f) }
%             k

shape = size(f);
t = t(:); % Format 't' into a column vector
x = x(:); % Format 'x' into a column vector
f = f(:); % Format 'f' into a column vector


% It's just this simple:
W = exp(-2*pi*j * f*t');
X = W * x;
X = reshape(X,shape);