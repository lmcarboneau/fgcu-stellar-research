PRO wire_ccd_flip_sat,proc,proc2,lim=lim

default9, lim, -30000.

np = n_elements(proc(0,0,*)) ; number of data points

i1 = 3 & i2 = 4
j1 = 3 & j2 = 4

cen = bytarr(8,8)
cen(3:4,3:4) = 1B
krans = bytarr(8,8)
krans(2,2:5) = 1
krans(2:5,2) = 1
krans(5,2:5) = 1
krans(2:5,5) = 1

wkrans = where(krans eq 1,ck)
wcen = where(cen eq 1,cc)


add = 2.^(16.) ;;; - 1.
proc2 = proc

for k=0L,np-1 do begin
 for i=i1,i2 do begin
  for j=j1,j2 do begin

   ccd = reform(proc2(*,*,k))
   mxkrans = median(ccd(wkrans))

   if proc(i,j,k) lt lim and mxkrans gt 2000. then $
     proc2(i,j,k) = proc(i,j,k) + add

  endfor
 endfor
endfor


END
