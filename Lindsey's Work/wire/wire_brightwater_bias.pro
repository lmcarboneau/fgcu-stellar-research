PRO wire_brightwater_bias, wildcard, outfile=outfile,bias=bias,flat=flat,subtr=subtr

default9, outfile,'temp.fit'
default9, maxfile, 30
default9, flat, 0B ; if prg. is used with flat fields, normalize first!
default9, smooth_box, 35 ; smooth long-range variation in flat field by 35 x 35 box
                         ; and taking into account edge effects!

spawnrob,'ls -1 ' + wildcard, l
n = n_elements(l)
nmax = min([maxfile,n])

temp1 = readfits(l(0),head)
nx = n_elements(temp1(*,0))
ny = n_elements(temp1(0,*))

bias1 = fltarr(nmax,nx,ny)
bias = fltarr(nx,ny)

for i=0,nmax-1 do begin
 bias1(i,*,*) = readfits(l(i),head)

 ; if prg. is used with flat fields, normalize first!
 if flat then begin
   bias1(i,*,*) = bias1(i,*,*) - subtr ; subtract bias level!
   bias1(i,*,*) = bias1(i,*,*) / smooth(bias1(i,*,*),smooth_box,/edge)
 endif


endfor

for kx=0,nx-1 do begin
 for ky=0,ny-1 do begin

; Use robust average value if > 5 images!
  if nmax ge 5 then begin
   resistant_mean, bias1(*,kx,ky), 3, me,sd,nr
   bias(kx,ky) = me
  endif else begin
   bias(kx,ky) = avg(bias1(*,kx,ky))
  endelse

 endfor
endfor

writefits,outfile,bias,head

END
