PRO wire_lc_point_weight, lcfile, ddt=ddt, outfile=outfile, $ 
 raw=raw, column=column, debug=debug, s1=s1, s2=s2, stetoff=stetoff, $
 inpt=inpt,inpd=inpd,power=power

; Compute the local RMS scatter and assign weights based on 
; 1/RMS ^ 2. First, a smoothed version of the L.C. is subtracted
; e.g if the star is variable. It may be better to subtract the
; modes using period04 first! Also, STETSON weights are multiplied
; by the local-RMS weights. The "ddt" parameter contols the width
; of the sliding time-box. 25 = include typically 50 data points.
; One WIRE orbit has ~100 data points (if binning every 31 data points).

col=getcolor(/load)

default9, ddt, 25.
default9, power, 2. ; WEIGHTS "PROP.-TO" 1/RMS^POWER
default9, raw, 0B ; save raw data points + new weights
default9, column, 3 ; three column LC file is default!
default9, debug, 0B
default9, stetoff, 0B ; do NOT use stetson weights? --> set to 1
                      ; when in doubt - try both and see the result!

; Good for wire: s1=6, s2=15
default9, s1, 6.
default9, s2, 15.

; -------------------------------------------------------------------------
; Input data: either from a file, or given as parameters to the program!
; -------------------------------------------------------------------------
if column ge 2 then begin
 if column eq 3 then $
  readcol,lcfile,t,d,w_stet,format='D,D,D' ; read times + data
 if column eq 2 then $
  readcol,lcfile,t,d,format='D,D' ; read times + data
endif ; read input from a file?

if column le 0 then begin ; data (d,t,w) is given as input
 if n_elements(inpd) ge 10 and n_elements(inpd) eq n_elements(inpt) then begin
  print,' %%% Data given as input parameters d and t -- so far so good !!'
  d = inpd & t = inpt
 endif else begin
  print,' *** Data input is given directly as keywords BUT: d NE t !!'
 endelse
endif
; -------------------------------------------------------------------------

 t0 = double(long(median(t)))

 t2 = t - t0
 print,' %%% Subtracting median time: ',t0

 sm = smooth(d, ddt*s1, /edge) ; box car smooth
 d2 = d - sm
 sm3 = smooth(d, ddt*s2, /edge) ; box car smooth
 d3 = d - sm3

if stetoff eq 0 then begin
 dat = d2 & debug_stet = 0B
 wire_stetson, dat, w_stetson, debug_stet, /silent
endif else begin
 w_stetson = d2 & w_stetson(*) = 1. / n_elements(d2)
endelse

 if debug then begin
  rms = robust_sigma(d)
  plot,t2,d,psym=1,symsi=.2,yr=[-1,1]*rms * 6.
  oplot,t2,d2+0.005,psym=1,col=col.sky,symsi=.1

  print,' %%% White points: Raw data -- blue points: subtracted smooth'

  hitme,s9 & if s9 eq 'x' then stop

  plot,t2,d,psym=1,symsi=.2,yr=[-1,1]*rms * 6.,xr=[-2.2,-1.8]
  oplot,t2,d2+0.005,psym=1,col=col.sky,symsi=.1
  hitme,s9 & if s9 eq 'x' then stop

endif

 np = n_elements(t2)
 dt = t2(1:np-1) - t2(0:np-2)
 ws = where(dt lt .1,cs)
 step = median(dt(ws)) ; typical time sted
 if step lt 1e-5 then begin
     print,' %%% step is LT 1 sec : WARNING !!'
     print,' %%% Check the times in the input file: ' + fil
     stop
 endif

 timelim = ddt * step

 cnt = 0L
 wei = fltarr(np)
 
 while (cnt lt np) do begin
  w = where( abs(t2 - t2(cnt)) lt timelim,c)
;  help,c
  if c le 2 then wei(cnt) = -1.
  if c ge 3 then wei(cnt) = robust_sigma(d2(w)) / sqrt(c - 1.) ; sqrt added NOV 2005
  cnt = cnt + 1
 endwhile

 fact = 16. ; smooth factor

 w1 = where(wei lt 0.,c1)
 w2 = where(wei gt 0.,c2)
 wei2 = wei 
 wei2(w2) = smooth(wei(w2), ddt*fact, /edge)

 if c1 ge 1 then $
  wei2(w1) = interpol( wei2(w2), t2(w2), t2(w1) )
 
 wei = 1./wei^power
 wei2 = 1./wei2^power

 wei = wei / total(wei)
 wei2 = wei2 / total(wei2)

if debug then begin
 plot,d2,wei,psym=1,yr=[0,0.006],xtit='Flux',ytit='Weight'
 hitme,s9 & if s9 eq 'x' then stop
endif

!P.charsize=1.5
!P.charthick=1

x1 = median(t2) & x2 = x1 + 1.
x1 = -2.2 & x2 = 0.3

yrr = robust_sigma(d2) * 6.

d_use = d2
wei3 = (wei2/max(wei2)) * (w_stetson/max(w_stetson))
wei3 = wei3 / total(wei3)

if debug then begin
!P.multi=[0,1,2]
 plot,t2,d_use,xr=[x1,x2],yr=[-1.,1]*yrr,psym=1,symsi=.2
 oplot,t2,(wei3/max(wei3))*yrr,psym=1,col=col.sky,symsi=.2

 plot_io,t2,wei/max(wei),psym=1,$
  xtit='Flux',ytit='Rel. Weight',xr=[x1,x2],yr=[1e-1,1.1],/nodata
 oplot,t2,wei3/max(wei3),psym=1,col=col.sky,symsi=.2

!P.multi = 0B
 hitme,s9 & if s9 eq 'x' then stop
endif

n = n_elements(wei3)
default9, outfile, '~/temp/newlc.dat'

if raw then dout = d else dout=d3

print,' %%% Adding median time: ',t0

get_lun,uu
openw,uu, outfile
for i=0L,n-1 do $
 printf,uu,t2(i)+t0, dout(i), wei3(i), $
  format='(D15.7, D15.7, D15.7)'
close,uu
free_lun,uu

print,' %%% Saved new LC w/ weights: ' + outfile

END

