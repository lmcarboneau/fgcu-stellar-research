PRO wire_lc_point_night_weight, lcfile, outfile=outfile, $ 
 column=column, debug=debug, dtt=dtt, power=power, tjump=tjump,d9=d9,submean=submean,avorbit=avorbit,$
  subper=subper,fsub=fsub,d2=d2,t2=t2,dwei=dwei

; Find jumps in time string of > 0.2 days, split each group of
; data points (each night or orbit) in three, and calculate the RMS noise
; after subtracting a smoothed version of the LC.
; Assign point weights based on the measured noise 
;
; IMPORTANT:
; Program orig. optimized for the WIRE data. Can it be used for GB data?
;
; Example:
;wire_lc_point_night_weight, 'wire_Feb05.dat',$
;                    outfile='wire_Feb05_wei.dat',$
;                    column=3,power=2., tjump = .04,$
;  subper='/export/brixx2/bruntt/wire_analysis/acir/master_per/ACir_Feb2005_s0_HD128898_Ap.per',/debug
;
; subper = output from period04
;


default9,'~/temp_lc.dat'
default9, tjump,  0.2
default9, dtt, 9
default9, power, 2.
default9, debug, 0B

default9, submean, 0B  ; in case you want to subr. mean of each orbit
default9, avobrit, 1.  ; average more orbits (only appl. if submean=1)
default9, subper, ''   ; subtract these periods before calculating scatter?

col=getcolor(/load)

!P.charthick=1

if column eq 3 then $
 readcol,lcfile,t,d,w_stet,format='D,D,D' ; read times + data
if column eq 2 then $
 readcol,lcfile,t,d,format='D,D' ; read times + data

 t0 = double(long(median(t)))

; Sort times 
s = sort(t) & t2 = t(s) & d2 = d(s)
n = n_elements(t)

fsub = dblarr(n)
if subper ne '' then begin
 readcol,subper,aa,f,a,p, format='a,d,d,d'
 nm = n_elements(f) & if nm eq 0 then stop
 for k=0,nm-1 do fsub = fsub + a(k) * sin(2D * !DPI * (f(k)*t2 + p(k)))
 print,' %%% I will subtract ',nm, ' modes '

 if debug then begin
   col=getcolor(/load)
   rr = robust_sigma(d2)
   !P.multi=[0,1,2]
   plot,t2,d2,psym=3,yr=[-1,1]*rr*5.
     oplot,t2,fsub,col=col.red
   plot,t2,d2-fsub,psym=3,yr=[-1,1]*rr*5.
     hitme,s9,mess = 'Red is the fitted LC. White is the observations. Hit any key to go on.'
     if s9 eq 'x' then stop
   !P.multi=0
 endif
endif



tstep = (t(1:n-1) - t(0:n-2)) ; typical time step
tmed = median(tstep)

wend2 = where(tstep gt tjump,c)
wend = findgen(c+1) & wend(0:c-1) = wend2 & wend(c) = n-1
wstart = findgen(c+1) & wstart(0) = 0 & wstart(1:c) = wend2 + 1

dsm = d2 & dsm(*) = 0.
d9 = dsm
dwei = dsm

; Debugger, wire data:
if debug then begin
 colx = [col.sky,col.red,col.magenta,col.aqua,col.charcoal] & ncolx = n_elements(colx)
 i = 0
 plot,t2(wstart(i):wend(i)),d2(wstart(i):wend(i))-fsub(wstart(i):wend(i)),psym=1,$
   xr=[t2(wstart(i)),t2(wend(i))]+[-.1,1]*.4,yr=[-1,1]*rr*5.
 for i=1,8 do $
  oplot,col=colx(i mod ncolx),t2(wstart(i):wend(i)),d2(wstart(i):wend(i))-fsub(wstart(i):wend(i)),psym=1
 hitme,s9,mess=' Colors show the groups of points identified by the program!
endif

print,' %%% Finding the weight for each orbit based on the local RMS after subtr. a smoothed LC.' 

for i=0,c do begin

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 time = t2(wstart(i):wend(i))
 data = d2(wstart(i):wend(i)) - fsub(wstart(i):wend(i))
 sm = smooth(data,dtt,/edge)
 rms = robust_sigma(data-sm)
 dwei(wstart(i):wend(i)) = 1.0 / rms^power
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 resistant_mean,data,3,me9,sd9,nr9
 d9(wstart(i):wend(i))  = me9
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 if debug and i lt 3 then begin
  !P.multi=[0,1,2]
  plot,time,data,yr=[-1,1]*rr*5.,psym=1,xsty=3,tit='+ = data (after subtr. modes). Blue = smoothed curve'
   oplot,time,sm,thick=2,col=col.sky
  plot,time,data-sm,yr=[-1,1]*rr*5.,psym=1,xsty=3,tit='Subtr. blue curve. Red dash = 1 sigma. Weight based on this.'
;  plots,!x.crange, rms*3.,line=2,col=col.red
;  plots,!x.crange,-rms*3.,line=2,col=col.red
  plots,!x.crange, rms*1.,line=5,col=col.red
  plots,!x.crange,-rms*1.,line=5,col=col.red
  plots,!x.crange,0.,col=col.red
  hitme,s9,mess = ' >>> I will plot three orbits ... Hit any key (x=stop)!' & if s9 eq 'x' then stop
 endif
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++


endfor



; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; d9 is the mean light curve:
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if submean then dout = d2 - smooth(d9,avorbit,/edge) else dout = d2
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Normalize the weights:
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dwei = dwei / total(dwei)
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!P.multi=0
if debug then plot,d2-d9-fsub,dwei/max(dwei),xr=[-1,1]*rr,yr=[0,1.05],psym=3


; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Export the light curve:
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
openw,1, outfile
for i=0L,n-1 do $
 printf,1,t2(i), dout(i), dwei(i), $
  format='(D15.7, D15.7, D15.7)'
close,1
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

print,' %%% Saved LC with nightly weights: ' + outfile

END

