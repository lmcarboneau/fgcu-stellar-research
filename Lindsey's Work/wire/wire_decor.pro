PRO wire_decor, file, wfreq=wfreq, decor_mode=decor_mode, ndec=ndec, beaminc=beaminc, chunk=chunk

; -------------------------------------------------------------------
; read output file from wirep.pro, format FILE --> SAVE LC --> Save DECOR
; -------------------------------------------------------------------
readcol, file, t,d,w,id,x,y,b,f,format='d,d,f,i,f,f,f,f'
; time, delta-mag, weight, data-point-id, x-pos, y-position, background, fwhm
; -------------------------------------------------------------------

; -------------------------------------------------------------------
default9, decor_mode, 'xbfp'
default9, beaminc, 1.

cnt = 0L
default9, wfreq, 15.393D
default9, ndec, 4 ; number of decor. parameters = m
default9, chunk, 4000.

if decor_mode eq 'xbfp' then ndec = 4
; -------------------------------------------------------------------

; -------------------------------------------------------------------
outfile = file + '.decor'
outfile2 = file + '.wei.decor'
get_lun, u
openw,u,outfile
; -------------------------------------------------------------------


; -------------------------------------------------------------------
n = n_elements(t)
; Put data in a structure for each access:
; -------------------------------------------------------------------
data = replicate({t:-100D , d:0D , w:0., id:0L, x:0d, y:0d, b:0., f:0.}, n)
data.t = t & data.d = d & data.w = w & data.id = id & data.x = x & data.y = y & data.b=b & data.f=f
; -------------------------------------------------------------------

; -------------------------------------------------------------------
; Max number of data points is about 6000 for decor matrix
; -------------------------------------------------------------------
max_chunk = chunk
cut = ceil(n / max_chunk)
each = round((n / float(cut)/100)) * 100. ; each decor: from a part of the dataset
; -------------------------------------------------------------------


; -------------------------------------------------------------------
; Repeat decorr for each chunk until all are done
; -------------------------------------------------------------------
while (cnt lt n) do begin
help,' %%% While loop: ',cnt,' < ', n

nmax = min([cnt+each, n])
if (n-nmax) lt (each * 0.3) then begin
  nmax = n ; almost no points left?
  each = nmax - cnt
endif

print,' %%% Chunk of data being decorrelated: ' + $
 strcompress(string(cnt,format='(I6)'),/remove_all) + ' to ' + $
 strcompress(string(nmax-1,format='(I6)'),/remove_all)
 

datuse = data(cnt:nmax-1)
ndat = n_elements(datuse)

beam = fltarr(ndec)

time = datuse.t
dec = fltarr(ndat,ndec) ; decor matrix, n * m

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if decor_mode eq 'xbfp' then begin ; decor x-pos, backgr, fwhm, phase
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; X and/or Y-position
 dec(*,0) = datuse.x(0)-median(datuse.x(0)) & beam(0) = robust_sigma(dec(*,0)) * 0.2
; dec(*,1) = datuse.y(0)-median(datuse.y(0)) & beam(1) = robust_sigma(dec(*,1)) * 0.2

; BACKGROUND:
 dec(*,1) = alog10(datuse.b(0))
 dec(*,1) = dec(*,1) - median(dec(*,1)) & beam(1) = robust_sigma(dec(*,1)) * 0.2

; FWHM:
 dec(*,2) = datuse.f(0)-median(datuse.f(0)) & beam(2) = robust_sigma(dec(*,2)) * 0.1

; PHASE:
 dec(*,3) = ((time) mod (1./wfreq)) * wfreq mod 1D
 w = where(dec(*,3) lt 0,c) & if c ge 1 then dec(w,3) = dec(w,3) + 1D
 beam(3) = robust_sigma(dec(*,3)) * 0.1
 dec(*,3) = (dec(*,3) + .5D ) mod 1D
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
endif
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if decor_mode eq 'xyb' then begin ; decor x-pos, backgr, fwhm, phase
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; X and/or Y-position
 dec(*,0) = datuse.x(0)-median(datuse.x(0)) & beam(0) = robust_sigma(dec(*,0)) * 0.2
 dec(*,1) = datuse.y(0)-median(datuse.y(0)) & beam(1) = robust_sigma(dec(*,1)) * 0.2

; BACKGROUND:
 dec(*,2) = alog10(datuse.b(0))
 dec(*,2) = dec(*,2) - median(dec(*,2)) & beam(2) = robust_sigma(dec(*,2)) * 0.2

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
endif
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if decor_mode eq 'xbf' then begin ; decor x-pos, backgr, fwhm, phase
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; X position
 dec(*,0) = datuse.x(0)-median(datuse.x(0)) & beam(0) = robust_sigma(dec(*,0)) * 0.2

; BACKGROUND:
 dec(*,1) = alog10(datuse.b(0))
 dec(*,1) = dec(*,1) - median(dec(*,1)) & beam(1) = robust_sigma(dec(*,1)) * 0.2

; FWHM:
 dec(*,2) = datuse.f(0)-median(datuse.f(0)) & beam(2) = robust_sigma(dec(*,2)) * 0.1

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
endif
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if decor_mode eq 'xf' then begin ; decor x-pos, backgr, fwhm, phase
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; X position
 dec(*,0) = datuse.x(0)-median(datuse.x(0)) & beam(0) = robust_sigma(dec(*,0)) * 0.2

; FWHM:
 dec(*,1) = datuse.f(0)-median(datuse.f(0)) & beam(1) = robust_sigma(dec(*,1)) * 0.2

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
endif
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if decor_mode eq 'xb' then begin ; decor x-pos, backgr
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; X position
 dec(*,0) = datuse.x(0)-median(datuse.x(0)) & beam(0) = robust_sigma(dec(*,0)) * 0.25

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
endif
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if decor_mode eq 'b' then begin ; decor backgr only!
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; BACKGROUND:
 dec = alog10(datuse.b(0))
 dec = dec(*,0) - median(dec(*,0)) & beam(0) = robust_sigma(dec(*,0)) * 0.25

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
endif
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; Offset magnitudes:
mag = datuse.d(0) - median(datuse.d(0))

; Remove bad data
if decor_mode eq 'xbfp' then $
 g = where(abs(dec(*,0)) lt .3 and abs(dec(*,1)) lt .5 and abs(dec(*,2)) lt .05 and abs(mag) lt .1,cg)

if decor_mode eq 'xf' then $
 g = where(abs(dec(*,0)) lt .06 and abs(dec(*,1)) lt .06,cg)

if decor_mode eq 'xyb' then $
 g = where(abs(dec(*,0)) lt .03 and abs(dec(*,1)) lt .02 and abs(dec(*,2)) lt 2. and abs(mag) lt .1,cg)

; Special mode for epscep
if decor_mode eq 'xbf' then $
 g = where(abs(dec(*,0)) lt .06 and abs(dec(*,1)) lt 2. and abs(mag) lt .1 and abs(dec(*,2)) lt .2,cg); and $
;           (abs(dec(*,1)-0.02) gt .2 or abs(mag-0.02) gt .01) and $  ; SPECIAL: remove region w/ too few PTS
;           (abs(dec(*,0)+0.025) gt .01 or abs(mag+0.0037) gt .006),cg)    ; SPECIAL: remove region w/ too few PTS

if decor_mode eq 'xb' then $
 g = where(abs(dec(*,0)) lt .03 and abs(dec(*,1)) lt 2.,cg)

if decor_mode eq 'b' then $
 g = where(abs(dec) lt 2.,cg)


rr = robust_sigma(mag(g))

if ndec ge 2 then begin
 plot,mag,dec(*,0),xr=[-1,1]*rr*12.,psym=3,xtit='Magnitude',ytit='Delta X'
 dec = dec(g,*) & mag = mag(g) & time = time(g)
endif else begin
 plot,mag,dec,xr=[-1,1]*rr*12.,psym=3,xtit='Magnitude',ytit='Delta X'
 dec = dec(g) & mag = mag(g) & time = time(g)
 
endelse

beam(0) = robust_sigma(dec(*,0)) * 0.15
if ndec ge 2 then  beam(1) = robust_sigma(dec(*,1)) * 0.15
if ndec ge 3 then  beam(2) = robust_sigma(dec(*,2)) * 0.4
if ndec ge 4 then  beam(3) = robust_sigma(dec(*,3)) * 0.15
if ndec ge 5 then  beam(4) = robust_sigma(dec(*,4)) * 0.15

beam = beam * beaminc

; Plot over view of decor parameters and beam sizes
!P.multi=[0,2,3] & !P.charthick=1 & !P.charsize=1.5
col=getcolor(/load)

if ndec eq 1 then begin
  w  = where(dec lt .0,c)
  w2 = where(dec gt .0,c)
  plot, mag, dec, psym=3 & plots,!x.crange,0 & plots,!x.crange,beam(0),line=2
   oplot,mag(w), dec(w),psym=3,col=col.red
   oplot,mag(w2),dec(w2),psym=3,col=col.sky

endif
if ndec ge 2 then begin
  w  = where(dec(*,0) lt .0,c)
  w2 = where(dec(*,0) gt .0,c)
  plot, mag, dec(*,0), psym=3 & plots,!x.crange,0 & plots,!x.crange,beam(0),line=2
   oplot,mag(w),dec(w,0),psym=3,col=col.red
   oplot,mag(w2),dec(w2,0),psym=3,col=col.sky
   plot, mag, dec(*,1), psym=3 & plots,!x.crange,0 & plots,!x.crange,beam(1),line=2
   oplot,mag(w),dec(w,1),psym=3,col=col.red
   oplot,mag(w2),dec(w2,1),psym=3,col=col.sky
endif
if ndec ge 3 then begin
  plot, mag, dec(*,2), psym=3 & plots,!x.crange,0 & plots,!x.crange,beam(2),line=2
  oplot,mag(w),dec(w,2),psym=3,col=col.red
  oplot,mag(w2),dec(w2,2),psym=3,col=col.sky
endif
if ndec ge 4 then begin
  plot, mag, dec(*,3), psym=3,yr=[0,1]
     plots,!x.crange,median(dec(*,3))+0. & plots,!x.crange,median(dec(*,3)) + beam(3),line=2
  oplot,mag(w),dec(w,3),psym=3,col=col.red
  oplot,mag(w2),dec(w2,3),psym=3,col=col.sky
endif

; !P.multi=0
decorr,d,mag,dec,beam,8.

!P.multi=[0,1,2]
phase = (time mod (1./wfreq)) * wfreq & wn = where(phase lt 0.,cn) & if cn ge 1 then phase(wn)=phase(wn) + 1.
rr = robust_sigma(d(2,*))
plot,phase,d(0,*),psym=1,xsty=3,symsi=.3,yr=[-6*rr,14*rr],xr=[0,1] ;  ,xr=[.6,.7]
oplot,phase,d(2,*)+8.*rr,psym=7,col=col.sky,symsi=.3

plot,time,d(2,*),psym=1,yr=[-1,1]*rr*12,xr=median(time)+[-1,1]*.2
oplot,time,d(0,*)-0.01,psym=1,col=col.sky

wait,3

; Calculate PTP scatter

; Sort data points accordin to phase, and determine PTP scatter:
xphase = sort(phase)
ptp_robust_fin,d(0,xphase),noise_org,1
ptp_robust_fin,d(2,xphase),noise_dec,1
rms_org = robust_sigma(d(0,*))
rms_dec = robust_sigma(d(2,*))

print,' %%% Noise estimate: Sorting data by phase for better PTP estimate: '
print,' %%% PTP scatter in org/decor. data: ' + $
  strcompress(string(noise_org,format='(F7.4)'),/remove_all) + ' --> ' + $
  strcompress(string(noise_dec,format='(F7.4)'),/remove_all) + '  Ratio: ' + $
  strcompress(string(noise_org/noise_dec,format='(F6.2)'),/remove_all)
print,' %%% RMS scatter in org/decor. data: ' + $
  strcompress(string(rms_org,format='(F7.4)'),/remove_all) + ' --> ' + $
  strcompress(string(rms_dec,format='(F7.4)'),/remove_all) + '  Ratio: ' + $
  strcompress(string(rms_org/rms_dec,format='(F6.2)'),/remove_all)

; Print results to a file: 
; --> time, new-data-points, phase,decor-LC, original-data-id
nout = n_elements(d(0,*))
  for i=0L,nout-1 do $
   printf,u,time(i),d(2,i),phase(i),d(0,i)-d(2,i),datuse(g(i)).id, format='(D16.6,D10.6,D12.6,D10.6,I7)'

; Increase absolute data counter
cnt = cnt + each
help,' %%% End of while loop --- cnt and each take values: ', cnt, each

endwhile
; -------------------------------------------------------------------

; -------------------------------------------------------------------
; Close the output file
; -------------------------------------------------------------------
close,u
free_lun, u
print,' %%% Saved decor file: ', outfile
; -------------------------------------------------------------------

; -------------------------------------------------------------------
; FINALLY:
; Read the output file -- and determine point weights based on local
;                         PTP scatter:
readcol, outfile, t, d, p, dataid, format='d,d,d,f'

wire_lc_point_weight, '', ddt=ddt, outfile=outfile2, $ 
 column=-1, inpt=t,inpd=d,power=1.,/raw ; ,/debug
; -------------------------------------------------------------------

END
