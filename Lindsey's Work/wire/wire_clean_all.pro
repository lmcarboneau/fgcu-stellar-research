PRO wire_clean_all,filename,unit,f1,f2,nfreq,freq,dat,$
 timeunit=timeunit,ampunit=ampunit,highres=highres,mode=mode,$
 discard=discard

; Clean spectrum for frequencies in the range f1 to f2
; Input file needs to have three columns: 
; Time (days), delta-mag, and point weights (total sum == 1)

; You need to compile: g77 -o ~/bin/clean ~/wire/software/clean.f
; Thus the output file will be the file: ~/bin/clean

; This program uses Hans Kjeldsen clean fortran code
; IDL version by HB made in the Spring 2005.

n_iter = 1
if n_elements(highres) eq 0 then highres = 1.0 ; higher res in ampl. spectra?

default9, discard, 0B

; friter = [3,3.9,5.1]

ff = findfile(filename,Count=cnt)
if cnt ne 1 then begin
 print,''
 print,' *** Light curve file not found: '+filename
 print,''
 hitme, s9 & if s9 eq 'x' then stop
 RETURN
endif

readcol,filename,t,d,w,format='D,D,D'
if discard then wg = where(w/max(w) gt 0.0001,c) else $
  wg = findgen(n_elements(t)) ; discard low weight points?
t = t(wg) & d = d(wg) & w = w(wg)/total(w(wg))
t0 = median(t) & d0 = median(d)

; =================================================================
fac  = 1e6 / 86400D
fac2 = 1e3 / 86400D
tfac = 86400D
; =================================================================

; =================================================================
if n_elements(timeunit) eq 1 then begin
  if timeunit eq 'seconds' then tfac = 1D
endif else begin
  timeunit = 'days'
  tfac = 86400D
endelse

if n_elements(ampunit) eq 1 then begin
  if ampunit eq 'ppm' then ampfac = 1D
  if ampunit eq 'ppt' then ampfac = 1D * double(1e-3)
  if ampunit eq ''    then ampfac = 1D * double(1e-6)
endif else begin
  ampunit = 'ppm'
  ampfac = 1D
endelse

; =================================================================

; =================================================================
f = 'c/day' 
facuse = 1.0 ; freq. conversion factor

if strmatch(unit,'*micro*',/fold_case) eq 1 then begin
 f = 'microHz'
 facuse = fac
endif

if strmatch(unit,'*milli*',/fold_case) eq 1 then begin
 f = 'milliHz'
 facuse = fac2
endif

 f1use = f1 / facuse ; frequencies in cycles pr. day
 f2use = f2 / facuse
; =================================================================

if n_elements(mode) eq 0 then $
 mode = 'SaveAll' ; ONLY THIS MODE WORKS 100% --> REITERATE TO GET ACC. FREQ!


; =================================================================
outtext = ' %%% Clean '+strcompress(string(nfreq,format='(I8)'),/remove_all) + $
      ' freq.s in range ' + $
      strcompress(string(f1use,format='(F9.1)'),/remove_all) + ' to ' + $
      strcompress(string(f2use,format='(F9.1)'),/remove_all) + ' c/day, ie. ' + $
      strcompress(string(f1,format='(F9.1)'),/remove_all) + ' to ' + $
      strcompress(string(f2,format='(F9.1)'),/remove_all) + ' ' + f

if f eq 'c/day' then $
outtext = ' %%% Clean '+strcompress(string(nfreq),/remove_all) + $
      ' freq. in range ' + $
      strcompress(string(f1use,format='(F9.1)'),/remove_all) + ' to ' + $
      strcompress(string(f2use,format='(F9.1)'),/remove_all) + ' c/day'
 

print,outtext
; =================================================================

; =================================================================
; Calculate nominal frequency resolution
t_total      = (max(t) - min(t)) * tfac ; total obs. time in seconds
f_resolution = 1e6 / t_total            ; resolution in microHz (often quoted in papers)

; =================================================================
f_min = (2.0 / t_total) * tfac ; the min freq. you can measure [c/day]
f1use2 = f_min / facuse ; frequencies in cycles pr. day
if f1use lt f1use2 then begin
  print,' %%% RE-Setting min frequency to: ' + strcompress(f1use2,/remove_all) + ' ' + f
  f1use = f1use2
endif

; =================================================================

; Take four points per. freq. resolution element
np = highres * 3.0 * (f2use - f1use) * fac / f_resolution
np = ceil(np / 1000.) * 1000. + 1.

; nphigh = 10.0 * 0.1 * (f2use - f1use) * fac / f_resolution
; nphigh = ceil(nphigh / 1000.) * 1000. + 1.

print,' %%% Resolution in Amp. Spectrum [microHz]: ' + $
 strcompress(string(f_resolution,format='(F9.3)'),/remove_all) ; ,f_resolution
print,' %%% Number of points in FFT calc: ' + string(np)

; =================================================================







; =================================================================
t1 = t * tfac      ; convert times to seconds
d1 = d - d0        ; remove any offset in magnitude
w1 = w             ; weights
n = n_elements(d1) ; number of data points

; Sort times ...
a = sort(t1) & t1 = t1(a) & d1 = d1(a) & w1 = w1(a)

temp_fil  = 'temp_data.dat'
temp_fil2 = 'temp_data_input.dat'

get_lun,u
openw,u,temp_fil

w1out = w1
d1out = d1

for i=0L,n-1 do $
 printf,u, t1(i), d1out(i), w1out(i), format='(D18.7, D15.7, D15.7)'

close,u
free_lun,u


; =================================================================
if mode eq 'SaveLast' then begin

spawnrob,'rm -f *clean*.dat spec*.dat'

 f1micro = f1use*1e6/86400
 f2micro = f2use*1e6/86400
 npuse = np

get_lun,u
openw,u,temp_fil2

printf,u,temp_fil
printf,u,'1' ; weights on ? 0 or 1
printf,u,'1' ; weight exponent
printf,u,strcompress(string(f1micro,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(f2micro,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(npuse,format='(I10)'),/remove_all) ; # points in spectrum
printf,u,strcompress(string(nfreq,format='(I6)'),/remove_all) ; # points for clea

close,u
free_lun,u

spawnrob,'clean < ' + temp_fil2, out


readcol, 'series.dat', tn, dn, wn, format='D, F, F'
ntt = n_elements(tn)
dat = replicate( {t:0D, d:0., w:0.}, ntt)
dat.t = (tn / tfac) & dat.d = dn + d0 & dat.w = wn

readcol, 'clean.dat', nr, aa, ff, ph, format='(I, F, F, F'
nff = n_elements(nr)

freq = replicate( {nr:0L, f:0D, a:0D, p:0D}, nff)
freq.nr = nr & freq.f = ff & freq.a = aa & freq.p = ph ; /(2.*!DPI)

endif
; =================================================================



; =================================================================
if mode eq 'SaveAll' then begin

nff = nfreq
ntt = n_elements(t)
dat = replicate( {t:0D, d:fltarr(nfreq), w:0.}, ntt)

freq = replicate( {nr:0L, f:0D, a:0D, p:0D, $
 freq:fltarr(np), amp:fltarr(np), phase:fltarr(np) }, nff)

for i=0,nfreq-1 do begin ; for each frequency

iter = 0
spawnrob,'rm -f *clean*.dat spec*.dat'

reiterate:

get_lun,u
openw,u,temp_fil2

if i eq 0 then begin ; the first frequency
 if iter eq 0 then begin
  readcol, temp_fil, tn2, dn2, wn2, format='D, D, D'
 endif

  tt2 = (tn2 / tfac) & dat2 = dn2 - median(dn2) 
  wei2 = wn2 / max(wn2)
  ; printf,u,temp_fil
  printf,u,'series_cleaned.dat'

 endif else begin
  if iter eq 0 then begin
   readcol, 'series.dat', tn2, dn2, wn2, format='D, D, D'
  endif
 tt2 = (tn2 / tfac) & dat2 = dn2 - median(dn2) 
 wei2 = wn2 / max(wn2)
 printf,u,'series_cleaned.dat'

endelse

  get_lun,u6
  openw,u6,'series_cleaned.dat'  
  nn = n_elements(tt2)
  for k=0L,n-1 do $
   printf,u6, tt2(k)*tfac, dat2(k), wei2(k), format='(D18.7, D15.7, D15.7)'
  close,u6
  free_lun,u6


; Calculate (new) amplitude spectrum:
if iter eq 0 then begin
 minfreq = f1use & maxfreq = f2use ; frequencies in cycles pr. day
 ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,freq2,amp2,phase2,highres=highres
 ; plot,freq2,amp2,psym=-6,symsi=.3,xr=[-1,1]*.2 + friter(i)
endif

; Time is always in seconds in the clean.f program, 
; hence the freq range is given in microHz
if iter eq 0 then begin
 f1micro = f1use*1e6/86400
 f2micro = f2use*1e6/86400
 npuse = np
endif
if iter ge 1 then begin
 f1micro = ff(0) - f_resolution * 1.2 ; freq range in microHz
 f2micro = ff(0) + f_resolution * 1.2 ; freq range in microHz
 error_mhz = f_resolution * 0.002 ; accepted error in microHz
 npuse = (f2micro -  f1micro) / error_mhz
 npuse = ceil(npuse/100.) * 100. + 1
 print,' %%% Amp. spec. resolution in microHz: ',error_mhz
 ; stop
 ; print,npuse
 ; stop
endif
if f1micro lt 0.02 then f1micro = 0.02 ; 0.1 microHz = 0.01 c/d

printf,u,'1' ; weights on ? 0 or 1
printf,u,'1' ; weight exponent
printf,u,strcompress(string(f1micro,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(f2micro,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(npuse,format='(I10)'),/remove_all) ; # points in spectrum
printf,u,strcompress(string(1,format='(I6)'),/remove_all) ; # points for clea

;if iter ge 1 then begin
;  tt2 = (tn2 / tfac) & dat2 = dn2 - median(dn2) 
;  wei2 = wn2 / max(wn2)
;  ampl_spec_calc_wire_rv,tt2,dat2,wei2,f1micro/11.57,f2micro/11.57,freq2a,amp2a,phase2a
;  col=getcolor(/load)
;  oplot,freq2a,amp2a,psym=-6,col=col.red,symsi=.2
;endif

close,u
free_lun,u

spawnrob,'~/bin/clean < ' + temp_fil2, out
klo = where(strmatch(out,'*Abort*'),c_klo)
if c_klo ge 1 then begin
 iter = iter + n_iter + 100 ; exit
 goto,bad_iter
endif

; Read the new time series after cleaning of ONE frequency:
readcol, 'series.dat', tn, dn, wn, format='D, D, D'
ntt = n_elements(tn)
dat.t = (tn / tfac) & dat.d(i) = dn + d0 & dat.w = wn

np2 = n_elements(freq2)

if iter eq 0 then begin ; save the amplitude spectrum (first iteration)
 freq(i).freq(0:np2-1)  = freq2 * facuse ; convert from c/day to the right unit
 freq(i).amp(0:np2-1)   =  amp2 * ampfac ; convert to the desired unit
 freq(i).phase(0:np2-1) = phase2
endif


readcol, 'clean.dat', nr, aa, ff, ph, format='I, D, D, D'
nff = n_elements(nr)

ff_unit = (ff / fac) * facuse ; convert freq to the right unit

; print, ' >>> Iter = ',iter, ff(0)/fac, aa(0), ph(0)/(2.*!DPI)

; =================================================================
; REITERATE TO GET ACCURATE FREQUENCY + AMPL + PHASE:
; =================================================================
bad_iter:
 iter = iter + 1
 if iter le n_iter then goto,reiterate

; print, ' ------------------------------- '
; =================================================================

if n_elements(nr) ne 0 then begin
 freq(i).nr = nr                & freq(i).f = ff_unit
 freq(i).a  = aa * 1e6 * ampfac & freq(i).p = ph
endif else print,' *** WARNING: NO RESULTS IN nr - ar -- wire_clean_all.pro !!'

endfor ; clean the next freq.

endif

; =================================================================

; Return only positive phases:
phase = freq.p / (2D * !DPI)
w = where(phase lt 0.,c)
if c ge 1 then phase(w) = phase(w) + 1.0D
freq.p = phase * 2D * !DPI

; Construct export filename
a = strsplit(filename,'.',/extract)
outfile = a(0) + '_clean.idl'
fc =freq & dc = dat
save,filename=outfile,fc,dc

print,' %%% Saved file with ampl. spectrum: '+outfile

END
