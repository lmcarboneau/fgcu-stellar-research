PRO wire_clean_all,filename,unit,f1,f2,nfreq,freq,dat,$
 timeunit=timeunit,ampunit=ampunit

ff = findfile(filename,Count=cnt)
if cnt ne 1 then begin
 print,''
 print,' *** Light curve file not found: '+filename
 print,''
 RETURN
endif

readcol,filename,t,d,w,format='D,D,F'
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
  if timeunit eq 'ppm' then ampfac = 1D
  if timeunit eq ''    then ampfac = 1D * 1e-6
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

mode = 'SaveAll' ; ONLY THIS MODE WORKS 100% --> REITERATE TO GET ACC. FREQ!


; =================================================================
outtext = ' %%% Clean '+strcompress(string(nfreq),/remove_all) + $
      ' freq. in range ' + $
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

; Take four points per. freq. resolution element
np = 2.5 * (f2use - f1use) * fac / f_resolution
np = ceil(np / 1000.) * 1000. + 1.
np2 = 20.5 * (f2use - f1use) * fac / f_resolution
np2 = ceil(np2 / 1000.) * 1000. + 1.

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

temp_fil  = 'temp_clean.dat'
temp_fil2 = 'temp_clean_input.dat'

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

get_lun,u
openw,u,temp_fil2

printf,u,temp_fil
printf,u,'1' ; weights on ? 0 or 1
printf,u,'1' ; weight exponent
printf,u,strcompress(string(f1a,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(f2a,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(np,format='(I10)'),/remove_all) ; # points in spectrum
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

freq = replicate( {nr:0L, f:0., a:0., p:0.}, nff)
freq.nr = nr & freq.f = ff & freq.a = aa & freq.p = ph ; /(2.*!PI)

endif
; =================================================================

; =================================================================
if mode eq 'SaveAll' then begin

nff = nfreq
ntt = n_elements(t)
dat = replicate( {t:0D, d:fltarr(nfreq), w:0.}, ntt)

freq = replicate( {nr:0L, f:0., a:0., p:0., $
 freq:fltarr(np), amp:fltarr(np), phase:fltarr(np) }, nff)

for i=0,nfreq-1 do begin ; for each frequency

reiterate:

get_lun,u
openw,u,temp_fil2

if i eq 0 then begin ; the first frequency
 readcol, temp_fil, tn2, dn2, wn2, format='D, F, F'
 tt2 = (tn2 / tfac) & dat2 = dn2 - median(dn2) 
 wei2 = wn2 / max(wn2)
  printf,u,temp_fil
endif else begin
 readcol, 'series.dat', tn2, dn2, wn2, format='D, F, F'
 tt2 = (tn2 / tfac) & dat2 = dn2 - median(dn2) 
 wei2 = wn2 / max(wn2)
  printf,u,'series.dat'
endelse

; Calculate (new) amplitude spectrum:
minfreq = f1use & maxfreq = f2use ; frequencies in cycles pr. day
ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,freq2,amp2,phase2

; Time is always in seconds in the clean.f program, 
; hence the freq range is given in microHz
f1micro = f1use*1e6/86400
f2micro = f2use*1e6/86400

printf,u,'1' ; weights on ? 0 or 1
printf,u,'1' ; weight exponent
printf,u,strcompress(string(f1micro,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(f2micro,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(np,format='(I10)'),/remove_all) ; # points in spectrum
printf,u,strcompress(string(1,format='(I6)'),/remove_all) ; # points for clea

close,u
free_lun,u

spawnrob,'~/bin/clean < ' + temp_fil2, out

; Read the new time series after cleaning of ONE frequency:
readcol, 'series.dat', tn, dn, wn, format='D, F, F'
ntt = n_elements(tn)
dat.t = (tn / tfac) & dat.d(i) = dn + d0 & dat.w = wn

np2 = n_elements(freq2)

freq(i).freq(0:np2-1)  = freq2 * facuse ; convert from c/day to the right unit
freq(i).amp(0:np2-1)   = amp2 * ampfac  ; convert to the desired unit
freq(i).phase(0:np2-1) = phase2

readcol, 'clean.dat', nr, aa, ff, ph, format='I, F, F, F'
nff = n_elements(nr)

ff_unit = (ff / fac) * facuse ; convert freq to the right unit

; =================================================================
; REITERATE TO GET ACCURATE FREQUENCY + AMPL + PHASE:
; =================================================================



; =================================================================


freq(i).nr = nr                & freq(i).f = ff_unit
freq(i).a  = aa * 1e6 * ampfac & freq(i).p = ph ; /(2.*!PI)
endfor ; clean the next freq.

endif

; =================================================================

a = strsplit(filename,'.',/extract)
outfile = a(0) + '_clean.idl'
fc =freq & dc = dat
save,filename=outfile,fc,dc

print,' %%% Saved file with ampl. spectrum: '+outfile

END
