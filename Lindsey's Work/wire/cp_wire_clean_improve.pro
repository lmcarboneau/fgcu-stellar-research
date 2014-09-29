PRO wire_clean_improve,filename,unit,f1,f2,fc,freq,dat,$
 timeunit=timeunit,ampunit=ampunit

n_iter = 0
nfreq = n_elements(fc)
calcamp = 1

col = getcolor(/load)

ff = findfile(filename,Count=cnt)
if cnt ne 1 then begin
 print,''
 print,' *** Light curve file not found: '+filename
 print,''
 RETURN
endif

readcol,filename,t,d,w,format='D,D,D'
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
outtext = ' %%% Improving '+strcompress(string(nfreq),/remove_all) + $
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

w1out = w1 & d1out = d1

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
printf,u,strcompress(string(f1a,  format='(D15.5)'),/remove_all)
printf,u,strcompress(string(f2a,  format='(D15.5)'),/remove_all)
printf,u,strcompress(string(np,   format='(I10)'),  /remove_all) ; # points in spectrum
printf,u,strcompress(string(nfreq,format='(I6)'),   /remove_all) ; # points for clea

close,u
free_lun,u

spawnrob,'clean < ' + temp_fil2, out

readcol, 'series.dat', tn, dn, wn, format='D, D, D'
ntt = n_elements(tn)
dat = replicate( {t:0D, d:0., w:0.}, ntt)
dat.t = (tn / tfac) & dat.d = dn + d0 & dat.w = wn

readcol, 'clean.dat', nr, aa, ff, ph, format='(I, D, D, D'
nff = n_elements(nr)

freq = replicate( {nr:0L, f:0D, a:0D, p:0D}, nff)
freq.nr = nr & freq.f = ff & freq.a = aa & freq.p = ph ; /(2.*!PI)

endif
; =================================================================

; =================================================================
if mode eq 'SaveAll' then begin

nff = nfreq
ntt = n_elements(t)
dat = replicate( {t:0D, d:fltarr(nfreq), w:0.}, ntt)

if calcamp eq 1 then $
freq = replicate( {nr:0L, f:0D, a:0D, p:0D, $
 freq:fltarr(np), amp:fltarr(np), phase:fltarr(np) }, nff)
if calcamp eq 0 then $
freq = replicate( {nr:0L, f:0D, a:0D, p:0D}, nff)


for i=0,nfreq-1 do begin ; for each frequency

iter = 0

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
  readcol, temp_fil, tn2, dn2, wn2, format='D, D, D'
 endif
  tt2 = (tn2 / tfac) & dat2 = dn2 - median(dn2) 
  wei2 = wn2 / max(wn2)
  printf,u,'series_cleaned.dat'
endelse

; Remove all freq. except the one you've reached
 won = bytarr(nfreq) & won(*) = 1 & won(i) = 0 ; won = 0 ---> turn off
 g = where(won eq 1,cg)

; Freqs. in fc-array is in cycles pr. day
 subtr = dat2 & subtr(*) = 0.
 phase = fc.p / (2D * !DPI)

 for p=0,cg-1 do $
  subtr = subtr + $
  (fc(g(p)).a/1e6) * $
  sin( 2. * !DPI * ( (fc(g(p)).f * tt2 / facuse) + phase(g(p)) ) )

 dat2a = dat2 - subtr & dat2a = dat2a - median(dat2a)

 dat2f = (fc(i).a/1e6) * $
    sin( 2. * !DPI * ( (fc(i).f * tt2 / facuse) + phase(i) ) )

; tt3 = (max(tt2)-min(tt2)) * ( findgen(100000)/99999. ) + min(tt2)
; dd3 =  (fc(i).a/1e6) * $
;    sin( 2. * !DPI * ( (fc(i).f * tt3 / facuse) + phase(i) ) )
;  plot,tt3,dd3,psym=3,col=col.red,xr=[-1,1]*.4 + 2
;  oplot,tt2,dat2f,psym=3
;  oplot,tt2,dat2a,psym=1,symsi=.1


; Write time series:
  get_lun,u6
  openw,u6,'series_cleaned.dat'  
  nn = n_elements(tt2)
  for k=0L,n-1 do $
   printf,u6, tt2(k)*tfac, dat2a(k), wei2(k), format='(D18.7, D15.7, D15.7)'
  close,u6
  free_lun,u6

 f1micro = fc(i).f*(fac/facuse) - f_resolution * 0.8 ; freq range in microHz
 if f1micro lt 0.02 then f1micro = 0.02 ; 0.1 microHz = 0.01 c/d
 f2micro = fc(i).f*(fac/facuse) + f_resolution * 0.8 ; freq range in microHz
 error_mhz = f_resolution * 0.0001 ; accepted error in microHz
 npuse = (f2micro -  f1micro) / error_mhz
 npuse = ceil(npuse/100.) * 100. + 1
 print,' %%% f = ' + string(fc(i).f *(fac/facuse),format='(F10.5)') + ' micHz ' + $
   'Calc spec ran: '+$
   string(f1micro,format='(F7.3)') + ' to ' + string(f2micro,format='(F7.3)') + $
   ' micHz : resol. in spec: ' +  string(error_mhz,format='(D9.6)')

printf,u,'1' ; weights on ? 0 or 1
printf,u,'1' ; weight exponent
printf,u,strcompress(string(f1micro,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(f2micro,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(npuse,format='(I10)'),/remove_all) ; # points in spectrum
printf,u,strcompress(string(1,format='(I6)'),/remove_all) ; # points for clea

close,u
free_lun,u

;if iter ge 1 then begin
;  tt2 = (tn2 / tfac) & dat2 = dn2 - median(dn2) 
;  wei2 = wn2 / max(wn2)
;  ampl_spec_calc_wire_rv,tt2,dat2,wei2,f1micro/11.57,f2micro/11.57,freq2a,amp2a,phase2a
;  col=getcolor(/load)
;  oplot,freq2a,amp2a,psym=-6,col=col.red,symsi=.2
;endif


; Calculate (new) amplitude spectrum:
if calcamp eq 1 and iter eq 0 then begin
 minfreq = f1micro*facuse/fac & maxfreq = f2micro*facuse/fac
 dat2p=dat2 & tt2p = tt2 & wei2p = wei2
 ampl_spec_calc_wire_rv,tt2p,dat2p,wei2p,minfreq,maxfreq,freq2,amp2,phase2

; minfreq = f1micro*facuse/fac & maxfreq = f2micro*facuse/fac
; dat2p=dat2a & tt2p = tt2 & wei2p = wei2
; ampl_spec_calc_wire_rv,tt2p,dat2p,wei2,minfreq,maxfreq,freq2p,amp2p,phase2p

;  col=getcolor(/load)
;  plot,freq2*facuse,amp2,psym=3,xr=[-1,1]*.002 + fc(i).f
;  oplot,freq2p*facuse,amp2p,col=col.red,psym=3

endif



spawnrob,'~/bin/clean < ' + temp_fil2, out

; Read the new time series after cleaning of ONE frequency:
readcol, 'series.dat', tn, dn, wn, format='D, D, D'
ntt = n_elements(tn)
dat.t = (tn / tfac) & dat.d(i) = dn + d0 & dat.w = wn

np2 = n_elements(freq2)

if calcamp eq 1 and iter eq 0 then begin ; save the amplitude spectrum (first iteration)
 freq(i).freq(0:np2-1)  = freq2 * facuse ; convert from c/day to the right unit
 freq(i).amp(0:np2-1)   =  amp2 * ampfac ; convert to the desired unit
 freq(i).phase(0:np2-1) = phase2
endif

readcol, 'clean.dat', nr, aa, ff, ph, format='I, D, D, D'
nff = n_elements(nr)

ff_unit = (ff / fac) * facuse ; convert freq to the right unit

; print, ' >>> Iter = ',iter, ff(0)/fac, aa(0), ph(0)/(2.*!PI)

; =================================================================
; REITERATE TO GET ACCURATE FREQUENCY + AMPL + PHASE:
; =================================================================
iter = iter + 1
if iter le n_iter then goto,reiterate
; print, ' ------------------------------- '

; =================================================================


freq(i).nr = nr                & freq(i).f = ff_unit
freq(i).a  = aa * 1e6 * ampfac & freq(i).p = ph

endfor ; clean the next freq.

endif

; =================================================================
; Return only positive phases:
phase = freq.p / (2D * !DPI)
w = where(phase lt 0.,c)
if c ge 1 then phase(w) = phase(w) + 1.0D
freq.p = phase * 2D * !DPI


a = strsplit(filename,'.',/extract)
outfile = a(0) + '_imp_clean.idl'
fc2 = freq & dc2 = dat
save,filename=outfile,fc2,dc2

print,' %%% Saved file with ampl. spectrum: '+outfile

END
