PRO wire_clean, t, d, w, f1, f2, nfreq, freq, dat

; Run Hans Kjeldsen's Clean.f program on your time series
; Time must be in HJD

spawnrob,'rm -f series.dat'

mode = 'SaveLast' ; save only the last time series
 mode = 'SaveAll'  ; save all time series
; mode = 'SaveAmp'  ; save all time series + compute ampl. spectrum

t0 = median(t)
d0 = median(d)

tfac  = 86400D
; tfac2 = 86400D
ffac = 1e6 / 86400D

f1a = f1 * ffac  
f2a = f2 * ffac

; =================================================================
; Calculate nominal frequency resolution
t_total = (max(t) - min(t)) * tfac ; total obs. time in seconds
f_resolution = 1e6 / t_total ; resolution in microHz (often quoted in papers)

; Take four points per. freq. resolution element
np = 2.5 * (f2 - f1) * ffac / f_resolution
np = ceil(np / 1000.) * 1000. + 1.


print,' %%% Number of points in FFT calc: ' + string(np)
; =================================================================

; t1 = t - t0 & 
t1 = t * tfac
d1 = d - d0
w1 = w ; / total(w) ; Weights!
n = n_elements(d1)

a = sort(t1) & t1 = t1(a) & d1 = d1(a) & w1 = w1(a)


temp_fil  = 'temp_clean.dat'
temp_fil2 = 'temp_clean_input.dat'

get_lun,u
openw,u,temp_fil

; scaledat = max(abs(d1))
w1out = w1 ; / max(w1)
d1out = d1 ; / scaledat

for i=0L,n-1 do $
 printf,u, t1(i), d1out(i), w1out(i), format='(D18.7, D15.7, D15.7)'

close,u
free_lun,u



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

if mode eq 'SaveAll' then begin

nff = nfreq
ntt = n_elements(t)
dat = replicate( {t:0D, d:fltarr(nfreq), w:0.}, ntt)

freq = replicate( {nr:0L, f:0., a:0., p:0., $
 freq:fltarr(np), amp:fltarr(np), phase:fltarr(np) }, nff)

for i=0,nfreq-1 do begin ; for each frequency

get_lun,u
openw,u,temp_fil2

if i eq 0 then begin
 ; tt2 = t-t0 & dat2 = d - median(d) & wei2 = w / total(w) ; IDENTIAL TO:

 readcol, temp_fil, tn2, dn2, wn2, format='D, F, F'
 tt2 = (tn2 / tfac) & dat2 = dn2 - median(dn2) 
 wei2 = wn2 / max(wn2)
 ; wei2 = wn2 / total(wn2)

 printf,u,temp_fil

endif else begin
 readcol, 'series.dat', tn2, dn2, wn2, format='D, F, F'
 tt2 = (tn2 / tfac) & dat2 = dn2 - median(dn2) 
 wei2 = wn2 / max(wn2)
 ; wei2 = wn2 / total(wn2)

 printf,u,'series.dat'
endelse

; Calculate (new) amplitude spectrum:
minfreq = f1 & maxfreq = f2
ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,freq2,amp2,phase2

printf,u,'1' ; weights on ? 0 or 1
printf,u,'1' ; weight exponent
printf,u,strcompress(string(f1a,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(f2a,format='(D15.5)'),/remove_all)
printf,u,strcompress(string(np,format='(I10)'),/remove_all) ; # points in spectrum
printf,u,strcompress(string(1,format='(I6)'),/remove_all) ; # points for clea

close,u
free_lun,u

spawnrob,'~/bin/clean < ' + temp_fil2, out

readcol, 'series.dat', tn, dn, wn, format='D, F, F'
ntt = n_elements(tn)
dat.t = (tn / tfac) & dat.d(i) = dn + d0 & dat.w = wn

np2 = n_elements(freq2)

freq(i).freq(0:np2-1)  = freq2
freq(i).amp(0:np2-1)   = amp2 
freq(i).phase(0:np2-1) = phase2

readcol, 'clean.dat', nr, aa, ff, ph, format='(I, F, F, F'
nff = n_elements(nr)

freq(i).nr = nr & freq(i).f = ff & freq(i).a = aa & freq(i).p = ph ; /(2.*!PI)
endfor ; clean the next freq.

endif

; =================================================================


END
