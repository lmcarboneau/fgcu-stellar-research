; --------------------------------------------------------------
; Program computed the AREA under the window function!
; --------------------------------------------------------------

; CHOOSE THE DATA STRING:
; --------------------------------------------------------------
; data = 'wire' ; WIRE 1999 obs. of procyon == 3.5270944D
; data = 'wire2000' ; WIRE 2000 obs. of procyon == 5.3577757D
; data = 'gunter' ; Hans-Gunter Ludwigs 3D simulations = 53.609993D
; data = 'gunter2' ; Hans-Gunter Ludwigs NEW 3D (11 AUG 04) = 16.520008D
; data = 'gunter3' ; Hans-Gunter Ludwigs NEW 3D (12 AUG 04) = 11.782363D
; data = 'virgo' ; virgo data for Solen = 0.55646399D

; data = 'alphacen1999'
 data = 'alphacen2004'

; --------------------------------------------------------------
; g77 -o four_trans_hk_RV_wire four_trans_hk_RV_wire.f

fj = 1e6/86400D ; conversion from c/d to microHz

; --------------------------------------------------------------
if data eq 'wire' then begin
; readcol,'/mnt/WinC/linux/wire/wire_lc/wire_procyon_sept99_22jun04.dat',t,d,w
 readcol,'/ai40/bruntt/wire/wire_lc/wire_procyon_sept99_22jun04.dat',t,d,w
 sim = d
 tt = t
 wei2 = w / total(w)
endif 
; --------------------------------------------------------------

; --------------------------------------------------------------
if data eq 'wire2000' then begin
 readcol,'/ai40/bruntt/wire/wire_lc/wire_procyon_sept00_22jun04.dat',t,d,w
 sim = d
 tt = t
 wei2 = w / total(w)
endif 
; --------------------------------------------------------------

; --------------------------------------------------------------
if data eq 'alphacen1999' then begin
 dir = '/ai40/bruntt/wire/procyon/procyon_data/'
 readcol,dir+'wire_lc_alphaCen1999_star_0.dat', t, d, w
 sim = d
 tt = t
 wei2 = w / total(w)
endif 
; --------------------------------------------------------------


; --------------------------------------------------------------
if data eq 'alphacen2004' then begin
 dir = '/ai40/bruntt/wire/procyon/procyon_data/'
 readcol,dir+'wire_lc_alphaCen2004_star_0.dat', t, d, w
 sim = d
 tt = t
 wei2 = w / total(w)
endif 
; --------------------------------------------------------------


; --------------------------------------------------------------
if data eq 'virgo' then begin ; 3. AUG 2004
 readcol,'~bruntt/wire/virgo/virgo_20days.dat', t, d, w
 sim = d & tt = t & wei2 = w / total(w)
endif 
; --------------------------------------------------------------

; --------------------------------------------------------------
if data eq 'gunter2' then begin
                                ; Get Hans-G'unter Ludwigs intensity
                                ; light curve for Procyon (11th of AUG 2004)
restore,'/ai40/bruntt/wire/procyon/procyon_data/procyon_signal.idlsave'
tt = time/86400. & dat2 = flux & sim = dat2 - median(dat2)
wei2 = fltarr(n_elements(dat2)) & wei2 = 1.

endif
; --------------------------------------------------------------

; --------------------------------------------------------------
if data eq 'gunter3' then begin
                                ; Get Hans-G'unter Ludwigs intensity
                                ; light curve for Procyon (11th of AUG 2004)
restore,'/ai40/bruntt/wire/procyon/procyon_data/procyon_signal_12AUG2004.idlsave'
tt = time/86400. & dat2 = flux & sim = dat2 - median(dat2)
wei2 = fltarr(n_elements(dat2)) & wei2 = 1.

endif
; --------------------------------------------------------------

; --------------------------------------------------------------
if data eq 'gunter' then begin
 ; Get Hans-G'unter Ludwigs intensity light curve for Procyon:
 restore,'~/wire/workshop/procyon.idlsave'
 sim = flux - avg(flux)
 sim = sim * sqrt(powerscale)
 sim = sim / avg(flux)

 ; Time step btw. data points in 10 seconds
 tt = findgen(1860) * 10./86400D & tt = tt - median(tt)

 ; Give equal weights to each simulated data point:
 wei2 = fltarr(1860) & wei2(*) = 1.
endif
; --------------------------------------------------------------

; Generate light curve for a perfect sinusoid
fcen = 1000. / fj ; frq. at 1000 microHz = 1 milliHz
d2 = sin(2.*!PI * (fcen*tt + 0.45))
; Use this sinusoid to calculate the area pr. resolution element
; in the power spectrum. Amplitude == 1.0

tt2 = tt & dat2 = d2
fmax = 4000. / fj ; calculate up to 4000 microHz
minfreq = 0.05 & maxfreq = fmax & nmax = 85000.

; Calculate ampl. spec in ppm, using weights = Hans Ks code
ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,$
 freq2res,amp2res,phase2res,highres=1.0,nmax=nmax

; Plot the POWER spectrum:
plot, freq2res*fj,(amp2res/1e6)^2.0,$
 ysty=3,yr=[0,1.2],xtit='Frequency [!4l!3Hz]',$
 ytit='Power [ppm!E2!N]',charsi=2

; Calc. area pr. resolution element in MICRO HZ
l1 = 100. & l2=4500. ; range of freqs. to consider
wa = where(freq2res*fj gt l1 and freq2res*fj lt l2,ca)

plots,l1,!y.crange,line=2
plots,l2,!y.crange,line=2

res = 0. ; calculate area of power spectrum
for i=0L,ca-2 do $
 res = res + $
   (freq2res(wa(i+1))-freq2res(wa(i))) * fj * $
   (0.5 * (amp2res(wa(i+1)) + amp2res(wa(i))) * 1e-6)^2.0 ; power!

print,$
 ' %%% Area pr. resolution element in power spectrum for **'+data+'**: ',res


end
