PRO wire_power_density_resolution, lcfile, fres=fres, fmax=fmax, highres=highres, phase=phase, dir=dir

; Compute area per resolution element in amplitude spectrum

default9, fres, 500.
default9, fmax, 1000.
default9, highres, 3.0
default9, phase, 0.0 ; range 0..1

readcol,lcfile,t,d,w

; Insert simple sine wave with amplitude == 1, frequency == fres
nn = n_elements(t)
dat2 = dblarr(nn)
dat2 = sin(2D * !DPI * (t * fres + phase))
wei2 = fltarr(nn) & wei2(*) = 1D / double(nn)
tobs = max(t) - min(t)  & tt2 = t
minfreq = 1./tobs & maxfreq = fmax


if n_elements(dir) eq 0 then begin
 m4_get_basedir, basedir
 dir = basedir + '/temp/'
endif



stat_weight = 1.0
weight_exp = 2.0
factor = 8. ; resolution factor -- DS recommends 5.
n_points = ceil((maxfreq-minfreq)*factor*(max(tt2)-min(tt2))/10.)*10.
type = 0 ; ampl. spectraum
f0 = 1e-3 ; not used, only used if type = 1
print,' %%% Number of points in spectrum: ' + strcompress(string(n_points,format='(I8)'),/remove_all)

; stop

; Using Dennis Stello's fourier program (all run in IDL):
four_trans_ds,tt2, dat2, wei2, dir, stat_weight, $
                  weight_exp, minfreq, maxfreq, n_points , $
                  type, f0, $
                  freq, ampl, phase,alpha,beta,/silent

power = alpha^2. + beta^2.

; Using HKs program instead:
; ampl_spec_calc_wire_rv,tt2,dat2,wei2,$
;  minfreq,maxfreq,freq2,amp2,phase,highres=2.0
; nfreq2 = n_elements(freq2)
; dfreq2 = median(freq2(1:nfreq2-1) - freq2(0:nfreq2-2)) ; step in freq
; warea2 = total((amp2/1e6)^2.) * dfreq2

; DS:
; nfreq3 = n_elements(freq)
; dfreq3 = median(freq(1:nfreq3-1) - freq(0:nfreq3-2)) ; step in freq
; warea3 = total((ampl)^2.) * dfreq3 ; 0.32

;plot,freq,ampl,/nodata
;oplot,freq2,amp2/1e6,col=col.sky
;oplot,freq,ampl


nfreq = n_elements(freq)
dfreq = median(freq(1:nfreq-1) - freq(0:nfreq-2)) ; step in freq
warea = total(power) * dfreq 

; plot,freq,power,yr=[0,1]

print,' %%% Area per resolution element in power for file: '+lcfile, warea

END
