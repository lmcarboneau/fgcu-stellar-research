PRO wire_synth_lc, inputlc, periodfile, whitenoise_in_mag, outputdir, $
 nlc = nlc, nper=nper, eqwei=eqwei, steton=steton, debug=debug, doamp=doamp, $
 maxfreq=maxfreq, resol=resol, minfreq=minfreq, $
 scaleamp=scaleamp

; =========================================================
; Simulate light curves for WIRE observations
; =========================================================

default9, nlc,  1     ; number of light curves to produce
default9, nper, 1e4   ; number of periods in the periodfile to use
default9, eqwei, 1B   ; set equal weights
default9, steton, 0B  ; use stetson weights
default9, debug, 1B   ; make debug plots?
default9, doamp, 0B   ; compute ampl. spectrum?
default9, maxfreq, 10.   ; Max freq. in c/day
default9, minfreq, 1e-3  ; Min freq. in c/day
default9, resol, 3.5     ; resolution in ampl. spectrum, values > 3.0 recommended
default9, scaleamp, 1.0D ; Scale input amplitudes?

bb1 = findfile(inputlc, Count=cnt)
if cnt ne 1 then begin
 print,' %%% Input LC not found: ' + inputlc
 RETURN
endif

if strmatch(periodfile,'*.per') eq 0 then periodfile = 'x9x9x9x9x9.x8'

bb2 = findfile(periodfile, Count=cnt2)
if cnt2 eq 0 then begin
 print,' %%% Input PERIOD FILE not found: ' + periodfile
 print,' %%% Data file will be pure white noise!'
 f = 1e9 &  a = 0D &  p = 0. & nper = 1
endif else begin
 readcol, periodfile, aa, f, a, p, format='a,d,d,d'
 nf = n_elements(f) & if nper gt nf then nper = nf
 f = f(0:nper-1) & a = a(0:nper-1) & p = p(0:nper-1)
 a = a * scaleamp ; scale the input amplitudes
endelse

spawnrob,'mkdir ' + outputdir

readcol, inputlc, t, d, w, format='d,f,f'
n = n_elements(t)

t2 = t              ; use the same times
d2 = t & d2(*) = 0. ; array for the data points
w2 = w              ; use the same weights

if eqwei then w2(*) = 1./n ; set equal weights
wei_stetson = t & wei_stetson(*) = 1D ; set Stetson weights == 1

; Synthetic light curve w/ the modes
for i=0,nper-1 do $
 d2 = d2 + a(i) * sin(2D * !DPI * (f(i) * t + p(i) ) )

x = strsplit(inputlc,'/',/extract) & nx = n_elements(x)
lcbase = x(nx-1)
y = strsplit(lcbase,'.',/extract)
lcbase = y(0)

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Make the light curve(s)
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++
for j=1,nlc do begin

noise = d & noise(*) = whitenoise_in_mag
 for k=0L,n-1 do $
  noise(k) = noise(k) * randomn(seed) ; add noise that may be independent of time

sim2 = noise + d2

if steton then $
 wire_stetson, sim2, wei_stetson, 0 ; , /silent

w2 = w2 * wei_stetson & w2 = w2 / total(w2)

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++
if j le 9 then              suffix = '00' + string(j,format='(I1)')
if j ge 10 and k le 99 then suffix = '0'  + string(j,format='(I2)')
if j ge 99 then             suffix =        string(j,format='(I2)')
fileout = outputdir + lcbase + '_' + suffix + '.dat'
fileout_amp = outputdir + lcbase + '_' + suffix + '.amp.idl'
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++

print,' %%% Saved simulated light curve: ' + fileout
if debug then begin
 !P.multi=[0,1,2]
  !P.charsize=1 & !P.thick=1 
  !P.charthick=1
  y1 = robust_sigma(d) * 4.
  plot,t,d,psym=3,tit='Input light curve', yr=[-1,1]*y1,xsty=3
  plot,t2, sim2,psym=3,tit='Simulated light curve', yr=[-1,1]*y1,xsty=3
 !P.multi= 0
endif

get_lun,u
openw,u,fileout
for l=0L,n-1 do printf,u,t2(l), sim2(l), w2(l), $
  format='(D16.8, 2D14.6)'
close,u  &  free_lun, u

; Calculate amplitude spectrum
if doamp then begin
  minfreq_use = 3.0 / (max(t2) - min(t2))
  if minfreq gt minfreq_use then minfreq_use = minfreq

; Synthetic LC
  tt2 = t2 & dat2 = sim2 & wei2 = w2  & fac = 1e6/86400D
  minfreq = minfreq_use & maxfreq_use = maxfreq & highres = resol
  ampl_spec_calc_wire_rv,tt2,dat2,wei2,$
   minfreq,maxfreq_use,freq,amp,phase,highres=highres

; Observed LC
  tt2 = t & dat2 = d & wei2 = w  & fac = 1e6/86400D
  minfreq = minfreq_use & maxfreq_use = maxfreq & highres = resol
  ampl_spec_calc_wire_rv,tt2,dat2,wei2,$
   minfreq,maxfreq_use,freq2,amp2,phase2,highres=highres

  save,filename=fileout_amp, freq,  amp,  phase, $
                             freq2, amp2, phase2, /compress
  print,' %%% Saved amplitude spectra: ' + fileout_amp
endif




; +++++++++++++++++++++++++++++++++++++++++++++++++++++++
endfor ; next light curve
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++

END
