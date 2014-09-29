PRO wire_ampl_ignore_orbit, t, d, w, f2, res, forb = forb, df=df, incwid=incwid

; Times are in days
; Data is in magnitudes
; f2 = max frequency in MICROHZ
; Ampl. will be in microMag, not ppm !

col=getcolor(/load)

if n_elements(forb) eq 0 then forb = 173.62563 ; microHz
if n_elements(df)   eq 0 then   df = 5.        ; microHz
if n_elements(incwid) eq 0 then incwid = -99.9


fj = 1e6 / 86400D
f1use = 1.5 / (max(t) - min(t)) ; cycles pr. day --- min allowed frequency
f2use = f2 / fj ; convert f2 to microHz


np = ceil(f2 / forb) ; number of frequency bands to compute!

nmax = 1e6
res = fltarr(4,nmax)
cnt = 0

print,' %%% Number of freq. windows: ' + string(np)


for i=0,np-1 do begin

; Define the freq. window where ampl. spectrum will be calculated:
  if i eq 0 then fstart = f1use else fstart = i * (forb/fj) + df/fj  
  fend = (i+1.) * (forb/fj) - df/fj

  if fend lt (incwid/fj) then begin
   print,' >>> Increasing width of spectrum that is IGNORED ... '
   factor = 2.5 ; increase width of spectrum that will not be computed!
   if i eq 0 then fstart = f1use else fstart = i * (forb/fj) + (df*factor)/fj  
   fend = (i+1.) * (forb/fj) - (df*factor)/fj
  endif
  
  print,' %%% Calculating freq. in range: ' + $
   string(fstart*fj,format='(F7.1)') + ' ' + $
   string(fend*fj,  format='(F7.1)')
  
  minfreq = fstart & maxfreq = fend
  tt2 = t & dd2 = d & ww2 = w
  
  ampl_spec_calc_wire_rv, tt2,dd2,ww2, minfreq,maxfreq, freq,amp,phase
  
  n = n_elements(freq)
  res(0,cnt:cnt+n-1) = freq
  res(1,cnt:cnt+n-1) = amp 



fcen = 0.5 * (fstart + fend)
d2 = sin(2.*!PI * (fcen*t + 0.5)) ; phase = 0.5 (random ...)
; Use this sinusoid to calculate the area pr. resolution element
; in the power spectrum

tt2res  = t & dat2res = d2 & wei2res = w & nmax = 5e4
minfreq = fstart & maxfreq = fend

; Calculate ampl. spec in ppm, using weights = Hans Ks code
ampl_spec_calc_wire_rv,tt2res,dat2res,wei2res,minfreq,maxfreq,$
 freq_res,amp_res,phase_res,highres=1.0,nmax=nmax

; Plot the POWER spectrum:
plot, freq_res*fj,(amp_res/1e6)^2.0,$
 ysty=3,yr=[0,1.2],xtit='Frequency [!4l!3Hz]',$
 ytit='Power [ppm!E2!N]',charsi=2

wa = where(freq_res*fj gt f1use,ca)

resol = 0. ; calculate area of power spectrum
for sum=0L,ca-2 do $
 resol = resol + $
   (freq_res(wa(sum+1))-freq_res(wa(sum))) * fj * $
   (0.5 * (amp_res(wa(sum+1)) + amp_res(wa(sum))) * 1e-6)^2.0 ; power!

print,$
 ' %%% Area pr. resolution element in power spectrum: '+string(resol,format='(F8.3)')

res(2,cnt:cnt+n-1) = resol           ; area pr. resolution element in power spectrum
res(3,cnt:cnt+n-1) = amp^2.0 / resol ; power density

cnt = cnt + n

plot,res(0,0:cnt-1)*fj,res(1,0:cnt-1)
for orb=1,i+1 do plots,forb*orb,!y.crange,col=col.red


print,' '

endfor

res = res(*,0:cnt-1)


END
