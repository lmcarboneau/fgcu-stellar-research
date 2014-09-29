PRO wire_simulator, wirefile, periodfile, noiselevel, starname, dirout, $
     nsim=nsim,debug=debug,eq_wei=eq_wei, startid=startid

; Create artifical light curves for WIRE data
; Example:
; wire_simulator, $
;  '/export/brixx1/bruntt/wire/lambdasco/data/lamsco_mar04_m1.5d.dat',$
;  '/export/brixx1/bruntt/wire/lambdasco/data/mar04.per',$
;  0.0004, 'lamsco_MARCH04', $
; '/export/brixx1/bruntt/wire/lambdasco/sim/'

default9, nsim, 50 ; number of simulations
default9, eq_wei, 0B
default9, debug, 0B
default9, startid, 0L

; -------------------------------------------------
; Import freq+ampl+phases in PERIOD04 - format
; -------------------------------------------------
if periodfile ne '' then begin
  readcol,periodfile, a, fw, aw, pw, format='A, D,D,D'
  endif else begin
 fw = 1. & aw = 0. & pw = 0.5
endelse
nf = n_elements(fw)
; -------------------------------------------------

; -------------------------------------------------
; Import times for WIRE light curve
; -------------------------------------------------
readcol, wirefile, time, data, weight, format='d,d,f'
; -------------------------------------------------

; Create dir and remove old simultions
spawnrob,'mkdir ' + dirout
 spawnrob,'rm -f ' + dirout + '/*' ; purge!

np = n_elements(time)
noise = replicate(noiselevel, np)

; The perfect light curve with no noise:
 sim = fltarr(np)
 for k=0,nf-1 do $
  sim = sim + aw(k) * sin( 2. * !DPI * (fw(k) * time + pw(k)) )

; Equal weights for fitting:
 weights_use = dblarr(np)  &  if eq_wei then weights_use(*) = 1D / np else weights_use = weight

; For each simulation, add noise with different seed
for i=0,nsim-1 do begin
 sim2 = sim
 for p=0L,np-1 do $
  sim2(p) = sim(p) + noise(p) * randomn(seed) ; add noise that is independent of time

if debug then begin
 col=getcolor(/load)
 plot,sim2,xr=[0,1000],psym=3,xsty=3,ysty=3
 oplot,data,col=col.sky,psym=3


endif

if dirout ne '' then begin
  get_lun, uu
  iout = i + startid

  if iout lt  10 then suffix = '00' + strcompress(string(iout,format='(I5)'),/remove_all)
  if iout ge  10 and iout le 99 then suffix = '0' + strcompress(string(iout,format='(I5)'),/remove_all)
  if iout ge 100 then suffix = strcompress(string(iout,format='(I5)'),/remove_all)

  fileout = dirout + '/' + starname + '_' + suffix + '.dat'
  openw,uu,fileout
    for k=0L,np-1 do printf,uu,time(k),sim2(k),weights_use(k),format='(D12.6, D12.6, D12.8)'
  close,uu
 free_lun, uu
 print, ' %%% Wrote file: ' + fileout
endif


endfor


END

