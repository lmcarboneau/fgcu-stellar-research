PRO wire_master_fixed_freq_lc_fit,wildcard, freqfit, resfix, $
                                  equal_weights=equal_weights, debug=debug, $
                                  aguess=aguess, pguess=pguess

; Read the simulation light curves given by the wildcard
; Fit amplitudes and phases, but fix the frequencies at 
; the known input parameters. 

default9, equal_weights, 1B
nf_usefit = n_elements(freqfit)
default9, debug, 0B
if n_elements(aguess) ne nf_usefit then aguess = replicate(.001, nf_usefit)
if n_elements(pguess) ne nf_usefit then pguess = randomu(seed,nf_usefit)


spawnrob,'ls -1 ' + wildcard, l  &  n = n_elements(l)
if n le 1 then begin
 print,' *** LE 1 simulation with wildcard: ' + wildcard
 RETURN
endif

resfix = replicate({freq:fltarr(nf_usefit), ampl:fltarr(nf_usefit),phase:fltarr(nf_usefit)}, n)

; Print file with frequencies: this is needed by the curvefit function!
m4_get_basedir, base
inpfile = base + '/wire/EpsilonCep/inpfreq.dat'
openw,1,inpfile
  for k=0,nf_usefit-1 do printf,1,freqfit(k),format='(D15.7)'
close,1

k2 = indgen(nf_usefit) * 2

for i=0L,n-1 do begin

 readcol,l(i),time,data,weights, format='d,f,f'
 np = n_elements(data) ; number of data points

; Equal weights for fitting:
 if equal_weights then begin
  weights = fltarr(np)  &  weights(*) = 1D / np
 endif

  a = fltarr(nf_usefit * 2) ; initial guess for the parameters ?
  for k=0,nf_usefit-1 do a(0 + k * 2) = aguess(k) ; init values for amplitudes
  for k=0,nf_usefit-1 do a(1 + k * 2) = pguess(k) ; init values for phases
  a_init = a

 fit  = curvefit(time, data, weights, a,  sigma,  function_name = 'wire_epscep_funct')

 ampl = a(k2) & phase = a(k2+1)
 w = where(ampl lt 0.,c)
 if c ge 1 then begin
  ampl(w) = -1D * ampl(w) & phase(w) = phase(w) + 0.5
 endif
 phase = phase mod 1D
 w2 = where(phase lt 0.,c2) & if c2 ge 1 then phase(w2) = phase(w2) + 1D
 w3 = where(phase gt 1.,c3) & if c3 ge 1 then phase(w3) = phase(w3) mod 1D

if debug then begin
 lcfit = fltarr(np)
 for i=0,nf_usefit-1 do lcfit = lcfit + ampl(i) * sin( 2D * !DPI * (time * freqfit(i) + phase(i))) 

 col=getcolor(/load)
 plot,time,data,xr=[11,13],yr=[-.03,.02],psym=1,symsi=.5
 oplot,time,lcfit,col=col.sky
 oplot,time,data-lcfit-0.02,psym=1

 plot, data,yr=[-.03,.02],psym=1,symsi=.5
 oplot,lcfit,col=col.sky
 oplot,data-lcfit-0.02,psym=1
endif
 
 resfix(i).freq  = freqfit
 resfix(i).ampl  = ampl
 resfix(i).phase = phase
; resfix(i).rms   = robust_sigma(data-lcfit)

endfor



END
