; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PRO wire_func_fixed_freq, x, a, f, pder
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Function evaluates the function value and partial derivatives
; Purpose: Used with curve_fit when fitting a number of 
; harmonic functions to real data. Ampl + phase are fitted.
; (c) March 2006, Hans Bruntt

; a "Work-around":
; Input frequencies are read from a file:
m4_get_basedir, base
default9, input_file, base + 'temp/inpfreq_ds.dat' ; THIS IS THE ONLY POSSIBLE FILE!!

readcol, input_file, freq, format='D', /silent
nf = n_elements(freq) ; number of frequences = number of modes

; Set up the arrays:
nfit = n_elements(a)
np = n_elements(x)
f = dblarr(np)

; Calculate sum of harmonics:
for k=0,nf-1 do $
 f = f + a(k*2) * sin( 2. * !DPI * (freq(k) * x + a(k*2+1)) )

; Calculate partial derivatives
 pder = dblarr(np, nfit)

 for k=0,nf-1 do begin
  pder(*,    k*2) =                          sin( 2. * !DPI * ( freq(k) * x + a(1 + k*2 ) ) )
  pder(*,1 + k*2) = 2. * !DPI * a(0 + k*2) * cos( 2. * !DPI * ( freq(k) * x + a(1 + k*2 ) ) )
 endfor

END
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PRO wire_func_variable_freq, x, a, f, pder
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Function evaluates the function value and partial derivatives
; Purpose: Used with curve_fit when fitting a number of 
; harmonic functions to real data. Freq + Ampl + phase are fitted.
; (c) March 2006, Hans Bruntt

; Set up the arrays:
nfit = n_elements(a)
np = n_elements(x)
f = dblarr(np)
nf = ceil(n_elements(a) / 3D ) ; must be 3 * even number !
if (nf - floor(n_elements(a) / 3.)) ne 0 then stop

; Calculate sum of harmonics:
for k=0,nf-1 do $
 f = f + a(k*3) * sin( 2D * !DPI * (a(k*3+2) * x + a(k*3+1)) )

; Calculate partial derivatives
 pder = dblarr(np, nfit)

; The partial derivatives of A = SUM a_i * sin (2D*!DPI* [f_i * time +p_i])
; time is "x"

 for k=0,nf-1 do begin
  pder(*,    k*3) =                              sin( 2. * !DPI * ( a(2 + k*3) * x + a(1 + k*3 ) ) )
  pder(*,1 + k*3) = 2. * !DPI * a(0 + k*3) *     cos( 2. * !DPI * ( a(2 + k*3) * x + a(1 + k*3 ) ) )
  pder(*,2 + k*3) = 2. * !DPI * a(0 + k*3) * x * cos( 2. * !DPI * ( a(2 + k*3) * x + a(1 + k*3 ) ) )
 endfor

END
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PRO wire_master_clean, file, minfreq, maxfreq, nfreq, result, $
 fit=fit, noweights=noweights, wildcard=wildcard, output=output, $
 freqforce=freqforce, startid=startid,dfreq_fine=dfreq_fine,$ 
 factor_pts=factor_pts
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Purpose:
; Clean a number (nfreq) of frequencies in a given
; frequency interval (minfreq-->maxfreq). This is
; done by calculating the amplitude spectrum, finding
; the highest peak, and fitting the light curve
; with that frequency as input using CURVEFIT.PRO.
; The mode is subtracted from the LC, and the 
; highest peak is found, and so on.
;
; Example:
; .r wire_master_clean
; wire_master_clean,'/export/brixx1/bruntt/wire/EpsilonCep/care8/lc_OSN_y_wei_p1.dat',13,15,2,res

; Do cleaning of several files if wildcard was given as input
if n_elements(wildcard) eq 1 then begin
 spawnrob,'ls -1 ' + wildcard, file
 nfile = n_elements(file)
 if nfile eq 0 then begin
   print,' *** WARNING: no files found: ' + wildcard
   RETURN
 endif
endif else begin
  thea = findfile(file,Count=nfile)
  if nfile ne 1 then begin
    print,' %%% File not found: ' + file
    RETURN
  endif
endelse
; --------------------------------------------------------------

; --------------------------------------------------------------
default9, debug, 1B
default9, output, 1B ; save structure with results
default9, factor_pts, [6.,30,100] ; resolution in FFT calculation
factor_pts = float(factor_pts)
nfac = n_elements(factor_pts)
default9, noweights, 0B
default9, startid, 0L

m4_get_basedir, basedir
if n_elements(ds_dir) eq 0 then ds_dir = basedir + 'temp/' ; temp-dir for dennis' Fourier analysis
; IMPORTANT: THIS MUST MATCH THE FILE IN wire_func_fixed_freq ABOVE!!
; --------------------------------------------------------------

print,' %%% Cleaning this file(s)'
for fil=startid,nfile-1 do print,file(fil)
wait,1.5

; --------------------------------------------------------------
; Do clean for each file:
; --------------------------------------------------------------
for fil=startid,nfile-1 do begin
 fileuse = file(fil)

print,' %%% Cleaning '+string(nfreq,format='(I3)')+' modes from file: ' + fileuse

; --------------------------------------------------------------

; --------------------------------------------------------------
; Import data using double precision
readcol,fileuse,time,data,weights,format='d,d,d'

ndat = n_elements(weights) & if ndat le 5 then stop

weights = weights / total(weights)
if noweights then weights(*) = 1
ndata = n_elements(data)

; Time baseline: used for calc. number of points to use
delta_time = (max(time)-min(time))
; --------------------------------------------------------------

; Set up structure with the result of the fitting
default9, nstore,500
fit = replicate( {data:dblarr(ndata), fit:dblarr(ndata), $
                  freq:0D, amp:0D, phase:0D, $
                  f:fltarr(nstore), a:fltarr(nstore)}, nfreq)
; data: fit was made to these data (cleaned modes subtracted)
; fit: the fitted LC to the data
; freq, amp, phase: fitted frequency, amplitude, phase 
; f,a : amplitude spectrum --> frequency and amplitude

; Final results for the LSQ fit are stored in the result structure
result = replicate({freq:0D,  amp:0D,  phase:0D, $
                    efreq:0D, eamp:0D, ephase:0D, $
                    freq1:dblarr(nfreq), amp1:dblarr(nfreq), phase1:dblarr(nfreq), $
                    freq2:dblarr(nfreq), amp2:dblarr(nfreq), phase2:dblarr(nfreq), $
                    freq3:0D           , amp3:0D,            phase3:0D  }, nfreq  )

; Use a finer and finer grid, absolute max. in ampl. spectrum:
if n_elements(dfreq_fine) eq 0 then begin
  dfreq_fine =  [999., 0.05, .01]
  dfreq_fine(1) = 4./delta_time
  dfreq_fine(2) = 1./delta_time
endif


; --------------------------------------------------------------
; Clean all "nfreq" modes:
; --------------------------------------------------------------
for ff=0,nfreq-1 do begin

; Fit to these data:
if ff ge 1 then $
 fit(ff).data = data - fit(ff-1).fit else $
 fit(ff).data = data 

; --------------------------------------------------------------
; Look for the highest peak in the ampl. spectrum:
; Do this iteratively, in second/third steps use a 
; higher number of points around the frequency of the highest peak
; Number of iterations == nfac
; --------------------------------------------------------------
for k=0,nfac-1 do begin
; --------------------------------------------------------------

; --------------------------------------------------------------
; If freqforce is set, the program will ONLY search for
; frequencies in small intervals around freqforce(i)
; --------------------------------------------------------------
if n_elements(freqforce) eq 0 then begin
 if k eq 0 then begin
  maxfreq_use = float(maxfreq)
  minfreq_use = float(minfreq)
 endif else begin
  w = where(amp eq max(amp),c) & w = w(0) & c = 1
  freq_max = freq(w)
  minfreq_use = freq_max - dfreq_fine(k)
  maxfreq_use = freq_max + dfreq_fine(k)
 endelse
; print,' %%% Region of clean [microHz]: ', minfreq_use*11.574, maxfreq_use*11.574

endif else begin

; FORCED FREQ (e.g. OSN / EPSCEP dataset with a complicated spectral window)
 if k eq 0 then begin
  maxfreq_use = freqforce(ff) - dfreq_fine(1) * 1.5
  minfreq_use = freqforce(ff) + dfreq_fine(1) * 1.5

;  print,' %% Init freq range: ',minfreq_use,maxfreq_use 

 endif else begin
  w = where(amp eq max(amp),c) & w = w(0) & c = 1
  freq_max = freq(w)
  minfreq_use = freq_max - dfreq_fine(k)
  maxfreq_use = freq_max + dfreq_fine(k)
 endelse
;
endelse
; --------------------------------------------------------------



npts = factor_pts(k) * (maxfreq_use - minfreq_use) * delta_time
npts = ceil(npts/100.) * 100L
if npts le 199 then npts = 200
if npts ge 15000 then help,npts ; warn user that this may take a while


; if debug then help, npts
; DS recommends Npoints > ~(max_freq-min_freq)*5*(T_end-T_start)

;
; AMPLITUDE SPECTRUM IS CALCULATED WITH ORG. TIME BUT DATA...
; DATA = fit(ff).data <--- prev. cleaned modes subtracted !!
;
four_trans_ds, time, fit(ff).data, weights, $
               ds_dir,0,1,minfreq_use,maxfreq_use,npts,$
               0,99,freq,amp,phase,alpha,beta,/silent

 w = where(amp eq max(amp),c) & w = w(0) & c = 1
 freq_max = freq(w)

if debug then begin
 col=getcolor(/load)
;; if k eq 0 then plot,freq,amp,xr=freq_max + [-1,1]*dfreq_fine(k+1),psym=-7 else $
 if k eq 0 then plot,freq,amp,xr=[minfreq,maxfreq],psym=-7 else $
 oplot,freq,amp,col=col.sky,psym=-6
 plots,freq_max,!y.crange,line=2,col=col.red,thick=2
endif

; Store the temporary results:
result(ff).amp3   = amp(w)
result(ff).phase3 = phase(w)
result(ff).freq3  = freq(w)

endfor ; next iteration in ampl. spectr. calculation
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; -------------------------------------------------------------------------------
; In the first fit, we keep frequencies fixed, only fitting ampl +
; phase to all found modes!
; -------------------------------------------------------------------------------

; -------------------------------------------------------------------------------
nfit = ff + 1 ; number of freq.s to fit
a = dblarr(nfit * 2) ; initial guesses for the parameters ?  ; help,a

; Store the GUESS for the paramters
; For the *new* mode you've just found, store freq,amp,phase from AMPL. spectrum
; For the modes you have already cleaned, store the improved values!
for mode=0,nfit-1 do begin
 if mode eq (nfit-1) then begin
   a(0 + mode*2) = result(ff).amp3         ; amplitude from ampl. spectrum
   a(1 + mode*2) = result(ff).phase3       ; phase     from ampl. spectrum
 endif else begin
   a(0 + mode*2) = result(ff-1).amp1(mode)   ; ampl. from previous LSQ fit of FAP*
   a(1 + mode*2) = result(ff-1).phase1(mode) ; ampl. from previous LSQ fit of FAP
 endelse
endfor

; stop


; FAP = F.A.P. = frequency, amplitude AND phase simulaneously fitted by LSQ fit
; -------------------------------------------------------------------------------

; -------------------------------------------------------------------------------
;if debug then begin
; print,a
; hitme,mess='debug -- x == stop',s9 & if s9 eq 'x' then stop
;endif
; -------------------------------------------------------------------------------

; if debug then help,nfit

; -------------------------------------------------------------------------------
; You must print frequencies in a file (they are read by wire_func_fixed.pro)
freqfile = basedir + '/temp/inpfreq_ds.dat' ; temporary file
get_lun,u  &  openw,u,freqfile ; OPEN THE FILE
freq_out = dblarr(nfit)
  for p=0,nfit-1 do begin
    if p eq (nfit-1) then $
     freq_out(p) = result(p).freq3 else $
     freq_out(p) = result(p).freq1(p)                     
    printf,u, freq_out(p), format='(D16.8)' ; WRITE THE FILE!
  endfor
close,u & free_lun, u ; CLOSE the temporary file
; -------------------------------------------------------------------------------

;if debug then begin
; help,a        &  print,a
; help,freq_out &  print,freq_out
; hitme,s9,mess='Init fit set up! x = break!' & if s9 eq 'x' then stop
;endif

; -------------------------------------------------------------------------------
; Fit all modes to original light curve with FIXED frequency: From wire_epscep_sim3.pro
fitv  = curvefit(time, data, weights, a, sigma, function_name = 'wire_func_fixed_freq')
; -------------------------------------------------------------------------------

; -------------------------------------------------------------------------------
; For negative amplitudes, invert and shift phase !
k2 = indgen(nfit) * 2
 wneg = where(a(k2) lt 0.,cneg)
 if cneg ge 1 then begin
   a(k2(wneg))   =  abs( a(k2(wneg)) )
   a(k2(wneg)+1) =  (a(k2(wneg)+1) + 0.5)   mod 1.0
 endif

; Shift phases to be in the interval 0..1
 wneg_phase = where(a(k2+1) lt 0.,cneg_phase)
 if cneg_phase ge 1 then a(k2(wneg_phase)+1) = (a(k2(wneg_phase)+1) + 1.0) mod 1.0
; -------------------------------------------------------------------------------
 
; Store the improved amplitude and phase, from LSQ fit; freq = constant
result(ff).freq2(0:nfit-1)  = freq_out ; Fixed frequencies
result(ff).amp2(0:nfit-1)   = a(k2)    ; Fitted ampl  for fixed freq
result(ff).phase2(0:nfit-1) = a(1+k2)  ; Fitted phase for fixed freq
; -------------------------------------------------------------------------------

if debug then begin
 plot,time,data,psym=-1,symsi=.5 ; ,xr=[11.3,11.7]

 tempfit = fltarr(ndata)
 for p=0,nfit-1 do begin
    tempfit = tempfit + a(0+2*p) * sin(2D * !DPI * (freq_out(p)*time + a(1+2*p)))
 endfor

 oplot,time,tempfit,col=col.sky

; hitme,mess='Blue Curve = Fit -- it it ok? -- x == stop',s9 & if s9 eq 'x' then stop
endif
; -------------------------------------------------------------------------------


; -------------------------------------------------------------------------------
;if debug then begin
; print,a
;; hitme,mess='Full LSQ fit done -- x == stop',s9 & if s9 eq 'x' then stop
;endif
; -------------------------------------------------------------------------------


; -------------------------------------------------------------------------------
; In the second fit, ampl + freq + phase are fitted simultaneously to
; all the modes found to far!
; -------------------------------------------------------------------------------

; -------------------------------------------------------------------------------
nfit = ff + 1 ; number of freq.s to fit in this clean step
b = dblarr(nfit * 3) ; initial guesses for the three parameters, ampl + phase + freq

; if ff ge 1 then stop

for mode=0,nfit-1 do begin
 if mode eq (nfit-1) then begin
  b(0 + mode*3) = result(ff).amp2(nfit-1)   ; improved amp (freq fixed)
  b(1 + mode*3) = result(ff).phase2(nfit-1) ; improved phase (freq fixed)
  b(2 + mode*3) = result(ff).freq2(nfit-1)  ; From maximum in ampl. spectrum
 endif else begin
  b(0 + mode*3) = result(ff-1).amp1(mode)   ; improved amp (var. fixed)
  b(1 + mode*3) = result(ff-1).phase1(mode) ; improved phase (var. fixed)
  b(2 + mode*3) = result(ff-1).freq1(mode)  ; improved freq 
 endelse
endfor
; -------------------------------------------------------------------------------

; -------------------------------------------------------------------------------
;if debug then begin
; print,'' & print,''
; help,b
; print,b
;; hitme,mess='Full LSQ fit TBD -- x == stop',s9 & if s9 eq 'x' then stop
;endif
; -------------------------------------------------------------------------------

; -------------------------------------------------------------------------------
; Fit all modes to original light curve with FIXED frequency: From wire_epscep_sim3.pro
fitv  = curvefit(time, data, weights, b, sigma, function_name = 'wire_func_variable_freq')
; -------------------------------------------------------------------------------

; -------------------------------------------------------------------------------
; Store the fit for the next iteration!
; -------------------------------------------------------------------------------
; STORE HARMONIC FIT PARAMETERS:
k3 = indgen(nfit)*3 ; PICK THE RIGHT ENTRIES IN FINAL "FAP" FIT FOR THIS CLEAN STEP
result(ff).amp1(0:nfit-1)   = b(0+k3)
result(ff).phase1(0:nfit-1) = b(1+k3)
result(ff).freq1(0:nfit-1)  = b(2+k3)

; STORE FIT:
for p=0,nfit-1 do $
 fit(ff).fit = fit(ff).fit + b(0+3*p) * sin(2D * !DPI * (b(2+3*p)*time + b(1+3*p)))
; -------------------------------------------------------------------------------



; -------------------------------------------------------------------------------
; Plot the fitted light curve and the observed data
; -------------------------------------------------------------------------------
;if debug then begin

; plot,data,psym=-1,symsi=.5,ysty=3
; for p=0,ff do   oplot,fit(p).fit,col=col.sky

;; plot,time,data,psym=-1,symsi=.5 ; ,xr=[11.3,11.7]
;; for p=0,ff-1 do   oplot,time,fit(p).fit,col=col.sky
;;
;; tempfit = fltarr(ndata) & tempfitf = tempfit
;; for p=0,ff-1 do begin
;;    tempfit  = tempfit  + a(0+2*p) * sin(2D * !DPI * (freq_out(p)*time + a(1+2*p)))
;;    tempfitf = tempfitf + b(0+3*p) * sin(2D * !DPI * (b(2+3*p)*time + b(1+3*p)))
;; endfor
;; oplot,time,tempfit,col=col.sky,thick=2 ; fit with fixed freq, variable AP
;; oplot,time,tempfitf,col=col.green,thick=2,line=5 ; fit with variable  FAP

;endif
; -------------------------------------------------------------------------------

; -------------------------------------------------------------------------------
;if debug then begin
; print,'' & print,''
; help,b
; print,b
;; hitme,mess='Full LSQ fit TBD -- x == stop',s9 & if s9 eq 'x' then stop
;endif
; -------------------------------------------------------------------------------

endfor                          ; Clean the next mode


; -------------------------------------------------------------------------------
; Store the final fit:
; -------------------------------------------------------------------------------
result.freq  = result(ff-1).freq1
result.amp   = result(ff-1).amp1
result.phase = result(ff-1).phase1

; Store the final errors on the fit:
result.eamp   = sigma(k3+0)
result.ephase = sigma(k3+1)
result.efreq  = sigma(k3+2)
; -------------------------------------------------------------------------------

; -------------------------------------------------------------------------------
; Save file with result of the cleaning + LSQ fit
; -------------------------------------------------------------------------------
if output then begin
  x = strsplit(file(fil),'.',/extract) & nx = n_elements(x)
  outfile = x(0) + '.clean.idl'
  save,filename=outfile,result,fit
  print,' %%% Saved structures: ' + outfile
endif
; -------------------------------------------------------------------------------



; -------------------------------------------------------------------------------
endfor ; go to the next file
; -------------------------------------------------------------------------------


END
