PRO wire_print_par, wire, out=out, debug=debug, low_f_limit=low_f_limit

; ===============================================================================
n = n_elements(wire)
out = strarr(n,17)
out(*,*) = '*0'

default9, low_f_limit, 1.5 ; freq. above 1.5 c/day
; ===============================================================================

; ===============================================================================
formaz  = '(I8, A8, A5, A6, A9, A11, A10, I4, F5.1, A8, 2F6.1, 4X, 3I5, 2I6)'
formaz2 = '(A8, A8, A5, A6, A9, A11, A10, A4, A5,   A8, 2A6  , 4X, 3A5, 2A6)'

div = '----------------------------------------------------------' + $
      '----------------------------------------------------------'
; ===============================================================================

; ===============================================================================
print,div
print,'','SPEC','','','EVOL.','PRIMARY','DATE OF','','',$
 'LC','NOISE','LEVEL',  'AMPL.','SPEC.','NOISE',$
 'ALL.', '>'+strcompress(string(low_f_limit,format='(F9.1)'),/remove_all),$
 format=formaz2
print,'HD','TYPE','V','B-V','STATUS','TARGET','OBS','SL.','Tobs',$
 'CLEAN','RAW','THEO','90','160','430',$
 'FREQ.','c/day', $
 format=formaz2
print,div
; ===============================================================================

; ===============================================================================
default9, sp_max, 6
default9, debug, 0
; ===============================================================================

; ===============================================================================
for i=0,n-1 do begin
; ===============================================================================

; ===============================================================================
s = strlen(wire(i).spec)
out(i,0) = strcompress(string(wire(i).hd,format='(I9)'),/remove_all)
out(i,1) = strmid(wire(i).spec, 0, min([s,sp_max]))
out(i,2) = strcompress(string(wire(i).v, format='(F9.1)'),/remove_all)
out(i,3) = strcompress(string(wire(i).v, format='(F9.2)'),/remove_all)
; ===============================================================================

; ===============================================================================
case wire(i).lumclass of
 0: out(i,4) = 'Unknown'
 1: out(i,4) = 'Super'
 2: out(i,4) = 'Super'
 3: out(i,4) = 'Giant'
 5: out(i,4) = 'Evolv'
 4: out(i,4) = 'M.S.'
endcase
; ===============================================================================

; ===============================================================================
out(i,5) = wire(i).primnam
out(i,6) = wire(i).period
out(i,7) = wire(i).entry
out(i,8) = strcompress(string(wire(i).tobs,format='(F9.1)'),/remove_all) ; observation time in days
; ===============================================================================

; ===============================================================================
 lc = wire(i).lcsub(0:wire(i).np-1)
 ptp_robust_fin, lc * 1e3/1.086, ptp, 1

 lc = wire(i).lc(0:wire(i).np-1)
 ptp_robust_fin, lc * 1e3/1.086, ptp_raw, 1

out(i,9) = strcompress(string(ptp,format='(F9.1)'),/remove_all) ; noise in ppt after subtr. all modes
out(i,10) = strcompress(string(ptp_raw,format='(F9.1)'),/remove_all) ; noise in ppt after subtr. all modes
; ===============================================================================

; ===============================================================================
; Theoretical noise level:
; ===============================================================================
     gain = 15.0 ; electrons pr. ADU
     err_ff = 10000000. * gain ; data kept on the same pixels
     counts = 10.^((25. - wire(i).mmag) / 2.5) * gain ; number of electrons
     sky = 100. * gain ; ?
     ron = 5.0 ; I guess the read out noise for WIRE
     avg_fwhm = median(wire(i).fwhm(0:wire(i).np-1))
     r_aperture = avg_fwhm * 1.5
                                ; Variance in Ap. phot according to
                                ; Kjeldsen & Frandsen, PASP 104, 413, 192:

     var_ptp = (2. * alog(2.) ) / (avg_fwhm^2.0 * !PI * err_ff) + $
               1.0 / counts + $
               !PI * (r_aperture^2.0) * ( (sky + 5.0^2.0) / (counts^2.0) )

     nmerge = 31. ; every 31st data point is merged to a single data point
     sig_ptp = 1e3 * sqrt(var_ptp / (nmerge-1.) )

out(i,11) = strcompress(string(sig_ptp,format='(F9.1)'),/remove_all)
if ptp / sig_ptp gt 2. then out(i,9) = '*' + out(i,9) ; MARK WARNING
; ===============================================================================

; ===============================================================================
; Calculate noise level in amplitude spectrum:
; ===============================================================================
f = wire(i).freq(2,*)
a = wire(i).ampl(2,*) ; cleaned ampl. spectrum
w = where(f gt 0. and a gt 0.,c)

fraw = wire(i).freq(0,*)
araw = wire(i).ampl(0,*) ; cleaned ampl. spectrum
wraw = where(fraw gt 0. and araw gt 0.,craw)

noise = fltarr(2,4)
if c le 10 then goto, skip_spec

f = f(w) & a = a(w)
fraw = fraw(wraw) & araw = araw(wraw)
; ===============================================================================

; ===============================================================================
if debug then begin
 plot_oo,fraw,araw,tit='Raw spectrum (white) -- Cleaned (red)'
 oplot,f,a,col=250
 hitme,s9 & if s9 eq 'x' then stop
endif
; ===============================================================================

; ===============================================================================
f1 = 4. & f2 = 11.
for k=0,3 do begin
 w1 = where(f gt (f1+k*wire(i).orbital) and f lt (f2+k*wire(i).orbital),c)
 if c ge 10 then begin
  noise(0,k) = f1 + (f2-f1)*0.5 + k * wire(i).orbital
  resistant_mean, a(w1), 3, me, sd, nr
  noise(1,k) = me ; robust noise level

  if debug then begin
   plot,f,a,tit='Cleaned spectrum: noise level btw. orbital harmonic freq.'
   oplot,f(w1),a(w1),col=250
   plots,!x.crange,me,col=200,thick=4,line=2
   plots,!x.crange,me*4.,col=200,thick=4,line=2
   hitme,s9 & if s9 eq 'x' then stop
  endif
 endif
endfor 
; ===============================================================================

; ===============================================================================
; Noise levels in subtracted lc amplitude spectrum
; ===============================================================================
out(i,12) = strcompress(string(noise(1,0),format='(I9)'),/remove_all)
out(i,13) = strcompress(string(noise(1,1),format='(I9)'),/remove_all)
out(i,14) = strcompress(string(noise(1,2),format='(I9)'),/remove_all)

; Theoretical noise level in amplitude spectrum:
theo = sqrt(2./wire(i).np) * sig_ptp * 1e3 ; in ppm
if noise(1,0)/theo gt 20. then out(i,12) = '*' + out(i,12)
if noise(1,1)/theo gt 20. then out(i,13) = '*' + out(i,13)
if noise(1,2)/theo gt 20. then out(i,14) = '*' + out(i,14)

; In Procyon the noise level is about 20 times higher at 10 c/day
; compared to the noise level around 500 c/day !!

; ===============================================================================


; ===============================================================================
; Count number of frequencies that are above 4 sigma and NOT orbital
; harmonics:
noise_limit = 4.0
; ===============================================================================

; ===============================================================================
for l=0,1 do begin
 w = where(wire(i).sn gt noise_limit and wire(i).f gt float(l)*low_f_limit,c)
 count = 0

 if c ge 1 then begin
  for j=0,c-1 do begin ; check for orbital freq
   fcheck = wire(i).f(w(j))
   worb = where(abs(fcheck - wire(i).f3) lt 0.0001,corb)
   if corb eq 0 then count=count + 1
  endfor
 endif

 out(i,15+l) = strcompress(string(count,format='(I9)'),/remove_all)
endfor
; ===============================================================================

skip_spec:

print,out(i,*),format=formaz2

; stop

endfor ; go to next star

END
