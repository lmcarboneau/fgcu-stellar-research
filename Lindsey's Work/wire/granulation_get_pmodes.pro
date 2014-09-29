PRO granulation_get_pmodes, nf, ampX, frq, amp, pha, $
 seed = seed, debug=debug, $
  df=df, d02=d02, relamp=relamp, $
  envelope_width=envelope_width, envelope_fcen=envelope_fcen

; =================================================================
; Get p-mode envelope for use with granulation.pro
; (c) 10th of December 2004 Hans Bruntt
; =================================================================

; granulation_get_pmodes, 20., 6.0, frq, amp, pha, $
;  df=135.,d02=12.

; =================================================================
default9, debug, 0B
if n_elements(seed) eq 0 then seed = ceil(randomn(seed) * 1e5)
default9, df, 55.52 ; Procyon default for the large splitting
default9, d02, 4.93 ; Procyon default for the small splitting
default9, relamp, [1.00, 1.15, 0.60] ; rel amplitudes for l=0,1 & 2
default9, envelope_width, 480.
default9, envelope_fcen, 1100.
; =================================================================

; =================================================================
; Number of frequencies must be even numbers of three
; =================================================================
nni = floor(nf / 3.)
nf2 = nni * 3
if nf2 ne nf then begin
 print,' %%% Number of frequencies must be even numbers of three!'
 print, nf, ' ----> ', nf2
endif
nf = nf2
; =================================================================

; =================================================================
frq = findgen(nf)
amp = frq
pha = randomu(seed,nf)*2.*!DPI ; Random phases btw "0" and "2 * PI"
; =================================================================

; =================================================================
; Frequencies:  Values for Procyon: Df = 55.52, d02 = 4.93
; =================================================================
for i=0L,nni-1 do frq(i)=df*(7.+float(i)+1.658)
for i=0L,nni-1 do frq(i+nni)=frq(i)   - d02
for i=0L,nni-1 do frq(i+nni*2)=frq(i) - d02/3.+df/2.
; =================================================================

; =================================================================
; Amplitudes   (l=0,1,2)
; =================================================================
for i=0L,nni-1 do amp(i)      = relamp(0)
for i=0L,nni-1 do amp(i+nni)  = relamp(1)
for i=0L,nni-1 do amp(i+nni*2)= relamp(2)
; =================================================================

; =================================================================
; Fold amplitudes by a Gaussian to simulate the envelope!
; =================================================================
for i=0L,nf-1 do $
  amp(i) = amp(i) * $
    exp(-1.*(frq(i)-envelope_fcen)^2/envelope_width^2)/sqrt(2.) * ampX
; =================================================================

; =================================================================
;  Plot the frequencies?
; =================================================================
if debug then begin
 ss = intarr(nf) 
 ss(0:nni-1) = 6
 ss(nni:nni*2-1) = 4
 ss(nni*2:nni*3-1) = 2

 plot,frq, amp,/nodata, xr=[-1,1]*envelope_width*2.5+envelope_fcen

 for i=0,nf-1 do $
  oplot,frq(i)*[1.,1.],[0,amp(i)],psym=-ss(i),symsi=.9
endif
; =================================================================


END
