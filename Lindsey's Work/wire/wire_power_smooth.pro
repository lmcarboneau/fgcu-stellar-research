PRO wire_power_smooth, f1, d1, dsout, debug=debug, fref=fref, df=df, res=res, unit=unit

; ================================================================
default9, unit, 'microHz'
case unit of 
 'microHz': fact = 1.0
 'milliHz': fact = 1e3
 'c/day':   fact = 1e6/86400.
 else:stop
endcase
; ================================================================

; ================================================================
default9, debug, 0B
default9, df, 50./fact
default9, fref, 500./fact
default9, debug, 0B
default9, res, 0.5/fact
; ================================================================

; ================================================================
wire_power_weights, f1, d1, w1, fref=fref, nn2=nn2, unit=unit
; ================================================================

; ================================================================
nd = n_elements(f1)
ds = fltarr(nd)
; ================================================================

; ================================================================
; FWHM at each frequency is scaled with the number of points 
; in the given freq. interval
; ================================================================
wg = where(abs(f1-fref) lt (fref*0.1),cg)
deltaf = df * nn2 / avg(nn2(wg))
; ================================================================

; ================================================================
; At very low frequencies, interpolate the number of data points:
; ================================================================
wlow = where(f1 lt (250./fact) and deltaf gt 0.,clow)
wbad = where(f1 lt (250./fact) and deltaf le 0.,cbad)
if clow ge 5 and cbad ge 1 then $
 deltaf(wbad) = interpol(deltaf(wlow), f1(wlow), f1(wbad))
; ================================================================


; ================================================================
if debug then begin
 !P.charthick=1
 plot_oo,f1,deltaf,xtit='Frequency',ytit='FWHM of gaussian ['+unit+']',yr=[.1,1e4],xr=[10,3000]
 oplot,f1(wg),deltaf(wg),thick=6
 hitme, s & if s eq 'x' then stop
endif
; ================================================================


; ================================================================
if debug then begin
 plot_oo,f1,d1,xtit='Frequency',ytit='Power',yr=[.1,1e5],xr=[10,3000]/fact,xsty=3
 oplot,fref+median(deltaf)*[-1,-1.],[.1,1e5],thick=2,col=100,line=5
 oplot,fref+median(deltaf)*[ 1, 1.],[.1,1e5],thick=2,col=100,line=5
 hitme, s & if s eq 'x' then stop
endif
; ================================================================


wok = where(deltaf gt 0.,cok)
; ================================================================
fcen = deltaf(wok(0)) 
fmax = max(f1)
cnt = 0L
ds = fltarr(1e5)
fs = ds
; ================================================================

; ================================================================
;  Smooth the amplitude spectrum
; ================================================================

print,' %%% Sky colour: every 50th gauss function = smoothed'

while fcen lt fmax do begin

 fwhm = interpol(deltaf, f1, fcen)

 if fwhm eq 0 then begin
     fwhm = min(deltaf(wok))  
     print,' %%% WARNING: FUDING FWHM IN SMOOTHING ... '
     goto,skipcal
 endif

 width = 2. * (fwhm / 2.35)^2.0

 wg = where(abs(f1 - fcen) lt fwhm * 4.5, cg)

 if cg ge 1 then begin
  g = exp(- ((fcen - f1(wg))^2.0) / width )
  g = g / total(g)
  ds(cnt) = total(g * d1(wg))
  fs(cnt) = fcen

  


 endif
 
 if cnt mod 500 eq 0 then print,cnt,format='(I5,$)'

if debug then begin
  col = getcolor(/load)
  if cnt mod 50 eq 0 then $
   if cg ge 2 then begin
    oplot,f1(wg), g * max(d1) * 0.5 / max(g), col=col.sky

  plotsym,0,/fill & col=getcolor(/load)
  plots, fs(cnt), ds(cnt), psym=8,col=col.red,symsi=2
;  hitme, s
   endif

endif

skipcal:

 fcen = fcen + fwhm * res ; deltaf(cnt)
 cnt = cnt + 1

endwhile
; ================================================================


; ================================================================
if cnt ge 1 then begin
 wall = where(fs gt 1e-8 and ds gt 0.,call) ; important!
 dsout  = interpol(ds(wall),fs(wall), f1) ; get all frequencies
 dscale = (total(d1) / total(dsout))
 dsout  = dsout * dscale
 print,' %%% Scale factor: ' + strcompress(dscale)
endif else dsout = ' *** wire_power_smooth.pro: Failed'
; ================================================================


; ================================================================
if debug then begin
 hitme, s & if s eq 'g' then stop
 col=getcolor(/load)
 plot_oo,f1,d1,xr=[10,3000]
 oplot,f1,dsout,thick=2,col=col.sky
endif
; ================================================================


END

