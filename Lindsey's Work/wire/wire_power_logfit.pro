PRO wire_power_logfit, x, y, out, out2, fmin=fmin, fmid=fmid,u,  $
 p=pars, debug=debug, even_weigh=even_weigh, fskip=fskip, skip2=skip2,method=method,$
 flow_up=flow_up, kpower=kpower

; ====================================================================================
;  Set the default values if keywords are not given:
; ====================================================================================
default9, fmin, 200.
default9, fmid, 2500.
default9, debug, 0B
default9, even_weigh, 1B
default9, fskip,  0B ; cut out a given freq. range?
default9, fakip2, 0B
default9, method, 0B ; method for counting #data points pr. freq. interval
default9, flow_up, 400.
default9, kpower, 0 ; to fit the exponent, set to 1! Harvey fit(exp=2.0) set kpower==0

; ====================================================================================
if n_elements(fskip) ge 2 then $
wfit = where(y gt 0. and x gt fmin and $
            ( x lt fskip(0) or x gt fskip(1) ), cfit) 

if n_elements(fskip) ge 2 and n_elements(skip2) ge 2 then begin
wfit = where(y gt 0. and x gt fmin and $
            ( x lt fskip(0) or x gt fskip(1) ) and $
            ( x lt skip2(0) or x gt skip2(1) ), cfit) 
print,' %%% Two skip freq. intervals! '
endif

if n_elements(fskip) le 1 then $
 wfit = where(y gt 0. and x gt fmin,cfit)

; flow: set weights below this freq == all points have equal weights!
wire_power_weights, x(wfit), y(wfit), wei2, nn2=nn2, flow=fmin*2.0, fref=fmid, $
 method=method ; , /debug ; Get logarithmic weights!
; ====================================================================================

if kpower eq 1 then begin
 wlow  = where(x(wfit) lt (flow_up) and x(wfit) gt fmin,clow)
 whigh = where(x(wfit) lt 50000. and x(wfit) gt 5000.,chigh)
 if chigh lt 50 then $ 
 whigh = where(x(wfit) lt 50000. and x(wfit) gt 3000.,chigh)
  pars = [median(y(wfit(wlow))), .003, 2.0, median(y(wfit(whigh)))]
  endif

if kpower eq 0 then begin
 wlow  = where(x(wfit) lt (flow_up) and x(wfit) gt fmin,clow)
 whigh = where(x(wfit) lt 50000. and x(wfit) gt 5000.,chigh)
 if chigh lt 50 then $ 
 whigh = where(x(wfit) lt 50000. and x(wfit) gt 3000.,chigh)
 pars = [median(y(wfit(wlow))), .003, median(y(wfit(whigh)))]
endif

; ====================================================================================

; ====================================================================================
if debug then begin
 col=getcolor(/load)

 ; y1 = min(x) & y2 = max(x)
 plot_oo,x(wfit),y(wfit), xtit='Frequency', $
  ytit='Power', tit='Mark Freq. ranges',thick=2,xr=[1,3e4]

 if n_elements(fskip) eq 2 then begin
  oplot,fskip(0)*[1,1],[1,100] &  oplot,fskip(1)*[1,1],[.1,100]
 endif

 if n_elements(skip2) eq 2 then begin
  oplot,skip2(0)*[1,1],[1,100] &  oplot,skip2(1)*[1,1],[.1,100]
 endif

 oplot,x(wfit),y(wfit),col=col.sky
 oplot,x(wfit(wlow)),y(wfit(wlow)),thick=2,col=col.red
 oplot,x(wfit(whigh)),y(wfit(whigh)),thick=2,col=col.green
 hitme, s
endif
; ====================================================================================

if kpower eq 1 then begin
 fct = 'wire_power_lawk' ; 4 parameter fit
 wmid = where(abs(x(wfit)-fmid) lt (fmid*0.3),cmid) & frq = median(y(wfit(wmid)))
 best = (pars(0)-frq+pars(3)) / ( (frq-pars(3)) * fmid^pars(2) )
 best = best ^ (1. / pars(2)) & pars(1) = best
endif


if kpower eq 0 then begin
 fct = 'wire_power_lawk2'
 wmid = where(abs(x(wfit)-fmid) lt (fmid*0.3),cmid) & frq = median(y(wfit(wmid)))

 best = sqrt( (pars(0) / (frq - pars(2))) - 1.0 ) / fmid
 pars(1) = best

endif

 if not finite(best) then begin
  print,' %%% Harvey power fit failed ... '
  ; stop
 endif

; ====================================================================================
; ====================================================================================

if debug then print,' Init pars: ',pars


; ====================================================================================
pow = curvefit(x(wfit),y(wfit), wei2, pars, sigma, function_name=fct)
fre = x(wfit)

; Reiterate:
pow = curvefit(x(wfit),y(wfit), wei2, pars, sigma, function_name=fct)
fre = x(wfit)
; ====================================================================================


; ====================================================================================
r = string( pars(1) * 1e6 / (2. * !PI), format='(F8.2)') + '+-' + $
    string( sigma(1) * 1e6 / (2. * !PI), format='(F8.2)')
; ex = string( pars(2) , format='(F8.3)') + '+-' + $
;     string( sigma(3) , format='(F8.3)')
r = strcompress(r,/remove_all) ; & ex = strcompress(ex,/remove_all)
; ====================================================================================


; ====================================================================================
print,' %%% Time scale: ' + r + ' s'
; print,' XXX Exponent: ' + ex 
; ====================================================================================


; ====================================================================================
n = n_elements(fre)
out = fltarr(2,n)
out(0,*) = fre
out(1,*) = pow

ii = interpol(out(1,*),out(0,*),x)
out2 = fltarr(2,n_elements(x))
out2(0,*) = x
out2(1,*) = ii
; ====================================================================================


if debug then print,' Final pars: ',pars

end
