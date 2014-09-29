PRO wire_power_weights, x, y, ww, fref=fref, debug=debug, nlimit=nlimit, $
 f1=f1, f2=f2, nn2=nn2, farr=farr, narr=narr, flow = flow, method=method, unit=unit

; ================================================================
default9, unit, 'microHz'
case unit of 
 'microHz': fact = 1.0
 'milliHz': fact = 1e3
 'c/day':   fact = 1e6/86400.
endcase
; ================================================================

; ================================================================
; Set up default values:
default9, fref, 1000./fact ; Weights normalized around this frequency
default9, debug, 0B
default9, nlimit, 2. ; At least this may points to assign weight
; Frequency interval:
default9, f1,    50./fact ; min freq.
default9, f2, 15000./fact ; max freq.
default9, flow,  50./fact
default9, method, 0B
; ================================================================


; ================================================================
logf1 = alog10(f1)
logf2 = alog10(f2)
np = 500.
fr = logf1 + (logf2 - logf1) * findgen(np) / (np-1.)
; ================================================================

; ================================================================
farr = fltarr(np-1)
narr = farr
alogfr = 10.^fr
; ================================================================

; ================================================================
;  Count the number of data points within each log freq. interval
; ================================================================
for i=0,np-2 do begin
 w = where(x ge (alogfr(i)) and x le alogfr(i+1),c)
  if c ge 2 then farr(i) = avg(x(w))
  narr(i) = c 
endfor
; ================================================================


; ================================================================
;  Make linear fit to log(freq) / log(n_data_points)
; ================================================================

if method eq 0 then begin
; This method assumes that the fit konstant, s(1) === 1.0
w = where(narr gt 1 and farr gt 500.,c)
fact = median(narr/farr)
s = [alog10(fact), 1.0]
nn = 10.^(s(0) + s(1) * alog10(x)) ; number of data points at each frequency interval

if debug then begin
 plot,farr,narr,psym=-6
 oplot,x,nn,thick=3
 hitme, g
endif
endif

if method eq 1 then begin
; ================================================================
a = alog10(farr) ; frequency
b = alog10(narr) ; number of data points
w = where(narr gt 1 and b gt 0.,c)
s = robust_poly_fit(a(w),b(w),1,myfit)
nn = 10.^(s(0) + s(1) * alog10(x)) ; number of data points at each frequency interval
; ================================================================

ninit = farr

; ================================================================
; Re-iterate:
fit = 10.^(s(0) + s(1) * alog10(farr)) 
wxx = where(narr gt 1 and b gt 0. and narr gt (fit*.8-5),cxx)
s2 = robust_poly_fit(a(wxx),b(wxx),1,myfit)
nn2 = 10.^(s(0) + s(1) * alog10(x)) ; number of data points at each frequency interval
; ================================================================

; ================================================================
; Re-iterate:
fit2 = 10.^(s2(0) + s2(1) * alog10(farr)) 
wxx = where(narr gt 1 and b gt 0. and narr gt (fit2*.7-5),cxx)
s3 = robust_poly_fit(a(wxx),b(wxx),1,myfit)
nn3 = 10.^(s3(0) + s3(1) * alog10(x)) ; number of data points at each frequency interval

s = s3
nn = nn3 ; final result for the weights
; ================================================================
endif


if debug then print,' %%% Final slope (wire_power_weights.pro): ',s
; ================================================================

; ================================================================
if debug then begin
 plot_oo,x,nn, xtit='Frequency',ytit='Number of data points'
 oplot,farr,narr,psym=-6
 hitme, 1
endif
; ================================================================


; ================================================================
wd = where(abs(x-fref) lt (fref*0.2),ccd)

if ccd lt 10 then begin
 print,' %%% No points close to reference frequency: ',fref
 stop
endif
; ================================================================

; ================================================================
nn2 = nn 
wbad = where(nn lt nlimit,cbad) ; too few data points ?
if cbad ge 1 then nn2(wbad) = 0.
wgood = where(nn ge nlimit,cgood)

ww = dblarr(n_elements(x))
ww(wgood) = median(nn2(wd)) / nn2(wgood)

wlow  = where(x lt flow and nn gt 0.,cmin)
whigh = where(x gt flow and nn gt 0. and ww gt 0.,chigh) 
if (cmin ge 1) and (chigh ge 1) then begin
 ww(wlow) = ww(whigh(0)) ; cutoff weights!
 if debug then print,' %%% Equal weights for freq below: ',flow,cmin
endif

ww = ww / total(ww)
; ================================================================


; ================================================================
if debug then begin

; plot_oo,nn,ww,ysty=3,yr=[1e-6,1],$
;  xtit='Number of data points',ytit='Weight'

 plot_oo,x,ww,ysty=3,yr=[1e-6,1],$
  xtit='Frequency',ytit='Weight',xr=[flow*.5,f2],xsty=3

 oplot,flow*[1.,1],[1e-6,1]
 xyouts,flow,.01,string(flow,format='(F8.2)') + '!4l!3Hz'

 hitme,s & if s eq 'g' then stop
endif
; ================================================================

; plot,f1,w1,xr=[0,100],psym=-6


END
