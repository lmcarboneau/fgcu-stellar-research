PRO wire_excessfit, f, d, f1, f2, fall, dall, fudge=fudge, silent=silent

default9, fudge, 0B
default9, silent, 0B

ff = [ 383.2,  1155.0,    3213.5   ]
dd = [  12.107,   3.226,     0.9388]

ff = [ 400.37568, 2479.6659]
dd = [ 12.492741, 0.95614644]


; Fit line based on the actual data -- otherwise keep a FIXED fit for
;                                      all data!
if fudge eq 0 then begin
 w1 = where(abs(f-f1) lt f1 * 0.1, c1)
 w2 = where(abs(f-f2) lt f2 * 0.1, c2)
 dd = [ median(d(w1)), median(d(w2)) ]
 ff = [ f1, f2 ]
endif else begin
 if silent eq 0 then $
  print,' %%% wire_excessfit.pro: Line fit is fudged, ie equal for all data sets!'
endelse

a = poly_fit(alog10(ff),alog10(dd),1,myfit,sig)

fall = .1 + 25000. * (findgen(10000)/9999.)
fact = 10.^(a(0))
slop = a(1)
dall = fact * fall ^ slop

if fudge then begin
 wn = where(f gt 5000,c)
 mm = avg(d(wn))
 wg = where(dall lt mm,cg)
 if cg ge 1 then dall(wg) = mm

 wn2 = where(f gt 40 and f lt 110,c)
 mm2 = avg(d(wn2))
 wg2 = where(dall gt mm2,cg)
 if cg ge 1 then dall(wg2) = mm2


endif

; plot_oo,f,ddsmooth(d,stiff=3)
; oplot,fall,dall,col=col.magenta

END
