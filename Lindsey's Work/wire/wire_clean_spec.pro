PRO wire_clean_spec, freq, amp, colx=colx
; nfreq, namp


col=getcolor(/load)
default9, col, col.white


; plot_oo,res0(0,*)*fj,smooth((res0(1,*)^2.)/w99,200,/edge_truncate),$
;  yr=[.4,400],ysty=3,xr=[10,10000]

; fr = replicate( {f:fltarr(500), a: fltarr(500)}, 10)

;w = where( freq lt 21.5 or (freq gt 73. and freq lt 112.) or $
;          (freq gt 252. and freq lt 288),c)

w = where( freq lt 21.5, c) & oplot, freq(w), amp(w), col=colx
w = where( freq gt 73. and freq lt 112.,c)  & oplot,freq(w),amp(w), col=colx
w = where( freq gt 252. and freq lt 288, c) & oplot,freq(w),amp(w), col=colx
w = where( freq gt 422 and freq lt 456., c) & oplot,freq(w),amp(w), col=colx
w = where( freq gt 580 and freq lt 630., c) & oplot,freq(w),amp(w), col=colx
w = where( freq gt 650 and freq lt 675., c) & oplot,freq(w),amp(w), col=colx
w = where( freq gt 720 and freq lt 810., c) & oplot,freq(w),amp(w), col=colx
w = where( freq gt 880 and freq lt 975., c) & oplot,freq(w),amp(w), col=colx
w = where( freq gt 1060 and freq lt 1080., c) & oplot,freq(w),amp(w), col=colx
w = where( freq gt 1115 and freq lt 1135., c) & oplot,freq(w),amp(w), col=colx






END
