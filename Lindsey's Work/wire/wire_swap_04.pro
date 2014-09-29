PRO wire_swap_04,wire2,t0,debug

; debug = 0

   t_alt = 51474.95D ; stars swapped before this HJD time !!

if debug eq 1 then begin
   d_slot1 = -2.5 * alog10(wire2.p(0,6) ) + 25.0
   d_slot4 = -2.5 * alog10(wire2.p(4,6) ) + 25.0
   o_slot1 = 11.916 ; brightest star == altair
   o_slot4 = 13.115 ; secondary star!

   off_14 = o_slot4 - o_slot1

   col = getcolor(/load)
endif

; ===========================================================================
   w1l = where( (wire2.hjd(0) lt t_alt), c1l) ; slot 0 contains  faint star
   w4l = where( (wire2.hjd(4) lt t_alt), c4l) ; slot 4 contains bright star
; ===========================================================================
   w1r = where( (wire2.hjd(0) ge t_alt), c1r) ; slot 0 contains bright star
   w4r = where( (wire2.hjd(4) ge t_alt), c4r) ; slot 4 contains  faint star
; ===========================================================================

;  plot,wire2(w1l).hjd(0)-t0,d_slot1(w1l)-o_slot1,xr=[-9,-2],ysty=3,psym=3,yr=[-1,2]
;  oplot,wire2(w4l).hjd(4)-t0,-2.5 * alog10(wire2(w4l).p(4,6) ) + 25.0 - o_slot1,psym=3,col=col.red
;  oplot,wire2(w4r).hjd(4)-t0,-2.5 * alog10(wire2(w4r).p(4,6) ) + 25.0 - o_slot1,psym=3,col=col.red
;  oplot,wire2(w1r).hjd(0)-t0,-2.5 * alog10(wire2(w1r).p(0,6) ) + 25.0 - o_slot1,psym=3

wire2a = -1

; ================================================
if c1l ne 0 then begin ; data needs to be swapped
; ================================================

; New structure ---> data needs to be swapped!
ndp = n_elements(wire2) & nslot = n_elements(wire2(0).x)
nap = n_elements(wire2(0).p(0,*))
wire2a = replicate({x:fltarr(nslot),y:fltarr(nslot),hjd:dblarr(nslot),$
                  co:fltarr(2,nslot),flux1:fltarr(nslot),flux2:fltarr(nslot),$
                  gc:fltarr(2,nslot),backgr:fltarr(nslot),backgr2:fltarr(nslot),$
                  p:fltarr(nslot,nap), a:bytarr(nslot,nap), fwhm:fltarr(nslot), $
                  col:intarr(nslot), row:intarr(nslot)}, ndp + 8000L) ; from wire_pos.pro

; Reset this structure:
wire2a.y = -9 & wire2a.x = -9 & wire2a.hjd = -9
wire2a.co(*,*) = -9 & wire2a.flux1 = -9  & wire2a.flux2 = -9 
wire2a.gc(*,*) = -9 & wire2a.backgr = -9 & wire2a.backgr2 = -9
wire2a.p(*,*)  = -9 & wire2a.fwhm = -9 


; Stars that are ok!
ook = [1,2,3]
wire2a(0:ndp-1).x(ook) = wire2(0:ndp-1).x(ook)
wire2a(0:ndp-1).y(ook) = wire2(0:ndp-1).y(ook)
wire2a(0:ndp-1).hjd(ook) = wire2(0:ndp-1).hjd(ook)
wire2a(0:ndp-1).flux1(ook) = wire2(0:ndp-1).flux1(ook)
wire2a(0:ndp-1).flux2(ook) = wire2(0:ndp-1).flux2(ook)
wire2a(0:ndp-1).backgr(ook) = wire2(0:ndp-1).backgr(ook)
wire2a(0:ndp-1).backgr2(ook) = wire2(0:ndp-1).backgr2(ook)
wire2a(0:ndp-1).fwhm(ook) = wire2(0:ndp-1).fwhm(ook)
wire2a(0:ndp-1).co(*,ook) = wire2(0:ndp-1).co(*,ook)
wire2a(0:ndp-1).gc(*,ook) = wire2(0:ndp-1).gc(*,ook)
wire2a(0:ndp-1).p(ook,*) = wire2(0:ndp-1).p(ook,*)
wire2a(0:ndp-1).a(ook,*) = wire2(0:ndp-1).a(ook,*)

; Swap slots 0 and 4
res = [0,4]

   wire2a(0:c4l-1).x(0)   = wire2(w4l).x(4)   &  wire2a(0:c1l-1).x(4)   = wire2(w1l).x(0)
   wire2a(0:c4l-1).y(0)   = wire2(w4l).y(4)   &  wire2a(0:c1l-1).y(4)   = wire2(w1l).y(0)
   wire2a(0:c4l-1).hjd(0) = wire2(w4l).hjd(4) &  wire2a(0:c1l-1).hjd(4) = wire2(w1l).hjd(0)
   wire2a(0:c4l-1).flux1(0) = wire2(w4l).flux1(4) &  wire2a(0:c1l-1).flux1(4) = wire2(w1l).flux1(0)
   wire2a(0:c4l-1).flux2(0) = wire2(w4l).flux2(4) &  wire2a(0:c1l-1).flux2(4) = wire2(w1l).flux2(0)
    wire2a(0:c4l-1).backgr(0) =  wire2(w4l).backgr(4) 
    wire2a(0:c1l-1).backgr(4) =  wire2(w1l).backgr(0)
   wire2a(0:c4l-1).backgr2(0) = wire2(w4l).backgr2(4) 
   wire2a(0:c1l-1).backgr2(4) = wire2(w1l).backgr2(0)
   wire2a(0:c4l-1).fwhm(0) = wire2(w4l).fwhm(4) &  wire2a(0:c1l-1).fwhm(4) = wire2(w1l).fwhm(0)
   wire2a(0:c4l-1).co(*,0) = wire2(w4l).co(*,4) &  wire2a(0:c1l-1).co(*,4) = wire2(w1l).co(*,0)
   wire2a(0:c4l-1).gc(*,0) = wire2(w4l).gc(*,4) &  wire2a(0:c1l-1).gc(*,4) = wire2(w1l).gc(*,0)
   wire2a(0:c4l-1).p(0,*) = wire2(w4l).p(4,*) &  wire2a(0:c1l-1).p(4,*) = wire2(w1l).p(0,*)
   wire2a(0:c4l-1).a(0,*) = wire2(w4l).a(4,*) &  wire2a(0:c1l-1).a(4,*) = wire2(w1l).a(0,*)

; Good data ... but wrong entries
if c4r ge 1 then begin
   wire2a(c1l:c1l+c4r-1).x(4)      = wire2(w4r).x(0) 
   wire2a(c1l:c1l+c4r-1).y(4)      = wire2(w4r).y(4) 
   wire2a(c1l:c1l+c4r-1).hjd(4)    = wire2(w4r).hjd(4) 
   wire2a(c1l:c1l+c4r-1).flux1(4)  = wire2(w4r).flux1(4) 
   wire2a(c1l:c1l+c4r-1).flux2(4)  = wire2(w4r).flux2(4) 
   wire2a(c1l:c1l+c4r-1).backgr(4) = wire2(w4r).backgr(4) 
   wire2a(c1l:c1l+c4r-1).backgr2(4)= wire2(w4r).backgr2(4) 
   wire2a(c1l:c1l+c4r-1).fwhm(4)   = wire2(w4r).fwhm(4) 
   wire2a(c1l:c1l+c4r-1).co(*,4)   = wire2(w4r).co(*,4) 
   wire2a(c1l:c1l+c4r-1).gc(*,4)   = wire2(w4r).gc(*,4) 
   wire2a(c1l:c1l+c4r-1).p(4,*)    = wire2(w4r).p(4,*) 
   wire2a(c1l:c1l+c4r-1).a(4,*)    = wire2(w4r).a(4,*) 
endif

if c1r ge 1 then begin
   wire2a(c4l:c4l+c1r-1).x(0)      = wire2(w1r).x(0) 
   wire2a(c4l:c4l+c1r-1).y(0)      = wire2(w1r).y(0)
   wire2a(c4l:c4l+c1r-1).hjd(0)    = wire2(w1r).hjd(0) 
   wire2a(c4l:c4l+c1r-1).flux1(0)  = wire2(w1r).flux1(0) 
   wire2a(c4l:c4l+c1r-1).flux2(0)  = wire2(w1r).flux2(0) 
   wire2a(c4l:c4l+c1r-1).backgr(0) = wire2(w1r).backgr(0) 
   wire2a(c4l:c4l+c1r-1).backgr2(0)= wire2(w1r).backgr2(0) 
   wire2a(c4l:c4l+c1r-1).fwhm(0)   = wire2(w1r).fwhm(0) 
   wire2a(c4l:c4l+c1r-1).co(*,0)   = wire2(w1r).co(*,0) 
   wire2a(c4l:c4l+c1r-1).gc(*,0)   = wire2(w1r).gc(*,0) 
   wire2a(c4l:c4l+c1r-1).p(0,*)    = wire2(w1r).p(0,*) 
   wire2a(c4l:c4l+c1r-1).a(0,*)    = wire2(w1r).a(0,*) 
endif


if debug eq 1 then begin

!P.multi=[0,1,2]

; Sanity checks:
;  plot,wire2a.hjd(0)-t0,-2.5*alog10(wire2a.p(0,6))+25.0-11.916,$
;   psym=3,ysty=3,yr=[-1,1]*0.05,xr=[-12,-6]
;  plot,wire2a.hjd(4)-t0,-2.5*alog10(wire2a.p(4,6))+25.0-13.115,$
;   psym=3,ysty=3,yr=[-1,1]*0.05,xr=[-10,2]
  
  plot,wire2.hjd(0)-t0,-2.5*alog10(wire2.p(0,6))+25.0-11.916,$
   psym=3,ysty=3,yr=[-1,1]*2.05,xr=[-12,-2],tit='White = Old Slot 0 -- Red = New slot 0'
  oplot,wire2a.hjd(0)-t0,-2.5*alog10(wire2a.p(0,6))+25.0-11.916,psym=3,col=col.red
  
  plot,wire2.hjd(4)-t0,-2.5*alog10(wire2.p(4,6))+25.0-13.115,$
   psym=3,ysty=3,yr=[-1,1]*2.05,xr=[-12,-2],tit='White = Old slot 4 -- Red = New slot 4'
  oplot,wire2a.hjd(4)-t0,-2.5*alog10(wire2a.p(4,6))+25.0-13.115,psym=3,col=col.red

!P.multi = 0

endif                    ; end of debug

wire2 = wire2a & wire2a = 'NA'

endif                           ; need to swap 0 and 4 ?

;   plot,wire2.hjd(0)-t0,d_slot1-o_slot1,xr=[-10,2],ysty=3,psym=3
;   oplot,wire2.hjd(4)-t0,-2.5 * alog10(wire2.p(4,6) ) + 25.0 - o_slot1,psym=3,col=col.red
;   oplot,wire2a.hjd(0)-t0,-2.5 * alog10(wire2a.p(0,6) ) + 25.0 - o_slot1,psym=3,col=col.red

;plot,wire2.hjd(0)-t0,-2.5*alog10(wire2.p(0,6))+25.-11.915,$
; psym=3,yr=[-1,1]*1.5,xr=[-10,2],xtit='!4D!3 HJD',ytit='!4D!3 mag',$
; title='WIRE -- Altair',xsty=1,ysty=1
;oplot,wire2a.hjd(0)-t0,-2.5*alog10(wire2a.p(0,6))+25.-11.915,$
; psym=3,col=col.red


END
