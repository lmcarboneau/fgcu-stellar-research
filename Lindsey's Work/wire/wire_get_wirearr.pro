PRO wire_get_wirearr, wire, nslot, nap, ndp

wire = replicate({x:fltarr(nslot),y:fltarr(nslot),hjd:dblarr(nslot),$
                   co:fltarr(2,nslot),flux1:fltarr(nslot),flux2:fltarr(nslot),$
                   gc:fltarr(2,nslot),backgr:fltarr(nslot),backgr2:fltarr(nslot),$
                   p:fltarr(nslot,nap), a:bytarr(nslot,nap), fwhm:fltarr(nslot), $
                   col:intarr(nslot), row:intarr(nslot)}, ndp)

wire.hjd(*)  = 1e9
wire.fwhm(*) = -1.
wire.p(*,*)  = -1.
wire.backgr2(*) = -32.

end
