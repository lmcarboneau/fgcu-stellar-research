PRO wire_merge_ww3,ttref,nstar,dat3,x3,y3,gc3,fwhm3,back3,cnt3,abort_mode,wire3

np  = n_elements(ttref)

if nstar ge 2 then begin
  dat4  = dat3
  gc4   = gc3
  x4    = x3
  y4    = y3
  fwhm4 = fwhm3
  back4 = back3
endif else begin
    npx   = n_elements(dat3)
    dat4  = fltarr(1,npx) & dat4(0,*) = dat3
    gc4   = gc3
    x4    = fltarr(1,npx) & x4(0,*) = x3
    y4    = fltarr(1,npx) & y4(0,*) = y3
    fwhm4 = fltarr(1,npx) & fwhm4(0,*) = fwhm3
    back4 = fltarr(1,npx) & back4(0,*) = back3
endelse

nmax = n_elements(wire3.hjd)
abort_mode = 0B

if cnt3 + np ge nmax then begin
 print,' %%% Max array size reached in wire_merge_ww3.pro'
 abort_mode = 1B
 wire3 = wire3(0:cnt3-1) ; prepare for storing data
 return
endif

if np ge 1 then begin
   wire3(cnt3:cnt3+np-1).hjd  = reform(ttref)
   wire3(cnt3:cnt3+np-1).mag  = dat4
   wire3(cnt3:cnt3+np-1).gc   = gc4
   wire3(cnt3:cnt3+np-1).x    = x4
   wire3(cnt3:cnt3+np-1).y    = y4
   wire3(cnt3:cnt3+np-1).fwhm = fwhm4
   wire3(cnt3:cnt3+np-1).back = back4 ; backround used for decorrelation !!
endif

; wire3(cnt3:cnt3+np-1).angle = angle4 ; angle to center of CCD !!

cnt3 = cnt3 + np

END
