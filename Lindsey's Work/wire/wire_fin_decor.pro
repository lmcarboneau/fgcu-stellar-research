PRO  wire_fin_decor, flux, gc, flux2, $
                  fluxall, gcall, flux2all, $
                  posmap, map, err, finegrid_size, debug, status

debug = 0

if debug ge 1 then begin
 !P.multi=0 
 col = getcolor(/load)
endif

flux2 = flux       ; new fluxes
flux2all = fluxall ; new fluxes

c1 = n_elements(flux) & c2 = n_elements(gc(0,*)) & if c1 ne c2 then stop

status = 1 ; all is ok ...

; =========================================================================
if c1 le 500 then begin
 print,' *** Warning: No or too few data point detected in wire_det_decor.pro !!'
 print,' ***          Aborting ... '
 status = -1
 goto,abort_decor
endif
; =========================================================================

; =========================================================================
noise = robust_sigma(flux) ; noise level
mlim = 20. * noise       ; extreme outlier limit
mmed = median(flux)

    x = gc(0,*) & offx = floor(median(x)) 
    x1 = min(x-offx) & x2 = max(x-offx)
    y = gc(1,*) & offy = floor(median(y)) 
    y1 = min(y-offy) & y2 = max(y-offy)

ww = where( abs(flux - mmed) lt mlim,c)

;ww = where( abs(flux - mmed) lt mlim and $
;            abs(gc(0,*)-3.5) lt 1.0 and $
;            abs(gc(1,*)-3.5) lt 1.0,c)
          
mm = flux(ww)
noise = robust_sigma(mm) ; noise level
mlim = 20. * noise       ; extreme outlier limit
mmed = median(mm)
mm = mm - mmed
xx = x(ww) - offx ; gc(0,ww) ; x-pos
yy = y(ww) - offy ; gc(1,ww) ; y-pos

;;; flux2a = flux2(ww)

; =========================================================================
if debug eq 1 then begin

;!P.multi=[0,2,1]
;ss = robust_sigma(flux-mmed)
;plot,x-offx,flux-mmed, $
; psym=3,ytit='!4D!3 mag',xtit='!4D!3 x',xr=[x1,x2],yr=[-1,1]*ss * 6.
;  oplot,xx,flux(ww)-mmed,psym=3,col=col.red

;plot,x-offx,y-offy,psym=3,$
; xtit='!4D!3 x',ytit='!4D!3 y',xr=[x1,x2],yr=[y1,y2]
;  oplot,xx,yy,psym=3,col=col.red
;!P.multi=0
; print,' Hit me for next plot ... x = exit '  &  s  = get_kbrd(1)  &  if s eq 'x' then stop

endif ; debug slut


dx = xx
dy = yy

; Make a x,y grid:
finegrid = finegrid_size ; r wire.
rangx = (x2 - x1) & if rangx lt 1 then rangx = 1.
rangy = (y2 - y1) & if rangy lt 1 then rangy = 1.
nx = ceil(finegrid * rangx)
ny = ceil(finegrid * rangy)

start_x = min(dx)  &  end_x   = max(dx)
start_y = min(dy)  &  end_y   = max(dy)

step_x  = (end_x - start_x) / nx
step_y  = (end_y - start_y) / ny
bordx   = step_x * 0.2
bordy   = step_y * 0.2 ; slight overlap with adj. grid points!

map = fltarr(nx,ny) ; delta mag at each x,y grid point
err = map           ; error in mag at each x,y, grid point
posmap = fltarr(4,nx,ny) ; x pos start, x pos end, y pos start, y pos end

print,' %%% X-Y Decorrelation'
print,'     step x = ', step_x, ' step y = ', step_y
print,'         nx = ', nx,     ' ny = ', ny

; =========================================================================

print,' %%% Determination of Correlation with x-y '  

if debug ge 2 and debug lt 50 then begin

; if debug eq 1 then begin 
;  print,' Hit a key to see plot ! '
;  s  = get_kbrd(1)  &  if s eq 'x' then stop
; endif
; plot,dx,dy,xr=[-1,1]*0.6,yr=[-1,1]*0.6,xsty=1,ysty=1,psym=3,$
;  xtit='!4D!8x!3',ytit='!4D!8y!3'

endif

ctot = nx * ny
cn9 = 0L
print,' Counter must reach ', ctot

; =========================================================================
for i=0,nx-1 do begin
 for j=0,ny-1 do begin

  posmap(0,i,j) = start_x + step_x *  i    ; start x for grid point
  posmap(1,i,j) = start_x + step_x * (i+1) ; end x   for grid point
  posmap(2,i,j) = start_y + step_y *  j    ; start y for grid point
  posmap(3,i,j) = start_y + step_y * (j+1) ; end y   for grid point

  ; Find the data points that fit in this data point
  wp = where(dx ge (posmap(0,i,j)-bordx) and dx lt (posmap(1,i,j)+bordx) and $
             dy ge (posmap(2,i,j)-bordy) and dy lt (posmap(3,i,j)+bordy) and $
             abs(mm) lt mlim,cp)

  if cp ge 5 then begin
   resistant_mean,mm(wp),3,me,sd,nr
   map(i,j) = me
   err(i,j) = sd
  endif

  if cp le 4 and cp gt 2 then begin   
   map(i,j) = avg(mm(wp))
   err(i,j) = stdev(mm(wp)) / sqrt(cp-1.)
  endif

  if cp eq 1 then begin
   map(i,j) = mm(wp)
   err(i,j) = noise
  endif

; New magnitudes
 ;;; if cp ge 1 then flux2a(wp) = flux2a(wp) + map(i,j)

;if debug ge 2 and debug lt 50 then begin
;  oplot, [ posmap(0,i,j) , posmap(1,i,j) ], [posmap(2,i,j), posmap(2,i,j)] ; lower horiz
;  oplot, [ posmap(0,i,j) , posmap(1,i,j) ], [posmap(3,i,j), posmap(3,i,j)] ; upper horiz
;  oplot, [ posmap(0,i,j) , posmap(0,i,j) ], [posmap(2,i,j), posmap(3,i,j)] ; left  vert
;  oplot, [ posmap(1,i,j) , posmap(1,i,j) ], [posmap(2,i,j), posmap(3,i,j)] ; right vert

;  xyouts,( posmap(0,i,j) + posmap(1,i,j) ) * 0.5, $
;         ( posmap(2,i,j) + posmap(3,i,j) ) * 0.5, $
;         /data,charsi=0.7,alignment=0.3, col=col.green, $
;         strcompress(string(cp),/remove_all)
;endif

cn9 = cn9 + 1
if cn9 mod 1e4 eq 0 then print, cn9, format='(I8,$)'

 endfor
endfor
; =========================================================================

; Insert corrected magnitudes!
; flux2(ww) = flux2a

; =========================================================================
;                           Apply decorrelation:
; =========================================================================
;dxa = gc(0,*)
;dya = gc(1,*)
;dxaall = gcall(0,*)
;dyaall = gcall(1,*)

    dxa = reform(gc(0,*)) - offx
    dya = reform(gc(1,*)) - offy
    dxaall = reform(gcall(0,*)) - offx
    dyaall = reform(gcall(1,*)) - offy

; Apply decorrelation:
for i=0,nx-1 do begin
 for j=0,ny-1 do begin

 ; Find the data points that fit in this data point
  wp = where(dxa ge posmap(0,i,j) and dxa lt posmap(1,i,j) and $
             dya ge posmap(2,i,j) and dya lt posmap(3,i,j) and $
             abs(flux) lt mlim,cp)
  if cp ge 1 then flux2(wp) = flux2(wp) - map(i,j)

 ; Find the data points that fit in this data point
  wp = where(dxaall ge posmap(0,i,j) and dxaall lt posmap(1,i,j) and $
             dyaall ge posmap(2,i,j) and dyaall lt posmap(3,i,j) and $
             abs(fluxall) lt mlim,cp)
  if cp ge 1 then flux2all(wp) = flux2all(wp) - map(i,j)
  
 endfor
endfor
; =========================================================================






; =========================================================================
if debug eq 1 then begin

plot,gc(0,*)-offx,flux,psym=3,xr=[x1,x2],yr=[-1,1]*ss*6.,xsty=3,ysty=3,$
 xtit='!4D!8x!3',ytit='!4D!3m',tit='!14Altair!3'
oplot,gc(0,*)-offx,flux2,psym=3,col=col.green

!P.multi = 0
endif
; =========================================================================


; =========================================================================
if debug eq 1 then begin
 nc = 8. ; number of coutour levels
 mx = max(map) * 1000.
 mi = min(map) * 1000.
 ll = ( mi + (mx-mi) * findgen(nc) / (nc-1.) ) 
 lab = findgen(nc) & lab(*) = 1

 print,' Next: contour ... '  &  s  = get_kbrd(1)  &  if s eq 'x' then stop
 contour,map*1000.,levels=ll,c_labels = lab
endif
; =========================================================================


if debug eq 99 then begin

map2 = map
w = where(abs(map) lt err,c)
if c ge 1 then map2(w) = 0.0

window,1,title='XY Decorrelation Map'

 nc = 5. ; number of coutour levels
 mx = max(map) * 1000.
 mi = min(map) * 1000.
 ll = ( mi + (mx-mi) * findgen(nc) / (nc-1.) ) 
 lab = findgen(nc) & lab(*) = 1

 contour,map*1000.,levels=ll,c_labels = lab
print,'Hit any key to go on ... '  &  s = get_kbrd(1)
;surface,map,az=45
;print,'Hit any key to go on ... '  &  s = get_kbrd(1)

ang =45
ang2=45
mapp = map

again:

shade_surf,mapp,ax=ang,az=ang2

print,' Use q-w  OR a-s to rotate the plot (x = exit) '
print,' m = map ... M = map where large err. on map are rejected'
print,'Hit any key to go on ... '  &  s = get_kbrd(1)

if s eq 'a' then ang = ang - 10   & if s eq 's' then ang = ang + 10
if ang le 20 then ang = 20          & if ang ge 90 then ang = 90
if s eq 'q' then ang2 = ang2 - 5 & if s eq 'w' then ang2 = ang2 + 5
if ang2 le 0 then ang2 = 0        & if ang2 ge 180 then ang2 = 180
if s eq 'm' then mapp = map
if s eq 'M' then mapp = map2 


if s ne 'x' then goto,again


wdelete,1


endif


abort_decor:

END
