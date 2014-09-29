; Program will examine how x,y offset changes the brightness of the star!
; (c) September 2003 -- Hans Bruntt

star = 0

; =========================================================================
; restore,"/ai39/bruntt/wire/altair/altair_merged2.idl"
if n_elements(wire4) eq 0 then $
 restore,'/ai39/bruntt/wire/altair/altair_merged_allslots_swap_x.idl'

;;; restore,'/ai39/bruntt/wire/altair/altair_merged_allslots.idl"
if n_elements(col) eq 0 then col = getcolor(/load)
; =========================================================================

; plot,wire4(w).hjd-51460.,wire4(w).gc(0,star),psym=3,ysty=3,$
;  xr=[8,32],xsty=3,yr=[2.9,4.1],xtit='HJD',ytit='8x!3 position',tit='!14Altair!3'
; oplot,wire4(w2).hjd-51460.,wire4(w2).gc(0,star),psym=3,col=col.red

; =========================================================================
fre1 = 15.009
ph = (wire4.hjd mod (1./fre1)) * fre1
m0 = median(wire4.mag(star))
plot,ph,wire4.mag(star)-m0,psym=3,yr=[-1,1]*0.02
wg = where( (ph ge 0.0 and ph lt 0.13) or (ph ge 0.81 and ph le 1.0), cg)
oplot,ph(wg),wire4(wg).mag(star)-m0,psym=3,col=col.red

  w = where(wire4.hjd gt 51475.)
 w2 = where(wire4.hjd lt 51475.)

tlim_low = 40000.0
tlim_up  = 51475.

  w = where( ((ph ge 0.0 and ph lt 0.13) or (ph ge 0.81 and ph le 1.0)) and $
            wire4.hjd gt tlim_low and wire4.hjd lt tlim_up and $
            wire4.gc(0,star) gt 1.5 and wire4.gc(1,star) gt 1.5 and $
            wire4.gc(0,star) lt 5.5 and wire4.gc(1,star) lt 5.5,c)

; =========================================================================
mm = wire4(w).mag(star)  ; magnitude
noise = robust_sigma(mm) ; noise level
mlim = 20. * noise       ; extreme outlier limit
m_med = median(mm)
tt = wire4(w).hjd

ww = where( ((ph ge 0.0 and ph lt 0.13) or (ph ge 0.81 and ph le 1.0)) and $
            wire4.hjd gt tlim_low and wire4.hjd lt tlim_up and $
            abs(wire4.mag(star)-m_med) lt mlim and $
            wire4.gc(0,star) gt 1.5 and wire4.gc(1,star) gt 1.5 and $
            wire4.gc(0,star) lt 5.5 and wire4.gc(1,star) lt 5.5,c)

w = w(ww)
mm = wire4(w).mag(star)  ; magnitude
noise = robust_sigma(mm) ; noise level
mlim = 20. * noise       ; extreme outlier limit
m_med = median(mm)
mm = mm - m_med
tt = wire4(w).hjd

plot,tt,mm,psym=3,ysty=3,yr=[-1,1]*0.001


; =========================================================================

;  plot,wire4(w).gc(0,star),wire4(w).mag(star)-mm,psym=3,yr=[-1,1]*0.05
;  plot,wire4(w).gc(1,star),wire4(w).mag(star)-mm,psym=3,yr=[-1,1]*0.05

xx = wire4(w).gc(0,star) ; x - position
yy = wire4(w).gc(1,star) ; y - position

tt = wire4(w).hjd

dx = xx - 3.5
dy = yy - 3.5

nx = 25
ny = nx

start_x = min(dx)
end_x   = max(dx)
start_y = min(dy)
end_y   = max(dy)

step_x  = (end_x - start_x) / nx
step_y  = (end_y - start_y) / ny

map = fltarr(nx,ny) ; delta mag at each x,y grid point
err = map           ; error in mag at each x,y, grid point
posmap = fltarr(4,nx,ny) ; x pos start, x pos end, y pos start, y pos end
; =========================================================================

plot,dx,dy,xr=[-1,1]*0.4,yr=[-1,1]*0.4,xsty=1,ysty=1,psym=3,$
 xtit='!4D!8x!3',ytit='!4D!8y!3'

; =========================================================================
for i=0,nx-1 do begin
 for j=0,ny-1 do begin

  posmap(0,i,j) = start_x + step_x *  i    ; start x for grid point
  posmap(1,i,j) = start_x + step_x * (i+1) ; end x   for grid point
  posmap(2,i,j) = start_y + step_y *  j    ; start y for grid point
  posmap(3,i,j) = start_y + step_y * (j+1) ; end y   for grid point

  ; Find the data points that fit in this data point
  wp = where(dx ge posmap(0,i,j) and dx lt posmap(1,i,j) and $
             dy ge posmap(2,i,j) and dy lt posmap(3,i,j) and $
             abs(mm) lt mlim,cp)

  if cp ge 5 then begin
   resistant_mean,mm(wp),3,me,sd,nr
   map(i,j) = me
   err(i,j) = sd
  endif

  if cp le 4 and cp gt 2 then begin   
   map(i,j) = avg(mm(wp))
   err(i,j) = stdev(mm(wp))
  endif

  if cp eq 1 then begin
   map(i,j) = mm(wp)
   err(i,j) = noise
  endif

  oplot, [ posmap(0,i,j) , posmap(1,i,j) ], [posmap(2,i,j), posmap(2,i,j)] ; lower horiz
  oplot, [ posmap(0,i,j) , posmap(1,i,j) ], [posmap(3,i,j), posmap(3,i,j)] ; upper horiz
  oplot, [ posmap(0,i,j) , posmap(0,i,j) ], [posmap(2,i,j), posmap(3,i,j)] ; left  vert
  oplot, [ posmap(1,i,j) , posmap(1,i,j) ], [posmap(2,i,j), posmap(3,i,j)] ; right vert

  xyouts,( posmap(0,i,j) + posmap(1,i,j) ) * 0.5, $
         ( posmap(2,i,j) + posmap(3,i,j) ) * 0.5, $
         /data,charsi=0.7,alignment=0.3, col=col.green, $
         strcompress(string(cp),/remove_all)

 endfor
endfor
; =========================================================================

; surface,map,charsi=2.0,zsty=3

; =========================================================================
; Now: correct magnitudes
; =========================================================================

mm2 = mm
mm2(*) = -99.9

; =========================================================================
for i=0,nx-1 do begin
 for j=0,ny-1 do begin

  ; Find the data points that fit in this data point
  wp = where(dx ge posmap(0,i,j) and dx lt posmap(1,i,j) and $
             dy ge posmap(2,i,j) and dy lt posmap(3,i,j) and $
             abs(mm) lt mlim,cp)

  ; New magnitudes:
  if cp ge 1 then $ 
   mm2(wp) = mm(wp) - map(i,j)

 endfor
endfor
; =========================================================================

print,'Hit me...' & s = get_kbrd(1)

!P.multi=[0,1,2]

t0 = 51480.0D ; double( floor(min(tt)) )
t1 = double( ceil(max(tt))  )
t0a = t0 - t0
t1a = t1 - t0

plot,tt-t0,mm,psym=3,xr=[t0a,t1a],yr=[-1,1]*0.008,xsty=3,ysty=3,$
 xtit='HJD',ytit='!4D!3m',tit='!14Altair!3'
oplot,tt-t0,mm2,psym=3,col=col.green

plot,dx,mm,psym=3,xr=[-1,1]*0.55,yr=[-1,1]*0.008,xsty=3,ysty=3,$
 xtit='!4D!8x!3',ytit='!4D!3m',tit='!14Altair!3'
oplot,dx,mm2,psym=3,col=col.green

!P.multi = 0

print,'Hit me...' & s = get_kbrd(1)

nc = 10. ; number of coutour levels
mx = max(map) * 1000.
mi = min(map) * 1000.
ll = ( mi + (mx-mi) * findgen(nc) / (nc-1.) ) 
lab = findgen(nc) & lab(*) = 1

contour,map*1000.,levels=ll,c_labels = lab

END
