PRO wire_det_decor, wire3, wire4, posmap, map, err, t0, star, debug

!P.multi=0
 
; star = 4 ; the brightest star ... for which x,y varies! x,y == constant for altair!
col=getcolor(/load)

wire4 = wire3

fre1 = 15.009 ; wire orbit
ph = (wire4.hjd mod (1./fre1)) * fre1
m0 = median(wire4.mag(star))

wg = where( (ph ge 0.0 and ph lt 0.13) or (ph ge 0.81 and ph le 1.0), cg)

if debug eq 1 then begin
 plot,ph,wire4.mag(star)-m0,psym=3,yr=[-1,1]*0.02
 oplot,ph(wg),wire4(wg).mag(star)-m0,psym=3,col=col.red
endif

tlim_low = 40000.0
tlim_up  = 99999.0 ; 51485.  ; ---> data with large spread in x,y position!

; =========================================================================
  w = where( ((ph ge 0.0 and ph lt 0.13) or (ph ge 0.81 and ph le 1.0)) and $
            wire4.hjd gt tlim_low and wire4.hjd lt tlim_up and $
            wire4.gc(0,star) gt 1.5 and wire4.gc(1,star) gt 1.5 and $
            wire4.gc(0,star) lt 5.5 and wire4.gc(1,star) lt 5.5,c)
; =========================================================================

; =========================================================================
if c le 100 then begin
 print,' *** Warning: No or too few data point detected in wire_det_decor.pro !!'
 print,' ***          Aborting ... '
 goto,abort_decor
endif
; =========================================================================

; =========================================================================
mm = wire4(w).mag(star)  ; magnitude
noise = robust_sigma(mm) ; noise level
mlim = 20. * noise       ; extreme outlier limit
m_med = median(mm)
tt = wire4(w).hjd

ww = where( ((ph ge 0.0 and ph lt 0.13) or (ph ge 0.81 and ph le 1.0)) and $
            wire4.hjd gt tlim_low and wire4.hjd lt tlim_up and $
            abs(wire4.mag(star)-m_med) lt 20.*mlim and $
            abs(wire4.gc(0,star)-3.5) lt 1.0 and $
            abs(wire4.gc(1,star)-3.5) lt 1.0, c)
            

mm = wire4(ww).mag(star)  ; magnitude
noise = robust_sigma(mm) ; noise level
mlim = 20. * noise       ; extreme outlier limit
m_med = median(mm)
mm = mm - m_med
tt = wire4(ww).hjd
xx = wire4(ww).gc(0,star) ; x - position
yy = wire4(ww).gc(1,star) ; y - position

wire_known_freq,'/ai39/bruntt/wire/altair/altair_t0_51480.per',tt,t0,mm, dmag, 0
if star ne 0 then dmag(*) = 0.0

; =========================================================================
if debug eq 1 then begin

!P.multi=[0,2,3]
plot,wire4.gc(0,star)-3.5,wire4.mag(star)-m_med,$
 psym=3,ytit='!4D!3 mag',xtit='!4D!3 x',xr=[-1,1],yr=[-1,1]*0.02
oplot,wire4(ww).gc(0,star)-3.5,wire4(ww).mag(star)-m_med,$
 psym=3,col=col.red

; oplot,wire4.gc(0,4)-3.5,wire4.mag(4)-median(wire4.mag(4)),$
;  psym=3,col=col.red

plot,ph,wire4.gc(0,star)-3.5,psym=3,xtit='Phase',ytit='!4D!3 x',yr=[-1,1]
 oplot,ph(ww),wire4(ww).gc(0,star)-3.5,col=col.red,psym=3
plot,ph,wire4.gc(1,star)-3.5,psym=3,xtit='Phase',ytit='!4D!3 x',yr=[-1,1]
 oplot,ph(ww),wire4(ww).gc(1,star)-3.5,col=col.red,psym=3
plot,wire4.gc(0,star)-3.5,wire4.gc(1,star)-3.5,psym=3,$
 xtit='!4D!3 x',ytit='!4D!3 y',xr=[-1,1],yr=[-1,1]
 oplot,wire4(ww).gc(0,star)-3.5,wire4(ww).gc(1,star)-3.5,psym=3,col=col.red
plot,tt-t0,mm,psym=3,ysty=3,yr=[-1,1]*0.005,/nodata,$
 xr=[-9.0,-8.5]
 oplot,wire4.hjd-t0,wire4.mag(star)-m_med,psym=3,col=col.red
 oplot,tt-t0,mm,psym=3
 ; if star eq 0 then oplot,tt-t0,dmag,col=col.sky
!P.multi=0
print,' Next plot ... x = exit '  &  s  = get_kbrd(1)  &  if s eq 'x' then stop
; =========================================================================

;  plot,wire4(w).gc(0,star),wire4(w).mag(star)-mm,psym=3,yr=[-1,1]*0.05
;  plot,wire4(w).gc(1,star),wire4(w).mag(star)-mm,psym=3,yr=[-1,1]*0.05

; plot,wire4.gc(0,star)-3.5,wire4.gc(1,star)-3.5,psym=3,xr=[-1,1],yr=[-1,1]
; oplot,xx-3.5,yy-3.5,psym=3,col=col.red

endif ; debug slut

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

print,' %%% Determination of Correlation with x-y '  

if debug ge 1 then begin
 if debug eq 1 then begin 
  print,' Hit a key to see plot ! '
  s  = get_kbrd(1)  &  if s eq 'x' then stop
 endif
 plot,dx,dy,xr=[-1,1]*0.6,yr=[-1,1]*0.6,xsty=1,ysty=1,psym=3,$
  xtit='!4D!8x!3',ytit='!4D!8y!3'
endif

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
   err(i,j) = stdev(mm(wp)) / sqrt(cp-1.)
  endif

  if cp eq 1 then begin
   map(i,j) = mm(wp)
   err(i,j) = noise
  endif

if debug ge 1 then begin
  oplot, [ posmap(0,i,j) , posmap(1,i,j) ], [posmap(2,i,j), posmap(2,i,j)] ; lower horiz
  oplot, [ posmap(0,i,j) , posmap(1,i,j) ], [posmap(3,i,j), posmap(3,i,j)] ; upper horiz
  oplot, [ posmap(0,i,j) , posmap(0,i,j) ], [posmap(2,i,j), posmap(3,i,j)] ; left  vert
  oplot, [ posmap(1,i,j) , posmap(1,i,j) ], [posmap(2,i,j), posmap(3,i,j)] ; right vert
endif

;  xyouts,( posmap(0,i,j) + posmap(1,i,j) ) * 0.5, $
;         ( posmap(2,i,j) + posmap(3,i,j) ) * 0.5, $
;         /data,charsi=0.7,alignment=0.3, col=col.green, $
;         strcompress(string(cp),/remove_all)

 endfor
endfor
; =========================================================================



; =========================================================================
; Now: correct magnitudes for all stars!
; =========================================================================

print,' %%% Applying decorrelation to all stars ! '

nstar = n_elements(wire4(0).mag)
; =========================================================================
for star=0,nstar-1 do begin
; =========================================================================

  w  = where(wire4.hjd gt 0. and wire4.mag(star) gt 5.0 and $
             wire4.gc(0,star) gt 1.5 and wire4.gc(1,star) gt 1.5 and $
             wire4.gc(0,star) lt 5.5 and wire4.gc(1,star) lt 5.5,c)


  mm     = wire4(w).mag(star)  ; magnitude
  mm2    = mm
  mm2(*) = -99.9

  dx     = wire4(w).gc(0,star) -3.5 ; x - position
  dy     = wire4(w).gc(1,star) -3.5 ; y - position

  noise  = robust_sigma(mm) ; noise level
  mlim   = 20. * noise       ; extreme outlier limit
  m_med  = median(mm)
  mm     = mm - m_med
  tt     = wire4(w).hjd

; =========================================================================
  if c ge 10 then begin

     for i=0,nx-1 do begin
      for j=0,ny-1 do begin
     
       ; Find the data points that fit in this data point
       wp = where(dx ge posmap(0,i,j) and dx lt posmap(1,i,j) and $
                  dy ge posmap(2,i,j) and dy lt posmap(3,i,j) and $
                  abs(mm) lt mlim,cp)

; if cp gt 100. and map(i,j) ne 0.0 then stop
     
                                ; New magnitudes: the error on the
                                ; correction must be much smaller than the
                                ; measurement error on the data point!
       if cp ge 1 and (3. * err(i,j)) lt (noise) then $ 
         mm2(wp) = mm(wp) - map(i,j)
     
      endfor
     endfor

   mm2 = mm2 + m_med ; add the original offset in magnitude!

  endif
; =========================================================================

; =========================================================================
  wire4(w).mag(star) = mm2
; =========================================================================

; stop
   
endfor ; go to next star !!
; =========================================================================


; =========================================================================
if debug ge 1 then begin
tx0 = double( floor(min(tt)) )
tx1 = double( ceil(max(tt))  )
t0a = tx0 - tx0
t1a = tx1 - tx0

print,'Hit me...' & s = get_kbrd(1)
!P.multi=[0,1,2]
plot,tt-tx0,mm,psym=3,xr=[t0a,t1a],yr=[-1,1]*0.008,xsty=3,ysty=3,$
 xtit='HJD',ytit='!4D!3m',tit='!14Altair!3'
oplot,tt-tx0,mm2,psym=3,col=col.green

plot,dx,mm,psym=3,xr=[-1,1]*0.55,yr=[-1,1]*0.008,xsty=3,ysty=3,$
 xtit='!4D!8x!3',ytit='!4D!3m',tit='!14Altair!3'
oplot,dx,mm2,psym=3,col=col.green

!P.multi = 0
endif
; =========================================================================


; =========================================================================
if debug eq 1 then begin
 nc = 10. ; number of coutour levels
 mx = max(map) * 1000.
 mi = min(map) * 1000.
 ll = ( mi + (mx-mi) * findgen(nc) / (nc-1.) ) 
 lab = findgen(nc) & lab(*) = 1

 print,' Next: contour ... '  &  s  = get_kbrd(1)  &  if s eq 'x' then stop
 contour,map*1000.,levels=ll,c_labels = lab
endif
; =========================================================================

abort_decor:

END
