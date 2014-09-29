PRO wire_swap_all, xy, wire2, wire2a, ap_number, debug

 debug = 1
 col=getcolor(/load)
 colx = [col.green,col.yellow,col.red,col.sky,col.magenta,col.cyan]
 nx = n_elements(colx)

plotsym,0

np = n_elements(xy(0,*))    ; number of possible x,y positions on CCD
nstar = n_elements(wire2(0).x) ; number of stars 
nslot = nstar
nap = n_elements(wire2(0).p(0,*))
nnn = long(n_elements(wire2) * 6.0)

; Taken from wire_pos.pro:
wire2a = replicate({x:fltarr(np),y:fltarr(np),hjd:dblarr(np),$
                   co:fltarr(2,np),flux1:fltarr(np),flux2:fltarr(np),$
                   gc:fltarr(2,np),backgr:fltarr(np),backgr2:fltarr(np),$
                   p:fltarr(np,nap), a:bytarr(np,nap), fwhm:fltarr(np), $
                   col:intarr(np), row:intarr(np)}, nnn)

rlim  = 50.0 ; overall drift in pixels < rlim
rlim2 = 8.0 ; difference in distance to reference star < rlim2

if debug ge 2 then begin
 plot,wire2.x(0),wire2.y(0),psym=3,xr=[-50,550],yr=[-50,550],/nodata,xsty=1,ysty=1
 
 ; for i=0,np-1 do plots,xy(0,i), xy(1,i),psym=8,symsi=5
 for i=0,nstar-1 do oplot,wire2.x(i),wire2.y(i),psym=3,col=(colx(i mod nx) )
 for i=0,np-1 do tvellipse,rlim,rlim,xy(0,i), xy(1,i), /data

endif

rlim = rlim^2.0
cnt_max = 0L

; Pick the main target (== w_min)
centr = [256., 256.] ; center of ccd 
dist_all = (centr(0) - xy(0,*))^2. + (centr(1) - xy(1,*))^2.
w_min = where(dist_all eq min(dist_all),c_min)
w_min = w_min(0)

;a2 = findgen(nstar)
;a3 = a2
;a3(0) = w_min
;a3(w_min) = a2(0)



for i=0,np-1 do begin ; for each possible x,y position, 0--5
cnt = 0L
 
 for s=0,nstar-1 do begin ; 0--4

; Distance from reference x,y position:
     dist = ( wire2.x(s)-xy(0,i) )^2.0 + ( wire2.y(s)-xy(1,i) )^2.0 

; The old limit:
;    w = where( dist lt rlim, c)

; Distance to reference star (near center of ccd):
; Compare the point-to-point difference and the reference positions
; (a very strict limit!)
 dist_to_center_of_ccd = (wire2.x(s) - xy(0,w_min))^2. + (wire2.y(s) - xy(1,w_min))^2
 dist_to_center_of_ccd = sqrt(dist_to_center_of_ccd)
 dist_ref = (xy(0,i) - xy(0,w_min))^2. + (xy(1,i) - xy(1,w_min))^2.
 dist_ref = sqrt(dist_ref)

; A new and more strict limit:
     w = where( (dist lt rlim) and $
                abs(dist_ref - dist_to_center_of_ccd) lt rlim2, c)


; dist_to_center_of_ccd = (wire2.x(s) - 255.5)^2. + (wire2.y(s) - 255.5)^2
; dist_to_center_of_ccd = sqrt(dist_to_center_of_ccd)
; dist_ref = (xy(0,i) - 255.5)^2. + (xy(1,i) - 255.5)^2.
; dist_ref = sqrt(dist_ref)

     if c ge 1 then begin
      wire2a(cnt:cnt+c-1).x(i)     = wire2(w).x(s)
      wire2a(cnt:cnt+c-1).y(i)     = wire2(w).y(s)
      wire2a(cnt:cnt+c-1).hjd(i)   = wire2(w).hjd(s)

      wire2a(cnt:cnt+c-1).co(*,i)  = wire2(w).co(*,s)
      wire2a(cnt:cnt+c-1).gc(*,i)  = wire2(w).gc(*,s)

      wire2a(cnt:cnt+c-1).flux1(i) = wire2(w).flux1(s)
      wire2a(cnt:cnt+c-1).flux2(i) = wire2(w).flux2(s)

      wire2a(cnt:cnt+c-1).backgr(i)  = wire2(w).backgr(s)
      wire2a(cnt:cnt+c-1).backgr2(i) = wire2(w).backgr2(s)

      wire2a(cnt:cnt+c-1).fwhm(i) = wire2(w).fwhm(s)

      wire2a(cnt:cnt+c-1).p(i,*)   = wire2(w).p(s,*)
      wire2a(cnt:cnt+c-1).a(i,*)   = wire2(w).a(s,*)

      wire2a(cnt:cnt+c-1).row(i) = wire2(w).row(s)
      wire2a(cnt:cnt+c-1).col(i) = wire2(w).col(s)

      cnt = cnt + c
     endif
 endfor

 if cnt gt cnt_max then cnt_max = cnt

endfor ; next possible x,y position

wire2a = wire2a(0:cnt_max) ; remove unused entries ...

for i=0,np-1 do begin
    mm =   -2.5*alog10(wire2a.p(i,ap_number(i))) + 25.0  
    wd = where(mm lt 5. or mm gt 25.,cdd)
    if cdd ge 1 then wire2a(wd).hjd(i) = -10.0 ; bad data !!
endfor


if debug ge 1 then begin

   plot,[0,1],/nodata,xr=[-5,5],yr=[10,20],tit='After Swapping of LCs'
   
   for i=0,np-1 do begin
    dist = (wire2a.x(i)-xy(0,i))^2. + (wire2a.y(i)-xy(1,i))^2.
    w = where(dist lt rlim, cx)  &  help,cx

    if cx ge 10 then begin
    mm = -2.5*alog10(wire2a.p(i,ap_number(i))) + 25.0 

    t0 = median(wire2a(w).hjd(i))   

    oplot,wire2a.hjd(i)-t0 + (-4. + i * 1.0) ,mm,psym=3,col=(colx(i mod nx) )
 
    wd = where(mm gt 10. and mm lt 25.,cdd)

       if cdd ge 10 then begin
           med = median(mm(wd))
       
           xyouts,2.5,med,'Star '+string(i,format='(I2)'), $
            charsi=1.3,col=(colx(i mod nx) )
       
           print,'Star '+string(i,format='(I2)'), med
       endif else print,'Bad data for star: ',i
    endif

   endfor
    
endif ; end of debug plot

end

