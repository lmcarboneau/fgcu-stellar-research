; Find the change in rotation angle for WIRE lc's with multiple
; objects

!P.multi=0

if n_elements(file) eq 0 then begin
 file = "/data1/bruntt/wire/AlphaUMi/alphaCen_2004_allslots_31.idl"
 restore,file  &  target = "alphaCen"
endif

wireult(*).angle(*) = -9999.

nstar = n_elements(wireult(0).x)
np = n_elements(wireult)

ang = replicate( {angle:fltarr(nstar), x:fltarr(nstar), y:fltarr(nstar)}, np)
mag = fltarr(2,nstar)
mag(*,*) = -9.
pos = fltarr(3,nstar)
pos(*,*) = -9.

wall = fltarr(nstar,np)
wall(*,*) = -1
nall = fltarr(nstar)

for star=0,nstar-1 do begin

; Valid data points:
 w = where(wireult.back(star) ne 0. and $
           wireult.gc(0,star) gt 2.5 and wireult.gc(1,star) gt 2.5 and $
           wireult.gc(0,star) lt 4.5 and wireult.gc(1,star) lt 4.5 and $
            wireult.mag(star) gt 5.  and  wireult.mag(star) lt 25.,c)

 if c ge 100 then begin
  dat = wireult(w).mag(star)
  resistant_mean,dat,3.,me,sd,nr
  rr = robust_sigma(dat)
  mag(0,star) = me
  mag(1,star) = rr

  pos(0,star) = median(wireult(w).x(star))
  pos(1,star) = median(wireult(w).y(star))
  pos(2,star) = sqrt( (pos(0,star)^2.) + (pos(1,star)^2.) )

  wall(star,0:c-1) = w
  nall(star) = c

 endif

endfor

plot,[0,1],/nodata,xsty=1,ysty=1,xr=[0,511],yr=[0,511]


plot,[0,1],/nodata,xsty=1,ysty=1,xr=[270,400],yr=[70,150]
wg = where(mag(0,*) gt 2,cg)
for i=0,cg-1 do $
 oplot,[pos(0,0),pos(0,wg(i))],[pos(1,0),pos(1,wg(i))],thick=1,line=2

for i=0,cg-1 do $
 if nall(wg(i)) gt 100 then $
 oplot,wireult(wall(wg(i),0:nall(wg(i)) -1)).x(wg(i)),$
       wireult(wall(wg(i),0:nall(wg(i)) -1)).y(wg(i)),$
       psym=3

; Find the "extreme" positions in x and y for the each star

t0 = double(round(median(wireult(wall(0,0:nall(0) -1)).hjd)))

dt = 0.5

angall = fltarr(nstar,5,1000)
cnt = 0
nang = fltarr(nstar)

for i=0,cg-1 do begin

x = wireult(wall(wg(i),0:nall(wg(i)) -1)).x(wg(i))
y = wireult(wall(wg(i),0:nall(wg(i)) -1)).y(wg(i))
t = wireult(wall(wg(i),0:nall(wg(i)) -1)).hjd - t0
m = wireult(wall(wg(i),0:nall(wg(i)) -1)).mag(wg(i))

tmin = min(t)
tmax = max(t)
tt1 = tmin
tt2 = tt1 + dt

while tt1 lt tmax do begin

 wt = where(t ge tt1 and t le tt2,ct)
 if ct ge 100 then begin

  xx = median(x(wt))
  yy = median(y(wt))
  mm = median(m(wt))
  
  angall(wg(i),0,cnt) = (tt2 + tt1) * 0.5
  angall(wg(i),1,cnt) = xx
  angall(wg(i),2,cnt) = yy
  angall(wg(i),3,cnt) = mm
 
  cnt = cnt + 1

 endif

  tt1 = tt1 + dt
  tt2 = tt2 + dt

endwhile

 nang(wg(i)) = cnt
 cnt = 0



endfor

col=getcolor(/load)

;outps = '~/wire/wire_eps/wire_position.ps'
;a4,x=18,y=16,name=outps

for i=1,cg-1 do begin

 plot,angall(wg(i),1,0:nang(wg(i))-1),angall(wg(i),2,0:nang(wg(i))-1),$
  psym=1,ysty=3,/nodata,xthick=2,ythick=2,charsi=1.5,charthick=2
 oplot,wireult(wall(wg(i),0:nall(wg(i)) -1)).x(wg(i)),$
       wireult(wall(wg(i),0:nall(wg(i)) -1)).y(wg(i)),$
       psym=1,col=col.red,symsi=.2

 plotsym,0
 oplot,angall(wg(i),1,0:nang(wg(i))-1),angall(wg(i),2,0:nang(wg(i))-1),psym=8,symsi=2

 xval = angall(wg(i),1,0:nang(wg(i))-1)
 yval = angall(wg(i),2,0:nang(wg(i))-1)
 time = angall(wg(i),0,0:nang(wg(i))-1)

 s = sort(time) & xval = xval(s) & yval = yval(s) & time = time(s)

 ctime = n_elements(time)
 djump = (time(1:ctime-1) - time(0:ctime-2))
 wj = where(djump gt 45.,cj)
 if cj ne 0 then begin
   print,''
   print,' *** Program can only handle one situation: '
   print,' *** Continuous data ...
   print,''
   print,' *** If you have more than two chunks ... you need to split'
   print,' *** data in the chunks. Otherwise you will have problems in
   print,' *** wirep.pro when decorrelating with the angle !!'
   print,''
   stop
 endif

 ;nojump=0B
 ;if cj eq 0 then begin 
 ;  cj = 1
 ;  nojump = 1B
 ;endif
 ;if cj ge 2 then stop

; j = 0
; while j le 1 do begin

; if nojump eq 0 then begin ; THERE IS A HUGE JUMP IN TIME!!
; if j eq 0 then begin
;   d1 = 0L
;   d2 = wj(0)-1
; endif else begin
;   d1 = wj(0)
;   d2 = ctime-1
;   
; endelse
; endif

; if nojump eq 1 then begin
   d1 = 0L
   d2 = ctime-1L
;   j = 2 ; exit while loop
; endif

 plot,time(d1:d2),xval(d1:d2),psym=1,ysty=3

 ppx = robust_poly_fit(time(d1:d2),xval(d1:d2),2,myfitx,sig)
 oplot,time(d1:d2),myfitx,line=2,thick=4

 plot,time(d1:d2),yval(d1:d2),psym=1,ysty=3

 ppy = robust_poly_fit(time(d1:d2),yval(d1:d2),2,myfity,sig)
 oplot,time(d1:d2),myfity,line=2,thick=4

 plot,myfitx,myfity,ysty=3
 oplot,xval(d1:d2),yval(d1:d2),psym=3

 cenx = median(angall(0,1,0:nang(0)-1))
 ceny = median(angall(0,2,0:nang(0)-1))

 myfitx = myfitx - cenx ; offset to central star!!
 myfity = myfity - ceny

 ang  = atan(myfity  / myfitx)

; Make sure the angle is right ...
 w1 = where(myfitx gt 0. and myfity gt 0.,c1)
  if c1 ge 1 then ang(w1) = abs(ang)
 w2 = where(myfitx gt 0. and myfity le 0.,c2)
  if c2 ge 1 then ang(w2) = ang + 1.5 * !PI
 w3 = where(myfitx le 0. and myfity le 0.,c3)
  if c3 ge 1 then ang(w3) = ang + !PI
 w4 = where(myfitx le 0. and myfity gt 0.,c4)
  if c4 ge 1 then ang(w4) = ang + !PI

 mit = min(time(d1:d2))
 mxt = max(time(d1:d2))
 wtt = where(wireult.hjd-t0 ge mit and wireult.hjd-t0 le mxt and $
             wireult.back(wg(i)) ne 0. and $
             wireult.gc(0,wg(i)) gt 1.5 and wireult.gc(1,wg(i)) gt 1.5 and $
             wireult.gc(0,wg(i)) lt 5.5 and wireult.gc(1,wg(i)) lt 5.5 and $
             wireult.mag(wg(i))  gt 5.  and wireult.mag(wg(i))  lt 25.,ctt)

 wireult(wtt).angle(wg(i)) = interpol(ang, time(d1:d2), wireult(wtt).hjd-t0) * 180. / !PI
 plot,wireult(wtt).hjd-t0,wireult(wtt).angle(wg(i)) ,psym=3,ysty=3

 ; print,' %%% Next large jump in time .. > 45 days ... Hit any key!'
 ; s = get_kbrd(1)

; j = j + 1 

; endwhile ; next data jump


;device,/close
;set_plot,'x'
   
print,' Hit'
s = get_kbrd(1)


endfor ; next secondary target



; Finally ... determine the angle swept pr. time unit:

!P.multi=[0,2,2]

for i=1,cg-1 do begin
 plot,angall(wg(i),0,0:nang(wg(i))-1),angall(wg(i),1,0:nang(wg(i))-1),psym=1,ysty=3,xtit='t',ytit='x'
  if i mod 4 eq 0 then begin
     print,' Hit any key for next plot window ... '
     s = get_kbrd(1)
  endif
endfor
  

dtt = fltarr(2, cg-1) ; change in angle and change in angle pr. day

; Go through secondary targets:
for i=1,cg-1 do begin

w2 = where(wireult.angle(wg(i)) gt -400. and $
           wireult.mag(wg(i)) gt 5. and $
           wireult.mag(wg(i)) lt 25.,c2)

dtheta =  wireult.angle(wg(i)) - $
          median(wireult(w2).angle(wg(i)))

delta_time = max(wireult(w2).hjd) - min(wireult(w2).hjd)

plot, wireult(w2).hjd-t0, dtheta(w2), yr=[-1,1] * 10., psym=3
dtt(0,i-1) = max(dtheta(w2)) - min(dtheta(w2))
dtt(1,i-1) = dtt(0,i-1) / delta_time

xyouts,0,0,string(dtt(0,i-1),format='(F5.2)')

endfor

print,dtt

; med_angle_pr_time = median(dtt(1,*))
resistant_mean,dtt(1,*),3,me,sd,nr
med_angle_pr_time = me

print,' %%% Aplied mean angle (degree) pr. day: ',med_angle_pr_time

cenx = median(angall(0,1,0:nang(0)-1))
ceny = median(angall(0,2,0:nang(0)-1))

rdist = fltarr(nstar)
for i=1,cg-1 do $
 rdist(wg(i)) = sqrt( ( median(angall(wg(i), 1, 0:nang(wg(i))-1)) - cenx )^2.0 + $
                  ( median(angall(wg(i), 2, 0:nang(wg(i))-1)) - ceny )^2.0 )

; The distance in pixels that the star has moved
for i=1,cg-1 do $
 wireult.angle(wg(i)) = med_angle_pr_time * (!PI / 180. ) * $
                         (wireult.hjd - t0) * rdist(wg(i))
   

; plot,wireult.hjd-t0,wireult.angle(1)
; plot,wireult.hjd-t0,wireult.angle(1)

!P.multi=0

END
