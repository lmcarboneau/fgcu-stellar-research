PRO wire_remove_offsets,tt,dat,dat2,n_orbits ; remove offsets?

debug = 0

aa = sort(tt)
tt = tt(aa)
dat= dat(aa)

freq = 15.009 ; cyc/day
period = 1./freq

;;; t0 = min(tt) ---> must be a direct input to the program!!!
nd = n_elements(tt)
used = bytarr(nd)

dat2 = dat

t1 = min(tt)
tmax = max(tt)

cnt = 0L

arr = 0

a = sort(tt)
tt_sort = tt(a)
dt = tt_sort(1:nd-1)-tt_sort(0:nd-2)
t_step = median(dt)

w = where(dt gt 30. * t_step,c)
tord = dblarr(c+2)
tord(0) = min(tt_sort) - t_step * 5.0
tord(1:c) = tt_sort(w) + t_step * 2.0
tord(c+1) = max(tt_sort) + t_step * 5.0
c = n_elements(tord)

if debug eq 2 then begin
col=getcolor(/load)
plot,tt,dat,psym=3,yr=[-1,1]*0.01,xr=[5,13]
for i=0,c-2 do begin
 w1 = where(tt ge tord(i) and tt lt tord(i+1),c1)
 if c1 ge 2 then oplot,tt(w1),dat(w1),psym=3,col=col.red
 s = get_kbrd(1)
 if s eq 'x' then stop
endfor
endif



; =============================================================
; while t1 lt tmax do begin

cc = c

nn = robust_sigma(dat)
mm = median(dat)

while cnt lt cc do begin

 cnt_max = cnt+n_orbits
 if cnt_max gt (cc-1) then cnt_max = (cc-1)
 w = where(tt ge tord(cnt) and tt lt tord(cnt_max) and $
           abs(dat-mm) lt 15.*nn,c)

; =============================================================
 if c ge 10 then begin
  resistant_mean,dat(w),3,me,sd,nr
  dat2(w) = dat2(w) - me ; subtract mean offset  
  used(w) = 1
  arr = arr + 1

; =============================================================
 if debug eq 1 then begin
;    if cnt gt 5 then stop
    
    ; Debug:
       col=getcolor(/load)
       x1 = min(tt(w)) & x2 = max(tt(w)) & ra = (x2-x1) * 20.
       plot,tt(w),dat(w)-median(dat),psym=3,yr=[-1,1]*0.01,$
        /nodata,ysty=3,xr=[x1-ra,x2+ra]
       dat0 = median(dat)
       oplot,tt   ,dat    -dat0,psym=3 
       oplot,tt(w),dat(w) -dat0,psym=3,col=col.red   
;      oplot,tt(w),dat2(w)-dat0,psym=3,col=col.green
       oplot,tt  ,dat2-dat0,psym=3,col=col.green
       s = get_kbrd(1)
 endif
; =============================================================


 endif
; =============================================================


; =============================================================
 cnt = cnt + n_orbits
; =============================================================

endwhile
; =============================================================

print,' %%% Offsets removed ... total number of groups: '+strcompress(arr)

END
