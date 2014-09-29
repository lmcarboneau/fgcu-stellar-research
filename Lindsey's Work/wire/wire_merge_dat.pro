 PRO wire_merge_dat,times,dat,gc,weight,newtimes,newdat,gc3,newsig, n_merge

; This code is used by wire_merge.pro
; n_merge: set to 15 to merge every 15 data points 
; (except in cases where delta_t is too large!)

debug = 0
col=getcolor(/load)

a = sort(times)
 times = times(a)
 dat = dat(a)
 weight = weight(a)

np = n_elements(times)

ts = median(times(1:np-1) - times(0:np-2)) ; typical time step

if n_elements(n_merge) eq 0 then n_merge = 8.
 
tlim = ts * n_merge ; merge every delta_t = tlim
used = bytarr(np)

; Data arrays
 ndd = long((np/n_merge) * 1.10) 
 newdat = fltarr(ndd)
 newsig = newdat
 newtimes = dblarr(ndd)
 gc3 = fltarr(2,ndd)

; Counters:
 cnt  = 0L
 cnt2 = 0L

; Progress message:
 print,''
 print,'%%% Merging '+strcompress(string(np),/remove_all)+ ' points!'
 print,''

for ii=0L,np-1 do begin
if cnt2 lt (np-1) then begin
 w = where(abs(times-times(cnt2)) lt tlim and used eq 0,c)
 ; used(w) = 1 ; do not use a data point more than once

 if c ge 3 then begin
  resistant_mean,dat(w),3,me,sd,nr
  sig = robust_sigma(dat(w))

  if sig gt 0 then begin
   w2 = where(abs(dat(w)-me) lt 3.5 * sig,cok)
  endif else begin
   w2 = findgen(c) ; incl all points
  endelse

;   newdat(cnt)   =          avg(dat(w(w2)))

  wei = weight(w(w2)) & wei = wei / total(wei) ; new weights!

  newdat(cnt)   = total(dat(w(w2)) * wei) ; weighted data point!
  newsig(cnt)   = robust_sigma(dat(w(w2)))
  newtimes(cnt) = total(times(w(w2)) * wei)
  gc3(0,cnt)    = median(gc(0,w(w2))) ; gaussian x,y position
  gc3(1,cnt)    = median(gc(1,w(w2)))

  used(w) = 1 ; mark points as used

  cnt = cnt + 1   ; a new (low-resolution) data point was made
  cnt2 = cnt2 + c ; "scroll" to the next (high-resolution) data points
 endif else begin
  used(cnt) = 1
  cnt2 = cnt2 + 1
 endelse

  if cnt mod 500 eq 0 then print,100. * float(cnt2) / np,format='(I3,$)'

if debug eq 1 then begin
    if cnt gt 520 then begin
     wp = where(abs(times-times(cnt2)) lt tlim*60.,c_wp)
    
    ; !P.multi=[0,2,1]
    
     t0 = 51480.0D ; median(times(wp))
     plot,times(wp)-t0,dat(wp),psym=1,symsi=.5,yr=[-1,1]*0.03,xsty=3,ysty=3
     oplot,newtimes-t0,newdat,psym=2,col=col.red
     print,'Hit me ... ' & s = get_kbrd(1)
     if s eq 'x' then stop
    endif    
endif

endif 
endfor

; Remove unused data points
newtimes = newtimes(0:cnt-1)
newdat   = newdat(0:cnt-1)
gc3      = gc3(*,0:cnt-1)
newsig   = newsig(0:cnt-1)


newdat = newdat - median(newdat)


END
