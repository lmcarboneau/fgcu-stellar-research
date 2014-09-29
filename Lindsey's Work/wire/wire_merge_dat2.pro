 PRO wire_merge_dat2,times,dat,gc,weight,newtimes,newdat,gc3,newsig, n_merge

; This code is used by wire_merge.pro
; n_merge: set to 15 to merge every 15 data points 
; (except in cases where delta_t is too large!)

; cnt2 = counter for raw lc array
; cnt  = counter for new "merged" lc array
 
; ---------------------------------
; Stragedy for merging data points: 
; ---------------------------------
; 1. Calculate mean lc for reference star and mean times (using Stetson weights). 
; 2. Using the same times, calculate mean lc for the other stars

; I do not use the weights from wire_merge2.pro ---> in case of local
; offsets in the light curve these points will be given low weight,
; eg. real oscillations or errs. due to scattered light will be
; artifically removed! Instead local weights are calculated --- a
; large number of points is used to calculate the noise level, the
; mean level is estimated from just the "n_merge" points that will be
; merged. The weights are then based on the deviation from this mean
; level and the noise level (which is based on a larger number of points).

                                                              
refstar = 0B ; usually = 0 == the brightest star!

firstref = 0B ; must always be set to 0 !
siglim = 4.5

tadj = 0D

nstar = n_elements(dat(*,0)) ; number of slots == stars

debug = 0 ; 1 or 9
col=getcolor(/load)

for star=0,nstar-1 do begin
 a = sort(times(star,*)) ; sort data points by times
 times(star,*)  = times(star,a)
 dat(star,*)    = dat(star,a)
 weight(star,*) = weight(star,a)
endfor

np = n_elements(times(0,*)) ; number of data points
ts = median(times(refstar,1:np-1) - times(refstar,0:np-2)) ; typical time step

; ; np = 3000 ; debug!

if n_elements(n_merge) eq 0 then n_merge = 8. ; approx. # data points to merge
 
tlim = ts * n_merge ; merge every delta_t = tlim

; Data arrays
 ndd = long((np/n_merge) * 1.10) 
 newdat = fltarr(nstar,ndd) & newdat(*,*) = -9.99
 newsig = newdat
 newtimes = dblarr(ndd)
 gc3 = fltarr(2,nstar,ndd)

; Progress message:
 print,''
 print,'%%% Merging '+strcompress(string(np),/remove_all)+ ' points!'
 print,''

; for star=0,nstar-1 do begin ; for each star

; ======================================================================
; ===== CALCULATE MEAN LIGHT CURVES ====================================
; ======================================================================

 star = refstar ; first of all, do the reference star

calc_lc: ; goto this point for any other star 
if star ge nstar then goto,slutt

 used = bytarr(np)
 used(*) = 0 ; reset "data point used" flags

; Reset counters:
 cnt  = 0L ; new merged lc array
 cnt2 = 0L ; raw        lc array

for ii=0L,np-1 do begin ; for each data point

if (cnt2 lt (np-1)) and $
    (cnt lt ndd)    then begin ; any unused data points left?

if star eq refstar then begin
 w = where(abs(times(star,*)-times(refstar,cnt2)) lt tlim and $
           used eq 0 and abs(dat(star,*)) lt 1.0,c)

 wss = where(abs(times(star,*)-times(refstar,cnt2)) lt (tlim*6.) and $
             abs(dat(star,*)) lt 1.0,css) ; data used to calc. local noise!
endif else begin ; USE SAME TIMES AS FOR THE REF. STAR:
 tadj = tlim*0.5
 w = where(abs(times(star,*)-(newtimes(cnt)-tadj)) lt tlim and $
           used eq 0 and abs(dat(star,*)) lt 5.0,c)

 wss = where(abs(times(star,*)-(newtimes(cnt)-tadj)) lt (tlim*6.) and $
             abs(dat(star,*)) lt 5.0,css) ; data used to calc. local noise!
endelse

; do not use a data point more than once (the "used" array)

  if star eq refstar then time0 = times(refstar,cnt2)
  if star ne refstar then time0 = newtimes(cnt)

 if c ge 3 then begin
  resistant_mean,dat(star,wss),3,me,sd,nr
  sig = robust_sigma(dat(star,wss))


  if time0 eq 0.0 then begin
    if star eq refstar then goto,bad_data_point_ref
    goto,bad_data_point
  endif

  if debug ge 9 and star ne refstar then begin
    plot,times(star,w)-time0,dat(star,w),psym=1,xr=[-1,1]*.00015*3,yr=[-1,1]*sig*5.
    oplot,times(star,*)-time0,dat(star,*),psym=3,col=col.red
    oplot,times(star,wss)-time0,dat(star,wss),psym=3,col=col.sky
    plots,0.,!y.crange,line=2

    plots,!x.crange,me,line=2,col=col.green
    plots,!x.crange,me+sig*siglim,line=1,col=col.green
    plots,!x.crange,me-sig*siglim,line=1,col=col.green
  endif

; Remove extreme outliers!  
  if sig gt 0.0 then begin 
   w2 = where(abs(dat(star,w)-me) lt siglim * sig,cok)
  endif else begin ; Calc. of sigma may fail ...
   w2 = findgen(c) ; incl all points
   cok = c
  endelse

   if cok le 3 and star ne refstar then goto,bad_data_point
   if cok le 3 and star eq refstar then goto,bad_data_point_ref

; Calculate Stetson outlier weights !
   resistant_mean,dat(star,w(w2)),3,me,sd_of_mean,nr ; mean level == use only local points!
   ; ptp_robust_fin,dat(star,wss),noise,1 ; noise = use more neighbouring points!
   fivesigma = 5. * robust_sigma(dat(star,wss)) ; 5. * noise
   astet = 0.7 & bstet = 6.0
   fudge_weight = (1. + (abs(dat(star,w)-me)/(astet*fivesigma))^bstet)^(-1.)
   fudge_weight = fudge_weight / total(fudge_weight)
   wei = fudge_weight
   ; plot,dat(star,w),wei,psym=1

  if debug ge 9 and star ne refstar then begin
   rw = max(wei) - min(wei) & r1 = max(wei)
   pz = fltarr(cok) & for p=0,cok-1 do pz(p) = 0.5 + 1.0 * ( 1.0 / r1 ) * wei(p)
   for p=0,cok-1 do $
    oplot,[1,1]*(times(star,w(w2(p)))-time0),[1,1]*dat(star,w(w2(p))),$
     psym=4,col=col.green,symsi=pz(p)
  endif

; print,'New data point stored!'
  newdat(star,cnt)   = total(dat(star,w(w2)) * wei(w2)) ; weighted data point!
  newsig(star,cnt)   = robust_sigma(dat(star,w(w2)))

; only make time array for reference star 
;  --- the other stars use this same time array  
  if star eq refstar then begin 
   tt0 = min(times(star,w(w2)))
   tt1 = times(star,w(w2)) - tt0
   newtimes(cnt) = total(tt1 * wei(w2)) + tt0
  endif

  if debug ge 9 and star ne refstar then begin
   oplot,[1,1]*newtimes(cnt)-time0,[1,1]*( newdat(star,cnt) ),$
    psym=2,col=col.red,symsi=2.0
   oplot,newtimes-time0,newdat(star,*),psym=2,symsi=.3,col=col.red
;   print,'Hit ' & s = get_kbrd(1) 
  endif
 
; gaussian x,y position
  gc3(0,star,cnt)    = total( gc(0,star,w(w2)) * wei(w2) ) 
  gc3(1,star,cnt)    = total( gc(1,star,w(w2)) * wei(w2) )

  used(w) = 1 ; mark points as used -- ie. only use each data point once!

; ---------------------------------------------------------------------------------
if debug ge 1 then begin ; and star eq 1 then begin
;    if cnt gt 520 then begin
   if (cnt2 mod 500) eq 0 then begin ; progress plot
     wp = where(abs(times(star,*)-(time0-tadj)) lt tlim*60. and abs(dat(star,*)) lt 2.,c_wp)
     wn = where(abs(newtimes-(time0-tadj)) lt tlim*60. and abs(newdat(star,*)) lt 2.,c_wn)
     ss = robust_sigma(dat(star,wp))
    
    ; !P.multi=[0,2,1]
    
     t0 = 51480.0D ; median(times(star,wp))
     if c_wp ge 2 then $
     plot,times(star,wp)-t0,dat(star,wp)-median(dat(star,wp)),$
      psym=1,symsi=.5,yr=[-1,1]*ss*7.,xsty=3,ysty=3,$
      tit='White = org data ; Red = Merged data ; star = '+$
      strcompress(string(star),/remove_all),$
      xtit='!4D!3 HJD',ytit='!4D!3m'
     if c_wn ge 2 then $
      oplot,newtimes(wn)-t0,newdat(star,wn)-median(newdat(star,wn)),psym=2,col=col.red
;      oplot,newtimes-t0,newdat(star,*)-median(newdat(star,wn)),psym=2,col=col.red

      if robust_sigma(newdat(star,wn)) eq 0.0 then begin
;        s = ''
;        print,'Hit me ... star ',star,cnt2 & s = get_kbrd(1)
;        if s eq 'x' then stop
      endif
   endif
endif
; ---------------------------------------------------------------------------------

 bad_data_point:
   cnt = cnt + 1   ; a new (low-resolution) data point was made
bad_data_point_ref: ; do not increase "cnt" if you are doing the reference star
  cnt2 = cnt2 + c  ; "scroll" to the next (high-resolution) data points

 endif else begin ; less than 3 data points in this bin !
  used(cnt) = 1
  cnt2 = cnt2 + 1 ; not used for a "non-ref. star"
  if star ne refstar then cnt = cnt + 1 ; if not the ref. star
 endelse ; END OF "if less than 3 data points in this bin ..."

 if (cnt mod 100) eq 0 and (cnt gt 100) then print,100. * float(cnt2) / np,format='(I3,$)'

endif ; any data points left?

endfor ; go to next data point

if star eq refstar and firstref eq 0 then begin
 firstref = 1 ; refstar done
 star = 0
endif

agg:
if star lt (nstar-1) then begin
 star = star + 1
 if star eq refstar then goto,agg
 print,'Moving on to do star.. '+strcompress(string(star),/remove_all)  ; &  s = get_kbrd(1)
 goto,calc_lc
endif

slutt:

; Remove unused data points
newtimes = newtimes(0:cnt-1)
newdat   = newdat(*,0:cnt-1)
gc3      = gc3(*,*,0:cnt-1)
newsig   = newsig(*,0:cnt-1)

; Subtract median of complete light curve
for star=0,nstar-1 do $
 newdat(star,*) = newdat(star,*) - median(newdat(star,*))


END
