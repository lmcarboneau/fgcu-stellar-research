PRO wire_merge_ref_time,tt,dd,t0,refmag,nmerge,ttref,ddref, aborting, $
 magvar=magvar,debug=debug

; Before merging data points, calculate a new time array: 
; the reference times!
; LC Points will be merged with the nmerge factor == number of points
; to be included in each time interval.

default9, magvar, 0.50 ; allowed limit in mag-range
default9, debug, 0B

 if debug eq 1 then col=getcolor(/load)

if refmag gt 20 then stop


nall = n_elements(dd)
w = where(tt gt 10000. and abs(dd-refmag) lt magvar and dd lt 25.,ctot) ; valid HJD times


IF ctot / nall lt 0.95 then begin
 print,' *** Many invalid points ... investigate:'
 help,tt,dd,nall,ctot
 print,$
  ' %%% Refmag from xyinfo structure + magvar range: ',refmag,magvar
 w99 = where(tt gt 10000. and dd lt 25.,c_99)
 print,' %%% Observed median of ref star: ',median(dd(w99))

 plot, dd-refmag,psym=3,yr=[-4,4],tit='Magn + Accepted ranges ... '
 plots,!x.crange,0
 plots,!x.crange,magvar,line=2
 plots,!x.crange,-magvar,line=2
ENDIF

aborting = 0B
if ctot le 1 then begin
 print,' No valid points ... returning'
 aborting = 1B
 RETURN
endif

print,' %%% Total lc points: ',nall
print,' %%% Valid lc points: ',ctot, ' ---  ',$
              100. * (float(ctot)/nall),' %'

d0 = median(dd(w))        ; median magnitude of star

tt2 = tt(w) - t0
dd2 = dd(w) - d0

a = sort(tt2)  &  tt2 = tt2(a)  &  dd2 = dd2(a)

cnt = 0L ; 1650L ; 0L
cnt_ref = 0L


tstep = median( tt2(a(1:ctot-1)) - tt2(a(0:ctot-2)) ) ; typical time step

tinc  =  0.40 * tstep
tinc2 =  0.50 * tstep
tlim  = tstep * nmerge + tinc

; tlim  = tstep * (nmerge*0.5) + tinc
 ; 0.3 * tstep: increase limit a bit --- but not enough to include neigh. points
tstart = min(tt2)

used = bytarr(ctot) ; only use each data point once!

; Calculate approx. number of points in reference time array
nm = float(ctot) / nmerge
nm = round(nm * 30.5)  ; reserve plenty of array entries!

ttref = dblarr(3,nm)
ddref = fltarr(nm)

ss = robust_sigma(dd2)

print,' %%% Progress [0 -- 100]: '

; ================================================================
while cnt lt ctot do begin
; ================================================================

 if (cnt_ref mod 800) eq 0 then $
  print,(float(cnt_ref)/(nm/1.2))*100.,format='(I3,$)' ; progress bar

 tx = tt2(cnt)
 wl       = where( (abs(tt2 - tx - tinc ) le tlim) and (used eq 0), cl       )
 wl_neigh = where( (abs(tt2 - tx )        le tlim * 12.)          , cl_neigh )


 if cl gt 1000 then begin
   print,' *** I detected > 1000 points for LC averaging ... hm!'
   stop ; too many points!!! that's suspicious!
 endif

 if cl gt nmerge then begin
    wl = wl(0:nmerge-1) ; keep max # points == nmerge
    cl = nmerge
    if cl eq 1 then wl = wl(0)
 endif

; ----------------------------------------------------------
cok = 0
if cl ge 1 then begin
 cok = cl & wok = findgen(cl) ; default
 if cl eq 1 then wok = wok(0)
endif
; ---------------------------------------------------

; =============================================================================
nlim =  5
nlim2 = 4

 if cl ge nlim then begin
      ss_neigh = robust_sigma(dd2(wl_neigh))
      me = median(dd2(wl))
      m_lim = ss_neigh * 5.5 + 1e-5 ; time limit : 1e-5 = 0.86 seconds
    
    ; Data points must lie within 5.5 of the local stdev of the points
      wok = where(abs(dd2(wl)-me) lt m_lim,cok)
      tajms = tt2(wl(wok))
      uu = uniq(tajms)
      cok2 = n_elements(uu)
      if cok2 eq 1 then begin
          wok = wok(uu)
          cok = cok2
      endif    

      if cok ge nlim2 then begin
       resistant_mean,tajms,3,t_mean,sd,nr
       ttref(0,cnt_ref) = t_mean
       ttref(1,cnt_ref) = min(tt2(wl(wok))) - tinc2
       ttref(2,cnt_ref) = max(tt2(wl(wok))) + tinc2
       ddref(cnt_ref) = avg(dd2(wl(wok)))
       used(wl) = 1
    
       cnt_ref = cnt_ref + 1L   
      endif
 endif

 if (cl lt nlim) or (cl ge nlim and cok lt nlim2) then begin
      if cl ge 2 then begin ; 2-5 data points?
       ttref(0,cnt_ref) = avg(tt2(wl))
       ttref(1,cnt_ref) = min(tt2(wl)) - tinc2
       ttref(2,cnt_ref) = max(tt2(wl)) + tinc2 
       ddref(cnt_ref) = avg(dd2(wl))
       used(wl) = 1
       cnt_ref = cnt_ref + 1L   
      endif
      if cl eq 1 then begin ; only 1 data point!
       ttref(0,cnt_ref) = tt2(wl)
       ttref(1,cnt_ref) = tt2(wl) - tinc2
       ttref(2,cnt_ref) = tt2(wl) + tinc2
       ddref(cnt_ref) = dd2(wl)
       used(wl) = 1
       cnt_ref = cnt_ref + 1L   
      endif
 endif


; =============================================================================
; Make sure that the interval is not too short in time:
; =============================================================================
min_dt = (0.4 * nmerge * tstep)

; Lower time limit
if ( (ttref(0,cnt_ref-1) - ttref(1,cnt_ref-1)) lt  min_dt) and $
   ( cnt_ref ge 2) then begin
   add = ttref(1,cnt_ref-1) - ttref(2,cnt_ref-2)
   if add gt min_dt then add = min_dt
   ttref(1,cnt_ref-1) = ttref(1,cnt_ref-1) - add + tinc
endif

; Upper time limit
if ( (ttref(2,cnt_ref-1) - ttref(0,cnt_ref-1)) lt  min_dt) and $
   ( (cnt+cl) le (ctot-1) ) then begin
   add = tt2(cnt+cl) - ttref(2,cnt_ref-1) ; next point in time series (sorted times!!)
   if add gt min_dt then add = min_dt
   ttref(2,cnt_ref-1) = ttref(2,cnt_ref-1) + add - tinc ; be careful!
endif

; =============================================================================

; print,cnt,cl,format='(2I6,$)'
 cnt = cnt + cl

 if debug eq 1 and cl gt 2 then begin
; =============================================================================
   plot,tt2,dd2,$
    psym=1,symsi=.1,xr=[-1,1] * tlim * 4.0 + tx,yr=[-1,1]*ss*6.0,$
    xtit='!4D!3 HJD',ytit='!4D!3 mag'
   if cl ge 2 then $
    oplot,tt2(wl),dd2(wl),psym=1,symsi=.5,col=col.green   
;    oplot,tt2(wl_neigh),dd2(wl_neigh),psym=1,symsi=.1,col=col.red

   g = findgen(cnt_ref) & g(*) = 0
   if cnt_ref gt 2 then $
    oplot,ttref(0,0:cnt_ref-1),ddref(0:cnt_ref-1),psym=6,symsi=1.0,col=col.sky

   plots,ttref(1,cnt_ref-1),!y.crange,line=2
   plots,ttref(2,cnt_ref-1),!y.crange,line=2

   if cnt_ref gt 2 then begin
    plots,ttref(1,cnt_ref-2),!y.crange,line=1,col=col.red
    plots,ttref(2,cnt_ref-2),!y.crange,line=1,col=col.red
   endif

 print,' Hit . ' & s = get_kbrd(1) & if s eq 'x' then stop

 endif
; =============================================================================

endwhile ; any data points left?
; =============================================================================

; =============================================================================
print,'' ; New line in progress bar
print,' %%% End of finding reference times; number of points: '+strcompress(cnt_ref,/remove_all)
; =============================================================================


; =============================================================================
ttref = ttref(*,0:cnt_ref-1)
for k=0,2 do ttref(k,*) = ttref(k,*) + t0
ddref = ddref(0:cnt_ref-1)
ddref = ddref + d0
; =============================================================================

; =============================================================================
wused = where(used eq 0,cused)
if ( float(cused) / float(n_elements(tt)) ) gt 0.05 then begin
 print,' *** More the five percent of points were not used ... '
 print,'investigate this now! Perhaps some "where" command is not right?'

 
 col=getcolor(/load)
 plot,tt,dd-d0,psym=3,yr=[-1,1]*0.01,tit='Red == unused data points'
 oplot,tt(wused),dd(wused)-d0,col=col.red,psym=3

 stop
endif
; =============================================================================


END
