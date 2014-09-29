; Merge data for a single light curve using the "ttef" time limits

PRO    wire_merge_dat_any,$
         tt , dd , x     , y     , gc     , fwhm,  back,  ttref, $
         ttn, ddn, xn    , yn    , gcn    , fwhmn, backn, nmerge, $
         dmag, $
         t0, $
         dlim, $
         use_weights, debug, $
         init, star, wireult

if n_elements(init) eq 0 then init = 0B

; dlim = 0.02 ; points must be this close in position on CCD
; dlim = 0.10 ; changed on 2.10.03
dlim = dlim^(2.0)

; LC Points will be merged with the nmerge factor == number of points
; to be included in each time interval.

; Adopted from "wire_merge_ref_time.pro"

; (c) Hans Bruntt -- September 12th 2003

; ---------------------------------------------------
if n_elements(debug) eq 0 then debug = 0
if debug eq 1 then col=getcolor(/load)
; ---------------------------------------------------

; ---------------------------------------------------
if n_elements(use_weights) eq 0 then use_weights = 1
; ---------------------------------------------------

; ---------------------------------------------------
a = sort(tt) ; sort times ... !!
tt = tt(a) & dd = dd(a) & x = x(a) & y = y(a) & gc = gc(*,a)
fwhm = fwhm(a) 
back = back(a)

w = where(tt gt 10000. and dd gt 2. and dd lt 25.,c) ; valid HJD times

dd2 = dd(w)
d0 = median(dd(w))  ; median magnitude of star

tt2   = tt(w) - t0 ; offset times
dd2   = dd2 - d0 ; offset magnitudes
x2    = x(w)
y2    = y(w)
gc2   = gc(*,w)
fwhm2 = fwhm(w)
back2 = back(w)

; ---------------------------------------------------
; typical time step (normally 0.5 secs): Nov. 2006: I use histogram in stead
tstep = median( tt2(1:c-1) - tt2(0:c-2) ) 
h = histogram( (tt2(1:c-1) - tt2(0:c-2))*86400.,min=0,max=10,bin=.1)
hx = findgen(10./.1) * .1
plot,hx,h,xr=[0,5],xtit='Time btw. observations (seconds)',ytit='Number of occurences'
hmax = where(h eq max(h),cx) & tmax = hx(hmax)
wait,2

; print,' %%% Time step: ' + strcompress(tstep * 86400.) + ' seconds'
print,' %%% Time step: ' + strcompress(tmax) + ' seconds'
if tmax lt .2 or tmax gt .7 then stop ; should be == 0.5 sec
; ---------------------------------------------------

used = bytarr(c) ; only use each data point once!

; Number of points in reference time array
nm = n_elements(ttref(0,*)) 
; ---------------------------------------------------

; ---------------------------------------------------
; ttn = dblarr(3,nm)
ddn   = fltarr(nm) ; avg. lc. (4.5 sigma clipping)
gcn   = fltarr(2,nm)
xn    = fltarr(nm)
yn    = fltarr(nm)
fwhmn = fltarr(nm)
backn = fltarr(nm)

ddn(*) = -9.99

ddn2   = fltarr(nm) ; using stetson weights!
gcn2   = fltarr(2,nm)
xn2    = fltarr(nm)
yn2    = fltarr(nm)
fwhmn2 = fltarr(nm)
backn2 = fltarr(nm)

ddn2(*) = -9.99

ss = robust_sigma(dd2) ; overall noise in light curve
; ---------------------------------------------------

print,' %%% Combining data points [0--100]: ',format='(A38,$)'

; ================================================================
for cnt_ref = 0L,nm-1 do begin
; ================================================================

 if (cnt_ref mod 500) eq 0 then print,(float(cnt_ref)/nm)*100.,format='(I3,$)' ; progress ?

 tx = ttref(0,cnt_ref) ; new zero point in time array

 wl = where(tt2 ge (ttref(1,cnt_ref)-t0) and $
            tt2 lt (ttref(2,cnt_ref)-t0) and $
            used eq 0,cl)

; Testing, 24th of August 2004:
; if (cnt_ref+1) ne n_elements(ttref(1,*)) then $
; wl = where(tt2 ge (ttref(1,cnt_ref)-t0) and $
;            tt2 lt (ttref(1,cnt_ref+1)-t0) and $
;            used eq 0,cl) else $
; wl = where(tt2 ge (ttref(1,cnt_ref)-t0) and $
;            used eq 0,cl)

; plot,tt2,dd2,psym=3
; oplot,tt2(wl),dd2(wl),psym=3,col=col.red

 if cl gt nmerge then begin
   wl = wl(0:nmerge-1) ; keep max # points == nmerge
   cl = nmerge
   if cl eq 1 then wl = wl(0)
 endif

; ----------------------------------------------------------
; Hard limit: Data points must be within "dlim" pixels !! 
; Otherwise (x,y) position is averaged out!
; ----------------------------------------------------------
 if cl ge 3 then begin
  resistant_mean,gc2(0,wl),3,x_res,x_sd,x_nr     ; resistant x position
  resistant_mean,gc2(1,wl),3,y_res,y_sd,y_nr     ; resistant y position 
  dist = (gc2(0,wl)-x_res)^2. + (gc2(1,wl)-y_res)^2 ; squared distance from mean x,y pos.
  wpos = where(dist lt dlim,cpos) ; select points within a given limit
  if cpos ge 3 then wl = wl(wpos) ; remove deviant points!
  if cpos le 2 then begin ; a bad group of points == no definite position
     wl = -1
     cl =  0
  endif
 endif
; ----------------------------------------------------------

; ----------------------------------------------------------
; Find neighbouring data points!
; ----------------------------------------------------------
  n1 = cnt_ref - 5 ; five data blocks previous in time
  n2 = cnt_ref + 5 ; five data blocks ahead in time
 if n1 lt 0 then n1 = 0
 if n2 gt (nm-1) then n2 = (nm-1)
 wl_neigh = where(tt2 ge (ttref(1,n1)-t0) and $
                  tt2 lt (ttref(2,n2)-t0),cl_neigh)

 if cl_neigh eq 0 then begin
;   print,' *** WARNING: No neigh. data points found in wire_merge_dat_any!'
   goto,fail_neigh
 endif
; ----------------------------------------------------------
cok = 0
if cl ge 1 then begin
 cok = cl & wok = findgen(cl) ; default
endif
; ---------------------------------------------------

 if cl ge 4 then begin ; check line at "24.9.03" if you change this!
  ss_neigh = robust_sigma(dd2(wl_neigh)) ; noise for neigh. points
  me = median(dd2(wl))   ; median level for the lc points
  m_lim = ss_neigh * 4.5 ; noise limit 

; Data points must lie within 4.5 of the local stdev of the points
  wok = where(abs(dd2(wl) - me) lt m_lim,cok)

; ---------------------------------------------------
  if cok ge 3 then begin ; check line at 24.9.03 if you change this!
; -----------------------------------------------------------------------------

  if use_weights eq 1 then begin

  ; Calculate Stetson outlier weights !
    fivesigma = 4.0 * ss_neigh
    astet = 0.7 & bstet = 6.0
    fudge_weight = (1. + (abs(dd2(wl(wok)) - me)/(astet*fivesigma))^bstet)^(-1.)
    wei = fudge_weight / total(fudge_weight)
    ;;; plot,dd2(wl(wok)),wei,psym=1
    ddn2(cnt_ref) = total( wei * dd2(wl(wok)) )    
    for k=0,1 do gcn2(k,cnt_ref) = total( wei * gc2(k,wl(wok)) )
    xn2(cnt_ref) = total( wei *    x2(wl(wok)) )
    yn2(cnt_ref) = total( wei *    y2(wl(wok)) )
 fwhmn2(cnt_ref) = total( wei * fwhm2(wl(wok)) )
 backn2(cnt_ref) = total( wei * back2(wl(wok)) )

   endif ; use weights?
   ; -----------------------------------------------------------------------------

   ; Data points without weights:
    ddn(cnt_ref) = avg(dd2(wl(wok)))
    for k=0,1 do gcn(k,cnt_ref) = avg( gc2(k,wl(wok)) )
     xn(cnt_ref) = avg(    x2(wl(wok)) )
     yn(cnt_ref) = avg(    y2(wl(wok)) )
  fwhmn(cnt_ref) = avg( fwhm2(wl(wok)) )
  backn(cnt_ref) = avg( back2(wl(wok)) )

    used(wl) = 1

   endif
; ---------------------------------------------------
endif
; ---------------------------------------------------

; -----------------------------------------------------------------------------
if (cl ge 4 and cok le 2) or (cl le 3) then begin ; 24.9.03

      if cl ge 2 then begin ; 2-5 data points?
        ddn(cnt_ref) = avg(dd2(wl))
       ddn2(cnt_ref) = ddn(cnt_ref)
       for k=0,1 do gcn(k,cnt_ref)  = avg( gc2(k,wl) )
       for k=0,1 do gcn2(k,cnt_ref) = avg( gc2(k,wl) )
        xn(cnt_ref) = avg( x2(wl) ) &  yn(cnt_ref) = avg( y2(wl) ) 
       xn2(cnt_ref) = avg( x2(wl) ) & yn2(cnt_ref) = avg( y2(wl) ) 
    fwhmn(cnt_ref)  = avg( fwhm2(wl) ) 
    fwhmn2(cnt_ref) = avg( fwhm2(wl) ) 
    backn(cnt_ref)  = avg( back2(wl) ) 
    backn2(cnt_ref) = avg( back2(wl) ) 
   
       used(wl) = 1
       wok = findgen(cl) ; for plotting = debug
      endif

      if cl eq 1 then begin ; only 1 data point!
        ddn(cnt_ref)   = dd2(wl)
        ddn2(cnt_ref)  = dd2(wl)
        for k=0,1 do  gcn(k,cnt_ref) = gc2(k,wl)
        for k=0,1 do gcn2(k,cnt_ref) = gc2(k,wl)
          xn(cnt_ref) = x2(wl) &  yn(cnt_ref) = y2(wl) 
         xn2(cnt_ref) = x2(wl) & yn2(cnt_ref) = y2(wl) 
       fwhmn(cnt_ref) = fwhm2(wl)  
      fwhmn2(cnt_ref) = fwhm2(wl) 
       backn(cnt_ref) = back2(wl)  
      backn2(cnt_ref) = back2(wl) 
     
         used(wl) = 1
         wok = 0 ; for plotting = debug
      endif

  endif
; ---------------------------------------------------


; ---------------------------------------------------
 if debug eq 1 and cl ge 3 then begin
 !P.multi=0 ; [0,1,2]

; goto,skippp

goto,nofwhm

   plot,fwhm2,dd2,$
    psym=1,symsi=.1,xr=[1.75,2.0],$
    yr=[-1,1]*ss*6.0,$
    ytit='!4D!3 mag',xtit='FWMH [pixels]',$
    tit='Green=Raw Points Used; Yel=Weighted Point; Red=Avg. Point',charsi=1.0,/nodata
   if cl ge 2 then $
    oplot,fwhm2(wl),dd2(wl),psym=1,symsi=1.0,col=col.green   
   if cnt_ref gt 2 then begin
;    oplot,fwhmn(0:cnt_ref-1),ddn(0:cnt_ref-1),psym=7,symsi=0.5,thick=2,col=col.red
;    oplot,fwhmn2(0:cnt_ref-1),ddn2(0:cnt_ref-1),psym=1,symsi=.5,thick=2,col=col.yellow
    plots,fwhmn(cnt_ref),ddn(cnt_ref),psym=7,symsi=1.0,thick=2,col=col.red
    plots,fwhmn2(cnt_ref),ddn2(cnt_ref),psym=1,symsi=1.0,thick=2,col=col.yellow

goto,ggg

nofwhm:

   tlim = ttref(2,cnt_ref) - ttref(1,cnt_ref)
   plot,tt2,dd2,$
    psym=1,symsi=.1,xr=[ttref(1,cnt_ref),ttref(2,cnt_ref)]-t0+[-tlim,tlim]*5.,$
    yr=[-1,1]*ss*6.0,$
    xtit='!4D!3 HJD',ytit='!4D!3 mag',$
    tit='Green=Raw Points Used; Yel=Weighted Point; Red=Avg. Point',charsi=1.0
   if cl ge 2 then $
    oplot,tt2(wl),dd2(wl),psym=1,symsi=.5,col=col.green   
   if cnt_ref gt 2 then begin
    oplot,ttref(0,0:cnt_ref-1)-t0,ddn(0:cnt_ref-1),psym=7,symsi=2.5,thick=2,col=col.red
    oplot,ttref(0,0:cnt_ref-1)-t0,ddn2(0:cnt_ref-1),psym=1,symsi=2.5,thick=2,col=col.yellow
   endif
   plots,ttref(1,cnt_ref)-t0,!y.crange,line=2
   plots,ttref(2,cnt_ref)-t0,!y.crange,line=2
   if cnt_ref gt 2 then begin
    plots,ttref(1,cnt_ref-1)-t0,!y.crange,line=1,col=col.red,thick=2
    plots,ttref(2,cnt_ref-1)-t0,!y.crange,line=1,col=col.red,thick=2
   endif

ggg:

 if cnt_ref ge 1 then begin
   if ttref(0,cnt_ref-1)-t0 gt 150 then begin 
     print,'Hit...' & s = get_kbrd(1) & if s eq 'x' then stop
   endif
 endif

skippp:
 goto,nnn

;    if cnt_ref gt 11300L and $
;       (max(abs( gcn2(0,cnt_ref) - gc2(0,wl(wok)) )) gt 0.15 or $
;        max(abs( gcn2(1,cnt_ref) - gc2(1,wl(wok)) )) gt 0.15) then begin

    plot,gc2(0,*)-3.5,gc2(1,*)-3.5,xr=[-1,1]*0.5,yr=[-1,1]*0.5,$
     xsty=1,ysty=1,xtit='!4D!3 x',ytit='!4D!3 y',tit='Mean Positions',psym=3 ; ,/nodata
    if n_elements(wok) ge 2 then $
     oplot,gc2(0,wl(wok))-3.5,gc2(1,wl(wok))-3.5,psym=1,symsi=.5,col=col.green

     plots,gcn2(0,cnt_ref)-3.5,gcn2(1,cnt_ref)-3.5,psym=4,symsi=1.0,col=col.red

      print,'Deviant point: Hit any key!'  &  s = get_kbrd(1)
  
    for ll=0,cok-1 do $
     arrow,0,0,gc2(0,wl(wok(ll)))-3.5,gc2(1,wl(wok(ll)))-3.5,/data,color=col.green
    
    arrow,0,0,gcn2(0,cnt_ref)-3.5,gcn2(1,cnt_ref)-3.5,/data,color=col.red,thick=1.5

;     print,(max(abs( gcn2(0,cnt_ref) - gc2(0,wl(wok)) ))), $
;            max(abs( gcn2(1,cnt_ref) - gc2(1,wl(wok)) ))
;      print,'Deviant point: Hit any key again!'
      s = get_kbrd(1) & if s eq 'x' then stop
   endif

nnn:


print,' %%% Wait ... ' & wait,1.0
;   print,'Hit any key!'  &  s = get_kbrd(1)
;   if s eq 'x' then stop

endif                           ; end of debug == plots
; -------------------------------------------------------

fail_neigh:

endfor                          ; any data points left?
; ---------------------------------------------------

print,'' ; new line in progress bar


; --------------------------------------------------------
wbad = where(ddn lt -5 or fwhmn lt .1,cbad)
wbad2 = where(ddn2 lt -5 or fwhmn2 lt .1,cbad2)

ttn  = ttref     ; The reference time points were used!
ddn  = ddn  + d0 ; Add the magnitude zero point!
ddn2 = ddn2 + d0 ; ... also for the weighted data points!

if cbad ge 1 then ddn(wbad) = -9.99    ; added 09MAR2004
if cbad2 ge 1 then ddn2(wbad2) = -9.99
; --------------------------------------------------------

; --------------------------------------------------------
if use_weights eq 1 then begin
 ddn = ddn2 ; Export the weighted data points
 gcn = gcn2 ; (Gaussian x,y position) ===> decorrelation
  xn = xn2  ; CCD    row position
  yn = yn2  ; CCD column position
fwhmn= fwhmn2 ; FWHM = Seeing of image ===> decorrelation
backn= backn2 ; FWHM = Seeing of image ===> decorrelation
endif
; --------------------------------------------


wused = where(used eq 1,cused)
ratio_use = (float(cused) / n_elements(tt))
if (ratio_use lt 0.05) and (init eq 0) then begin

 print,''
 print,' *** More the 95 percent of points were not used ... ',ratio_use
 print,'investigate this now! Perhaps some "where" command is not right?';
 print,''

 wait, 3.5

; stop

endif



 if debug ge 9 then begin
 col=getcolor(/load)
 ti = median(tt)
 plot,tt-ti,dd-d0,psym=1,yr=[-1,1]*0.2,xr=[-.1,.1],symsi=.2
 if n_elements(wused) ge 2 then $
  oplot,tt(wused)-ti+0.01,dd(wused)-d0,col=col.green,psym=4,symsi=.2
 wnot_used = where(used eq 0,cbad)
 if cbad ge 10 then $
  oplot,tt(wnot_used)-ti+0.02,dd(wnot_used)-d0,col=col.red,psym=6,symsi=.4

 wait,1.0

 wnot_used2 = where(used eq 0 and fwhm lt 1.85,cbad2)
 plot_io,fwhm,back,psym=1,symsi=.2,xr=[1.5,2.5],yr=[.1,5000],ysty=3
 if cbad ge 3 then $
  oplot,fwhm(wnot_used),back(wnot_used),psym=4,col=col.yellow,symsi=.2
 if cbad2 ge 3 then $
 oplot,fwhm(wnot_used2),back(wnot_used2),psym=6,col=col.red,symsi=.5

 xyouts,1.7,2000,strcompress(cbad),col=col.yellow,charsi=1.5
 xyouts,1.8,5,strcompress(cbad2),col=col.red,charsi=1.5
 xyouts,1.5,2,'Total: '+strcompress(n_elements(tt)),charsi=1.5
 xyouts,1.5,3,'Used: '+strcompress(cused),col=col.green,charsi=1.5
 
 wait, 0.8
 
 oplot,wireult.fwhm(star,*), wireult.back(star,*),psym=3,col=col.magenta

 wait, 0.8

 endif



  if debug ge 1 then !P.multi = 0

 
END
