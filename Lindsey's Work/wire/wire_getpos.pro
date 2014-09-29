PRO wire_getpos, sample_wire_file, target, position_file, target_info_file

; -------------------------------------------------------------------------
; Run this after wire_pos.pro ---> find the xy positions of each star
;                                  and an appropriate t0 value.
; -------------------------------------------------------------------------
; Example:
; -------------------------------------------------------------------------
; target = "AlphaUMi"
; ref = "/data1/bruntt/wire/oct04/polaris/polaris_wire_AlphaUMi_018.ph1.idl"
; wire_getpos, ref, target, $
;              "~/wire/wire_essential/xy_positions4.idl", $
;              "~/wire/wire_essential/targets_wire.txt"
; -------------------------------------------------------------------------


; -------------------------------------------------------------------------
;  Restore important information
; -------------------------------------------------------------------------
!P.multi=0

;nsample = n_elements(sample_wire_file)
;for nn=0,nsample-1 do begin

restore,sample_wire_file
if n_elements(wire2) le 100 then begin
 print,' *** Invalid filename: '+sample_wire_file
 RETURN
endif

if position_file ne '' then restore,position_file
if n_elements(wireinfo) le 0 then begin
 print,' *** position file not found: '+position_file
 RETURN
endif


; Get basic info on all targets: RA, DEC, vsini, stromgren photometry etc.
wire_target_info, target_info_file, info
if n_elements(info) lt 5 then begin
 print,' *** Target info file not found: '+target_info_file
 RETURN
endif
; =============================================================

; =============================================================
; Get some colours for the plots:
; =============================================================
col = getcolor(/load)
colx = [col.white,col.yellow,col.green,col.sky,col.magenta,col.cyan,$
        col.charcoal, col.red, col.red, col.red]
nx = n_elements(colx)
; =============================================================

; =============================================================
; Restore a random idl1x file, and then this program!
; =============================================================
ww = wire2
nstar = n_elements(ww(0).x)
aper  = 4  ; default aperture
; =============================================================

plot,ww.x(0),ww.y(0),psym=3,xr=[-50,550],yr=[-50,550],/nodata, $
 xsty=1,ysty=1
; for i=0,5 do plots,xy(0,i), xy(1,i),psym=8,symsi=5

; =============================================================
device,retain=2
print,''
print,' %%% INSTRUCTIONS: '
print,' %%% (a) Click at various group of points.' 
print,' %%% (b) Click at x,y < 0 (twice) to exit.'
print,''
; =============================================================

np = n_elements(ww)
every = 5
; =============================================================
for i=0,nstar-1 do $
 for p=0L,np-1,every do $
 plots,ww(p).x(i),ww(p).y(i),psym=3,col=colx(i mod nx)
; =============================================================

; =============================================================
aaa = 0
rlim = 35.0

dat = replicate( {x:0., y:0., n:0., m:0. }, 150)
cl = 0
; =============================================================

;restore,'temp.idl'
;ng = cl
;goto,temp

; =============================================================
while aaa eq 0 do begin
    
    cursor,x,y,/up,/data
    ; print,x,y

    if x lt 0 or y lt 0 then aaa = 1

    tvellipse, rlim, rlim, x, y, /data, thick=2
    
    cnt = 0L
    
    for i=0,nstar-1 do begin
        dist = (x - ww.x(i))^2.0 + (y - ww.y(i))^2.0
        mag = -2.5 * alog10(ww.p(i,aper)) + 25.0

        g = where(dist lt rlim*rlim and mag gt 5. and $
         finite(mag),c)

        cnt = cnt + c 
        if c ge 10 then begin
     
          resistant_mean,ww(g).x(i), 3, mex ,sdx, nr
          resistant_mean,ww(g).y(i), 3, mey ,sdy, nr
          resistant_mean,mag(g), 3, mem ,sdm, nr
    
          print,i,mex, mey, mem, ' -- npt = ',c

          dat(cl).x = mex
          dat(cl).y = mey
          dat(cl).m = mem
          dat(cl).n = c
          cl = cl + 1
 
        endif
   
        ; if c ge 10 then print,i,median(ww(g).mag(i)),robust_sigma(ww(g).mag(i))
    endfor

    print,'Number of stars: ',cnt
    
endwhile
; =============================================================

; =============================================================
dat = dat(0:cl-1)
 a = sort(dat.x)
 dat = dat(a)
; =============================================================


; =============================================================
print,' Selected stars: '
for i=0,cl-1 do print,i,':',dat(i),format='(I3,A2,2F6.1, I8, F6.2)'
print,' Set min number of data points ... '
min_data_points = 250.
read,min_data_points

w = where(dat.n gt min_data_points,cl)
dat = dat(w)

agg:

print,' Selected stars: '
for i=0,cl-1 do print,i,':',dat(i),format='(I3,A2,2F6.1, I8, F6.2)'

print,' Enter the stars you think are unique -- seperated by space (eg: 0 2 3 5)'
true2 = ''
read,true2
g = strsplit(true2,' ',/extract) & ng = n_elements(g)
use = fix(g)

dat2 = dat(use)

temp:
; =============================================================
print,' Selected stars: '
for i=0,ng-1 do print,i,':',dat2(i),format='(I3,A2,2F6.1, I8, F6.2)'


plot,ww.x(0),ww.y(0),psym=3,xr=[-50,550],yr=[-50,550],/nodata, $
 xsty=1,ysty=1
for i=0,nstar-1 do $
 for p=0L,np-1,every do $
  plots,ww(p).x(i),ww(p).y(i),psym=3,col=colx(i mod nx)

plotsym,0
for i=0,ng-1 do plots,dat2(i).x,dat2(i).y,/data,psym=8,symsi=4,thick=4,col=col.red; =============================================================


; =============================================================
print,' Does this seem to be right? (y/n) '
true2 = ''
read,true2
case true2 of
 'n': goto,agg
 'y': print,' Good ... '
 else: goto,agg
endcase
; =============================================================


; =============================================================
; Make sure the first entry is the main target star!
; =============================================================
cl = n_elements(dat2)
centr = [256., 256.] ; center of ccd 
dist_all = (centr(0) - dat2.x)^2. + (centr(1) - dat2.y)^2.
w_min = where(dist_all eq min(dist_all),c_min)
w_min = w_min(0)
a2 = findgen(cl)
a3 = a2
a3(0) = w_min
a3(w_min) = a2(0)
dat2 = dat2(a3) ; reshuffle
; =============================================================

; =============================================================

wok = where(strmatch(wireinfo.object,'*' + target + '*') eq  1,cok)

 ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if cok ge 1 then begin ; already found ; November 2006
 print,' *** Similar target found: '
 for pp=0,cok-1 do $
  print,strcompress(pp) + ': ',$
   wireinfo(wok(pp)).object,' -- t0 = '+strcompress(wireinfo(wok(pp)).t0)
 ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 hitme,mess = ' >>> Overwrite (one of) the above? (y/n)',yessir
 if yessir eq 'y' then begin
    if cok ge 2 then begin
      hitme,mess=' >>> Which one (enter number): ',numb
      numb = fix(numb)
      wok = wok(numb) & cok = 1
    endif else begin
      wok = wok(0) & cok = 1 ; overwrite the only one present
    endelse

   print,' %%% Overwriting this position: ',wireinfo(wok)
 endif else begin
   cok = 0
   print,' %%% New slot will be allocated!'
 endelse ; do not overwrite
endif
 ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; =============================================================
if cok eq 0 then begin ; a new object ?
 wunused = where(wireinfo.object eq '',cunused)
 if cunused eq 0 then stop ; no unused slots left ... this should never happen!
 wok = wunused(0) ; pick the first one
 cok = 1 
endif
; =============================================================

if cok eq 1 then begin ; Reset star info!
 xyinfo(wok).xy(*,*) = 0.
 xyinfo(wok).exy(*,*) = 0.
 xyinfo(wok).xy2(*,*) = 0.
 xyinfo(wok).flux(*,*) = 0.
 xyinfo(wok).fwhm(*,*) = 0.
 xyinfo(wok).object(*) = ''
 xyinfo(wok).aperture(*) = 999
endif
; =============================================================


; xyinfo = replicate( {xy:fltarr(2,10), flux:fltarr(10), $
;                       object:strarr(10), aperture:intarr(10)}, xymax)
; wireinfo = replicate( {t0:0D, dir:'', nstars:0, object:''}, xymax )

; ================================================================
aptest = 2 ; start with this aperture ...
ap_again:
w = where(wire2.p(0,aptest) gt 10 and finite(wire2.p(0,aptest)),c)
; ================================================================

; ================================================================
if c lt 100 then begin
 nap = n_elements(wire2(0).p(0,*))
 aptest = aptest + 1
 if aptest lt (nap-1) then goto,ap_again
 print,' %%% Something is wrong with the aperture results ... '
 stop 
endif
; ================================================================

; ================================================================
wireinfo(wok).t0 = round(median(wire2(w).hjd(0)))
t0_init = wireinfo(wok).t0
a = strsplit(sample_wire_file,'/',/extract)
na = n_elements(a)
wireinfo(wok).dir = '/'
for j=0,na-2 do wireinfo(wok).dir = wireinfo(wok).dir + a(j) + '/'
wireinfo(wok).nstars = ng
; ================================================================

; ================================================================
; w9 = where(strmatch(info.object,'*' + target + '*',/fold_case) eq 1,c9)
w9 = where(strmatch(info.object,'*' + target + '*',/fold) eq 1,c9)
if c9 ne 1 then begin
  print,' Unique object not found!!' & help,w9

  for k9=0,c9-1 do $
   print,strcompress(k9)+': ', info(w9(k9)).object
   print,' >>> Which is the right one ? '
  s9 = get_kbrd(1)
  s9 = fix(s9)
  w9 = w9(s9)
  c9 = 1
endif
wireinfo(wok).object = info(w9).object
; ================================================================

; ================================================================
; Now enter unique x,y information
; ================================================================
 xyinfo(wok).object = 'Unknown'
 xyinfo(wok).aperture = 999
for j=0,ng-1 do begin
 xyinfo(wok).xy(0,j) = dat2(j).x
 xyinfo(wok).xy(1,j) = dat2(j).y
 xyinfo(wok).flux(j) = dat2(j).m ; magnitude
 ; if j eq 0 then xyinfo(wok).object(j) = 'Quite possibly this is '+info(w9).object
 xyinfo(wok).aperture(j) = 5 ; default aperture
endfor
; ================================================================


; ===================================================================
; Now choose the best aperture ... the one with the lowest ptp noise!
; ===================================================================

distlim = 5.^2.
np = round(n_elements(wire2) * 1.5) ; reserve enough data points
nap = n_elements(wire2(0).p(0,*)) ; number of apertures

; =============================================================
for j=0,ng-1 do begin ; for each star you entered
 tt = fltarr(np) ; times
 mm = fltarr(nap,np) ; magn.
 xy = fltarr(2,np) ; xy position
 xy2 = xy
 fwhm = fltarr(np)
 c5 = 0L

; =============================================================
 for k=0,nstar-1 do begin ; for each slot ...

  dist = (wire2.x(k) - xyinfo(wok).xy(0,j) )^2. + $
         (wire2.y(k) - xyinfo(wok).xy(1,j) )^2.
  ww = where(dist lt distlim,cc)

  if cc ge 500 then begin
      tt(c5:c5+cc-1) = wire2(ww).hjd(k) - wireinfo(wok).t0
    mm(*,c5:c5+cc-1) = wire2(ww).p(k,*) ; magn. in each aperture
    xy(0,c5:c5+cc-1) = wire2(ww).gc(0,k) + wire2(ww).x(k) ; true x position
    xy(1,c5:c5+cc-1) = wire2(ww).gc(1,k) + wire2(ww).y(k) ; true y position
    xy2(0,c5:c5+cc-1) = wire2(ww).gc(0,k)  ; rel. x position
    xy2(1,c5:c5+cc-1) = wire2(ww).gc(1,k)  ; rel. y position
    fwhm(c5:c5+cc-1) = wire2(ww).fwhm(k)  ; FWHM

    c5 = c5 + cc
  endif 

 endfor
; =============================================================


  if c5 lt 100 then begin
    print, ' Less than 100 points for star: ', dat2(j)
    xyinfo(wok).xy(*,j) = [-999,-999] ; this was a bad star after all 
    xyinfo(wok).flux(j) = -1
    goto, skip_aper
  endif

  tt   = tt(0:c5-1)
  mm   = mm(*,0:c5-1)
  xy   = xy(*,0:c5-1)
  xy2  = xy2(*,0:c5-1)
  fwhm = fwhm(0:c5-1)

  noise = fltarr(2,nap)

    test_ap = 5
    mag = reform( -2.5 * alog10(mm(test_ap,*)) + 25.0 )
    wg = where(mag gt 5 and mag lt 25,cg)

    resistant_mean, mag(wg), 3, me, sd, nr
    rr = robust_sigma(mag(wg))
    ff = findgen(n_elements(mag))

    plot,ff(wg),mag(wg),psym=3,symsi=.1,yr=[-1,1]*rr*6.+me
   
    print,' %%% Click with mouse: Range representative for the data set!'
    print,' %%% ie. at constant flux level. First left then right ... '

    cursor,x1,y1

    wait,1
    print,' %%% Now click right limit ...' 
 
    cursor,x2,y2

    wg2 = where(mag gt 5 and mag lt 25 and ff gt x1 and ff lt x2,cg2)
    oplot,ff(wg2),mag(wg2),psym=6,symsi=.4
  


; =============================================================
  for pp=0,nap-1 do begin ; for each aperture

    mag = reform( -2.5 * alog10(mm(pp,*)) + 25.0 )
    wg = where(mag gt 5 and mag lt 25 and ff gt x1 and ff lt x2,cg2)


   if cg lt 100 then begin ; at least 100 valid points
    print, ' *** Star number ' + strcompress(j) + $
           ' aperture ' + strcompress(k) + ' seems to be bad!'
    goto, skip_aper
   endif

   resistant_mean,mag(wg),3,me_mag,sd,nr
   rr_mag = robust_sigma(mag(wg))
     ; plot,tt,mag,yr=me_mag + rr_mag * 5. * [-1,1],psym=3

   ptp_robust_fin,mag(wg),ptp, 0
   ;if n_elements(wg) gt 2500 then $
   ;  ptp_robust_fin,mag(wg(1000:2000)),ptp, 0 

   print,'Ap = ',pp, ' ... ptp noise: ',ptp, ' ... overall noise: ', rr_mag, $
    format = '(A4, I2, A15, F7.4, A20, F7.4)'

   noise(0,pp) = ptp
   noise(1,pp) = rr_mag

   skip_aper:
  endfor 
; =============================================================
 
   ;;; noise(*,0:1) = 100. ; newer use the smallest aperture

   ; The most sensitive indicator is the over all noise level !!
   low_noise1 = min(noise(0,*))
   low_noise2 = min(noise(1,*))
  
   wmin1 = where(noise(0,*) eq low_noise1, cmin)
   wmin1 = wmin1(cmin-1) ; choose the larger aperture if two have the same noise 

   wmin2 = where(noise(1,*) eq low_noise2, cmin2)
   wmin2 = wmin2(cmin2-1) ; choose the larger aperture if two have the same noise 

   wmin = max([wmin1,wmin2])

; =============================================================
   agg9:
    dev = max(noise)
    plot_io,findgen(nap),noise(0,*),$
     xtit='Ap size',ytit='Overall robust noise level',$
     tit='Star: '+string(j),yr=[0.0001,dev * 1.2],xsty=3,ysty=1
    oplot,findgen(nap),noise(1,*),line=2
    oplot,[1,1.]*wmin,[.0001,dev*1.2],thick=2
    sa = ''
    read,$
     ' Choose aperture (0..'+string(nap-1,format='(I2)')+ '  ... or a = accept): ',sa
    if sa eq 'a' then goto,accept
    wmin = fix(sa)
    goto,agg9
; =============================================================

; =============================================================
    accept:
    print,' Best aperture for star no. ',j,' ... is ',wmin
; =============================================================

  
; =============================================================
   xyinfo(wok).aperture(j) = wmin ; the best aperture ===> lowest noise!

; Redetermine x,y, fwhm, and flux:
   resistant_mean,xy(0,*),  3, me, sd, nr  &   xyinfo(wok).xy(0,j) = me
   resistant_mean,xy(1,*),  3, me, sd, nr  &   xyinfo(wok).xy(1,j) = me
   rx = robust_sigma(xy(0,*)) & xyinfo(wok).exy(0,j) = rx
   ry = robust_sigma(xy(1,*)) & xyinfo(wok).exy(1,j) = ry

; Subpixel position
   resistant_mean,xy2(0,*), 3, me, sd, nr  &   xyinfo(wok).xy2(0,j) = me
   resistant_mean,xy2(1,*), 3, me, sd, nr  &   xyinfo(wok).xy2(1,j) = me

; FWHM & error 
   resistant_mean,fwhm,     3, me, sd, nr  &   xyinfo(wok).fwhm(0,j) = me
   rfwhm = robust_sigma(fwhm) & xyinfo(wok).fwhm(1,j) = rfwhm

; Magnitude = use lc with lowest noise !
   mag = reform( -2.5 * alog10(mm(wmin,*)) + 25.0 )
   wg = where(mag gt 5. and mag lt 25,cg)
   resistant_mean,mag(wg), 3, me_mag, sd, nr
   rr_mag = robust_sigma(mag(wg))
   xyinfo(wok).flux(0,j) = me_mag
   xyinfo(wok).flux(1,j) = rr_mag 

; Update the time zero points ... probably no change here
   print,t0_init, wireinfo(wok).t0
   if j eq 0 then wireinfo(wok).t0 = round( median(tt(wg)) + t0_init )
; =============================================================
  skipstar:
endfor
; =============================================================


; =============================================================
if position_file ne '' then begin
  save,filename=position_file,xyinfo,wireinfo
   print,''
   print,'Saved target information file as: ' + position_file
   print,''
endif
; =============================================================

; =============================================================
g = '/'
gg = strsplit(sample_wire_file,'/',/extract) & ngg = n_elements(gg)
for k=0,ngg-2 do g = g + gg(k) + '/'
basedir = g
h = strsplit(gg(k),'_',/extract) & nh = n_elements(h)
for l=0,nh-2 do g = g + h(l) + '_' 
add=''
if strmatch(sample_wire_file,'*obj1*') eq 1 then add = 'obj1'
if strmatch(sample_wire_file,'*obj2*') eq 1 then add = 'obj2'
g = g + '???.'+add+'.idlr'

spawnrob,'ls -1 ' + g,aa
na = n_elements(aa)
print,' %%% ' + strcompress(na) + ' WIRE files to be reduced!'
if na eq 0 then begin
 print,' *** Something is wrong ... '
 stop
endif
; =============================================================

; =============================================================
; =============================================================
print,''
print,' ============================================================='
print,' Now launch wire_flat (aperture photometry on all WIRE files):'
print,' ============================================================='
print,'  target  = "' + target + '"'
print,'  basedir = "' + basedir + '"'
print,'  spawnrob,"ls -1 '+g+'",files'
print,'  wire_flat,files,1,$'
print,'            "'+position_file+'","'+target+'","",0'
print,''
; =============================================================

END
