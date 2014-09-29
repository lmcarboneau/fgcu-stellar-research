PRO wire_angle, wirefile, target, position_file, option1, option2, err_lim, wireult

; Example:
; wire_angle, '/data2/bruntt/wire/dat/SigmaSgr/SigmaSgr_merged_allslots_31.idl', $
;  'SigmaSgr', '/data1/bruntt/wire/xy_positions2.idl', 1, 250, 0.1, 0

; wire_angle,'/data2/bruntt/wire/dat/ProcyonF5IV-V/data/data_merged_allslots_31.idl',$
;  'ProcyonF5IV-V', '/data1/bruntt/wire/xy_positions2.idl', 1, 250, 0.1, 0

;if n_elements(wireult) eq 0 then $
; restore,'/data2/bruntt/wire/dat/SigmaSgr/SigmaSgr_merged_allslots_31.idl'
; target = 'SigmaSgr'

;if n_elements(wireult) eq 0 then $
; restore,'/data2/bruntt/wire/dat/AltairA7V/data/data_merged_allslots_31.idl'
;target = 'AltairA7V'

restore,wirefile
if n_elements(wireult.hjd) lt 10 then begin
 print,' %%% Restore file not imported correctly: '+wirefile
 RETURN
endif

a = sort(wireult.hjd)
wireult = wireult(a)

; position_file = '/data1/bruntt/wire/xy_positions2.idl'

; If position_file is given as input, use only the best aperture!
 restore,position_file
 if n_elements(wireinfo) eq 0 then begin
   print,' %%% Positon file not found: '+position_file 
   RETURN
 endif 
 winf = where(strmatch(wireinfo.object,'*' + target + '*') eq 1,cok)

 if cok ne 1 then begin
  print,' %%% Star not found in xy_positions.idl structure!'
  RETURN
 endif

 nstars = wireinfo(winf).nstars
 magn     = reform( xyinfo(winf).flux(0,0:nstars-1) )
 magn_err = reform( xyinfo(winf).flux(1,0:nstars-1) )

 print,' %%% Magn. errors: ' , magn_err

 t0 = wireinfo(winf).t0
 xy    = xyinfo(winf).xy
 apuse = xyinfo(winf).aperture
 wuse = where(xy(0,*) gt 5. and  magn_err lt err_lim,nstars)
 xy = xy(*,wuse)
 apuse = apuse(wuse)

 refstar = 0

 if nstars le 2 then begin
  print,' I need at least 3 stars to do this ...'
  stop
 endif

 distcen = fltarr(nstars)
 angles  = fltarr(nstars - 2)
 xyall   = fltarr(2,nstars)

 nmerge = 100
 cn = 0L
 nmax = n_elements(wireult.x(refstar)) ; number of data points

 ttref = wireult.hjd
  nref = n_elements(ttref)

 rinfo = fltarr(nstars,4,nref) ; dist to cent. and angle
 progress = round(0.1 * nref)

 print,''

plot,[0,1],/nodata,xr=[0,511],yr=[0,511],xsty=1,ysty=1,title='WIRE Positions'

uu = findgen(nstars)
uu(*) = 1

for star=0,nstars-1 do begin
 print,'Star '+string(star,format='(I2)')+': ',format='(A15,$)'
 
 ; Select good data in this time interval
 wg = where(wireult.mag(wuse(star))  gt  5. and $
            wireult.mag(wuse(star))  lt 25. and $
            wireult.fwhm(wuse(star)) gt .3  and $
            wireult.hjd        gt 4e4 and $
            wireult.hjd        lt 7e4,cg)

 if cg ge 3 then begin
  rinfo(star,0,0:cg-1) = wireult(wg).x(wuse(star))
  rinfo(star,2,0:cg-1) = wireult(wg).y(wuse(star))
 endif

 rstar = [5,20,20,20,20,20,20]
 plot,rinfo(star,0,*),rinfo(star,2,*),psym=3  ,$
  xr=xy(0,star)+[-1,1]*rstar(star),yr=xy(1,star)+[-1,1]*rstar(star)
  print,'Hit...' & s = get_kbrd(1)
  if s eq 'x' then uu(star) = -1

endfor

print,nstars
wuse2 = where(uu ge 0.5,nstars)
rinfo = rinfo(wuse2, *, *)
wuse = wuse(wuse2)
print,' Stars that are left: ',nstars


; Compute mean angles btw. the stars
angles = fltarr(nstars)
conv = (180./!PI)
distorigo = fltarr(nstars)

yshift = findgen(nref) * 2L ; all even numbers
yshuse = fltarr(nref * 2)

print,''
print,' %%% Measured angles in degrees:'
for star=1,nstars-1 do begin

wg = where(rinfo(star,0,*) gt 5. and rinfo(refstar,0,*) gt 5.,cg)
dx = reform( rinfo(star,0,wg) - rinfo(refstar,0,wg) )
dy = reform( rinfo(star,2,wg) - rinfo(refstar,2,wg) )
distcen = reform(sqrt(dx^2. + dy^2.))
ang = atan(dy / dx)
ysh = yshift(wg)

wgc = where(distcen gt 3. and $
            abs(dx) gt 1. and abs(dx) lt 510. and $
            abs(dy) gt 1. and abs(dy) lt 510.,cgc)
if cgc lt 25 then stop
distorigo(star) = median(distcen(wgc))

 mx = median(dx(wgc))
 my = median(dy(wgc))

 if mx ge 0. and my ge 0. then angl = atan( my/mx)
 if mx lt 0. and my ge 0. then angl = atan( my/mx) + !PI
 if mx lt 0. and my lt 0. then angl = atan( my/mx) + !PI
 if mx gt 0. and my lt 0. then angl = atan( my/mx) + 2. * !PI

if star eq 1 then begin
 angles(star) = angl
endif else begin
 angles(star) = angl  ; - angles(1)
endelse

 print,star,median(dx),median(dy),median(distcen),$
  median(ang) * conv,angles(star)*conv

endfor

plot,xy(0,*),xy(1,*),psym=2,xr=[0,511],yr=[0,511],xsty=1,ysty=1,tit='Location of stars'
plots,!x.crange,0,line=2 & plots,0.,!y.crange,line=2
for i=0,nstars-1 do begin
 xyouts,xy(0,i),xy(1,i),/data, $
  'S'+string(i,format='(I2)') + ' A=' + $
  string(angles(i)*conv,format='(F5.1)') + $
  ' d=' + string(distorigo(i),format='(F5.1)'),$  
  alignment=0.5,charsi=1.5
 oplot,[xy(0,0),xy(0,i)],[xy(1,0),xy(1,i)]
endfor

n_equations = (nstars-1) * 2
n_unknowns = 2

x = fltarr( n_equations )
y = fltarr( n_unknowns, n_equations )
; Eg. for five stars, four of them have angle-information:
;     There are two unknowns (sintheta and costheta) and 
;     2 * (5-1) = 8 known equations.

angles2 = angles
; angles2(1) = 0. ; reference rotation star is entry 2

ang = fltarr(2,nref)
ang(1,*) = 999.

for solv = 0L,nref-1 do begin ; for each set of images

xypos = reform(rinfo(*,[0,2],solv))

if xypos(0,refstar) lt 3. or xypos(1,refstar) lt 3. then begin
 ; print, ' %%%% Invalid x,y position of reference star ... ',solv
 goto,skip_this_time 
endif
xc = reform(xypos(refstar,*))

; The central star -- main target -- the approximate center of rotation:
cnt = 0
for j=1,nstars-1 do begin

if xypos(j,0) gt 5. then begin
; x(cnt)   = xypos(j,0) - xc(0)                   ; known value of x position
; x(cnt+1) = xypos(j,1) - xc(1)                   ; known value of y position
 x(cnt)   = xypos(j,0) - xypos(0,0)                   ; known value of x position
 x(cnt+1) = xypos(j,1) - xypos(0,1)                   ; known value of y position

 y(0,cnt) = cos(angles2(j)) * distorigo(j)           ; equation for x position
 y(1,cnt) = -1.0 * sin(angles2(j)) * distorigo(j)    ; equation for x position
 y(0,cnt+1) = sin(angles2(j)) * distorigo(j)         ; equation for y position 
 y(1,cnt+1) = cos(angles2(j)) * distorigo(j)         ; equation for y position

; y(0,cnt) = cos(angles2(j)) * distorigo(j)           ; equation for x position
; y(1,cnt) = sin(angles2(j)) * distorigo(j)    ; equation for x position
; y(0,cnt+1) = sin(angles2(j)) * distorigo(j)         ; equation for y position 
; y(1,cnt+1) = -1.0 * cos(angles2(j)) * distorigo(j)         ; equation for y position

 cnt = cnt + 2
endif
endfor

 a = robust_regress(y,x,yfit,sig)    
 ; print,acos(a(1))*conv , asin(a(2)) *conv

 ang(0,solv) = ttref(solv)
 ang(1,solv) = avg([ [acos(a(1))], [asin(a(2))] ] )


skip_this_time:
endfor ; next image

w = where(abs(ang(0,*)-t0) lt 200. and abs(ang(1,*)) lt 450.,c)
x = reform(ang(0,w)-t0)
y = reform(ang(1,w) * conv)

; Sub divide the angle information into 'np' equally wide angle regions 
np = option2
minx = min(x(w)) - 0.1
maxx = max(x(w)) + 0.1
step = (maxx - minx) / (np-1)
xx = minx + findgen(np) * step

if abs(maxx - minx) lt 2.0 then begin
  print,' Range in angle too small: ',abs(maxx - minn)
  stop
endif


yy = fltarr(2,np)
yy(*,*) = 1999. ; assume all angles are bad!

for i=0,np-1 do begin
 ww = where(abs(x - xx(i)) lt step,cww)
 if cww ge 30. then begin
  resistant_mean,y(ww),3,me,sd,nr
  yy(0,i) = me
  yy(1,i) = robust_sigma(y(ww))
 endif
endfor

ss = where(abs(yy(0,*)) lt 450.,css)

gg = robust_poly_fit(xx(ss),yy(0,ss),2,yfit,sig)

col = getcolor(/load)
plotsym,0,/fill
plot,x,y,psym=3,xtit='HJD -' + string(t0,format='(F7.1)'),ytit='Roll Angle [degrees]',$
 max_value = 500.,xsty=1,ysty=1,charsi=2
print,'t0 = ',t0
oplot,xx(ss),yy(0,ss),psym=8,symsi=2,col=col.red
for i=0,css-1 do oplot,xx(ss(i))*[1,1.],yy(0,ss(i))+[-1,1]*yy(1,ss(i)),thick=2,col=col.red
oplot,xx(ss),yfit,line=2,thick=3

; The smoothed angle approach:
if option1 eq 0 then begin
 ang_all = gg(0) + gg(1) * (wireult.hjd - t0) + gg(2) * (wireult.hjd - t0)^2.0 
endif

; The fine angle steps approach
if option1 eq 1 then begin
 times = wireult.hjd - t0
 ang_all = spline(xx(ss),yy(0,ss),times)
endif

 ang_all = ang_all - median(ang_all)

for i=0,nstars-1 do wireult.angle(wuse(i)) = (ang_all/conv) * distorigo(i) 

outfile = wirefile + '_ang' + string(option1,format='(I1)') + $
 strcompress(string(option2,format='(I7)'),/remove_all)

save,filename=outfile,wireult,ang_all,conv,distorigo,angles
print,' %%% Exported file: '+outfile

; plot,wireult.angle(1),wireult.mag(1),psym=3,ysty=3,min_value = 10 ; ,xr=[1,2]

;w = where(wireult.back(1) lt 35.,c)
; plot,wireult2(w).angle(1),wireult2(w).mag(1),psym=3,col=col.sky,ysty=3,min_value=10,xr=[1,2]
; oplot,wireult(w).angle(1),wireult(w).mag(1),psym=3,col=col.red

END
