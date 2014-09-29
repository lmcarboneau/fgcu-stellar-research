; Compare evolution tracks from Tine Bjorn / JCD and Lejeune

dops = 1B

; Tine B:
metal2_on = 0B
metal06_on = 1B

; Lejeune:
lej_metal2_on = 0B
lej_metal08_on = 1B

easyps, keywords, kk, dops=dops, /prepare, dim = [15,15,-1,1], $
 fil = 'bstars_evoltr.ps', dir = '/home/bruntt/papers/wire/bstars/'
if dops then col=getcolor(/load)

xxx = [4.7,3.6] & yyy = [-.8,5] ; nice fig, all stars
;xxx = [4.7,3.6] & yyy = [2,5] ; nice fig, mass above 3 Msun
 xxx = [4.2,3.6] & yyy = [-.8,2] ; nice fig, masses below 3 Msun

 plot,$
  [0,1],/nodata,xr=xxx,yr=yyy,xtit='!17log !3T!Ieff!N',ytit='!17log !3L/L!ISun!N',$
  psym=8,xsty=1,ysty=1,xthick=2,ythick=2,charsi=1.5,charthick=2,thick=2, $
  tit='Evol. tracks: Tine vs. Lejeune (blue)'

; Metal content:
; Lejeune has Z = 0.008
; Tine B has  Z = 0.06, or [Fe/H] = -0.52



x = 22 & xx = [4.7,3.5] & yy = [-.5,6]
   xo = 0 & aa = 1.0  ; xo = x & aa = 0.0
   offtxt_y = -0.1

if lej_metal08_on then begin
import_lejeune, '/home/bruntt/evol/lejeune/modc008.dat', l ; Z=0.008

  for i=5,n_elements(l)-1  do oplot, l(i).teff(0:x), l(i).l(0:x),psym=-6,symsi=.3,col=col.red

  for i=5,n_elements(l)-1 do $
    if (l(i).l(xo) + offtxt_y) lt max(yyy) and (l(i).l(xo) + offtxt_y) gt min(yyy) then $
    xyouts,l(i).teff(xo), l(i).l(xo) + offtxt_y,$
     strcompress(string(l(i).m,format='(F9.1)'),/remove_all), align=aa, charsi=.8,col=col.red
endif

if lej_metal2_on then begin
   import_lejeune, '/home/bruntt/evol/lejeune/modc020.dat', ls ; SOLAR ABUND

   for i=5,n_elements(ls)-1  do oplot, ls(i).teff(0:x), ls(i).l(0:x),psym=-6,symsi=.3,col=col.red
   
   for i=5,n_elements(ls)-1 do $
    if (ls(i).l(xo) + offtxt_y) lt max(yyy) and (ls(i).l(xo) + offtxt_y) gt min(yyy) then $
    xyouts,ls(i).teff(xo), ls(i).l(xo) + offtxt_y,$
     strcompress(string(ls(i).m,format='(F9.2)'),/remove_all), align=aa, charsi=.8,col=col.red
endif


if metal06_on then begin
   spawnrob,'ls -1 ~/evol/tineb/*metal06.txt',tb ; Fe/H = -0.52, or Z=0.006
   ntb = n_elements(tb)

; Special case for the same metal ab. as lejeune
   spec = read_evol('~/evol/tineb/mass0200_metal08.txt')
    oplot,alog10(spec.teff),alog10(spec.lum),symsi=.2, col=col.cyan 
   spec2 = read_evol('~/evol/tineb/extra/mass0200_metal2.txt')
    oplot,alog10(spec2.teff),alog10(spec2.lum),symsi=.2, col=col.magenta
    
   for i=0,ntb-1 do begin
    fil = tb(i)
    a = read_evol(fil)
   
    x = strsplit(fil,'_',/extract)
    y = strsplit(x(0),'s',/extract)
    mass = float(y(1)) / 100.
   
    oplot,alog10(a.teff),alog10(a.lum),symsi=.2, col=col.sky  ;  ,psym=-7

    if ( alog10(a.lum(0)) lt max(yyy) ) and ( (alog10(a.lum(0))-yoffx) gt min(yyy) ) then begin
    arrow,alog10(a.teff(0))+0.05, alog10(a.lum(0))-yoffx, $
          alog10(a.teff(0)),      alog10(a.lum(0)),$
          col=col.sky,thick=2,/data

    xyouts,alog10(a.teff(0))+0.05,alog10(a.lum(0))-yoffx,$
     strcompress(string(mass,format='(F9.2)'),/remove_all),col=col.sky, $
     alignment=1.,charsi=0.8
    endif
   
   endfor
endif

yoffx = 0.15

if metal2_on then begin
   spawnrob,'ls -1 ~/evol/tineb/*metal2.txt',tb ; solar abundance
   ntb = n_elements(tb)
   
   for i=0,ntb-1 do begin
    fil = tb(i)
    a = read_evol(fil)
   
    x = strsplit(fil,'_',/extract)
    y = strsplit(x(0),'s',/extract)
    mass = float(y(1)) / 100.
   
    oplot,alog10(a.teff),alog10(a.lum), symsi=.2, col=col.sky ; ,psym=-7

    if ( alog10(a.lum(0)) lt max(yyy) ) and ( (alog10(a.lum(0))-yoffx) gt min(yyy) ) then begin
    arrow,alog10(a.teff(0))+0.05, alog10(a.lum(0))-yoffx, $
          alog10(a.teff(0)),      alog10(a.lum(0)),$
          col=col.sky,thick=2,/data

    xyouts,alog10(a.teff(0))+0.05,alog10(a.lum(0))-yoffx,$
     strcompress(string(mass,format='(F9.2)'),/remove_all),col=col.sky, $
     alignment=1.,charsi=0.8
    endif
   
   endfor
endif


easyps, keywords, kk, dops=dops, /close

if n_elements(keywords) ge 1 then $
 if keywords.color then col=getcolor(/load)


END
