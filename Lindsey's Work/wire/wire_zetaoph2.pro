; Spectral window

; base = '/export/brixx1/bruntt/wire_analysis/bstars/zetaoph/lc/' ; Bruntt
 base = '/import/brixx1/bruntt/wire_analysis/bstars/zetaoph/lc/' ; Rahmi

create_lc = 0B ; only need to do this once, then import to p04.

; Create artificial signal at 5.18 c/day
fc = 5.18 
phase = 0.

if create_lc then begin

readcol,'ZetaOph_most_2004.dat',t,d,format='d,d'
d = sin(2D * !DPI * fc * t + phase)
openw,1,'ZetaOph_most_2004_art.dat'
for i=0L,n_elements(t)-1 do printf,1,t(i),d(i),format='(D16.5,D16.5)'
close,1

readcol,'ZetaOph_wire_2004.dat',t,d,format='d,d'
d = sin(2D * !DPI * fc * t + phase)
openw,1,'ZetaOph_wire_2004_art.dat'
for i=0L,n_elements(t)-1 do printf,1,t(i),d(i),format='(D16.5,D16.5)'
close,1

readcol,'ZetaOph_wire_2005.dat',t,d,format='d,d'
d = sin(2D * !DPI * fc * t + phase)
openw,1,'ZetaOph_wire_2005_art.dat'
for i=0L,n_elements(t)-1 do printf,1,t(i),d(i),format='(D16.5,D16.5)'
close,1

endif

if n_elements(ww05) eq 0 then begin
 readcol,base + 'art_most2004.fou',fm04,wm04
 readcol,base + 'art_wire2004.fou',fw04,ww04
 readcol,base + 'art_wire2005.fou',fw05,ww05
; readcol,base + 'window_most2004.fou',fm04,wm04
; readcol,base + 'window_wire2004.fou',fw04,ww04
; readcol,base + 'window_wire2005.fou',fw05,ww05
endif

getpos,1,3,0,0,0,pos

fc = 0.
clear = replicate(' ',10)
x1 = 13
clear2 = ['',' ','',' ','',' ','',' ','',' ']

px1 = 25

!P.charsize=1.2
!x.thick=2
!y.thick=2
!P.charthick=2

easyps, keywords, kk, dops=dops, /prepare, dim = [15,10.0,-1,1], $
 fil = 'zeta_window.ps', dir = '~/papers/wire/zetaoph/'

plot,fw04+fc,ww04,position=pos(*,0),xr=[0,px1],yr=[0,1.05],xtickname=clear,$
 ytickname=clear
oplot,-fw04+fc,ww04
xyouts,x1,.9,'WIRE Feb 2004'

plot,fm04+fc,wm04,position=pos(*,1),xr=[0,px1],yr=[0,1.05],/noerase,xtickname=clear,$
 ytit='Amplitude',ytickname=clear
oplot,-fm04+fc,wm04
xyouts,x1,.9,'MOST May 2004'

plot,fw05+fc,ww05,position=pos(*,2),xr=[0,px1],yr=[0,1.05],/noerase,xtit='Frequency [c/day]';,$
; ytickname=clear2
oplot,-fw05+fc,ww05
xyouts,x1,.9,'WIRE Sep 2005'

easyps, keywords, kk, dops=dops, /close

END

