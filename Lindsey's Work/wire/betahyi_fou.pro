if n_elements(fwire) eq 0 then $
  readcol,'~/papers/conferences/Vienna2006/talk/bh.wire.ft',ff,aa

fwire = 178.53730
w = where(abs((ff*1e6 mod (15.42*1e6/86400))-fwire) lt (3.0) or $
          abs((ff*1e6 mod (15.42*1e6/86400))-fwire) gt (fwire-3.0),c)


names = ['white','black','red','green','blue','cyan','magenta','yellow','lightred', $
         'darkred','lightgreen','darkgreen','lightblue','darkblue','grey','azure']
R=byte([255,0,255,  0,127,  0,255,255,255,220,127,  0,177,  0,127,240])
G=byte([255,0,  0,200,127,255,  0,255,127,  0,255,160,177,  0,127,255])
B=byte([255,0,  0,  0,255,255,255,  0,127,  0,127,  0,255,127,127,255])
TVLCT,R,G,B
white=0 & black=1 & red=2 & green=3 & blue=4 & cyan=5 & magenta=6 & yellow=7 & lightred=8
darkred=9 & lightgreen=10 & darkgreen=11 & lightblue=12 & darkblue=13 & grey=14 & azure=15
!p.color = black

; COLOURS:
bordercol = white
textcol = yellow
plotcol = azure

; Thickness of fit
fitt = 8.

A = findgen(20) * (!pi * 2.0 / 16.0)
BLANK = replicate(' ',30)

outfile = '~/papers/conferences/Vienna2006/talk/betahyi.eps'

set_plot, 'ps'
device, filename=outfile, $
 /encap, xsize=18, ysize=12, /color  ; , /cmyk

XR1  = 0.000 + 0.013*[-1.0,1.0]
XR2  = 0.821 + 0.013*[-1.0,1.0]
XR12 = [0,3000]
YR1  = [0,100]
XT   = 'Orbital phase'
XT1  = '!6Frequency [!4l!6Hz]'
YT   = 'Amplitude [ppm]'
TIT1 = 'Primary eclipse'
TIT2 = 'Secondary eclipse'
; TIT  = 'WIRE satellite light curve of the eclipsing binary star !7w!X Centauri'
TIT  = '!7b!X Hydri'
XM   = [6.5,0.5]
YM   = [3.5,2]
SYM  = 0.1
CHAR = 0.8

POS12= [0.08,0.57, 0.99,0.95]                                      ; (xlow,ylow,xhigh,yhigh)
POS3 = [0.08,0.08, 0.52,0.46]  &  POS4 = [0.55,0.08,0.99,0.46]

if (pos3(1)-pos3(3) ne pos12(1)-pos12(3)) or (pos4(1)-pos4(3) ne pos12(1)-pos12(3)) or $
   (pos4(0)-pos4(2) ne pos3(0)-pos3(2)) then stop

usersym, SYM*cos(A), SYM*sin(A), /fill

!P.charthick=2
!P.thick=2
!x.thick = 3
!y.thick = 3

; www.dfanning.com:
; There is only one way to get a background color different from white
; in a PostScript file,  and that is essentially to draw an image of a 
; single color in the PostScript window. I like to use POLYFILL for
; this purpose, like this:

   POLYFILL, [1,1,0,0,1], [1,0,0,1,1], /NORMAL, COLOR=black

plot, [0], /nodata, xr=XR12, yr=YR1, ytit=YT, tit=TIT, charsize=CHAR,$ ;  position=POS12,     $
  color=textcol, /noerase, xtit=XT1, xtickformat='(i5)',xsty=1,ysty=1, yticklen=0.004

polyfill, [xr12[0],xr12[1],xr12[1],xr12[0],xr12[0]], $
 [yr1[0],yr1[0],yr1[1],yr1[1],yr1[0]], color=lightblue

plot, [0], /nodata, xr=XR12, yr=YR1, charsize=CHAR, $ ; position=POS12,                       $
  xtickn=BLANK, ytickn=BLANK, /noerase, color=bordercol,xsty=1,ysty=1, yticklen=0.01

df = [ff(w(1:c-1)) - ff(w(0:c-2)), 0.]
w2 = where(df gt 1.5e-4,c2)

; Convert to milliHz & ppm 
oplot,ff   *1e6,sqrt(aa   /1.086)*1e6,col=plotcol

for k=0,c2-2 do $
 oplot,ff(w(w2(k)+1:w2(k+1)))*1e6,sqrt(aa(w(w2(k)+1:w2(k+1)))/1.086)*1e6,col=red




device, /close
set_plot, 'x'
; spawn, 'kghostview lcplot3.eps &'

print,' $  kghostview ' + outfile + ' &' 


end






