; In the spring of 2005 I discovered a problem
; with the WIRE times. I compare the eclipses
; seen in HIPPARCOS data with the eclipses seen
; in the WIRE data

; Hipparcos data for lam sco, Feb 1999:
fi = '/home/bruntt/papers/wire/lambdasco/hipparcos_HD158926.dat'         
readcol,fi,t,d,d2,f,format='d,f,f,f'
readcol,'/home/bruntt/wire/wire_lc/smooth/lamsco_noosc_f18_merged.dat',tw,dw,ww,format='d,f,f'

readcol,'/home/bruntt/wire/bstars/lambdasco/lamsco_smei.dat',ts,ds,format='d,f'

tw = tw - 1.5D ; Alan Penny / Willy Torres correction is -1.5 days to be added to WIRE times

p0 = 5.95175D ; this is the optimal solution!
p0init = p0
p0step = 0.00005D


; K. Uytterhoeven's dissertation:
; p0 = 5.95254D ; (4) ; error on the last digit

; ===========
p0 = p0 - 3D * p0step

plotsym,0,/fill

t00 = 7927D
t = t - t00
wc = where(d lt 1.55,c)
d = d - median(d(wc))

t0_hipp = 2440000D + t00 ; Observations started: Feb 4th 1990
t0_hipp_init = t0_hipp
t0_wire = 2453086D       ; 21st of March 2004 ; 14.1 years later
t0_wire = t0_wire - t0_hipp 

ts2 = ts + 2452000D + 0.5 ; convert MJD to JDs
ts3 = ts2 - t0_hipp
ds3 = ds - median(ds)

t0_hipp = 0D

e1 = where(tw lt 100) ; first run
e2 = where(tw gt 100) ; second run

plot,tw+t0_wire,dw,psym=1,xr=[5137,5170],yr=[.05,-.01]
oplot,ts3,ds3,psym=7,col=col.sky

plot,tw+t0_wire,dw,psym=1,xr=[5137,5170],yr=[.05,-.01]
oplot,ts3,ds3,psym=7,col=col.sky

period = 5.95175D
phase_w = ((tw+t0_wire) mod period) / period
phase_s = ((ts3) mod period) / period
plot,phase_w, dw,psym=1,yr=[.05,-.01],/nodata,xr=[0,1] ; xr=[0,.3]
 oplot,phase_w(e1),dw(e1),psym=1,col=col.green
 oplot,phase_w(e2),dw(e2),psym=1,col=col.red
 oplot,phase_s,ds3,psym=7,col=col.sky
print,' %%% Green: Wire ep1, Red: WIRE ep2, Blue: SMEI'
hitme,s99 & if s99 eq 'x' then stop

plot,t+t0_hipp,d-median(d),xr=[0,5200],yr=[-1,1]*0.04,psym=4
oplot,tw+t0_wire,dw,psym=1
oplot,ts3,ds3,psym=1,col=col.sky



!P.multi=0
n6791pos,2,6,0.07,0,0,pos
n6791pos,1,6,0.07,0,0,pos


xtt = [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
window,1,ysize=1200,xsize=1300

for i=0,5 do begin
  period = p0 + i * p0step
  phw = ((tw+t0_wire) mod period)/period & w = where(phw lt 0.,c) 
  if c ge 1 then phw(w) = phw(w) + 1.
  phh =  ((t) mod period)/period & w = where(phh lt 0.,c) & if c ge 1 then phh(w) = phh(w) + 1.
  
  plot,phw,dw,psym=1,yr=[0.07,-0.03],/noerase,position=pos(*,i),/nodata,xr=[0,1],xtickn=xtt
  oplot,phw(e1),dw(e1),psym=1,col=col.sky & oplot,phw(e2),dw(e2),psym=1,col=col.magenta
  oplot,phh,d,psym=8,col=col.green
  xyouts,.05,.04,'Period = ' + strcompress(string(period,format='(D9.5)'),/remove_all) + ' d'
endfor

goto,skipsec
for i=0,5 do begin
   period = p0 + i * p0step
   phw = ((tw+t0_wire) mod period)/period
   w = where(phw lt 0.,c) & if c ge 1 then phw(w) = phw(w) + 1.
   phh =  ((t) mod period)/period 
   w = where(phh lt 0.,c) & if c ge 1 then phh(w) = phh(w) + 1.

   plot,phw,dw,psym=1,yr=[0.03,-0.03],/noerase,position=pos(*,i+6),/nodata,xr=[.8,1.],xtickn=xtt
   oplot,phw(e1),dw(e1),psym=1,col=col.sky & oplot,phw(e2),dw(e2),psym=1,col=col.magenta
   oplot,phh,d,psym=8,col=col.green
   xyouts,.65,.02,'Period = ' + strcompress(string(period,format='(D9.5)'),/remove_all) + ' d'
endfor
skipsec:

; ==============
easyps, keywords, kk, dops=dops, /prepare, dim = [15,8.0,-1,1], $
 fil = 'lamsco_smei_wire_hipp.ps', dir = '/home/bruntt/wire/bstars/lambdasco/'

col=getcolor(/load)

; window,0,xsize=900,ysize=600
period = p0init

phw = ((tw+t0_wire) mod period)/period
w = where(phw lt 0.,c) & if c ge 1 then phw(w) = phw(w) + 1.
phh =  ((t) mod period)/period
w = where(phh lt 0.,c) & if c ge 1 then phh(w) = phh(w) + 1.

plot,phw,dw,psym=1,yr=[0.07,-0.03],/nodata,xr=[0,1],xtit='Phase',ytit='Delta Mag.'
oplot,phw(e1),dw(e1),psym=7,col=col.sky,symsi=.5 ; WIRE 1
oplot,phw(e2),dw(e2),psym=1,col=col.magenta,symsi=.5 ; WIRE2

oplot,phh,d,psym=8,col=col.green,symsi=.5
oplot,phase_s,ds3,psym=7,col=col.red,symsi=0.5


easyps, keywords, kk, dops=dops, /close


easyps, keywords, kk, dops=dops, /prepare, dim = [15,12.0,-1,1], $
 fil = 'lamsco_wire_only.ps', dir = '/home/bruntt/wire/bstars/lambdasco/'
col=getcolor(/load)

;!P.multi=[0,1,2]

plot,phw,dw,psym=1,yr=[0.045,-0.01],/nodata,xr=[0.05,.18],xtit='Phase',ytit='Delta Mag.'
oplot,phw(e1),dw(e1),psym=7,col=col.sky,symsi=.5 ; WIRE 1
oplot,phw(e2),dw(e2),psym=1,col=col.magenta,symsi=.5 ; WIRE2
 oplot,phw(e2)+0.0008,dw(e2),psym=1,col=col.yellow,symsi=.5 ; WIRE2, apsidal motion?
; oplot,phw(e2)+0.0032,dw(e2),psym=1,col=col.green,symsi=.5 ; SMEI = 1aar
;oplot,phase_s,ds3,psym=7,col=col.red,symsi=0.5

;plot,phw,dw,psym=1,yr=[0.085,-0.03],/nodata,xr=[0.05,.18],xtit='Phase',ytit='Delta Mag.'
;oplot,phw(e1),dw(e1),psym=7,col=col.sky,symsi=.5 ; WIRE 1
;oplot,phw(e2),dw(e2),psym=1,col=col.magenta,symsi=.5 ; WIRE2
;oplot,phase_s,ds3,psym=7,col=col.red,symsi=0.5 ; SMEI ... too low signal to see aps. motion


; Sec. eclipse
;plot,phw,dw,psym=1,yr=[0.015,-0.005],/nodata,xr=[0.05,.18]+.56,xtit='Phase',ytit='Delta Mag.'
;oplot,phw(e1),dw(e1),psym=7,col=col.sky,symsi=.5 ; WIRE 1
;oplot,phw(e2),dw(e2),psym=1,col=col.magenta,symsi=.5 ; WIRE2
;oplot,phw(e2)+0.0008,dw(e2),psym=1,col=col.yellow,symsi=.5 ; WIRE2, apsidal motion?

;!P.multi=0

easyps, keywords, kk, dops=dops, /close



outfile = '/home/bruntt/wire/bstars/lambdasco/lamsco_wire_smei_hipp.dat'
openw,1, outfile

printf,1,'LambdaSco data from HIPP (flag in column 4 is "0"), WIRE(1) and SMEI(2)'
printf,1,'Column 1: Times are HJD - ' + string(t0_hipp_init,format='(D10.1)')
printf,1,'Column 2: Change in magnitudes - oscillations subtracted from WIRE data'
printf,1,'Column 3: Phases computed for the period ' + string(p0init,format='(D10.6)') + ' days'

for i=0,n_elements(t)-1 do $
 printf,1,t(i), phh(i), d(i), 0, format='(D15.5,D15.6,F9.4, I3)' ; HIPP
for i=0,n_elements(tw)-1 do $
 printf,1,tw(i)+t0_wire, phw(i), dw(i), 1, format='(D15.6,D15.6,F9.5,I3)' ; WIRE
for i=0,n_elements(ts3)-1 do $
 printf,1,ts3(i),phase_s(i),ds3(i),2,format='(D15.3,D15.6,F9.5,I3)' ; SMEI
close,1

print,' %%% Saved complete data file: ' + outfile

; -----------
END


