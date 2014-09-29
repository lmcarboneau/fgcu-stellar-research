; Feb 2007: plot Procyon RVC from AAT

if n_elements(t) eq 0 then readcol,'/export/brixx2/bruntt/procyon/att/hd61421.ascii',t,d,e,flux,format='d,f,f,f'

easyps, keywords, kk, dops=dops, /prepare, dim = [15,24.0,-1,1], $
 fil = 'att_rvc.ps', dir = '/export/brixx2/bruntt/procyon/att/'
!P.thick=2
col=getcolor(/load)

use = [0,4,5,6,7,8,9,10]
nuse = n_elements(use)
clear = replicate(' ',10)
clear2 = [' ','','','',' ']
ssym=8 & yy = 14. & plotsym,0,/fill
yrr = 20

tplot = t - 4110.
toff = -9.9

!P.charsize=1.5

getpos,1,nuse,0,0,0,pos,ppx1=0.17
 i=0 
 plot,tplot,d,psym=ssym,symsi=.2,xr=toff+[-1,1]*.2+use(i),xtickname=clear,position=pos(*,i),yr=[-1,1]*yrr,/nodata,ytickname=clear2
 oplot,tplot,d,col=col.sky
 oplot,tplot,d,psym=ssym,symsi=.2
 xyouts,toff-.2*.95+use(i),yy,string(2454100 + use(i),format='(D8.0)'),charsi=1
for i=1,nuse-2 do begin
 plot,tplot,d,symsi=.2,xr=toff+[-1,1]*.2+use(i),xtickname=clear,/noerase,position=pos(*,i),yr=[-1,1]*yrr,/nodata,ytickname=clear2
 oplot,tplot,d,col=col.sky
 oplot,tplot,d,psym=ssym,symsi=.2
 plots,!x.crange,0.,line=1,col=col.red
 xyouts,toff-.2*.95+use(i),yy,string(2454100 + use(i),format='(D8.0)'),charsi=1
endfor
 plot,tplot,d,psym=ssym,symsi=.2,xr=toff+[-1,1]*.2+use(i),/noerase,position=pos(*,i),yr=[-1,1]*yrr,/nodata,$
 xtit='Time [JD]',ytit='Rad. vel. [m/s]',ytickname=clear2
 oplot,tplot,d,col=col.sky
 oplot,tplot,d,psym=ssym,symsi=.2
 xyouts,toff-.2*.95+use(i),yy,string(2454100 + use(i),format='(D8.0)'),charsi=1

easyps, keywords, kk, dops=dops, /close

wei = 1./e^2.
wei = wei / total(wei)
plot,d-smooth(d,30,/edge),wei,psym=1

n = n_elements(d)
dt = t(1:n-1) - t(0:n-2)
w = where(dt lt 0.0015,c)
tstep = median(dt(w))
print,' %%% Avg. time btw. exposures (secs.): ',tstep * 86400.

smfac = 90. ; about 60 minutes cut off
col=getcolor(/load)
plot,t,d-smooth(d,smfac,/edge),xr=4106.1+[-1,1]*.05,psym=-1,symsi=.2
oplot,t,d,psym=-1,symsi=.2,col=col.sky

dsm = d-smooth(d,smfac,/edge)

openw,1,'/export/brixx2/bruntt/procyon/att/hd61421_wei.dat'
for i=0L,n-1 do $
 printf,1,t(i),dsm(i),d(i),wei(i),format='(D16.4, D16.5, D16.5, D16.7)'
close,1

if n_elements(ff) eq 0 then $
 readcol,'/export/brixx2/bruntt/procyon/att/att_90min_highpass_weights.fou',ff,aa,format='f,f'

easyps, keywords, kk, dops=dops, /prepare, dim = [20,11.0,-1,1], $
 fil = 'att_rvc_power.ps', dir = '/export/brixx2/bruntt/procyon/att/'

plot,ff*1e3/86400,aa^2.,xtit='!6Frequency [mHz]',ytit='!6Power [m!E2!N s!E-2!N]',/nodata,tit='Procyon from AAT',yr=[0,.8]
plots,1e3/(90.*60.),!y.crange,line=1,col=col.red,thick=2
oplot,ff*1e3/86400,aa^2.

; Zoom inset:
; plot,ff*1e3/86400,aa^2.,xr=[.5,1],yr=[0,.8],/noerase,position=[.6,.45,.9,.88],xtickname=['',' ','',' ','',' ','',' ']
plot,ff*1e3/86400,aa^2.,xr=[.7,.8],yr=[0,.8],/noerase,position=[.6,.45,.9,.88],xtickname=['',' ','',' ','',' ','',' ']

easyps, keywords, kk, dops=dops, /close

END
