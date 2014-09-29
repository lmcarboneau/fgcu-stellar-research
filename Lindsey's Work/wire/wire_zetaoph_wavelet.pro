; Feb 2007, wavelet analysis of Zeta Oph LCs
; HB.

calc_wave = 0B

lc = 'w2004'
lc = 'w2005'
lc = 'w2006'

if lc eq 'w2006' then begin
 readcol,'/export/brixx1/bruntt/wire/wire_lc/wire_lc_ZetaOph_Aug2006_s0_HD149757_O9V.dat',t,d,w,format='d,f,f' 
 tspan = 16
endif

if lc eq 'w2005' then begin
 readcol,'/export/brixx1/bruntt/wire_analysis/bstars/zetaoph/lc/ZetaOph_wire_2005.dat',t,d,w,format='d,f,f' 
 tspan = 18
endif

if lc eq 'w2004' then begin
 readcol,'/export/brixx1/bruntt/wire/wire_lc/wire_lc_ZetaOph_Feb2004_s0_HD149757_O9V.dat',t,d,w,format='d,f,f'
 tspan = 4
endif


 n = n_elements(t)
 wei = fltarr(n) & wei(*) = 1./float(n)
 dir = '/export/brixx1/bruntt/temp/'
 fwhm = 2.0
 gaussn = 10. 
 timestep = 1. ; 1 day average
 minf = 8. / 32. ; at least eight cycles
 maxf = 7.  ; c/day

nmax = n_elements(t) ;  & 
; nmax = 7000 ; use all data points?

t2 = t(0:nmax-1) & d2 = d(0:nmax-1) & wei2 = wei(0:nmax-1)
plot,t2,d2,psym=3,yr=[-1,1]*0.03,xsty=3
print,max(t2) - min(t2)

if calc_wave then $
wavelet_ds,$
 t2, d2, wei2, dir, 0., 1., $
 minf, maxf, 0, 5.18, fwhm, gaussn, timestep, $
 frqarr, amparr, pharr, alparr, betarr

; help,frqarr, amparr & print,frqarr(*,0) ; frequency axis
!P.charthick=1 & !P.charsize = 1.5

freq = reform(frqarr(*,0))
time = findgen(n_elements(amparr(0,*))) * timestep + timestep * 0.5
clear=replicate(' ',12)
clear2 = [' ','',' ','',' ','',' ','',' ','',' ','']

; surface,amparr*1e3,freq,time,xtit='Frequency [c/day]',ytit='Time [d]',ztit='Amplitude [mmag]',$

easyps, keywords, kk, dops=dops, /prepare, dim = [22,26.0,-1,1], $
 fil = 'zeta_oph_wavelet_'+lc+'.ps', dir = '/export/brixx1/bruntt/papers/wire/zetaoph/'

 col=getcolor(/load)
 colarr = col.sky

 yyp = 0.7 ; lc plot: position in y

 tstep = 8.5

; Phase lag marked?
 nm = 100
 tstep = 0.5 * 0.76147817 & markf = -16.08 + (findgen(nm)) * tstep & rrt = step*2. ; range in time

 tstep01 = 0.34266517 & nm01 = 20 & markf01 = 0.307 + (findgen(nm01)) * tstep01
 tstep02 = 0.35896    & nm02 = 20 & markf02 = 5.282 + (findgen(nm02)) * tstep02


; plot,t2,d2,xr=[-18,-10] + 22.
; fidus2,x,y
; nx = n_elements(x) & dx = x(1:nx-1) - x(0:nx-2)
; resistant_mean,dx,3,me,sd,nr & print,me,min(x)


; Time intervals for LC plot
 tstep2 = 4. & nm2 = 9 & markf2 = -16. + findgen(nm2) * tstep2 & rrt2 = tstep2

 plot,t2,d2,psym=1,symsi=.1,position=[.15,yyp,.95,.95],xr=[-tspan,tspan],yr=[-1,1]*0.03,xtickname=clear,ytit='!4D!6mag'
 ; nf = n_elements(markf)
 ; for k=0,nf-1 do arrow,markf(k),0.025,markf(k),0.015,/data,thick=2,col=colarr

 contour,transpose(amparr*1e3),time-tspan,freq,ytit='!6Frequency [c/day]',xtit='!6Time [d]',$
  levels=[2,4,6,8,10,12],c_labels=[1,1,1,1,1,1,1],c_charsi=2,/cell_fill,/noerase,$
  position=[.15,.1,.95,yyp],xr=[-tspan,tspan]
 plots,!x.crange,5.1825,line=2,col=col.red,thick=3 
 plots,!x.crange,2.9614,line=2,col=col.red,thick=3

 ; for k=0,nf-1 do arrow,markf(k),5,markf(k),4.4,/data,thick=2,col=colarr

 contour,transpose(amparr*1e3),time-tspan,freq,/noerase,$
 levels=[2,4,6,8,10,12],c_labels=[1,1,1,1,1,1,1],c_charsi=2,xtickname=clear,ytickname=clear,$
  position=[.15,.1,.95,yyp],xr=[-tspan,tspan]

 easyps, keywords, kk, dops=dops, /close


; ++++++++++++++++++++++++++++++++++++
; plot interesting moments in time:
; ++++++++++++++++++++++++++++++++++++
 

 

 getpos,1,nm2+1,0,.03,0,pos,ppx1=0.15

!P.charsize=0.9

easyps, keywords, kk, dops=dops, /prepare, dim = [22,26.0,-1,1], $
 fil = 'zeta_oph_LC_'+lc+'.ps', dir = '/export/brixx1/bruntt/papers/wire/zetaoph/'

 plot,t2,d2,psym=1,symsi=.1,ytit='!4D!6mag',yr=[-1,1]*0.03,/nodata,$
   position=pos(*,0),xr=[-tspan,tspan],ytickname=clear2
  oplot,t2,d2,col=col.charcoal
  oplot,t2,d2,psym=1,symsi=.2
 for k=0,nm2-1 do arrow,markf2(k),0.025,markf2(k),0.015,/data,thick=2,col=col.red

 for k=0,nm2-2 do begin

  plot,t2,d2,psym=1,symsi=.1,xr=markf2(k)+[-1,1]*rrt2*.6,$
   ytit='!4D!6mag',yr=[-1,1]*0.03,/nodata,position=pos(*,k+1),/noerase,ytickname=clear2
   for g=0,nm-1 do if markf(g) gt markf2(k)-rrt2*.6 and markf(g) lt markf2(k)+rrt2*.6 then $
      plots,markf(g),!y.crange,line=2,thick=2,col=col.sky
   for g=0,nm01-1 do if markf01(g) gt markf2(k)-rrt2*.6 and markf01(g) lt markf2(k)+rrt2*.6 then $
      plots,markf01(g),!y.crange,line=2,thick=2,col=col.red
   for g=0,nm02-1 do if markf02(g) gt markf2(k)-rrt2*.6 and markf02(g) lt markf2(k)+rrt2*.6 then $
      plots,markf02(g),!y.crange,line=2,thick=2,col=col.aqua
   oplot,t2,d2,col=col.charcoal
   oplot,t2,d2,psym=1,symsi=.2

 endfor

 plot,t2,d2,psym=1,symsi=.1,xr=markf2(k)+[-1,1]*rrt2*.6,$
  ytit='!4D!6mag',yr=[-1,1]*0.03,/nodata,position=pos(*,k+1),/noerase,ytickname=clear2,xtit='!6Time [d]'
   for g=0,nm-1 do if markf(g) gt markf2(k)-rrt2*.6 and markf(g) lt markf2(k)+rrt2*.6 then $
      plots,markf(g),!y.crange,line=2,thick=2,col=col.sky
   for g=0,nm01-1 do if markf01(g) gt markf2(k)-rrt2*.6 and markf01(g) lt markf2(k)+rrt2*.6 then $
      plots,markf01(g),!y.crange,line=2,thick=2,col=col.red
   for g=0,nm02-1 do if markf02(g) gt markf2(k)-rrt2*.6 and markf02(g) lt markf2(k)+rrt2*.6 then $
      plots,markf02(g),!y.crange,line=2,thick=2,col=col.aqua
  oplot,t2,d2,col=col.charcoal
  oplot,t2,d2,psym=1,symsi=.2


 easyps, keywords, kk, dops=dops, /close

END
