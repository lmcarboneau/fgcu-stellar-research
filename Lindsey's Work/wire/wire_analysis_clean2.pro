PRO wire_analysis_clean2, res, gat, nx=nx, ny=ny, finput=finput, yrr=yrr, $
 modeout=modeout, dops=dops, fil=fil, dir=dir, xaxis=xaxis, pinput=pinput,filter=filter,$
 xx1=xx1,xx2=xx2,ymax=ymax,bz=bz,frr=frr, gat2=gat2, f2=f2, explain=explain

; Purpose:
; Run this program after wire_master_clean.pro 
; and wire_gather_clean.pro 

; This prg. is for HISTOGRAM plots!

; finput: for simulations, supply the input frequency!

!P.charsize=1.3

nfreq = n_elements(res) ; number of modes
nsim  = n_elements(gat) ; number of simulations

default9, nx, 4
default9, ny, 4
default9, flim, 0.005
default9, yrr, 0.05
default9, modeout, 1B
default9, bz, 0.001 ; BIN SIZE for HISTOGRAMS
default9, frr, 0.06
default9, gat2, 0
default9, ymax, 0.6
default9, explain, 0B

m4_get_basedir, basedir
if n_elements(dir) eq 0 then dir = basedir + '/papers/'
if n_elements(fil) eq 0 then fil = 'histogram_clean_'+filter+'.ps'
default9, dops, 1B

default9, xaxis, 'nsim'
default9, filter, ''

if dops then $
 easyps, keywords, kk, dops=dops, /prepare, dim = [22,12.0,-1,1], $
  fil = fil, dir = dir


if n_elements(finput) ne nfreq then begin
 finput = res.freq
 print,' %%% The simulations input freq. not given!'
endif

if n_elements(pinput) ne nfreq then begin
 pinput = res.phase
 print,' %%% The simulations input PHASES were not given!'
endif


col=getcolor(/load)
if dops eq 0 then $
 colx=[col.black,col.sky,col.red] else $
 colx=[col.black, col.black, col.black]

nplot = min([nfreq, nx * ny])

; !P.multi=[0,nx,ny]
n6791pos,nx,ny,0.0,0.,1,pos, ppx1 = 0.1, ppy1 = 0.12 ; ppy1, ppx2=ppx2, ppy2=ppy2
xtt = ['','',' ','',' ','',' ','',' ','',' ','']
if xaxis eq 'phase' then xtt = ['',' ','',' ',' '] ; [' ','',' ','',' ','',' ','',' ','',' ','']
ytt = replicate(' ',10)
youtt = '!4D!17f!3 [c/day]'
youtt = '!17n/n!I!3tot!N!3'
xoutt = '!4D!17f!3 [c/day]' ; '!17n!3'
if xaxis eq 'phase' then xoutt = '!4Du!3'

; Wipe the slate clean:
!P.multi=0
plot, [0,1], /nodata, xsty=6, ysty=6

; nplot    = nx * ny
nsim     = n_elements(gat) ; number of simulations
xdefault = findgen(nsim)

nsim2 = n_elements(gat2)

; -------------------------------------------------
; Do the 'nplot' plots!
; -------------------------------------------------

plotsym,0,/fill

for i=0,nplot-1 do begin

; print,nx,ny,nplot,i

; xt2 = xtt & if (i mod ny) eq 0 then xt2 = ''
; xt2 = ytt & if (i lt nx) eq 0 then xt2 = xtt
 yout = ''
 yt2 = ytt
 xout = ''
 xt2 = ytt & if (i eq nx * (ny-1)) then begin
   xt2 = ['',' ','',' ','','',' ','',' ','']
   xout = xoutt
   yt2 = '' ; ['',' ','',' ','',' ','',' ']
   yout = youtt
 endif

; yt2 = ytt & if (i mod nx) eq 0 then yt2 = ''
; yout = '' & if i eq nx then yout = youtt

 if xaxis eq 'phase' then begin
   xval = gat.phase(i) - pinput(i) 
   if n_elements(xx1) eq 0 then xx1 = -0.5
   if n_elements(xx2) eq 0 then xx2 =  0.5
 endif else begin
   xval = xdefault
   xx1 = 0 & xx2 = nsim-1
 endelse

; plot,xval,gat.freq(i)-finput(i),psym=3,ysty=3,$
;  yr=[-1,1]*yrr,charthick=1,/nodata,xsty=3,position=pos(*,i),/noerase, $
;  xtickname = xt2,ytickname=yt2,xtit=xout, ytit=yout,xr=[xx1,xx2]


mx = frr & mn = -frr
hh = histogram(gat.freq(i)-finput(i), binsize=bz,max=mx, min=mn)
xhh = mn + (findgen(n_elements(hh))/(n_elements(hh)-1.)) * (mx-mn)
nd = n_elements(gat)
rms = robust_sigma(gat.freq(i)-finput(i))
est = [nd*0.3, 0., rms] ; estimates for the fit!

;gg = gaussfit(xhh,hh,ag,nterms=3,estimates=est)
;xxv2 = (findgen(100)/99) * rms * 5. - rms*2.5
;gg2 = ag(0) * exp(-((xxv2-ag(1))^2.) / (2. * ag(2)^2.) )
;sigma1 = ag(2) ; the approx. 1-sigma error from the gauss fit



plot_io,xhh,hh/float(nd),psym=10,xr=[-1,1]*frr,  $ xr=[-1,1]*rms * 5.,$
 charthick=1,xsty=3,position=pos(*,i),/noerase, $
 xtickname = xt2,ytickname=yt2,xtit=xout, ytit=yout,yr=[0.003,yrr],/nodata

;stop

if nsim2 ge 2 then begin
 hh2  = histogram(gat2.freq(i)-f2(i), binsize=bz,max=mx, min=mn)
 xhh2 = mn + (findgen(n_elements(hh2))/(n_elements(hh2)-1.)) * (mx-mn)
 nd2 = n_elements(gat2) & rms2 = robust_sigma(gat2.freq(i)-f2(i))
 ; est2 = [nd2*0.3, 0., rms2] ; estimates for the fit!
 oplot,xhh2, hh2/float(nd2),psym=10,col=150,thick=3


endif

oplot,xhh,hh/float(nd),psym=10,thick=2

yyrange = 10.^(!y.crange) & yout = min(yyrange) + (max(yyrange)-min(yyrange)) * .3
if modeout then $
 xyouts, -frr*.95,yout,'!17f!I!3'+strcompress(string(i+1,format='(I3)'),/remove_all)+'!N!3',$
  charsi=1.3,charthick=2


; Make some arrows on plot that explain what's going on!
if (i eq 0) and explain then begin
 arrow,.013,.3,.003,.2,thick=2,/data
 xyouts,.014,.28,'!17f!Iinp!3!N',charsi=1.2,charthick=2

; Alias:
; arrow,.009+.009,0.065,.001+.009, 0.035,thick=2,/data
; xyouts,.009+.001, 0.09,'!17f!Iinp!N+!3T!S!Iobs!N!R!E -1!N',charsi=1.2,charthick=2

;;; xyouts,.009+.009, 0.07,'!17f!Iinp!N+',charsi=1.2,charthick=2
;;; xyouts,.009+.016, 0.03,'!3T!S!Iobs!N!R!E -1!N',charsi=1.2,charthick=2

endif


if (i eq 1) and explain then begin
 arrow,-.013,.3,-.003,.2,thick=2,/data
 xyouts,-.014-0.008,.28,'!17f!Iinp!3!N',charsi=1.2,charthick=2

; Alias:
 arrow,.013+.009,0.3,.003+.009, 0.2,thick=2,/data
 xyouts,.013+.003, 0.41,'!17f!Iinp!N+!3T!S!Iobs!N!R!E -1!N',charsi=1.2,charthick=2

endif


endfor


; CLOSE POSTSCRIPT FILE:
if dops then $
 easyps, keywords, kk, dops=dops, /close

END

