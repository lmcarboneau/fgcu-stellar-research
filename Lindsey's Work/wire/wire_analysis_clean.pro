PRO wire_analysis_clean, res, gat, nx=nx, ny=ny, finput=finput, yrr=yrr, $
 modeout=modeout, dops=dops, fil=fil, dir=dir, xaxis=xaxis, pinput=pinput,filter=filter,$
 xx1=xx1,xx2=xx2

; Purpose:
; Run this program after wire_master_clean.pro 
; and wire_gather_clean.pro 

; finput: for simulations, supply the input frequency!

!P.charsize=1.2

nfreq = n_elements(res) ; number of modes
nsim  = n_elements(gat) ; number of simulations


default9, nx, 4
default9, ny, 4
default9, flim, 0.005
default9, yrr, 0.05
default9, modeout, 0B

default9, dir, '~bruntt/temp/'
if n_elements(fil) eq 0 then fil = 'temp_clean_'+filter+'.ps'
default9, dops, 1B

default9, xaxis, 'nsim'
default9, filter, ''

if dops then $
 easyps, keywords, kk, dops=dops, /prepare, dim = [20,12.0,-1,1], $
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
n6791pos,nx,ny,0.0,0.,1,pos, ppx1 = 0.18, ppy1 = 0.12 ; ppy1, ppx2=ppx2, ppy2=ppy2
xtt = ['','',' ','',' ','',' ','',' ','',' ','']
if xaxis eq 'phase' then xtt = ['',' ',' ','',' ',' '] ; [' ','',' ','',' ','',' ','',' ','',' ','']
ytt = replicate(' ',10)
youtt = '!4D!17f!3 [c/day]
xoutt = '!17n!3'
if xaxis eq 'phase' then xoutt = '!4Du!3'

; Wipe the slate clean:
!P.multi=0
plot, [0,1], /nodata, xsty=6, ysty=6

;;; nplot    = nx * ny
nsim     = n_elements(gat) ; number of simulations
xdefault = findgen(nsim)

; -------------------------------------------------
; Do the 'nplot' plots!
; -------------------------------------------------

plotsym,0,/fill

for i=0,nplot-1 do begin

; print,nx,ny,nplot,i

; xt2 = xtt & if (i mod ny) eq 0 then xt2 = ''
; xt2 = ytt & if (i lt nx) eq 0 then xt2 = xtt
 yout = ''
 yt2 = ytt ; clear all
 xout = ''
 xt2 = ytt & if (i eq nx * (ny-1)) then begin
   xt2 = ['',' ','',' ','',' ','',' ','']
   xout = xoutt
   yt2 = ['',' ','',' ','',' ','',' ','']
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

 plot,xval,gat.freq(i)-finput(i),psym=3,ysty=3,$
  yr=[-1,1]*yrr,charthick=1,/nodata,xsty=3,position=pos(*,i),/noerase, $
  xtickname = xt2,ytickname=yt2,xtit=xout, ytit=yout,xr=[xx1,xx2]



 resistant_mean, gat.freq(i) - finput(i), 3, offset, sd, nr

; plots,!x.crange, offset , line=0,col=colx(1)
; plots,!x.crange, offset+flim, line=2,col=colx(1)
; plots,!x.crange, offset-flim, line=2,col=colx(1)


plotsym,0,thick=2
plots,.0,.0,psym=8,thick=4,symsi=3,col=150 ; input value!
plots,.0,.0,psym=7,thick=2,symsi=3/sqrt(2.),col=150 ; input value!
plotsym,0,/fill


 w = where(abs(gat.freq(i) - offset - finput(i)) gt flim,c, comp=wgood)
 cg = n_elements(wgood)
 x = findgen(nsim)
 if c ge 2 then $
   oplot,xval(w),gat(w).freq(i)-finput(i),psym=8,symsi=.2,col=colx(2)
 if cg ge 2 then $
   oplot,xval(wgood),gat(wgood).freq(i)-finput(i),psym=8,symsi=.2 ; all simulations results

 dev = 100. * (float(c) / float(nsim))
; Percent of data point that are outliers

; out  = '!3Offset=' + strcompress(string(offset,format='(F8.4)'),/remove_all) 
; out2 = '!3Outliers='+strcompress(string(dev,format='(I3)'),/remove_all)+'%'
 out2 = '!3'+strcompress(string(dev,format='(I3)'),/remove_all)+'%'
 out3 = '!17f!3!I'+strcompress(string(i+1,format='(I3)'),/remove_all)+$
           '!N='+strcompress(string(finput(i),format='(F5.1)'),/remove_all) ; +' c/d'
 out4 = '!17f!3!I'+strcompress(string(i+1,format='(I3)'),/remove_all)

if xaxis eq 'sim' then begin
 xyouts, nsim * .08, yrr * 0.85,  out2, charsi=1.5,charthick=1
 ; xyouts, nsim * .08, yrr * 0.70,  out,  charsi=1.5,charthick=1
 if modeout then $
  xyouts, nsim * .08, -yrr * 0.85,  out3, charsi=1.5,charthick=1
endif else begin

 xyouts, xx1+(xx2-xx1)*.06, -yrr * 0.9,  out4, charsi=1.4,charthick=1 ; freq. name
 if i eq 0 and filter ne '' then xyouts, xx1+(xx2-xx1)*.06, yrr * 0.75, '!17'+filter+'!3', charsi=1.4,charthick=1

endelse
  



endfor

; CLOSE POSTSCRIPT FILE:
if dops then $
 easyps, keywords, kk, dops=dops, /close

END

