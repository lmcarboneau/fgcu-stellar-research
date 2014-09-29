; Plot power density spectrum of Zeta Oph
; HB January 2007

base = '/export/brixx1/bruntt/wire_analysis/bstars/zetaoph/'

; Calculate area per resolution element:
; wire_power_density_resolution, '/home/bruntt/wire/wire_lc/wire_lc_BetaAur.dat', fres=200, fmax=1000, highres=3.
; 
; I used fres = 200 c/day -- about 13 WIRE "harmonics" from 0.0 c/day.
; /home-cudos/rahmi/work/wire/ZetaOph_wire_2004.dat     0.803384 ; fres = 80
;
; /home-cudos/rahmi/work/wire/zetaoph_most_2004.dat     0.214972
; /home-cudos/rahmi/work/wire/ZetaOph_wire_2004.dat     0.803892 ; fres = 200
; /home-cudos/rahmi/work/wire/ZetaOph_wire_2005.dat     0.128954

cday = 1e6/86400D ; concert from c/day to microHz
fac = 1e6         ; concert from magnitudes to micromagnitudes
sm = 50           ; smooth the spectra by a box-car this wide
ppmfac = 1.086 ; convert micromag to ppm
; col=getcolor(/load)
most_colour   = col.sky ; 100  ; colour of most spectrum
wire_colour05 = col.red ; 100  ; colour of most spectrum

; Compare with Beta Hydri
if n_elements(fbeta) eq 0 then $
 readcol,'/export/brixx1/bruntt/wire/binary/betaAur/wire_lc_BetaAur_Apr2006_v7_decor2_sub.fou',fbeta,abeta
; /export/brixx1/bruntt/wire/binary/betaAur/wire_lc_BetaAur_Apr2006_v7_nodecor_sub.fou',fbeta,abeta

 ; from c/day to microHz 
area_w04 = 0.803384 * cday ; 6 days?
area_m04 = 0.214972 * cday ; 20 days?
area_w05 = 0.128954        ; 32 days
area_beta = cday/(21*.25) ; microHz

duty = [.15,.60,.25] ; approx. duty cycles: wire 04, most 04 (paper), wire 05
dura = [9.,20.,33]  ; duration of observations in days

print,cday/(dura*duty) ; microHz
print,area_w04, area_m04, area_w05
; Theory: 8.5733881      0.96450617       1.4029181
; Actual: 9.2984260       2.4881019       0.128954

; MicroHz, theory:
area_w04 = 8.5733881     
area_m04 = 0.96450617       
area_w05 = 1.4029181

if n_elements(fm04) eq 0 then begin

restore,'/export/brixx1/bruntt/wire_analysis/bstars/zetaoph/zetaoph_spec.idl'

; base = '/import/suphys1/laszlo/rahmi/wire/freq/'
; readcol,base + 'zetaoph_most_2004_sub_all.fou',fm04,am04 ; 0-2500 c/day
; readcol,base + 'zetaoph_wire_2004_sub_all.fou',fw04,aw04
; readcol,base + 'zetaoph_wire_2005_sub_all.fou',fw05,aw05

;fname = '/export/brixx1/bruntt/wire_analysis/bstars/zetaoph/zetaoph_spec.idl'
; save,filename=fname,fm04,am04,fw04,aw04,fw05,aw05,/compress 
endif

fmax_most = 3000.
wm = where(fm04*cday lt fmax_most,cm)

ttit = ''
; ttit = '!4f!6 Ophiuci' ; use star name as title on plot?

sm = 101

ymax = 1e5

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
easyps, keywords, kk, dops=dops, /prepare, dim = [20,11.0,-1,1], $
 fil = 'power_density.ps', dir = '/export/brixx1/bruntt/papers/wire/zetaoph/'
col=getcolor(/load)
!P.charsize=1.5
!P.thick=2
!P.charthick=2

plot_oo,[0,1],$
 xtit='!6Frequency [!4l!6Hz]',ytit='Power density [ppm!E2!N/!4l!6Hz]',$
 /nodata,title=ttit,xsty=5,position=[.15,.165,.95,.85],xr=[5,3e4],yr=[10,ymax]
axis,xaxis=0,xtit='!6Frequency [!4l!6Hz]'
xx = 10.^[1,2,3,4,5]
xtt = strarr(5)
xtt(0) = strcompress(string(xx(0)/cday,format='(F9.2)'),/remove_all)
xtt(1) = strcompress(string(xx(1)/cday,format='(F9.1)'),/remove_all)
xtt(2) = strcompress(string(xx(2)/cday,format='(I9)'),/remove_all)
xtt(3) = strcompress(string(xx(3)/cday,format='(I9)'),/remove_all)
xtt(4) = strcompress(string(xx(4)/cday,format='(I9)'),/remove_all)
; xtt(5) = strcompress(string(xx(5)/cday,format='(I9)'),/remove_all)

axis,xaxis=1,xtit='!6[c/day]',xtickname=xtt
polyfill,[1,10,10,1.]*cday,[1e3,1e3,ymax,ymax],col=200   ; ,ori=235,/line_fill


;; plot_oo,[0,1],xr=[5,3e4],yr=[10,ymax],$
;;  xtit='!6Frequency [!4l!6Hz]',ytit='Power density [ppm!E2!N/!4l!6Hz]',$
;;  /nodata,title=ttit
;; axis,xaxis=0,xtit='!6Frequency [!4l!6Hz]'
;; polyfill,[1,10,10,1.]*cday,[1e3,1e3,ymax,ymax],col=200   ; ,ori=235,/line_fill


;oplot,fm04(wm)*cday,smooth( ((am04(wm)*fac/ppmfac)^2.)/area_m04,sm,/edge),col=most_colour
;oplot,fw04    *cday,smooth( ((aw04    *fac/ppmfac)^2.)/area_w04,sm,/edge)
;oplot,fw05    *cday,smooth( ((aw05    *fac/ppmfac)^2.)/area_w05,sm,/edge),col=wire_colour05

; Beta Aur:
; oplot,fbeta   *cday,smooth( ((abeta    *fac/ppmfac)^2.)/area_beta,sm,/edge),col=col.green

if n_elements(dsout_most04) eq 0 then begin
 wire_power_smooth, fm04(wm)*cday, ((am04(wm)*fac/ppmfac)^2.)/area_m04, dsout_most04,df=100,res=.5 ; , /debug
 wire_power_smooth, fw04    *cday, ((aw04    *fac/ppmfac)^2.)/area_w04, dsout_wire04,df=100,res=.5 ; , /debug
 wire_power_smooth, fw05    *cday, ((aw05    *fac/ppmfac)^2.)/area_w05, dsout_wire05,df=100,res=.5 ; , /debug
endif

ths = 4

oplot, fw04    *cday, dsout_wire04,thick=ths
oplot, fm04(wm)*cday, dsout_most04,thick=ths,col=most_colour
oplot, fw05    *cday, dsout_wire05,thick=ths,col=wire_colour05

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++



; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; legend:
fyy = 50.
wire04y = 1000*fyy
mosty   =  wire04y * .5
wire05y =  mosty * .5
xyouts,2500,mosty,   'MOST 2004',align=0
xyouts,2500,wire04y, 'WIRE 2004',align=0
xyouts,2500,wire05y, 'WIRE 2005',align=0

obsy =  wire05y * .5 * .5
polyfill,[1000,2000,2000,1000],[2.5e3,2.5e3,obsy*2,obsy*2],col=200   
xyouts,2500,obsy, 'Pulsation'

; Offset the line:
dx = 20    ; offset x value 
dy = 1.2  ; multiply y value (log plot)

thx = 6
oplot,[1000,2000]+dx,[mosty,mosty]*dy,thick=thx,color=most_colour
oplot,[1000,2000]+dx,[wire04y,wire04y]*dy,thick=thx
oplot,[1000,2000]+dx,[wire05y,wire05y]*dy,thick=thx,col=wire_colour05
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
easyps, keywords, kk, dops=dops, /close

; Direct plots?

goto,skip_dir



sm = 1
plot,[0,1],xr=[0,1000],yr=[0,100000],$
 xtit='!6Frequency [!4l!6Hz]',ytit='Power [ppm]',$
 /nodata,title=ttit

oplot,fm04(wm)*cday,smooth( ((am04(wm)*fac/ppmfac)^2.),sm,/edge),col=most_colour
oplot,fw04    *cday,smooth( ((aw04    *fac/ppmfac)^2.),sm,/edge)
oplot,fw05    *cday,smooth( ((aw05    *fac/ppmfac)^2.),sm,/edge),col=wire_colour05


sm = 1
plot,[0,1],xr=[0,100],yr=[0,1e6],$
 xtit='!6Frequency [!4l!6Hz]',ytit='Power [ppm]',$
 /nodata,title=ttit
oplot,fm04(wm)*cday,smooth( ((am04(wm)*fac/ppmfac)^2.),sm,/edge),col=most_colour
;oplot,fw04    *cday,smooth( ((aw04    *fac/ppmfac)^2.),sm,/edge)
oplot,fw05    *cday,smooth( ((aw05    *fac/ppmfac)^2.),sm,/edge),col=wire_colour05;,line=1


skip_dir:

END

