; Make HR diagram for observed gamma dor stars

; Run these programs first:
; .r wire_logg_gammador

zoomin     = 0B
mark_32115 = 0B

dops = 1

mark_blue_edge = 0B
error_bar_on = 1B
corot_on = 0B
plot_these_hd = [  7455,  12901,  14940,  22001, 26298,  27290,  27604,  33262, $
                  40745,  48501,  65526,  81421, 85964, 110379, 125081, 126516, $
                 135825, 147787, 167858, 218225]



print, ' %%% Launch wire_logg_gammador.pro first to get hipparcos logg / mass estimates'
if n_elements(mass) eq 0 then stop

m4_get_basedir, basedir

; filename=basedir + 'papers/gammador/gammador_info.idl'
filename=basedir + 'papers/gammador/gammador_data/gammador_info_Oct06.idl'
restore,filename ; gam ; from wire_gamma_combine_info.pro + wire_read_templogg


;   .r wire_gamma_combine_info ; med corot_a_stars
;    outfile = '~/VWA/MEGA/info/corot_atype_info.idl'
;    restore,outfile
;    wire_read_templogg, cor, '/mnt/hda5/data/wire/corotAOUT.csv',calib='Nap',/debug
;    save,filename=outfile,cor
;   .r wire_gammador_hr

 
 outfile99 = basedir + 'VWA/MEGA/info/corot_atype_info.idl' 
 restore,outfile99 ; cor structure

hd_vsini = [5590, 7455, 10167, 12901, 14940, 22001, 26298, 27604, 33262, 35416, $
            40745, 48501, 81421, 85964, 135825, 147787, 149989, 167858, 187028, $
            209295, 214291, 216910, 218225,  $
            45431, 51332, 99028, 169725, 172230, 172588, 177177, $  ; COROT
            65526, 110379, 125081, 126516] ; 'new' Gamma Dor targets

   vsini = [85,7,5,64,39,10, 50,70,12,17,37,40,54,69,38,19,136,13,80, 93,60,70, 60,8,24,15,20,17,15,12,$
            56, 24, 6., 4]

logg_out = 0B ; print logg ?
hdout  = 0B
hdout2 = hdout
 if dops then hdout = 0B

if zoomin then begin
   hdout = 1B
   logg_out = 1B
endif

; ===================================================================
if dops eq 0 then $
 window, 1, title='GAM B-type stars',xsize=700,ysize=700,xpos=750,ypos=0 ; 900
; ===================================================================

filname = 'gammador_hr.ps'
if zoomin then filname = 'gammador_hr_zoomin_solar_metallicity.ps'

easyps, keywords, kk, dops=dops, /prepare, dim = [15,13,-1,1], $
 fil = filname, dir = basedir + 'papers/gammador/gammador/'
if dops then device,/color

offx = 0.03
offy = 0.

xxx = [4.05,3.7] & yyy = [.8,1.55] ; nice fig, all stars
if zoomin then begin
  xxx = [3.9,3.75] & yyy = [1.1,1.42]
endif

; Missing Nap teff's for these HD numbers: 65526, 81421 ,     218225
; Almost no data on these two stars...
w1 = where(gam.hd eq 14940,c1)
gam(w1).teff_nap = 7520  &   gam(w1).eteff_nap = 800.
gam(w1).logg_nap = 4.5   &   gam(w1).elogg_nap = 0.5

w1 = where(gam.hd eq 12901,c1)
gam(w1).teff_nap = 7100  &   gam(w1).eteff_nap = 800.
gam(w1).logg_nap = 4.5   &   gam(w1).elogg_nap = 0.5

w1 = where(gam.hd eq 65526,c1)
gam(w1).teff_nap = 7100  &   gam(w1).eteff_nap = 800.
gam(w1).logg_nap = 4.2   &   gam(w1).elogg_nap = 0.5

w1 = where(gam.hd eq 81421,c1)
gam(w1).teff_nap = 7500  &   gam(w1).eteff_nap = 800.
gam(w1).logg_nap = 4.4   &   gam(w1).elogg_nap = 0.5

w2 = where(gam.hd eq 218225L,c2)
gam(w2).teff_nap = 6790  &   gam(w2).eteff_nap = 800.
gam(w2).logg_nap = 4.02  &   gam(w2).elogg_nap = 0.5


; =======================================================
n = n_elements(gam)
bc = fltarr(n)
w = where(gam.teff_nap gt 1000,c)

for j=0,c-1 do begin
; teff_use = gam(w(j)).teff_nap
; logg_use = gam(w(j)).logg_nap
  found = where(vwares.hd eq gam(j).hd,cfound)
  if cfound ne 1 then print,' *** bolometric correction failed for star (not in vwares): ',gam(j).hd else begin
  teff_use = vwares(found).teff
  logg_use = vwares(found).logg
 
 if teff_use gt 4000 and teff_use lt 15000 and $
    logg_use gt 1.5  and logg_use lt 5.5 then begin
   bessell, teff_use, logg_use, bc1
   bc(w(j)) = bc1
 endif
 endelse
endfor

; Make sure extinction is ok:
extby = fltarr(n)
wx = where(gam.extby gt -0.01,cx)
extby(wx) = gam(wx).extby

; DOUBLE CHECK THIS:

; g2 = where(gam.hd eq 125081) & gam(g2).v = avg([7.41,7.46]) ; from Hipparcos. See Paunzen & Maitzen 1998.

lum = fltarr(n) & elum = lum
lum(w) = gam(w).v + 5 - 5. * alog10(gam(w).par/1e3) + bc(w) + 4.1 * extby(w)

; The uncertainty on the luminosity depends on the accuracy of the
; parallax! For some stars it is very uncertain!
;              V-mag    Parallax   B.C.       Extinction
elum(w) = sqrt(0.02^2. + 0.15^2. + 0.01^2. + (4.1 * 0.01)^2.)
elum(w) = sqrt(0.02^2. + 0.10^2. + 0.01^2. + (4.1 * 0.01)^2.) ; for the best stars

; Uncertainty due to parallax:
 x = (5. * alog10((gam(w).par+gam(w).epar)/1e3) - 5. * $
                                                  alog10((gam(w).par-gam(w).epar)/1e3)) * 0.5
 s = where(finite(x))  &  print,avg(x(s))

tef = gam.teff_nap
tef = fltarr(n_elements(gam))
for k=0,n_elements(vwares)-1 do begin
 p = where(gam.hd eq vwares(k).hd, ccp)
 if ccp eq 1 then begin
    gam(p).teff = vwares(k).teff
    gam(p).logg = vwares(k).logg
 endif
endfor

tef = gam.teff ; these are used on the plot

; =======================================================

; =======================================================
; COROT stars:
; =======================================================
if corot_on then begin
  n2 = n_elements(cor)
  bc2 = fltarr(n2)
  w2 = where(cor.teff_nap gt 1000,c2)
  for j=0,c2-1 do begin
   bessell, cor(w2(j)).teff_nap, cor(w2(j)).logg_nap, bc2temp
   bc2(j) = bc2temp
  endfor
  
  ; Make sure extinction is ok:
  extby2 = fltarr(n2)
  wx = where(cor.extby gt -0.01,cx)
  extby2(wx) = cor(wx).extby
  
  ; DOUBLE CHECK THIS:
  lum2     = fltarr(n2)
  lum2(w2) = cor(w2).v + 5 - 5. * alog10(cor(w2).par/1e3) + bc2(w2) + 4.1 * extby2(w2)
  tef2     = cor.teff_nap
endif
; =======================================================

ttt = [' ','4.0',' ','3.9',' ','3.8',' ','',' ','',' ','']
if zoomin then ttt=[' ','',' ','',' ','',' ','',' ','',' ','']

plotsym,4 ; open
 plot,$
  alog10(tef(w)),alog10(lum(w)),xr=xxx,yr=yyy,xtit='!17log !3T!Ieff!N',ytit='!17log !3L/L!ISun!N',$
  psym=8,xsty=1,ysty=1,xthick=2,ythick=2,charsi=1.5,charthick=2,thick=2,/nodata  ,$
  xtickname=ttt

if error_bar_on then begin
 et    = 200.
 el    = 0.10  

 tstart = 10000. & if zoomin then tstart = 6000.
 lstart = 0.95   & if zoomin then lstart = 1.30

 oplot,alog10(tstart + [-1, 1.]*et),lstart+[ 0,0.]*el
 oplot,alog10(tstart + [ 1, 1.]*et),lstart+[  1,-1.]*el*0.05
 oplot,alog10(tstart + [-1,-1.]*et),lstart+[  1,-1.]*el*0.05

 oplot,alog10(tstart + [ 0,0]*et),lstart+[-1,1.]*el
 oplot,alog10(tstart + [1,-1]*et*0.15),lstart+[ 1, 1.]*el
 oplot,alog10(tstart + [1,-1]*et*0.15),lstart+[-1,-1.]*el

endif

sz = 1.0
plotsym,0,/fill
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
for i=0,n-1 do begin
 wg = where(plot_these_hd eq gam(i).hd,cg)
 if cg eq 1 then $
   plots,alog10(tef(i)), alog10(lum(i)), psym=8, symsi=sz
endfor
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++


;for i=0,c-1 do begin
; wv = where(gam(w(i)).hd eq hd_vsini,cvsini)
; if cvsini eq 1 then begin
;   vs = vsini(wv)
;   if vs lt 20 then sz = 1.0
;   if vs ge 20 and vs lt 50 then sz = 0.75
;   if vs ge 50 then sz = 0.5
;   print,sz
;   plots,alog10(tef(w(i))), alog10(lum(w(i))), psym=8, symsi=sz
;   if hdout2 then xyouts,alog10(tef(w(i))), alog10(lum(w(i))),$
;       strcompress(string(gam(w(i)).hd,format='(I8)'),/remove_all),charsi=.7
; endif
;endfor

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if corot_on then begin
 plotsym,3,/fill ; star symbols for COROT stars
 oplot,alog10(tef2(w2)),alog10(lum2(w2)),psym=8
 if hdout2 then begin
   for k=0,c2-1 do $
    xyouts, alog10(tef2(w2(k))),alog10(lum2(w2(k))), $
    strcompress(string(cor(w2(k)).hd,format='(I8)'),/remove_all),charsi=.7
 endif
endif
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Print HD numbers of all stars:
nall = n_elements(tef)

if hdout then begin
for j=0,nall-1 do begin
 wg = where(plot_these_hd eq gam(j).hd,cg)

 strout = strcompress(string(gam(j).hd,format='(I8)'),/remove_all)
 if logg_out then begin
 vp = where(gam(j).hd eq vwares.hd,cvp)
 if cvp eq 1 then $
  strout = strcompress(string(gam(j).hd,format='(I8)'),/remove_all) + $
     '(' + strcompress(string(vwares(vp).logg,format='(F9.1)'),/remove_all)+')'
 endif 

 if cg eq 1 then $
   xyouts, alog10(tef(j))-0.01/10, alog10(lum(j)), $
    strout,$
    charsi=.6,charthick=1
endfor
endif
 
sb = 0 ; no symbols

import_lejeune, basedir + '/evol/lejeune/modc020.dat', l20
import_lejeune, basedir + '/evol/lejeune/modc008.dat', l08

luse = l08 ; low metallicity, Z = 0.008
if zoomin then luse = l20 ; solar metallicity, Z = 0.02

x = 22 ; default!
x = 42

; PLot all ev. tracks
; for i=5,n_elements(luse)-1  do oplot, luse(i).teff(0:x), luse(i).l(0:x),psym=sb,symsi=.3

; Plot only two ev. tracks
for i=14,17 do oplot, luse(i).teff(0:x), luse(i).l(0:x)

; Manual mass determination:
; offset solar metallicity ev. track for M=1.7Ms
if zoomin then begin
 for oo=1,1 do oplot, luse(15).teff(0:x)+oo*0.058/3., luse(15).l(0:x)+0.285*oo/3.,line=2,thick=3,col=150 ; 1.7 Ms
 for oo=1,1 do oplot, luse(14).teff(0:x)-oo*0.058/3., luse(14).l(0:x)-0.285*oo/3.,line=5,thick=3,col=100 ; 2.0 Ms
endif

if mark_32115 then begin
 logl32 = 1.30 ; assuming no extinction
 t32    = 3.86 ; 7250 K
 plots,t32,logl32,psym=7,symsi=1.2,thick=2
endif

xo = 0 & aa = 1.0     ; xo = x & aa = 0.0
offtxt_y = -0.03

kkk = 14 ; ev. track for Z=0.02
oplot, l20(kkk).teff(0:x), l20(kkk).l(0:x),psym=sb,symsi=.3,line=5,thick=2
 x1 = l20(kkk).teff(xo)  &  y1 = l20(kkk).l(xo) + offtxt_y
; xyouts,x1,y1,$
;  strcompress(string(l20(kkk).m,format='(F9.1)'),/remove_all), align=aa,charsi=2.2
; xyouts above: important sanity check
  xyouts,x1,y1+0.01,'Z=0.02', align=aa,charsi=1.
 
for i=10,n_elements(luse)-1 do begin

 offtxt_y = -0.03 & if luse(i).m eq 1.5 then offtxt_y = 0.06

 x1 = luse(i).teff(xo)
 y1 = luse(i).l(xo) + offtxt_y

 if x1 lt xxx(0) and x1 gt xxx(1) and $
    y1 gt yyy(0) and y1 lt yyy(1) then $
 xyouts,x1,y1,$
  strcompress(string(luse(i).m,format='(F9.1)'),/remove_all), align=aa,charsi=1.2
endfor

; Blue edge of instab. strip.
if mark_blue_edge then begin
 pamyat, blue=2
 xyouts,3.918,1.33,align=0,'Blue Edge',orientation=80,charsi=1.1,charthick=2
endif

; Fudged Teff/logg of these stars:
; plots,alog10(tef(w1)),alog10(lum(w1)),psym=6,symsi=2,thick=2
; plots,alog10(tef(w2)),alog10(lum(w2)),psym=6,symsi=2,thick=2

; ===================================================================
easyps, keywords, kk, dops=dops, /close
; ===================================================================

; Make table for paper
outtab = basedir + 'papers/gammador/gammador/gammador_sample.tex'
close,1

gam2 = gam ; store the gam structure
; Get vmicro etc:
; restore,basedir + 'papers/gammador/gammador/vwa_results.idl' ; from vwa_get_params,list_of_directories,vwares
; restore,basedir + '/VWA/GAMMA/vwares_24MAY2006.idl' ; 26 October 2006 (THIS WILL OVERWRITE THE gam STRUCTURE!)

restore,'~/VWA/GAMMA/vwares_02JAN2007.idl' ; ,vwares,gam, 2/1/2007

; Replace HD numbers!
wire_replace_hdnumber, vwares


openw,1,outtab
s = '&' & s2 = ' \\'
teff_out = round(gam2.teff_nap / 10.) * 10.
teff_out_corot = round(cor.teff_nap / 10.) * 10.

printf,1,' %%%%%%%% GRAND TABLE PRODUCED WITH: wire_gamma_hr.pro %%%%%%%%%% '

printf,1,'   \begin{table*}
printf,1,'      \caption[]{The sample of A/F-type stars. We list the HD number,
printf,1,'\str\ indices \citep{hauck98},' ; and the GAUDI data base for the COROT targets (\cite{solano05}), 
printf,1,'\hipp\ parallaxes \citep{hipp},
printf,1,'masses derived from evolution tracks (\cf\ Fig.~\ref{fig:hr}), derived \logg\ parallax,
printf,1,'% using stellar masses, temperatures from \templogg\ and the \hipp para
printf,1,'fundamental atmospheric parameters using \str\ indices with \templogg,
printf,1,'and the parameters derived from the \vwa\ analysis.' 
printf,1,'The last two columns list the microturbulence and \vsini.'
printf,1,'         \label{tab:sample}}

; (for stars with \vsini$>60$\,\kms\ we could not contrain \teff\ and give the adopted values.'
; printf,1,'and \vsini.' ;  was found using synthetic spectra fitted to the observations.
; printf,1,'The typical errors on \teff, \logg, and \feh\ from \templogg\ is 200--400~K, 0.3 dex, and 0.2 dex.
; printf,1,'Typical errors on these parameters from the spectroscopic analysis depend on \vsini\ and S/N in the
; printf,1,'spectrum (see text for details) but is around 150~K, 0.15, and 0.10 for stars with \vsini$<20$\,\kms.

printf,1,'\centering                          
printf,1,'\begin{tiny}
printf,1,'\begin{tabular}{r|rrrrr|rrr|rrr|rrrrr} % HD, stromgren indices, hipparcos data, templogg, VWA
printf,1,'\hline\hline
printf,1,'
printf,1,'\multicolumn{1}{c|}{} & \multicolumn{5}{c|}{\str\ indices$^a$} & \multicolumn{3}{c|}{\hipp\ $+$ Evol.\ tracks$^b$} & \multicolumn{3}{c|}{\templogg$^c$} & \multicolumn{5}{c}{Spectroscopy$^d$} \\
printf,1,'\hline'

printf,1,$
"HD      & $V$   &$b-y$& $c_1$  &$m_1$&$H_\beta$& $\pi_{\rm Hip} [mas]$"
printf,1,$
'                                                             &$M/M_\odot$  &\logg$_\pi$&\teff$_{\rm Ph}$ [K]'
printf,1,$
'                                                                                               &\logg$_{\rm Ph}$&\feh$_{\rm Ph}$'
printf,1,$
'                                                                                                               &\teff$_{\rm Sp}$ [K]&\logg$_{\rm Sp}$&\feh&\vmic&\vsini \\'
printf,1,'\hline'

for k=0,n-1 do begin

 wvwa = where(vwares.hd eq gam2(k).hd,cvwa)
 if cvwa ne 1 then begin
   print,' *** No vwa res for ', gam2(k).hd 
   teff_vwa   = 0
   logg_vwa   = 0
   feh_vwa    = 999
   vmicro_vwa = 0
   vsini_vwa  = 999
  endif else begin
   teff_vwa   = vwares(wvwa).teff
   logg_vwa   = vwares(wvwa).logg
   feh_vwa    = vwares(wvwa).feh
   vmicro_vwa = vwares(wvwa).vmicro
   vsini_vwa  = vwares(wvwa).vsini
  endelse

; Parallax + error on parallax:
; if gam2(k).epar lt 0.95 then begin ; error less than one arc second (rounded off):
;  par  = strcompress(string(gam2(k).par,format='(F5.1)'),/remove_all)
;  epar = strcompress(string(gam2(k).epar*10.,format='(I5)'),/remove_all)
; endif else begin
;  par  = strcompress(string(gam2(k).par,format='(I5)'),/remove_all) 
;  epar = strcompress(string(gam2(k).epar,format='(I5)'),/remove_all)
; endelse

  par  = strcompress(string(gam2(k).par,format='(F5.1)'),/remove_all)
  epar = strcompress(string(gam2(k).epar,format='(F5.1)'),/remove_all)

 par_out = par + '(' + epar + ')'
 
 massout = strcompress(string(mass(k),format='(F5.1)'),/remove_all) ; + '(' + $
;           strcompress(string(emass(k),format='(F5.2)'),/remove_all) + ')' 

 loggout = strcompress(string(logg_pi(k),format='(F5.2)'),/remove_all) ; + '(' + $
;           strcompress(string( err_pi(k),format='(F5.2)'),/remove_all) + ')' 

 wout = where(plot_these_hd eq gam2(k).hd,cout)
 if cout then $
 printf,1,gam2(k).hd,s,gam2(k).v,s,gam2(k).by,s,gam2(k).c1,s, gam2(k).m1,s, gam2(k).hbeta,s,$
  par_out,s, massout, s, loggout, s,$
  teff_out(k),s, gam2(k).logg_nap,s, gam2(k).feh_nap, s, $
  teff_vwa, s, logg_vwa, s, feh_vwa, s, vmicro_vwa, s, vsini_vwa, s2, $
  format='(I6,A3,F5.2,A3,F5.3,A3,F5.3,A3,F5.3,A3,F5.3,A3,  A10,A3,A10, A3,A10,A3,  I5,A3,F5.1,A3,F5.1,A3,    I5, A3, F5.1, A3, F5.2, A3, F5.1, A3, I3, A3)'
endfor



; +++++++++++++++++++++++++++++
if corot_on then begin
printf,1,'\hline'

n_corot = n_elements(cor)
for k=0,n_corot-1 do begin

 wvwa = where(vwares.hd eq cor(k).hd,cvwa)
 if cvwa ne 1 then begin
   print,' *** No vwa res for COROT star', cor(k).hd 
   teff_vwa = 0 &   logg_vwa = 0 &   vmicro_vwa = 0 & vmicro_vwa = 0 & vsini_vwa = 0 &   feh_vwa = 999
  endif else begin
   teff_vwa  =  vwares(wvwa).teff &   logg_vwa  =  vwares(wvwa).logg &   vmicro_vwa =  vwares(wvwa).vmicro & vmicro_vwa =  vwares(wvwa).vmicro & vsini_vwa = vwares(wvwa).vsini
   feh_vwa = vwares(wvwa).feh
  endelse

  par2  = strcompress(string(cor(k).par,format='(F5.1)'),/remove_all)
  epar2 = strcompress(string(cor(k).epar,format='(F5.1)'),/remove_all)
 par_out2 = par2 + '(' + epar2 + ')'
 
 massout2 = strcompress(string(mass2(k),format='(F5.2)'),/remove_all) + '(' + $
            strcompress(string(emass2(k),format='(F5.2)'),/remove_all) + ')' 

 loggout2 = strcompress(string(logg_pi2(k),format='(F5.2)'),/remove_all) + '(' + $
            strcompress(string( err_pi2(k),format='(F5.2)'),/remove_all) + ')' 

 printf,1,cor(k).hd,s,cor(k).v,s,cor(k).by,s,cor(k).c1,s, cor(k).m1,s, cor(k).hbeta,s,$
  par_out2, s, massout2, s, loggout2, s,$
  teff_out_corot(k),s, cor(k).logg_nap,s, cor(k).feh_nap, s, $
  teff_vwa, s, logg_vwa, s, feh_vwa, s, vmicro_vwa, s, vsini_vwa, s2, $
  format='(I6,A3,F5.2,A3,F5.3,A3,F5.3,A3,F5.3,A3,F5.3,A3,  A10,A3,A10, A3,A10,A3,  I5,A3,F5.1,A3,F5.1,A3,     I5, A3, F5.1, A3, F5.2, A3, F5.1, A3, I3, A3)'
endfor
endif                             ; print corot in table?
; +++++++++++++++++++++++++++++

printf,1,''
printf,1,'\multicolumn{17}{l}{Notes:} \\
printf,1,'\multicolumn{17}{l}{$(a)$ \str\ indices are from \cite{hauck98} except for the COROT targets: HD~45431, 51332, 169725, 172230, 172588, 177177}\\
printf,1,'\multicolumn{17}{l}{$(b)$ We used evolution tracks from \cite{lejeune01} to derive stellar mass. This mass, \teff\ from \templogg, and \hipp\ parallaxes to derive \logg$_\pi$}\\
printf,1,'\multicolumn{17}{l}{$(c)$ \templogg\ uses the calibrations of \cite{napi93}}\\
printf,1,'\multicolumn{17}{l}{$(d)$ Fundamental atmospheric parameters derived VWA except for stars with \vsini$>60$ \kms where we give the adopted parameters}\\
printf,1,'\hline'
printf,1,''
printf,1,'\end{tabular}
printf,1,'\end{tiny}
printf,1,'\end{table*}


close,1
print,' %%% Wrote table for paper: ' + outtab


END

