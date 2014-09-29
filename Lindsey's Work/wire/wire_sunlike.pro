; Make log/log power density plots

dops    = 1

rawspec = 0B
smooth_spec = 1B
harvey_on = 0B
simGRANon = 0B

most_spec_on = 0B

import  = 0B
if n_elements(p00nov) eq 0 then import = 1B
colon   = 1B ; & if dops then colon=0B

virgo_on   = 0B ; plot virgo power density ---> data from HGL (recommended)
virgo2_on  = 0B ;
virgo_green_on = 0B ; green channel VIRGO; dp = 10000-110000, 
                    ; approx. times: 10th Feb 1996 -- 20th of April 1996

procyon_noscat_on = 0B ; INTERESTING: procyon 1999 - data affected by scatter light data removed
procyon_boxsmooth = 0B ; UNINTERESTING! 
;                /ai40/bruntt/wire/wire_amp/pro99box.amp.idl ---> small difference < 5 mill.
                                ; ie. 5/11.57=0.43 c/day --> Period =
                                ; 2.3 days (the width of the box-car)

alphacen_smooth07 = 0B ; alpha cen 1999 - box car smoothed - width = 2*0.7 days

hglon = 0B ; hgl's simulations on ? == red curve

procyon_simulations = 1B ; procyon simulations RECOMMENDED, HK
sim99on = 0B & sim00on = 0B


procyon_2000_noscat = 0B

gammaequ_on = 0B
cyboo_on = 0B
altair_on = 0B

; if n_elements(cmi8) eq 0 then import = 1B
tt=1  & if dops then tt  = 3.0
tt1=2 & if dops then tt1 = 2.0

if import then begin


readcol,'~/wire/procyon/most_ampspec_Fig3_in_bedding_paper.DAT',fmost,dmost,format='F,F'
fmost = fmost * 1000. ; transform from milliHz to MicroHz

; ===========================================================================
; USAFA:
; ===========================================================================
; restore,'~/wire/procyon/procyon99newscat.amp.idl' ; pro99newscat
; restore,'~/wire/procyon/pro00fwhmscat.amp.idl' ; pro00fwhmscat ; scat light also removed!

; New reduction of 99 data set, procyon:
; No scat + fwhm fit:
restore,'~/wire/wire_amp/wire_lc_Procyon_2000.amp.idl' ; p00nov
restore,'~/wire/wire_amp/wire_lc_Procyon_1999.amp.idl' ; p99nov
; restore,'~/wire/wire_amp/wire_lc_Procyon_2000_scat.amp.idl' ; p00novS
restore,'~/wire/wire_amp/wire_lc_Procyon_1999_scat.amp.idl' ; p99novS


; nmerge = 5 instead of 31 ... any difference?
restore,'~/wire/wire_amp/wire_lc_Procyon_1999_mer5.amp.idl' ; p99novM5

; Alpha Cen new resutls: september 2004
restore,'~/wire/procyon/acen99new.amp.idl'  ; acen99new 
;  restore,'~/wire/procyon/acen99tbox.amp.idl' ; acen99tbox

; HL simulation of Procyon ... duration of time series = 1 day
 restore,'~/wire/procyon/hgl_procyon_power_12AUG04.idl' ; hgl2

; VIRGO:
restore,'~/wire/procyon/virgo_powerdensity.idl'



; ===========================================================================

; August 2004: procyon alternative light curves
;restore,'/ai40/bruntt/wire/wire_amp/proc00_noscat_box.amp.idl' ; proc00_noscat_box
;restore,'/ai40/bruntt/wire/wire_amp/proc00_noscat.amp.idl'     ; proc00_noscat
;restore,'/ai40/bruntt/wire/wire_amp/proc00_cutfwhm.amp.idl'    ; proc00_cutfwhm

; September 2004: Alternative light curves
;restore,'/ai40/bruntt/wire/wire_amp/pro00noscat2.amp.idl' ;  ,pro00noscat2
;restore,'/ai40/bruntt/wire/wire_amp/pro00scat2.amp.idl'   ;  ,pro00scat2

; restore,'/ai40/bruntt/wire/wire_amp/pro00all.amp.idl'   ; pro00all; 
; restore,'/ai40/bruntt/wire/wire_amp/pro00allback.amp.idl' ; pro00allback


; Data from VIRGO: green channel
; restore,'/ai40/bruntt/wire/procyon/procyon_data/virgo_green.idl';
; restore,'/ai40/bruntt/wire/procyon/procyon_data/virgo_long.idl' ; freq9, amp9
; f99 = freq9 * 1e6/86400D & w = where(f99 lt 1e6/(2.5*60.),c) ; nykvist
; virgo_long = fltarr(2,c) & virgo_long(0,*) = f99(w) & virgo_long(1,*) = amp9(w)^2.
; virgo_long(1,*) = virgo_long(1,*) / 2e6

; restore,'/ai40/bruntt/wire/procyon/procyon_data/virgo_long_endof_2000.idl'
; f99 = freq9 * 1e6/86400D & w = where(f99 lt 1e6/(2.5*60.),c) ; nykvist
; virgo_long2000 = fltarr(2,c) & virgo_long2000(0,*) = f99(w) & virgo_long2000(1,*) = amp9(w)^2.
; virgo_long2000(1,*) = virgo_long2000(1,*) / 2e6
; DATA IS DETRENDED ---> ACTIVITY CANNOT BE MEASURED!!!


; Import HansKs amplitude spectra:
; us wire_import_hansk, hk

endif


y1 = .1 & y2 = 1e7
x1 = 1  & x2 = 35000.
txt1='' & txt2=''

; ==================================================================

; dat1 = SKY   COLOUR
; dat2 = GREEN COLOUR

; ===================================================================
; Nov 19th 2004:
; New reduction of 1999+2000 data set, excluding scattered light:
; ===================================================================

dat1 = p99nov & dat2 = p00nov & dat3 = 0B & dat4 = 0
 y1=.1 & y2=10000. & x1=1 & x2=25000. & sm=61
txt1 = 'Procyon 1999' & txt2 = 'Procyon 2000' & rawspec = 0B
sim99on = 1B & sim00on = 0B & simGRANon = 0B & most_spec_on = 1B

dat1 = p99nov & dat2 = p00nov & dat3 = 0B & dat4 = 0
 y1=.6 & y2=100. & x1=100. & x2=25000. & sm=61
txt1 = 'Procyon 1999' & txt2 = 'Procyon 2000' & rawspec = 0B
sim99on = 1B & sim00on = 1B & simGRANon = 1B & most_spec_on = 0B

dat1 = p99nov & dat2 = 0. & dat3 = 0B & dat4 = 0
 y1=.6 & y2=100. & x1=100. & x2=25000. & sm=61
txt1 = 'Procyon 1999' & txt2 = '' & rawspec = 0B
sim99on = 1B & sim00on = 0B & simGRANon = 1B & most_spec_on = 1B


;dat1 = p00nov & dat2 = 0. & dat3 = 0B & dat4 = 0
; y1=.6 & y2=100. & x1=100. & x2=25000. & sm=61
;txt1 = 'Procyon 2000' & txt2 = '' & rawspec = 0B
;sim99on = 0B & sim00on = 1B & simGRANon = 1B & most_spec_on = 0B

;dat1 = p99nov & dat2 = p00nov & dat3 = 0B & dat4 = 0
; y1=.6 & y2=100. & x1=100. & x2=25000. & sm=61
;txt1 = 'Procyon 1999' & txt2 = 'Procyon 2000' & rawspec = 0B
;sim99on = 0B & sim00on = 0B & simGRANon = 1B & most_spec_on = 1B


;dat1 = p99nov & dat2 = p99novM5 & dat3 = 0B & dat4 = 0
; y1=.6 & y2=100. & x1=10. & x2=25000. & sm=61
;txt1 = 'Procyon 1999' & txt2 = 'Procyon 1999 nmer=5' & rawspec = 0B
;sim99on = 1B & sim00on = 0B & simGRANon = 1B & most_spec_on = 0B



; ===================================================================================


; ===================================================================================
;dat1 = cen99 & dat2 = lamara  & dat3 = pro99 & dat4 = 0 ; lambda Ara
;dat1 = cen99 & dat2 = oph20   & dat3 = pro99 & dat4 = 0 ; 20 Oph --> high noise level
;dat1 = cen99 & dat2 = ceri    & dat3 = pro99 & dat4 = 0 ; i Cer --> very high noise level
;dat1 = cen99 & dat2 = cmi8    & dat3 = pro99 & dat4 = 0 ; 8 CMi --> very high noise level
;dat1 = cen99 & dat2 = hd60803 & dat3 = pro99 & dat4 = 0 ; HD 60803

; dat1 = cen99 & dat2 = hd60803 & dat3 = pro99 & dat4 = lamara ; HD 60803 + lambda ara (printed)
; dat1 = cen99 & dat2 = oph20   & dat3 = ceri & dat4 = cmi8   ; 

;dat1 = pro99 & dat2 = hd60803 & dat3 = hd60803t     & dat4 = cen99 &y1=.1&y2=1e3&x1=1&x2=35000& sm=61
;txt1 = 'Procyon 1999' & txt2 = 'HD 60803'
; ===================================================================================

suffix = strcompress(txt1,/remove_all) + '_' + strcompress(txt2,/remove_all)
if suffix eq '' then suffix = 'dummy'
outps = '~/wire/wire_eps/procyon/sunlike_density_'+suffix + '.ps'

if dops then a4ps, filename=outps, /landscape
col=getcolor(/load)



if not dops then colx = [255, 128, 64, 32,64] else colx=[0, 100, 150, 200,75]
if not dops then coly = [128, 64, 32,64] else coly=[100, 150, 200,75]


if colon then $
 colx = [col.sky, col.green, col.red, col.magenta,col.cyan]


plot_oo,dat1.f,smooth(dat1.d,31,/edge), /nodata,$
 xtit='Frequency [!4l!3Hz]',ytit='Power Density [ppm!E2!N/!4l!3Hz]',$
 xr=[x1,x2],yr=[y1,y2],xsty=1,ysty=1,xthick=2,ythick=2,charthick=2.0


if most_spec_on then begin
 ; 32 days of observation at 99% duty cycle:
 dummy_fac = (1e6/(32. * 0.99 * 86400.)) & sm_fac_most = 51

; Hans K:
; powerdensity =    amp^2  * 32.*0.99*86400. / 1000000.


forb = 328.5*0.5
wire_exclude_orbital,fmost, dmost,fny,dny,$
 forb=forb,df=11.,inc=.001
wg = where(fny gt 1e-3,cg)
f1 = fny(wg) & d1 = dny(wg)
wire_power_smooth, f1, d1, ds, df=150.,fref=1000.,res=0.1 ; ,/debug

; plot,fmost,dmost & oplot,fny(wg),dny(wg),col=col.sky
; oplot,f1,ds,col=col.magenta

 oplot, fny(wg), smooth((dny(wg)^2.0)/dummy_fac,sm_fac_most,/edge), $
  col=col.red,thick=2,min_value = 4.5
 oplot,f1,(ds^2.0)/dummy_fac,col=col.yellow,thick=5

endif



if rawspec ge 1 then begin

 if rawspec eq 1 then begin
  wire_pick_freq, dat1.f1, dat1.d1, sm, ff, dd
  oplot,ff,dd,col=coly(0),thick=tt1,min_value=1e-6
 endif

 if rawspec ge 2 then begin
   oplot,dat1.f, smooth(dat1.d, sm, /edge_truncate),col=coly(0)
   fac = 1e6/86400D
   a = 3.527
   f = 15.00

   arrow,/data,a*fac,3,a*fac,9,thick=2
   arrow,/data,(a+f)*fac,4,(a+f)*fac,9,thick=2
   arrow,/data,(-a+f)*fac,4,(-a+f)*fac,9,thick=2
   arrow,/data,(f)*fac,3.0,(f)*fac,10.,thick=3
   arrow,/data,(2.*f)*fac,2.5,(2.*f)*fac,9.,thick=3
   arrow,/data,(3.*f)*fac,2.0,(3.*f)*fac,8.,thick=3

   ff = 0.97
   xyouts,a*fac*ff,2.8,'f!Ia!N',charthick=2.0,charsi=1.2,orientation=270

   xyouts,f*fac*ff,2.8,'f!Iorb!N',charthick=2.0,charsi=1.2,orientation=270
   xyouts,2.*f*fac*ff,2.3,'2 * f!Iorb!N',charthick=2.0,charsi=1.2,orientation=270
   xyouts,3.*f*fac*ff,1.8,'3 * f!Iorb!N',charthick=2.0,charsi=1.2,orientation=270

   xyouts,(f+a)*fac*ff,3.8,'f!Iorb!N + f!Ia!N',charthick=2.0,charsi=1.2,orientation=270
   xyouts,(f-a)*fac*ff,3.8,'f!Iorb!N - f!Ia!N',charthick=2.0,charsi=1.2,orientation=270


 endif

if n_elements(dat2) gt 2 then begin
 if rawspec eq 1 then begin
  wire_pick_freq, dat2.f1, dat2.d1, sm, ff, dd
  oplot,ff,dd,col=colx(1),thick=tt1,min_value=1e-6
 endif
 if rawspec ge 2 then oplot,dat2.f, smooth(dat2.d, sm, /edge_truncate),col=coly(0)
endif


if n_elements(dat3) gt 2 then begin
 wire_pick_freq, dat3.f1, dat3.d1, sm, ff, dd
 oplot,ff,dd,col=colx(2),thick=tt1,min_value=1e-6
endif

if n_elements(dat4) gt 2 then begin
 wire_pick_freq, dat4.f1, dat4.d1, sm, ff, dd
 oplot,ff,dd,col=colx(2),thick=tt1,min_value=1e-6
endif


; oplot,dat1.f1,smooth(dat1.d1,sm),col=colx(0),thick=tt1,psym=-1
;oplot,dat2.f1,smooth(dat2.d1,sm),col=colx(1),thick=tt1
;if n_elements(dat3) gt 2 then $
; oplot,dat3.f1,smooth(dat3.d1,sm),col=colx(2),thick=tt1
;if n_elements(dat4) gt 2 then $
; oplot,dat4.f1,smooth(dat4.d1,sm),col=colx(3),thick=tt1
endif

if smooth_spec then begin

oplot,dat1.f2,dat1.d2,col=colx(0),thick=tt1
if n_elements(dat2) gt 2 then $
 oplot,dat2.f2,dat2.d2,col=colx(1),thick=tt1
if n_elements(dat3) gt 2 then $
 oplot,dat3.f2,dat3.d2,col=colx(2),thick=tt1
if n_elements(dat4) gt 2 then $
 oplot,dat4.f2,dat4.d2,col=colx(3),thick=tt1

if harvey_on then begin
 oplot,dat1.f,dat1.dkfit,col=colx(0),thick=tt
if n_elements(dat2) gt 2 then $
  oplot,dat2.f,dat2.dkfit,col=colx(1),thick=tt
 if n_elements(dat3) gt 2 then $
  oplot,dat3.f,dat3.dkfit,col=colx(2),thick=tt
 if n_elements(dat4) gt 2 then $
  oplot,dat4.f,dat4.dkfit,col=colx(3),thick=tt
endif

endif

if virgo_on then begin
 w = where(virgo(0,*) gt 1e-3 and virgo(0,*) lt 5000.,c)
; if dops then $
; oplot,virgo(0,w),smooth(virgo(1,w), 61,/edge) ,col=colx(3),thick=tt1 else $
 oplot,virgo(0,w),smooth(virgo(1,w)*4., 61,/edge) ,col=col.magenta,thick=tt1 
endif

if virgo2_on then begin
 w = where(virgo2(0,*) gt 1e-3 and virgo2(0,*) lt 4500.,c)
 oplot,virgo2(0,w),smooth(virgo2(1,w)*2., 51,/edge) ,col=colx(1),thick=3 ; tt1
endif


if hglon then begin
 sm_hgl = 21
 if dops then $
 oplot, hgl2(0,*), smooth(hgl2(1,*), sm_hgl, /edge),col=colx(2),thick=tt1 else $
 oplot, hgl2(0,*), smooth(hgl2(1,*), sm_hgl, /edge),col=col.magenta,thick=3
endif

if virgo_green_on then begin
 oplot,virgo_long(0,*),smooth(virgo_long(1,*), 51,/edge) ,col=colx(3),thick=tt1
 oplot,virgo_long2000(0,*),smooth(virgo_long2000(1,*), 51,/edge) ,col=colx(4),thick=tt1
endif


if gammaequ_on then begin
 ; plot_oo,gammaequ.f,gammaequ.d,xr=[100,35000],xsty=1
 ; oplot,gammaequ.f1,gammaequ.d1,col=col.red
 oplot,gammaequ.f2,gammaequ.d2,col=col.yellow,thick=3
endif

if gammaequ_on then begin
 oplot,cy_boo.f2,cy_boo.d2,col=col.red,thick=3 ; M3III type star
endif

if altair_on then begin
; oplot,altair.f1,smooth(altair.d1,31),col=col.cyan,thick=1 ; A7
 oplot,altair.f2,altair.d2,col=col.sky,thick=2 ; A7
; oplot,altair.f,altair.dkfit,col=col.sky,thick=tt,line=2
endif

if procyon_noscat_on then begin
 oplot,pro99noscat.f2,pro99noscat.d2,col=col.navy,thick=3
 ; oplot,pro99noscat.f,pro99noscat.dkfit,col=col.sky,thick=2
endif

if procyon_boxsmooth then begin
 oplot,pro99box.f2,pro99box.d2,col=col.magenta,thick=2,line=2
endif

if alphacen_smooth07 then begin
; restore,'/ai40/bruntt/wire/wire_amp/acen99t07.amp.idl'
 oplot,acen99t07.f2,acen99t07.d2,col=col.beige,thick=3
; oplot,acen99t07.f,acen99t07.dkfit,col=col.cyan,thick=2
endif

if procyon_simulations then begin
; proc1999v2.gran.oscl.white.tdays.oscl.white.density
; proc2000rem.gran.oscl.white.tdays.oscl.white.density
    
; restore,'~/wire/hansk/proc1999v2.gran.oscl.white.tdays.oscl.white.density' ; old
restore,'/home/bruntt/wire/procyon/simul19nov04/WIRE1999_2_gran.dat.oscl.white.tdays.oscl.white.density'
p99_gr_os_wn = gros
; restore,'~/wire/hansk/proc1999v2.gran.tdays.density'
restore,'/home/bruntt/wire/procyon/simul19nov04/WIRE1999_2_gran.dat.tdays.density'
p99_gr = gran

;restore,'~/wire/hansk/proc2000rem.gran.oscl.white.tdays.oscl.white.density'
;p00_gr_os_wn = gros
;restore,'~/wire/hansk/proc2000rem.gran.tdays.density'
;p00_gr = gran

; Sim 2000: 19 nov 2004
restore,'~/wire/procyon/simul19nov04/WIRE2000_gran.dat.tdays.density'
restore,'~/wire/procyon/simul19nov04/WIRE2000_gran.dat.oscl.white.tdays.oscl.white.density'
p00_gr_os_wn = gros
p00_gr = gran



if sim99on then begin
 oplot,p99_gr_os_wn.f2,p99_gr_os_wn.d2, col=col.orchid,thick=3
 if simGRANon then $
  oplot,p99_gr.f2,p99_gr.d2, col=col.sky,thick=3,line=5
endif

;restore,'~/wire/procyon/simul19nov04/WIRE2000_gran.dat.tdays.density'
;restore,'~/wire/procyon/simul19nov04/WIRE2000_gran.dat.oscl.white.tdays.oscl.white.density'
;p00_gr_os_wn = gros
;p00_gr = gran


; xxx
if sim00on then begin
 oplot,p00_gr_os_wn.f2,p00_gr_os_wn.d2, col=col.cyan,thick=3,line=5
 if simGRANon then $
 oplot,p00_gr.f2,p00_gr.d2, col=col.sky,thick=3,line=5
endif

; oplot,p99_gr_os.f, smooth(p99_gr_os.d, sm, /edge_truncate),col=col.orchid
; oplot,p99_gr_os.f, p99_gr_os.dkfit,thick=tt,col=col.cyan ; harvey

endif


if procyon_2000_noscat then begin
 ; oplot,proc00_noscat.f2, proc00_noscat.d2,col=col.yellow,thick=2 
 oplot,proc00_noscat_box.f2, proc00_noscat_box.d2,col=col.pink,thick=4
 oplot,proc00_cutfwhm.f2, proc00_cutfwhm.d2,col=col.red,thick=4


endif

; Show v^-2 behaviour:
;ff1 = 3000. & ff2 = 5000. & p1 = 30 & p2 = p1 / (ff2/ff1)^2. & p4=p1 / (ff2/ff1)^4.
;oplot, [ff1,ff2], [p1,p2], thick=2, line=2
;xyouts,ff2*1.05,p2*.95,'P !9?!3 !4m!3!E-2!N!3',charthick=tt1,charsi=2;
; Show v^-4 behaviour:
;oplot, [ff1,ff2],[p1,p4], thick=2, line=5
;xyouts,ff2*1.05,p4*.95,'P !9?!3 !4m!3!E-4!N!3',charthick=tt1,charsi=2


; oplot,[1,1.]*1000.,[y1,y2],line=2,thick=2


rax = x2 - x1 & ray = y2 - y1

 if txt1 ne '' then $
   xyouts,x1 * 2., y1 * 2., txt1, charthick=2, charsi=2, col=colx(0)
 if txt2 ne '' then $
   xyouts,x2 /30., y2/3.,txt2, charthick=2, charsi=2, col=colx(1)

if dops then begin
 a4ps,/close
 print,' $  ggv ' + outps + ' & ' 
endif

END

