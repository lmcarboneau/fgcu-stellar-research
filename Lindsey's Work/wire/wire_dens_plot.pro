; Prepare plot with the best models for Procyon paper / poster
; (c) December 8th 2004 by Hans Bruntt


; =================
coloff = 1B
dops = 1B
; =================

; =============================================
;  Scale amplitudes of p-modes or granulation?
; =============================================
facpmode = sqrt(2.)
facgran = 47. ; / 30. ; relative to the sun

; To easily identify the best model:
; facgran = 1. & facpmode = 1. ;

identify_model = 0B
if facgran eq 1 and facpmode eq 1 then identify_model=1
; =============================================


addyear = '2000' & bestfit = '2000'
; addyear = '1999' & bestfit = '1999'
; bestfit = '1999'
; bestfit = '2000'

; Turn off models for certain time scales of the granulation?
 off500 = 1B
if addyear eq '1999' then off500 = 1B
 off750 = 0B
off1000 = 0B


 most = 0B

sm_mod = 11. ; smooth factor for models

sep_plot = 0B ; 1 or 3 plots?

thx = 1.  &  if dops then thx = 2
thx_obs = 3 & if dops then thx_obs = 4.

if dops then begin
   dir = '~/papers/procyon/'
   startFilename = 'Procyon_Dens_'+addyear+'.ps'

   x = 20. & y = 10. & xo = (21.5-x) * 0.5 & yo = (30.5-y)*.5

   keywords = PSConfig(Cancel=cancelled, Filename=startFilename, $
    /European, directory=dir, $
    xsize=x, ysize=y, xoffset=xo, yoffset=yo)

   IF cancelled THEN RETURN
      thisDevice = !D.Name
      thisFont   = !P.Font
      !P.Font = 0
    Set_Plot, 'PS'
    Device, _Extra=keywords
endif

; =======================================================
; Colours on or off?
; =======================================================
if coloff then begin
 colx = [100, 50, 175,150] ; 500, 750 and 1000s simulations
 coly = colx
 col_most = 200
endif else begin
; Colours of various simulations:
 col = getcolor(/load)
 colx = [col.red, col.green, col.sky]
 ; colx = [col.sky, col.sky, col.sky]
 ; colx  = [ 175, 175, 175, 175]
 coly = colx
 col_most = col.magenta
endelse 
; =======================================================


ori = [45,45,45] ; orientation of fill
ori = [0,45,90]


if addyear eq '1999' then begin
 restore,'/home/bruntt/wire/sim/Procyon_1999_m5_grand.idl' ; 8 DEC 2004
 restore,'~/wire/wire_amp/wire_lc_Procyon_1999_mer5.amp.idl' ; p99novM5
 pobs = p99novM5
endif

if addyear eq '2000' then begin
 restore,'/home/bruntt/wire/sim/Procyon_2000_m5_grander.idl' ; merg?
 restore,'~/wire/wire_amp/wire_lc_Procyon_2000_mer5.amp.idl' ; p00novM5
 pobs = p00novM5
endif

if bestfit eq '1999' then begin
 w5 = where(merg.tgran eq  500 and merg.agran eq 1.05  and merg.amode eq 8.,c5)

 w7 = where(merg.tgran eq  750 and merg.agran eq 0.9 and merg.amode eq 12,c7)
; w7 = where(merg.tgran eq  750 and merg.agran eq 0.95 and merg.amode eq 10,c7)

 w1 = where(merg.tgran eq 1000 and merg.agran eq 0.75  and merg.amode eq 14,c1)
; w1 = where(merg.tgran eq 1000 and merg.agran eq 0.80  and merg.amode eq 14,c1)
endif

; Btw. tscale = 900s, agran = 0.80

if bestfit eq '2000' then begin
 w5 = where(merg.tgran eq  500 and merg.agran eq 1.00  and merg.amode eq 10,c5)

 w7 = where(merg.tgran eq  750 and merg.agran eq 0.8  and merg.amode eq 14,c7)

;  w1 = where(merg.tgran eq 1000 and merg.agran eq 0.70  and merg.amode eq 14,c1)
 w1 = where(merg.tgran eq 1000 and merg.agran eq 0.75  and merg.amode eq 12,c1)

endif

; c5 = 0 ; turn off tgran = 500s 



if  off500 then c5 = 0
if  off750 then c7 = 0
if off1000 then c1 = 0


x1 = 50 & x2 = 7000
y1 = .7 & y2 = 80

plot_oo,merg(w5).f,merg(w5).d,xr=[x1,x2],yr=[y1,y2],xsty=1,ysty=1,/nodata,$
 xtit='Frequency [microHz]',ytit='Power Density [ppm!E2!N / microHz]',$
 xthick=2,ythick=2,charsi=1.2,charthick=2

if c5 eq 1 then begin
 a = [merg(w5).f, reverse(merg(w5).f)] ; reverse---> make a well-defined polygon!
 b = [        smooth(merg(w5).d+merg(w5).e,sm_mod,/edge),$
      reverse(smooth(merg(w5).d-merg(w5).e,sm_mod,/edge))] 
; like drawing w/ a pencil!

 w = where(a gt x1,c) & polyfill,a(w),b(w),color=colx(0), $
  orientation=ori(0), /line_fill, thick=2,/data
 ; oplot,merg(w5).f,merg(w5).d,thick=2
 oplot,merg(w5).f,smooth(merg(w5).d-merg(w5).e,sm_mod,/edge),$
  thick=thx,col=coly(0)
 oplot,merg(w5).f,smooth(merg(w5).d+merg(w5).e,sm_mod,/edge),$
  thick=thx,col=coly(0)
 if sep_plot then begin
  oplot,pobs.f2,pobs.d2,thick=thx_obs
  if n_elements(pobs2) ge 10 then oplot,pobs2.f2,pobs2.d2,thick=thx_obs
  wire_dens_txtout, $
   merg(w5).tgran, merg(w5).agran*facgran, merg(w5).amode/facpmode, $
   merg(w5).lifet, merg(w5).wn,colx=colx,addyear=addyear
 endif
endif



if c7 eq 1 then begin
 if sep_plot then $
 plot_oo,merg(w5).f,merg(w5).d,xr=[x1,x2],yr=[y1,y2],xsty=1,ysty=1,/nodata,$
  xtit='Frequency [microHz]',ytit='Power Density [ppm!E2!N / microHz]',$
  xthick=2,ythick=2,charsi=1.2,charthick=2

 a = [merg(w7).f, reverse(merg(w7).f)] ; reverse---> make a well-defined polygon!
 b = [        smooth(merg(w7).d+merg(w7).e,sm_mod,/edge),$
      reverse(smooth(merg(w7).d-merg(w7).e,sm_mod,/edge))] 
 ; like drawing w/ a pencil!

 w = where(a gt x1,c) & polyfill,a(w),b(w),color=colx(1), $
  orientation=ori(1), /line_fill, thick=2,/data
 ; oplot,merg(w7).f,merg(w7).d,thick=2
 oplot,merg(w7).f,smooth(merg(w7).d-merg(w7).e,sm_mod,/edge),$
  thick=thx,col=coly(1)
 oplot,merg(w7).f,smooth(merg(w7).d+merg(w7).e,sm_mod,/edge),$
  thick=thx,col=coly(1)

 if sep_plot then begin
  oplot,pobs.f2,pobs.d2,thick=thx_obs
  if n_elements(pobs2) ge 10 then oplot,pobs2.f2,pobs2.d2,thick=thx_obs
  wire_dens_txtout, $
   merg(w7).tgran, merg(w7).agran*facgran, merg(w7).amode/facpmode, $
   merg(w7).lifet, merg(w7).wn,colx=colx,addyear=addyear
 endif
endif

if c1 then begin
 if sep_plot then $
 plot_oo,merg(w5).f,merg(w5).d,xr=[x1,x2],yr=[y1,y2],xsty=1,ysty=1,/nodata,$
  xtit='Frequency [microHz]',ytit='Power Density [ppm!E2!N / microHz]',$
  xthick=2,ythick=2,charsi=1.2,charthick=2
 a = [merg(w1).f, reverse(merg(w1).f)] ; reverse---> make a well-defined polygon!
 b = [        smooth(merg(w1).d+merg(w1).e,sm_mod,/edge),$
      reverse(smooth(merg(w1).d-merg(w1).e,sm_mod,/edge))] 
 ; like drawing w/ a pencil!

 w = where(a gt x1,c) & polyfill,a(w),b(w),color=colx(2), $
  orientation=ori(2), /line_fill, thick=2,/data
 ; oplot,merg(w1).f,merg(w1).d,thick=2
 oplot,merg(w1).f,merg(w1).d-merg(w1).e,thick=thx,col=coly(2)
 oplot,merg(w1).f,merg(w1).d+merg(w1).e,thick=thx,col=coly(2) 
endif

oplot,pobs.f2,pobs.d2,thick=thx_obs
if n_elements(pobs2) ge 10 then oplot,pobs2.f2,pobs2.d2,thick=thx_obs



if sep_plot eq 0 then $
if c5 eq 1 then $
 wire_dens_txtout, $
  merg([w5,w7,w1]).tgran, $
  merg([w5,w7,w1]).agran*facgran, merg([w5,w7,w1]).amode/facpmode, $
  merg([w5,w7,w1]).lifet, merg([w5,w7,w1]).wn,$
   colx=colx,addyear=addyear + ' (' + bestfit + ')'
if c5 eq 0 then $
 wire_dens_txtout, merg([w7,w1]).tgran, merg([w7,w1]).agran*facgran, $
  merg([w7,w1]).amode/facpmode, $
  merg([w7,w1]).lifet, merg([w7,w1]).wn,$
   colx=colx([1,2]),addyear=addyear + ' (' + bestfit + ')'


if sep_plot eq 1 then $
 wire_dens_txtout, $
  merg(w1).tgran, merg(w1).agran*facgran, merg(w1).amode/facpmode, $
  merg(w1).lifet, merg(w1).wn,colx=colx,addyear=addyear

if most then begin
 most_file = '~/wire/procyon/most_density.idl'
 restore,most_file  
 oplot,most_freq,most_dens2,col=col_most,thick=thx, $
   psym=6,symsi=.07
endif


if dops then begin
      Device, /Close_File
      Set_Plot, thisDevice
      !P.Font = thisFont
      set_plot,'x'
   print,' $  ggv ' + keywords.filename + '  & '
endif




END
