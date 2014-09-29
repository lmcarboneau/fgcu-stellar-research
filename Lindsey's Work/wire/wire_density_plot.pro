PRO wire_density_plot, wildcard, $
 p2=p2, p9=p9, px2=px2, px9=px9, most=most, $
  x1=x1, x2=x2, y1=y1, y2=y2, dops=dops, raw=raw, rawsim=rawsim, $
   numout=numout, fnum=fnum, colon=colon

; Example:
; wire_density_plot,/p2,y1=.7,y2=50,x1=100,x2=1e4,/dops,/numout,$
; '~/wire/wire_lc/p00_fine/procyon_2000.gt*.density',/colon,fnum=750


spawnrob,'ls -1 ' + wildcard, list
nl = n_elements(list)
if nl le 0 then RETURN

default9, most, 0B
default9, p2, 0B
default9, p9, 0B
default9, px2, 0B
default9, px9, 0B
default9, dops, 0B
default9, raw, 0B
default9, rawsim, 0B
default9, numout, 0B
default9, fnum, 1000. ; microHz for placing the ID numbers
default9, colon, 0B


   w99 = where(strmatch(wildcard,'*1999*') eq 1,c99)
   w00 = where(strmatch(wildcard,'*2000*') eq 1,c00)
   addyear = 'XXXX' & if c99 eq 1 then addyear = '1999'
   if c00 eq 1 then addyear = '2000'



if dops then begin
   s = strsplit(wildcard,'/',/extract) & ns = n_elements(s)
   if s(0) ne '~' then begin
     dir = '/' 
     js = 0 
   endif else begin
     dir = '~/'
     js = 1
   endelse
 
   for j=js,ns-2 do dir = dir + s(j) + '/'


   startFilename = 'Procyon_'+addyear+'.ps'

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



; X and Y range:
if n_elements(x1) eq 0 then  x1 = 100.
if n_elements(x2) eq 0 then  x2 = 10000.
if n_elements(y1) eq 0 then  y1 = .7
if n_elements(y2) eq 0 then  y2 = 50.

col = getcolor(/load)
colx  = [col.green, col.red, col.sky, col.magenta, col.cyan]
colx2 = [col.green, col.red, col.sky, col.magenta, col.cyan]
colp00 = col.white & colp99 = col.white & col_most = col.white

thx = 1.0 & thx_obs = 2.0
if dops then begin
 colp00 = 0 & colp99 = 0 & col_most = 50
 if p2 and p9 then p9 = 100
 thx = 2.0 & thx_obs = 4.0
endif

if dops then colx = [0,50,100,150,200]
if dops and colon then colx = colx2
colp = [100,200]
ncol = n_elements(colx)

; Empty plot:
plot_oo,[0,1],/nodata,$
 xtit='Frequency [!4l!3Hz]',ytit='Power Density [ppm!E2!N/!4l!3Hz]',$
 xr=[x1,x2],yr=[y1,y2],xsty=1,ysty=1,xthick=2,ythick=2,charthick=2.0


for i=0,nl-1 do begin

 restore,list(i)
 if rawsim then $
  oplot,psim.f, smooth(psim.d,61,/edge), col=colx(i mod ncol),thick=thx
 oplot,psim.f2, psim.d2, col=colx(i mod ncol),thick=thx

 ; ~/wire/wire_lc/procyon_2000.gt500.0_aG1.0_aP0.0.dat.density
 s = strsplit(list(i),'_',/extract)
 p = where(strmatch(s,'*cyon*',/fold) eq 1,ccp)
 init = p(0) - 1
 pii = where(strmatch(s,'*s0*',/fold) eq 1,cii)
 if cii eq 1 then init = init + 2

 s1a = strsplit(s(init+2),'t',/extract)
 timegran = float(s1a(1))
 s1b = strsplit(s(init+3),'G',/extract)
 ampgran = float(s1b(1))
 s1b = strsplit(s(init+4),'P',/extract)
 ampmode = float(s1b(1))
 s1b = strsplit(s(init+5),'T',/extract)
 lifetime = float(s1b(1))
 s1b = strsplit(s(init+6),'N',/extract)
 s1c = strsplit(s1b(1),'.',/extract)
 whitenoise = float(s1c(0)) + float(s1c(1)) / 10.
 
 w = where(psim.f2 gt 1e4,c)
 if c ge 50 then begin
  ypos = median(psim(w).d2)
 endif else begin
  ypos = min(psim.d2) * 1.05
 endelse

 xpos = x1 * 2.0 ; (x2-x1)*0.05 + x1

  txtout2 = strarr(6)
  txtout2(0) =  '' & txtout2(1) = 't!IG!N [s]'
  txtout2(2) = 'a!IG!N [ppm]' & txtout2(3) = 'a!Im!N [ppm]'
  txtout2(4) = '!4g!3 [d]' & txtout2(5) = 'WN [ppm]'

 if numout then begin ; print numbers on plot ?
  nnf = n_elements(fnum)
  for gi=0,nnf-1 do begin
   d_num = fnum(gi) * 0.2 ; 150. ; central freq and width
   whz = where( abs(psim.f2 - fnum(gi)) lt d_num,chz)
   ypp = median(psim(whz).d2)
   xyouts, fnum(gi), ypp*1.05,strcompress(i,/remove_all), $
    col = colx(i mod ncol),charsi=0.8,charthick=2
  endfor
  txtout2(0) = 'ID'
 endif

 txtout = strarr(6)
 ; lifetime = 1.0 ; 1.0 days Hans K

 txtout(0) = strcompress(i,/remove_all)
 if numout eq 0 then txtout(0) = ''
 txtout(1) = strcompress(string(timegran,format='(I8)'),/remove_all)
 txtout(2) = strcompress(string(ampgran,format='(F8.2)'),/remove_all)
 txtout(3) = strcompress(string(ampmode,format='(I8)'),/remove_all)
 txtout(4) =strcompress(string(lifetime,format='(F9.1)'),/remove_all)
 txtout(5) =strcompress(string(whitenoise,format='(I9)'),/remove_all)
  
 log_y2a = alog10(y2 *  .4) & log_y1a = alog10(y2*.15) ; upper 
 ; log_y1a = alog10(y1 *  1.5) & log_y2a = alog10(y1 *  8.0) ;lower
  log_k = findgen(nl+1) * (log_y2a - log_y1a) / (nl-1.+1.) + log_y1a
 k = 10.^(log_k) ; & k = reverse(k)

 nxx = 6 ; antal data ud paa plot (ID, timeG, ampG, ampP, lifetime, wn)
 log_x1a = alog10(x2 * 0.15) & log_x2a = alog10(x2 * 0.7) ; range x
  log_kx = findgen(nxx) * (log_x2a - log_x1a) / (nxx-1.) + log_x1a
 kx = 10.^(log_kx)

 for it = 0, nxx-1 do $
  xyouts,kx(it),k(i),txtout(it),$
   col=colx(i mod ncol), charsi=0.9, charthick=2

; Write explanation of numbers on the plot
 ori = replicate(270,6) & ori(0) = 0.
 if i eq (nl-1) then for j=0,nxx-1 do $
   xyouts, kx(j)*1.1, k(i+1), txtout2(j), $
    charsi=1.0, charthick=2, $
    alignment = 1., orientation = ori(j)

 ; print,' Time: ' , timegran

endfor

if px2 then begin
 restore,'~/wire/wire_amp/wire_lc_Procyon_2000.amp.idl' ; p00nov
 if raw then oplot,p00nov.f,smooth(p00nov.d,61),col=colp(1),thick=2
 oplot,p00nov.f,p00nov.d2,col=colp(0),thick=thx_obs
endif

if p2 then begin
 restore,'~/wire/wire_amp/wire_lc_Procyon_2000_mer5.amp.idl' ; p00novM5
 if raw then oplot,p00nov.f,smooth(p00nov.d,61),col=colp(1),thick=2
 oplot,p00novM5.f,p00novM5.d2,col=colp(0),thick=thx_obs
endif


if px9 then begin
 restore,'~/wire/wire_amp/wire_lc_Procyon_1999.amp.idl' ; p00nov
 oplot,p99nov.f2,p99nov.d2,col=colp99,thick=thx_obs
endif

if p9 then begin
 restore,'~/wire/wire_amp/wire_lc_Procyon_1999_mer5.amp.idl' ; p00novM5
 oplot,p99novM5.f2,p99novM5.d2,col=colp99,thick=thx_obs
; restore,'~/wire/wire_amp/wire_lc_Procyon_1999.amp.idl' ; p00nov
; oplot,p99nov.f2,p99nov.d2,col=colp99,thick=thx_obs
endif



if most then begin

 most_file = '~/wire/procyon/most_density.idl'
goto,skip_comp_most

 readcol,$
  '~/wire/procyon/most_ampspec_Fig3_in_bedding_paper.DAT',$
   fmost,dmost,format='F,F'
 fmost = fmost * 1000. ; transform from milliHz to MicroHz

 density_fac = (1e6/(32. * 0.99 * 86400.)) & sm_fac_most = 51
 forb = 328.5*0.5

 wire_exclude_orbital,fmost, dmost,fny,dny,$
  forb=forb,df=11.,inc=.001

 wg = where(fny gt 1e-3,cg)
 most_freq = fny(wg) & most_amp = dny(wg)

 wire_power_smooth, most_freq, most_amp, most_amp2, $
   df=150.,fref=1000.,res=0.1 ; ,/debug

 most_dens  = (most_amp^2.0) /density_fac
 most_dens2 = (most_amp2^2.0)/density_fac ; smoothed version

 save,filename=most_file, $
   most_freq, most_amp, most_amp2, most_dens, most_dens2

 print,' %%% Saved file: ' + most_file

skip_comp_most:
 restore,most_file 
 
  oplot,most_freq,most_dens2,col=col_most,thick=thx, $
    psym=6,symsi=.07

endif ; plot most?


xyouts,x1*1.3,y1*1.5,'Procyon ' + addyear, $
 charsi=1.6,charthick=3.0


if dops then begin
      Device, /Close_File
      Set_Plot, thisDevice
      !P.Font = thisFont
      set_plot,'x'
   print,' $  ggv ' + keywords.filename + '  & '

endif


END
