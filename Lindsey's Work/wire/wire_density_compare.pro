PRO wire_density_compare, wildcard, obsfile, $
 readonly=readonly, comp=comp, silent=silent, merg=merg, debug=debug,$
 fmax=fmax

; Example:
; wire_density_compare, $
; '~/wire/wire_sim99/density/procyon_1999.gt*.density',$
; '~/wire/wire_amp/wire_lc_Procyon_1999.amp.idl'

;if p2 then begin
; restore,'~/wire/wire_amp/wire_lc_Procyon_2000.amp.idl' ; p00nov
; if raw then oplot,p00nov.f,smooth(p00nov.d,61),col=colp(1),thick=2
; oplot,p00nov.f,p00nov.d2,col=colp(0),thick=thx_obs
;endif

;if p9 then begin
; restore,'~/wire/wire_amp/wire_lc_Procyon_1999.amp.idl' ; p00nov
; oplot,p99nov.f2,p99nov.d2,col=colp99,thick=thx_obs
;endif
; ====================================
; Make sure the input files exist:
; ====================================
default9, silent, 0B
default9, readonly, 0B ; only read data = no plots

spawnrob,'ls -1 ' + wildcard, list
nl = n_elements(list)
if nl le 1 then begin ; added 31st of January 2004 by HB (> 1000 simulations)
 spawnrob,'pwd',org_dir
 dirr = '/'
 gg = strsplit(wildcard,'/',/extract) & ngg = n_elements(gg)
 ws = where(strmatch(gg,'*.density*') eq 1,cs)
 ngg = max(ws)
 for ll=0,ngg-1 do dirr = dirr + gg(ll) + '/'
 cd,dirr
 spawnrob,'ls -1 ' + gg(ngg), list
 nl = n_elements(list)
 list = dirr + list
 cd, org_dir
endif

 list2 = list

if nl le 0 or n_elements(merg) eq 1 then begin
 print,' %%% No files found: ' + wildcard
 RETURN
endif
g = findfile(obsfile,Count=cnt)
if ((cnt ne 1) and (readonly eq 0)) and (n_elements(merg) eq 0) then begin
 print,' %%% Obs file not found: ' + obsfile
 RETURN
endif
; ====================================

default9, dops , 0B
default9, fmax , 7000
default9, npover, 30.
default9, debug, 0B


   w99 = where(strmatch(wildcard,'*1999*') eq 1,c99)
   w00 = where(strmatch(wildcard,'*2000*') eq 1,c00)
   addyear = 'XXXX' & if c99 eq 1 then addyear = '1999'
   if c00 eq 1 then addyear = '2000'

; Get filename
   s = strsplit(wildcard,'/',/extract) & ns = n_elements(s)
   if s(0) ne '~' then begin
     dir = '/' 
     js = 0 
   endif else begin
     dir = '~/'
     js = 1
   endelse
    for j=js,ns-2 do dir = dir + s(j) + '/'

; Save file:
fileout = dir + 'Procyon_sim_comp_' + addyear + '.idl'

if readonly eq 0 and dops then begin
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


if readonly eq 0 then begin
 restore,obsfile ; restore the observed density plot
; if addyear eq '2000' then obs = p00novM5
; if addyear eq '1999' then obs = p99novM5
 if addyear eq '2000' then obs = p00l ; FEB18 2005
 if addyear eq '1999' then obs = p99l ; FEB18 2005
 col=getcolor(/load)

fmm = 2100.
np = n_elements(obs)
df = obs(1:np-1).d2 - obs(0:np-2).d2



; Beware if you change the low freq part! (search for "lo freq")
wj = where(df lt -1 and obs.f2 gt 150. and obs.f2 lt fmm,cj)
wj2 = where(df gt 1 and obs.f2 gt 150. and obs.f2 lt fmm,cj2)
freq = fltarr(2,100)

if cj eq 0 or cj2 eq 0 then begin
 print,' %%% No gaps in freq. detected ...' 
 print,' %%% Program will skip to skip_gap'
 freq(0,0) = 10.
 freq(1,0) = 50.
 uu=1L

 if debug then $
  plot_oo,obs.f2,obs.d2,yr=[.1,550],ysty=1,xr=[1,7000],xsty=1

 goto,skip_gap
endif

if debug then begin
; plot,df,yr=[-5,5],psym=6,symsi=.5
 plot_oo,obs.f2,obs.d2,yr=[.1,550],ysty=1,xr=[1,7000],xsty=1
 oplot,obs(wj).f2,obs(wj).d2,psym=4,col=col.red
 oplot,obs(wj2+1).f2,obs(wj2+1).d2,psym=6,col=col.sky
 hitme,s & if s eq 'x' then stop
endif

uu = min([cj,cj2])

freq(0,0) = 50. & freq(1,0) = 120 & add = 1 ; lo freq part!

freq(1,add:add+uu-1) = obs(wj(0:uu-1)).f2
freq(0,add:add+uu-1) = obs(wj2(0:uu-1)+1).f2
uu = uu + add

if freq(0,0) gt freq(1,0) then begin
 print,' %%% freq(0,0) > freq(1,0) !!'
 stop
endif 

skip_gap:

f_start1 = alog10(freq(1,uu-1) * 1.1) & f_slut1 = alog10(fmax*0.90)
f_start2 = alog10(freq(1,uu-1) * 1.2) & f_slut2 = alog10(fmax*0.95)
logx1 = findgen(npover) * (f_slut1 - f_start1) / (npover-1.) + f_start1
logx2 = findgen(npover) * (f_slut2 - f_start2) / (npover-1.) + f_start2
freq(0,uu:uu+npover-1) = 10.^(logx1)
freq(1,uu:uu+npover-1) = 10.^(logx2)
 
freq = freq(*,0:uu+npover-1)
nfreq = n_elements(freq(0,*))

if debug then begin
 plot_oo,obs.f2,obs.d2,yr=[.7,50],ysty=1,xr=[50,7000],xsty=1
 for k=0,nfreq-1 do oplot,[1.,1]*freq(0,k),[.7,100],psym=-6,col=col.sky
 for k=0,nfreq-1 do oplot,[1.,1]*freq(1,k),[.7,100],psym=-6,col=col.red
 hitme,s
endif

; Empty plot:
;plot_oo,[0,1],/nodata,$
; xtit='Frequency [!4l!3Hz]',ytit='Power Density [ppm!E2!N/!4l!3Hz]',$
; xr=[x1,x2],yr=[y1,y2],xsty=1,ysty=1,xthick=2,ythick=2,charthick=2.0

endif                           ; readonly mode?

if n_elements(merg) ge 1 then nl = n_elements(merg)


; Structure with results:
default9, nfreq, 1B ; for readonly mode!
comp = replicate({d:fltarr(3,nfreq), dif:-999., chi2:-99., $
 chi2r:-99., spread:-1., spreadr:-1., $
 ratio:-1.,$
 tgran:-1., agran:-1., amode:-1., lifet:-1., wn:-1., $
 nam:''}, nl)


for i=0,nl-1 do begin

if n_elements(merg) eq 0 then begin
 restore,list(i) ; get psim structure
 comp(i).nam = list(i) ; store original filename
endif else begin
 nd = n_elements(merg(i).f)
 psim = replicate({f2:0., d2:0.}, nd)
 psim.f2 = merg(i).f  &  psim.d2 = merg(i).d
endelse


; if rawsim then $
;  oplot,psim.f, smooth(psim.d,61,/edge), col=colx(i mod ncol),thick=thx
; oplot,psim.f2, psim.d2, col=colx(i mod ncol),thick=thx

if n_elements(merg) eq 0 then begin
 s99 = strsplit(list(i),'/',/extract) & n99 = n_elements(s99)
 list2(i) = s99(n99-1)

 s = strsplit(list2(i),'_',/extract)
 p = where(strmatch(s,'*cyon*',/fold) eq 1,ccp)
 init = p(0) - 1

 pii = where(strmatch(s,'*s0*',/fold) eq 1,cii)
 if cii eq 1 then init = init + 2

; 21 Feb 2005:
 pii = where(strmatch(s,'*sublow*',/fold) eq 1,cii)
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
endif else begin
 timegran   = merg(i).tgran
 ampgran    = merg(i).agran
 ampmode    = merg(i).amode
 lifetime   = merg(i).lifet
 whitenoise = merg(i).wn
endelse

 comp(i).tgran = timegran
 comp(i).agran = ampgran
 comp(i).amode = ampmode
 comp(i).lifet = lifetime
 comp(i).wn    = whitenoise

; ========================================================
if readonly eq 0 then begin
; ========================================================
; Determine difference btw. simulation and observations
; ========================================================

for j=0,nfreq-1 do begin
 wd = where(obs.f2 gt freq(0,j) and obs.f2 lt freq(1,j),cdata)
 if cdata ge 3 then begin
  resistant_mean, obs(wd).d2, 3, me, sd, nr
  comp(i).d(0,j) = me
 endif

 ws = where(psim.f2 gt freq(0,j) and psim.f2 lt freq(1,j),csim)
 if csim ge 3 then begin
  resistant_mean, psim(ws).d2, 3, me_sim, sd_sim, nr_sim
  comp(i).d(1,j) = me_sim
 endif

endfor

comp(i).d(2,*) = (comp(i).d(0,*) - comp(i).d(1,*)) / comp(i).d(0,*)
comp(i).dif = median(comp(i).d(2,*))
comp(i).chi2 = total( (comp(i).d(0,*) - comp(i).d(1,*))^2.)
comp(i).chi2r = total( ((comp(i).d(0,*) - comp(i).d(1,*))/comp(i).d(0,*))^2.)
comp(i).spreadr = robust_sigma(comp(i).d(2,*))
comp(i).spread  = stdev(comp(i).d(2,*))

; More subtle index: sum of multiplying rations SIM / OBS:
;f_ref = 1000. ; Reference ration at this frequency
;mf = (freq(0,*) + freq(1,*) ) * 0.5
;dref  = abs(mf - f_ref)
;wref = where(dref eq min(dref),cref) & wref = wref(0)
;comp(i).ratio   = 1.0 ; perfect ratios at all frequencies
;; Multiply the ratios:
;for pp=0,nfreq-1 do $
; comp(i).ratio = comp(i).ratio * (comp(i).d(1,pp) / comp(i).d(0,wref))

comp(i).ratio   = 1.0 ; perfect ratios at all frequencies
r1 = 2 & r2 = 14 ; for pp=0,nfreq-1 do $
for pp=r1,r2 do $
 comp(i).ratio = comp(i).ratio * ( comp(i).d(1,pp) / comp(i).d(0,pp) )

; Progress plot:
;plot,(freq(0,*)+freq(1,*) ) * 0.5, comp(i).d(2,*),psym=-6,$
; xr=[50,fmax],yr=[-1,1],xsty=1,ysty=1, $
; xtit='Frequency [microHz]',ytit='(OBS - SIM) / OBS'

; ========================================================
endif ; readonly?
; ========================================================
 
; ========================================================
goto,skipthis
; ========================================================

 w = where(psim.f2 gt 1e4,c)
 ypos = median(psim(w).d2)
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

; for it = 0, nxx-1 do $
;  xyouts,kx(it),k(i),txtout(it),$
;   col=colx(i mod ncol), charsi=0.9, charthick=2

; Write explanation of numbers on the plot
; ori = replicate(270,6) & ori(0) = 0.
; if i eq (nl-1) then for j=0,nxx-1 do $
;   xyouts, kx(j)*1.1, k(i+1), txtout2(j), $
;    col=colx(i mod ncol), charsi=1.1, charthick=2, $
;    alignment = 1., orientation = ori(j);

; ========================================================
skipthis:
; ========================================================

if silent eq 0 then print,i,format='(I4,$)'


endfor

if silent eq 0 then print,''


if readonly eq 0 and dops then begin
      Device, /Close_File
      Set_Plot, thisDevice
      !P.Font = thisFont
      set_plot,'x'
   print,' $  ggv ' + keywords.filename + '  & '
endif

if n_elements(freq) ge 1 then $
 save,filename=fileout,comp,freq else $
 save,filename=fileout,comp
 
print,' %%% Saved comparison structure as: ' 
print,'      ' + fileout


END
