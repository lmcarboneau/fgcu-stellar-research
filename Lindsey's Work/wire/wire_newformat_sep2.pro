PRO wire_newformat_sep2, fil, ranger=ranger

; This program will separate a WIRE data file
; for two primary objects. This is mandatory
; for data from december 2003 or later

; Example:
; --------------
;  spawnrob, 'ls -1 /data1/bruntt/wire/AlphaUMi/wire_AlphaUMi_*.idl',fil
;  wire_newformat_sep2, fil

nf = n_elements(fil)
print,' %%% Reshuffling data for '+string(nf,format='(I4)') + ' files...'

ploton = 0B
edge_check = 0B ; option to deselect points
spawnrob,'hostname',host
if strmatch(host,'*usafa*') then edge_check = 1B
if edge_check then ploton = 1B

 ; accepted range in magnitude for reference object:
if n_elements(ranger) eq 0 then ranger = 0.10

; ==============================================================
; Pick the file to be used for defining the REFERENCE magnitude:
nref = ceil(nf * 0.30) ; recommended: nf * 0.30
; nref = ceil(nf * 0.85)
; ==============================================================

fcnt = nref
 done_ref = 0B

; Set datlim > 2^16. to include all data points
datlim = 70000. ; was 15000 ---> may data points are discarded (~50%)

; ==============================================================
backccd = intarr(8,8)
backccd(0:1,0) = 1    &  backccd(0,0:1) = 1
backccd(6:7,0) = 1    &  backccd(0,6:7) = 1
backccd(7,0:1) = 1    &  backccd(0:1,7) = 1
backccd(6:7,7) = 1    &  backccd(7,6:7) = 1
wback = where(backccd eq 1,cback)
; ==============================================================

; ==============================================================
; for all .idl files ...
; ==============================================================
while fcnt lt nf do begin 

; ===================================================
print,' %%% Restoring: '+fil(fcnt) + ' ' + $
 string(fcnt+1,format='(I4)') + '/' + string(nf,format='(I4)')
restore,fil(fcnt)
; ===================================================

; ===================================================
window,0,xsize=950,ysize=550,title='WIRE reshuffling of data'
col = getcolor(/load)
colx = [col.white, col.green, col.yellow, col.red, col.sky]

colx1= [col.white, col.yellow, col.cyan, col.magenta, col.pink]
colx2= [col.sky, col.magenta, col.red, col.navy, col.orchid]

nstar = n_elements(wire(0).d(*,0,0))
d1 = 0L 
d2 = n_elements(wire.hjd(0))-1 
st = 20 ; take every 20'th data point (fast!)
; ===================================================

; plot,[0,1],/nodata,xr=[-.5,0.1],yr=[18,10],ysty=1,xsty=1,$
; if ploton then $
 plot,[0,1],/nodata,xr=[-.5,.5],yr=[18,10],ysty=1,xsty=1,$
  xtit='HJD',ytit='Magnitude'
 ndat = (d2-d1 + 1) / st
 datuse = d1 + findgen(ndat) * st
; ===================================================

; ===================================================
 flux = fltarr(nstar,ndat)
 back = flux
 me = fltarr(2,nstar)
 times = dblarr(nstar,ndat)
 mag = flux
; ===================================================

; ===================================================
 ass = sort(wire.hjd(0))
 wire = wire(ass) ; sort HJD for the primary star
; ===================================================

; ===================================================
; Find the median time, t0:
; ===================================================
 w0 = where(wire.d(0,3,3) gt 50,c0)
 t0 = double(long(median(wire(w0).hjd(0))*10.))/ 10.
; ===================================================

; ===================================================
; For each slot (1-5):
; ===================================================
for star=0,nstar-1 do begin
 
 ccd = reform(wire(datuse).d(star,*,*))
 ; print,' Star = '+string(star,format='(I3)')+': ', format='(A12,$)'

; ===================================================
 for i=0L,ndat-1 do begin
  i2 = i ; * st
  ; if i2 mod (5000.) eq 0 then print,i2,format='(I7,$)'
  dat = reform(ccd(*,*,i2))
  ; back_temp = [ dat(0,0), dat(0,7), dat(7,0), dat(7,7) ] 
  back_temp = dat(wback) ; 12 pixels !!
  resistant_mean,back_temp,3,backgr,sd,nr
  flux(star,i) = total(dat(2:5,2:5)) -  backgr * 16. ; 16 pixels summed!
  back(star,i) = backgr
 endfor
; ===================================================

 ; Good data ...
 w = where(flux(star,*) gt 50. and back(star,*) lt 1000.,c,complement=wbad)
 
 mag(star,w) = -2.5 * alog10( flux(star,w) ) + 25.
 if n_elements(wbad) ge 2 then mag(star,wbad) = -1.0
 me(0,star) = median(mag(star,w))
 me(1,star) = robust_sigma(mag(star,w))

 tt    = wire(datuse).hjd(star) - t0
 times(star,w) = tt(w) 

; if ploton then $
 oplot,times(star,w),mag(star,w), psym=3, col = colx(star)

; Progress:
 print,' %%% Zero point in time was HJD = : ',t0
 print,' %%% WIRE stamp values:'
; for sm9=0,9 do $
sm9 = 0
 for xx9=0,4 do print,wire(sm9).stamp(xx9)

 ; print,' Hit any key ' & s = get_kbrd(1)
 
endfor
; ===================================================


; ===================================================
if done_ref eq 0 then begin

print,$
 ' %%% Mark approx. "start" and "end" time of a typical observing slot'
print,$
 ' %%% Click on the approx. magnitude of the brighest (main) target!'

cursor,x1,y1,/up
cursor,x2,y2,/up
print,'' & print,' %%% Thanks!' & print,' '

yy = avg([y1,y2])
datref = dblarr(2,5000)
cnt = 0L

maglim_rough = 0.05

; ===================================================
for star=0,nstar-1 do begin
 w = where(times(star,*) ge x1 and times(star,*) le x2 and $
           abs(mag(star,*)-yy) lt maglim_rough,c)
 if c ge 10 then begin
   datref(0,cnt:cnt+c-1) = times(star,w)
   datref(1,cnt:cnt+c-1) = mag(star,w)
   cnt = cnt + c
 endif
endfor
; ===================================================

datref = datref(*,0:cnt-1)

asort = sort(datref(0,*)) ; Sort by times
datref = datref(*,asort)
;if ploton then $
 oplot,datref(0,*),datref(1,*),psym=1,symsi=.8,col=col.green

magref = fltarr(2)
magref(0) = median(datref(1,*))
magref(1) = robust_sigma(datref(1,*))

;if ploton then begin
 plots,!x.crange,magref(0) + magref(1) * 8.,col=col.green,line=2
 plots,!x.crange,magref(0) - magref(1) * 8.,col=col.green,line=2
;endif

endif ; only for the reference magn. --> done only once!
; ===================================================


; ===================================================
maglim = max([magref(1),ranger]) * 1.0 ; you may need to change increase / decrease 1.0 value?
;print,' %%% Mag in range: ' + strcompress(magref(0)) + ' +- ' + $
;                              strcompress(maglim)

; Over plot the limits:
 plots,!x.crange,magref(0) + maglim,line=5
 plots,!x.crange,magref(0) - maglim,line=5
 if fcnt eq 0 then begin
    hitme,s9 & if s9 eq 'x' then stop
 endif

; ===================================================

; ===================================================
; Collect all data fitting the magref magnitude:
; Find jumps magn ---> shift of target
; ===================================================

; ===================================================
datjump = dblarr(2,15000)
cnt = 0L
; ===================================================

; ===================================================
; For each slot:
; ===================================================
for star=0,nstar-1 do begin
 w = where(abs( mag(star,*)-magref(0) ) lt maglim,c)
 if c ge 10 then begin
   datjump(1,cnt:cnt+c-1) = mag(star,w)
   datjump(0,cnt:cnt+c-1) = times(star,w)
   cnt = cnt + c
 endif
endfor
; ===================================================


; ===================================================
if cnt eq 0 then begin
 print,''
 print,' %%% The reference magnitude at : ' + $
   string(magref(0),format='(F5.1)') + ' not found (red. dashed curve) !!'
 print,' %%% Your separation of the light curves may not be correct ... '
 print,''
 print,' %%% Possible explanations:'
 print,' %%% The current file: '
 print,'     '+ fil(fcnt)
 print,'     does not contain prim. target?'
 print,'     Solution: Delete that file (probl. the first file=*001?)'
 print,' '
 print,' %%% Alternatively, try to use another bright star for' + $
  ' separating the data, '
 print,' %%% e.g. try using the other main target! ' + $
  'Remember to choose a star with a '
 print,' %%% magnitude significantly different from ANY other star !!'
 print,''
 ; if ploton then $
  plots,!x.crange,magref(0),line=2,col=col.red,thick=2

 ; +++++++++++++++++++++++++++++++++++++++++++++
 ; Added by HB 6 Feb. 2007:
 ; +++++++++++++++++++++++++++++++++++++++++++++
 hitme,mess=' %%% Skip to the next file? ',s9
 if s9 eq 'y' then begin
   fcnt = fcnt + 1 ; increase counter
   goto,problem_go_next_file
 endif else stop
 ; +++++++++++++++++++++++++++++++++++++++++++++

endif
; ===================================================

; ===================================================
; Remove unused entries and sort by HJD time:
datjump = datjump(*,0:cnt-1)
asorter = sort(datjump(0,*)) ; the time in HJD
datjump = datjump(*,asorter)
; ===================================================

; ===================================================
; typical_timestep = median(dt)
; ===================================================
dt      = reform(datjump(0,1:cnt-1) - datjump(0,0:cnt-2))
jumplim = 1200. / 86400. ; typical_timestep * 15.
wj      = where( abs(dt) gt jumplim,cj )
; ===================================================

;if ploton then begin
 plot,times(0,*),mag(0,*),psym=3,yr=[20,10],xr=[-.5,0],/nodata
 for u=0,4 do oplot,times(u,*),mag(u,*),col=colx(u),psym=1,symsi=.1
 plots,0-.3,!y.crange,col=col.green ; min jump in time ...
 plots,jumplim-.3,!y.crange,col=col.green

 for k=0,cj-1 do $
  oplot, [1.,1.]*datjump(0,wj(k)), [10.,14],col=col.magenta,line=5
;endif

; ===================================================
cj2 = 1 + cj * 2 + 1            ; points at end and start (+1 & +1)
jump = fltarr(cj2)
jump(0) = min(datjump(0,*))
jump(cj2-1) = max(datjump(0,*))
for j=0,cj-1 do begin
 jump(1 + j * 2) = datjump(0,wj(j))
 jump(1 + j * 2 + 1) = datjump(0,wj(j)+1)
endfor

;jump2 = fltarr(cj2 + 4)
;jump2(0) = min(jump) - 0.05 & jump2(1) = min(jump) 
;jump2(2:cj2+2-1) = jump

;if ploton then begin
 for k=0,cj*2+2-1,2 do $
  oplot,[1.,1.] * jump(k),[20.,14],$
  col=col.red
 for k=0+1,cj*2+2-1,2 do $
  oplot,[1.,1.] * jump(k),[20.,14],$
  col=col.red,line=5,thick=2
;endif

; ===================================================

;for s=0,nstar-1 do begin ; find abs. min & max times !!
;wok = where(wire.d(*,2,2) gt 50.,c)
;mint = min(wire(wok).hjd)

; Is this the first run to get approx. reference magn.
if (done_ref) eq 0 and (fcnt eq nref) then begin
 fcnt = 0
 done_ref = 1B
endif else begin

nmax = n_elements(wire)
wire1 = replicate( {x: fltarr(nstar), y: fltarr(nstar), hjd: dblarr(5), $
                    stamp: strarr(nstar), d: fltarr(nstar,8,8)}, nmax)
wire1.x = -99 & wire1.hjd = -3e4
wire1.d(*,3,3) = -1e6; clearly mark bad dat!
wire2 = wire1

cnt1 = fltarr(nstar) ; counter
cnt2 = cnt1

; Progress plot:
if ploton then $
 plot_io,[2,1],xr=[-1,1],yr=[1,65000],ysty=1,xsty=1,$
  xtit='HJD',ytit='Counts in pixel (3,3)',charsi=1.2,$
   tit='Progress plot!'

; For each time interval:
 for j=0,cj2-2,2 do begin

  
;  if ploton then begin
   oplot,jump(j)  *[1.,1],[1,65000],col=col.yellow,thick=5
   oplot,jump(j+1)*[1.,1],[1,65000],col=col.yellow,line=5,thick=3
;  endif


  for star=0,nstar-1 do begin
   tt = wire.hjd(star) - t0

   w = where(tt ge jump(j) and $
             tt lt jump(j+1) and $
             wire.hjd(star) gt 5.2e4 and wire.hjd(star) lt 5.9e4 and $
             wire.d(star,0,0) lt datlim and $
             wire.d(star,7,7) lt datlim, c) ; good data

   if c ge 10 then begin
      wire1(cnt1(star):cnt1(star)+c-1).x(star)     = wire(w).x(star)
      wire1(cnt1(star):cnt1(star)+c-1).y(star)     = wire(w).y(star)
      wire1(cnt1(star):cnt1(star)+c-1).hjd(star)   = wire(w).hjd(star)
      wire1(cnt1(star):cnt1(star)+c-1).stamp(star) = wire(w).stamp(star)
      wire1(cnt1(star):cnt1(star)+c-1).d(star,*,*) = wire(w).d(star,*,*)

      cnt1(star) = cnt1(star) + c

     ; Progress:
      if ploton then $
      oplot,wire(w).hjd(star)-t0,wire(w).d(star,3,3),$
       psym=1,col=colx1(star),symsi=.1

   w = where(tt ge jump(j) and $
             tt lt jump(j+1) and $
             wire.hjd(star) gt 5.2e4 and wire.hjd(star) lt 5.9e4,call)
      
      ;txt_out = strcompress(c,   /remove_all) + '/' + $
      ;          strcompress(call,/remove_all)
      ; print, ' %%% Data points used: '+ txt_out
      ; hitme,ss2 &     if ss2 eq 'x' then stop

   endif

endfor




  for star=0,nstar-1 do begin

; Second main target:

   if j ne (cj2-2) and j ne 0 then $
   w = where(tt ge jump(j+1) and $
             tt lt jump(j+2) and $
             wire.hjd(star) gt 5.2e4 and wire.hjd(star) lt 5.9e4 and $
             wire.d(star,0,0) lt datlim and $
             wire.d(star,7,7) lt datlim, c) ; good data
   if j eq 0 and j ne (cj2-2) then $ ; edge at early times
   w = where( ( (tt lt (jump(0)-.01)) or $
                (tt gt jump(1) and tt lt jump(2)) ) and $
             wire.hjd(star) gt 5.2e4 and wire.hjd(star) lt 5.9e4 and $
             wire.d(star,0,0) lt datlim and $
             wire.d(star,7,7) lt datlim, c) ; good data
   if j eq (cj2-2) then $ ; edge at late times
   w = where(tt gt (jump(cj2-1)+0.01) and $
             wire.hjd(star) gt 5.2e4 and wire.hjd(star) lt 5.9e4 and $
             wire.d(star,0,0) lt datlim and $
             wire.d(star,7,7) lt datlim, c) ; good data


   if c ge 10 then begin
      ; Mark time limits for secondary obervations!
      mint = min(tt(w)) & maxt = max(tt(w))
      if ploton then begin
       plots,[1.,1] * mint, [10,14], col=col.sky,thick=2
       plots,[1.,1] * maxt, [10,14], col=col.sky,thick=2,line=2
      endif

      wire2(cnt2(star):cnt2(star)+c-1).x(star)     = wire(w).x(star)
      wire2(cnt2(star):cnt2(star)+c-1).y(star)     = wire(w).y(star)
      wire2(cnt2(star):cnt2(star)+c-1).hjd(star)   = wire(w).hjd(star)
      wire2(cnt2(star):cnt2(star)+c-1).stamp(star) = wire(w).stamp(star)
      wire2(cnt2(star):cnt2(star)+c-1).d(star,*,*) = wire(w).d(star,*,*)
      cnt2(star) = cnt2(star) + c
      
     ; Progress:
      if ploton then $
       oplot,wire(w).hjd(star)-t0,wire(w).d(star,3,3),$
        psym=1,col=colx2(star),symsi=.1

      ;print,' %%% np = ' + strcompress(c)
      ;hitme,ss &     if ss eq 'x' then stop


   endif

  endfor
                          ; next star

endfor                          ; next jump



max1 = max(cnt1) & wire1 = wire1(0:max1-1) ; remove unused entries
max2 = max(cnt2) & wire2 = wire2(0:max2-1) ; remove unused entries
a = strsplit(fil(fcnt),'.',/extract)
out1 = a(0) + '.obj1.idlr'
out2 = a(0) + '.obj2.idlr'


if ploton then begin
 plot_io,wire.hjd(0)-t0,wire.d(0,3,3)*1.05,psym=3,yr=[1,70000],$
  ysty=1,xsty=1,ytit='Counts in pixel (3,3)',xtit='!4D!3 HJD',$
  xr=[-1,1]
 for s=0,4 do $
  oplot,wire1.hjd(s)-t0,wire1.d(s,3,3),psym=3,col=col.green
 for s=0,4 do $
  oplot,wire2.hjd(s)-t0,wire2.d(s,3,3),psym=3,col=col.red
 for i=0,cj2-1 do oplot,[1.,1.]*jump(i),[1,70000],line=2
endif

; ========================================================================
if edge_check and fcnt eq 0 then begin
 print, '' 
 print, ' %%% Click w/ mouse at the point where new data set starts '
 print, ' %%% This should be identified by where there is a jump in the'
 print, ' %%% magnitude of the stars ... '
 print, ' %%% To avoid removing points, click to the left most part'
 print, ' %%% of the plot!'

 print,''
 print,' %%% Do it for the Green points ... '
 print,''

  cursor,x3,y3
  oplot,[x3,x3],[1,70000],thick=3

 print,' %%% Accepted points will be plotted as yellow !'

 for ss=0,4 do begin
  wg1 = where( (wire1.hjd(ss)-t0) lt x3,cg1)
  if cg1 ge 1 then begin
    wire1(wg1).hjd(ss) = -1e5 ; bad times !!
    for k=0L,cg1-1 do $
    wire1(wg1(k)).d(ss,*,*) = -99.9 ; -1. * wire1(wg1(k)).d(ss,*,*)
  endif
 endfor 

 for s=0,4 do $
  oplot,wire1.hjd(s)-t0,wire1.d(s,3,3),psym=6,symsi=.1,col=col.yellow
 ; hitme,s & if s eq 'x' then stop
 
 print,''
 print,' %%% Do it for the red points ... '
 print,''

 cursor,x4,y4
 oplot,[x4,x4],[1,70000],thick=4,line=5

 for ss=0,4 do begin
  wg2 = where( (wire2.hjd(ss)-t0) lt x4,cg2)
  if cg2 ge 1 then begin
    wire2(wg2).hjd(ss) = -1e5 ; bad times !!
    for k=0L,cg2-1 do $
    wire2(wg2(k)).d(ss,*,*) = -99.9 ; -1. * wire2(wg2(k)).d(ss,*,*)
  endif
 endfor 

 for s=0,4 do $
  oplot,wire2.hjd(s)-t0,wire2.d(s,3,3),psym=6,symsi=.1,col=col.orchid
 ; hitme,s & if s eq 'x' then stop

endif

; ========================================================================


; ========================================================================
if edge_check and fcnt eq (nf-1) then begin 
 print, '' 
 print, ' %%% Click w/ mouse at the point where the current data set ENDS!'
 print, ' %%% This should be identified by where there is a jump in the'
 print, ' %%% magnitude of the stars ... '
 print, ' %%% To avoid removing points, click to the right-most part'
 print, ' %%% of the plot!'

 print,''
 print,' %%% Do it for the Green points ... '
 print,''
 cursor,x3e,y3e
 oplot,[x3e,x3e],[1,70000],thick=3
 print,' %%% Accepted points will be plotted as yellow !'

 for ss=0,4 do begin
  wg1 = where( (wire1.hjd(ss)-t0) gt x3e,cg1)
  if cg1 ge 1 then begin
    wire1(wg1).hjd(ss) = -1e5 ; bad times !!
    for k=0L,cg1-1 do $
    wire1(wg1(k)).d(ss,*,*) = -99.9 ; -1. * wire1(wg1(k)).d(ss,*,*)
  endif
 endfor 

 for s=0,4 do $
  oplot,wire1.hjd(s)-t0,wire1.d(s,3,3),psym=6,symsi=.1,col=col.yellow
 ; hitme,s & if s eq 'x' then stop
 
 print,''
 print,' %%% Do it for the red points ... '
 print,''

 cursor,x4e,y4e
 oplot,[x4e,x4e],[1,70000],thick=4,line=5

 for ss=0,4 do begin
  wg2 = where( (wire2.hjd(ss)-t0) gt x4e,cg2)
  if cg2 ge 1 then begin
    wire2(wg2).hjd(ss) = -1e5 ; bad times !!
    for k=0L,cg2-1 do $
    wire2(wg2(k)).d(ss,*,*) = -99.9 ; -1. * wire2(wg2(k)).d(ss,*,*)
  endif
 endfor 

 for s=0,4 do $
  oplot,wire2.hjd(s)-t0,wire2.d(s,3,3),psym=6,symsi=.1,col=col.orchid
; hitme,s & if s eq 'x' then stop

endif
; ========================================================================



wire = wire1 & wire1 = 0B
 save,filename=out1,wire,/compress
wire = wire2 & wire2 = 0B
 save,filename=out2,wire,/compress

print, ' %%% fil = ' + fil(fcnt)
print, ' %%% Saved file: ' + out1 + ' ' + string(max1,format='(I7)')
print, ' %%% Saved file: ' + out2 + ' ' + string(max2,format='(I7)')

; print, ' %%% Hit any key ... ' & s = get_kbrd(1)

  fcnt = fcnt + 1 ; increase counter

endelse                         ; not reference magn?

problem_go_next_file:
endwhile                        ; read the next file




end
