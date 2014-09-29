PRO wire_clean_plot,fc,r1,r2,y2,mode=mode,over=over,auto=auto, $
 fc2 = fc2,name=name,dops=dops, id=id

col = getcolor(/load)

nf = n_elements(fc)

markfreq = 1B ; mark all found frequencies
if n_elements(dops) eq 0 then dops = 0
if dops ge 1 then markfreq = 0

f_orbit = 174.0 * 1e-3 ; orbit frequency in milliHz
if n_elements(over) eq 0 then over = 0.
if over lt 0. then f_orbit = -1D * over
if n_elements(mode) eq 0 then mode = ''


if n_elements(y2) eq 0 then y2 = max(fc(0).amp) * 1.05 ; max y range

if n_elements(auto) eq 1 then begin
 r1 = min(fc(0).freq) & r2 = max(fc(0).freq) ; full freq. range
 mode = ''
 over = 0
 markfreq = 0
 dops = 1
endif

if n_elements(name) eq 1 then begin
 a = strsplit(name,'_',/extract)
 wa = where(a eq 'star',ca)
 titl = a(wa-1) + ' -  Slot = ' + a(wa+1)
 psfile = '/ai40/bruntt/wire/wire_amp/' + a(wa-1) + '_star_' + a(wa+1) + '.ps'
endif

f1 = min(fc(nf-1).freq)
f2 = max(fc(nf-1).freq)
wire_calc_ampl_noise, fc(nf-1).freq, fc(nf-1).amp, f1, f2, 50, noise

if dops ge 1 then begin
 if n_elements(psfile) eq 0 then psfile = '/ai40/bruntt/wire/wire_amp/default.ps'
 setpl,18,9,psfile,1,1
 device,/color
 col=getcolor(/load)
endif


!P.multi=0
if mode eq 'a' then !P.multi=[0,1,2]

if markfreq eq 0 then nf = 1 ; only do plot once


for i=0,nf-1 do begin
  ; plot,fc(i).freq,fc(i).amp,xr=[r1,r2],yr=[0,y2]

if mode eq 'a' then begin

  w2 = where(fc(i).freq gt 0.,c2)
  plot,fc(i).freq(w2),fc(i).amp(w2),xr=[ 0, 3]-.5,yr=[0,y2]
  oplot,noise(0,*),noise(1,*)*3.,psym=-6,col=col.red,symsi=0.5,line=1
  oplot,noise(0,*),noise(1,*)*4.,psym=-6,col=col.red,symsi=0.5,line=2
  oplot,noise(0,*),noise(1,*)*5.,psym=-6,col=col.red,symsi=0.5
  for j=0,nf-1 do plots,fc(j).f,!y.crange,col=col.red,line=2,thick=2
  plots,fc(i).f,!y.crange,thick=2,col=col.green,line=2


  plot,fc(i).freq(w2),fc(i).amp(w2),xr=[15,18]-.5,yr=[0,y2]
  oplot,noise(0,*),noise(1,*)*3.,psym=-6,col=col.red,symsi=0.5,line=1
  oplot,noise(0,*),noise(1,*)*4.,psym=-6,col=col.red,symsi=0.5,line=2
  oplot,noise(0,*),noise(1,*)*5.,psym=-6,col=col.red,symsi=0.5
  for j=0,nf-1 do plots,fc(j).f,!y.crange,col=col.red,line=2,thick=2
  plots,fc(i).f,!y.crange,thick=2,col=col.green,line=2
endif else begin

  w2 = where(fc(i).freq gt 0.,c2)
  plot,fc(i).freq(w2),fc(i).amp(w2),xr=[r1,r2],yr=[0,y2], $
   xtit='Frequency',ytit='Amplitude',tit=titl

  if markfreq then begin
   oplot,noise(0,*),noise(1,*)*3.,psym=-6,col=col.red,symsi=0.5,line=1
   oplot,noise(0,*),noise(1,*)*4.,psym=-6,col=col.red,symsi=0.5,line=2
   oplot,noise(0,*),noise(1,*)*5.,psym=-6,col=col.red,symsi=0.5
   for j=0,nf-1 do plots,fc(j).f,!y.crange,col=col.red,line=2,thick=2
   plots,fc(i).f,!y.crange,thick=2,col=col.green,line=2
 endif
  
 if markfreq eq 0 and n_elements(fc2) ge 1 then begin
  ngood = n_elements(fc2)
  for g=0,ngood-1 do $
   plots,fc2(g).f,!y.crange,thick=1,line=2,col=col.red

  if n_elements(id) eq 1 then $
   xyouts,(r2-r1) * 0.05 + r1, y2 * 0.9, id,charthick=2.0,charsi=1.1
   
 endif

endelse

; Overplot orbital frequencies?
if (over) ne 0 then $
 for j=0,100 do $
  if f_orbit * (1. + j) lt max(!x.crange) then $
  plots,f_orbit * (1. + j),!y.crange,col=col.sky,line=5


  if nf ge 2 then begin
   print,' %%% Hit any key: '+string(fc(i).f,format='(F9.3)') 
   s = get_kbrd(1)
   if s eq 'x' then RETURN
  endif

endfor


if dops ge 1 then begin
 print,' %%%  $  ggv ' + psfile + '  & '
 device,/close
 set_plot,'x'
endif


END
