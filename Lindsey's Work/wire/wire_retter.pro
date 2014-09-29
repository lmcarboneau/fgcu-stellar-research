; Compare power spectra of H. Bruntt & A. Retter
; (c) Hans Bruntt, 2nd of October 2003

; outfile = '/ai39/bruntt/wire/altair/altair_merged_allslots_xx_decor1.idl' ; 1.10.03
outfile = '/ai39/bruntt/wire/altair/altair_merged_allslots_41_decor1.idl' ; 2.10.03
export_dat = 0
remove_spline_fit_to_scattered_light = 0
remove_scattered_light_data = 1

; Read Retter's data:
if n_elements(d2) eq 0 then begin
readcol,'/usr/users/bruntt/wire/retter_altair_series02.dat',$
 skipline=23,t2,d2,w2,format='D,D,F'
 t2 = t2 - 10.297       ; approx offset in time!
 d2 = d2 - median(d2)
endif

; Read Bruntt data
; if n_elements(wire3) eq 0 then 
restore,outfile
t0  = 51480.0
tt  = wire3.hjd - t0
dat = wire3.mag(0)   ; main target!
gc  = wire3.gc(*,0)
dat = dat - median(dat)
w   = where(abs(dat) lt .02,np)
dat = dat(w) & tt = tt(w)
col = getcolor(/load)
fre1= 15.009
ph  = (tt mod (1./fre1)) * fre1

fac = 1e6 / 86400.0D
per1 = 15.009D

if remove_spline_fit_to_scattered_light eq 1 then begin
 wire_spline,tt, dat, 1./per1, phasex, dat2x, dat3x
 dat = dat3x
endif

; -------------------------------------------------------------------
if remove_scattered_light_data eq 1 then begin
   pha = (tt mod (1./per1)) * per1
   wx = where( (pha ge -.51 and pha lt -0.22) or $
               (pha ge  .495 and pha lt  0.79),cx) ; found using wire_phase.pro

     plot,pha,dat,psym=3,yr=[-1,1]*0.01
     oplot,pha(wx),dat(wx),psym=3,col=col.red
   
   dat = dat(wx) - median(dat(wx))
   tt = tt(wx)
   gc = gc(wx)

   if n_elements(dat3x) ne 0 then begin
    phasex = phasex(wx)
    dat2x = dat2x(wx)
    dat3x = dat3x(wx)
   endif
   print,' %%% Scattered light data removed!'
endif

ttx = tt  &  datx = -1.0 * dat ; change sign!

; Get weights
wire_stetson, datx, weix
wire_stetson, d2  , w2

if export_dat eq 1 then begin
  openw,1,'/ai39/bruntt/wire/altair/altair.dat'
  nx = n_elements(ttx)
  offset = median(datx)
  for i=0L,nx-1 do printf,1,ttx(i),datx(i)-offset,weix(i),format='(D10.6,F9.5,F11.7)'
  close,1
endif

print,' Hit '  &  s = get_kbrd(1)


ptp_robust_fin,d2  ,noise1,1
ptp_robust_fin,datx,noise2,1
print,'Point To Point noise levels: Retter & Bruntt'
c1 = n_elements(d2)
c2 = n_elements(datx)
n1 = noise1 / sqrt(c1-1.)
n2 = noise2 / sqrt(c2-1.)
print,n1,n2
print,'Noise ratio (taking into account the number of points):'
print,n1/n2
print,'Expected noise in power spectra:'
print,'Retter: ',sqrt(!DPI/n1) * noise1
print,'Bruntt: ',sqrt(!DPI/n2) * noise2

plot,ttx,datx,psym=3,xr=[7,7.5],yr=[-1,1]*0.005
 oplot,t2,d2,psym=1,col=col.red,symsi=.2

print,' Hit to comp. power spectra '
s = get_kbrd(1)
print,' ... ok!'

datx3 = datx & ttx3 = ttx & weix3 = weix
ampl_spec_calc_wire,ttx3,datx3,weix3,0.0,70.0,freqx,ampx,phasex

daty3 = d2 & tty3 = t2 & weiy3 = w2
ampl_spec_calc_wire,tty3,daty3,weiy3,0.0,70.0,freqy,ampy,phasey

fac = 1e6 / 86400. 
fl = max(freqx)*0.8
w = where(freqx gt fl,c) & resistant_mean,ampx(w),3,mex,sd,nr
fl = max(freqy)*0.8
w = where(freqy gt fl,c) & resistant_mean,ampy(w),3,mey,sd,nr

x = [' ',' ',' ',' ',' ',' ',' ',' ']
getpos,1,2,0.0,0.0,0,pp & plot,[0,1],/nodata,xsty=6,ysty=6

a4,x=18,y=14,name='~/wire/retter_bruntt_ampl_ver2.ps'

!P.charsize=1.5

plot,freqx*fac,ampx,xr=[0,70]*fac,ysty=1,position=pp(*,0),xtickname=x,/noerase,yr=[0,600]
 plots,!x.crange,250.,line=1
 plots,!x.crange,500.,line=2
 xyouts,500,400,'Bruntt' & xyouts,600,400,string(mex,format='(I4)')+ ' ppm'
plot,freqy*fac,ampy,xr=[0,70]*fac,position=pp(*,1),/noerase,yr=[0,600],xtit='f [!4n!3Hz]'
 plots,!x.crange,250.,line=1
 plots,!x.crange,500.,line=2
 xyouts,500,400,'Retter' & xyouts,600,400,string(mey,format='(I4)')+ ' ppm'

device,/close
set_plot,'x'

print,' $  gv ~/wire/retter_bruntt_ampl.ps &'
print,' $  lpr -Phpnew ~/wire/retter_bruntt_ampl.ps'

end
