dops = 1

if n_elements(s00) eq 0 then begin
 restore,'/ai40/bruntt/wire/wire_simul/wire_granul_spec_00_2.gran.idl'
 s00 = s
 restore,'/ai40/bruntt/wire/wire_simul/wire_granul_spec_99_2.dat.idl' ; 
 sgranmode = s
endif

if dops eq 1 then begin
 outps = '/ai40/bruntt/wire/wire_eps/procyon_simulations_zoom.eps'
 setpl,20,9,outps,1,1,encap=1
endif

mode = 'zoom'

t1 = 9.75 & y1 = -0.00015
w = where(sgranmode.tob gt -5,c) & t99 = min(sgranmode(w).tob)
w = where(s00.tob gt -5,c) & t00 = min(s00(w).tob)
xr1 = -.5 & xr2 = 14.

if mode eq 'zoom' then begin
 xr1 = 9.2 & xr2 = 9.65 & t1 = xr2 - 0.15
endif

plot,sgranmode.tob-t99,sgranmode.dob/1.086,psym=3,$
 yr=[-.007,.003],ysty=1,xr=[xr1,xr2],xsty=1,xtit='Time [days]',ytit='!4D!3I [ppt]',$
  ytickname=['-6', '-4','-2','0','+2']
 xyouts,t1,y1,'WIRE September 1999'

if mode ne 'zoom' then begin
  oplot,s00.tob-t00,s00.dob/1.086+0.002,psym=3 & xyouts,t1,y1+0.002,'WIRE September 2000'
endif

oplot,sgran.ts,sgran.ds-0.0015,psym=3 & xyouts,t1,-0.0015+y1,'Simulated granulation'
w = where(sgranmode.dn gt -.5 and sgranmode.tn gt -20,c)
nn = 1e6 * robust_sigma(sgranmode(w).dn) / 1.086

oplot,sgranmode.tn,sgranmode.dn-0.003,psym=3
xyouts,t1,-0.003+y1,'White noise (' + string(nn,format='(I3)') + ' ppm)'

oplot,sgranmode.tn,sgranmode.dn + sgran.ds-0.0045,psym=3
  xyouts,t1,-0.0045+y1,'Granulation + white noise'
oplot,spmode.ts,spmode.ds-0.006,psym=3 &  xyouts,t1,-0.006+y1,'Simulated oscillations'


if mode eq 'zoom' then begin
 plot,spmode.ts,spmode.ds*1e6,position=[.25,.78,.93,.91],$
  xr=[9.2,9.5],yr=[-1,1]*100.,psym=3,/noerase, $
  ytit='!4D!3I [ppm]',charsi=0.7,$
  ytickname=['-100',' ','0',' ','+100']

endif

if dops eq 1 then begin
 device,/close & set_plot,'x'
endif

print,' $ gv ' + outps + ' & ' 

END
