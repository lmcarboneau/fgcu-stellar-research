outpsfile = '/users/bruntt/PAPER/wire_altair.ps'

a4,x=16,y=12,name=outpsfile

bv = -0.3 ; typical sun like star
vv = (findgen(100)/99.) * 15. + 0. 

plot_io,vv,vv,/nodata,xr=[-1,8],yr=[1e3,3e6],ysty=3,xthick=3,ythick=2,charsi=1.5,xsty=3,$
 xtit='V'

for jj =0,6 do begin 
 nbv = bv+jj*0.3
 cr = (534400 + 246230*(nbv)+ 976300*(nbv^2)) * 0.5 * (10.^(-0.4*vv)) ; in 0.5 seconds
 oplot,vv,cr,thick=2
 xyouts,vv(0),cr(0),strcompress(string(nbv,format='(F9.2)'),/remove_all),charsi=.7,alignment=1
endfor

ct = [57817   ,3269  , 15210 ,    3592,173200]

for i=0,n_elements(ct)-1 do begin
 plots,!x.crange,ct(i),line=1
 xyouts,7.,ct(i)*1.1,'Slot '+string(i+1,format='(I1)'),charsi=0.9
endfor

xyouts,-.5,2e6,'B-V',charsi=1.5

; oplot,[0.77,0.77],[1e3,3e6],line=1
arrow,0.,1e5,0.77,1.7e5,thick=2,/data
xyouts,0,1e5,alignment=1,'Altair',charsi=.8

device,/close
set_plot,'x'
print,'$gv '+outpsfile+' &'

end
