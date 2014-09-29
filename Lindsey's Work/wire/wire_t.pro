; Wire transformation by match / master

device,set_Graphics_function = 3 ; ordinary plot
window,0,xsize=550,ysize=550,title='Wire Identification / Seconday Targets'

t = [0.053   ,  0.099 ,  1.05032 , -0.03515 ,  0.09595 ,  1.00449 ] ; ,  -2.458 ,   1.6105]

x = ra_proj-mra
y = dec-mdec

xn = t(0) + t(2) * x + t(4) * y
yn = t(1) + t(3) * x + t(5) * y

 plot,ra_proj-mra,dec-mdec,psym=3,xsty=3,ysty=3,/nodata,$
  xtit='Projected RA / Degrees',ytit='DEC / Degrees',$
  xr= [-5,5],yr=[-5,5]
 plotsym,0

for i=0,nt-1 do begin
 rmax = sqrt( dx1(i)^2. + dy1(i)^2. ) & rmin = rmax
 tvellipse, rmax, rmin, 0, 0, 0.,thick=2,/data,linestyle=2 
endfor

for i=0,c-1 do begin
 vv1 = vv(i) & bv1 = bv(i)
 cr1 = (534400 + 246230*bv1+ 976300*bv1^2) * 0.5 * (10.^(-0.4*vv1))
 mg1 = 25. -2.5 * alog10(cr1)
 sz1 = (25.-mg1)/5.
 plots,xn(i),yn(i),psym=8,symsize=sz1
 
endfor

dx2 = dx1 & dy2 = dy1

readalf,'wire.als',a

nz = n_elements(dx1)
 for i=0,nz-1 do begin
    mg2 = a(3,i)
    sz2 = (25.-mg2) / 5.
    plots,a(1,i),a(2,i),psym=8,col=col.green,symsize = sz2

;   mg2 = 25. -fac * alog10(ct(i))
;   sz2 = (25.-mg2) / fac2 
;   plots,dx2(i),dy2(i),psym=8,col=col.green,symsize = sz2
 endfor


end