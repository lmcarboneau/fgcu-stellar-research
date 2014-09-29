; Import heather's reaults with:
; .r  wire_heather
; Then compare freq's with Hans B's results

dops = 1

n = n_elements(h)

n6791pos,2,8,0.07,0,0,pos

yy = [10,10,2,20,5,4,20,4, $
      1,1,2,10,2,1,4,6]

facy = 1e3

hbdir = '/home/bruntt/wire/wire_periods/'

; ===============================================================
easyps, keywords, kk, dops=dops, /prepare, dim = [15,26,-1,1], $
 fil = 'giants_comparison.ps', $
 dir = '/home/bruntt/wire/giants/'
col= getcolor(/load)
; ===============================================================


plot,[0,1],/nodata,xsty=6,ysty=6

x1 = 0.01 & x2 = 10
y1 = 0.1 & y2 = 10.

xxt = [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']

colx = [col.sky, col.red, col.magenta]

for i=0,n-1 do begin

 xxt_use = xxt
 xtxt = '' & ytxt = ''

 if i eq 7 or i eq 14 then begin
    xxt_use = ''
    xtxt = 'Frequency [c/day]'
 endif
 if i eq 7 then begin
  ytxt = 'Ampl. [mmag]'
 endif

; plot_io,[0,1],/noerase,position=pos(*,i),$
;  xr = [x1,x2], yr=[y1,yy(i)],/nodata,xtickname = xxt_use, $
;  xtit = xtxt

 plot_oo,[0,1],/noerase,position=pos(*,i),$
  xr = [x1,x2], yr=[y1,y2],/nodata,xtickname = xxt_use, $
  xtit = xtxt,charsi=.9,ytit=ytxt

  for k=0,h(i).n-1 do oplot,h(i).f(k)*[1.,1],[y1,h(i).a(k)*facy],thick=2

 s = findfile(hbdir + '*HD' + $
   strcompress(string(h(i).hd,format='(I8)'),/remove_all) + '*')
 
 nhb = n_elements(s)

 for l=0,nhb-1 do begin
  readcol,s(l),aa,f,a,p,format='a,f,f,f',/silent
  nf = n_elements(f)

  print,s(l)
  for p=0,nf-1 do begin
   oplot,f(p)*[1.,1],[y1,a(p)*facy],thick=2,col=colx(l),line=5
   print,f(p),a(p)*facy,format='(F9.3,F9.2)'
  endfor

endfor



; xyouts,x2*.1,yy(i)*.9,$

xyouts,x2*.1,y2 * .5,$
 'HD ' + strcompress(string(h(i).hd),/remove_all),charsi=.9,charthick=2


endfor

; ===============================================================
easyps, keywords, kk, dops=dops, /close
; ===============================================================


END
