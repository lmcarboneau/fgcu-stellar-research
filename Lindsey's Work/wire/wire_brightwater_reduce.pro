PRO wire_brightwater_reduce, wildcard, bias, flat, red=red

; Locate position of target star?
default9, xy_method, 'colight'
default9, box, 25
default9, back1, 28
default9, back2, 40
default9, ap1, 15
default9, ap2, 20
default9, ap3, 25
default9, ap4, 30

col=getcolor(/load)

spawnrob,'ls -1 ' + wildcard, l
n = n_elements(l)

red = replicate({fil:'', jd:0D, hjd:0D, mag:0., x:0., y:0., $
                 flux1:0., flux2:0., flux3:0., flux4:0.},n)

for i=0,n-1 do begin

 fil = l(i)
 reduced = (readfits(fil,head) - bias) / flat

 x = strsplit(fil,'.',/extract)
 fil2 = x(0) + 'r.fit'

 writefits,fil2,reduced,head


if xy_method eq 'colight' then begin

  colight_x = 0.
  colight_y = 0.

  me = median(reduced)
  showim, reduced, me*.9,me*1.2,title=fil
  cursor,xs,ys & xs = round(xs) & ys = round(ys)

  reduced2 = reduced - median(reduced) ; subtr. approx background
  boxer = reduced2(xs-box:xs+box,ys-box:ys+box)
  flux = total(boxer) ; approx flux

  for x1 =xs-box,xs+box-1 do begin
   for y1 =ys-box,ys+box-1 do begin
     colight_x = colight_x + (x1+0.5) * reduced2(x1,y1)
     colight_y = colight_y + (y1+0.5) * reduced2(x1,y1)
   endfor
 endfor
 colight_x = (colight_x / flux)  - .5 ;+ x4
 colight_y = (colight_y / flux)  - .5 ;+ y4

 nx = n_elements(reduced(*,0)) & ny = n_elements(reduced(0,*))
 dist_circle,dd,[nx,ny],colight_x,colight_y

 wback = where(dd ge back1 and dd le back2, cback)
 back = median(reduced2(wback))
 w1 = where(dd le ap1,c1)
 w2 = where(dd le ap2,c2)
 w3 = where(dd le ap3,c3)
 w4 = where(dd le ap4,c4)
 flux1 = total(reduced2(w1)) - back * c1
 flux2 = total(reduced2(w2)) - back * c2
 flux3 = total(reduced2(w3)) - back * c3
 flux4 = total(reduced2(w4)) - back * c4

 red(i).x = colight_x
 red(i).y = colight_y
 red(i).mag = -2.5 * alog10(flux) + 25.
 red(i).fil = fil2
 red(i).flux1 = -2.5 * alog10(flux1) + 25.
 red(i).flux2 = -2.5 * alog10(flux2) + 25.
 red(i).flux3 = -2.5 * alog10(flux3) + 25.
 red(i).flux4 = -2.5 * alog10(flux4) + 25.

 jd = strsplit(head(39),' ',/extract) &  red(i).jd = double(jd(2))
 hjd = strsplit(head(38),' ',/extract) &  red(i).hjd = double(hjd(2))

 plots,colight_x,colight_y, psym=7,thick=2,col=col.sky
 wait,0.3
; plot,reduced2(w3),psym=3
; wait,0.3


; hitme,mess = ' OK? ', s9  &  if s9 eq 'x' then stop

endif ; end of center of light


endfor

END
