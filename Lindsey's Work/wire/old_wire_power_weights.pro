PRO wire_power_weights, x, y, ww, factor=factor, $
 va=va, fr=fr, width=width, gauss_plot=gauss_plot, overlap=overlap, $
 reiterate=reiterate, fcen=fcen, fwid=fwid, antal=wi, gwid=gwid

; In a log log plot the density of points is much
; larger at the high freq. range. Any fit to such
; a distribution of points will give exponentially
; higher weights to the points at high values

; This program will calculate weights so that 
; points at high and low frequency get equal
; weights ... 

default9, width,  0.5
default9, factor, 0.5
default9, gauss_plot, 0B
default9, reiterate,  0B
default9, overlap, 0.0075
default9, fcen, 2000.  ; reference frequency in MicroHz
default9, fwid, 100.   ; width of averaging width of gaussians
default9, gwid, 20.   ; width of the smoothing in microHz

; if gauss_plot then plot_oo,x,y,xr=[100,10000],yr=[.001,max(y)],xsty=1
if gauss_plot then plot,x,y,xr=[2000,2500],xsty=1,ysty=3

x1 = min(x)
x2 = max(x)

xlog = alog10(x)
xlog1 = min(xlog)
xlog2 = max(xlog)

whz = where( abs(x-fcen) lt fwid,chz)

np = 99
; if reiterate then np = ceil( ((x2-x1) / 10000.) * 200. )



again:

if reiterate eq 1 then begin
 print,  ' %%% wi: ',n_elements(wi)

 if n_elements(wi) ge 3 then begin
;  actual = median(wi(whz))
;  np = ceil(100. * (  actual / chz ) ) 

  actual = median(freq_width(whz)) ; typical width of gaussian fct.

  print,' Org. np: ',np
 
  np = np * actual / gwid

  print,np, actual, chz, actual/chz
 endif

endif

print,' %%% np factor: ',np

xx = xlog1 + (( xlog2 - xlog1)/np ) * findgen(np+1)

; Define output arrays:
wi = fltarr(n_elements(x)) & va = wi & fr = wi

freq_width =  fltarr(n_elements(x))

enh_width = overlap ; 0.0075

for i=0,np-1 do begin
 
 x1 =  10.^(xx(i)   * (1.0 - enh_width) )  
 x2 =  10.^(xx(i+1) * (1.0 + enh_width) )
 
 w = where(x ge x1 and x le x2,c)

 if c ge 6 then begin 

   ; Define a gaussian to do the smoothing:
    x_cen = (x2+x1) * 0.5
    fac   = 2. * ((x2-x1) * 0.25 / 2.35)^2.0
    gauss = exp(- (x - x_cen)^2. / fac )
    gauss = gauss / total(gauss)

    freq_width(w) = (x2-x1)
 
    if gauss_plot then begin
     col=getcolor(/load) & colx = [col.white, col.sky]
     oplot,x,gauss * (max(y) / max(gauss)), col=colx(reiterate)
    endif

    wi(w) = c
    va(w) = total(y * gauss)
    fr(w) = avg([x1,x2])

;    resistant_mean, y(w), 3, me, sd, nr
;    wi(w) = c-nr
;    va(w) = me
;
    fr(w) = avg(x(w))
 endif
 if c ge 2 and c le 5 then begin 
    wi(w) = c
    va(w) = avg(y(w))
    fr(w) = avg(x(w)) ; (x2+x1) * 0.5
 endif
 if c eq 1 then begin
    wi(w) = c
    va(w) = y(w)
    fr(w) = x(w) ; (x2+x1) * 0.5
 endif

 ; if i ge 50 then stop

endfor

ww = wi
g = where(wi gt 0. and finite(wi),c)
ww(g) = (1.0 / ww(g))^factor ; ^0.5 ; ^0.2
ww(g) = ww(g) / total(ww(g))



; print,' %%% True number of points at 1milliHz: ',median(wi(whz))

reiterate = reiterate + 1B
if reiterate eq 1 then goto, again




END
