PRO wire_bin_most, t, d, t2, d2, fac

np = n_elements(t)
ts = median( t(1:np-1) - t(0:np-2)) ; typical time step
jump = ts * fac

d2   = fltarr(np)
t2   = d2
used = bytarr(np)

count = 0L
tstart = min(t)
cnt = 0L


while count lt np do begin

 tstart = t(count)
 tav = t(count:np-1)
 dav = d(count:np-1)

 w = where( (tav ge tstart) and (tav lt (tstart+jump)), c)
 if c ge 3 then begin
   d2(cnt) = avg(dav(w))
   t2(cnt) = avg(tav(w))
   cnt = cnt + 1
   tstart = max(t(w))
   count = count + c
 endif else begin
   count = count + 1
endelse

print,count, format='(I6,$)'

endwhile

d2 = d2(0:cnt-1)
t2 = t2(0:cnt-1)
print,''

END
