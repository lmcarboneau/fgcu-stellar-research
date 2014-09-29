; goto,here

virgo = fltarr(2,500000)
cnt = 0L

spawn,'ls -1 ~bruntt/wire/virgo/n_*.p00',p00
spawn,'ls -1 ~bruntt/wire/virgo/n_*.p01',p01
n00 = n_elements(p00) & n01 = n_elements(p01)

for i=0,n00-1 do begin
 read_level1P,p00(i),day,value,flag
 plot,day,value,ysty=3,min_value = 100
 ; s = get_kbrd(1)

 w = where(day gt 1. and value gt 100.,c)
 if c ge 3 then begin
  virgo(0,cnt:cnt+c-1) = day(w)
  virgo(1,cnt:cnt+c-1) = value(w)
  cnt = cnt + c
 endif

endfor


for i=0,n01-1 do begin
 read_level1P,p01(i),day,value,flag
 plot,day,value,ysty=3,min_value = 100
 ; s = get_kbrd(1)

 w = where(day gt 1. and value gt 100.,c)
 if c ge 3 then begin
  virgo(0,cnt:cnt+c-1) = day(w)
  virgo(1,cnt:cnt+c-1) = value(w)
  cnt = cnt + c
 endif

endfor


virgo = virgo(*,0:cnt-1)
plot,virgo(0,*),virgo(1,*),ysty=3

med = avg(virgo(1,*))

virgo2 = virgo
virgo2(1,*) = virgo2(1,*) / med
virgo2(1,*) = virgo2(1,*) - avg(virgo2(1,*))
virgo2(0,*) = virgo2(0,*) - avg(virgo2(0,*))

plot, virgo2(0,*), virgo2(1,*), ysty=3,psym=3


here:

; t2 = max(virgo2(0,*)) & t1 = min(virgo2(1,*))

dn = 5L
np = n_elements(virgo2(0,*))
n = 0L
p = 0L

virgo3 = fltarr(2, long((np / dn) + 10000))

while n lt (np-dn) do begin
 time = virgo2(0,n:n+dn)
 data = virgo2(1,n:n+dn)

 resistant_mean, data, 3, me, sd, nr
 
 virgo3(0,p) = median(time)
 virgo3(1,p) = me
 
 n = n + dn
 p = p + 1L

endwhile

virgo3 = virgo3(*,0:p-1)

col=getcolor(/load)
oplot,virgo3(0,*),virgo3(1,*),psym=3,col=col.red

print,'Avg time step: ',avg((virgo3(0,501:531) - virgo3(0,500:530)))*86400.
print,'Avg time step: ',avg((virgo2(0,501:531) - virgo2(0,500:530)))*86400.

w = where(virgo2(0,*) gt 25. and virgo2(0,*) lt 45.,c)

dm = avg(virgo2(1,w))
dt = avg(virgo2(0,w))

outfile = '~bruntt/wire/virgo/virgo_20days.dat'
openw,1,outfile
for i=0L,c-1 do $
 printf,1,virgo2(0,w(i))-dt, virgo2(1,w(i))-dm, 1., $
  format='(D15.7, D15.8, F8.2)'
close,1

print,' %%% Saved file: '+outfile

end
 
; read_level1P,file_pmo1,day,value,flag

