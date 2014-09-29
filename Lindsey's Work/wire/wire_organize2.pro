;

; Taken from wire_organize_read.pro:
pp = strarr(4)
pp(0) = 'log'
pp(1) = 'data'
pp(2) = 'ps'
pp(3) = 'time_series'
npp = n_elements(pp)


aa = ''
obj = strarr(100)
cnt = 0

close,1

openr,1,'~/wireobj.txt'
for i=0,1 do readf,1,aa

while not eof(1) do begin

readf,1,aa

g = strsplit(aa,' ',/extract)

obj(cnt) = g(0)
cnt = cnt + 1

endwhile

obj = obj(0:cnt-1)


outdir = '/data2/bruntt/wire/dat/'

; istart = 11
; for i=istart,cnt-1 do begin

for i=0,10 do begin


out = outdir + obj(i)
spawnrob,'mkdir '+out

for p=0,npp-1 do begin

base = pp(p)

nn = where(strmatch(info.object,obj(i)) eq 1 and $
           strmatch(info.filename,'*' + base + '*') eq 1,cc)

if cc ge 1 then begin
 out2 = out + '/' + base
 spawnrob,'mkdir '+ out2

for j=0,cc-1 do begin
 command = 'mv -f '+ info(nn(j)).dir + $
   info(nn(j)).filename + ' ' + $
   out2
 ; print,command
 spawnrob,command

endfor

endif


endfor


endfor




close,1


END
