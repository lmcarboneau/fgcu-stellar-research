PRO wire_target_info, file, inf

; Example: wire_target_info, '~/wire/targets_wire.txt', info

cnt = 0L

get_lun,u
openr,u,file

dummy = ''
temp  = ''
for i=0,1 do readf,u,dummy

nmax = 200
inf = replicate( {object:'', hd:0L, spec:'', b:0., v:0., bv:0., pax: 0., epax:0., vsini:0., evsini:0., $
                  ra1:'', dec1:'', $
                  ra2:0., dec2:0., $
                  ra_1:0.,   ra_2:0.,  ra_3:0., $
                  dec_1:0., dec_2:0., dec_3:0., $
                  hb:0., ehb:0., nhb:0., $
                  by:0., eby:0., m1:0., em1:0., c1:0., ec1:0., nuvby:0} , nmax)

while not eof(u) do begin
 readf,u,temp
 a = strsplit(temp,' ',/extract)

 inf(cnt).object = a(0)
 inf(cnt).hd    = long(a(1))
 inf(cnt).spec  = a(2)
 inf(cnt).b     = float(a(3))
 inf(cnt).v     = float(a(4))
 inf(cnt).bv    = inf(cnt).b - inf(cnt).v 
 inf(cnt).pax   = float(a(5))
 inf(cnt).epax  = float(a(6))
 inf(cnt).vsini = float(a(7))
 inf(cnt).evsini= float(a(8))

 inf(cnt).ra1   = a(9)  + ' ' + a(10) + ' ' + a(11)
 inf(cnt).dec1  = a(12) + ' ' + a(13) + ' ' + a(14)

 inf(cnt).ra_1  = float(a(9))  & inf(cnt).ra_2  = float(a(10)) &  inf(cnt).ra_3  = float(a(11)) 
 inf(cnt).dec_1 = float(a(12)) & inf(cnt).dec_2 = float(a(13)) &  inf(cnt).dec_3 = float(a(14)) 

 inf(cnt).ra2   = 15. * ten([inf(cnt).ra_1, inf(cnt).ra_2,inf(cnt).ra_3])
 inf(cnt).dec2  = ten([inf(cnt).dec_1, inf(cnt).dec_2,inf(cnt).dec_3])


 inf(cnt).hb    = float(a(15))
 inf(cnt).ehb   = float(a(16))
 inf(cnt).nhb   = float(a(17))
 inf(cnt).by    = float(a(18))
 inf(cnt).eby   = float(a(19))
 inf(cnt).m1    = float(a(20))
 inf(cnt).em1   = float(a(21))
 inf(cnt).c1    = float(a(22))
 inf(cnt).ec1   = float(a(23))
 inf(cnt).nuvby = float(a(24))
 cnt = cnt + 1
endwhile


close,u
free_lun,u

inf = inf(0:cnt-1)

END
