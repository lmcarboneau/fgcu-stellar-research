; Reorganize Wire files on mons machine at USAFA
; Run this after wire_orgainze_read.pro

n = n_elements(b)
if n eq 0 then restore,'~/wire_files.idl'
n = n_elements(b)

info = replicate( {dir:'', $
                   basedir:'', filename:'', suffix1:'', suffix2:'', suffix3:'', $
                   object:'', object2: '', object3: '', $
                   datestamp:0L, $
                   timestamp:0.}, n)


cnt = 0L

print,' Counter much reach ... ',n

for i=0L,n-1 do begin

if i mod 2000 eq 0 then print,i,format='(I7,$)'

nam = b(i)

if nam eq '' then goto,fail_nam

;; if strmatch(nam,'*data1*') eq 1 then addslash = 1

a = strsplit(nam,'/',/extract)
na = n_elements(a)
if na le 1 then goto,fail_nam ; eg. a subdir

base1 = a(na-1)
info(cnt).filename = base1

dir = '/'
for kk=0,na-2 do $
 dir = dir + a(kk) + '/'
info(cnt).dir = dir

basedir = a(na-3) + '/' + a(na-2)
info(cnt).basedir = basedir

xx = strsplit(base1,'.',/extract)
nxx  =  n_elements(xx) 
if nxx le 1 then goto,fail_nam

info(cnt).datestamp = long(xx(0))

info(cnt).suffix1 = xx(3)
if nxx ge 5 then info(cnt).suffix2 = xx(4)

ent  = strmid(xx(2),1,1)
ent2 = strmid(xx(2),2,1)
lll = strlen(xx(2))

addt = 0B
if ent eq '0' or ent eq '1' or ent eq '2' or $
   ent eq '3' or ent eq '4' or ent eq '5' or $
   ent eq '6' or ent eq '7' or ent eq '8' or ent eq '9' then $
   addt = 1B
if ent2 eq '0' or ent2 eq '1' or ent2 eq '2' or $
   ent2 eq '3' or ent2 eq '4' or ent2 eq '5' or $
   ent2 eq '6' or ent2 eq '7' or ent2 eq '8' or ent2 eq '9' then $
   addt = addt + 1B

info(cnt).timestamp = float( xx(1)+'.'+ strmid(xx(2),0, 1 + addt) )
info(cnt).object    = strmid(xx(2),1 + addt, lll-addt-1)
cnt = cnt + 1

fail_nam:


endfor


ss = sort(info.object)
info = info(ss)


w = where(strmatch(info.filename,'*data*') eq 1,c)
info2 = info(w)
g = uniq(info2.object)
ng = n_elements(g)
for i=0,ng-1 do $
 print,info2(g(i)).object,$
 info2(g(i)).basedir+'/', $
 info2(g(i)).filename,$
 format='(A20,A18,A40)'




end
