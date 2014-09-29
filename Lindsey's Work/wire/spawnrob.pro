PRO spawnrob, command, output

spawn, command, a

na = n_elements(a)
uu = bytarr(na)

for i=0L,na-1 do $
 if strmatch(a(i),'*user_shell*') eq 1 then uu(i) = 1

w = where(uu eq 0,c)
if c ge 1 then a = a(w) else a = ''

if n_elements(a) eq 1 then a = a(0)

output = a


END
