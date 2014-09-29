PRO hitme, bb, silent=silent, message=message

default9, message, ' >>> Hit any key to continue!'

if n_elements(silent) eq 0 then silent = 0B
if silent ne 1 then print,message

bb = get_kbrd(1)
if bb eq 'X' then stop

END
