
Type foo
End Type

Local i = Handle Object.foo Handle Object.foo Handle Object.foo 42

Local a.foo = New foo
i = Handle a
Print Str Object.foo i; + 1   ;- Object binds more tightly than infix operators

a = Last foo
a = Before Last foo
a = Before Object.foo i

WaitKey
End

;~IDEal Editor Parameters:
;~C#Blitz3D