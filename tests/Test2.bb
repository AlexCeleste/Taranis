
;Namespace test - arrays obscure functions

Dim foo(10)
foo(5) = 6

Print foo(4)
WaitKey


Function foo(n)
	Print n
	Return foo(5)
End Function


;~IDEal Editor Parameters:
;~C#Blitz3D