name = real

default: 
	cabal build
	cp .\dist-newstyle\build\x86_64-windows\ghc-9.8.2\Haskeudo-0.1.0.0\x\hsc\build\hsc\hsc.exe .
	.\hsc.exe .\examples\${name}.hsc .\examples\${name}.cpp .\test.exe
	.\test.exe