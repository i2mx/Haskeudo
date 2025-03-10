default: 
	ghc -o hsc .\haskeudo.hs
	.\hsc.exe .\test.hsc .\test.cpp .\test.exe
	.\test.exe