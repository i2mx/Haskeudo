name = primes

default: 
	ghc -o hsc .\haskeudo.hs
	.\hsc.exe .\examples\${name}.hsc .\examples\${name}.cpp .\test.exe
	.\test.exe