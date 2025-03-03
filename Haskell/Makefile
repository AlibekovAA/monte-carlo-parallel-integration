.PHONY: all clean run rebuild

RMCMD = del /Q /F
RMDIR = rmdir /S /Q
CHCP_CMD = chcp 65001
LLVM_WINDRES = C:/ghcup/msys64/mingw64/binllvm-windres.exe

all: compile

compile:
	$(CHCP_CMD)
	cabal update
	cabal build

run: compile
	cabal run

clean:
	$(CHCP_CMD)
	del /Q /F *.hi *.o 2>nul
	if exist dist-newstyle rmdir /S /Q dist-newstyle 2>nul

rebuild: clean all run
