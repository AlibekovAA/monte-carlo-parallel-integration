CXX = g++
MPIPATH = "C:/Program Files (x86)/Microsoft SDKs/MPI"
MPIEXEC = "C:/Program Files/Microsoft MPI/Bin/mpiexec.exe"
CXXFLAGS = -O3 -fexec-charset=UTF-8 -finput-charset=UTF-8 -DUNICODE -D_UNICODE
LDFLAGS = -L$(MPIPATH)/Lib/x64 -lmsmpi

SRCS = main.cpp monte_carlo.cpp helper.cpp
OBJS = $(SRCS:.cpp=.o)
TARGET = program.exe

all: build

build: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(OBJS) -o $(TARGET) $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -I$(MPIPATH)/Include -c $< -o $@

run: build
	chcp 65001 && $(MPIEXEC) -n 4 .\$(TARGET)

clean:
	del /Q /F $(OBJS) $(TARGET) 2>nul || exit 0

rebuild: clean build run
