CXX = C:/msys64/mingw64/bin/g++.exe
CXXFLAGS = -O3 -fopenmp -fexec-charset=UTF-8
LDFLAGS = -fopenmp

SRCS = main.cpp monte_carlo.cpp helper.cpp
OBJS = $(SRCS:.cpp=.o)
TARGET = program.exe
TEST = test.exe

.PHONY: all build run clean rebuild test

all: build

build: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(OBJS) -o $(TARGET) $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

run: build
	.\$(TARGET)

test: test.cpp
	$(CXX) $(CXXFLAGS) test.cpp -o $(TEST) $(LDFLAGS)
	.\$(TEST)

clean:
	del /Q /F $(OBJS) $(TARGET) $(TEST) 2>nul || exit 0

rebuild: clean build run
