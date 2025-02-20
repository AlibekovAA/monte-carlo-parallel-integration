CXX = g++
CXXFLAGS = -O3 -Wall -Wextra -Werror -std=c++17 -fexec-charset=UTF-8

SRCS = main.cpp monte_carlo.cpp helper.cpp
OBJS = $(SRCS:.cpp=.o)
TARGET = program.exe

.PHONY: all build run clean rebuild

all: build

build: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(OBJS) -o $(TARGET)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

run: build
	./$(TARGET)

clean:
	del /Q /F $(OBJS) $(TARGET) 2>nul || exit 0

rebuild: clean build run
