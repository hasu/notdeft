PROGRAM := notdeft-xapian
CXX := c++
CXXFLAGS := -Wall

default : build

-include local.mk

build : $(PROGRAM)

$(PROGRAM) : notdeft-xapian.cc
	$(CXX) -o $@ -std=c++11 $(CXXFLAGS) $(shell pkg-config --cflags tclap) $(shell xapian-config --cxxflags) $< $(shell pkg-config --libs tclap) $(shell xapian-config --libs)

clean :
	-rm $(PROGRAM)

install-deps :
	sudo aptitude install pkg-config libtclap-dev libxapian-dev
