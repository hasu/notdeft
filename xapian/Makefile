PROGRAM := notdeft-xapian
CXX := c++
CXXFLAGS := -Wall

default : build

-include local.mk

build : $(PROGRAM)

$(PROGRAM) : notdeft-xapian.cc
	$(CXX) -o $@ $< -std=c++11 $(CXXFLAGS) `pkg-config --cflags --libs tclap` `xapian-config --cxxflags --libs`

clean :
	-rm $(PROGRAM)

install-deps :
	sudo aptitude install pkg-config libtclap-dev libxapian-dev
