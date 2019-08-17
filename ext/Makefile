TARGET=gaucheadapter
TARGET_STATIC_LIB=$(TARGET).a

LFLAGS=-pthread -Wall -Wno-switch -Wpedantic -std=c++11
ROOT_DIR=../tws/twsapi/IBJts/source/cppclient
BASE_SRC_DIR=${ROOT_DIR}/client
CFLAGS=-I${BASE_SRC_DIR} -I${ROOT_DIR} $(shell gauche-config -I)

SOURCES=GaucheAdapter.cpp Main.cpp Utils.cpp \
	$(wildcard $(BASE_SRC_DIR)/*.cpp)
OBJECTS=$(subst .cpp,.o,$(SOURCES))

$(TARGET_STATIC_LIB): $(OBJECTS)
	$(AR) r $@ $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CXX) $(LFLAGS) $(OBJECTS) -o $@

%.o: %.cpp
	$(CXX) -c $(CFLAGS) $< -o $@

clean:
	rm -rf $(OBJECTS) $(TARGET)
