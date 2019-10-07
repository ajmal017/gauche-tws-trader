TARGET=gaucheadapter
TARGET_STATIC_LIB=$(TARGET).a

LFLAGS=-pthread -Wall -Wno-switch -Wpedantic -std=c++11
ROOT_DIR=../tws/twsapi/source/cppclient
BASE_SRC_DIR=${ROOT_DIR}/client
CFLAGS=-I${BASE_SRC_DIR} -I${ROOT_DIR} $(shell gauche-config -I)

SOURCES=$(wildcard *.cpp) \
	$(wildcard $(BASE_SRC_DIR)/*.cpp)
OBJECTS=$(subst .cpp,.o,$(SOURCES)) tws_client_lib.o

$(TARGET_STATIC_LIB): $(OBJECTS)
	$(AR) r $@ $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CXX) $(LFLAGS) $(OBJECTS) -o $@

tws_client_lib.o: tws_client_lib.stub
	gosh genstub $<
	$(CXX) -c $(CFLAGS) $(subst .stub,.c,$<) -o $@

%.o: %.cpp
	$(CXX) -c $(CFLAGS) $< -o $@

clean:
	rm -rf $(OBJECTS) $(TARGET) $(TARGET_STATIC_LIB) tws_client_lib.c
