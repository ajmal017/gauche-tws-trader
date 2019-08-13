TARGET=gauchetest

LFLAGS=-pthread -Wall -Wno-switch -Wpedantic -std=c++11
ROOT_DIR=../tws/twsapi/IBJts/source/cppclient
BASE_SRC_DIR=${ROOT_DIR}/client
CFLAGS=-I${BASE_SRC_DIR} -I${ROOT_DIR}

SOURCES=GaucheAdapter.cpp Main.cpp Utils.cpp \
	$(wildcard $(BASE_SRC_DIR)/*.cpp)
OBJECTS=$(subst .cpp,.o,$(SOURCES))

$(TARGET): $(OBJECTS)
	$(CXX) $(LFLAGS) $(OBJECTS) -o $(TARGET)

%.o: %.cpp
	$(CXX) -c $(CFLAGS) $< -o $@

clean:
	rm -rf $(OBJECTS) $(TARGET)
