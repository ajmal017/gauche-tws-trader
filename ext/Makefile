TARGET=empty

CXXFLAGS=-pthread -Wall -Wno-switch -Wpedantic -std=c++11
ROOT_DIR=../tws/twsapi/IBJts/source/cppclient
BASE_SRC_DIR=${ROOT_DIR}/client
INCLUDES=-I${BASE_SRC_DIR} -I${ROOT_DIR}

SOURCES=EmptyClient.cpp Main.cpp Utils.cpp

$(TARGET): $(SOURCES)
	$(CXX) $(CXXFLAGS) $(INCLUDES) $(BASE_SRC_DIR)/*.cpp $(SOURCES) -o$(TARGET)
