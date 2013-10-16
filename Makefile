OBJS=jpge.o jpgd.o encoder.o
BIN=encoder
CXXFLAGS ?= -O3 -ffast-math -fno-signed-zeros

$(BIN): $(OBJS)
	$(CXX) $(CFLAGS) $(CXXFLAGS) -o $@ $^

clean:
	rm $(OBJS) $(BIN)
