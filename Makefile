OBJS=jpge.o jpgd.o encoder.o
BIN=encoder
CXXFLAGS ?= -O3 -ffast-math -fno-signed-zeros -fopenmp -freciprocal-math -ffinite-math-only

$(BIN): $(OBJS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

clean:
	rm $(OBJS) $(BIN)
