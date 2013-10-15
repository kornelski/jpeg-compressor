OBJS=jpge.o jpgd.o encoder.o
BIN=encoder

$(BIN): $(OBJS)
	$(CXX) $(CFLAGS) $(CPPFLAGS) -o $@ $^

clean:
	rm $(OBJS) $(BIN)
