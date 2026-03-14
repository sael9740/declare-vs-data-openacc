FC = nvfortran
GPUFLAGS ?= -O3 -Mstack_arrays -acc -gpu=ccnative,mem:separate -Minfo=accel
ASMFLAGS ?= -O3 -Mstack_arrays -S -acc -gpu=ccnative,mem:separate
LDFLAGS ?=
TARGET := repro_many_create_3d
SRC := repro_many_create_3d.F90

.PHONY: all asm clean run

all: $(TARGET)

$(TARGET): $(SRC)
	$(FC) $(GPUFLAGS) $(LDFLAGS) -o $@ $<

asm: $(SRC)
	$(FC) $(ASMFLAGS) -o $(basename $(SRC)).s $<

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET) *.o *.mod *.s
