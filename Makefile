FC = nvfortran
GPUFLAGS ?= -g -O3 -Mstack_arrays -acc -gpu=ccnative,mem:separate -Minfo=accel
ASMFLAGS ?= -g -O3 -Mstack_arrays -S -acc -gpu=ccnative,mem:separate
LDFLAGS ?=
TARGET := repro_declare_vs_data
SRC := repro_declare_vs_data.F90
TARGET_MANY := repro_many_create_3d
SRC_MANY := repro_many_create_3d.F90
RUN_ARGS ?= 26214400 200
RUN_ARGS_MANY ?= 500 6 500 100 1 1
RUN_ARGS_MANY_PRESENT ?= 500 6 500 100 0 0 20000 64

.PHONY: all asm asm-many clean run run-many run-many-present

all: $(TARGET) $(TARGET_MANY)

$(TARGET): $(SRC)
	$(FC) $(GPUFLAGS) $(LDFLAGS) -o $@ $<

$(TARGET_MANY): $(SRC_MANY)
	$(FC) $(GPUFLAGS) $(LDFLAGS) -o $@ $<

asm: $(SRC)
	$(FC) $(ASMFLAGS) -o $(basename $(SRC)).s $<

asm-many: $(SRC_MANY)
	$(FC) $(ASMFLAGS) -o $(basename $(SRC_MANY)).s $<

run: $(TARGET)
	./$(TARGET) $(RUN_ARGS)

run-many: $(TARGET_MANY)
	./$(TARGET_MANY) $(RUN_ARGS_MANY)

run-many-present: $(TARGET_MANY)
	./$(TARGET_MANY) $(RUN_ARGS_MANY_PRESENT)

clean:
	rm -f $(TARGET) $(TARGET_MANY) *.o *.mod *.s
