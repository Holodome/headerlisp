.PHONY: all opt clean debug release 

all: debug
opt: release

GENERATOR ?=
FORCE_COLOR ?=
ASAN_FLAG ?=

ifeq ($(GEN),ninja)
	GENERATOR=-G "Ninja"
	FORCE_COLOR=-DFORCE_COLORED_OUTPUT=1
endif
ifeq (${ASAN}, 1)
	ASAN_FLAG=-DASAN=1
endif

clean:
	rm -rf build

debug: 
	mkdir -p ./build/debug && \
	cd build/debug && \
	cmake $(GENERATOR) $(FORCE_COLOR) $(ASAN_FLAG) -DCMAKE_EXPORT_COMPILE_COMMANDS=On -DCMAKE_BUILD_TYPE=Debug ../.. && \
	cmake --build . --config Debug --target all 

release: 
	mkdir -p ./build/release && \
	cd build/release && \
	cmake $(GENERATOR) $(FORCE_COLOR) -DCMAKE_BUILD_TYPE=Release ../.. && \
	cmake --build . --config Release --target all -j$(nproc)

reldebug: 
	mkdir -p ./build/reldebug && \
	cd build/reldebug && \
	cmake $(GENERATOR) $(FORCE_COLOR) -DCMAKE_BUILD_TYPE=RelWithDebInfo ../.. && \
	cmake --build . --config RelWithDebInfo --target all -j$(nproc)
