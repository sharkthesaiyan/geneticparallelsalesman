flags=-O3
compiler=mpif90

salesman: main.f95 length_measurement.o breeding.o randomgen.o qsort_mod.o
	$(compiler) main.f95 length_measurement.o breeding.o randomgen.o qsort_mod.o -o salesman $(flags)

length_measurement.o: length_measurement.f95
	$(compiler) length_measurement.f95 -c $(flags)

randomgen.o: randomgen.f95
	$(compiler) randomgen.f95 -c $(flags)

qsort_mod.o: qsort_mod.f95 randomgen.o
	$(compiler) qsort_mod.f95 randomgen.o -c $(flags)

breeding.o: breeding.f95 length_measurement.o randomgen.o qsort_mod.o
	$(compiler) breeding.f95 length_measurement.o randomgen.o qsort_mod.o -c $(flags)

clean:
	rm *.mod
	rm *.o
	rm salesman
