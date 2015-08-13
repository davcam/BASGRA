gfortran -x f95-cpp-input -Dweathergen -O3 -c -fdefault-real-8 model\parameters_site.f90 model\parameters_plant.f90 model\environment.f90 model\resources.f90 model\soil.f90 model\plant.f90 model\set_params.f90 model\BASGRA.f90
gfortran -shared -o BASGRA_WG.DLL parameters_site.o parameters_plant.o environment.o resources.o soil.o plant.o set_params.o BASGRA.o
del *.o
del *.mod
pause