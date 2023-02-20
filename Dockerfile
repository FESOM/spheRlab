# Use an official R runtime as a parent image
FROM r-base:latest

RUN apt update && apt install -y libnetcdf-dev

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

# Build and Install SpheRlab
RUN R CMD INSTALL --build .

# Install NetCDF into the R Environment
RUN Rscript -e 'install.packages("ncdf4")'
