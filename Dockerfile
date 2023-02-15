# Use an official R runtime as a parent image
FROM r-base:latest

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

# Build Spherelab
RUN R CMD build .
