# Use an official Haskell runtime as a parent image
FROM haskell:9.6.4

# Set the working directory
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

# Install stack dependencies and build the project
RUN stack setup
RUN stack build --system-ghc

# Make port 9700 available to the world outside this container
EXPOSE 9700

# Define volumes for certificates
VOLUME ["/app/certs"]

# Run hs-tic-tac-toe-net when the container launches
# Certificates are expected to be in the /app/certs directory
ENTRYPOINT ["stack", "exec", "hs-tic-tac-toe-net-exe", "--"]
CMD ["--cert=/app/certs/server.crt", "--key=/app/certs/server.key", "0.0.0.0", "9700"]