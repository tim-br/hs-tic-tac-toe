# Use the official Haskell image from Docker Hub
FROM haskell:8.10.7 as builder

# Set the working directory in the container
WORKDIR /app

RUN curl -sSL https://get.haskellstack.org/ | sh

# Copy the stack configuration files
COPY stack.yaml package.yaml /app/
## COPY .stack-work/ /app/.stack-work/

# Install system dependencies (if any)
RUN apt-get update && apt-get install -y \
    libgmp-dev

# Copy all files needed by the stack to build the project
COPY . /app

# Build the project using stack
# Ensure stack will use the system GHC already available in the image
RUN stack build --system-ghc

# Reduce image size by using a smaller base image for the runtime environment
FROM debian:buster-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

# Copy the build artifacts from the builder stage
WORKDIR /app
COPY --from=builder /app/.stack-work/install/x86_64-linux/ghc-8.10.7/bin/ /app/

# Set the environment variables (if needed)
ENV PATH="/app:${PATH}"

# Expose the port the app runs on
EXPOSE 9700

# Run the application
CMD ["hs-tic-tac-toe-net-exe", "--cert=/app/certs/server.crt", "--key=/app/certs/server.key", "localhost", "9700"]
