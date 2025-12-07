# Use the official Arch Linux base image
FROM archlinux:latest

# Update package list and install necessary system dependencies.
# The 'stack' package from the official repos can be installed here.
RUN pacman -Syu --noconfirm && \
    pacman -S --noconfirm \
      diffutils \
      ghc cabal-install \
      git \
      make \
      man-db \
      patch \
      pkgfile \
      vim \
      which \
      zsh \
      zsh-completions \
      zsh-doc \
      zsh-syntax-highlighting \
    && \
    pkgfile -u && \
    pacman -Scc --noconfirm

RUN useradd -m coder
USER coder
ENV HOME=/home/coder

RUN cabal update

# Set the working directory inside the container
WORKDIR /home/coder

# Default command when the container starts (can be overridden)
CMD ["zsh"]
